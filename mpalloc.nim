##
##  Copyright (c) 2008-2020 Stefan Krah. All rights reserved.
##
##  Redistribution and use in source and binary forms, with or without
##  modification, are permitted provided that the following conditions
##  are met:
##
##  1. Redistributions of source code must retain the above copyright
##     notice, this list of conditions and the following disclaimer.
##
##  2. Redistributions in binary form must reproduce the above copyright
##     notice, this list of conditions and the following disclaimer in the
##     documentation and/or other materials provided with the distribution.
##
##  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND
##  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
##  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
##  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
##  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
##  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
##  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
##  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
##  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
##  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
##  SUCH DAMAGE.
##

import
  mpdecimal, mpalloc, typearith

when defined(_MSC_VER):
##  Guaranteed minimum allocation for a coefficient. May be changed once
##    at program start using mpd_setminalloc().

var MPD_MINALLOC*: mpd_ssize_t = MPD_MINALLOC_MIN

##  Custom allocation and free functions
##  c2nim TODO void *(* mpd_mallocfunc)(size_t size) = malloc;
##  c2nim TODO void *(* mpd_reallocfunc)(void *ptr, size_t size) = realloc;
##  c2nim TODO void *(* mpd_callocfunc)(size_t nmemb, size_t size) = calloc;
##  c2nim TODO void (* mpd_free)(void *ptr) = free;
##  emulate calloc if it is not available

proc mpd_callocfunc_em*(nmemb: csize; size: csize): pointer =
  var `ptr`: pointer
  var req: csize
  var overflow: mpd_size_t
  req = mul_size_t_overflow(cast[mpd_size_t](nmemb), cast[mpd_size_t](size),
                          addr(overflow))
  if overflow:
    return nil
  `ptr` = mpd_mallocfunc(req)
  if `ptr` == nil:
    return nil
  memset(`ptr`, 0, req)
  return `ptr`

##  malloc with overflow checking

proc mpd_alloc*(nmemb: mpd_size_t; size: mpd_size_t): pointer =
  var
    req: mpd_size_t
    overflow: mpd_size_t
  req = mul_size_t_overflow(nmemb, size, addr(overflow))
  if overflow:
    return nil
  return mpd_mallocfunc(req)

##  calloc with overflow checking

proc mpd_calloc*(nmemb: mpd_size_t; size: mpd_size_t): pointer =
  var overflow: mpd_size_t
  cast[nil](mul_size_t_overflow(nmemb, size, addr(overflow)))
  if overflow:
    return nil
  return mpd_callocfunc(nmemb, size)

##  realloc with overflow checking

proc mpd_realloc*(`ptr`: pointer; nmemb: mpd_size_t; size: mpd_size_t; err: ptr uint8_t): pointer =
  var new: pointer
  var
    req: mpd_size_t
    overflow: mpd_size_t
  req = mul_size_t_overflow(nmemb, size, addr(overflow))
  if overflow:
    err[] = 1
    return `ptr`
  new = mpd_reallocfunc(`ptr`, req)
  if new == nil:
    err[] = 1
    return `ptr`
  return new

##  struct hack malloc with overflow checking

proc mpd_sh_alloc*(struct_size: mpd_size_t; nmemb: mpd_size_t; size: mpd_size_t): pointer =
  var
    req: mpd_size_t
    overflow: mpd_size_t
  req = mul_size_t_overflow(nmemb, size, addr(overflow))
  if overflow:
    return nil
  req = add_size_t_overflow(req, struct_size, addr(overflow))
  if overflow:
    return nil
  return mpd_mallocfunc(req)

##  Allocate a new decimal with a coefficient of length 'nwords'. In case
##    of an error the return value is NULL.

proc mpd_qnew_size*(nwords: mpd_ssize_t): ptr mpd_t =
  var result: ptr mpd_t
  nwords = if (nwords < MPD_MINALLOC): MPD_MINALLOC else: nwords
  result = mpd_alloc(1, sizeof(result[]))
  if result == nil:
    return nil
  result.data = mpd_alloc(nwords, sizeof(result.data[]))
  if result.data == nil:
    mpd_free(result)
    return nil
  result.flags = 0
  result.exp = 0
  result.digits = 0
  result.len = 0
  result.alloc = nwords
  return result

##  Allocate a new decimal with a coefficient of length MPD_MINALLOC.
##    In case of an error the return value is NULL.

proc mpd_qnew*(): ptr mpd_t =
  return mpd_qnew_size(MPD_MINALLOC)

##  Allocate new decimal. Caller can check for NULL or MPD_Malloc_error.
##    Raises on error.

proc mpd_new*(ctx: ptr mpd_context_t): ptr mpd_t =
  var result: ptr mpd_t
  result = mpd_qnew()
  if result == nil:
    mpd_addstatus_raise(ctx, MPD_Malloc_error)
  return result

##
##  Input: 'result' is a static mpd_t with a static coefficient.
##  Assumption: 'nwords' >= result->alloc.
##
##  Resize the static coefficient to a larger dynamic one and copy the
##  existing data. If successful, the value of 'result' is unchanged.
##  Otherwise, set 'result' to NaN and update 'status' with MPD_Malloc_error.
##

proc mpd_switch_to_dyn*(result: ptr mpd_t; nwords: mpd_ssize_t; status: ptr uint32_t): cint =
  var p: ptr mpd_uint_t = result.data
  assert(nwords >= result.alloc)
  result.data = mpd_alloc(nwords, sizeof(result.data[]))
  if result.data == nil:
    result.data = p
    mpd_set_qnan(result)
    mpd_set_positive(result)
    result.exp = result.digits = result.len = 0
    status[] = status[] or MPD_Malloc_error
    return 0
  memcpy(result.data, p, result.alloc * (sizeof(result.data[])))
  result.alloc = nwords
  mpd_set_dynamic_data(result)
  return 1

##
##  Input: 'result' is a static mpd_t with a static coefficient.
##
##  Convert the coefficient to a dynamic one that is initialized to zero. If
##  malloc fails, set 'result' to NaN and update 'status' with MPD_Malloc_error.
##

proc mpd_switch_to_dyn_zero*(result: ptr mpd_t; nwords: mpd_ssize_t;
                            status: ptr uint32_t): cint =
  var p: ptr mpd_uint_t = result.data
  result.data = mpd_calloc(nwords, sizeof(result.data[]))
  if result.data == nil:
    result.data = p
    mpd_set_qnan(result)
    mpd_set_positive(result)
    result.exp = result.digits = result.len = 0
    status[] = status[] or MPD_Malloc_error
    return 0
  result.alloc = nwords
  mpd_set_dynamic_data(result)
  return 1

##
##  Input: 'result' is a static or a dynamic mpd_t with a dynamic coefficient.
##  Resize the coefficient to length 'nwords':
##    Case nwords > result->alloc:
##      If realloc is successful:
##        'result' has a larger coefficient but the same value. Return 1.
##      Otherwise:
##        Set 'result' to NaN, update status with MPD_Malloc_error and return 0.
##    Case nwords < result->alloc:
##      If realloc is successful:
##        'result' has a smaller coefficient. result->len is undefined. Return 1.
##      Otherwise (unlikely):
##        'result' is unchanged. Reuse the now oversized coefficient. Return 1.
##

proc mpd_realloc_dyn*(result: ptr mpd_t; nwords: mpd_ssize_t; status: ptr uint32_t): cint =
  var err: uint8_t = 0
  result.data = mpd_realloc(result.data, nwords, sizeof(result.data[]), addr(err))
  if not err:
    result.alloc = nwords
  elif nwords > result.alloc:
    mpd_set_qnan(result)
    mpd_set_positive(result)
    result.exp = result.digits = result.len = 0
    status[] = status[] or MPD_Malloc_error
    return 0
  return 1

##
##  Input: 'result' is a static mpd_t with a static coefficient.
##  Assumption: 'nwords' >= result->alloc.
##
##  Resize the static coefficient to a larger dynamic one and copy the
##  existing data.
##
##  On failure the value of 'result' is unchanged.
##

proc mpd_switch_to_dyn_cxx*(result: ptr mpd_t; nwords: mpd_ssize_t): cint =
  assert(nwords >= result.alloc)
  var data: ptr mpd_uint_t = mpd_alloc(nwords, sizeof(result.data[]))
  if data == nil:
    return 0
  memcpy(data, result.data, result.alloc * (sizeof(result.data[])))
  result.data = data
  result.alloc = nwords
  mpd_set_dynamic_data(result)
  return 1

##
##  Input: 'result' is a static or a dynamic mpd_t with a dynamic coefficient.
##  Resize the coefficient to length 'nwords':
##    Case nwords > result->alloc:
##      If realloc is successful:
##        'result' has a larger coefficient but the same value. Return 1.
##      Otherwise:
##        'result' has a the same coefficient. Return 0.
##    Case nwords < result->alloc:
##      If realloc is successful:
##        'result' has a smaller coefficient. result->len is undefined. Return 1.
##      Otherwise (unlikely):
##        'result' is unchanged. Reuse the now oversized coefficient. Return 1.
##

proc mpd_realloc_dyn_cxx*(result: ptr mpd_t; nwords: mpd_ssize_t): cint =
  var err: uint8_t = 0
  var p: ptr mpd_uint_t = mpd_realloc(result.data, nwords, sizeof(result.data[]),
                                 addr(err))
  if not err:
    result.data = p
    result.alloc = nwords
  elif nwords > result.alloc:
    return 0
  return 1