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

when defined(msc_Ver):
##  Guaranteed minimum allocation for a coefficient. May be changed once
##    at program start using mpd_setminalloc().

var MPD_MINALLOC*: MpdSsizeT = mpd_Minalloc_Min

##  Custom allocation and free functions
##  C2NIM:
##
##  void *(* mpd_mallocfunc)(size_t size) = malloc;
##  void *(* mpd_reallocfunc)(void *ptr, size_t size) = realloc;
##  void *(* mpd_callocfunc)(size_t nmemb, size_t size) = calloc;
##  void (* mpd_free)(void *ptr) = free;
##  emulate calloc if it is not available

proc mpdCallocfuncEm*(nmemb: csize; size: csize): pointer =
  var `ptr`: pointer
  var req: csize
  var overflow: MpdSizeT
  req = mulSizeTOverflow(cast[MpdSizeT](nmemb), cast[MpdSizeT](size), addr(overflow))
  if overflow:
    return nil
  `ptr` = mpdMallocfunc(req)
  if `ptr` == nil:
    return nil
  memset(`ptr`, 0, req)
  return `ptr`

##  malloc with overflow checking

proc mpdAlloc*(nmemb: MpdSizeT; size: MpdSizeT): pointer =
  var
    req: MpdSizeT
    overflow: MpdSizeT
  req = mulSizeTOverflow(nmemb, size, addr(overflow))
  if overflow:
    return nil
  return mpdMallocfunc(req)

##  calloc with overflow checking

proc mpdCalloc*(nmemb: MpdSizeT; size: MpdSizeT): pointer =
  var overflow: MpdSizeT
  cast[nil](mulSizeTOverflow(nmemb, size, addr(overflow)))
  if overflow:
    return nil
  return mpdCallocfunc(nmemb, size)

##  realloc with overflow checking

proc mpdRealloc*(`ptr`: pointer; nmemb: MpdSizeT; size: MpdSizeT; err: ptr uint8T): pointer =
  var new: pointer
  var
    req: MpdSizeT
    overflow: MpdSizeT
  req = mulSizeTOverflow(nmemb, size, addr(overflow))
  if overflow:
    err[] = 1
    return `ptr`
  new = mpdReallocfunc(`ptr`, req)
  if new == nil:
    err[] = 1
    return `ptr`
  return new

##  struct hack malloc with overflow checking

proc mpdShAlloc*(structSize: MpdSizeT; nmemb: MpdSizeT; size: MpdSizeT): pointer =
  var
    req: MpdSizeT
    overflow: MpdSizeT
  req = mulSizeTOverflow(nmemb, size, addr(overflow))
  if overflow:
    return nil
  req = addSizeTOverflow(req, structSize, addr(overflow))
  if overflow:
    return nil
  return mpdMallocfunc(req)

##  Allocate a new decimal with a coefficient of length 'nwords'. In case
##    of an error the return value is NULL.

proc mpdQnewSize*(nwords: MpdSsizeT): ptr MpdT =
  var result: ptr MpdT
  nwords = if (nwords < mpd_Minalloc): mpd_Minalloc else: nwords
  result = mpdAlloc(1, sizeof(result[]))
  if result == nil:
    return nil
  result.data = mpdAlloc(nwords, sizeof(result.data[]))
  if result.data == nil:
    mpdFree(result)
    return nil
  result.flags = 0
  result.exp = 0
  result.digits = 0
  result.len = 0
  result.alloc = nwords
  return result

##  Allocate a new decimal with a coefficient of length MPD_MINALLOC.
##    In case of an error the return value is NULL.

proc mpdQnew*(): ptr MpdT =
  return mpdQnewSize(mpd_Minalloc)

##  Allocate new decimal. Caller can check for NULL or MPD_Malloc_error.
##    Raises on error.

proc mpdNew*(ctx: ptr MpdContextT): ptr MpdT =
  var result: ptr MpdT
  result = mpdQnew()
  if result == nil:
    mpdAddstatusRaise(ctx, mPD_MallocError)
  return result

##
##  Input: 'result' is a static mpd_t with a static coefficient.
##  Assumption: 'nwords' >= result->alloc.
##
##  Resize the static coefficient to a larger dynamic one and copy the
##  existing data. If successful, the value of 'result' is unchanged.
##  Otherwise, set 'result' to NaN and update 'status' with MPD_Malloc_error.
##

proc mpdSwitchToDyn*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint =
  var p: ptr MpdUintT = result.data
  assert(nwords >= result.alloc)
  result.data = mpdAlloc(nwords, sizeof(result.data[]))
  if result.data == nil:
    result.data = p
    mpdSetQnan(result)
    mpdSetPositive(result)
    result.exp = result.digits = result.len = 0
    status[] = status[] or mPD_MallocError
    return 0
  memcpy(result.data, p, result.alloc * (sizeof(result.data[])))
  result.alloc = nwords
  mpdSetDynamicData(result)
  return 1

##
##  Input: 'result' is a static mpd_t with a static coefficient.
##
##  Convert the coefficient to a dynamic one that is initialized to zero. If
##  malloc fails, set 'result' to NaN and update 'status' with MPD_Malloc_error.
##

proc mpdSwitchToDynZero*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint =
  var p: ptr MpdUintT = result.data
  result.data = mpdCalloc(nwords, sizeof(result.data[]))
  if result.data == nil:
    result.data = p
    mpdSetQnan(result)
    mpdSetPositive(result)
    result.exp = result.digits = result.len = 0
    status[] = status[] or mPD_MallocError
    return 0
  result.alloc = nwords
  mpdSetDynamicData(result)
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

proc mpdReallocDyn*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint =
  var err: uint8T = 0
  result.data = mpdRealloc(result.data, nwords, sizeof(result.data[]), addr(err))
  if not err:
    result.alloc = nwords
  elif nwords > result.alloc:
    mpdSetQnan(result)
    mpdSetPositive(result)
    result.exp = result.digits = result.len = 0
    status[] = status[] or mPD_MallocError
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

proc mpdSwitchToDynCxx*(result: ptr MpdT; nwords: MpdSsizeT): cint =
  assert(nwords >= result.alloc)
  var data: ptr MpdUintT = mpdAlloc(nwords, sizeof(result.data[]))
  if data == nil:
    return 0
  memcpy(data, result.data, result.alloc * (sizeof(result.data[])))
  result.data = data
  result.alloc = nwords
  mpdSetDynamicData(result)
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

proc mpdReallocDynCxx*(result: ptr MpdT; nwords: MpdSsizeT): cint =
  var err: uint8T = 0
  var p: ptr MpdUintT = mpdRealloc(result.data, nwords, sizeof(result.data[]), addr(err))
  if not err:
    result.data = p
    result.alloc = nwords
  elif nwords > result.alloc:
    return 0
  return 1
