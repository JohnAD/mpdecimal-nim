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
  basearith, bits, constants, convolute, crt, mpalloc, typearith

when defined(PPRO):
  when defined(_MSC_VER):
  elif not defined(__OpenBSD__) and not defined(__NetBSD__):
    ##  C99
##  Disable warning that is part of -Wextra since gcc 7.0.

when defined(__GNUC__) and not defined(__INTEL_COMPILER) and __GNUC__ >= 7:
##  c2nim TODO
## #if defined(_MSC_VER)
##   #define ALWAYS_INLINE __forceinline
## #elif defined (__IBMC__) || defined(LEGACY_COMPILER)
##   #define ALWAYS_INLINE
##   #undef inline
##   #define inline
## #else
##   #ifdef TEST_COVERAGE
##     #define ALWAYS_INLINE
##   #else
##     #define ALWAYS_INLINE inline __attribute__ ((always_inline))
##   #endif
## #endif
##

const
  MPD_NEWTONDIV_CUTOFF* = 1024

##  c2nim TODO
## #define MPD_NEW_STATIC(name, flags, exp, digits, len) \
##         mpd_uint_t name##_data[MPD_MINALLOC_MAX];                    \
##         mpd_t name = {flags|MPD_STATIC|MPD_STATIC_DATA, exp, digits, \
##                       len, MPD_MINALLOC_MAX, name##_data}
##
## #define MPD_NEW_CONST(name, flags, exp, digits, len, alloc, initval) \
##         mpd_uint_t name##_data[alloc] = {initval};                   \
##         mpd_t name = {flags|MPD_STATIC|MPD_CONST_DATA, exp, digits,  \
##                       len, alloc, name##_data}
##
## #define MPD_NEW_SHARED(name, a) \
##         mpd_t name = {(a->flags&~MPD_DATAFLAGS)|MPD_STATIC|MPD_SHARED_DATA, \
##                       a->exp, a->digits, a->len, a->alloc, a->data}
##

var data_one*: array[1, mpd_uint_t] = [1]

var data_zero*: array[1, mpd_uint_t] = [0]

var one*: mpd_t = [MPD_STATIC or MPD_CONST_DATA, 0, 1, 1, 1, data_one]

var minus_one*: mpd_t = [MPD_NEG or MPD_STATIC or MPD_CONST_DATA, 0, 1, 1, 1, data_one]

var zero*: mpd_t = [MPD_STATIC or MPD_CONST_DATA, 0, 1, 1, 1, data_zero]

proc _mpd_check_exp*(dec: ptr mpd_t; ctx: ptr mpd_context_t; status: ptr uint32_t) {.
    inline.}
proc _settriple*(result: ptr mpd_t; sign: uint8_t; a: mpd_uint_t; exp: mpd_ssize_t)
proc _mpd_real_size*(data: ptr mpd_uint_t; size: mpd_ssize_t): mpd_ssize_t {.inline.}
proc _mpd_cmp_abs*(a: ptr mpd_t; b: ptr mpd_t): cint
proc _mpd_qadd*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t)
proc _mpd_qmul*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) {.inline.}
proc _mpd_base_ndivmod*(q: ptr mpd_t; r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                       status: ptr uint32_t)
proc _mpd_qpow_uint*(result: ptr mpd_t; base: ptr mpd_t; exp: mpd_uint_t;
                    resultsign: uint8_t; ctx: ptr mpd_context_t; status: ptr uint32_t) {.
    inline.}
proc mpd_qsshiftr*(result: ptr mpd_t; a: ptr mpd_t; n: mpd_ssize_t): mpd_uint_t
## ****************************************************************************
##                                   Version
## ****************************************************************************

proc mpd_version*(): cstring =
  return MPD_VERSION

## ****************************************************************************
##                   Performance critical inline functions
## ****************************************************************************

when defined(CONFIG_64):
  ##  Digits in a word, primarily useful for the most significant word.
  ##  ALWAYS_INLINE
  proc mpd_word_digits*(word: mpd_uint_t): cint =
    if word < mpd_pow10[9]:
      if word < mpd_pow10[4]:
        if word < mpd_pow10[2]:
          return if (word < mpd_pow10[1]): 1 else: 2
        return if (word < mpd_pow10[3]): 3 else: 4
      if word < mpd_pow10[6]:
        return if (word < mpd_pow10[5]): 5 else: 6
      if word < mpd_pow10[8]:
        return if (word < mpd_pow10[7]): 7 else: 8
      return 9
    if word < mpd_pow10[14]:
      if word < mpd_pow10[11]:
        return if (word < mpd_pow10[10]): 10 else: 11
      if word < mpd_pow10[13]:
        return if (word < mpd_pow10[12]): 12 else: 13
      return 14
    if word < mpd_pow10[18]:
      if word < mpd_pow10[16]:
        return if (word < mpd_pow10[15]): 15 else: 16
      return if (word < mpd_pow10[17]): 17 else: 18
    return if (word < mpd_pow10[19]): 19 else: 20

else:
  ##  ALWAYS_INLINE
  proc mpd_word_digits*(word: mpd_uint_t): cint =
    if word < mpd_pow10[4]:
      if word < mpd_pow10[2]:
        return if (word < mpd_pow10[1]): 1 else: 2
      return if (word < mpd_pow10[3]): 3 else: 4
    if word < mpd_pow10[6]:
      return if (word < mpd_pow10[5]): 5 else: 6
    if word < mpd_pow10[8]:
      return if (word < mpd_pow10[7]): 7 else: 8
    return if (word < mpd_pow10[9]): 9 else: 10

##  Adjusted exponent
##  ALWAYS_INLINE

proc mpd_adjexp*(dec: ptr mpd_t): mpd_ssize_t =
  return (dec.exp + dec.digits) - 1

##  Etiny
##  ALWAYS_INLINE

proc mpd_etiny*(ctx: ptr mpd_context_t): mpd_ssize_t =
  return ctx.emin - (ctx.prec - 1)

##  Etop: used for folding down in IEEE clamping
##  ALWAYS_INLINE

proc mpd_etop*(ctx: ptr mpd_context_t): mpd_ssize_t =
  return ctx.emax - (ctx.prec - 1)

##  Most significant word
##  ALWAYS_INLINE

proc mpd_msword*(dec: ptr mpd_t): mpd_uint_t =
  assert(dec.len > 0)
  return dec.data[dec.len - 1]

##  Most significant digit of a word

proc mpd_msd*(word: mpd_uint_t): mpd_uint_t {.inline.} =
  var n: cint
  n = mpd_word_digits(word)
  return word div mpd_pow10[n - 1]

##  Least significant digit of a word
##  ALWAYS_INLINE

proc mpd_lsd*(word: mpd_uint_t): mpd_uint_t =
  return word mod 10

##  Coefficient size needed to store 'digits'

proc mpd_digits_to_size*(digits: mpd_ssize_t): mpd_ssize_t =
  var
    q: mpd_ssize_t
    r: mpd_ssize_t
  _mpd_idiv_word(addr(q), addr(r), digits, MPD_RDIGITS)
  return if (r == 0): q else: q + 1

##  Number of digits in the exponent. Not defined for MPD_SSIZE_MIN.

proc mpd_exp_digits*(exp: mpd_ssize_t): cint {.inline.} =
  exp = if (exp < 0): -exp else: exp
  return mpd_word_digits(exp)

##  Canonical
##  ALWAYS_INLINE

proc mpd_iscanonical*(dec: ptr mpd_t): cint =
  cast[nil](dec)
  return 1

##  Finite
##  ALWAYS_INLINE

proc mpd_isfinite*(dec: ptr mpd_t): cint =
  return not (dec.flags and MPD_SPECIAL)

##  Infinite
##  ALWAYS_INLINE

proc mpd_isinfinite*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_INF

##  NaN
##  ALWAYS_INLINE

proc mpd_isnan*(dec: ptr mpd_t): cint =
  return dec.flags and (MPD_NAN or MPD_SNAN)

##  Negative
##  ALWAYS_INLINE

proc mpd_isnegative*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_NEG

##  Positive
##  ALWAYS_INLINE

proc mpd_ispositive*(dec: ptr mpd_t): cint =
  return not (dec.flags and MPD_NEG)

##  qNaN
##  ALWAYS_INLINE

proc mpd_isqnan*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_NAN

##  Signed
##  ALWAYS_INLINE

proc mpd_issigned*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_NEG

##  sNaN
##  ALWAYS_INLINE

proc mpd_issnan*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_SNAN

##  Special
##  ALWAYS_INLINE

proc mpd_isspecial*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_SPECIAL

##  Zero
##  ALWAYS_INLINE

proc mpd_iszero*(dec: ptr mpd_t): cint =
  return not mpd_isspecial(dec) and mpd_msword(dec) == 0

##  Test for zero when specials have been ruled out already
##  ALWAYS_INLINE

proc mpd_iszerocoeff*(dec: ptr mpd_t): cint =
  return mpd_msword(dec) == 0

##  Normal

proc mpd_isnormal*(dec: ptr mpd_t; ctx: ptr mpd_context_t): cint {.inline.} =
  if mpd_isspecial(dec):
    return 0
  if mpd_iszerocoeff(dec):
    return 0
  return mpd_adjexp(dec) >= ctx.emin

##  Subnormal

proc mpd_issubnormal*(dec: ptr mpd_t; ctx: ptr mpd_context_t): cint {.inline.} =
  if mpd_isspecial(dec):
    return 0
  if mpd_iszerocoeff(dec):
    return 0
  return mpd_adjexp(dec) < ctx.emin

##  Odd word
##  ALWAYS_INLINE

proc mpd_isoddword*(word: mpd_uint_t): cint =
  return word and 1

##  Odd coefficient
##  ALWAYS_INLINE

proc mpd_isoddcoeff*(dec: ptr mpd_t): cint =
  return mpd_isoddword(dec.data[0])

##  0 if dec is positive, 1 if dec is negative
##  ALWAYS_INLINE

proc mpd_sign*(dec: ptr mpd_t): uint8_t =
  return dec.flags and MPD_NEG

##  1 if dec is positive, -1 if dec is negative
##  ALWAYS_INLINE

proc mpd_arith_sign*(dec: ptr mpd_t): cint =
  return 1 - 2 * mpd_isnegative(dec)

##  Radix
##  ALWAYS_INLINE

proc mpd_radix*(): clong =
  return 10

##  Dynamic decimal
##  ALWAYS_INLINE

proc mpd_isdynamic*(dec: ptr mpd_t): cint =
  return not (dec.flags and MPD_STATIC)

##  Static decimal
##  ALWAYS_INLINE

proc mpd_isstatic*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_STATIC

##  Data of decimal is dynamic
##  ALWAYS_INLINE

proc mpd_isdynamic_data*(dec: ptr mpd_t): cint =
  return not (dec.flags and MPD_DATAFLAGS)

##  Data of decimal is static
##  ALWAYS_INLINE

proc mpd_isstatic_data*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_STATIC_DATA

##  Data of decimal is shared
##  ALWAYS_INLINE

proc mpd_isshared_data*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_SHARED_DATA

##  Data of decimal is const
##  ALWAYS_INLINE

proc mpd_isconst_data*(dec: ptr mpd_t): cint =
  return dec.flags and MPD_CONST_DATA

## ****************************************************************************
##                          Inline memory handling
## ****************************************************************************
##  Fill destination with zeros
##  ALWAYS_INLINE

proc mpd_uint_zero*(dest: ptr mpd_uint_t; len: mpd_size_t) =
  var i: mpd_size_t
  i = 0
  while i < len:
    dest[i] = 0
    inc(i)

##  Free a decimal
##  ALWAYS_INLINE

proc mpd_del*(dec: ptr mpd_t) =
  if mpd_isdynamic_data(dec):
    mpd_free(dec.data)
  if mpd_isdynamic(dec):
    mpd_free(dec)

##
##  Resize the coefficient. Existing data up to 'nwords' is left untouched.
##  Return 1 on success, 0 otherwise.
##
##  Input invariant: MPD_MINALLOC <= result->alloc.
##
##  Case nwords == result->alloc:
##      'result' is unchanged. Return 1.
##
##  Case nwords > result->alloc:
##    Case realloc success:
##      The value of 'result' does not change. Return 1.
##    Case realloc failure:
##      'result' is NaN, status is updated with MPD_Malloc_error. Return 0.
##
##  Case nwords < result->alloc:
##    Case is_static_data or realloc failure [1]:
##      'result' is unchanged. Return 1.
##    Case realloc success:
##      The value of result is undefined (expected). Return 1.
##
##
##  [1] In that case the old (now oversized) area is still valid.
##
##  ALWAYS_INLINE

proc mpd_qresize*(result: ptr mpd_t; nwords: mpd_ssize_t; status: ptr uint32_t): cint =
  assert(not mpd_isconst_data(result))
  ##  illegal operation for a const
  assert(not mpd_isshared_data(result))
  ##  illegal operation for a shared
  assert(MPD_MINALLOC <= result.alloc)
  nwords = if (nwords <= MPD_MINALLOC): MPD_MINALLOC else: nwords
  if nwords == result.alloc:
    return 1
  if mpd_isstatic_data(result):
    if nwords > result.alloc:
      return mpd_switch_to_dyn(result, nwords, status)
    return 1
  return mpd_realloc_dyn(result, nwords, status)

##  Same as mpd_qresize, but do not set the result no NaN on failure.

proc mpd_qresize_cxx*(result: ptr mpd_t; nwords: mpd_ssize_t): cint =
  assert(not mpd_isconst_data(result))
  ##  illegal operation for a const
  assert(not mpd_isshared_data(result))
  ##  illegal operation for a shared
  assert(MPD_MINALLOC <= result.alloc)
  nwords = if (nwords <= MPD_MINALLOC): MPD_MINALLOC else: nwords
  if nwords == result.alloc:
    return 1
  if mpd_isstatic_data(result):
    if nwords > result.alloc:
      return mpd_switch_to_dyn_cxx(result, nwords)
    return 1
  return mpd_realloc_dyn_cxx(result, nwords)

##  Same as mpd_qresize, but the complete coefficient (including the old
##  memory area!) is initialized to zero.
##  ALWAYS_INLINE

proc mpd_qresize_zero*(result: ptr mpd_t; nwords: mpd_ssize_t; status: ptr uint32_t): cint =
  assert(not mpd_isconst_data(result))
  ##  illegal operation for a const
  assert(not mpd_isshared_data(result))
  ##  illegal operation for a shared
  assert(MPD_MINALLOC <= result.alloc)
  nwords = if (nwords <= MPD_MINALLOC): MPD_MINALLOC else: nwords
  if nwords != result.alloc:
    if mpd_isstatic_data(result):
      if nwords > result.alloc:
        return mpd_switch_to_dyn_zero(result, nwords, status)
    elif not mpd_realloc_dyn(result, nwords, status):
      return 0
  mpd_uint_zero(result.data, nwords)
  return 1

##
##  Reduce memory size for the coefficient to MPD_MINALLOC. In theory,
##  realloc may fail even when reducing the memory size. But in that case
##  the old memory area is always big enough, so checking for MPD_Malloc_error
##  is not imperative.
##
##  ALWAYS_INLINE

proc mpd_minalloc*(result: ptr mpd_t) =
  assert(not mpd_isconst_data(result))
  ##  illegal operation for a const
  assert(not mpd_isshared_data(result))
  ##  illegal operation for a shared
  if not mpd_isstatic_data(result) and result.alloc > MPD_MINALLOC:
    var err: uint8_t = 0
    result.data = mpd_realloc(result.data, MPD_MINALLOC, sizeof(result.data[]),
                            addr(err))
    if not err:
      result.alloc = MPD_MINALLOC

proc mpd_resize*(result: ptr mpd_t; nwords: mpd_ssize_t; ctx: ptr mpd_context_t): cint =
  var status: uint32_t = 0
  if not mpd_qresize(result, nwords, addr(status)):
    mpd_addstatus_raise(ctx, status)
    return 0
  return 1

proc mpd_resize_zero*(result: ptr mpd_t; nwords: mpd_ssize_t; ctx: ptr mpd_context_t): cint =
  var status: uint32_t = 0
  if not mpd_qresize_zero(result, nwords, addr(status)):
    mpd_addstatus_raise(ctx, status)
    return 0
  return 1

## ****************************************************************************
##                        Set attributes of a decimal
## ****************************************************************************
##  Set digits. Assumption: result->len is initialized and > 0.

proc mpd_setdigits*(result: ptr mpd_t) {.inline.} =
  var wdigits: mpd_ssize_t = mpd_word_digits(mpd_msword(result))
  result.digits = wdigits + (result.len - 1) * MPD_RDIGITS

##  Set sign
##  ALWAYS_INLINE

proc mpd_set_sign*(result: ptr mpd_t; sign: uint8_t) =
  result.flags = result.flags and not MPD_NEG
  result.flags = result.flags or sign

##  Copy sign from another decimal
##  ALWAYS_INLINE

proc mpd_signcpy*(result: ptr mpd_t; a: ptr mpd_t) =
  var sign: uint8_t = a.flags and MPD_NEG
  result.flags = result.flags and not MPD_NEG
  result.flags = result.flags or sign

##  Set infinity
##  ALWAYS_INLINE

proc mpd_set_infinity*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_SPECIAL
  result.flags = result.flags or MPD_INF

##  Set qNaN
##  ALWAYS_INLINE

proc mpd_set_qnan*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_SPECIAL
  result.flags = result.flags or MPD_NAN

##  Set sNaN
##  ALWAYS_INLINE

proc mpd_set_snan*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_SPECIAL
  result.flags = result.flags or MPD_SNAN

##  Set to negative
##  ALWAYS_INLINE

proc mpd_set_negative*(result: ptr mpd_t) =
  result.flags = result.flags or MPD_NEG

##  Set to positive
##  ALWAYS_INLINE

proc mpd_set_positive*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_NEG

##  Set to dynamic
##  ALWAYS_INLINE

proc mpd_set_dynamic*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_STATIC

##  Set to static
##  ALWAYS_INLINE

proc mpd_set_static*(result: ptr mpd_t) =
  result.flags = result.flags or MPD_STATIC

##  Set data to dynamic
##  ALWAYS_INLINE

proc mpd_set_dynamic_data*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_DATAFLAGS

##  Set data to static
##  ALWAYS_INLINE

proc mpd_set_static_data*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_DATAFLAGS
  result.flags = result.flags or MPD_STATIC_DATA

##  Set data to shared
##  ALWAYS_INLINE

proc mpd_set_shared_data*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_DATAFLAGS
  result.flags = result.flags or MPD_SHARED_DATA

##  Set data to const
##  ALWAYS_INLINE

proc mpd_set_const_data*(result: ptr mpd_t) =
  result.flags = result.flags and not MPD_DATAFLAGS
  result.flags = result.flags or MPD_CONST_DATA

##  Clear flags, preserving memory attributes.
##  ALWAYS_INLINE

proc mpd_clear_flags*(result: ptr mpd_t) =
  result.flags = result.flags and (MPD_STATIC or MPD_DATAFLAGS)

##  Set flags, preserving memory attributes.
##  ALWAYS_INLINE

proc mpd_set_flags*(result: ptr mpd_t; flags: uint8_t) =
  result.flags = result.flags and (MPD_STATIC or MPD_DATAFLAGS)
  result.flags = result.flags or flags

##  Copy flags, preserving memory attributes of result.
##  ALWAYS_INLINE

proc mpd_copy_flags*(result: ptr mpd_t; a: ptr mpd_t) =
  var aflags: uint8_t = a.flags
  result.flags = result.flags and (MPD_STATIC or MPD_DATAFLAGS)
  result.flags = result.flags or (aflags and not (MPD_STATIC or MPD_DATAFLAGS))

##  Initialize a workcontext from ctx. Set traps, flags and newtrap to 0.

proc mpd_workcontext*(workctx: ptr mpd_context_t; ctx: ptr mpd_context_t) {.inline.} =
  workctx.prec = ctx.prec
  workctx.emax = ctx.emax
  workctx.emin = ctx.emin
  workctx.round = ctx.round
  workctx.traps = 0
  workctx.status = 0
  workctx.newtrap = 0
  workctx.clamp = ctx.clamp
  workctx.allcr = ctx.allcr

## ****************************************************************************
##                   Getting and setting parts of decimals
## ****************************************************************************
##  Flip the sign of a decimal

proc _mpd_negate*(dec: ptr mpd_t) {.inline.} =
  dec.flags = dec.flags xor MPD_NEG

##  Set coefficient to zero

proc mpd_zerocoeff*(result: ptr mpd_t) =
  mpd_minalloc(result)
  result.digits = 1
  result.len = 1
  result.data[0] = 0

##  Set the coefficient to all nines.

proc mpd_qmaxcoeff*(result: ptr mpd_t; ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var
    len: mpd_ssize_t
    r: mpd_ssize_t
  _mpd_idiv_word(addr(len), addr(r), ctx.prec, MPD_RDIGITS)
  len = if (r == 0): len else: len + 1
  if not mpd_qresize(result, len, status):
    return
  result.len = len
  result.digits = ctx.prec
  dec(len)
  if r > 0:
    result.data[dec(len)] = mpd_pow10[r] - 1
  while len >= 0:
    result.data[len] = MPD_RADIX - 1
    dec(len)

##
##  Cut off the most significant digits so that the rest fits in ctx->prec.
##  Cannot fail.
##

proc _mpd_cap*(result: ptr mpd_t; ctx: ptr mpd_context_t) =
  var dummy: uint32_t
  var
    len: mpd_ssize_t
    r: mpd_ssize_t
  if result.len > 0 and result.digits > ctx.prec:
    _mpd_idiv_word(addr(len), addr(r), ctx.prec, MPD_RDIGITS)
    len = if (r == 0): len else: len + 1
    if r != 0:
      result.data[len - 1] = result.data[len - 1] mod mpd_pow10[r]
    len = _mpd_real_size(result.data, len)
    ##  resize to fewer words cannot fail
    mpd_qresize(result, len, addr(dummy))
    result.len = len
    mpd_setdigits(result)
  if mpd_iszero(result):
    _settriple(result, mpd_sign(result), 0, result.exp)

##
##  Cut off the most significant digits of a NaN payload so that the rest
##  fits in ctx->prec - ctx->clamp. Cannot fail.
##

proc _mpd_fix_nan*(result: ptr mpd_t; ctx: ptr mpd_context_t) =
  var dummy: uint32_t
  var prec: mpd_ssize_t
  var
    len: mpd_ssize_t
    r: mpd_ssize_t
  prec = ctx.prec - ctx.clamp
  if result.len > 0 and result.digits > prec:
    if prec == 0:
      mpd_minalloc(result)
      result.len = result.digits = 0
    else:
      _mpd_idiv_word(addr(len), addr(r), prec, MPD_RDIGITS)
      len = if (r == 0): len else: len + 1
      if r != 0:
        result.data[len - 1] = result.data[len - 1] mod mpd_pow10[r]
      len = _mpd_real_size(result.data, len)
      ##  resize to fewer words cannot fail
      mpd_qresize(result, len, addr(dummy))
      result.len = len
      mpd_setdigits(result)
      if mpd_iszerocoeff(result):
        ##  NaN0 is not a valid representation
        result.len = result.digits = 0

##
##  Get n most significant digits from a decimal, where 0 < n <= MPD_UINT_DIGITS.
##  Assumes MPD_UINT_DIGITS == MPD_RDIGITS+1, which is true for 32 and 64 bit
##  machines.
##
##  The result of the operation will be in lo. If the operation is impossible,
##  hi will be nonzero. This is used to indicate an error.
##

proc _mpd_get_msdigits*(hi: ptr mpd_uint_t; lo: ptr mpd_uint_t; dec: ptr mpd_t; n: cuint) {.
    inline.} =
  var
    r: mpd_uint_t
    tmp: mpd_uint_t
  assert(0 < n and n <= MPD_RDIGITS + 1)
  _mpd_div_word(addr(tmp), addr(r), dec.digits, MPD_RDIGITS)
  r = if (r == 0): MPD_RDIGITS else: r
  ##  digits in the most significant word
  hi[] = 0
  lo[] = dec.data[dec.len - 1]
  if n <= r:
    lo[] = lo[] / mpd_pow10[r - n]
  elif dec.len > 1:
    ##  at this point 1 <= r < n <= MPD_RDIGITS+1
    _mpd_mul_words(hi, lo, lo[], mpd_pow10[n - r])
    tmp = dec.data[dec.len - 2] div mpd_pow10[MPD_RDIGITS - (n - r)]
    lo[] = lo[] + tmp
    if lo[] < tmp:
      inc((hi[]))

## ****************************************************************************
##                    Gathering information about a decimal
## ****************************************************************************
##  The real size of the coefficient without leading zero words.

proc _mpd_real_size*(data: ptr mpd_uint_t; size: mpd_ssize_t): mpd_ssize_t {.inline.} =
  while size > 1 and data[size - 1] == 0:
    dec(size)
  return size

##  Return number of trailing zeros. No errors are possible.

proc mpd_trail_zeros*(dec: ptr mpd_t): mpd_ssize_t =
  var word: mpd_uint_t
  var
    i: mpd_ssize_t
    tz: mpd_ssize_t = 0
  i = 0
  while i < dec.len:
    if dec.data[i] != 0:
      word = dec.data[i]
      tz = i * MPD_RDIGITS
      while word mod 10 == 0:
        word = word / 10
        inc(tz)
      break
    inc(i)
  return tz

##  Integer: Undefined for specials

proc _mpd_isint*(dec: ptr mpd_t): cint =
  var tz: mpd_ssize_t
  if mpd_iszerocoeff(dec):
    return 1
  tz = mpd_trail_zeros(dec)
  return dec.exp + tz >= 0

##  Integer

proc mpd_isinteger*(dec: ptr mpd_t): cint =
  if mpd_isspecial(dec):
    return 0
  return _mpd_isint(dec)

##  Word is a power of 10

proc mpd_word_ispow10*(word: mpd_uint_t): cint =
  var n: cint
  n = mpd_word_digits(word)
  if word == mpd_pow10[n - 1]:
    return 1
  return 0

##  Coefficient is a power of 10

proc mpd_coeff_ispow10*(dec: ptr mpd_t): cint =
  if mpd_word_ispow10(mpd_msword(dec)):
    if _mpd_isallzero(dec.data, dec.len - 1):
      return 1
  return 0

##  All digits of a word are nines

proc mpd_word_isallnine*(word: mpd_uint_t): cint =
  var n: cint
  n = mpd_word_digits(word)
  if word == mpd_pow10[n] - 1:
    return 1
  return 0

##  All digits of the coefficient are nines

proc mpd_coeff_isallnine*(dec: ptr mpd_t): cint =
  if mpd_word_isallnine(mpd_msword(dec)):
    if _mpd_isallnine(dec.data, dec.len - 1):
      return 1
  return 0

##  Odd decimal: Undefined for non-integers!

proc mpd_isodd*(dec: ptr mpd_t): cint =
  var
    q: mpd_uint_t
    r: mpd_uint_t
  assert(mpd_isinteger(dec))
  if mpd_iszerocoeff(dec):
    return 0
  if dec.exp < 0:
    _mpd_div_word(addr(q), addr(r), -dec.exp, MPD_RDIGITS)
    q = dec.data[q] div mpd_pow10[r]
    return mpd_isoddword(q)
  return dec.exp == 0 and mpd_isoddword(dec.data[0])

##  Even: Undefined for non-integers!

proc mpd_iseven*(dec: ptr mpd_t): cint =
  return not mpd_isodd(dec)

## ****************************************************************************
##                       Getting and setting decimals
## ****************************************************************************
##  Internal function: Set a static decimal from a triple, no error checking.

proc _ssettriple*(result: ptr mpd_t; sign: uint8_t; a: mpd_uint_t; exp: mpd_ssize_t) =
  mpd_set_flags(result, sign)
  result.exp = exp
  _mpd_div_word(addr(result.data[1]), addr(result.data[0]), a, MPD_RADIX)
  result.len = if (result.data[1] == 0): 1 else: 2
  mpd_setdigits(result)

##  Internal function: Set a decimal from a triple, no error checking.

proc _settriple*(result: ptr mpd_t; sign: uint8_t; a: mpd_uint_t; exp: mpd_ssize_t) =
  mpd_minalloc(result)
  mpd_set_flags(result, sign)
  result.exp = exp
  _mpd_div_word(addr(result.data[1]), addr(result.data[0]), a, MPD_RADIX)
  result.len = if (result.data[1] == 0): 1 else: 2
  mpd_setdigits(result)

##  Set a special number from a triple

proc mpd_setspecial*(result: ptr mpd_t; sign: uint8_t; `type`: uint8_t) =
  mpd_minalloc(result)
  result.flags = result.flags and not (MPD_NEG or MPD_SPECIAL)
  result.flags = result.flags or (sign or `type`)
  result.exp = result.digits = result.len = 0

##  Set result of NaN with an error status

proc mpd_seterror*(result: ptr mpd_t; flags: uint32_t; status: ptr uint32_t) =
  mpd_minalloc(result)
  mpd_set_qnan(result)
  mpd_set_positive(result)
  result.exp = result.digits = result.len = 0
  status[] = status[] or flags

##  quietly set a static decimal from an mpd_ssize_t

proc mpd_qsset_ssize*(result: ptr mpd_t; a: mpd_ssize_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
  var u: mpd_uint_t
  var sign: uint8_t = MPD_POS
  if a < 0:
    if a == MPD_SSIZE_MIN:
      u = cast[mpd_uint_t](MPD_SSIZE_MAX) + (-(MPD_SSIZE_MIN + MPD_SSIZE_MAX))
    else:
      u = -a
    sign = MPD_NEG
  else:
    u = a
  _ssettriple(result, sign, u, 0)
  mpd_qfinalize(result, ctx, status)

##  quietly set a static decimal from an mpd_uint_t

proc mpd_qsset_uint*(result: ptr mpd_t; a: mpd_uint_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
  _ssettriple(result, MPD_POS, a, 0)
  mpd_qfinalize(result, ctx, status)

##  quietly set a static decimal from an int32_t

proc mpd_qsset_i32*(result: ptr mpd_t; a: int32_t; ctx: ptr mpd_context_t;
                   status: ptr uint32_t) =
  mpd_qsset_ssize(result, a, ctx, status)

##  quietly set a static decimal from a uint32_t

proc mpd_qsset_u32*(result: ptr mpd_t; a: uint32_t; ctx: ptr mpd_context_t;
                   status: ptr uint32_t) =
  mpd_qsset_uint(result, a, ctx, status)

when defined(CONFIG_64):
  ##  quietly set a static decimal from an int64_t
  proc mpd_qsset_i64*(result: ptr mpd_t; a: int64_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
    mpd_qsset_ssize(result, a, ctx, status)

  ##  quietly set a static decimal from a uint64_t
  proc mpd_qsset_u64*(result: ptr mpd_t; a: uint64_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
    mpd_qsset_uint(result, a, ctx, status)

##  quietly set a decimal from an mpd_ssize_t

proc mpd_qset_ssize*(result: ptr mpd_t; a: mpd_ssize_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
  mpd_minalloc(result)
  mpd_qsset_ssize(result, a, ctx, status)

##  quietly set a decimal from an mpd_uint_t

proc mpd_qset_uint*(result: ptr mpd_t; a: mpd_uint_t; ctx: ptr mpd_context_t;
                   status: ptr uint32_t) =
  _settriple(result, MPD_POS, a, 0)
  mpd_qfinalize(result, ctx, status)

##  quietly set a decimal from an int32_t

proc mpd_qset_i32*(result: ptr mpd_t; a: int32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qset_ssize(result, a, ctx, status)

##  quietly set a decimal from a uint32_t

proc mpd_qset_u32*(result: ptr mpd_t; a: uint32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qset_uint(result, a, ctx, status)

when defined(CONFIG_32) and not defined(LEGACY_COMPILER):
  ##  set a decimal from a uint64_t
  proc _c32setu64*(result: ptr mpd_t; u: uint64_t; sign: uint8_t; status: ptr uint32_t) =
    var w: array[3, mpd_uint_t]
    var q: uint64_t
    var
      i: cint
      len: cint
    len = 0
    while true:
      q = u div MPD_RADIX
      w[len] = (mpd_uint_t)(u - q * MPD_RADIX)
      u = q
      inc(len)
      if not (u != 0):
        break
    if not mpd_qresize(result, len, status):
      return
    i = 0
    while i < len:
      result.data[i] = w[i]
      inc(i)
    mpd_set_flags(result, sign)
    result.exp = 0
    result.len = len
    mpd_setdigits(result)

  proc _c32_qset_u64*(result: ptr mpd_t; a: uint64_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
    _c32setu64(result, a, MPD_POS, status)
    mpd_qfinalize(result, ctx, status)

  ##  set a decimal from an int64_t
  proc _c32_qset_i64*(result: ptr mpd_t; a: int64_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
    var u: uint64_t
    var sign: uint8_t = MPD_POS
    if a < 0:
      if a == INT64_MIN:
        u = cast[uint64_t](INT64_MAX) + (-(INT64_MIN + INT64_MAX))
      else:
        u = -a
      sign = MPD_NEG
    else:
      u = a
    _c32setu64(result, u, sign, status)
    mpd_qfinalize(result, ctx, status)

when not defined(LEGACY_COMPILER):
  ##  quietly set a decimal from an int64_t
  proc mpd_qset_i64*(result: ptr mpd_t; a: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    when defined(CONFIG_64):
      mpd_qset_ssize(result, a, ctx, status)
    else:
      _c32_qset_i64(result, a, ctx, status)

  ##  quietly set a decimal from an int64_t, use a maxcontext for conversion
  proc mpd_qset_i64_exact*(result: ptr mpd_t; a: int64_t; status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    mpd_maxcontext(addr(maxcontext))
    when defined(CONFIG_64):
      mpd_qset_ssize(result, a, addr(maxcontext), status)
    else:
      _c32_qset_i64(result, a, addr(maxcontext), status)
    if status[] and (MPD_Inexact or MPD_Rounded or MPD_Clamped):
      ##  we want exact results
      mpd_seterror(result, MPD_Invalid_operation, status)
    status[] = status[] and MPD_Errors

  ##  quietly set a decimal from a uint64_t
  proc mpd_qset_u64*(result: ptr mpd_t; a: uint64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    when defined(CONFIG_64):
      mpd_qset_uint(result, a, ctx, status)
    else:
      _c32_qset_u64(result, a, ctx, status)

  ##  quietly set a decimal from a uint64_t, use a maxcontext for conversion
  proc mpd_qset_u64_exact*(result: ptr mpd_t; a: uint64_t; status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    mpd_maxcontext(addr(maxcontext))
    when defined(CONFIG_64):
      mpd_qset_uint(result, a, addr(maxcontext), status)
    else:
      _c32_qset_u64(result, a, addr(maxcontext), status)
    if status[] and (MPD_Inexact or MPD_Rounded or MPD_Clamped):
      ##  we want exact results
      mpd_seterror(result, MPD_Invalid_operation, status)
    status[] = status[] and MPD_Errors

##
##  Quietly get an mpd_uint_t from a decimal. Assumes
##  MPD_UINT_DIGITS == MPD_RDIGITS+1, which is true for
##  32 and 64 bit machines.
##
##  If the operation is impossible, MPD_Invalid_operation is set.
##

proc _mpd_qget_uint*(use_sign: cint; a: ptr mpd_t; status: ptr uint32_t): mpd_uint_t =
  var tmp: mpd_t
  var tmp_data: array[2, mpd_uint_t]
  var
    lo: mpd_uint_t
    hi: mpd_uint_t
  if mpd_isspecial(a):
    status[] = status[] or MPD_Invalid_operation
    return MPD_UINT_MAX
  if mpd_iszero(a):
    return 0
  if use_sign and mpd_isnegative(a):
    status[] = status[] or MPD_Invalid_operation
    return MPD_UINT_MAX
  if a.digits + a.exp > MPD_RDIGITS + 1:
    status[] = status[] or MPD_Invalid_operation
    return MPD_UINT_MAX
  if a.exp < 0:
    if not _mpd_isint(a):
      status[] = status[] or MPD_Invalid_operation
      return MPD_UINT_MAX
    tmp.data = tmp_data
    tmp.flags = MPD_STATIC or MPD_STATIC_DATA
    tmp.alloc = 2
    mpd_qsshiftr(addr(tmp), a, -a.exp)
    tmp.exp = 0
    a = addr(tmp)
  _mpd_get_msdigits(addr(hi), addr(lo), a, MPD_RDIGITS + 1)
  if hi:
    status[] = status[] or MPD_Invalid_operation
    return MPD_UINT_MAX
  if a.exp > 0:
    _mpd_mul_words(addr(hi), addr(lo), lo, mpd_pow10[a.exp])
    if hi:
      status[] = status[] or MPD_Invalid_operation
      return MPD_UINT_MAX
  return lo

##
##  Sets Invalid_operation for:
##    - specials
##    - negative numbers (except negative zero)
##    - non-integers
##    - overflow
##

proc mpd_qget_uint*(a: ptr mpd_t; status: ptr uint32_t): mpd_uint_t =
  return _mpd_qget_uint(1, a, status)

##  Same as above, but gets the absolute value, i.e. the sign is ignored.

proc mpd_qabs_uint*(a: ptr mpd_t; status: ptr uint32_t): mpd_uint_t =
  return _mpd_qget_uint(0, a, status)

##  quietly get an mpd_ssize_t from a decimal

proc mpd_qget_ssize*(a: ptr mpd_t; status: ptr uint32_t): mpd_ssize_t =
  var workstatus: uint32_t = 0
  var u: mpd_uint_t
  var isneg: cint
  u = mpd_qabs_uint(a, addr(workstatus))
  if workstatus and MPD_Invalid_operation:
    status[] = status[] or workstatus
    return MPD_SSIZE_MAX
  isneg = mpd_isnegative(a)
  if u <= MPD_SSIZE_MAX:
    return if isneg: -(cast[mpd_ssize_t](u)) else: cast[mpd_ssize_t](u)
  elif isneg and u + (MPD_SSIZE_MIN + MPD_SSIZE_MAX) == MPD_SSIZE_MAX:
    return MPD_SSIZE_MIN
  status[] = status[] or MPD_Invalid_operation
  return MPD_SSIZE_MAX

when defined(CONFIG_32) and not defined(LEGACY_COMPILER):
  ##
  ##  Quietly get a uint64_t from a decimal. If the operation is impossible,
  ##  MPD_Invalid_operation is set.
  ##
  proc _c32_qget_u64*(use_sign: cint; a: ptr mpd_t; status: ptr uint32_t): uint64_t =
    MPD_NEW_STATIC(tmp, 0, 0, 20, 3)
    var maxcontext: mpd_context_t
    var ret: uint64_t
    tmp_data[0] = 709551615
    tmp_data[1] = 446744073
    tmp_data[2] = 18
    if mpd_isspecial(a):
      status[] = status[] or MPD_Invalid_operation
      return UINT64_MAX
    if mpd_iszero(a):
      return 0
    if use_sign and mpd_isnegative(a):
      status[] = status[] or MPD_Invalid_operation
      return UINT64_MAX
    if not _mpd_isint(a):
      status[] = status[] or MPD_Invalid_operation
      return UINT64_MAX
    if _mpd_cmp_abs(a, addr(tmp)) > 0:
      status[] = status[] or MPD_Invalid_operation
      return UINT64_MAX
    mpd_maxcontext(addr(maxcontext))
    mpd_qrescale(addr(tmp), a, 0, addr(maxcontext), addr(maxcontext.status))
    maxcontext.status = maxcontext.status and not MPD_Rounded
    if maxcontext.status != 0:
      status[] = status[] or (maxcontext.status or MPD_Invalid_operation)
      ##  GCOV_NOT_REACHED
      return UINT64_MAX
      ##  GCOV_NOT_REACHED
    ret = 0
    case tmp.len
    of 3:
      inc(ret, cast[uint64_t](tmp_data[2] * 1000000000000000000'i64))
    of 2:
      inc(ret, cast[uint64_t](tmp_data[1] * 1000000000))
    of 1:
      inc(ret, tmp_data[0])
    else:
      abort()
      ##  GCOV_NOT_REACHED
    return ret

  proc _c32_qget_i64*(a: ptr mpd_t; status: ptr uint32_t): int64_t =
    var u: uint64_t
    var isneg: cint
    u = _c32_qget_u64(0, a, status)
    if status[] and MPD_Invalid_operation:
      return INT64_MAX
    isneg = mpd_isnegative(a)
    if u <= INT64_MAX:
      return if isneg: -(cast[int64_t](u)) else: cast[int64_t](u)
    elif isneg and u + (INT64_MIN + INT64_MAX) == INT64_MAX:
      return INT64_MIN
    status[] = status[] or MPD_Invalid_operation
    return INT64_MAX

when defined(CONFIG_64):
  ##  quietly get a uint64_t from a decimal
  proc mpd_qget_u64*(a: ptr mpd_t; status: ptr uint32_t): uint64_t =
    return mpd_qget_uint(a, status)

  ##  quietly get an int64_t from a decimal
  proc mpd_qget_i64*(a: ptr mpd_t; status: ptr uint32_t): int64_t =
    return mpd_qget_ssize(a, status)

  ##  quietly get a uint32_t from a decimal
  proc mpd_qget_u32*(a: ptr mpd_t; status: ptr uint32_t): uint32_t =
    var workstatus: uint32_t = 0
    var x: uint64_t = mpd_qget_uint(a, addr(workstatus))
    if workstatus and MPD_Invalid_operation:
      status[] = status[] or workstatus
      return UINT32_MAX
    if x > UINT32_MAX:
      status[] = status[] or MPD_Invalid_operation
      return UINT32_MAX
    return cast[uint32_t](x)

  ##  quietly get an int32_t from a decimal
  proc mpd_qget_i32*(a: ptr mpd_t; status: ptr uint32_t): int32_t =
    var workstatus: uint32_t = 0
    var x: int64_t = mpd_qget_ssize(a, addr(workstatus))
    if workstatus and MPD_Invalid_operation:
      status[] = status[] or workstatus
      return INT32_MAX
    if x < INT32_MIN or x > INT32_MAX:
      status[] = status[] or MPD_Invalid_operation
      return INT32_MAX
    return cast[int32_t](x)

else:
  when not defined(LEGACY_COMPILER):
    ##  quietly get a uint64_t from a decimal
    proc mpd_qget_u64*(a: ptr mpd_t; status: ptr uint32_t): uint64_t =
      var workstatus: uint32_t = 0
      var x: uint64_t = _c32_qget_u64(1, a, addr(workstatus))
      status[] = status[] or workstatus
      return x

    ##  quietly get an int64_t from a decimal
    proc mpd_qget_i64*(a: ptr mpd_t; status: ptr uint32_t): int64_t =
      var workstatus: uint32_t = 0
      var x: int64_t = _c32_qget_i64(a, addr(workstatus))
      status[] = status[] or workstatus
      return x

  ##  quietly get a uint32_t from a decimal
  proc mpd_qget_u32*(a: ptr mpd_t; status: ptr uint32_t): uint32_t =
    return mpd_qget_uint(a, status)

  ##  quietly get an int32_t from a decimal
  proc mpd_qget_i32*(a: ptr mpd_t; status: ptr uint32_t): int32_t =
    return mpd_qget_ssize(a, status)

## ****************************************************************************
##          Filtering input of functions, finalizing output of functions
## ****************************************************************************
##
##  Check if the operand is NaN, copy to result and return 1 if this is
##  the case. Copying can fail since NaNs are allowed to have a payload that
##  does not fit in MPD_MINALLOC.
##

proc mpd_qcheck_nan*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t): cint =
  if mpd_isnan(a):
    status[] = status[] or if mpd_issnan(a): MPD_Invalid_operation else: 0
    mpd_qcopy(result, a, status)
    mpd_set_qnan(result)
    _mpd_fix_nan(result, ctx)
    return 1
  return 0

##
##  Check if either operand is NaN, copy to result and return 1 if this
##  is the case. Copying can fail since NaNs are allowed to have a payload
##  that does not fit in MPD_MINALLOC.
##

proc mpd_qcheck_nans*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                     ctx: ptr mpd_context_t; status: ptr uint32_t): cint =
  if (a.flags or b.flags) and (MPD_NAN or MPD_SNAN):
    var choice: ptr mpd_t = b
    if mpd_issnan(a):
      choice = a
      status[] = status[] or MPD_Invalid_operation
    elif mpd_issnan(b):
      status[] = status[] or MPD_Invalid_operation
    elif mpd_isqnan(a):
      choice = a
    mpd_qcopy(result, choice, status)
    mpd_set_qnan(result)
    _mpd_fix_nan(result, ctx)
    return 1
  return 0

##
##  Check if one of the operands is NaN, copy to result and return 1 if this
##  is the case. Copying can fail since NaNs are allowed to have a payload
##  that does not fit in MPD_MINALLOC.
##

proc mpd_qcheck_3nans*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; c: ptr mpd_t;
                      ctx: ptr mpd_context_t; status: ptr uint32_t): cint =
  if (a.flags or b.flags or c.flags) and (MPD_NAN or MPD_SNAN):
    var choice: ptr mpd_t = c
    if mpd_issnan(a):
      choice = a
      status[] = status[] or MPD_Invalid_operation
    elif mpd_issnan(b):
      choice = b
      status[] = status[] or MPD_Invalid_operation
    elif mpd_issnan(c):
      status[] = status[] or MPD_Invalid_operation
    elif mpd_isqnan(a):
      choice = a
    elif mpd_isqnan(b):
      choice = b
    mpd_qcopy(result, choice, status)
    mpd_set_qnan(result)
    _mpd_fix_nan(result, ctx)
    return 1
  return 0

##  Check if rounding digit 'rnd' leads to an increment.

proc _mpd_rnd_incr*(dec: ptr mpd_t; rnd: mpd_uint_t; ctx: ptr mpd_context_t): cint {.
    inline.} =
  var ld: cint
  case ctx.round
  of MPD_ROUND_DOWN, MPD_ROUND_TRUNC:
    return 0
  of MPD_ROUND_HALF_UP:
    return rnd >= 5
  of MPD_ROUND_HALF_EVEN:
    return (rnd > 5) or ((rnd == 5) and mpd_isoddcoeff(dec))
  of MPD_ROUND_CEILING:
    return not (rnd == 0 or mpd_isnegative(dec))
  of MPD_ROUND_FLOOR:
    return not (rnd == 0 or mpd_ispositive(dec))
  of MPD_ROUND_HALF_DOWN:
    return rnd > 5
  of MPD_ROUND_UP:
    return not (rnd == 0)
  of MPD_ROUND_05UP:
    ld = cast[cint](mpd_lsd(dec.data[0]))
    return not (rnd == 0) and (ld == 0 or ld == 5)
  else:
    nil
  return 0
  ##  GCOV_NOT_REACHED

##
##  Apply rounding to a decimal that has been right-shifted into a full
##  precision decimal. If an increment leads to an overflow of the precision,
##  adjust the coefficient and the exponent and check the new exponent for
##  overflow.
##

proc _mpd_apply_round*(dec: ptr mpd_t; rnd: mpd_uint_t; ctx: ptr mpd_context_t;
                      status: ptr uint32_t) {.inline.} =
  if _mpd_rnd_incr(dec, rnd, ctx):
    ##  We have a number with exactly ctx->prec digits. The increment
    ##  can only lead to an overflow if the decimal is all nines. In
    ##  that case, the result is a power of ten with prec+1 digits.
    ##
    ##  If the precision is a multiple of MPD_RDIGITS, this situation is
    ##  detected by _mpd_baseincr returning a carry.
    ##  If the precision is not a multiple of MPD_RDIGITS, we have to
    ##  check if the result has one digit too many.
    ##
    var carry: mpd_uint_t = _mpd_baseincr(dec.data, dec.len)
    if carry:
      dec.data[dec.len - 1] = mpd_pow10[MPD_RDIGITS - 1]
      inc(dec.exp, 1)
      _mpd_check_exp(dec, ctx, status)
      return
    mpd_setdigits(dec)
    if dec.digits > ctx.prec:
      mpd_qshiftr_inplace(dec, 1)
      inc(dec.exp, 1)
      dec.digits = ctx.prec
      _mpd_check_exp(dec, ctx, status)

##
##  Apply rounding to a decimal. Allow overflow of the precision.
##

proc _mpd_apply_round_excess*(dec: ptr mpd_t; rnd: mpd_uint_t; ctx: ptr mpd_context_t;
                             status: ptr uint32_t) {.inline.} =
  if _mpd_rnd_incr(dec, rnd, ctx):
    var carry: mpd_uint_t = _mpd_baseincr(dec.data, dec.len)
    if carry:
      if not mpd_qresize(dec, dec.len + 1, status):
        return
      dec.data[dec.len] = 1
      inc(dec.len, 1)
    mpd_setdigits(dec)

##
##  Apply rounding to a decimal that has been right-shifted into a decimal
##  with full precision or less. Return failure if an increment would
##  overflow the precision.
##

proc _mpd_apply_round_fit*(dec: ptr mpd_t; rnd: mpd_uint_t; ctx: ptr mpd_context_t;
                          status: ptr uint32_t): cint {.inline.} =
  if _mpd_rnd_incr(dec, rnd, ctx):
    var carry: mpd_uint_t = _mpd_baseincr(dec.data, dec.len)
    if carry:
      if not mpd_qresize(dec, dec.len + 1, status):
        return 0
      dec.data[dec.len] = 1
      inc(dec.len, 1)
    mpd_setdigits(dec)
    if dec.digits > ctx.prec:
      mpd_seterror(dec, MPD_Invalid_operation, status)
      return 0
  return 1

##  Check a normal number for overflow, underflow, clamping. If the operand
##    is modified, it will be zero, special or (sub)normal with a coefficient
##    that fits into the current context precision.

proc _mpd_check_exp*(dec: ptr mpd_t; ctx: ptr mpd_context_t; status: ptr uint32_t) {.
    inline.} =
  var
    adjexp: mpd_ssize_t
    etiny: mpd_ssize_t
    shift: mpd_ssize_t
  var rnd: cint
  adjexp = mpd_adjexp(dec)
  if adjexp > ctx.emax:
    if mpd_iszerocoeff(dec):
      dec.exp = ctx.emax
      if ctx.clamp:
        dec(dec.exp, (ctx.prec - 1))
      mpd_zerocoeff(dec)
      status[] = status[] or MPD_Clamped
      return
    case ctx.round
    of MPD_ROUND_HALF_UP, MPD_ROUND_HALF_EVEN, MPD_ROUND_HALF_DOWN, MPD_ROUND_UP,
      MPD_ROUND_TRUNC:
      mpd_setspecial(dec, mpd_sign(dec), MPD_INF)
    of MPD_ROUND_DOWN, MPD_ROUND_05UP:
      mpd_qmaxcoeff(dec, ctx, status)
      dec.exp = ctx.emax - ctx.prec + 1
    of MPD_ROUND_CEILING:
      if mpd_isnegative(dec):
        mpd_qmaxcoeff(dec, ctx, status)
        dec.exp = ctx.emax - ctx.prec + 1
      else:
        mpd_setspecial(dec, MPD_POS, MPD_INF)
    of MPD_ROUND_FLOOR:
      if mpd_ispositive(dec):
        mpd_qmaxcoeff(dec, ctx, status)
        dec.exp = ctx.emax - ctx.prec + 1
      else:
        mpd_setspecial(dec, MPD_NEG, MPD_INF)
    else:                     ##  debug
      abort()
      ##  GCOV_NOT_REACHED
    status[] = status[] or (MPD_Overflow or MPD_Inexact or MPD_Rounded)
  elif ctx.clamp and dec.exp > mpd_etop(ctx):
    ##  At this point adjexp=exp+digits-1 <= emax and exp > etop=emax-prec+1:
    ##    (1) shift = exp -emax+prec-1 > 0
    ##    (2) digits+shift = exp+digits-1 - emax + prec <= prec
    shift = dec.exp - mpd_etop(ctx)
    if not mpd_qshiftl(dec, dec, shift, status):
      return
    dec(dec.exp, shift)
    status[] = status[] or MPD_Clamped
    if not mpd_iszerocoeff(dec) and adjexp < ctx.emin:
      ##  Underflow is impossible, since exp < etiny=emin-prec+1
      ##  and exp > etop=emax-prec+1 would imply emax < emin.
      status[] = status[] or MPD_Subnormal
  elif adjexp < ctx.emin:
    etiny = mpd_etiny(ctx)
    if mpd_iszerocoeff(dec):
      if dec.exp < etiny:
        dec.exp = etiny
        mpd_zerocoeff(dec)
        status[] = status[] or MPD_Clamped
      return
    status[] = status[] or MPD_Subnormal
    if dec.exp < etiny:
      ##  At this point adjexp=exp+digits-1 < emin and exp < etiny=emin-prec+1:
      ##    (1) shift = emin-prec+1 - exp > 0
      ##    (2) digits-shift = exp+digits-1 - emin + prec < prec
      shift = etiny - dec.exp
      rnd = cast[cint](mpd_qshiftr_inplace(dec, shift))
      dec.exp = etiny
      ##  We always have a spare digit in case of an increment.
      _mpd_apply_round_excess(dec, rnd, ctx, status)
      status[] = status[] or MPD_Rounded
      if rnd:
        status[] = status[] or (MPD_Inexact or MPD_Underflow)
        if mpd_iszerocoeff(dec):
          mpd_zerocoeff(dec)
          status[] = status[] or MPD_Clamped

##  Transcendental functions do not always set Underflow reliably,
##  since they only use as much precision as is necessary for correct
##  rounding. If a result like 1.0000000000e-101 is finalized, there
##  is no rounding digit that would trigger Underflow. But we can
##  assume Inexact, so a short check suffices.

proc mpd_check_underflow*(dec: ptr mpd_t; ctx: ptr mpd_context_t; status: ptr uint32_t) {.
    inline.} =
  if mpd_adjexp(dec) < ctx.emin and not mpd_iszero(dec) and dec.exp < mpd_etiny(ctx):
    status[] = status[] or MPD_Underflow

##  Check if a normal number must be rounded after the exponent has been checked.

proc _mpd_check_round*(dec: ptr mpd_t; ctx: ptr mpd_context_t; status: ptr uint32_t) {.
    inline.} =
  var rnd: mpd_uint_t
  var shift: mpd_ssize_t
  ##  must handle specials: _mpd_check_exp() can produce infinities or NaNs
  if mpd_isspecial(dec):
    return
  if dec.digits > ctx.prec:
    shift = dec.digits - ctx.prec
    rnd = mpd_qshiftr_inplace(dec, shift)
    inc(dec.exp, shift)
    _mpd_apply_round(dec, rnd, ctx, status)
    status[] = status[] or MPD_Rounded
    if rnd:
      status[] = status[] or MPD_Inexact

##  Finalize all operations.

proc mpd_qfinalize*(result: ptr mpd_t; ctx: ptr mpd_context_t; status: ptr uint32_t) =
  if mpd_isspecial(result):
    if mpd_isnan(result):
      _mpd_fix_nan(result, ctx)
    return
  _mpd_check_exp(result, ctx, status)
  _mpd_check_round(result, ctx, status)

## ****************************************************************************
##                                  Copying
## ****************************************************************************
##  Internal function: Copy a decimal, share data with src: USE WITH CARE!

proc _mpd_copy_shared*(dest: ptr mpd_t; src: ptr mpd_t) {.inline.} =
  dest.flags = src.flags
  dest.exp = src.exp
  dest.digits = src.digits
  dest.len = src.len
  dest.alloc = src.alloc
  dest.data = src.data
  mpd_set_shared_data(dest)

##
##  Copy a decimal. In case of an error, status is set to MPD_Malloc_error.
##

proc mpd_qcopy*(result: ptr mpd_t; a: ptr mpd_t; status: ptr uint32_t): cint =
  if result == a:
    return 1
  if not mpd_qresize(result, a.len, status):
    return 0
  mpd_copy_flags(result, a)
  result.exp = a.exp
  result.digits = a.digits
  result.len = a.len
  memcpy(result.data, a.data, a.len * (sizeof(result.data[])))
  return 1

##  Same as mpd_qcopy, but do not set the result to NaN on failure.

proc mpd_qcopy_cxx*(result: ptr mpd_t; a: ptr mpd_t): cint =
  if result == a:
    return 1
  if not mpd_qresize_cxx(result, a.len):
    return 0
  mpd_copy_flags(result, a)
  result.exp = a.exp
  result.digits = a.digits
  result.len = a.len
  memcpy(result.data, a.data, a.len * (sizeof(result.data[])))
  return 1

##
##  Copy to a decimal with a static buffer. The caller has to make sure that
##  the buffer is big enough. Cannot fail.
##

proc mpd_qcopy_static*(result: ptr mpd_t; a: ptr mpd_t) =
  if result == a:
    return
  memcpy(result.data, a.data, a.len * (sizeof(result.data[])))
  mpd_copy_flags(result, a)
  result.exp = a.exp
  result.digits = a.digits
  result.len = a.len

##
##  Return a newly allocated copy of the operand. In case of an error,
##  status is set to MPD_Malloc_error and the return value is NULL.
##

proc mpd_qncopy*(a: ptr mpd_t): ptr mpd_t =
  var result: ptr mpd_t
  if (result = mpd_qnew_size(a.len)) == nil:
    return nil
  memcpy(result.data, a.data, a.len * (sizeof(result.data[])))
  mpd_copy_flags(result, a)
  result.exp = a.exp
  result.digits = a.digits
  result.len = a.len
  return result

##
##  Copy a decimal and set the sign to positive. In case of an error, the
##  status is set to MPD_Malloc_error.
##

proc mpd_qcopy_abs*(result: ptr mpd_t; a: ptr mpd_t; status: ptr uint32_t): cint =
  if not mpd_qcopy(result, a, status):
    return 0
  mpd_set_positive(result)
  return 1

##
##  Copy a decimal and negate the sign. In case of an error, the
##  status is set to MPD_Malloc_error.
##

proc mpd_qcopy_negate*(result: ptr mpd_t; a: ptr mpd_t; status: ptr uint32_t): cint =
  if not mpd_qcopy(result, a, status):
    return 0
  _mpd_negate(result)
  return 1

##
##  Copy a decimal, setting the sign of the first operand to the sign of the
##  second operand. In case of an error, the status is set to MPD_Malloc_error.
##

proc mpd_qcopy_sign*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; status: ptr uint32_t): cint =
  var sign_b: uint8_t = mpd_sign(b)
  ##  result may equal b!
  if not mpd_qcopy(result, a, status):
    return 0
  mpd_set_sign(result, sign_b)
  return 1

## ****************************************************************************
##                                 Comparisons
## ****************************************************************************
##
##  For all functions that compare two operands and return an int the usual
##  convention applies to the return value:
##
##  -1 if op1 < op2
##   0 if op1 == op2
##   1 if op1 > op2
##
##   INT_MAX for error
##
##  Convenience macro. If a and b are not equal, return from the calling
##  function with the correct comparison value.

template CMP_EQUAL_OR_RETURN*(a, b: untyped): void =
  if a != b:
    if a < b:
      return -1
    return 1

##
##  Compare the data of big and small. This function does the equivalent
##  of first shifting small to the left and then comparing the data of
##  big and small, except that no allocation for the left shift is needed.
##

proc _mpd_basecmp*(big: ptr mpd_uint_t; small: ptr mpd_uint_t; n: mpd_size_t;
                  m: mpd_size_t; shift: mpd_size_t): cint =
  when defined(__GNUC__) and not defined(__INTEL_COMPILER) and
      not defined(__clang__):
    ##  spurious uninitialized warnings
    var
      l: mpd_uint_t = l
      lprev: mpd_uint_t = lprev
      h: mpd_uint_t = h
  else:
    var
      l: mpd_uint_t
      lprev: mpd_uint_t
      h: mpd_uint_t
  var
    q: mpd_uint_t
    r: mpd_uint_t
  var
    ph: mpd_uint_t
    x: mpd_uint_t
  assert(m > 0 and n >= m and shift > 0)
  _mpd_div_word(addr(q), addr(r), cast[mpd_uint_t](shift), MPD_RDIGITS)
  if r != 0:
    ph = mpd_pow10[r]
    dec(m)
    dec(n)
    _mpd_divmod_pow10(addr(h), addr(lprev), small[dec(m)], MPD_RDIGITS - r)
    if h != 0:
      CMP_EQUAL_OR_RETURN(big[n], h)
      dec(n)
    while m != MPD_SIZE_MAX:
      _mpd_divmod_pow10(addr(h), addr(l), small[m], MPD_RDIGITS - r)
      x = ph * lprev + h
      CMP_EQUAL_OR_RETURN(big[n], x)
      lprev = l
      dec(m)
      dec(n)
    x = ph * lprev
    CMP_EQUAL_OR_RETURN(big[q], x)
  else:
    while dec(m) != MPD_SIZE_MAX:
      CMP_EQUAL_OR_RETURN(big[m + q], small[m])
  return not _mpd_isallzero(big, q)

##  Compare two decimals with the same adjusted exponent.

proc _mpd_cmp_same_adjexp*(a: ptr mpd_t; b: ptr mpd_t): cint =
  var
    shift: mpd_ssize_t
    i: mpd_ssize_t
  if a.exp != b.exp:
    ##  Cannot wrap: a->exp + a->digits = b->exp + b->digits, so
    ##  a->exp - b->exp = b->digits - a->digits.
    shift = a.exp - b.exp
    if shift > 0:
      return -(1 * _mpd_basecmp(b.data, a.data, b.len, a.len, shift))
    else:
      return _mpd_basecmp(a.data, b.data, a.len, b.len, -shift)
  i = a.len - 1
  while i >= 0:
    CMP_EQUAL_OR_RETURN(a.data[i], b.data[i])
    dec(i)
  return 0

##  Compare two numerical values.

proc _mpd_cmp*(a: ptr mpd_t; b: ptr mpd_t): cint =
  var
    adjexp_a: mpd_ssize_t
    adjexp_b: mpd_ssize_t
  ##  equal pointers
  if a == b:
    return 0
  if mpd_isinfinite(a):
    if mpd_isinfinite(b):
      return mpd_isnegative(b) - mpd_isnegative(a)
    return mpd_arith_sign(a)
  if mpd_isinfinite(b):
    return -mpd_arith_sign(b)
  if mpd_iszerocoeff(a):
    if mpd_iszerocoeff(b):
      return 0
    return -mpd_arith_sign(b)
  if mpd_iszerocoeff(b):
    return mpd_arith_sign(a)
  if mpd_sign(a) != mpd_sign(b):
    return mpd_sign(b) - mpd_sign(a)
  adjexp_a = mpd_adjexp(a)
  adjexp_b = mpd_adjexp(b)
  if adjexp_a != adjexp_b:
    if adjexp_a < adjexp_b:
      return -(1 * mpd_arith_sign(a))
    return mpd_arith_sign(a)
  return _mpd_cmp_same_adjexp(a, b) * mpd_arith_sign(a)

##  Compare the absolutes of two numerical values.

proc _mpd_cmp_abs*(a: ptr mpd_t; b: ptr mpd_t): cint =
  var
    adjexp_a: mpd_ssize_t
    adjexp_b: mpd_ssize_t
  ##  equal pointers
  if a == b:
    return 0
  if mpd_isinfinite(a):
    if mpd_isinfinite(b):
      return 0
    return 1
  if mpd_isinfinite(b):
    return -1
  if mpd_iszerocoeff(a):
    if mpd_iszerocoeff(b):
      return 0
    return -1
  if mpd_iszerocoeff(b):
    return 1
  adjexp_a = mpd_adjexp(a)
  adjexp_b = mpd_adjexp(b)
  if adjexp_a != adjexp_b:
    if adjexp_a < adjexp_b:
      return -1
    return 1
  return _mpd_cmp_same_adjexp(a, b)

##  Compare two values and return an integer result.

proc mpd_qcmp*(a: ptr mpd_t; b: ptr mpd_t; status: ptr uint32_t): cint =
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_isnan(a) or mpd_isnan(b):
      status[] = status[] or MPD_Invalid_operation
      return INT_MAX
  return _mpd_cmp(a, b)

##
##  Compare a and b, convert the usual integer result to a decimal and
##  store it in 'result'. For convenience, the integer result of the comparison
##  is returned. Comparisons involving NaNs return NaN/INT_MAX.
##

proc mpd_qcompare*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t): cint =
  var c: cint
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      return INT_MAX
  c = _mpd_cmp(a, b)
  _settriple(result, (c < 0), (c != 0), 0)
  return c

##  Same as mpd_compare(), but signal for all NaNs, i.e. also for quiet NaNs.

proc mpd_qcompare_signal*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                         ctx: ptr mpd_context_t; status: ptr uint32_t): cint =
  var c: cint
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      status[] = status[] or MPD_Invalid_operation
      return INT_MAX
  c = _mpd_cmp(a, b)
  _settriple(result, (c < 0), (c != 0), 0)
  return c

##  Compare the operands using a total order.

proc mpd_cmp_total*(a: ptr mpd_t; b: ptr mpd_t): cint =
  var
    aa: mpd_t
    bb: mpd_t
  var
    nan_a: cint
    nan_b: cint
  var c: cint
  if mpd_sign(a) != mpd_sign(b):
    return mpd_sign(b) - mpd_sign(a)
  if mpd_isnan(a):
    c = 1
    if mpd_isnan(b):
      nan_a = if (mpd_isqnan(a)): 1 else: 0
      nan_b = if (mpd_isqnan(b)): 1 else: 0
      if nan_b == nan_a:
        if a.len > 0 and b.len > 0:
          _mpd_copy_shared(addr(aa), a)
          _mpd_copy_shared(addr(bb), b)
          aa.exp = bb.exp = 0
          ##  compare payload
          c = _mpd_cmp_abs(addr(aa), addr(bb))
        else:
          c = (a.len > 0) - (b.len > 0)
      else:
        c = nan_a - nan_b
  elif mpd_isnan(b):
    c = -1
  else:
    c = _mpd_cmp_abs(a, b)
    if c == 0 and a.exp != b.exp:
      c = if (a.exp < b.exp): -1 else: 1
  return c * mpd_arith_sign(a)

##
##  Compare a and b according to a total order, convert the usual integer result
##  to a decimal and store it in 'result'. For convenience, the integer result
##  of the comparison is returned.
##

proc mpd_compare_total*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t): cint =
  var c: cint
  c = mpd_cmp_total(a, b)
  _settriple(result, (c < 0), (c != 0), 0)
  return c

##  Compare the magnitude of the operands using a total order.

proc mpd_cmp_total_mag*(a: ptr mpd_t; b: ptr mpd_t): cint =
  var
    aa: mpd_t
    bb: mpd_t
  _mpd_copy_shared(addr(aa), a)
  _mpd_copy_shared(addr(bb), b)
  mpd_set_positive(addr(aa))
  mpd_set_positive(addr(bb))
  return mpd_cmp_total(addr(aa), addr(bb))

##
##  Compare the magnitude of a and b according to a total order, convert the
##  the usual integer result to a decimal and store it in 'result'.
##  For convenience, the integer result of the comparison is returned.
##

proc mpd_compare_total_mag*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t): cint =
  var c: cint
  c = mpd_cmp_total_mag(a, b)
  _settriple(result, (c < 0), (c != 0), 0)
  return c

##  Determine an ordering for operands that are numerically equal.

proc _mpd_cmp_numequal*(a: ptr mpd_t; b: ptr mpd_t): cint {.inline.} =
  var
    sign_a: cint
    sign_b: cint
  var c: cint
  sign_a = mpd_sign(a)
  sign_b = mpd_sign(b)
  if sign_a != sign_b:
    c = sign_b - sign_a
  else:
    c = if (a.exp < b.exp): -1 else: 1
    c = c * mpd_arith_sign(a)
  return c

## ****************************************************************************
##                          Shifting the coefficient
## ****************************************************************************
##
##  Shift the coefficient of the operand to the left, no check for specials.
##  Both operands may be the same pointer. If the result length has to be
##  increased, mpd_qresize() might fail with MPD_Malloc_error.
##

proc mpd_qshiftl*(result: ptr mpd_t; a: ptr mpd_t; n: mpd_ssize_t; status: ptr uint32_t): cint =
  var size: mpd_ssize_t
  assert(not mpd_isspecial(a))
  assert(n >= 0)
  if mpd_iszerocoeff(a) or n == 0:
    return mpd_qcopy(result, a, status)
  size = mpd_digits_to_size(a.digits + n)
  if not mpd_qresize(result, size, status):
    return 0
    ##  result is NaN
  _mpd_baseshiftl(result.data, a.data, size, a.len, n)
  mpd_copy_flags(result, a)
  result.exp = a.exp
  result.digits = a.digits + n
  result.len = size
  return 1

##  Determine the rounding indicator if all digits of the coefficient are shifted
##  out of the picture.

proc _mpd_get_rnd*(data: ptr mpd_uint_t; len: mpd_ssize_t; use_msd: cint): mpd_uint_t =
  var
    rnd: mpd_uint_t = 0
    rest: mpd_uint_t = 0
    word: mpd_uint_t
  word = data[len - 1]
  ##  special treatment for the most significant digit if shift == digits
  if use_msd:
    _mpd_divmod_pow10(addr(rnd), addr(rest), word, mpd_word_digits(word) - 1)
    if len > 1 and rest == 0:
      rest = not _mpd_isallzero(data, len - 1)
  else:
    rest = not _mpd_isallzero(data, len)
  return if (rnd == 0 or rnd == 5): rnd + not not rest else: rnd

##
##  Same as mpd_qshiftr(), but 'result' is an mpd_t with a static coefficient.
##  It is the caller's responsibility to ensure that the coefficient is big
##  enough. The function cannot fail.
##

proc mpd_qsshiftr*(result: ptr mpd_t; a: ptr mpd_t; n: mpd_ssize_t): mpd_uint_t =
  var rnd: mpd_uint_t
  var size: mpd_ssize_t
  assert(not mpd_isspecial(a))
  assert(n >= 0)
  if mpd_iszerocoeff(a) or n == 0:
    mpd_qcopy_static(result, a)
    return 0
  if n >= a.digits:
    rnd = _mpd_get_rnd(a.data, a.len, (n == a.digits))
    mpd_zerocoeff(result)
  else:
    result.digits = a.digits - n
    size = mpd_digits_to_size(result.digits)
    rnd = _mpd_baseshiftr(result.data, a.data, a.len, n)
    result.len = size
  mpd_copy_flags(result, a)
  result.exp = a.exp
  return rnd

##
##  Inplace shift of the coefficient to the right, no check for specials.
##  Returns the rounding indicator for mpd_rnd_incr().
##  The function cannot fail.
##

proc mpd_qshiftr_inplace*(result: ptr mpd_t; n: mpd_ssize_t): mpd_uint_t =
  var dummy: uint32_t
  var rnd: mpd_uint_t
  var size: mpd_ssize_t
  assert(not mpd_isspecial(result))
  assert(n >= 0)
  if mpd_iszerocoeff(result) or n == 0:
    return 0
  if n >= result.digits:
    rnd = _mpd_get_rnd(result.data, result.len, (n == result.digits))
    mpd_zerocoeff(result)
  else:
    rnd = _mpd_baseshiftr(result.data, result.data, result.len, n)
    dec(result.digits, n)
    size = mpd_digits_to_size(result.digits)
    ##  reducing the size cannot fail
    mpd_qresize(result, size, addr(dummy))
    result.len = size
  return rnd

##
##  Shift the coefficient of the operand to the right, no check for specials.
##  Both operands may be the same pointer. Returns the rounding indicator to
##  be used by mpd_rnd_incr(). If the result length has to be increased,
##  mpd_qcopy() or mpd_qresize() might fail with MPD_Malloc_error. In those
##  cases, MPD_UINT_MAX is returned.
##

proc mpd_qshiftr*(result: ptr mpd_t; a: ptr mpd_t; n: mpd_ssize_t; status: ptr uint32_t): mpd_uint_t =
  var rnd: mpd_uint_t
  var size: mpd_ssize_t
  assert(not mpd_isspecial(a))
  assert(n >= 0)
  if mpd_iszerocoeff(a) or n == 0:
    if not mpd_qcopy(result, a, status):
      return MPD_UINT_MAX
    return 0
  if n >= a.digits:
    rnd = _mpd_get_rnd(a.data, a.len, (n == a.digits))
    mpd_zerocoeff(result)
  else:
    result.digits = a.digits - n
    size = mpd_digits_to_size(result.digits)
    if result == a:
      rnd = _mpd_baseshiftr(result.data, a.data, a.len, n)
      ##  reducing the size cannot fail
      mpd_qresize(result, size, status)
    else:
      if not mpd_qresize(result, size, status):
        return MPD_UINT_MAX
      rnd = _mpd_baseshiftr(result.data, a.data, a.len, n)
    result.len = size
  mpd_copy_flags(result, a)
  result.exp = a.exp
  return rnd

## ****************************************************************************
##                          Miscellaneous operations
## ****************************************************************************
##  Logical And

proc mpd_qand*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  var
    big: ptr mpd_t = a
    small: ptr mpd_t = b
  var
    x: mpd_uint_t
    y: mpd_uint_t
    z: mpd_uint_t
    xbit: mpd_uint_t
    ybit: mpd_uint_t
  var
    k: cint
    mswdigits: cint
  var i: mpd_ssize_t
  if mpd_isspecial(a) or mpd_isspecial(b) or mpd_isnegative(a) or mpd_isnegative(b) or
      a.exp != 0 or b.exp != 0:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if b.digits > a.digits:
    big = b
    small = a
  if not mpd_qresize(result, big.len, status):
    return
  i = 0
  while i < small.len - 1:
    x = small.data[i]
    y = big.data[i]
    z = 0
    k = 0
    while k < MPD_RDIGITS:
      xbit = x mod 10
      x = x / 10
      ybit = y mod 10
      y = y / 10
      if xbit > 1 or ybit > 1:
        break invalid_operation
      inc(z, if (xbit and ybit): mpd_pow10[k] else: 0)
      inc(k)
    result.data[i] = z
    inc(i)
  ##  most significant word of small
  x = small.data[i]
  y = big.data[i]
  z = 0
  mswdigits = mpd_word_digits(x)
  k = 0
  while k < mswdigits:
    xbit = x mod 10
    x = x / 10
    ybit = y mod 10
    y = y / 10
    if xbit > 1 or ybit > 1:
      break invalid_operation
    inc(z, if (xbit and ybit): mpd_pow10[k] else: 0)
    inc(k)
  result.data[inc(i)] = z
  ##  scan the rest of y for digits > 1
  while k < MPD_RDIGITS:
    ybit = y mod 10
    y = y / 10
    if ybit > 1:
      break invalid_operation
    inc(k)
  ##  scan the rest of big for digits > 1
  while i < big.len:
    y = big.data[i]
    k = 0
    while k < MPD_RDIGITS:
      ybit = y mod 10
      y = y / 10
      if ybit > 1:
        break invalid_operation
      inc(k)
    inc(i)
  mpd_clear_flags(result)
  result.exp = 0
  result.len = _mpd_real_size(result.data, small.len)
  mpd_qresize(result, result.len, status)
  mpd_setdigits(result)
  _mpd_cap(result, ctx)
  return
  mpd_seterror(result, MPD_Invalid_operation, status)

##  Class of an operand. Returns a pointer to the constant name.

proc mpd_class*(a: ptr mpd_t; ctx: ptr mpd_context_t): cstring =
  if mpd_isnan(a):
    if mpd_isqnan(a):
      return "NaN"
    else:
      return "sNaN"
  elif mpd_ispositive(a):
    if mpd_isinfinite(a):
      return "+Infinity"
    elif mpd_iszero(a):
      return "+Zero"
    elif mpd_isnormal(a, ctx):
      return "+Normal"
    else:
      return "+Subnormal"
  else:
    if mpd_isinfinite(a):
      return "-Infinity"
    elif mpd_iszero(a):
      return "-Zero"
    elif mpd_isnormal(a, ctx):
      return "-Normal"
    else:
      return "-Subnormal"

##  Logical Xor

proc mpd_qinvert*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                 status: ptr uint32_t) =
  var
    x: mpd_uint_t
    z: mpd_uint_t
    xbit: mpd_uint_t
  var
    i: mpd_ssize_t
    digits: mpd_ssize_t
    len: mpd_ssize_t
  var
    q: mpd_ssize_t
    r: mpd_ssize_t
  var k: cint
  if mpd_isspecial(a) or mpd_isnegative(a) or a.exp != 0:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  digits = if (a.digits < ctx.prec): ctx.prec else: a.digits
  _mpd_idiv_word(addr(q), addr(r), digits, MPD_RDIGITS)
  len = if (r == 0): q else: q + 1
  if not mpd_qresize(result, len, status):
    return
  i = 0
  while i < len:
    x = if (i < a.len): a.data[i] else: 0
    z = 0
    k = 0
    while k < MPD_RDIGITS:
      xbit = x mod 10
      x = x / 10
      if xbit > 1:
        break invalid_operation
      inc(z, if not xbit: mpd_pow10[k] else: 0)
      inc(k)
    result.data[i] = z
    inc(i)
  mpd_clear_flags(result)
  result.exp = 0
  result.len = _mpd_real_size(result.data, len)
  mpd_qresize(result, result.len, status)
  mpd_setdigits(result)
  _mpd_cap(result, ctx)
  return
  mpd_seterror(result, MPD_Invalid_operation, status)

##  Exponent of the magnitude of the most significant digit of the operand.

proc mpd_qlogb*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) =
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    mpd_setspecial(result, MPD_POS, MPD_INF)
  elif mpd_iszerocoeff(a):
    mpd_setspecial(result, MPD_NEG, MPD_INF)
    status[] = status[] or MPD_Division_by_zero
  else:
    mpd_qset_ssize(result, mpd_adjexp(a), ctx, status)

##  Logical Or

proc mpd_qor*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
             status: ptr uint32_t) =
  var
    big: ptr mpd_t = a
    small: ptr mpd_t = b
  var
    x: mpd_uint_t
    y: mpd_uint_t
    z: mpd_uint_t
    xbit: mpd_uint_t
    ybit: mpd_uint_t
  var
    k: cint
    mswdigits: cint
  var i: mpd_ssize_t
  if mpd_isspecial(a) or mpd_isspecial(b) or mpd_isnegative(a) or mpd_isnegative(b) or
      a.exp != 0 or b.exp != 0:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if b.digits > a.digits:
    big = b
    small = a
  if not mpd_qresize(result, big.len, status):
    return
  i = 0
  while i < small.len - 1:
    x = small.data[i]
    y = big.data[i]
    z = 0
    k = 0
    while k < MPD_RDIGITS:
      xbit = x mod 10
      x = x / 10
      ybit = y mod 10
      y = y / 10
      if xbit > 1 or ybit > 1:
        break invalid_operation
      inc(z, if (xbit or ybit): mpd_pow10[k] else: 0)
      inc(k)
    result.data[i] = z
    inc(i)
  ##  most significant word of small
  x = small.data[i]
  y = big.data[i]
  z = 0
  mswdigits = mpd_word_digits(x)
  k = 0
  while k < mswdigits:
    xbit = x mod 10
    x = x / 10
    ybit = y mod 10
    y = y / 10
    if xbit > 1 or ybit > 1:
      break invalid_operation
    inc(z, if (xbit or ybit): mpd_pow10[k] else: 0)
    inc(k)
  ##  scan for digits > 1 and copy the rest of y
  while k < MPD_RDIGITS:
    ybit = y mod 10
    y = y / 10
    if ybit > 1:
      break invalid_operation
    inc(z, ybit * mpd_pow10[k])
    inc(k)
  result.data[inc(i)] = z
  ##  scan for digits > 1 and copy the rest of big
  while i < big.len:
    y = big.data[i]
    k = 0
    while k < MPD_RDIGITS:
      ybit = y mod 10
      y = y / 10
      if ybit > 1:
        break invalid_operation
      inc(k)
    result.data[i] = big.data[i]
    inc(i)
  mpd_clear_flags(result)
  result.exp = 0
  result.len = _mpd_real_size(result.data, big.len)
  mpd_qresize(result, result.len, status)
  mpd_setdigits(result)
  _mpd_cap(result, ctx)
  return
  mpd_seterror(result, MPD_Invalid_operation, status)

##
##  Rotate the coefficient of 'a' by 'b' digits. 'b' must be an integer with
##  exponent 0.
##

proc mpd_qrotate*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                 status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  MPD_NEW_STATIC(tmp, 0, 0, 0, 0)
  MPD_NEW_STATIC(big, 0, 0, 0, 0)
  MPD_NEW_STATIC(small, 0, 0, 0, 0)
  var
    n: mpd_ssize_t
    lshift: mpd_ssize_t
    rshift: mpd_ssize_t
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      return
  if b.exp != 0 or mpd_isinfinite(b):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  n = mpd_qget_ssize(b, addr(workstatus))
  if workstatus and MPD_Invalid_operation:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if n > ctx.prec or n < -ctx.prec:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if mpd_isinfinite(a):
    mpd_qcopy(result, a, status)
    return
  if n >= 0:
    lshift = n
    rshift = ctx.prec - n
  else:
    lshift = ctx.prec + n
    rshift = -n
  if a.digits > ctx.prec:
    if not mpd_qcopy(addr(tmp), a, status):
      mpd_seterror(result, MPD_Malloc_error, status)
      break finish
    _mpd_cap(addr(tmp), ctx)
    a = addr(tmp)
  if not mpd_qshiftl(addr(big), a, lshift, status):
    mpd_seterror(result, MPD_Malloc_error, status)
    break finish
  _mpd_cap(addr(big), ctx)
  if mpd_qshiftr(addr(small), a, rshift, status) == MPD_UINT_MAX:
    mpd_seterror(result, MPD_Malloc_error, status)
    break finish
  _mpd_qadd(result, addr(big), addr(small), ctx, status)
  mpd_del(addr(tmp))
  mpd_del(addr(big))
  mpd_del(addr(small))

##
##  b must be an integer with exponent 0 and in the range +-2*(emax + prec).
##  XXX: In my opinion +-(2*emax + prec) would be more sensible.
##  The result is a with the value of b added to its exponent.
##

proc mpd_qscaleb*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                 status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  var
    n: mpd_uint_t
    maxjump: mpd_uint_t
  when not defined(LEGACY_COMPILER):
    var exp: int64_t
  else:
    var x: mpd_uint_t
    var
      x_sign: cint
      n_sign: cint
    var exp: mpd_ssize_t
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      return
  if b.exp != 0 or mpd_isinfinite(b):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  n = mpd_qabs_uint(b, addr(workstatus))
  ##  the spec demands this
  maxjump = 2 * (mpd_uint_t)(ctx.emax + ctx.prec)
  if n > maxjump or workstatus and MPD_Invalid_operation:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if mpd_isinfinite(a):
    mpd_qcopy(result, a, status)
    return
  when not defined(LEGACY_COMPILER):
    exp = a.exp + cast[int64_t](n * mpd_arith_sign(b))
    exp = if (exp > MPD_EXP_INF): MPD_EXP_INF else: exp
    exp = if (exp < MPD_EXP_CLAMP): MPD_EXP_CLAMP else: exp
  else:
    x = if (a.exp < 0): -a.exp else: a.exp
    x_sign = if (a.exp < 0): 1 else: 0
    n_sign = if mpd_isnegative(b): 1 else: 0
    if x_sign == n_sign:
      x = x + n
      if x < n:
        x = MPD_UINT_MAX
    else:
      x_sign = if (x >= n): x_sign else: n_sign
      x = if (x >= n): x - n else: n - x
    if not x_sign and x > MPD_EXP_INF:
      x = MPD_EXP_INF
    if x_sign and x > -MPD_EXP_CLAMP:
      x = -MPD_EXP_CLAMP
    exp = if x_sign: -(cast[mpd_ssize_t](x)) else: cast[mpd_ssize_t](x)
  mpd_qcopy(result, a, status)
  result.exp = cast[mpd_ssize_t](exp)
  mpd_qfinalize(result, ctx, status)

##
##  Shift the coefficient by n digits, positive n is a left shift. In the case
##  of a left shift, the result is decapitated to fit the context precision. If
##  you don't want that, use mpd_shiftl().
##

proc mpd_qshiftn*(result: ptr mpd_t; a: ptr mpd_t; n: mpd_ssize_t;
                 ctx: ptr mpd_context_t; status: ptr uint32_t) =
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    mpd_qcopy(result, a, status)
    return
  if n >= 0 and n <= ctx.prec:
    mpd_qshiftl(result, a, n, status)
    _mpd_cap(result, ctx)
  elif n < 0 and n >= -ctx.prec:
    if not mpd_qcopy(result, a, status):
      return
    _mpd_cap(result, ctx)
    mpd_qshiftr_inplace(result, -n)
  else:
    mpd_seterror(result, MPD_Invalid_operation, status)

##
##  Same as mpd_shiftn(), but the shift is specified by the decimal b, which
##  must be an integer with a zero exponent. Infinities remain infinities.
##

proc mpd_qshift*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  var n: mpd_ssize_t
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      return
  if b.exp != 0 or mpd_isinfinite(b):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  n = mpd_qget_ssize(b, addr(workstatus))
  if workstatus and MPD_Invalid_operation:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if n > ctx.prec or n < -ctx.prec:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if mpd_isinfinite(a):
    mpd_qcopy(result, a, status)
    return
  if n >= 0:
    mpd_qshiftl(result, a, n, status)
    _mpd_cap(result, ctx)
  else:
    if not mpd_qcopy(result, a, status):
      return
    _mpd_cap(result, ctx)
    mpd_qshiftr_inplace(result, -n)

##  Logical Xor

proc mpd_qxor*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  var
    big: ptr mpd_t = a
    small: ptr mpd_t = b
  var
    x: mpd_uint_t
    y: mpd_uint_t
    z: mpd_uint_t
    xbit: mpd_uint_t
    ybit: mpd_uint_t
  var
    k: cint
    mswdigits: cint
  var i: mpd_ssize_t
  if mpd_isspecial(a) or mpd_isspecial(b) or mpd_isnegative(a) or mpd_isnegative(b) or
      a.exp != 0 or b.exp != 0:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if b.digits > a.digits:
    big = b
    small = a
  if not mpd_qresize(result, big.len, status):
    return
  i = 0
  while i < small.len - 1:
    x = small.data[i]
    y = big.data[i]
    z = 0
    k = 0
    while k < MPD_RDIGITS:
      xbit = x mod 10
      x = x / 10
      ybit = y mod 10
      y = y / 10
      if xbit > 1 or ybit > 1:
        break invalid_operation
      inc(z, if (xbit xor ybit): mpd_pow10[k] else: 0)
      inc(k)
    result.data[i] = z
    inc(i)
  ##  most significant word of small
  x = small.data[i]
  y = big.data[i]
  z = 0
  mswdigits = mpd_word_digits(x)
  k = 0
  while k < mswdigits:
    xbit = x mod 10
    x = x / 10
    ybit = y mod 10
    y = y / 10
    if xbit > 1 or ybit > 1:
      break invalid_operation
    inc(z, if (xbit xor ybit): mpd_pow10[k] else: 0)
    inc(k)
  ##  scan for digits > 1 and copy the rest of y
  while k < MPD_RDIGITS:
    ybit = y mod 10
    y = y / 10
    if ybit > 1:
      break invalid_operation
    inc(z, ybit * mpd_pow10[k])
    inc(k)
  result.data[inc(i)] = z
  ##  scan for digits > 1 and copy the rest of big
  while i < big.len:
    y = big.data[i]
    k = 0
    while k < MPD_RDIGITS:
      ybit = y mod 10
      y = y / 10
      if ybit > 1:
        break invalid_operation
      inc(k)
    result.data[i] = big.data[i]
    inc(i)
  mpd_clear_flags(result)
  result.exp = 0
  result.len = _mpd_real_size(result.data, big.len)
  mpd_qresize(result, result.len, status)
  mpd_setdigits(result)
  _mpd_cap(result, ctx)
  return
  mpd_seterror(result, MPD_Invalid_operation, status)

## ****************************************************************************
##                          Arithmetic operations
## ****************************************************************************
##
##  The absolute value of a. If a is negative, the result is the same
##  as the result of the minus operation. Otherwise, the result is the
##  result of the plus operation.
##

proc mpd_qabs*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
  if mpd_isnegative(a):
    mpd_qminus(result, a, ctx, status)
  else:
    mpd_qplus(result, a, ctx, status)

proc _mpd_ptrswap*(a: ptr ptr mpd_t; b: ptr ptr mpd_t) {.inline.} =
  var t: ptr mpd_t = a[]
  a[] = b[]
  b[] = t

##  Add or subtract infinities.

proc _mpd_qaddsub_inf*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; sign_b: uint8_t;
                      status: ptr uint32_t) =
  if mpd_isinfinite(a):
    if mpd_sign(a) != sign_b and mpd_isinfinite(b):
      mpd_seterror(result, MPD_Invalid_operation, status)
    else:
      mpd_setspecial(result, mpd_sign(a), MPD_INF)
    return
  assert(mpd_isinfinite(b))
  mpd_setspecial(result, sign_b, MPD_INF)

##  Add or subtract non-special numbers.

proc _mpd_qaddsub*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; sign_b: uint8_t;
                  ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var
    big: ptr mpd_t
    small: ptr mpd_t
  MPD_NEW_STATIC(big_aligned, 0, 0, 0, 0)
  MPD_NEW_CONST(tiny, 0, 0, 1, 1, 1, 1)
  var carry: mpd_uint_t
  var
    newsize: mpd_ssize_t
    shift: mpd_ssize_t
  var
    exp: mpd_ssize_t
    i: mpd_ssize_t
  var swap: cint = 0
  ##  compare exponents
  big = a
  small = b
  if big.exp != small.exp:
    if small.exp > big.exp:
      _mpd_ptrswap(addr(big), addr(small))
      inc(swap)
    if not mpd_iszerocoeff(big):
      exp = big.exp - 1
      inc(exp, if (big.digits > ctx.prec): 0 else: big.digits - ctx.prec - 1)
      if mpd_adjexp(small) < exp:
        ##
        ##  Avoid huge shifts by substituting a value for small that is
        ##  guaranteed to produce the same results.
        ##
        ##  adjexp(small) < exp if and only if:
        ##
        ##    bdigits <= prec AND
        ##    bdigits+shift >= prec+2+sdigits AND
        ##    exp = bexp+bdigits-prec-2
        ##
        ##      1234567000000000  ->  bdigits + shift
        ##      ----------XX1234  ->  sdigits
        ##      ----------X1      ->  tiny-digits
        ##      |- prec -|
        ##
        ##       OR
        ##
        ##    bdigits > prec AND
        ##    shift > sdigits AND
        ##    exp = bexp-1
        ##
        ##      1234567892100000  ->  bdigits + shift
        ##      ----------XX1234  ->  sdigits
        ##      ----------X1      ->  tiny-digits
        ##      |- prec -|
        ##
        ##  If tiny is zero, adding or subtracting is a no-op.
        ##  Otherwise, adding tiny generates a non-zero digit either
        ##  below the rounding digit or the least significant digit
        ##  of big. When subtracting, tiny is in the same position as
        ##  the carry that would be generated by subtracting sdigits.
        ##
        mpd_copy_flags(addr(tiny), small)
        tiny.exp = exp
        tiny.digits = 1
        tiny.len = 1
        tiny.data[0] = if mpd_iszerocoeff(small): 0 else: 1
        small = addr(tiny)
      shift = big.exp - small.exp
      if not mpd_qshiftl(addr(big_aligned), big, shift, status):
        mpd_seterror(result, MPD_Malloc_error, status)
        break finish
      big = addr(big_aligned)
  result.exp = small.exp
  ##  compare length of coefficients
  if big.len < small.len:
    _mpd_ptrswap(addr(big), addr(small))
    inc(swap)
  newsize = big.len
  if not mpd_qresize(result, newsize, status):
    break finish
  if mpd_sign(a) == sign_b:
    carry = _mpd_baseadd(result.data, big.data, small.data, big.len, small.len)
    if carry:
      newsize = big.len + 1
      if not mpd_qresize(result, newsize, status):
        break finish
      result.data[newsize - 1] = carry
    result.len = newsize
    mpd_set_flags(result, sign_b)
  else:
    if big.len == small.len:
      i = big.len - 1
      while i >= 0:
        if big.data[i] != small.data[i]:
          if big.data[i] < small.data[i]:
            _mpd_ptrswap(addr(big), addr(small))
            inc(swap)
          break
        dec(i)
    _mpd_basesub(result.data, big.data, small.data, big.len, small.len)
    newsize = _mpd_real_size(result.data, big.len)
    ##  resize to smaller cannot fail
    cast[nil](mpd_qresize(result, newsize, status))
    result.len = newsize
    sign_b = if (swap and 1): sign_b else: mpd_sign(a)
    mpd_set_flags(result, sign_b)
    if mpd_iszerocoeff(result):
      mpd_set_positive(result)
      if ctx.round == MPD_ROUND_FLOOR:
        mpd_set_negative(result)
  mpd_setdigits(result)
  mpd_del(addr(big_aligned))

##  Add a and b. No specials, no finalizing.

proc _mpd_qadd*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) =
  _mpd_qaddsub(result, a, b, mpd_sign(b), ctx, status)

##  Subtract b from a. No specials, no finalizing.

proc _mpd_qsub*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) =
  _mpd_qaddsub(result, a, b, not mpd_sign(b), ctx, status)

##  Add a and b.

proc mpd_qadd*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      return
    _mpd_qaddsub_inf(result, a, b, mpd_sign(b), status)
    return
  _mpd_qaddsub(result, a, b, mpd_sign(b), ctx, status)
  mpd_qfinalize(result, ctx, status)

##  Add a and b. Set NaN/Invalid_operation if the result is inexact.

proc _mpd_qadd_exact*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                     ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  mpd_qadd(result, a, b, ctx, addr(workstatus))
  status[] = status[] or workstatus
  if workstatus and (MPD_Inexact or MPD_Rounded or MPD_Clamped):
    mpd_seterror(result, MPD_Invalid_operation, status)

##  Subtract b from a.

proc mpd_qsub*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      return
    _mpd_qaddsub_inf(result, a, b, not mpd_sign(b), status)
    return
  _mpd_qaddsub(result, a, b, not mpd_sign(b), ctx, status)
  mpd_qfinalize(result, ctx, status)

##  Subtract b from a. Set NaN/Invalid_operation if the result is inexact.

proc _mpd_qsub_exact*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                     ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  mpd_qsub(result, a, b, ctx, addr(workstatus))
  status[] = status[] or workstatus
  if workstatus and (MPD_Inexact or MPD_Rounded or MPD_Clamped):
    mpd_seterror(result, MPD_Invalid_operation, status)

##  Add decimal and mpd_ssize_t.

proc mpd_qadd_ssize*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_ssize_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  mpd_maxcontext(addr(maxcontext))
  mpd_qsset_ssize(addr(bb), b, addr(maxcontext), status)
  mpd_qadd(result, a, addr(bb), ctx, status)
  mpd_del(addr(bb))

##  Add decimal and mpd_uint_t.

proc mpd_qadd_uint*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_uint_t;
                   ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  mpd_maxcontext(addr(maxcontext))
  mpd_qsset_uint(addr(bb), b, addr(maxcontext), status)
  mpd_qadd(result, a, addr(bb), ctx, status)
  mpd_del(addr(bb))

##  Subtract mpd_ssize_t from decimal.

proc mpd_qsub_ssize*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_ssize_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  mpd_maxcontext(addr(maxcontext))
  mpd_qsset_ssize(addr(bb), b, addr(maxcontext), status)
  mpd_qsub(result, a, addr(bb), ctx, status)
  mpd_del(addr(bb))

##  Subtract mpd_uint_t from decimal.

proc mpd_qsub_uint*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_uint_t;
                   ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  mpd_maxcontext(addr(maxcontext))
  mpd_qsset_uint(addr(bb), b, addr(maxcontext), status)
  mpd_qsub(result, a, addr(bb), ctx, status)
  mpd_del(addr(bb))

##  Add decimal and int32_t.

proc mpd_qadd_i32*(result: ptr mpd_t; a: ptr mpd_t; b: int32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qadd_ssize(result, a, b, ctx, status)

##  Add decimal and uint32_t.

proc mpd_qadd_u32*(result: ptr mpd_t; a: ptr mpd_t; b: uint32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qadd_uint(result, a, b, ctx, status)

when defined(CONFIG_64):
  ##  Add decimal and int64_t.
  proc mpd_qadd_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    mpd_qadd_ssize(result, a, b, ctx, status)

  ##  Add decimal and uint64_t.
  proc mpd_qadd_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
    mpd_qadd_uint(result, a, b, ctx, status)

elif not defined(LEGACY_COMPILER):
  ##  Add decimal and int64_t.
  proc mpd_qadd_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    MPD_NEW_STATIC(bb, 0, 0, 0, 0)
    mpd_maxcontext(addr(maxcontext))
    mpd_qset_i64(addr(bb), b, addr(maxcontext), status)
    mpd_qadd(result, a, addr(bb), ctx, status)
    mpd_del(addr(bb))

  ##  Add decimal and uint64_t.
  proc mpd_qadd_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    MPD_NEW_STATIC(bb, 0, 0, 0, 0)
    mpd_maxcontext(addr(maxcontext))
    mpd_qset_u64(addr(bb), b, addr(maxcontext), status)
    mpd_qadd(result, a, addr(bb), ctx, status)
    mpd_del(addr(bb))

##  Subtract int32_t from decimal.

proc mpd_qsub_i32*(result: ptr mpd_t; a: ptr mpd_t; b: int32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qsub_ssize(result, a, b, ctx, status)

##  Subtract uint32_t from decimal.

proc mpd_qsub_u32*(result: ptr mpd_t; a: ptr mpd_t; b: uint32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qsub_uint(result, a, b, ctx, status)

when defined(CONFIG_64):
  ##  Subtract int64_t from decimal.
  proc mpd_qsub_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    mpd_qsub_ssize(result, a, b, ctx, status)

  ##  Subtract uint64_t from decimal.
  proc mpd_qsub_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
    mpd_qsub_uint(result, a, b, ctx, status)

elif not defined(LEGACY_COMPILER):
  ##  Subtract int64_t from decimal.
  proc mpd_qsub_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    MPD_NEW_STATIC(bb, 0, 0, 0, 0)
    mpd_maxcontext(addr(maxcontext))
    mpd_qset_i64(addr(bb), b, addr(maxcontext), status)
    mpd_qsub(result, a, addr(bb), ctx, status)
    mpd_del(addr(bb))

  ##  Subtract uint64_t from decimal.
  proc mpd_qsub_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    MPD_NEW_STATIC(bb, 0, 0, 0, 0)
    mpd_maxcontext(addr(maxcontext))
    mpd_qset_u64(addr(bb), b, addr(maxcontext), status)
    mpd_qsub(result, a, addr(bb), ctx, status)
    mpd_del(addr(bb))

##  Divide infinities.

proc _mpd_qdiv_inf*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                   status: ptr uint32_t) =
  if mpd_isinfinite(a):
    if mpd_isinfinite(b):
      mpd_seterror(result, MPD_Invalid_operation, status)
      return
    mpd_setspecial(result, mpd_sign(a) xor mpd_sign(b), MPD_INF)
    return
  assert(mpd_isinfinite(b))
  _settriple(result, mpd_sign(a) xor mpd_sign(b), 0, mpd_etiny(ctx))
  status[] = status[] or MPD_Clamped

const
  NO_IDEAL_EXP* = 0
  SET_IDEAL_EXP* = 1

##  Divide a by b.

proc _mpd_qdiv*(action: cint; q: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
               ctx: ptr mpd_context_t; status: ptr uint32_t) =
  MPD_NEW_STATIC(aligned, 0, 0, 0, 0)
  var ld: mpd_uint_t
  var
    shift: mpd_ssize_t
    exp: mpd_ssize_t
    tz: mpd_ssize_t
  var newsize: mpd_ssize_t
  var ideal_exp: mpd_ssize_t
  var rem: mpd_uint_t
  var sign_a: uint8_t = mpd_sign(a)
  var sign_b: uint8_t = mpd_sign(b)
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(q, a, b, ctx, status):
      return
    _mpd_qdiv_inf(q, a, b, ctx, status)
    return
  if mpd_iszerocoeff(b):
    if mpd_iszerocoeff(a):
      mpd_seterror(q, MPD_Division_undefined, status)
    else:
      mpd_setspecial(q, sign_a xor sign_b, MPD_INF)
      status[] = status[] or MPD_Division_by_zero
    return
  if mpd_iszerocoeff(a):
    exp = a.exp - b.exp
    _settriple(q, sign_a xor sign_b, 0, exp)
    mpd_qfinalize(q, ctx, status)
    return
  shift = (b.digits - a.digits) + ctx.prec + 1
  ideal_exp = a.exp - b.exp
  exp = ideal_exp - shift
  if shift > 0:
    if not mpd_qshiftl(addr(aligned), a, shift, status):
      mpd_seterror(q, MPD_Malloc_error, status)
      break finish
    a = addr(aligned)
  elif shift < 0:
    shift = -shift
    if not mpd_qshiftl(addr(aligned), b, shift, status):
      mpd_seterror(q, MPD_Malloc_error, status)
      break finish
    b = addr(aligned)
  newsize = a.len - b.len + 1
  if (q != b and q != a) or (q == b and newsize > b.len):
    if not mpd_qresize(q, newsize, status):
      mpd_seterror(q, MPD_Malloc_error, status)
      break finish
  if b.len == 1:
    rem = _mpd_shortdiv(q.data, a.data, a.len, b.data[0])
  elif b.len <= MPD_NEWTONDIV_CUTOFF:
    var ret: cint = _mpd_basedivmod(q.data, nil, a.data, b.data, a.len, b.len)
    if ret < 0:
      mpd_seterror(q, MPD_Malloc_error, status)
      break finish
    rem = ret
  else:
    MPD_NEW_STATIC(r, 0, 0, 0, 0)
    _mpd_base_ndivmod(q, addr(r), a, b, status)
    if mpd_isspecial(q) or mpd_isspecial(addr(r)):
      mpd_setspecial(q, MPD_POS, MPD_NAN)
      mpd_del(addr(r))
      break finish
    rem = not mpd_iszerocoeff(addr(r))
    mpd_del(addr(r))
    newsize = q.len
  newsize = _mpd_real_size(q.data, newsize)
  ##  resize to smaller cannot fail
  mpd_qresize(q, newsize, status)
  mpd_set_flags(q, sign_a xor sign_b)
  q.len = newsize
  mpd_setdigits(q)
  shift = ideal_exp - exp
  if rem:
    ld = mpd_lsd(q.data[0])
    if ld == 0 or ld == 5:
      inc(q.data[0], 1)
  elif action == SET_IDEAL_EXP and shift > 0:
    tz = mpd_trail_zeros(q)
    shift = if (tz > shift): shift else: tz
    mpd_qshiftr_inplace(q, shift)
    inc(exp, shift)
  q.exp = exp
  mpd_del(addr(aligned))
  mpd_qfinalize(q, ctx, status)

##  Divide a by b.

proc mpd_qdiv*(q: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  MPD_NEW_STATIC(aa, 0, 0, 0, 0)
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  var xstatus: uint32_t = 0
  if q == a:
    if not mpd_qcopy(addr(aa), a, status):
      mpd_seterror(q, MPD_Malloc_error, status)
      break `out`
    a = addr(aa)
  if q == b:
    if not mpd_qcopy(addr(bb), b, status):
      mpd_seterror(q, MPD_Malloc_error, status)
      break `out`
    b = addr(bb)
  _mpd_qdiv(SET_IDEAL_EXP, q, a, b, ctx, addr(xstatus))
  if xstatus and (MPD_Malloc_error or MPD_Division_impossible):
    ##  Inexact quotients (the usual case) fill the entire context precision,
    ##  which can lead to the above errors for very high precisions. Retry
    ##  the operation with a lower precision in case the result is exact.
    ##
    ##  We need an upper bound for the number of digits of a_coeff / b_coeff
    ##  when the result is exact.  If a_coeff' * 1 / b_coeff' is in lowest
    ##  terms, then maxdigits(a_coeff') + maxdigits(1 / b_coeff') is a suitable
    ##  bound.
    ##
    ##  1 / b_coeff' is exact iff b_coeff' exclusively has prime factors 2 or 5.
    ##  The largest amount of digits is generated if b_coeff' is a power of 2 or
    ##  a power of 5 and is less than or equal to log5(b_coeff') <= log2(b_coeff').
    ##
    ##  We arrive at a total upper bound:
    ##
    ##    maxdigits(a_coeff') + maxdigits(1 / b_coeff') <=
    ##    log10(a_coeff) + log2(b_coeff) =
    ##    log10(a_coeff) + log10(b_coeff) / log10(2) <=
    ##    a->digits + b->digits * 4;
    ##
    var workctx: mpd_context_t = ctx[]
    var ystatus: uint32_t = 0
    workctx.prec = a.digits + b.digits * 4
    if workctx.prec >= ctx.prec:
      status[] = status[] or (xstatus and MPD_Errors)
      break `out`
      ##  No point in retrying, keep the original error.
    _mpd_qdiv(SET_IDEAL_EXP, q, a, b, addr(workctx), addr(ystatus))
    if ystatus != 0:
      ystatus = status[] or ((ystatus or xstatus) and MPD_Errors)
      mpd_seterror(q, ystatus, status)
  else:
    status[] = status[] or xstatus
  mpd_del(addr(aa))
  mpd_del(addr(bb))

##  Internal function.

proc _mpd_qdivmod*(q: ptr mpd_t; r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                  ctx: ptr mpd_context_t; status: ptr uint32_t) =
  MPD_NEW_STATIC(aligned, 0, 0, 0, 0)
  var
    qsize: mpd_ssize_t
    rsize: mpd_ssize_t
  var
    ideal_exp: mpd_ssize_t
    expdiff: mpd_ssize_t
    shift: mpd_ssize_t
  var sign_a: uint8_t = mpd_sign(a)
  var sign_ab: uint8_t = mpd_sign(a) xor mpd_sign(b)
  ideal_exp = if (a.exp > b.exp): b.exp else: a.exp
  if mpd_iszerocoeff(a):
    if not mpd_qcopy(r, a, status):
      break nanresult
      ##  GCOV_NOT_REACHED
    r.exp = ideal_exp
    _settriple(q, sign_ab, 0, 0)
    return
  expdiff = mpd_adjexp(a) - mpd_adjexp(b)
  if expdiff < 0:
    if a.exp > b.exp:
      ##  positive and less than b->digits - a->digits
      shift = a.exp - b.exp
      if not mpd_qshiftl(r, a, shift, status):
        break nanresult
      r.exp = ideal_exp
    else:
      if not mpd_qcopy(r, a, status):
        break nanresult
    _settriple(q, sign_ab, 0, 0)
    return
  if expdiff > ctx.prec:
    status[] = status[] or MPD_Division_impossible
    break nanresult
  if a.exp != b.exp:
    shift = a.exp - b.exp
    if shift > 0:
      ##  by (3), after the shift a->digits <= prec + b->digits
      if not mpd_qshiftl(addr(aligned), a, shift, status):
        break nanresult
      a = addr(aligned)
    else:
      shift = -shift
      ##  by (2), after the shift b->digits <= a->digits
      if not mpd_qshiftl(addr(aligned), b, shift, status):
        break nanresult
      b = addr(aligned)
  qsize = a.len - b.len + 1
  if not (q == a and qsize < a.len) and not (q == b and qsize < b.len):
    if not mpd_qresize(q, qsize, status):
      break nanresult
  rsize = b.len
  if not (r == a and rsize < a.len):
    if not mpd_qresize(r, rsize, status):
      break nanresult
  if b.len == 1:
    assert(b.data[0] != 0)
    ##  annotation for scan-build
    if a.len == 1:
      _mpd_div_word(addr(q.data[0]), addr(r.data[0]), a.data[0], b.data[0])
    else:
      r.data[0] = _mpd_shortdiv(q.data, a.data, a.len, b.data[0])
  elif b.len <= MPD_NEWTONDIV_CUTOFF:
    var ret: cint
    ret = _mpd_basedivmod(q.data, r.data, a.data, b.data, a.len, b.len)
    if ret == -1:
      status[] = status[] or MPD_Malloc_error
      break nanresult
  else:
    _mpd_base_ndivmod(q, r, a, b, status)
    if mpd_isspecial(q) or mpd_isspecial(r):
      break nanresult
    qsize = q.len
    rsize = r.len
  qsize = _mpd_real_size(q.data, qsize)
  ##  resize to smaller cannot fail
  mpd_qresize(q, qsize, status)
  q.len = qsize
  mpd_setdigits(q)
  mpd_set_flags(q, sign_ab)
  q.exp = 0
  if q.digits > ctx.prec:
    status[] = status[] or MPD_Division_impossible
    break nanresult
  rsize = _mpd_real_size(r.data, rsize)
  ##  resize to smaller cannot fail
  mpd_qresize(r, rsize, status)
  r.len = rsize
  mpd_setdigits(r)
  mpd_set_flags(r, sign_a)
  r.exp = ideal_exp
  mpd_del(addr(aligned))
  return
  mpd_setspecial(q, MPD_POS, MPD_NAN)
  mpd_setspecial(r, MPD_POS, MPD_NAN)
  break `out`

##  Integer division with remainder.

proc mpd_qdivmod*(q: ptr mpd_t; r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                 ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var sign: uint8_t = mpd_sign(a) xor mpd_sign(b)
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(q, a, b, ctx, status):
      mpd_qcopy(r, q, status)
      return
    if mpd_isinfinite(a):
      if mpd_isinfinite(b):
        mpd_setspecial(q, MPD_POS, MPD_NAN)
      else:
        mpd_setspecial(q, sign, MPD_INF)
      mpd_setspecial(r, MPD_POS, MPD_NAN)
      status[] = status[] or MPD_Invalid_operation
      return
    if mpd_isinfinite(b):
      if not mpd_qcopy(r, a, status):
        mpd_seterror(q, MPD_Malloc_error, status)
        return
      mpd_qfinalize(r, ctx, status)
      _settriple(q, sign, 0, 0)
      return
    abort()
    ##  GCOV_NOT_REACHED
  if mpd_iszerocoeff(b):
    if mpd_iszerocoeff(a):
      mpd_setspecial(q, MPD_POS, MPD_NAN)
      mpd_setspecial(r, MPD_POS, MPD_NAN)
      status[] = status[] or MPD_Division_undefined
    else:
      mpd_setspecial(q, sign, MPD_INF)
      mpd_setspecial(r, MPD_POS, MPD_NAN)
      status[] = status[] or (MPD_Division_by_zero or MPD_Invalid_operation)
    return
  _mpd_qdivmod(q, r, a, b, ctx, status)
  mpd_qfinalize(q, ctx, status)
  mpd_qfinalize(r, ctx, status)

proc mpd_qdivint*(q: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                 status: ptr uint32_t) =
  MPD_NEW_STATIC(r, 0, 0, 0, 0)
  var sign: uint8_t = mpd_sign(a) xor mpd_sign(b)
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(q, a, b, ctx, status):
      return
    if mpd_isinfinite(a) and mpd_isinfinite(b):
      mpd_seterror(q, MPD_Invalid_operation, status)
      return
    if mpd_isinfinite(a):
      mpd_setspecial(q, sign, MPD_INF)
      return
    if mpd_isinfinite(b):
      _settriple(q, sign, 0, 0)
      return
    abort()
    ##  GCOV_NOT_REACHED
  if mpd_iszerocoeff(b):
    if mpd_iszerocoeff(a):
      mpd_seterror(q, MPD_Division_undefined, status)
    else:
      mpd_setspecial(q, sign, MPD_INF)
      status[] = status[] or MPD_Division_by_zero
    return
  _mpd_qdivmod(q, addr(r), a, b, ctx, status)
  mpd_del(addr(r))
  mpd_qfinalize(q, ctx, status)

##  Divide decimal by mpd_ssize_t.

proc mpd_qdiv_ssize*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_ssize_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  mpd_maxcontext(addr(maxcontext))
  mpd_qsset_ssize(addr(bb), b, addr(maxcontext), status)
  mpd_qdiv(result, a, addr(bb), ctx, status)
  mpd_del(addr(bb))

##  Divide decimal by mpd_uint_t.

proc mpd_qdiv_uint*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_uint_t;
                   ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  mpd_maxcontext(addr(maxcontext))
  mpd_qsset_uint(addr(bb), b, addr(maxcontext), status)
  mpd_qdiv(result, a, addr(bb), ctx, status)
  mpd_del(addr(bb))

##  Divide decimal by int32_t.

proc mpd_qdiv_i32*(result: ptr mpd_t; a: ptr mpd_t; b: int32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qdiv_ssize(result, a, b, ctx, status)

##  Divide decimal by uint32_t.

proc mpd_qdiv_u32*(result: ptr mpd_t; a: ptr mpd_t; b: uint32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qdiv_uint(result, a, b, ctx, status)

when defined(CONFIG_64):
  ##  Divide decimal by int64_t.
  proc mpd_qdiv_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    mpd_qdiv_ssize(result, a, b, ctx, status)

  ##  Divide decimal by uint64_t.
  proc mpd_qdiv_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
    mpd_qdiv_uint(result, a, b, ctx, status)

elif not defined(LEGACY_COMPILER):
  ##  Divide decimal by int64_t.
  proc mpd_qdiv_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    MPD_NEW_STATIC(bb, 0, 0, 0, 0)
    mpd_maxcontext(addr(maxcontext))
    mpd_qset_i64(addr(bb), b, addr(maxcontext), status)
    mpd_qdiv(result, a, addr(bb), ctx, status)
    mpd_del(addr(bb))

  ##  Divide decimal by uint64_t.
  proc mpd_qdiv_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    MPD_NEW_STATIC(bb, 0, 0, 0, 0)
    mpd_maxcontext(addr(maxcontext))
    mpd_qset_u64(addr(bb), b, addr(maxcontext), status)
    mpd_qdiv(result, a, addr(bb), ctx, status)
    mpd_del(addr(bb))

##  Pad the result with trailing zeros if it has fewer digits than prec.

proc _mpd_zeropad*(result: ptr mpd_t; ctx: ptr mpd_context_t; status: ptr uint32_t) =
  if not mpd_isspecial(result) and not mpd_iszero(result) and
      result.digits < ctx.prec:
    var shift: mpd_ssize_t = ctx.prec - result.digits
    mpd_qshiftl(result, result, shift, status)
    dec(result.exp, shift)

##  Check if the result is guaranteed to be one.

proc _mpd_qexp_check_one*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                         status: ptr uint32_t): cint =
  MPD_NEW_CONST(lim, 0, -(ctx.prec + 1), 1, 1, 1, 9)
  MPD_NEW_SHARED(aa, a)
  mpd_set_positive(addr(aa))
  ##  abs(a) <= 9 * 10**(-prec-1)
  if _mpd_cmp(addr(aa), addr(lim)) <= 0:
    _settriple(result, 0, 1, 0)
    status[] = status[] or (MPD_Rounded or MPD_Inexact)
    return 1
  return 0

##
##  Get the number of iterations for the Horner scheme in _mpd_qexp().
##

proc _mpd_get_exp_iterations*(r: ptr mpd_t; p: mpd_ssize_t): mpd_ssize_t {.inline.} =
  var log10pbyr: mpd_ssize_t
  ##  lower bound for log10(p / abs(r))
  var n: mpd_ssize_t
  assert(p >= 10)
  assert(not mpd_iszero(r))
  assert(-p < mpd_adjexp(r) and mpd_adjexp(r) <= -1)
  when defined(CONFIG_64):
    if p > (mpd_ssize_t)(1 shl 52):
      return MPD_SSIZE_MAX
  ##
  ##  Lower bound for log10(p / abs(r)): adjexp(p) - (adjexp(r) + 1)
  ##  At this point (for CONFIG_64, CONFIG_32 is not problematic):
  ##     1) 10 <= p <= 2**52
  ##     2) -p < adjexp(r) <= -1
  ##     3) 1 <= log10pbyr <= 2**52 + 14
  ##
  log10pbyr = (mpd_word_digits(p) - 1) - (mpd_adjexp(r) + 1)
  ##
  ##  The numerator in the paper is 1.435 * p - 1.182, calculated
  ##  exactly. We compensate for rounding errors by using 1.43503.
  ##  ACL2 proofs:
  ##     1) exp-iter-approx-lower-bound: The term below evaluated
  ##        in 53-bit floating point arithmetic is greater than or
  ##        equal to the exact term used in the paper.
  ##     2) exp-iter-approx-upper-bound: The term below is less than
  ##        or equal to 3/2 * p <= 3/2 * 2**52.
  ##
  n = cast[mpd_ssize_t](ceil((1.43503 * cast[cdouble](p) - 1.182) div
      cast[cdouble](log10pbyr)))
  return if n >= 3: n else: 3

##
##  Internal function, specials have been dealt with. Apart from Overflow
##  and Underflow, two cases must be considered for the error of the result:
##
##    1) abs(a) <= 9 * 10**(-prec-1)  ==>  result == 1
##
##       Absolute error: abs(1 - e**x) < 10**(-prec)
##       -------------------------------------------
##
##    2) abs(a) > 9 * 10**(-prec-1)
##
##       Relative error: abs(result - e**x) < 0.5 * 10**(-prec) * e**x
##       -------------------------------------------------------------
##
##  The algorithm is from Hull&Abrham, Variable Precision Exponential Function,
##  ACM Transactions on Mathematical Software, Vol. 12, No. 2, June 1986.
##
##  Main differences:
##
##   - The number of iterations for the Horner scheme is calculated using
##     53-bit floating point arithmetic.
##
##   - In the error analysis for ER (relative error accumulated in the
##     evaluation of the truncated series) the reduced operand r may
##     have any number of digits.
##     ACL2 proof: exponent-relative-error
##
##   - The analysis for early abortion has been adapted for the mpd_t
##     ranges.
##

proc _mpd_qexp*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) =
  var workctx: mpd_context_t
  MPD_NEW_STATIC(tmp, 0, 0, 0, 0)
  MPD_NEW_STATIC(sum, 0, 0, 0, 0)
  MPD_NEW_CONST(word, 0, 0, 1, 1, 1, 1)
  var
    j: mpd_ssize_t
    n: mpd_ssize_t
    t: mpd_ssize_t
  assert(not mpd_isspecial(a))
  if mpd_iszerocoeff(a):
    _settriple(result, MPD_POS, 1, 0)
    return
  when defined(CONFIG_64):
    const
      MPD_EXP_MAX_T = 19
  elif defined(CONFIG_32):
    const
      MPD_EXP_MAX_T = 10
  t = a.digits + a.exp
  t = if (t > 0): t else: 0
  if t > MPD_EXP_MAX_T:
    if mpd_ispositive(a):
      mpd_setspecial(result, MPD_POS, MPD_INF)
      status[] = status[] or (MPD_Overflow or MPD_Inexact or MPD_Rounded)
    else:
      _settriple(result, MPD_POS, 0, mpd_etiny(ctx))
      status[] = status[] or
          (MPD_Inexact or MPD_Rounded or MPD_Subnormal or MPD_Underflow or
          MPD_Clamped)
    return
  if _mpd_qexp_check_one(result, a, ctx, status):
    return
  mpd_maxcontext(addr(workctx))
  workctx.prec = ctx.prec + t + 2
  workctx.prec = if (workctx.prec < 10): 10 else: workctx.prec
  workctx.round = MPD_ROUND_HALF_EVEN
  if not mpd_qcopy(result, a, status):
    return
  dec(result.exp, t)
  ##
  ##  At this point:
  ##     1) 9 * 10**(-prec-1) < abs(a)
  ##     2) 9 * 10**(-prec-t-1) < abs(r)
  ##     3) log10(9) - prec - t - 1 < log10(abs(r)) < adjexp(abs(r)) + 1
  ##     4) - prec - t - 2 < adjexp(abs(r)) <= -1
  ##
  n = _mpd_get_exp_iterations(result, workctx.prec)
  if n == MPD_SSIZE_MAX:
    mpd_seterror(result, MPD_Invalid_operation, status)
    ##  GCOV_UNLIKELY
    return
    ##  GCOV_UNLIKELY
  _settriple(addr(sum), MPD_POS, 1, 0)
  j = n - 1
  while j >= 1:
    word.data[0] = j
    mpd_setdigits(addr(word))
    mpd_qdiv(addr(tmp), result, addr(word), addr(workctx), addr(workctx.status))
    mpd_qfma(addr(sum), addr(sum), addr(tmp), addr(one), addr(workctx),
             addr(workctx.status))
    dec(j)
  when defined(CONFIG_64):
    _mpd_qpow_uint(result, addr(sum), mpd_pow10[t], MPD_POS, addr(workctx), status)
  else:
    if t <= MPD_MAX_POW10:
      _mpd_qpow_uint(result, addr(sum), mpd_pow10[t], MPD_POS, addr(workctx), status)
    else:
      dec(t, MPD_MAX_POW10)
      _mpd_qpow_uint(addr(tmp), addr(sum), mpd_pow10[MPD_MAX_POW10], MPD_POS,
                     addr(workctx), status)
      _mpd_qpow_uint(result, addr(tmp), mpd_pow10[t], MPD_POS, addr(workctx), status)
  mpd_del(addr(tmp))
  mpd_del(addr(sum))
  status[] = status[] or (workctx.status and MPD_Errors)
  status[] = status[] or (MPD_Inexact or MPD_Rounded)

##  exp(a)

proc mpd_qexp*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  var workctx: mpd_context_t
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    if mpd_isnegative(a):
      _settriple(result, MPD_POS, 0, 0)
    else:
      mpd_setspecial(result, MPD_POS, MPD_INF)
    return
  if mpd_iszerocoeff(a):
    _settriple(result, MPD_POS, 1, 0)
    return
  workctx = ctx[]
  workctx.round = MPD_ROUND_HALF_EVEN
  if ctx.allcr:
    MPD_NEW_STATIC(t1, 0, 0, 0, 0)
    MPD_NEW_STATIC(t2, 0, 0, 0, 0)
    MPD_NEW_STATIC(ulp, 0, 0, 0, 0)
    MPD_NEW_STATIC(aa, 0, 0, 0, 0)
    var prec: mpd_ssize_t
    var ulpexp: mpd_ssize_t
    var workstatus: uint32_t
    if result == a:
      if not mpd_qcopy(addr(aa), a, status):
        mpd_seterror(result, MPD_Malloc_error, status)
        return
      a = addr(aa)
    workctx.clamp = 0
    prec = ctx.prec + 3
    while 1:
      workctx.prec = prec
      workstatus = 0
      _mpd_qexp(result, a, addr(workctx), addr(workstatus))
      status[] = status[] or workstatus
      ulpexp = result.exp + result.digits - workctx.prec
      if workstatus and MPD_Underflow:
        ##  The effective work precision is result->digits.
        ulpexp = result.exp
      _ssettriple(addr(ulp), MPD_POS, 1, ulpexp)
      ##
      ##  At this point [1]:
      ##    1) abs(result - e**x) < 0.5 * 10**(-prec) * e**x
      ##    2) result - ulp < e**x < result + ulp
      ##    3) result - ulp < result < result + ulp
      ##
      ##  If round(result-ulp)==round(result+ulp), then
      ##  round(result)==round(e**x). Therefore the result
      ##  is correctly rounded.
      ##
      ##  [1] If abs(a) <= 9 * 10**(-prec-1), use the absolute
      ##      error for a similar argument.
      ##
      workctx.prec = ctx.prec
      mpd_qadd(addr(t1), result, addr(ulp), addr(workctx), addr(workctx.status))
      mpd_qsub(addr(t2), result, addr(ulp), addr(workctx), addr(workctx.status))
      if mpd_isspecial(result) or mpd_iszerocoeff(result) or
          mpd_qcmp(addr(t1), addr(t2), status) == 0:
        workctx.clamp = ctx.clamp
        _mpd_zeropad(result, addr(workctx), status)
        mpd_check_underflow(result, addr(workctx), status)
        mpd_qfinalize(result, addr(workctx), status)
        break
      inc(prec, MPD_RDIGITS)
    mpd_del(addr(t1))
    mpd_del(addr(t2))
    mpd_del(addr(ulp))
    mpd_del(addr(aa))
  else:
    _mpd_qexp(result, a, addr(workctx), status)
    _mpd_zeropad(result, addr(workctx), status)
    mpd_check_underflow(result, addr(workctx), status)
    mpd_qfinalize(result, addr(workctx), status)

##  Fused multiply-add: (a * b) + c, with a single final rounding.

proc mpd_qfma*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; c: ptr mpd_t;
              ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  var cc: ptr mpd_t = nil
  if result == c:
    if (cc = mpd_qncopy(c)) == nil:
      mpd_seterror(result, MPD_Malloc_error, status)
      return
    c = cc
  _mpd_qmul(result, a, b, ctx, addr(workstatus))
  if not (workstatus and MPD_Invalid_operation):
    mpd_qadd(result, result, c, ctx, addr(workstatus))
  if cc:
    mpd_del(cc)
  status[] = status[] or workstatus

##
##  Schedule the optimal precision increase for the Newton iteration.
##    v := input operand
##    z_0 := initial approximation
##    initprec := natural number such that abs(log(v) - z_0) < 10**-initprec
##    maxprec := target precision
##
##  For convenience the output klist contains the elements in reverse order:
##    klist := [k_n-1, ..., k_0], where
##      1) k_0 <= initprec and
##      2) abs(log(v) - result) < 10**(-2*k_n-1 + 1) <= 10**-maxprec.
##

proc ln_schedule_prec*(klist: array[MPD_MAX_PREC_LOG2, mpd_ssize_t];
                      maxprec: mpd_ssize_t; initprec: mpd_ssize_t): cint {.inline.} =
  var k: mpd_ssize_t
  var i: cint
  assert(maxprec >= 2 and initprec >= 2)
  if maxprec <= initprec:
    return -1
  i = 0
  k = maxprec
  while true:
    k = (k + 2) div 2
    klist[inc(i)] = k
    if not (k > initprec):
      break
  return i - 1

##  The constants have been verified with both decimal.py and mpfr.

when defined(CONFIG_64):
  when MPD_RDIGITS != 19:
  ##  c2nim TODO
  ## static const mpd_uint_t mpd_ln10_data[MPD_MINALLOC_MAX] = {
  ##   6983716328982174407ULL, 9089704281976336583ULL, 1515961135648465461ULL,
  ##   4416816335727555703ULL, 2900988039194170265ULL, 2307925037472986509ULL,
  ##    107598438319191292ULL, 3466624107184669231ULL, 4450099781311469159ULL,
  ##   9807828059751193854ULL, 7713456862091670584ULL, 1492198849978748873ULL,
  ##   6528728696511086257ULL, 2385392051446341972ULL, 8692180205189339507ULL,
  ##   6518769751037497088ULL, 2375253577097505395ULL, 9095610299291824318ULL,
  ##    982748238504564801ULL, 5438635917781170543ULL, 7547331541421808427ULL,
  ##    752371033310119785ULL, 3171643095059950878ULL, 9785265383207606726ULL,
  ##   2932258279850258550ULL, 5497347726624257094ULL, 2976979522110718264ULL,
  ##   9221477656763693866ULL, 1979650047149510504ULL, 6674183485704422507ULL,
  ##   9702766860595249671ULL, 9278096762712757753ULL, 9314848524948644871ULL,
  ##   6826928280848118428ULL,  754403708474699401ULL,  230105703089634572ULL,
  ##   1929203337658714166ULL, 7589402567763113569ULL, 4208241314695689016ULL,
  ##   2922455440575892572ULL, 9356734206705811364ULL, 2684916746550586856ULL,
  ##    644507064800027750ULL, 9476834636167921018ULL, 5659121373450747856ULL,
  ##   2835522011480466371ULL, 6470806855677432162ULL, 7141748003688084012ULL,
  ##   9619404400222105101ULL, 5504893431493939147ULL, 6674744042432743651ULL,
  ##   2287698219886746543ULL, 7773262884616336622ULL, 1985283935053089653ULL,
  ##   4680843799894826233ULL, 8168948290720832555ULL, 8067566662873690987ULL,
  ##   6248633409525465082ULL, 9829834196778404228ULL, 3524802359972050895ULL,
  ##   3327900967572609677ULL,  110148862877297603ULL,  179914546843642076ULL,
  ##   2302585092994045684ULL
  ## };
  ##
else:
  when MPD_RDIGITS != 9:
  var mpd_ln10_data*: array[MPD_MINALLOC_MAX, mpd_uint_t] = [401682692, 708474699,
      720754403, 30896345, 602301057, 765871416, 192920333, 763113569, 589402567,
      956890167, 82413146, 589257242, 245544057, 811364292, 734206705, 868569356,
      167465505, 775026849, 706480002, 18064450, 636167921, 569476834, 734507478,
      156591213, 148046637, 283552201, 677432162, 470806855, 880840126, 417480036,
      210510171, 940440022, 939147961, 893431493, 436515504, 440424327, 654366747,
      821988674, 622228769, 884616336, 537773262, 350530896, 319852839, 989482623,
      468084379, 720832555, 168948290, 736909878, 675666628, 546508280, 863340952,
      404228624, 834196778, 508959829, 23599720, 967735248, 96757260, 603332790,
      862877297, 760110148, 468436420, 401799145, 299404568, 230258509]
##  _mpd_ln10 is used directly for precisions smaller than MINALLOC_MAX*RDIGITS.
##    Otherwise, it serves as the initial approximation for calculating ln(10).

var _mpd_ln10*: mpd_t = [MPD_STATIC or MPD_CONST_DATA,
                     -(MPD_MINALLOC_MAX * MPD_RDIGITS - 1),
                     MPD_MINALLOC_MAX * MPD_RDIGITS, MPD_MINALLOC_MAX,
                     MPD_MINALLOC_MAX, cast[ptr mpd_uint_t](mpd_ln10_data)]

##
##  Set 'result' to log(10).
##    Ulp error: abs(result - log(10)) < ulp(log(10))
##    Relative error: abs(result - log(10)) < 5 * 10**-prec * log(10)
##
##  NOTE: The relative error is not derived from the ulp error, but
##  calculated separately using the fact that 23/10 < log(10) < 24/10.
##

proc mpd_qln10*(result: ptr mpd_t; prec: mpd_ssize_t; status: ptr uint32_t) =
  var
    varcontext: mpd_context_t
    maxcontext: mpd_context_t
  MPD_NEW_STATIC(tmp, 0, 0, 0, 0)
  MPD_NEW_CONST(static10, 0, 0, 2, 1, 1, 10)
  var klist: array[MPD_MAX_PREC_LOG2, mpd_ssize_t]
  var rnd: mpd_uint_t
  var shift: mpd_ssize_t
  var i: cint
  assert(prec >= 1)
  shift = MPD_MINALLOC_MAX * MPD_RDIGITS - prec
  shift = if shift < 0: 0 else: shift
  rnd = mpd_qshiftr(result, addr(_mpd_ln10), shift, status)
  if rnd == MPD_UINT_MAX:
    mpd_seterror(result, MPD_Malloc_error, status)
    return
  result.exp = -(result.digits - 1)
  mpd_maxcontext(addr(maxcontext))
  if prec < MPD_MINALLOC_MAX * MPD_RDIGITS:
    maxcontext.prec = prec
    _mpd_apply_round_excess(result, rnd, addr(maxcontext), status)
    status[] = status[] or (MPD_Inexact or MPD_Rounded)
    return
  mpd_maxcontext(addr(varcontext))
  varcontext.round = MPD_ROUND_TRUNC
  i = ln_schedule_prec(klist, prec + 2, -result.exp)
  while i >= 0:
    varcontext.prec = 2 * klist[i] + 3
    result.flags = result.flags xor MPD_NEG
    _mpd_qexp(addr(tmp), result, addr(varcontext), status)
    result.flags = result.flags xor MPD_NEG
    mpd_qmul(addr(tmp), addr(static10), addr(tmp), addr(varcontext), status)
    mpd_qsub(addr(tmp), addr(tmp), addr(one), addr(maxcontext), status)
    mpd_qadd(result, result, addr(tmp), addr(maxcontext), status)
    if mpd_isspecial(result):
      break
    dec(i)
  mpd_del(addr(tmp))
  maxcontext.prec = prec
  mpd_qfinalize(result, addr(maxcontext), status)

##
##  Initial approximations for the ln() iteration. The values have the
##  following properties (established with both decimal.py and mpfr):
##
##  Index 0 - 400, logarithms of x in [1.00, 5.00]:
##    abs(lnapprox[i] * 10**-3 - log((i+100)/100)) < 10**-2
##    abs(lnapprox[i] * 10**-3 - log((i+1+100)/100)) < 10**-2
##
##  Index 401 - 899, logarithms of x in (0.500, 0.999]:
##    abs(-lnapprox[i] * 10**-3 - log((i+100)/1000)) < 10**-2
##    abs(-lnapprox[i] * 10**-3 - log((i+1+100)/1000)) < 10**-2
##

var lnapprox*: array[900, uint16_t] = [0, 10, 20, 30, 39, 49, 58, 68, 77, 86, 95, 104, 113, 122,
                                  131, 140, 148, 157, 166, 174, 182, 191, 199, 207, 215,
                                  223, 231, 239, 247, 255, 262, 270, 278, 285, 293, 300,
                                  308, 315, 322, 329, 336, 344, 351, 358, 365, 372, 378,
                                  385, 392, 399, 406, 412, 419, 425, 432, 438, 445, 451,
                                  457, 464, 470, 476, 482, 489, 495, 501, 507, 513, 519,
                                  525, 531, 536, 542, 548, 554, 560, 565, 571, 577, 582,
                                  588, 593, 599, 604, 610, 615, 621, 626, 631, 637, 642,
                                  647, 652, 658, 663, 668, 673, 678, 683, 688, 693, 698,
                                  703, 708, 713, 718, 723, 728, 732, 737, 742, 747, 751,
                                  756, 761, 766, 770, 775, 779, 784, 788, 793, 798, 802,
                                  806, 811, 815, 820, 824, 829, 833, 837, 842, 846, 850,
                                  854, 859, 863, 867, 871, 876, 880, 884, 888, 892, 896,
                                  900, 904, 908, 912, 916, 920, 924, 928, 932, 936, 940,
                                  944, 948, 952, 956, 959, 963, 967, 971, 975, 978, 982,
                                  986, 990, 993, 997, 1001, 1004, 1008, 1012, 1015, 1019,
                                  1022, 1026, 1030, 1033, 1037, 1040, 1044, 1047, 1051,
                                  1054, 1058, 1061, 1065, 1068, 1072, 1075, 1078, 1082,
                                  1085, 1089, 1092, 1095, 1099, 1102, 1105, 1109, 1112,
                                  1115, 1118, 1122, 1125, 1128, 1131, 1135, 1138, 1141,
                                  1144, 1147, 1151, 1154, 1157, 1160, 1163, 1166, 1169,
                                  1172, 1176, 1179, 1182, 1185, 1188, 1191, 1194, 1197,
                                  1200, 1203, 1206, 1209, 1212, 1215, 1218, 1221, 1224,
                                  1227, 1230, 1233, 1235, 1238, 1241, 1244, 1247, 1250,
                                  1253, 1256, 1258, 1261, 1264, 1267, 1270, 1273, 1275,
                                  1278, 1281, 1284, 1286, 1289, 1292, 1295, 1297, 1300,
                                  1303, 1306, 1308, 1311, 1314, 1316, 1319, 1322, 1324,
                                  1327, 1330, 1332, 1335, 1338, 1340, 1343, 1345, 1348,
                                  1351, 1353, 1356, 1358, 1361, 1364, 1366, 1369, 1371,
                                  1374, 1376, 1379, 1381, 1384, 1386, 1389, 1391, 1394,
                                  1396, 1399, 1401, 1404, 1406, 1409, 1411, 1413, 1416,
                                  1418, 1421, 1423, 1426, 1428, 1430, 1433, 1435, 1437,
                                  1440, 1442, 1445, 1447, 1449, 1452, 1454, 1456, 1459,
                                  1461, 1463, 1466, 1468, 1470, 1472, 1475, 1477, 1479,
                                  1482, 1484, 1486, 1488, 1491, 1493, 1495, 1497, 1500,
                                  1502, 1504, 1506, 1509, 1511, 1513, 1515, 1517, 1520,
                                  1522, 1524, 1526, 1528, 1530, 1533, 1535, 1537, 1539,
                                  1541, 1543, 1545, 1548, 1550, 1552, 1554, 1556, 1558,
                                  1560, 1562, 1564, 1567, 1569, 1571, 1573, 1575, 1577,
                                  1579, 1581, 1583, 1585, 1587, 1589, 1591, 1593, 1595,
                                  1597, 1599, 1601, 1603, 1605, 1607, 1609, 691, 689,
                                  687, 685, 683, 681, 679, 677, 675, 673, 671, 669, 668,
                                  666, 664, 662, 660, 658, 656, 654, 652, 650, 648, 646,
                                  644, 642, 641, 639, 637, 635, 633, 631, 629, 627, 626,
                                  624, 622, 620, 618, 616, 614, 612, 611, 609, 607, 605,
                                  603, 602, 600, 598, 596, 594, 592, 591, 589, 587, 585,
                                  583, 582, 580, 578, 576, 574, 573, 571, 569, 567, 566,
                                  564, 562, 560, 559, 557, 555, 553, 552, 550, 548, 546,
                                  545, 543, 541, 540, 538, 536, 534, 533, 531, 529, 528,
                                  526, 524, 523, 521, 519, 518, 516, 514, 512, 511, 509,
                                  508, 506, 504, 502, 501, 499, 498, 496, 494, 493, 491,
                                  489, 488, 486, 484, 483, 481, 480, 478, 476, 475, 473,
                                  472, 470, 468, 467, 465, 464, 462, 460, 459, 457, 456,
                                  454, 453, 451, 449, 448, 446, 445, 443, 442, 440, 438,
                                  437, 435, 434, 432, 431, 429, 428, 426, 425, 423, 422,
                                  420, 419, 417, 416, 414, 412, 411, 410, 408, 406, 405,
                                  404, 402, 400, 399, 398, 396, 394, 393, 392, 390, 389,
                                  387, 386, 384, 383, 381, 380, 378, 377, 375, 374, 372,
                                  371, 370, 368, 367, 365, 364, 362, 361, 360, 358, 357,
                                  355, 354, 352, 351, 350, 348, 347, 345, 344, 342, 341,
                                  340, 338, 337, 336, 334, 333, 331, 330, 328, 327, 326,
                                  324, 323, 322, 320, 319, 318, 316, 315, 313, 312, 311,
                                  309, 308, 306, 305, 304, 302, 301, 300, 298, 297, 296,
                                  294, 293, 292, 290, 289, 288, 286, 285, 284, 282, 281,
                                  280, 278, 277, 276, 274, 273, 272, 270, 269, 268, 267,
                                  265, 264, 263, 261, 260, 259, 258, 256, 255, 254, 252,
                                  251, 250, 248, 247, 246, 245, 243, 242, 241, 240, 238,
                                  237, 236, 234, 233, 232, 231, 229, 228, 227, 226, 224,
                                  223, 222, 221, 219, 218, 217, 216, 214, 213, 212, 211,
                                  210, 208, 207, 206, 205, 203, 202, 201, 200, 198, 197,
                                  196, 195, 194, 192, 191, 190, 189, 188, 186, 185, 184,
                                  183, 182, 180, 179, 178, 177, 176, 174, 173, 172, 171,
                                  170, 168, 167, 166, 165, 164, 162, 161, 160, 159, 158,
                                  157, 156, 154, 153, 152, 151, 150, 148, 147, 146, 145,
                                  144, 143, 142, 140, 139, 138, 137, 136, 135, 134, 132,
                                  131, 130, 129, 128, 127, 126, 124, 123, 122, 121, 120,
                                  119, 118, 116, 115, 114, 113, 112, 111, 110, 109, 108,
                                  106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 95, 94, 93,
                                  92, 91, 90, 89, 88, 87, 86, 84, 83, 82, 81, 80, 79, 78, 77,
                                  76, 75, 74, 73, 72, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61,
                                  60, 59, 58, 57, 56, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45,
                                  44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 31, 30, 29,
                                  28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14,
                                  13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

##  index 0 - 400: log((i+100)/100) * 1000
##
##  Internal ln() function that does not check for specials, zero or one.
##  Relative error: abs(result - log(a)) < 0.1 * 10**-prec * abs(log(a))
##

proc _mpd_qln*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  var
    varcontext: mpd_context_t
    maxcontext: mpd_context_t
  var z: ptr mpd_t = result
  MPD_NEW_STATIC(v, 0, 0, 0, 0)
  MPD_NEW_STATIC(vtmp, 0, 0, 0, 0)
  MPD_NEW_STATIC(tmp, 0, 0, 0, 0)
  var klist: array[MPD_MAX_PREC_LOG2, mpd_ssize_t]
  var
    maxprec: mpd_ssize_t
    shift: mpd_ssize_t
    t: mpd_ssize_t
  var
    a_digits: mpd_ssize_t
    a_exp: mpd_ssize_t
  var
    dummy: mpd_uint_t
    x: mpd_uint_t
  var i: cint
  assert(not mpd_isspecial(a) and not mpd_iszerocoeff(a))
  ##
  ##  We are calculating ln(a) = ln(v * 10^t) = ln(v) + t*ln(10),
  ##  where 0.5 < v <= 5.
  ##
  if not mpd_qcopy(addr(v), a, status):
    mpd_seterror(result, MPD_Malloc_error, status)
    break finish
  _mpd_get_msdigits(addr(dummy), addr(x), addr(v), 3)
  if x < 10:
    x = x * 10
  if x < 100:
    x = x * 10
  dec(x, 100)
  ##  a may equal z
  a_digits = a.digits
  a_exp = a.exp
  mpd_minalloc(z)
  mpd_clear_flags(z)
  z.data[0] = lnapprox[x]
  z.len = 1
  z.exp = -3
  mpd_setdigits(z)
  if x <= 400:
    ##  Reduce the input operand to 1.00 <= v <= 5.00. Let y = x + 100,
    ##  so 100 <= y <= 500. Since y contains the most significant digits
    ##  of v, y/100 <= v < (y+1)/100 and abs(z - log(v)) < 10**-2.
    v.exp = -(a_digits - 1)
    t = a_exp + a_digits - 1
  else:
    ##  Reduce the input operand to 0.500 < v <= 0.999. Let y = x + 100,
    ##  so 500 < y <= 999. Since y contains the most significant digits
    ##  of v, y/1000 <= v < (y+1)/1000 and abs(z - log(v)) < 10**-2.
    v.exp = -a_digits
    t = a_exp + a_digits
    mpd_set_negative(z)
  mpd_maxcontext(addr(maxcontext))
  mpd_maxcontext(addr(varcontext))
  varcontext.round = MPD_ROUND_TRUNC
  maxprec = ctx.prec + 2
  if t == 0 and (x <= 15 or x >= 800):
    ##  0.900 <= v <= 1.15: Estimate the magnitude of the logarithm.
    ##  If ln(v) will underflow, skip the loop. Otherwise, adjust the
    ##  precision upwards in order to obtain a sufficient number of
    ##  significant digits.
    ##
    ##    Case v > 1:
    ##       abs((v-1)/10) < abs((v-1)/v) < abs(ln(v)) < abs(v-1)
    ##    Case v < 1:
    ##       abs(v-1) < abs(ln(v)) < abs((v-1)/v) < abs((v-1)*10)
    ##
    var cmp: cint = _mpd_cmp(addr(v), addr(one))
    ##  Upper bound (assume v > 1): abs(v-1), unrounded
    _mpd_qsub(addr(tmp), addr(v), addr(one), addr(maxcontext),
              addr(maxcontext.status))
    if maxcontext.status and MPD_Errors:
      mpd_seterror(result, MPD_Malloc_error, status)
      break finish
    if cmp < 0:
      ##  v < 1: abs((v-1)*10)
      inc(tmp.exp, 1)
    if mpd_adjexp(addr(tmp)) < mpd_etiny(ctx):
      ##  The upper bound is less than etiny: Underflow to zero
      _settriple(result, (cmp < 0), 1, mpd_etiny(ctx) - 1)
      break finish
    dec(tmp.exp, 1)
    if mpd_adjexp(addr(tmp)) < 0:
      ##  Absolute error of the loop: abs(z - log(v)) < 10**-p. If
      ##  p = ctx->prec+2-adjexp(lower), then the relative error of
      ##  the result is (using 10**adjexp(x) <= abs(x)):
      ##
      ##    abs(z - log(v)) / abs(log(v)) < 10**-p / abs(log(v))
      ##                                  <= 10**(-ctx->prec-2)
      ##
      maxprec = maxprec - mpd_adjexp(addr(tmp))
  i = ln_schedule_prec(klist, maxprec, 2)
  while i >= 0:
    varcontext.prec = 2 * klist[i] + 3
    z.flags = z.flags xor MPD_NEG
    _mpd_qexp(addr(tmp), z, addr(varcontext), status)
    z.flags = z.flags xor MPD_NEG
    if v.digits > varcontext.prec:
      shift = v.digits - varcontext.prec
      mpd_qshiftr(addr(vtmp), addr(v), shift, status)
      inc(vtmp.exp, shift)
      mpd_qmul(addr(tmp), addr(vtmp), addr(tmp), addr(varcontext), status)
    else:
      mpd_qmul(addr(tmp), addr(v), addr(tmp), addr(varcontext), status)
    mpd_qsub(addr(tmp), addr(tmp), addr(one), addr(maxcontext), status)
    mpd_qadd(z, z, addr(tmp), addr(maxcontext), status)
    if mpd_isspecial(z):
      break
    dec(i)
  ##
  ##  Case t == 0:
  ##     t * log(10) == 0, the result does not change and the analysis
  ##     above applies. If v < 0.900 or v > 1.15, the relative error is
  ##     less than 10**(-ctx.prec-1).
  ##  Case t != 0:
  ##       z := approx(log(v))
  ##       y := approx(log(10))
  ##       p := maxprec = ctx->prec + 2
  ##    Absolute errors:
  ##       1) abs(z - log(v)) < 10**-p
  ##       2) abs(y - log(10)) < 10**-p
  ##    The multiplication is exact, so:
  ##       3) abs(t*y - t*log(10)) < t*10**-p
  ##    The sum is exact, so:
  ##       4) abs((z + t*y) - (log(v) + t*log(10))) < (abs(t) + 1) * 10**-p
  ##    Bounds for log(v) and log(10):
  ##       5) -7/10 < log(v) < 17/10
  ##       6) 23/10 < log(10) < 24/10
  ##    Using 4), 5), 6) and t != 0, the relative error is:
  ##
  ##       7) relerr < ((abs(t) + 1)*10**-p) / abs(log(v) + t*log(10))
  ##                 < 0.5 * 10**(-p + 1) = 0.5 * 10**(-ctx->prec-1)
  ##
  mpd_qln10(addr(v), maxprec + 1, status)
  mpd_qmul_ssize(addr(tmp), addr(v), t, addr(maxcontext), status)
  mpd_qadd(result, addr(tmp), z, addr(maxcontext), status)
  status[] = status[] or (MPD_Inexact or MPD_Rounded)
  mpd_del(addr(v))
  mpd_del(addr(vtmp))
  mpd_del(addr(tmp))

##  ln(a)

proc mpd_qln*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
             status: ptr uint32_t) =
  var workctx: mpd_context_t
  var
    adjexp: mpd_ssize_t
    t: mpd_ssize_t
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    if mpd_isnegative(a):
      mpd_seterror(result, MPD_Invalid_operation, status)
      return
    mpd_setspecial(result, MPD_POS, MPD_INF)
    return
  if mpd_iszerocoeff(a):
    mpd_setspecial(result, MPD_NEG, MPD_INF)
    return
  if mpd_isnegative(a):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if _mpd_cmp(a, addr(one)) == 0:
    _settriple(result, MPD_POS, 0, 0)
    return
  adjexp = mpd_adjexp(a)
  t = if (adjexp < 0): -adjexp - 1 else: adjexp
  t = t * 2
  if mpd_exp_digits(t) - 1 > ctx.emax:
    status[] = status[] or (MPD_Overflow or MPD_Inexact or MPD_Rounded)
    mpd_setspecial(result, (adjexp < 0), MPD_INF)
    return
  workctx = ctx[]
  workctx.round = MPD_ROUND_HALF_EVEN
  if ctx.allcr:
    MPD_NEW_STATIC(t1, 0, 0, 0, 0)
    MPD_NEW_STATIC(t2, 0, 0, 0, 0)
    MPD_NEW_STATIC(ulp, 0, 0, 0, 0)
    MPD_NEW_STATIC(aa, 0, 0, 0, 0)
    var prec: mpd_ssize_t
    if result == a:
      if not mpd_qcopy(addr(aa), a, status):
        mpd_seterror(result, MPD_Malloc_error, status)
        return
      a = addr(aa)
    workctx.clamp = 0
    prec = ctx.prec + 3
    while 1:
      workctx.prec = prec
      _mpd_qln(result, a, addr(workctx), status)
      _ssettriple(addr(ulp), MPD_POS, 1, result.exp + result.digits - workctx.prec)
      workctx.prec = ctx.prec
      mpd_qadd(addr(t1), result, addr(ulp), addr(workctx), addr(workctx.status))
      mpd_qsub(addr(t2), result, addr(ulp), addr(workctx), addr(workctx.status))
      if mpd_isspecial(result) or mpd_iszerocoeff(result) or
          mpd_qcmp(addr(t1), addr(t2), status) == 0:
        workctx.clamp = ctx.clamp
        mpd_check_underflow(result, addr(workctx), status)
        mpd_qfinalize(result, addr(workctx), status)
        break
      inc(prec, MPD_RDIGITS)
    mpd_del(addr(t1))
    mpd_del(addr(t2))
    mpd_del(addr(ulp))
    mpd_del(addr(aa))
  else:
    _mpd_qln(result, a, addr(workctx), status)
    mpd_check_underflow(result, addr(workctx), status)
    mpd_qfinalize(result, addr(workctx), status)

##
##  Internal log10() function that does not check for specials, zero or one.
##  Case SKIP_FINALIZE:
##    Relative error: abs(result - log10(a)) < 0.1 * 10**-prec * abs(log10(a))
##  Case DO_FINALIZE:
##    Ulp error: abs(result - log10(a)) < ulp(log10(a))
##

const
  SKIP_FINALIZE* = 0
  DO_FINALIZE* = 1

proc _mpd_qlog10*(action: cint; result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                 status: ptr uint32_t) =
  var workctx: mpd_context_t
  MPD_NEW_STATIC(ln10, 0, 0, 0, 0)
  mpd_maxcontext(addr(workctx))
  workctx.prec = ctx.prec + 3
  ##  relative error: 0.1 * 10**(-p-3). The specific underflow shortcut
  ##  in _mpd_qln() does not change the final result.
  _mpd_qln(result, a, addr(workctx), status)
  ##  relative error: 5 * 10**(-p-3)
  mpd_qln10(addr(ln10), workctx.prec, status)
  if action == DO_FINALIZE:
    workctx = ctx[]
    workctx.round = MPD_ROUND_HALF_EVEN
  _mpd_qdiv(NO_IDEAL_EXP, result, result, addr(ln10), addr(workctx), status)
  mpd_del(addr(ln10))

##  log10(a)

proc mpd_qlog10*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                status: ptr uint32_t) =
  var workctx: mpd_context_t
  var
    adjexp: mpd_ssize_t
    t: mpd_ssize_t
  workctx = ctx[]
  workctx.round = MPD_ROUND_HALF_EVEN
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    if mpd_isnegative(a):
      mpd_seterror(result, MPD_Invalid_operation, status)
      return
    mpd_setspecial(result, MPD_POS, MPD_INF)
    return
  if mpd_iszerocoeff(a):
    mpd_setspecial(result, MPD_NEG, MPD_INF)
    return
  if mpd_isnegative(a):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if mpd_coeff_ispow10(a):
    var sign: uint8_t = 0
    adjexp = mpd_adjexp(a)
    if adjexp < 0:
      sign = 1
      adjexp = -adjexp
    _settriple(result, sign, adjexp, 0)
    mpd_qfinalize(result, addr(workctx), status)
    return
  adjexp = mpd_adjexp(a)
  t = if (adjexp < 0): -adjexp - 1 else: adjexp
  if mpd_exp_digits(t) - 1 > ctx.emax:
    status[] = status[] or (MPD_Overflow or MPD_Inexact or MPD_Rounded)
    mpd_setspecial(result, (adjexp < 0), MPD_INF)
    return
  if ctx.allcr:
    MPD_NEW_STATIC(t1, 0, 0, 0, 0)
    MPD_NEW_STATIC(t2, 0, 0, 0, 0)
    MPD_NEW_STATIC(ulp, 0, 0, 0, 0)
    MPD_NEW_STATIC(aa, 0, 0, 0, 0)
    var prec: mpd_ssize_t
    if result == a:
      if not mpd_qcopy(addr(aa), a, status):
        mpd_seterror(result, MPD_Malloc_error, status)
        return
      a = addr(aa)
    workctx.clamp = 0
    prec = ctx.prec + 3
    while 1:
      workctx.prec = prec
      _mpd_qlog10(SKIP_FINALIZE, result, a, addr(workctx), status)
      _ssettriple(addr(ulp), MPD_POS, 1, result.exp + result.digits - workctx.prec)
      workctx.prec = ctx.prec
      mpd_qadd(addr(t1), result, addr(ulp), addr(workctx), addr(workctx.status))
      mpd_qsub(addr(t2), result, addr(ulp), addr(workctx), addr(workctx.status))
      if mpd_isspecial(result) or mpd_iszerocoeff(result) or
          mpd_qcmp(addr(t1), addr(t2), status) == 0:
        workctx.clamp = ctx.clamp
        mpd_check_underflow(result, addr(workctx), status)
        mpd_qfinalize(result, addr(workctx), status)
        break
      inc(prec, MPD_RDIGITS)
    mpd_del(addr(t1))
    mpd_del(addr(t2))
    mpd_del(addr(ulp))
    mpd_del(addr(aa))
  else:
    _mpd_qlog10(DO_FINALIZE, result, a, addr(workctx), status)
    mpd_check_underflow(result, addr(workctx), status)

##
##  Maximum of the two operands. Attention: If one operand is a quiet NaN and the
##  other is numeric, the numeric operand is returned. This may not be what one
##  expects.
##

proc mpd_qmax*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  var c: cint
  if mpd_isqnan(a) and not mpd_isnan(b):
    mpd_qcopy(result, b, status)
  elif mpd_isqnan(b) and not mpd_isnan(a):
    mpd_qcopy(result, a, status)
  elif mpd_qcheck_nans(result, a, b, ctx, status):
    return
  else:
    c = _mpd_cmp(a, b)
    if c == 0:
      c = _mpd_cmp_numequal(a, b)
    if c < 0:
      mpd_qcopy(result, b, status)
    else:
      mpd_qcopy(result, a, status)
  mpd_qfinalize(result, ctx, status)

##
##  Maximum magnitude: Same as mpd_max(), but compares the operands with their
##  sign ignored.
##

proc mpd_qmax_mag*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  var c: cint
  if mpd_isqnan(a) and not mpd_isnan(b):
    mpd_qcopy(result, b, status)
  elif mpd_isqnan(b) and not mpd_isnan(a):
    mpd_qcopy(result, a, status)
  elif mpd_qcheck_nans(result, a, b, ctx, status):
    return
  else:
    c = _mpd_cmp_abs(a, b)
    if c == 0:
      c = _mpd_cmp_numequal(a, b)
    if c < 0:
      mpd_qcopy(result, b, status)
    else:
      mpd_qcopy(result, a, status)
  mpd_qfinalize(result, ctx, status)

##
##  Minimum of the two operands. Attention: If one operand is a quiet NaN and the
##  other is numeric, the numeric operand is returned. This may not be what one
##  expects.
##

proc mpd_qmin*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  var c: cint
  if mpd_isqnan(a) and not mpd_isnan(b):
    mpd_qcopy(result, b, status)
  elif mpd_isqnan(b) and not mpd_isnan(a):
    mpd_qcopy(result, a, status)
  elif mpd_qcheck_nans(result, a, b, ctx, status):
    return
  else:
    c = _mpd_cmp(a, b)
    if c == 0:
      c = _mpd_cmp_numequal(a, b)
    if c < 0:
      mpd_qcopy(result, a, status)
    else:
      mpd_qcopy(result, b, status)
  mpd_qfinalize(result, ctx, status)

##
##  Minimum magnitude: Same as mpd_min(), but compares the operands with their
##  sign ignored.
##

proc mpd_qmin_mag*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  var c: cint
  if mpd_isqnan(a) and not mpd_isnan(b):
    mpd_qcopy(result, b, status)
  elif mpd_isqnan(b) and not mpd_isnan(a):
    mpd_qcopy(result, a, status)
  elif mpd_qcheck_nans(result, a, b, ctx, status):
    return
  else:
    c = _mpd_cmp_abs(a, b)
    if c == 0:
      c = _mpd_cmp_numequal(a, b)
    if c < 0:
      mpd_qcopy(result, a, status)
    else:
      mpd_qcopy(result, b, status)
  mpd_qfinalize(result, ctx, status)

##  Minimum space needed for the result array in _karatsuba_rec().

proc _kmul_resultsize*(la: mpd_size_t; lb: mpd_size_t): mpd_size_t {.inline.} =
  var
    n: mpd_size_t
    m: mpd_size_t
  n = add_size_t(la, lb)
  n = add_size_t(n, 1)
  m = (la + 1) div 2 + 1
  m = mul_size_t(m, 3)
  return if (m > n): m else: n

##  Work space needed in _karatsuba_rec(). lim >= 4

proc _kmul_worksize*(n: mpd_size_t; lim: mpd_size_t): mpd_size_t {.inline.} =
  var m: mpd_size_t
  if n <= lim:
    return 0
  m = (n + 1) div 2 + 1
  return add_size_t(mul_size_t(m, 2), _kmul_worksize(m, lim))

const
  MPD_KARATSUBA_BASECASE* = 16

##
##  Add the product of a and b to c.
##  c must be _kmul_resultsize(la, lb) in size.
##  w is used as a work array and must be _kmul_worksize(a, lim) in size.
##  Roman E. Maeder, Storage Allocation for the Karatsuba Integer Multiplication
##  Algorithm. In "Design and implementation of symbolic computation systems",
##  Springer, 1993, ISBN 354057235X, 9783540572350.
##

proc _karatsuba_rec*(c: ptr mpd_uint_t; a: ptr mpd_uint_t; b: ptr mpd_uint_t;
                    w: ptr mpd_uint_t; la: mpd_size_t; lb: mpd_size_t) =
  var
    m: mpd_size_t
    lt: mpd_size_t
  assert(la >= lb and lb > 0)
  assert(la <= MPD_KARATSUBA_BASECASE or w != nil)
  if la <= MPD_KARATSUBA_BASECASE:
    _mpd_basemul(c, a, b, la, lb)
    return
  m = (la + 1) div 2
  ##  ceil(la/2)
  ##  lb <= m < la
  if lb <= m:
    ##  lb can now be larger than la-m
    if lb > la - m:
      lt = lb + lb + 1
      ##  space needed for result array
      mpd_uint_zero(w, lt)
      ##  clear result array
      _karatsuba_rec(w, b, a + m, w + lt, lb, la - m)
      ##  b*ah
    else:
      lt = (la - m) + (la - m) + 1
      ##  space needed for result array
      mpd_uint_zero(w, lt)
      ##  clear result array
      _karatsuba_rec(w, a + m, b, w + lt, la - m, lb)
      ##  ah*b
    _mpd_baseaddto(c + m, w, (la - m) + lb)
    ##  add ah*b*B**m
    lt = m + m + 1
    ##  space needed for the result array
    mpd_uint_zero(w, lt)
    ##  clear result array
    _karatsuba_rec(w, a, b, w + lt, m, lb)
    ##  al*b
    _mpd_baseaddto(c, w, m + lb)
    ##  add al*b
    return
  memcpy(w, a, m * sizeof(w[]))
  w[m] = 0
  _mpd_baseaddto(w, a + m, la - m)
  memcpy(w + (m + 1), b, m * sizeof(w[]))
  w[m + 1 + m] = 0
  _mpd_baseaddto(w + (m + 1), b + m, lb - m)
  _karatsuba_rec(c + m, w, w + (m + 1), w + 2 * (m + 1), m + 1, m + 1)
  lt = (la - m) + (la - m) + 1
  mpd_uint_zero(w, lt)
  _karatsuba_rec(w, a + m, b + m, w + lt, la - m, lb - m)
  _mpd_baseaddto(c + 2 * m, w, (la - m) + (lb - m))
  _mpd_basesubfrom(c + m, w, (la - m) + (lb - m))
  lt = m + m + 1
  mpd_uint_zero(w, lt)
  _karatsuba_rec(w, a, b, w + lt, m, m)
  _mpd_baseaddto(c, w, m + m)
  _mpd_basesubfrom(c + m, w, m + m)
  return

##
##  Multiply u and v, using Karatsuba multiplication. Returns a pointer
##  to the result or NULL in case of failure (malloc error).
##  Conditions: ulen >= vlen, ulen >= 4
##

proc _mpd_kmul*(u: ptr mpd_uint_t; v: ptr mpd_uint_t; ulen: mpd_size_t; vlen: mpd_size_t;
               rsize: ptr mpd_size_t): ptr mpd_uint_t =
  var
    result: ptr mpd_uint_t = nil
    w: ptr mpd_uint_t = nil
  var m: mpd_size_t
  assert(ulen >= 4)
  assert(ulen >= vlen)
  rsize[] = _kmul_resultsize(ulen, vlen)
  if (result = mpd_calloc(rsize[], sizeof(result[]))) == nil:
    return nil
  m = _kmul_worksize(ulen, MPD_KARATSUBA_BASECASE)
  if m and ((w = mpd_calloc(m, sizeof(w[]))) == nil):
    mpd_free(result)
    return nil
  _karatsuba_rec(result, u, v, w, ulen, vlen)
  if w:
    mpd_free(w)
  return result

##
##  Determine the minimum length for the number theoretic transform. Valid
##  transform lengths are 2**n or 3*2**n, where 2**n <= MPD_MAXTRANSFORM_2N.
##  The function finds the shortest length m such that rsize <= m.
##

proc _mpd_get_transform_len*(rsize: mpd_size_t): mpd_size_t {.inline.} =
  var log2rsize: mpd_size_t
  var
    x: mpd_size_t
    step: mpd_size_t
  assert(rsize >= 4)
  log2rsize = mpd_bsr(rsize)
  if rsize <= 1024:
    ##  2**n is faster in this range.
    x = (cast[mpd_size_t](1)) shl log2rsize
    return if (rsize == x): x else: x shl 1
  elif rsize <= MPD_MAXTRANSFORM_2N:
    x = (cast[mpd_size_t](1)) shl log2rsize
    if rsize == x:
      return x
    step = x shr 1
    inc(x, step)
    return if (rsize <= x): x else: x + step
  elif rsize <= MPD_MAXTRANSFORM_2N + MPD_MAXTRANSFORM_2N div 2:
    return MPD_MAXTRANSFORM_2N + MPD_MAXTRANSFORM_2N div 2
  elif rsize <= 3 * MPD_MAXTRANSFORM_2N:
    return 3 * MPD_MAXTRANSFORM_2N
  else:
    return MPD_SIZE_MAX

when defined(PPRO):
  when not defined(_MSC_VER):
    proc _mpd_get_control87*(): cushort {.inline.} =
      var cw: cushort
      ##  c2nim TODO __asm__ __volatile__ ("fnstcw %0" : "=m" (cw));
      return cw

    proc _mpd_set_control87*(cw: cushort) {.inline.} =
      ##  c2nim TODO __asm__ __volatile__ ("fldcw %0" : : "m" (cw));

  proc mpd_set_fenv*(): cuint =
    var cw: cuint
    when defined(_MSC_VER):
      var flags: cuint = _EM_INVALID or _EM_DENORMAL or _EM_ZERODIVIDE or _EM_OVERFLOW or
          _EM_UNDERFLOW or _EM_INEXACT or _RC_CHOP or _PC_64
      var mask: cuint = _MCW_EM or _MCW_RC or _MCW_PC
      var dummy: cuint
      __control87_2(0, 0, addr(cw), nil)
      __control87_2(flags, mask, addr(dummy), nil)
    else:
      cw = _mpd_get_control87()
      _mpd_set_control87(cw or 0x00000F3F)
    return cw

  proc mpd_restore_fenv*(cw: cuint) =
    when defined(_MSC_VER):
      var mask: cuint = _MCW_EM or _MCW_RC or _MCW_PC
      var dummy: cuint
      __control87_2(cw, mask, addr(dummy), nil)
    else:
      _mpd_set_control87(cast[cushort](cw))

##
##  Multiply u and v, using the fast number theoretic transform. Returns
##  a pointer to the result or NULL in case of failure (malloc error).
##

proc _mpd_fntmul*(u: ptr mpd_uint_t; v: ptr mpd_uint_t; ulen: mpd_size_t;
                 vlen: mpd_size_t; rsize: ptr mpd_size_t): ptr mpd_uint_t =
  var
    c1: ptr mpd_uint_t = nil
    c2: ptr mpd_uint_t = nil
    c3: ptr mpd_uint_t = nil
    vtmp: ptr mpd_uint_t = nil
  var n: mpd_size_t
  when defined(PPRO):
    var cw: cuint
    cw = mpd_set_fenv()
  rsize[] = add_size_t(ulen, vlen)
  if (n = _mpd_get_transform_len(rsize[])) == MPD_SIZE_MAX:
    break malloc_error
  if (c1 = mpd_calloc(n, sizeof(c1[]))) == nil:
    break malloc_error
  if (c2 = mpd_calloc(n, sizeof(c2[]))) == nil:
    break malloc_error
  if (c3 = mpd_calloc(n, sizeof(c3[]))) == nil:
    break malloc_error
  memcpy(c1, u, ulen * (sizeof(c1[])))
  memcpy(c2, u, ulen * (sizeof(c2[])))
  memcpy(c3, u, ulen * (sizeof(c3[])))
  if u == v:
    if not fnt_autoconvolute(c1, n, P1) or not fnt_autoconvolute(c2, n, P2) or
        not fnt_autoconvolute(c3, n, P3):
      break malloc_error
  else:
    if (vtmp = mpd_calloc(n, sizeof(vtmp[]))) == nil:
      break malloc_error
    memcpy(vtmp, v, vlen * (sizeof(vtmp[])))
    if not fnt_convolute(c1, vtmp, n, P1):
      mpd_free(vtmp)
      break malloc_error
    memcpy(vtmp, v, vlen * (sizeof(vtmp[])))
    mpd_uint_zero(vtmp + vlen, n - vlen)
    if not fnt_convolute(c2, vtmp, n, P2):
      mpd_free(vtmp)
      break malloc_error
    memcpy(vtmp, v, vlen * (sizeof(vtmp[])))
    mpd_uint_zero(vtmp + vlen, n - vlen)
    if not fnt_convolute(c3, vtmp, n, P3):
      mpd_free(vtmp)
      break malloc_error
    mpd_free(vtmp)
  crt3(c1, c2, c3, rsize[])
  when defined(PPRO):
    mpd_restore_fenv(cw)
  if c2:
    mpd_free(c2)
  if c3:
    mpd_free(c3)
  return c1
  if c1:
    mpd_free(c1)
  c1 = nil
  break `out`

##
##  Karatsuba multiplication with FNT/basemul as the base case.
##

proc _karatsuba_rec_fnt*(c: ptr mpd_uint_t; a: ptr mpd_uint_t; b: ptr mpd_uint_t;
                        w: ptr mpd_uint_t; la: mpd_size_t; lb: mpd_size_t): cint =
  var
    m: mpd_size_t
    lt: mpd_size_t
  assert(la >= lb and lb > 0)
  assert(la <= 3 * (MPD_MAXTRANSFORM_2N div 2) or w != nil)
  if la <= 3 * (MPD_MAXTRANSFORM_2N div 2):
    if lb <= 192:
      _mpd_basemul(c, b, a, lb, la)
    else:
      var result: ptr mpd_uint_t
      var dummy: mpd_size_t
      if (result = _mpd_fntmul(a, b, la, lb, addr(dummy))) == nil:
        return 0
      memcpy(c, result, (la + lb) * (sizeof(result[])))
      mpd_free(result)
    return 1
  m = (la + 1) div 2
  ##  ceil(la/2)
  ##  lb <= m < la
  if lb <= m:
    ##  lb can now be larger than la-m
    if lb > la - m:
      lt = lb + lb + 1
      ##  space needed for result array
      mpd_uint_zero(w, lt)
      ##  clear result array
      if not _karatsuba_rec_fnt(w, b, a + m, w + lt, lb, la - m):
        ##  b*ah
        return 0
        ##  GCOV_UNLIKELY
    else:
      lt = (la - m) + (la - m) + 1
      ##  space needed for result array
      mpd_uint_zero(w, lt)
      ##  clear result array
      if not _karatsuba_rec_fnt(w, a + m, b, w + lt, la - m, lb):
        ##  ah*b
        return 0
        ##  GCOV_UNLIKELY
    _mpd_baseaddto(c + m, w, (la - m) + lb)
    ##  add ah*b*B**m
    lt = m + m + 1
    ##  space needed for the result array
    mpd_uint_zero(w, lt)
    ##  clear result array
    if not _karatsuba_rec_fnt(w, a, b, w + lt, m, lb):
      ##  al*b
      return 0
      ##  GCOV_UNLIKELY
    _mpd_baseaddto(c, w, m + lb)
    ##  add al*b
    return 1
  memcpy(w, a, m * sizeof(w[]))
  w[m] = 0
  _mpd_baseaddto(w, a + m, la - m)
  memcpy(w + (m + 1), b, m * sizeof(w[]))
  w[m + 1 + m] = 0
  _mpd_baseaddto(w + (m + 1), b + m, lb - m)
  if not _karatsuba_rec_fnt(c + m, w, w + (m + 1), w + 2 * (m + 1), m + 1, m + 1):
    return 0
    ##  GCOV_UNLIKELY
  lt = (la - m) + (la - m) + 1
  mpd_uint_zero(w, lt)
  if not _karatsuba_rec_fnt(w, a + m, b + m, w + lt, la - m, lb - m):
    return 0
    ##  GCOV_UNLIKELY
  _mpd_baseaddto(c + 2 * m, w, (la - m) + (lb - m))
  _mpd_basesubfrom(c + m, w, (la - m) + (lb - m))
  lt = m + m + 1
  mpd_uint_zero(w, lt)
  if not _karatsuba_rec_fnt(w, a, b, w + lt, m, m):
    return 0
    ##  GCOV_UNLIKELY
  _mpd_baseaddto(c, w, m + m)
  _mpd_basesubfrom(c + m, w, m + m)
  return 1

##
##  Multiply u and v, using Karatsuba multiplication with the FNT as the
##  base case. Returns a pointer to the result or NULL in case of failure
##  (malloc error). Conditions: ulen >= vlen, ulen >= 4.
##

proc _mpd_kmul_fnt*(u: ptr mpd_uint_t; v: ptr mpd_uint_t; ulen: mpd_size_t;
                   vlen: mpd_size_t; rsize: ptr mpd_size_t): ptr mpd_uint_t =
  var
    result: ptr mpd_uint_t = nil
    w: ptr mpd_uint_t = nil
  var m: mpd_size_t
  assert(ulen >= 4)
  assert(ulen >= vlen)
  rsize[] = _kmul_resultsize(ulen, vlen)
  if (result = mpd_calloc(rsize[], sizeof(result[]))) == nil:
    return nil
  m = _kmul_worksize(ulen, 3 * (MPD_MAXTRANSFORM_2N div 2))
  if m and ((w = mpd_calloc(m, sizeof(w[]))) == nil):
    mpd_free(result)
    ##  GCOV_UNLIKELY
    return nil
    ##  GCOV_UNLIKELY
  if not _karatsuba_rec_fnt(result, u, v, w, ulen, vlen):
    mpd_free(result)
    result = nil
  if w:
    mpd_free(w)
  return result

##  Deal with the special cases of multiplying infinities.

proc _mpd_qmul_inf*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; status: ptr uint32_t) =
  if mpd_isinfinite(a):
    if mpd_iszero(b):
      mpd_seterror(result, MPD_Invalid_operation, status)
    else:
      mpd_setspecial(result, mpd_sign(a) xor mpd_sign(b), MPD_INF)
    return
  assert(mpd_isinfinite(b))
  if mpd_iszero(a):
    mpd_seterror(result, MPD_Invalid_operation, status)
  else:
    mpd_setspecial(result, mpd_sign(a) xor mpd_sign(b), MPD_INF)

##
##  Internal function: Multiply a and b. _mpd_qmul deals with specials but
##  does NOT finalize the result. This is for use in mpd_fma().
##

proc _mpd_qmul*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) {.inline.} =
  var
    big: ptr mpd_t = a
    small: ptr mpd_t = b
  var rdata: ptr mpd_uint_t = nil
  var rbuf: array[MPD_MINALLOC_MAX, mpd_uint_t]
  var
    rsize: mpd_size_t
    i: mpd_size_t
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      return
    _mpd_qmul_inf(result, a, b, status)
    return
  if small.len > big.len:
    _mpd_ptrswap(addr(big), addr(small))
  rsize = big.len + small.len
  if big.len == 1:
    _mpd_singlemul(result.data, big.data[0], small.data[0])
    break finish
  if rsize <= cast[mpd_size_t](MPD_MINALLOC_MAX):
    if big.len == 2:
      _mpd_mul_2_le2(rbuf, big.data, small.data, small.len)
    else:
      mpd_uint_zero(rbuf, rsize)
      if small.len == 1:
        _mpd_shortmul(rbuf, big.data, big.len, small.data[0])
      else:
        _mpd_basemul(rbuf, small.data, big.data, small.len, big.len)
    if not mpd_qresize(result, rsize, status):
      return
    i = 0
    while i < rsize:
      result.data[i] = rbuf[i]
      inc(i)
    break finish
  if small.len <= 256:
    rdata = mpd_calloc(rsize, sizeof(rdata[]))
    if rdata != nil:
      if small.len == 1:
        _mpd_shortmul(rdata, big.data, big.len, small.data[0])
      else:
        _mpd_basemul(rdata, small.data, big.data, small.len, big.len)
  elif rsize <= 1024:
    rdata = _mpd_kmul(big.data, small.data, big.len, small.len, addr(rsize))
  elif rsize <= 3 * MPD_MAXTRANSFORM_2N:
    rdata = _mpd_fntmul(big.data, small.data, big.len, small.len, addr(rsize))
  else:
    rdata = _mpd_kmul_fnt(big.data, small.data, big.len, small.len, addr(rsize))
  if rdata == nil:
    mpd_seterror(result, MPD_Malloc_error, status)
    return
  if mpd_isdynamic_data(result):
    mpd_free(result.data)
  result.data = rdata
  result.alloc = rsize
  mpd_set_dynamic_data(result)
  mpd_set_flags(result, mpd_sign(a) xor mpd_sign(b))
  result.exp = big.exp + small.exp
  result.len = _mpd_real_size(result.data, rsize)
  ##  resize to smaller cannot fail
  mpd_qresize(result, result.len, status)
  mpd_setdigits(result)

##  Multiply a and b.

proc mpd_qmul*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  _mpd_qmul(result, a, b, ctx, status)
  mpd_qfinalize(result, ctx, status)

##  Multiply a and b. Set NaN/Invalid_operation if the result is inexact.

proc _mpd_qmul_exact*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                     ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  mpd_qmul(result, a, b, ctx, addr(workstatus))
  status[] = status[] or workstatus
  if workstatus and (MPD_Inexact or MPD_Rounded or MPD_Clamped):
    mpd_seterror(result, MPD_Invalid_operation, status)

##  Multiply decimal and mpd_ssize_t.

proc mpd_qmul_ssize*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_ssize_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  mpd_maxcontext(addr(maxcontext))
  mpd_qsset_ssize(addr(bb), b, addr(maxcontext), status)
  mpd_qmul(result, a, addr(bb), ctx, status)
  mpd_del(addr(bb))

##  Multiply decimal and mpd_uint_t.

proc mpd_qmul_uint*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_uint_t;
                   ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(bb, 0, 0, 0, 0)
  mpd_maxcontext(addr(maxcontext))
  mpd_qsset_uint(addr(bb), b, addr(maxcontext), status)
  mpd_qmul(result, a, addr(bb), ctx, status)
  mpd_del(addr(bb))

proc mpd_qmul_i32*(result: ptr mpd_t; a: ptr mpd_t; b: int32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qmul_ssize(result, a, b, ctx, status)

proc mpd_qmul_u32*(result: ptr mpd_t; a: ptr mpd_t; b: uint32_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  mpd_qmul_uint(result, a, b, ctx, status)

when defined(CONFIG_64):
  proc mpd_qmul_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    mpd_qmul_ssize(result, a, b, ctx, status)

  proc mpd_qmul_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
    mpd_qmul_uint(result, a, b, ctx, status)

elif not defined(LEGACY_COMPILER):
  ##  Multiply decimal and int64_t.
  proc mpd_qmul_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    MPD_NEW_STATIC(bb, 0, 0, 0, 0)
    mpd_maxcontext(addr(maxcontext))
    mpd_qset_i64(addr(bb), b, addr(maxcontext), status)
    mpd_qmul(result, a, addr(bb), ctx, status)
    mpd_del(addr(bb))

  ##  Multiply decimal and uint64_t.
  proc mpd_qmul_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
    var maxcontext: mpd_context_t
    MPD_NEW_STATIC(bb, 0, 0, 0, 0)
    mpd_maxcontext(addr(maxcontext))
    mpd_qset_u64(addr(bb), b, addr(maxcontext), status)
    mpd_qmul(result, a, addr(bb), ctx, status)
    mpd_del(addr(bb))

##  Like the minus operator.

proc mpd_qminus*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                status: ptr uint32_t) =
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
  if mpd_iszero(a) and ctx.round != MPD_ROUND_FLOOR:
    mpd_qcopy_abs(result, a, status)
  else:
    mpd_qcopy_negate(result, a, status)
  mpd_qfinalize(result, ctx, status)

##  Like the plus operator.

proc mpd_qplus*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) =
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
  if mpd_iszero(a) and ctx.round != MPD_ROUND_FLOOR:
    mpd_qcopy_abs(result, a, status)
  else:
    mpd_qcopy(result, a, status)
  mpd_qfinalize(result, ctx, status)

##  The largest representable number that is smaller than the operand.

proc mpd_qnext_minus*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
  var workctx: mpd_context_t
  MPD_NEW_CONST(tiny, MPD_POS, mpd_etiny(ctx) - 1, 1, 1, 1, 1)
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    assert(mpd_isinfinite(a))
    if mpd_isnegative(a):
      mpd_qcopy(result, a, status)
      return
    else:
      mpd_clear_flags(result)
      mpd_qmaxcoeff(result, ctx, status)
      if mpd_isnan(result):
        return
      result.exp = mpd_etop(ctx)
      return
  mpd_workcontext(addr(workctx), ctx)
  workctx.round = MPD_ROUND_FLOOR
  if not mpd_qcopy(result, a, status):
    return
  mpd_qfinalize(result, addr(workctx), addr(workctx.status))
  if workctx.status and (MPD_Inexact or MPD_Errors):
    status[] = status[] or (workctx.status and MPD_Errors)
    return
  workctx.status = 0
  mpd_qsub(result, a, addr(tiny), addr(workctx), addr(workctx.status))
  status[] = status[] or (workctx.status and MPD_Errors)

##  The smallest representable number that is larger than the operand.

proc mpd_qnext_plus*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                    status: ptr uint32_t) =
  var workctx: mpd_context_t
  MPD_NEW_CONST(tiny, MPD_POS, mpd_etiny(ctx) - 1, 1, 1, 1, 1)
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    assert(mpd_isinfinite(a))
    if mpd_ispositive(a):
      mpd_qcopy(result, a, status)
    else:
      mpd_clear_flags(result)
      mpd_qmaxcoeff(result, ctx, status)
      if mpd_isnan(result):
        return
      mpd_set_flags(result, MPD_NEG)
      result.exp = mpd_etop(ctx)
    return
  mpd_workcontext(addr(workctx), ctx)
  workctx.round = MPD_ROUND_CEILING
  if not mpd_qcopy(result, a, status):
    return
  mpd_qfinalize(result, addr(workctx), addr(workctx.status))
  if workctx.status and (MPD_Inexact or MPD_Errors):
    status[] = status[] or (workctx.status and MPD_Errors)
    return
  workctx.status = 0
  mpd_qadd(result, a, addr(tiny), addr(workctx), addr(workctx.status))
  status[] = status[] or (workctx.status and MPD_Errors)

##
##  The number closest to the first operand that is in the direction towards
##  the second operand.
##

proc mpd_qnext_toward*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                      ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var c: cint
  if mpd_qcheck_nans(result, a, b, ctx, status):
    return
  c = _mpd_cmp(a, b)
  if c == 0:
    mpd_qcopy_sign(result, a, b, status)
    return
  if c < 0:
    mpd_qnext_plus(result, a, ctx, status)
  else:
    mpd_qnext_minus(result, a, ctx, status)
  if mpd_isinfinite(result):
    status[] = status[] or (MPD_Overflow or MPD_Rounded or MPD_Inexact)
  elif mpd_adjexp(result) < ctx.emin:
    status[] = status[] or
        (MPD_Underflow or MPD_Subnormal or MPD_Rounded or MPD_Inexact)
    if mpd_iszero(result):
      status[] = status[] or MPD_Clamped

##
##  Internal function: Integer power with mpd_uint_t exponent. The function
##  can fail with MPD_Malloc_error.
##
##  The error is equal to the error incurred in k-1 multiplications. Assuming
##  the upper bound for the relative error in each operation:
##
##    abs(err) = 5 * 10**-prec
##    result = x**k * (1 + err)**(k-1)
##

proc _mpd_qpow_uint*(result: ptr mpd_t; base: ptr mpd_t; exp: mpd_uint_t;
                    resultsign: uint8_t; ctx: ptr mpd_context_t; status: ptr uint32_t) {.
    inline.} =
  var workstatus: uint32_t = 0
  var n: mpd_uint_t
  if exp == 0:
    _settriple(result, resultsign, 1, 0)
    ##  GCOV_NOT_REACHED
    return
    ##  GCOV_NOT_REACHED
  if not mpd_qcopy(result, base, status):
    return
  n = mpd_bits[mpd_bsr(exp)]
  while n = n shr 1:
    mpd_qmul(result, result, result, ctx, addr(workstatus))
    if exp and n:
      mpd_qmul(result, result, base, ctx, addr(workstatus))
    if mpd_isspecial(result) or
        (mpd_iszerocoeff(result) and (workstatus and MPD_Clamped)):
      break
  status[] = status[] or workstatus
  mpd_set_sign(result, resultsign)

##
##  Internal function: Integer power with mpd_t exponent, tbase and texp
##  are modified!! Function can fail with MPD_Malloc_error.
##
##  The error is equal to the error incurred in k multiplications. Assuming
##  the upper bound for the relative error in each operation:
##
##    abs(err) = 5 * 10**-prec
##    result = x**k * (1 + err)**k
##

proc _mpd_qpow_mpd*(result: ptr mpd_t; tbase: ptr mpd_t; texp: ptr mpd_t;
                   resultsign: uint8_t; ctx: ptr mpd_context_t; status: ptr uint32_t) {.
    inline.} =
  var workstatus: uint32_t = 0
  var maxctx: mpd_context_t
  MPD_NEW_CONST(two, 0, 0, 1, 1, 1, 2)
  mpd_maxcontext(addr(maxctx))
  ##  resize to smaller cannot fail
  mpd_qcopy(result, addr(one), status)
  while not mpd_iszero(texp):
    if mpd_isodd(texp):
      mpd_qmul(result, result, tbase, ctx, addr(workstatus))
      status[] = status[] or workstatus
      if mpd_isspecial(result) or
          (mpd_iszerocoeff(result) and (workstatus and MPD_Clamped)):
        break
    mpd_qmul(tbase, tbase, tbase, ctx, addr(workstatus))
    mpd_qdivint(texp, texp, addr(two), addr(maxctx), addr(workstatus))
    if mpd_isnan(tbase) or mpd_isnan(texp):
      mpd_seterror(result, workstatus and MPD_Errors, status)
      return
  mpd_set_sign(result, resultsign)

##
##  The power function for integer exponents. Relative error _before_ the
##  final rounding to prec:
##    abs(result - base**exp) < 0.1 * 10**-prec * abs(base**exp)
##

proc _mpd_qpow_int*(result: ptr mpd_t; base: ptr mpd_t; exp: ptr mpd_t;
                   resultsign: uint8_t; ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var workctx: mpd_context_t
  MPD_NEW_STATIC(tbase, 0, 0, 0, 0)
  MPD_NEW_STATIC(texp, 0, 0, 0, 0)
  var n: mpd_uint_t
  mpd_workcontext(addr(workctx), ctx)
  inc(workctx.prec, (exp.digits + exp.exp + 2))
  workctx.round = MPD_ROUND_HALF_EVEN
  workctx.clamp = 0
  if mpd_isnegative(exp):
    var workstatus: uint32_t = 0
    inc(workctx.prec, 1)
    mpd_qdiv(addr(tbase), addr(one), base, addr(workctx), addr(workstatus))
    status[] = status[] or workstatus
    if workstatus and MPD_Errors:
      mpd_setspecial(result, MPD_POS, MPD_NAN)
      break finish
  else:
    if not mpd_qcopy(addr(tbase), base, status):
      mpd_setspecial(result, MPD_POS, MPD_NAN)
      break finish
  n = mpd_qabs_uint(exp, addr(workctx.status))
  if workctx.status and MPD_Invalid_operation:
    if not mpd_qcopy(addr(texp), exp, status):
      mpd_setspecial(result, MPD_POS, MPD_NAN)
      ##  GCOV_UNLIKELY
      break finish
      ##  GCOV_UNLIKELY
    _mpd_qpow_mpd(result, addr(tbase), addr(texp), resultsign, addr(workctx), status)
  else:
    _mpd_qpow_uint(result, addr(tbase), n, resultsign, addr(workctx), status)
  if mpd_isinfinite(result):
    ##  for ROUND_DOWN, ROUND_FLOOR, etc.
    _settriple(result, resultsign, 1, MPD_EXP_INF)
  mpd_del(addr(tbase))
  mpd_del(addr(texp))
  mpd_qfinalize(result, ctx, status)

##
##  If the exponent is infinite and base equals one, the result is one
##  with a coefficient of length prec. Otherwise, result is undefined.
##  Return the value of the comparison against one.
##

proc _qcheck_pow_one_inf*(result: ptr mpd_t; base: ptr mpd_t; resultsign: uint8_t;
                         ctx: ptr mpd_context_t; status: ptr uint32_t): cint =
  var shift: mpd_ssize_t
  var cmp: cint
  if (cmp = _mpd_cmp(base, addr(one))) == 0:
    shift = ctx.prec - 1
    mpd_qshiftl(result, addr(one), shift, status)
    result.exp = -shift
    mpd_set_flags(result, resultsign)
    status[] = status[] or (MPD_Inexact or MPD_Rounded)
  return cmp

##
##  If abs(base) equals one, calculate the correct power of one result.
##  Otherwise, result is undefined. Return the value of the comparison
##  against 1.
##
##  This is an internal function that does not check for specials.
##

proc _qcheck_pow_one*(result: ptr mpd_t; base: ptr mpd_t; exp: ptr mpd_t;
                     resultsign: uint8_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t): cint =
  var workstatus: uint32_t = 0
  var shift: mpd_ssize_t
  var cmp: cint
  if (cmp = _mpd_cmp_abs(base, addr(one))) == 0:
    if _mpd_isint(exp):
      if mpd_isnegative(exp):
        _settriple(result, resultsign, 1, 0)
        return 0
      mpd_qmul_ssize(result, exp, -base.exp, ctx, addr(workstatus))
      if workstatus and MPD_Errors:
        status[] = status[] or (workstatus and MPD_Errors)
        return 0
      shift = mpd_qget_ssize(result, addr(workstatus))
      ##  shift is MPD_SSIZE_MAX if result is too large
      if shift > ctx.prec - 1:
        shift = ctx.prec - 1
        status[] = status[] or MPD_Rounded
    elif mpd_ispositive(base):
      shift = ctx.prec - 1
      status[] = status[] or (MPD_Inexact or MPD_Rounded)
    else:
      return -2
      ##  GCOV_NOT_REACHED
    if not mpd_qshiftl(result, addr(one), shift, status):
      return 0
    result.exp = -shift
    mpd_set_flags(result, resultsign)
  return cmp

##
##  Detect certain over/underflow of x**y.
##  ACL2 proof: pow-bounds.lisp.
##
##    Symbols:
##
##      e: EXP_INF or EXP_CLAMP
##      x: base
##      y: exponent
##
##      omega(e) = log10(abs(e))
##      zeta(x)  = log10(abs(log10(x)))
##      theta(y) = log10(abs(y))
##
##    Upper and lower bounds:
##
##      ub_omega(e) = ceil(log10(abs(e)))
##      lb_theta(y) = floor(log10(abs(y)))
##
##                   | floor(log10(floor(abs(log10(x))))) if x < 1/10 or x >= 10
##      lb_zeta(x) = | floor(log10(abs(x-1)/10)) if 1/10 <= x < 1
##                   | floor(log10(abs((x-1)/100))) if 1 < x < 10
##
##    ub_omega(e) and lb_theta(y) are obviously upper and lower bounds
##    for omega(e) and theta(y).
##
##    lb_zeta is a lower bound for zeta(x):
##
##      x < 1/10 or x >= 10:
##
##        abs(log10(x)) >= 1, so the outer log10 is well defined. Since log10
##        is strictly increasing, the end result is a lower bound.
##
##      1/10 <= x < 1:
##
##        We use: log10(x) <= (x-1)/log(10)
##                abs(log10(x)) >= abs(x-1)/log(10)
##                abs(log10(x)) >= abs(x-1)/10
##
##      1 < x < 10:
##
##        We use: (x-1)/(x*log(10)) < log10(x)
##                abs((x-1)/100) < abs(log10(x))
##
##        XXX: abs((x-1)/10) would work, need ACL2 proof.
##
##
##    Let (0 < x < 1 and y < 0) or (x > 1 and y > 0).                  (H1)
##    Let ub_omega(exp_inf) < lb_zeta(x) + lb_theta(y)                 (H2)
##
##    Then:
##        log10(abs(exp_inf)) < log10(abs(log10(x))) + log10(abs(y)).   (1)
##                    exp_inf < log10(x) * y                            (2)
##                10**exp_inf < x**y                                    (3)
##
##    Let (0 < x < 1 and y > 0) or (x > 1 and y < 0).                  (H3)
##    Let ub_omega(exp_clamp) < lb_zeta(x) + lb_theta(y)               (H4)
##
##    Then:
##      log10(abs(exp_clamp)) < log10(abs(log10(x))) + log10(abs(y)).   (4)
##               log10(x) * y < exp_clamp                               (5)
##                       x**y < 10**exp_clamp                           (6)
##
##

proc _lower_bound_zeta*(x: ptr mpd_t; status: ptr uint32_t): mpd_ssize_t =
  var maxctx: mpd_context_t
  MPD_NEW_STATIC(scratch, 0, 0, 0, 0)
  var
    t: mpd_ssize_t
    u: mpd_ssize_t
  t = mpd_adjexp(x)
  if t > 0:
    ##  x >= 10 -> floor(log10(floor(abs(log10(x)))))
    return mpd_exp_digits(t) - 1
  elif t < -1:
    ##  x < 1/10 -> floor(log10(floor(abs(log10(x)))))
    return mpd_exp_digits(t + 1) - 1
  else:
    mpd_maxcontext(addr(maxctx))
    mpd_qsub(addr(scratch), x, addr(one), addr(maxctx), status)
    if mpd_isspecial(addr(scratch)):
      mpd_del(addr(scratch))
      return MPD_SSIZE_MAX
    u = mpd_adjexp(addr(scratch))
    mpd_del(addr(scratch))
    ##  t == -1, 1/10 <= x < 1 -> floor(log10(abs(x-1)/10))
    ##  t == 0,  1 < x < 10    -> floor(log10(abs(x-1)/100))
    return if (t == 0): u - 2 else: u - 1

##
##  Detect cases of certain overflow/underflow in the power function.
##  Assumptions: x != 1, y != 0. The proof above is for positive x.
##  If x is negative and y is an odd integer, x**y == -(abs(x)**y),
##  so the analysis does not change.
##

proc _qcheck_pow_bounds*(result: ptr mpd_t; x: ptr mpd_t; y: ptr mpd_t;
                        resultsign: uint8_t; ctx: ptr mpd_context_t;
                        status: ptr uint32_t): cint =
  MPD_NEW_SHARED(abs_x, x)
  var
    ub_omega: mpd_ssize_t
    lb_zeta: mpd_ssize_t
    lb_theta: mpd_ssize_t
  var sign: uint8_t
  mpd_set_positive(addr(abs_x))
  lb_theta = mpd_adjexp(y)
  lb_zeta = _lower_bound_zeta(addr(abs_x), status)
  if lb_zeta == MPD_SSIZE_MAX:
    mpd_seterror(result, MPD_Malloc_error, status)
    return 1
  sign = (mpd_adjexp(addr(abs_x)) < 0) xor mpd_sign(y)
  if sign == 0:
    ##  (0 < |x| < 1 and y < 0) or (|x| > 1 and y > 0)
    ub_omega = mpd_exp_digits(ctx.emax)
    if ub_omega < lb_zeta + lb_theta:
      _settriple(result, resultsign, 1, MPD_EXP_INF)
      mpd_qfinalize(result, ctx, status)
      return 1
  else:
    ##  (0 < |x| < 1 and y > 0) or (|x| > 1 and y < 0).
    ub_omega = mpd_exp_digits(mpd_etiny(ctx))
    if ub_omega < lb_zeta + lb_theta:
      _settriple(result, resultsign, 1, mpd_etiny(ctx) - 1)
      mpd_qfinalize(result, ctx, status)
      return 1
  return 0

##
##  TODO: Implement algorithm for computing exact powers from decimal.py.
##  In order to prevent infinite loops, this has to be called before
##  using Ziv's strategy for correct rounding.
##
##
## static int
## _mpd_qpow_exact(mpd_t *result, const mpd_t *base, const mpd_t *exp,
##                 const mpd_context_t *ctx, uint32_t *status)
## {
##     return 0;
## }
##
##
##  The power function for real exponents.
##    Relative error: abs(result - e**y) < e**y * 1/5 * 10**(-prec - 1)
##

proc _mpd_qpow_real*(result: ptr mpd_t; base: ptr mpd_t; exp: ptr mpd_t;
                    ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var workctx: mpd_context_t
  MPD_NEW_STATIC(texp, 0, 0, 0, 0)
  if not mpd_qcopy(addr(texp), exp, status):
    mpd_seterror(result, MPD_Malloc_error, status)
    return
  mpd_maxcontext(addr(workctx))
  workctx.prec = if (base.digits > ctx.prec): base.digits else: ctx.prec
  inc(workctx.prec, (4 + MPD_EXPDIGITS))
  workctx.round = MPD_ROUND_HALF_EVEN
  workctx.allcr = ctx.allcr
  ##
  ##  extra := MPD_EXPDIGITS = MPD_EXP_MAX_T
  ##  wp := prec + 4 + extra
  ##  abs(err) < 5 * 10**-wp
  ##  y := log(base) * exp
  ##  Calculate:
  ##    1)   e**(y * (1 + err)**2) * (1 + err)
  ##       = e**y * e**(y * (2*err + err**2)) * (1 + err)
  ##         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ##  Relative error of the underlined term:
  ##    2) abs(e**(y * (2*err + err**2)) - 1)
  ##  Case abs(y) >= 10**extra:
  ##    3) adjexp(y)+1 > log10(abs(y)) >= extra
  ##    This triggers the Overflow/Underflow shortcut in _mpd_qexp(),
  ##    so no further analysis is necessary.
  ##  Case abs(y) < 10**extra:
  ##    4) abs(y * (2*err + err**2)) < 1/5 * 10**(-prec - 2)
  ##    Use (see _mpd_qexp):
  ##      5) abs(x) <= 9/10 * 10**-p ==> abs(e**x - 1) < 10**-p
  ##    With 2), 4) and 5):
  ##      6) abs(e**(y * (2*err + err**2)) - 1) < 10**(-prec - 2)
  ##    The complete relative error of 1) is:
  ##      7) abs(result - e**y) < e**y * 1/5 * 10**(-prec - 1)
  ##
  mpd_qln(result, base, addr(workctx), addr(workctx.status))
  mpd_qmul(result, result, addr(texp), addr(workctx), addr(workctx.status))
  mpd_qexp(result, result, addr(workctx), status)
  mpd_del(addr(texp))
  status[] = status[] or (workctx.status and MPD_Errors)
  status[] = status[] or (MPD_Inexact or MPD_Rounded)

##  The power function: base**exp

proc mpd_qpow*(result: ptr mpd_t; base: ptr mpd_t; exp: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  var resultsign: uint8_t = 0
  var intexp: cint = 0
  var cmp: cint
  if mpd_isspecial(base) or mpd_isspecial(exp):
    if mpd_qcheck_nans(result, base, exp, ctx, status):
      return
  if mpd_isinteger(exp):
    intexp = 1
    resultsign = mpd_isnegative(base) and mpd_isodd(exp)
  if mpd_iszero(base):
    if mpd_iszero(exp):
      mpd_seterror(result, MPD_Invalid_operation, status)
    elif mpd_isnegative(exp):
      mpd_setspecial(result, resultsign, MPD_INF)
    else:
      _settriple(result, resultsign, 0, 0)
    return
  if mpd_isnegative(base):
    if not intexp or mpd_isinfinite(exp):
      mpd_seterror(result, MPD_Invalid_operation, status)
      return
  if mpd_isinfinite(exp):
    ##  power of one
    cmp = _qcheck_pow_one_inf(result, base, resultsign, ctx, status)
    if cmp == 0:
      return
    else:
      cmp = cmp * mpd_arith_sign(exp)
      if cmp < 0:
        _settriple(result, resultsign, 0, 0)
      else:
        mpd_setspecial(result, resultsign, MPD_INF)
    return
  if mpd_isinfinite(base):
    if mpd_iszero(exp):
      _settriple(result, resultsign, 1, 0)
    elif mpd_isnegative(exp):
      _settriple(result, resultsign, 0, 0)
    else:
      mpd_setspecial(result, resultsign, MPD_INF)
    return
  if mpd_iszero(exp):
    _settriple(result, resultsign, 1, 0)
    return
  if _qcheck_pow_one(result, base, exp, resultsign, ctx, status) == 0:
    return
  if _qcheck_pow_bounds(result, base, exp, resultsign, ctx, status):
    return
  if intexp:
    _mpd_qpow_int(result, base, exp, resultsign, ctx, status)
  else:
    _mpd_qpow_real(result, base, exp, ctx, status)
    if not mpd_isspecial(result) and _mpd_cmp(result, addr(one)) == 0:
      var shift: mpd_ssize_t = ctx.prec - 1
      mpd_qshiftl(result, addr(one), shift, status)
      result.exp = -shift
    if mpd_isinfinite(result):
      ##  for ROUND_DOWN, ROUND_FLOOR, etc.
      _settriple(result, MPD_POS, 1, MPD_EXP_INF)
    mpd_qfinalize(result, ctx, status)

##
##  Internal function: Integer powmod with mpd_uint_t exponent, base is modified!
##  Function can fail with MPD_Malloc_error.
##

proc _mpd_qpowmod_uint*(result: ptr mpd_t; base: ptr mpd_t; exp: mpd_uint_t;
                       `mod`: ptr mpd_t; status: ptr uint32_t) {.inline.} =
  var maxcontext: mpd_context_t
  mpd_maxcontext(addr(maxcontext))
  ##  resize to smaller cannot fail
  mpd_qcopy(result, addr(one), status)
  while exp > 0:
    if exp and 1:
      _mpd_qmul_exact(result, result, base, addr(maxcontext), status)
      mpd_qrem(result, result, `mod`, addr(maxcontext), status)
    _mpd_qmul_exact(base, base, base, addr(maxcontext), status)
    mpd_qrem(base, base, `mod`, addr(maxcontext), status)
    exp = exp shr 1

##  The powmod function: (base**exp) % mod

proc mpd_qpowmod*(result: ptr mpd_t; base: ptr mpd_t; exp: ptr mpd_t; `mod`: ptr mpd_t;
                 ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(tbase, 0, 0, 0, 0)
  MPD_NEW_STATIC(texp, 0, 0, 0, 0)
  MPD_NEW_STATIC(tmod, 0, 0, 0, 0)
  MPD_NEW_STATIC(tmp, 0, 0, 0, 0)
  MPD_NEW_CONST(two, 0, 0, 1, 1, 1, 2)
  var
    tbase_exp: mpd_ssize_t
    texp_exp: mpd_ssize_t
  var i: mpd_ssize_t
  var t: mpd_t
  var r: mpd_uint_t
  var sign: uint8_t
  if mpd_isspecial(base) or mpd_isspecial(exp) or mpd_isspecial(`mod`):
    if mpd_qcheck_3nans(result, base, exp, `mod`, ctx, status):
      return
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if not _mpd_isint(base) or not _mpd_isint(exp) or not _mpd_isint(`mod`):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if mpd_iszerocoeff(`mod`):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if `mod`.digits + `mod`.exp > ctx.prec:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  sign = (mpd_isnegative(base)) and (mpd_isodd(exp))
  if mpd_iszerocoeff(exp):
    if mpd_iszerocoeff(base):
      mpd_seterror(result, MPD_Invalid_operation, status)
      return
    r = if (_mpd_cmp_abs(`mod`, addr(one)) == 0): 0 else: 1
    _settriple(result, sign, r, 0)
    return
  if mpd_isnegative(exp):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if mpd_iszerocoeff(base):
    _settriple(result, sign, 0, 0)
    return
  mpd_maxcontext(addr(maxcontext))
  mpd_qrescale(addr(tmod), `mod`, 0, addr(maxcontext), addr(maxcontext.status))
  if maxcontext.status and MPD_Errors:
    mpd_seterror(result, maxcontext.status and MPD_Errors, status)
    break `out`
  maxcontext.status = 0
  mpd_set_positive(addr(tmod))
  mpd_qround_to_int(addr(tbase), base, addr(maxcontext), status)
  mpd_set_positive(addr(tbase))
  tbase_exp = tbase.exp
  tbase.exp = 0
  mpd_qround_to_int(addr(texp), exp, addr(maxcontext), status)
  texp_exp = texp.exp
  texp.exp = 0
  ##  base = (base.int % modulo * pow(10, base.exp, modulo)) % modulo
  mpd_qrem(addr(tbase), addr(tbase), addr(tmod), addr(maxcontext), status)
  mpd_qshiftl(result, addr(one), tbase_exp, status)
  mpd_qrem(result, result, addr(tmod), addr(maxcontext), status)
  _mpd_qmul_exact(addr(tbase), addr(tbase), result, addr(maxcontext), status)
  mpd_qrem(addr(tbase), addr(tbase), addr(tmod), addr(maxcontext), status)
  if mpd_isspecial(addr(tbase)) or mpd_isspecial(addr(texp)) or
      mpd_isspecial(addr(tmod)):
    break mpd_errors
  i = 0
  while i < texp_exp:
    _mpd_qpowmod_uint(addr(tmp), addr(tbase), 10, addr(tmod), status)
    t = tmp
    tmp = tbase
    tbase = t
    inc(i)
  if mpd_isspecial(addr(tbase)):
    break mpd_errors
    ##  GCOV_UNLIKELY
  mpd_qcopy(result, addr(one), status)
  while mpd_isfinite(addr(texp)) and not mpd_iszero(addr(texp)):
    if mpd_isodd(addr(texp)):
      _mpd_qmul_exact(result, result, addr(tbase), addr(maxcontext), status)
      mpd_qrem(result, result, addr(tmod), addr(maxcontext), status)
    _mpd_qmul_exact(addr(tbase), addr(tbase), addr(tbase), addr(maxcontext), status)
    mpd_qrem(addr(tbase), addr(tbase), addr(tmod), addr(maxcontext), status)
    mpd_qdivint(addr(texp), addr(texp), addr(two), addr(maxcontext), status)
  if mpd_isspecial(addr(texp)) or mpd_isspecial(addr(tbase)) or
      mpd_isspecial(addr(tmod)) or mpd_isspecial(result):
    ##  MPD_Malloc_error
    break mpd_errors
  else:
    mpd_set_sign(result, sign)
  mpd_del(addr(tbase))
  mpd_del(addr(texp))
  mpd_del(addr(tmod))
  mpd_del(addr(tmp))
  return
  mpd_setspecial(result, MPD_POS, MPD_NAN)
  break `out`

proc mpd_qquantize*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                   status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  var b_exp: mpd_ssize_t = b.exp
  var
    expdiff: mpd_ssize_t
    shift: mpd_ssize_t
  var rnd: mpd_uint_t
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(result, a, b, ctx, status):
      return
    if mpd_isinfinite(a) and mpd_isinfinite(b):
      mpd_qcopy(result, a, status)
      return
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if b.exp > ctx.emax or b.exp < mpd_etiny(ctx):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if mpd_iszero(a):
    _settriple(result, mpd_sign(a), 0, b.exp)
    mpd_qfinalize(result, ctx, status)
    return
  expdiff = a.exp - b.exp
  if a.digits + expdiff > ctx.prec:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if expdiff >= 0:
    shift = expdiff
    if not mpd_qshiftl(result, a, shift, status):
      return
    result.exp = b_exp
  else:
    ##  At this point expdiff < 0 and a->digits+expdiff <= prec,
    ##  so the shift before an increment will fit in prec.
    shift = -expdiff
    rnd = mpd_qshiftr(result, a, shift, status)
    if rnd == MPD_UINT_MAX:
      return
    result.exp = b_exp
    if not _mpd_apply_round_fit(result, rnd, ctx, status):
      return
    workstatus = workstatus or MPD_Rounded
    if rnd:
      workstatus = workstatus or MPD_Inexact
  if mpd_adjexp(result) > ctx.emax or mpd_adjexp(result) < mpd_etiny(ctx):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  status[] = status[] or workstatus
  mpd_qfinalize(result, ctx, status)

proc mpd_qreduce*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                 status: ptr uint32_t) =
  var
    shift: mpd_ssize_t
    maxexp: mpd_ssize_t
    maxshift: mpd_ssize_t
  var sign_a: uint8_t = mpd_sign(a)
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    mpd_qcopy(result, a, status)
    return
  if not mpd_qcopy(result, a, status):
    return
  mpd_qfinalize(result, ctx, status)
  if mpd_isspecial(result):
    return
  if mpd_iszero(result):
    _settriple(result, sign_a, 0, 0)
    return
  shift = mpd_trail_zeros(result)
  maxexp = if (ctx.clamp): mpd_etop(ctx) else: ctx.emax
  ##  After the finalizing above result->exp <= maxexp.
  maxshift = maxexp - result.exp
  shift = if (shift > maxshift): maxshift else: shift
  mpd_qshiftr_inplace(result, shift)
  inc(result.exp, shift)

proc mpd_qrem*(r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
              status: ptr uint32_t) =
  MPD_NEW_STATIC(q, 0, 0, 0, 0)
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(r, a, b, ctx, status):
      return
    if mpd_isinfinite(a):
      mpd_seterror(r, MPD_Invalid_operation, status)
      return
    if mpd_isinfinite(b):
      mpd_qcopy(r, a, status)
      mpd_qfinalize(r, ctx, status)
      return
    abort()
    ##  GCOV_NOT_REACHED
  if mpd_iszerocoeff(b):
    if mpd_iszerocoeff(a):
      mpd_seterror(r, MPD_Division_undefined, status)
    else:
      mpd_seterror(r, MPD_Invalid_operation, status)
    return
  _mpd_qdivmod(addr(q), r, a, b, ctx, status)
  mpd_del(addr(q))
  mpd_qfinalize(r, ctx, status)

proc mpd_qrem_near*(r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t;
                   status: ptr uint32_t) =
  var workctx: mpd_context_t
  MPD_NEW_STATIC(btmp, 0, 0, 0, 0)
  MPD_NEW_STATIC(q, 0, 0, 0, 0)
  var
    expdiff: mpd_ssize_t
    qdigits: mpd_ssize_t
  var
    cmp: cint
    isodd: cint
    allnine: cint
  assert(r != nil)
  ##  annotation for scan-build
  if mpd_isspecial(a) or mpd_isspecial(b):
    if mpd_qcheck_nans(r, a, b, ctx, status):
      return
    if mpd_isinfinite(a):
      mpd_seterror(r, MPD_Invalid_operation, status)
      return
    if mpd_isinfinite(b):
      mpd_qcopy(r, a, status)
      mpd_qfinalize(r, ctx, status)
      return
    abort()
    ##  GCOV_NOT_REACHED
  if mpd_iszerocoeff(b):
    if mpd_iszerocoeff(a):
      mpd_seterror(r, MPD_Division_undefined, status)
    else:
      mpd_seterror(r, MPD_Invalid_operation, status)
    return
  if r == b:
    if not mpd_qcopy(addr(btmp), b, status):
      mpd_seterror(r, MPD_Malloc_error, status)
      return
    b = addr(btmp)
  _mpd_qdivmod(addr(q), r, a, b, ctx, status)
  if mpd_isnan(addr(q)) or mpd_isnan(r):
    break finish
  if mpd_iszerocoeff(r):
    break finish
  expdiff = mpd_adjexp(b) - mpd_adjexp(r)
  if -1 <= expdiff and expdiff <= 1:
    allnine = mpd_coeff_isallnine(addr(q))
    qdigits = q.digits
    isodd = mpd_isodd(addr(q))
    mpd_maxcontext(addr(workctx))
    if mpd_sign(a) == mpd_sign(b):
      ##  sign(r) == sign(b)
      _mpd_qsub(addr(q), r, b, addr(workctx), addr(workctx.status))
    else:
      ##  sign(r) != sign(b)
      _mpd_qadd(addr(q), r, b, addr(workctx), addr(workctx.status))
    if workctx.status and MPD_Errors:
      mpd_seterror(r, workctx.status and MPD_Errors, status)
      break finish
    cmp = _mpd_cmp_abs(addr(q), r)
    if cmp < 0 or (cmp == 0 and isodd):
      ##  abs(r) > abs(b)/2 or abs(r) == abs(b)/2 and isodd(quotient)
      if allnine and qdigits == ctx.prec:
        ##  abs(quotient) + 1 == 10**prec
        mpd_seterror(r, MPD_Division_impossible, status)
        break finish
      mpd_qcopy(r, addr(q), status)
  mpd_del(addr(btmp))
  mpd_del(addr(q))
  mpd_qfinalize(r, ctx, status)

proc _mpd_qrescale*(result: ptr mpd_t; a: ptr mpd_t; exp: mpd_ssize_t;
                   ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var
    expdiff: mpd_ssize_t
    shift: mpd_ssize_t
  var rnd: mpd_uint_t
  if mpd_isspecial(a):
    mpd_qcopy(result, a, status)
    return
  if mpd_iszero(a):
    _settriple(result, mpd_sign(a), 0, exp)
    return
  expdiff = a.exp - exp
  if expdiff >= 0:
    shift = expdiff
    if a.digits + shift > MPD_MAX_PREC + 1:
      mpd_seterror(result, MPD_Invalid_operation, status)
      return
    if not mpd_qshiftl(result, a, shift, status):
      return
    result.exp = exp
  else:
    shift = -expdiff
    rnd = mpd_qshiftr(result, a, shift, status)
    if rnd == MPD_UINT_MAX:
      return
    result.exp = exp
    _mpd_apply_round_excess(result, rnd, ctx, status)
    status[] = status[] or MPD_Rounded
    if rnd:
      status[] = status[] or MPD_Inexact
  if mpd_issubnormal(result, ctx):
    status[] = status[] or MPD_Subnormal

##
##  Rescale a number so that it has exponent 'exp'. Does not regard context
##  precision, emax, emin, but uses the rounding mode. Special numbers are
##  quietly copied. Restrictions:
##
##      MPD_MIN_ETINY <= exp <= MPD_MAX_EMAX+1
##      result->digits <= MPD_MAX_PREC+1
##

proc mpd_qrescale*(result: ptr mpd_t; a: ptr mpd_t; exp: mpd_ssize_t;
                  ctx: ptr mpd_context_t; status: ptr uint32_t) =
  if exp > MPD_MAX_EMAX + 1 or exp < MPD_MIN_ETINY:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  _mpd_qrescale(result, a, exp, ctx, status)

##
##  Same as mpd_qrescale, but with relaxed restrictions. The result of this
##  function should only be used for formatting a number and never as input
##  for other operations.
##
##      MPD_MIN_ETINY-MPD_MAX_PREC <= exp <= MPD_MAX_EMAX+1
##      result->digits <= MPD_MAX_PREC+1
##

proc mpd_qrescale_fmt*(result: ptr mpd_t; a: ptr mpd_t; exp: mpd_ssize_t;
                      ctx: ptr mpd_context_t; status: ptr uint32_t) =
  if exp > MPD_MAX_EMAX + 1 or exp < MPD_MIN_ETINY - MPD_MAX_PREC:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  _mpd_qrescale(result, a, exp, ctx, status)

##  Round to an integer according to 'action' and ctx->round.

const
  TO_INT_EXACT* = 0
  TO_INT_SILENT* = 1
  TO_INT_TRUNC* = 2

proc _mpd_qround_to_integral*(action: cint; result: ptr mpd_t; a: ptr mpd_t;
                             ctx: ptr mpd_context_t; status: ptr uint32_t) =
  var rnd: mpd_uint_t
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    mpd_qcopy(result, a, status)
    return
  if a.exp >= 0:
    mpd_qcopy(result, a, status)
    return
  if mpd_iszerocoeff(a):
    _settriple(result, mpd_sign(a), 0, 0)
    return
  rnd = mpd_qshiftr(result, a, -a.exp, status)
  if rnd == MPD_UINT_MAX:
    return
  result.exp = 0
  if action == TO_INT_EXACT or action == TO_INT_SILENT:
    _mpd_apply_round_excess(result, rnd, ctx, status)
    if action == TO_INT_EXACT:
      status[] = status[] or MPD_Rounded
      if rnd:
        status[] = status[] or MPD_Inexact

proc mpd_qround_to_intx*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                        status: ptr uint32_t) =
  cast[nil](_mpd_qround_to_integral(TO_INT_EXACT, result, a, ctx, status))

proc mpd_qround_to_int*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                       status: ptr uint32_t) =
  cast[nil](_mpd_qround_to_integral(TO_INT_SILENT, result, a, ctx, status))

proc mpd_qtrunc*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                status: ptr uint32_t) =
  if mpd_isspecial(a):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  cast[nil](_mpd_qround_to_integral(TO_INT_TRUNC, result, a, ctx, status))

proc mpd_qfloor*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                status: ptr uint32_t) =
  var workctx: mpd_context_t = ctx[]
  if mpd_isspecial(a):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  workctx.round = MPD_ROUND_FLOOR
  cast[nil](_mpd_qround_to_integral(TO_INT_SILENT, result, a, addr(workctx), status))

proc mpd_qceil*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) =
  var workctx: mpd_context_t = ctx[]
  if mpd_isspecial(a):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  workctx.round = MPD_ROUND_CEILING
  cast[nil](_mpd_qround_to_integral(TO_INT_SILENT, result, a, addr(workctx), status))

proc mpd_same_quantum*(a: ptr mpd_t; b: ptr mpd_t): cint =
  if mpd_isspecial(a) or mpd_isspecial(b):
    return (mpd_isnan(a) and mpd_isnan(b)) or
        (mpd_isinfinite(a) and mpd_isinfinite(b))
  return a.exp == b.exp

##  Schedule the increase in precision for the Newton iteration.

proc recpr_schedule_prec*(klist: array[MPD_MAX_PREC_LOG2, mpd_ssize_t];
                         maxprec: mpd_ssize_t; initprec: mpd_ssize_t): cint {.inline.} =
  var k: mpd_ssize_t
  var i: cint
  assert(maxprec > 0 and initprec > 0)
  if maxprec <= initprec:
    return -1
  i = 0
  k = maxprec
  while true:
    k = (k + 1) div 2
    klist[inc(i)] = k
    if not (k > initprec):
      break
  return i - 1

##
##  Initial approximation for the reciprocal:
##     k_0 := MPD_RDIGITS-2
##     z_0 := 10**(-k_0) * floor(10**(2*k_0 + 2) / floor(v * 10**(k_0 + 2)))
##  Absolute error:
##     |1/v - z_0| < 10**(-k_0)
##  ACL2 proof: maxerror-inverse-approx
##

proc _mpd_qreciprocal_approx*(z: ptr mpd_t; v: ptr mpd_t; status: ptr uint32_t) =
  var p10data: array[2, mpd_uint_t] = [0, mpd_pow10[MPD_RDIGITS - 2]]
  var
    dummy: mpd_uint_t
    word: mpd_uint_t
  var n: cint
  assert(v.exp == -v.digits)
  _mpd_get_msdigits(addr(dummy), addr(word), v, MPD_RDIGITS)
  n = mpd_word_digits(word)
  word = word * mpd_pow10[MPD_RDIGITS - n]
  mpd_qresize(z, 2, status)
  cast[nil](_mpd_shortdiv(z.data, p10data, 2, word))
  mpd_clear_flags(z)
  z.exp = -(MPD_RDIGITS - 2)
  z.len = if (z.data[1] == 0): 1 else: 2
  mpd_setdigits(z)

##
##  Reciprocal, calculated with Newton's Method. Assumption: result != a.
##  NOTE: The comments in the function show that certain operations are
##  exact. The proof for the maximum error is too long to fit in here.
##  ACL2 proof: maxerror-inverse-complete
##

proc _mpd_qreciprocal*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                      status: ptr uint32_t) =
  var
    varcontext: mpd_context_t
    maxcontext: mpd_context_t
  var z: ptr mpd_t = result
  ##  current approximation
  var v: ptr mpd_t
  ##  a, normalized to a number between 0.1 and 1
  MPD_NEW_SHARED(vtmp, a)
  ##  v shares data with a
  MPD_NEW_STATIC(s, 0, 0, 0, 0)
  ##  temporary variable
  MPD_NEW_STATIC(t, 0, 0, 0, 0)
  ##  temporary variable
  MPD_NEW_CONST(two, 0, 0, 1, 1, 1, 2)
  ##  const 2
  var klist: array[MPD_MAX_PREC_LOG2, mpd_ssize_t]
  var
    adj: mpd_ssize_t
    maxprec: mpd_ssize_t
    initprec: mpd_ssize_t
  var sign: uint8_t = mpd_sign(a)
  var i: cint
  assert(result != a)
  v = addr(vtmp)
  mpd_clear_flags(v)
  adj = v.digits + v.exp
  v.exp = -v.digits
  ##  Initial approximation
  _mpd_qreciprocal_approx(z, v, status)
  mpd_maxcontext(addr(varcontext))
  mpd_maxcontext(addr(maxcontext))
  varcontext.round = maxcontext.round = MPD_ROUND_TRUNC
  varcontext.emax = maxcontext.emax = MPD_MAX_EMAX + 100
  varcontext.emin = maxcontext.emin = MPD_MIN_EMIN - 100
  maxcontext.prec = MPD_MAX_PREC + 100
  maxprec = ctx.prec
  inc(maxprec, 2)
  initprec = MPD_RDIGITS - 3
  i = recpr_schedule_prec(klist, maxprec, initprec)
  while i >= 0:
    ##  Loop invariant: z->digits <= klist[i]+7
    ##  Let s := z**2, exact result
    _mpd_qmul_exact(addr(s), z, z, addr(maxcontext), status)
    varcontext.prec = 2 * klist[i] + 5
    if v.digits > varcontext.prec:
      ##  Let t := v, truncated to n >= 2*k+5 fraction digits
      mpd_qshiftr(addr(t), v, v.digits - varcontext.prec, status)
      t.exp = -varcontext.prec
      ##  Let t := trunc(v)*s, truncated to n >= 2*k+1 fraction digits
      mpd_qmul(addr(t), addr(t), addr(s), addr(varcontext), status)
    else:
      ##  v->digits <= 2*k+5
      ##  Let t := v*s, truncated to n >= 2*k+1 fraction digits
      mpd_qmul(addr(t), v, addr(s), addr(varcontext), status)
    ##  Let s := 2*z, exact result
    _mpd_qmul_exact(addr(s), z, addr(two), addr(maxcontext), status)
    ##  s.digits < t.digits <= 2*k+5, |adjexp(s)-adjexp(t)| <= 1,
    ##  so the subtraction generates at most 2*k+6 <= klist[i+1]+7
    ##  digits. The loop invariant is preserved.
    _mpd_qsub_exact(z, addr(s), addr(t), addr(maxcontext), status)
    dec(i)
  if not mpd_isspecial(z):
    dec(z.exp, adj)
    mpd_set_flags(z, sign)
  mpd_del(addr(s))
  mpd_del(addr(t))
  mpd_qfinalize(z, ctx, status)

##
##  Internal function for large numbers:
##
##      q, r = divmod(coeff(a), coeff(b))
##
##  Strategy: Multiply the dividend by the reciprocal of the divisor. The
##  inexact result is fixed by a small loop, using at most one iteration.
##
##  ACL2 proofs:
##  ------------
##     1) q is a natural number.  (ndivmod-quotient-natp)
##     2) r is a natural number.  (ndivmod-remainder-natp)
##     3) a = q * b + r           (ndivmod-q*b+r==a)
##     4) r < b                   (ndivmod-remainder-<-b)
##

proc _mpd_base_ndivmod*(q: ptr mpd_t; r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                       status: ptr uint32_t) =
  var workctx: mpd_context_t
  var
    qq: ptr mpd_t = q
    rr: ptr mpd_t = r
  var
    aa: mpd_t
    bb: mpd_t
  var k: cint
  _mpd_copy_shared(addr(aa), a)
  _mpd_copy_shared(addr(bb), b)
  mpd_set_positive(addr(aa))
  mpd_set_positive(addr(bb))
  aa.exp = 0
  bb.exp = 0
  if q == a or q == b:
    if (qq = mpd_qnew()) == nil:
      status[] = status[] or MPD_Malloc_error
      break nanresult
  if r == a or r == b:
    if (rr = mpd_qnew()) == nil:
      status[] = status[] or MPD_Malloc_error
      break nanresult
  mpd_maxcontext(addr(workctx))
  ##  Let prec := adigits - bdigits + 4
  workctx.prec = a.digits - b.digits + 1 + 3
  if a.digits > MPD_MAX_PREC or workctx.prec > MPD_MAX_PREC:
    status[] = status[] or MPD_Division_impossible
    break nanresult
  _mpd_qreciprocal(rr, addr(bb), addr(workctx), addr(workctx.status))
  ##  Get an estimate for the quotient. Let q := a * x
  ##  Then q is bounded by:
  ##     3) a/b - 10**-4 < q < a/b + 10**-4
  ##
  _mpd_qmul(qq, addr(aa), rr, addr(workctx), addr(workctx.status))
  ##  Truncate q to an integer:
  ##     4) a/b - 2 < trunc(q) < a/b + 1
  ##
  mpd_qtrunc(qq, qq, addr(workctx), addr(workctx.status))
  workctx.prec = aa.digits + 3
  workctx.emax = MPD_MAX_EMAX + 3
  workctx.emin = MPD_MIN_EMIN - 3
  ##  Multiply the estimate for q by b:
  ##     5) a - 2 * b < trunc(q) * b < a + b
  ##
  _mpd_qmul(rr, addr(bb), qq, addr(workctx), addr(workctx.status))
  ##  Get the estimate for r such that a = q * b + r.
  _mpd_qsub_exact(rr, addr(aa), rr, addr(workctx), addr(workctx.status))
  ##  Fix the result. At this point -b < r < 2*b, so the correction loop
  ##        takes at most one iteration.
  k = 0
  while true:
    if mpd_isspecial(qq) or mpd_isspecial(rr):
      status[] = status[] or (workctx.status and MPD_Errors)
      break nanresult
    if k > 2:
      ##  Allow two iterations despite the proof.
      mpd_err_warn("libmpdec: internal error in _mpd_base_ndivmod: please report")
      ##  GCOV_NOT_REACHED
      status[] = status[] or MPD_Invalid_operation
      ##  GCOV_NOT_REACHED
      break nanresult
      ##  GCOV_NOT_REACHED
    elif _mpd_cmp(addr(zero), rr) == 1: ##  0 <= r < b
      _mpd_qadd_exact(rr, rr, addr(bb), addr(workctx), addr(workctx.status))
      _mpd_qadd_exact(qq, qq, addr(minus_one), addr(workctx), addr(workctx.status))
    elif _mpd_cmp(rr, addr(bb)) == -1: ##  r >= b
      break
    else:
      _mpd_qsub_exact(rr, rr, addr(bb), addr(workctx), addr(workctx.status))
      _mpd_qadd_exact(qq, qq, addr(one), addr(workctx), addr(workctx.status))
    inc(k)
  if qq != q:
    if not mpd_qcopy(q, qq, status):
      break nanresult
      ##  GCOV_UNLIKELY
    mpd_del(qq)
  if rr != r:
    if not mpd_qcopy(r, rr, status):
      break nanresult
      ##  GCOV_UNLIKELY
    mpd_del(rr)
  status[] = status[] or (workctx.status and MPD_Errors)
  return
  if qq and qq != q:
    mpd_del(qq)
  if rr and rr != r:
    mpd_del(rr)
  mpd_setspecial(q, MPD_POS, MPD_NAN)
  mpd_setspecial(r, MPD_POS, MPD_NAN)

##  LIBMPDEC_ONLY
##
##  Schedule the optimal precision increase for the Newton iteration.
##    v := input operand
##    z_0 := initial approximation
##    initprec := natural number such that abs(sqrt(v) - z_0) < 10**-initprec
##    maxprec := target precision
##
##  For convenience the output klist contains the elements in reverse order:
##    klist := [k_n-1, ..., k_0], where
##      1) k_0 <= initprec and
##      2) abs(sqrt(v) - result) < 10**(-2*k_n-1 + 2) <= 10**-maxprec.
##

proc invroot_schedule_prec*(klist: array[MPD_MAX_PREC_LOG2, mpd_ssize_t];
                           maxprec: mpd_ssize_t; initprec: mpd_ssize_t): cint {.
    inline.} =
  var k: mpd_ssize_t
  var i: cint
  assert(maxprec >= 3 and initprec >= 3)
  if maxprec <= initprec:
    return -1
  i = 0
  k = maxprec
  while true:
    k = (k + 3) div 2
    klist[inc(i)] = k
    if not (k > initprec):
      break
  return i - 1

##
##  Initial approximation for the inverse square root function.
##    Input:
##      v := rational number, with 1 <= v < 100
##      vhat := floor(v * 10**6)
##    Output:
##      z := approximation to 1/sqrt(v), such that abs(z - 1/sqrt(v)) < 10**-3.
##

proc _invroot_init_approx*(z: ptr mpd_t; vhat: mpd_uint_t) {.inline.} =
  var lo: mpd_uint_t = 1000
  var hi: mpd_uint_t = 10000
  var
    a: mpd_uint_t
    sq: mpd_uint_t
  assert(lo * lo <= vhat and vhat < (hi + 1) * (hi + 1))
  while true:
    a = (lo + hi) div 2
    sq = a * a
    if vhat >= sq:
      if vhat < sq + 2 * a + 1:
        break
      lo = a + 1
    else:
      hi = a - 1
  ##
  ##  After the binary search we have:
  ##   1) a**2 <= floor(v * 10**6) < (a + 1)**2
  ##  This implies:
  ##   2) a**2 <= v * 10**6 < (a + 1)**2
  ##   3) a <= sqrt(v) * 10**3 < a + 1
  ##  Since 10**3 <= a:
  ##   4) 0 <= 10**prec/a - 1/sqrt(v) < 10**-prec
  ##  We have:
  ##   5) 10**3/a - 10**-3 < floor(10**9/a) * 10**-6 <= 10**3/a
  ##  Merging 4) and 5):
  ##   6) abs(floor(10**9/a) * 10**-6 - 1/sqrt(v)) < 10**-3
  ##
  mpd_minalloc(z)
  mpd_clear_flags(z)
  z.data[0] = 1000000000 div a
  z.len = 1
  z.exp = -6
  mpd_setdigits(z)

##
##  Set 'result' to 1/sqrt(a).
##    Relative error: abs(result - 1/sqrt(a)) < 10**-prec * 1/sqrt(a)
##

proc _mpd_qinvroot*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                   status: ptr uint32_t) =
  var workstatus: uint32_t = 0
  var
    varcontext: mpd_context_t
    maxcontext: mpd_context_t
  var z: ptr mpd_t = result
  ##  current approximation
  var v: ptr mpd_t
  ##  a, normalized to a number between 1 and 100
  MPD_NEW_SHARED(vtmp, a)
  ##  by default v will share data with a
  MPD_NEW_STATIC(s, 0, 0, 0, 0)
  ##  temporary variable
  MPD_NEW_STATIC(t, 0, 0, 0, 0)
  ##  temporary variable
  MPD_NEW_CONST(one_half, 0, -1, 1, 1, 1, 5)
  MPD_NEW_CONST(three, 0, 0, 1, 1, 1, 3)
  var klist: array[MPD_MAX_PREC_LOG2, mpd_ssize_t]
  var
    ideal_exp: mpd_ssize_t
    shift: mpd_ssize_t
  var
    adj: mpd_ssize_t
    tz: mpd_ssize_t
  var
    maxprec: mpd_ssize_t
    fracdigits: mpd_ssize_t
  var
    vhat: mpd_uint_t
    dummy: mpd_uint_t
  var
    i: cint
    n: cint
  ideal_exp = -((a.exp - (a.exp and 1)) div 2)
  v = addr(vtmp)
  if result == a:
    if (v = mpd_qncopy(a)) == nil:
      mpd_seterror(result, MPD_Malloc_error, status)
      return
  if (v.digits + v.exp) and 1:
    fracdigits = v.digits - 1
    v.exp = -fracdigits
    n = if (v.digits > 7): 7 else: cast[cint](v.digits)
    ##  Let vhat := floor(v * 10**(2*initprec))
    _mpd_get_msdigits(addr(dummy), addr(vhat), v, n)
    if n < 7:
      vhat = vhat * mpd_pow10[7 - n]
  else:
    fracdigits = v.digits - 2
    v.exp = -fracdigits
    n = if (v.digits > 8): 8 else: cast[cint](v.digits)
    ##  Let vhat := floor(v * 10**(2*initprec))
    _mpd_get_msdigits(addr(dummy), addr(vhat), v, n)
    if n < 8:
      vhat = vhat * mpd_pow10[8 - n]
  adj = (a.exp - v.exp) div 2
  ##  initial approximation
  _invroot_init_approx(z, vhat)
  mpd_maxcontext(addr(maxcontext))
  mpd_maxcontext(addr(varcontext))
  varcontext.round = MPD_ROUND_TRUNC
  maxprec = ctx.prec + 1
  ##  initprec == 3
  i = invroot_schedule_prec(klist, maxprec, 3)
  while i >= 0:
    varcontext.prec = 2 * klist[i] + 2
    mpd_qmul(addr(s), z, z, addr(maxcontext), addr(workstatus))
    if v.digits > varcontext.prec:
      shift = v.digits - varcontext.prec
      mpd_qshiftr(addr(t), v, shift, addr(workstatus))
      inc(t.exp, shift)
      mpd_qmul(addr(t), addr(t), addr(s), addr(varcontext), addr(workstatus))
    else:
      mpd_qmul(addr(t), v, addr(s), addr(varcontext), addr(workstatus))
    mpd_qsub(addr(t), addr(three), addr(t), addr(maxcontext), addr(workstatus))
    mpd_qmul(z, z, addr(t), addr(varcontext), addr(workstatus))
    mpd_qmul(z, z, addr(one_half), addr(maxcontext), addr(workstatus))
    dec(i)
  dec(z.exp, adj)
  tz = mpd_trail_zeros(result)
  shift = ideal_exp - result.exp
  shift = if (tz > shift): shift else: tz
  if shift > 0:
    mpd_qshiftr_inplace(result, shift)
    inc(result.exp, shift)
  mpd_del(addr(s))
  mpd_del(addr(t))
  if v != addr(vtmp):
    mpd_del(v)
  status[] = status[] or (workstatus and MPD_Errors)
  status[] = status[] or (MPD_Rounded or MPD_Inexact)

proc mpd_qinvroot*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                  status: ptr uint32_t) =
  var workctx: mpd_context_t
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    if mpd_isnegative(a):
      mpd_seterror(result, MPD_Invalid_operation, status)
      return
    _settriple(result, MPD_POS, 0, mpd_etiny(ctx))
    status[] = status[] or MPD_Clamped
    return
  if mpd_iszero(a):
    mpd_setspecial(result, mpd_sign(a), MPD_INF)
    status[] = status[] or MPD_Division_by_zero
    return
  if mpd_isnegative(a):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  workctx = ctx[]
  inc(workctx.prec, 2)
  workctx.round = MPD_ROUND_HALF_EVEN
  _mpd_qinvroot(result, a, addr(workctx), status)
  mpd_qfinalize(result, ctx, status)

##  END LIBMPDEC_ONLY
##  Algorithm from decimal.py

proc _mpd_qsqrt*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
                status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  MPD_NEW_STATIC(c, 0, 0, 0, 0)
  MPD_NEW_STATIC(q, 0, 0, 0, 0)
  MPD_NEW_STATIC(r, 0, 0, 0, 0)
  MPD_NEW_CONST(two, 0, 0, 1, 1, 1, 2)
  var
    prec: mpd_ssize_t
    ideal_exp: mpd_ssize_t
  var
    l: mpd_ssize_t
    shift: mpd_ssize_t
  var exact: cint = 0
  ideal_exp = (a.exp - (a.exp and 1)) div 2
  if mpd_isspecial(a):
    if mpd_qcheck_nan(result, a, ctx, status):
      return
    if mpd_isnegative(a):
      mpd_seterror(result, MPD_Invalid_operation, status)
      return
    mpd_setspecial(result, MPD_POS, MPD_INF)
    return
  if mpd_iszero(a):
    _settriple(result, mpd_sign(a), 0, ideal_exp)
    mpd_qfinalize(result, ctx, status)
    return
  if mpd_isnegative(a):
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  mpd_maxcontext(addr(maxcontext))
  prec = ctx.prec + 1
  if not mpd_qcopy(addr(c), a, status):
    break malloc_error
  c.exp = 0
  if a.exp and 1:
    if not mpd_qshiftl(addr(c), addr(c), 1, status):
      break malloc_error
    l = (a.digits shr 1) + 1
  else:
    l = (a.digits + 1) shr 1
  shift = prec - l
  if shift >= 0:
    if not mpd_qshiftl(addr(c), addr(c), 2 * shift, status):
      break malloc_error
    exact = 1
  else:
    exact = not mpd_qshiftr_inplace(addr(c), -(2 * shift))
  dec(ideal_exp, shift)
  ##  find result = floor(sqrt(c)) using Newton's method
  if not mpd_qshiftl(result, addr(one), prec, status):
    break malloc_error
  while 1:
    _mpd_qdivmod(addr(q), addr(r), addr(c), result, addr(maxcontext),
                 addr(maxcontext.status))
    if mpd_isspecial(result) or mpd_isspecial(addr(q)):
      mpd_seterror(result, maxcontext.status and MPD_Errors, status)
      break `out`
    if _mpd_cmp(result, addr(q)) <= 0:
      break
    _mpd_qadd_exact(result, result, addr(q), addr(maxcontext),
                    addr(maxcontext.status))
    if mpd_isspecial(result):
      mpd_seterror(result, maxcontext.status and MPD_Errors, status)
      break `out`
    _mpd_qdivmod(result, addr(r), result, addr(two), addr(maxcontext),
                 addr(maxcontext.status))
  if exact:
    _mpd_qmul_exact(addr(r), result, result, addr(maxcontext),
                    addr(maxcontext.status))
    if mpd_isspecial(addr(r)):
      mpd_seterror(result, maxcontext.status and MPD_Errors, status)
      break `out`
    exact = (_mpd_cmp(addr(r), addr(c)) == 0)
  if exact:
    if shift >= 0:
      mpd_qshiftr_inplace(result, shift)
    else:
      if not mpd_qshiftl(result, result, -shift, status):
        break malloc_error
    inc(ideal_exp, shift)
  else:
    var lsd: cint = cast[cint](mpd_lsd(result.data[0]))
    if lsd == 0 or lsd == 5:
      inc(result.data[0], 1)
  result.exp = ideal_exp
  mpd_del(addr(c))
  mpd_del(addr(q))
  mpd_del(addr(r))
  maxcontext = ctx[]
  maxcontext.round = MPD_ROUND_HALF_EVEN
  mpd_qfinalize(result, addr(maxcontext), status)
  return
  mpd_seterror(result, MPD_Malloc_error, status)
  break `out`

proc mpd_qsqrt*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t;
               status: ptr uint32_t) =
  MPD_NEW_STATIC(aa, 0, 0, 0, 0)
  var xstatus: uint32_t = 0
  if result == a:
    if not mpd_qcopy(addr(aa), a, status):
      mpd_seterror(result, MPD_Malloc_error, status)
      break `out`
    a = addr(aa)
  _mpd_qsqrt(result, a, ctx, addr(xstatus))
  if xstatus and (MPD_Malloc_error or MPD_Division_impossible):
    ##  The above conditions can occur at very high context precisions
    ##  if intermediate values get too large. Retry the operation with
    ##  a lower context precision in case the result is exact.
    ##
    ##  If the result is exact, an upper bound for the number of digits
    ##  is the number of digits in the input.
    ##
    ##  NOTE: sqrt(40e9) = 2.0e+5 /\ digits(40e9) = digits(2.0e+5) = 2
    ##
    var ystatus: uint32_t = 0
    var workctx: mpd_context_t = ctx[]
    workctx.prec = a.digits
    if workctx.prec >= ctx.prec:
      status[] = status[] or (xstatus or MPD_Errors)
      break `out`
      ##  No point in repeating this, keep the original error.
    _mpd_qsqrt(result, a, addr(workctx), addr(ystatus))
    if ystatus != 0:
      ystatus = status[] or ((xstatus or ystatus) and MPD_Errors)
      mpd_seterror(result, ystatus, status)
  else:
    status[] = status[] or xstatus
  mpd_del(addr(aa))

## ****************************************************************************
##                               Base conversions
## ****************************************************************************
##  Space needed to represent an integer mpd_t in base 'base'.

proc mpd_sizeinbase*(a: ptr mpd_t; base: uint32_t): csize =
  var x: cdouble
  var digits: csize
  var upper_bound: cdouble
  assert(mpd_isinteger(a))
  assert(base >= 2)
  if mpd_iszero(a):
    return 1
  digits = a.digits + a.exp
  when defined(CONFIG_64):
    ##  ceil(2711437152599294 / log10(2)) + 4 == 2**53
    if digits > 2711437152599294'i64:
      return SIZE_MAX
    upper_bound = (double)((1 shl 53) - 1)
  else:
    upper_bound = (double)(SIZE_MAX - 1)
  x = cast[cdouble](digits div log10(base))
  return if (x > upper_bound): SIZE_MAX else: cast[csize](x) + 1

##  Space needed to import a base 'base' integer of length 'srclen'.

proc _mpd_importsize*(srclen: csize; base: uint32_t): mpd_ssize_t =
  var x: cdouble
  var upper_bound: cdouble
  assert(srclen > 0)
  assert(base >= 2)
  when SIZE_MAX == UINT64_MAX:
    if srclen > (1 shl 53):
      return MPD_SSIZE_MAX
    assert((1 shl 53) <= MPD_MAXIMPORT)
    upper_bound = (double)((1 shl 53) - 1)
  else:
    upper_bound = MPD_MAXIMPORT - 1
  x = cast[cdouble](srclen * (log10(base) div MPD_RDIGITS))
  return if (x > upper_bound): MPD_SSIZE_MAX else: cast[mpd_ssize_t](x) + 1

proc mpd_resize_u16*(w: ptr ptr uint16_t; nmemb: csize): uint8_t =
  var err: uint8_t = 0
  w[] = mpd_realloc(w[], nmemb, sizeof(w[][]), addr(err))
  return not err

proc mpd_resize_u32*(w: ptr ptr uint32_t; nmemb: csize): uint8_t =
  var err: uint8_t = 0
  w[] = mpd_realloc(w[], nmemb, sizeof(w[][]), addr(err))
  return not err

proc _baseconv_to_u16*(w: ptr ptr uint16_t; wlen: csize; wbase: mpd_uint_t;
                      u: ptr mpd_uint_t; ulen: mpd_ssize_t): csize =
  var n: csize = 0
  assert(wlen > 0 and ulen > 0)
  assert(wbase <= (1 shl 16))
  while true:
    if n >= wlen:
      if not mpd_resize_u16(w, n + 1):
        return SIZE_MAX
      wlen = n + 1
    (w[])[inc(n)] = cast[uint16_t](_mpd_shortdiv(u, u, ulen, wbase))
    ##  ulen is at least 1. u[ulen-1] can only be zero if ulen == 1.
    ulen = _mpd_real_size(u, ulen)
    if not (u[ulen - 1] != 0):
      break
  return n

proc _coeff_from_u16*(w: ptr mpd_t; wlen: mpd_ssize_t; u: ptr mpd_uint_t; ulen: csize;
                     ubase: uint32_t; status: ptr uint32_t): csize =
  var n: mpd_ssize_t = 0
  var carry: mpd_uint_t
  assert(wlen > 0 and ulen > 0)
  assert(ubase <= (1 shl 16))
  w.data[inc(n)] = u[dec(ulen)]
  while dec(ulen) != SIZE_MAX:
    carry = _mpd_shortmul_c(w.data, w.data, n, ubase)
    if carry:
      if n >= wlen:
        if not mpd_qresize(w, n + 1, status):
          return SIZE_MAX
        wlen = n + 1
      w.data[inc(n)] = carry
    carry = _mpd_shortadd(w.data, n, u[ulen])
    if carry:
      if n >= wlen:
        if not mpd_qresize(w, n + 1, status):
          return SIZE_MAX
        wlen = n + 1
      w.data[inc(n)] = carry
  return n

##  target base wbase < source base ubase

proc _baseconv_to_smaller*(w: ptr ptr uint32_t; wlen: csize; wbase: uint32_t;
                          u: ptr mpd_uint_t; ulen: mpd_ssize_t; ubase: mpd_uint_t): csize =
  var n: csize = 0
  assert(wlen > 0 and ulen > 0)
  assert(wbase < ubase)
  while true:
    if n >= wlen:
      if not mpd_resize_u32(w, n + 1):
        return SIZE_MAX
      wlen = n + 1
    (w[])[inc(n)] = cast[uint32_t](_mpd_shortdiv_b(u, u, ulen, wbase, ubase))
    ##  ulen is at least 1. u[ulen-1] can only be zero if ulen == 1.
    ulen = _mpd_real_size(u, ulen)
    if not (u[ulen - 1] != 0):
      break
  return n

when defined(CONFIG_32):
  ##  target base 'wbase' == source base 'ubase'
  proc _copy_equal_base*(w: ptr ptr uint32_t; wlen: csize; u: ptr uint32_t; ulen: csize): csize =
    if wlen < ulen:
      if not mpd_resize_u32(w, ulen):
        return SIZE_MAX
    memcpy(w[], u, ulen * (sizeof(w[][])))
    return ulen

  ##  target base 'wbase' > source base 'ubase'
  proc _baseconv_to_larger*(w: ptr ptr uint32_t; wlen: csize; wbase: mpd_uint_t;
                           u: ptr mpd_uint_t; ulen: csize; ubase: mpd_uint_t): csize =
    var n: csize = 0
    var carry: mpd_uint_t
    assert(wlen > 0 and ulen > 0)
    assert(ubase < wbase)
    (w[])[inc(n)] = u[dec(ulen)]
    while dec(ulen) != SIZE_MAX:
      carry = _mpd_shortmul_b(w[], w[], n, ubase, wbase)
      if carry:
        if n >= wlen:
          if not mpd_resize_u32(w, n + 1):
            return SIZE_MAX
          wlen = n + 1
        (w[])[inc(n)] = carry
      carry = _mpd_shortadd_b(w[], n, u[ulen], wbase)
      if carry:
        if n >= wlen:
          if not mpd_resize_u32(w, n + 1):
            return SIZE_MAX
          wlen = n + 1
        (w[])[inc(n)] = carry
    return n

  ##  target base wbase < source base ubase
  proc _coeff_from_larger_base*(w: ptr mpd_t; wlen: csize; wbase: mpd_uint_t;
                               u: ptr mpd_uint_t; ulen: mpd_ssize_t;
                               ubase: mpd_uint_t; status: ptr uint32_t): csize =
    var n: csize = 0
    assert(wlen > 0 and ulen > 0)
    assert(wbase < ubase)
    while true:
      if n >= wlen:
        if not mpd_qresize(w, n + 1, status):
          return SIZE_MAX
        wlen = n + 1
      w.data[inc(n)] = cast[uint32_t](_mpd_shortdiv_b(u, u, ulen, wbase, ubase))
      ##  ulen is at least 1. u[ulen-1] can only be zero if ulen == 1.
      ulen = _mpd_real_size(u, ulen)
      if not (u[ulen - 1] != 0):
        break
    return n

##  target base 'wbase' > source base 'ubase'

proc _coeff_from_smaller_base*(w: ptr mpd_t; wlen: mpd_ssize_t; wbase: mpd_uint_t;
                              u: ptr uint32_t; ulen: csize; ubase: mpd_uint_t;
                              status: ptr uint32_t): csize =
  var n: mpd_ssize_t = 0
  var carry: mpd_uint_t
  assert(wlen > 0 and ulen > 0)
  assert(wbase > ubase)
  w.data[inc(n)] = u[dec(ulen)]
  while dec(ulen) != SIZE_MAX:
    carry = _mpd_shortmul_b(w.data, w.data, n, ubase, wbase)
    if carry:
      if n >= wlen:
        if not mpd_qresize(w, n + 1, status):
          return SIZE_MAX
        wlen = n + 1
      w.data[inc(n)] = carry
    carry = _mpd_shortadd_b(w.data, n, u[ulen], wbase)
    if carry:
      if n >= wlen:
        if not mpd_qresize(w, n + 1, status):
          return SIZE_MAX
        wlen = n + 1
      w.data[inc(n)] = carry
  return n

##
##  Convert an integer mpd_t to a multiprecision integer with base <= 2**16.
##  The least significant word of the result is (*rdata)[0].
##
##  If rdata is NULL, space is allocated by the function and rlen is irrelevant.
##  In case of an error any allocated storage is freed and rdata is set back to
##  NULL.
##
##  If rdata is non-NULL, it MUST be allocated by one of libmpdec's allocation
##  functions and rlen MUST be correct. If necessary, the function will resize
##  rdata. In case of an error the caller must free rdata.
##
##  Return value: In case of success, the exact length of rdata, SIZE_MAX
##  otherwise.
##

proc mpd_qexport_u16*(rdata: ptr ptr uint16_t; rlen: csize; rbase: uint32_t;
                     src: ptr mpd_t; status: ptr uint32_t): csize =
  MPD_NEW_STATIC(tsrc, 0, 0, 0, 0)
  var alloc: cint = 0
  ##  rdata == NULL
  var n: csize
  assert(rbase <= (1 shl 16))
  if mpd_isspecial(src) or not _mpd_isint(src):
    status[] = status[] or MPD_Invalid_operation
    return SIZE_MAX
  if rdata[] == nil:
    rlen = mpd_sizeinbase(src, rbase)
    if rlen == SIZE_MAX:
      status[] = status[] or MPD_Invalid_operation
      return SIZE_MAX
    rdata[] = mpd_alloc(rlen, sizeof(rdata[][]))
    if rdata[] == nil:
      break malloc_error
    alloc = 1
  if mpd_iszero(src):
    rdata[][] = 0
    return 1
  if src.exp >= 0:
    if not mpd_qshiftl(addr(tsrc), src, src.exp, status):
      break malloc_error
  else:
    if mpd_qshiftr(addr(tsrc), src, -src.exp, status) == MPD_UINT_MAX:
      break malloc_error
  n = _baseconv_to_u16(rdata, rlen, rbase, tsrc.data, tsrc.len)
  if n == SIZE_MAX:
    break malloc_error
  mpd_del(addr(tsrc))
  return n
  if alloc:
    mpd_free(rdata[])
    rdata[] = nil
  n = SIZE_MAX
  status[] = status[] or MPD_Malloc_error
  break `out`

##
##  Convert an integer mpd_t to a multiprecision integer with base<=UINT32_MAX.
##  The least significant word of the result is (*rdata)[0].
##
##  If rdata is NULL, space is allocated by the function and rlen is irrelevant.
##  In case of an error any allocated storage is freed and rdata is set back to
##  NULL.
##
##  If rdata is non-NULL, it MUST be allocated by one of libmpdec's allocation
##  functions and rlen MUST be correct. If necessary, the function will resize
##  rdata. In case of an error the caller must free rdata.
##
##  Return value: In case of success, the exact length of rdata, SIZE_MAX
##  otherwise.
##

proc mpd_qexport_u32*(rdata: ptr ptr uint32_t; rlen: csize; rbase: uint32_t;
                     src: ptr mpd_t; status: ptr uint32_t): csize =
  MPD_NEW_STATIC(tsrc, 0, 0, 0, 0)
  var alloc: cint = 0
  ##  rdata == NULL
  var n: csize
  if mpd_isspecial(src) or not _mpd_isint(src):
    status[] = status[] or MPD_Invalid_operation
    return SIZE_MAX
  if rdata[] == nil:
    rlen = mpd_sizeinbase(src, rbase)
    if rlen == SIZE_MAX:
      status[] = status[] or MPD_Invalid_operation
      return SIZE_MAX
    rdata[] = mpd_alloc(rlen, sizeof(rdata[][]))
    if rdata[] == nil:
      break malloc_error
    alloc = 1
  if mpd_iszero(src):
    rdata[][] = 0
    return 1
  if src.exp >= 0:
    if not mpd_qshiftl(addr(tsrc), src, src.exp, status):
      break malloc_error
  else:
    if mpd_qshiftr(addr(tsrc), src, -src.exp, status) == MPD_UINT_MAX:
      break malloc_error
  when defined(CONFIG_64):
    n = _baseconv_to_smaller(rdata, rlen, rbase, tsrc.data, tsrc.len, MPD_RADIX)
  else:
    if rbase == MPD_RADIX:
      n = _copy_equal_base(rdata, rlen, tsrc.data, tsrc.len)
    elif rbase < MPD_RADIX:
      n = _baseconv_to_smaller(rdata, rlen, rbase, tsrc.data, tsrc.len, MPD_RADIX)
    else:
      n = _baseconv_to_larger(rdata, rlen, rbase, tsrc.data, tsrc.len, MPD_RADIX)
  if n == SIZE_MAX:
    break malloc_error
  mpd_del(addr(tsrc))
  return n
  if alloc:
    mpd_free(rdata[])
    rdata[] = nil
  n = SIZE_MAX
  status[] = status[] or MPD_Malloc_error
  break `out`

##
##  Converts a multiprecision integer with base <= UINT16_MAX+1 to an mpd_t.
##  The least significant word of the source is srcdata[0].
##

proc mpd_qimport_u16*(result: ptr mpd_t; srcdata: ptr uint16_t; srclen: csize;
                     srcsign: uint8_t; srcbase: uint32_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
  var usrc: ptr mpd_uint_t
  ##  uint16_t src copied to an mpd_uint_t array
  var rlen: mpd_ssize_t
  ##  length of the result
  var n: csize
  assert(srclen > 0)
  assert(srcbase <= (1 shl 16))
  rlen = _mpd_importsize(srclen, srcbase)
  if rlen == MPD_SSIZE_MAX:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  usrc = mpd_alloc(cast[mpd_size_t](srclen), sizeof(usrc[]))
  if usrc == nil:
    mpd_seterror(result, MPD_Malloc_error, status)
    return
  n = 0
  while n < srclen:
    usrc[n] = srcdata[n]
    inc(n)
  if not mpd_qresize(result, rlen, status):
    break finish
  n = _coeff_from_u16(result, rlen, usrc, srclen, srcbase, status)
  if n == SIZE_MAX:
    break finish
  mpd_set_flags(result, srcsign)
  result.exp = 0
  result.len = n
  mpd_setdigits(result)
  mpd_qresize(result, result.len, status)
  mpd_qfinalize(result, ctx, status)
  mpd_free(usrc)

##
##  Converts a multiprecision integer with base <= UINT32_MAX to an mpd_t.
##  The least significant word of the source is srcdata[0].
##

proc mpd_qimport_u32*(result: ptr mpd_t; srcdata: ptr uint32_t; srclen: csize;
                     srcsign: uint8_t; srcbase: uint32_t; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
  var rlen: mpd_ssize_t
  ##  length of the result
  var n: csize
  assert(srclen > 0)
  rlen = _mpd_importsize(srclen, srcbase)
  if rlen == MPD_SSIZE_MAX:
    mpd_seterror(result, MPD_Invalid_operation, status)
    return
  if not mpd_qresize(result, rlen, status):
    return
  when defined(CONFIG_64):
    n = _coeff_from_smaller_base(result, rlen, MPD_RADIX, srcdata, srclen, srcbase,
                               status)
  else:
    if srcbase == MPD_RADIX:
      if not mpd_qresize(result, srclen, status):
        return
      memcpy(result.data, srcdata, srclen * (sizeof(srcdata[])))
      n = srclen
    elif srcbase < MPD_RADIX:
      n = _coeff_from_smaller_base(result, rlen, MPD_RADIX, srcdata, srclen, srcbase,
                                 status)
    else:
      var usrc: ptr mpd_uint_t = mpd_alloc(cast[mpd_size_t](srclen), sizeof(usrc[]))
      if usrc == nil:
        mpd_seterror(result, MPD_Malloc_error, status)
        return
      n = 0
      while n < srclen:
        usrc[n] = srcdata[n]
        inc(n)
      n = _coeff_from_larger_base(result, rlen, MPD_RADIX, usrc,
                                cast[mpd_ssize_t](srclen), srcbase, status)
      mpd_free(usrc)
  if n == SIZE_MAX:
    return
  mpd_set_flags(result, srcsign)
  result.exp = 0
  result.len = n
  mpd_setdigits(result)
  mpd_qresize(result, result.len, status)
  mpd_qfinalize(result, ctx, status)

## ****************************************************************************
##                                 From triple
## ****************************************************************************

when defined(CONFIG_64) and defined(__SIZEOF_INT128__):
  proc _set_coeff*(data: array[3, uint64_t]; hi: uint64_t; lo: uint64_t): mpd_ssize_t =
    var d: __uint128_t = (cast[__uint128_t](hi) shl 64) + lo
    var
      q: __uint128_t
      r: __uint128_t
    q = d div MPD_RADIX
    r = d mod MPD_RADIX
    data[0] = cast[uint64_t](r)
    d = q
    q = d div MPD_RADIX
    r = d mod MPD_RADIX
    data[1] = cast[uint64_t](r)
    d = q
    q = d div MPD_RADIX
    r = d mod MPD_RADIX
    data[2] = cast[uint64_t](r)
    if q != 0:
      abort()
      ##  GCOV_NOT_REACHED
    return if data[2] != 0: 3 else: (if data[1] != 0: 2 else: 1)

else:
  proc _uint_from_u16*(w: ptr mpd_uint_t; wlen: mpd_ssize_t; u: ptr uint16_t; ulen: csize): csize =
    var ubase: mpd_uint_t = 1 shl 16
    var n: mpd_ssize_t = 0
    var carry: mpd_uint_t
    assert(wlen > 0 and ulen > 0)
    w[inc(n)] = u[dec(ulen)]
    while dec(ulen) != SIZE_MAX:
      carry = _mpd_shortmul_c(w, w, n, ubase)
      if carry:
        if n >= wlen:
          abort()
          ##  GCOV_NOT_REACHED
        w[inc(n)] = carry
      carry = _mpd_shortadd(w, n, u[ulen])
      if carry:
        if n >= wlen:
          abort()
          ##  GCOV_NOT_REACHED
        w[inc(n)] = carry
    return n

  proc _set_coeff*(data: ptr mpd_uint_t; len: mpd_ssize_t; hi: uint64_t; lo: uint64_t): mpd_ssize_t =
    var u16: array[8, uint16_t] = [0]
    u16[7] = (uint16_t)((hi and 0x0000000000000000'i64) shr 48)
    u16[6] = (uint16_t)((hi and 0x0000000000000000'i64) shr 32)
    u16[5] = (uint16_t)((hi and 0x0000000000000000'i64) shr 16)
    u16[4] = (uint16_t)(hi and 0x0000000000000000'i64)
    u16[3] = (uint16_t)((lo and 0x0000000000000000'i64) shr 48)
    u16[2] = (uint16_t)((lo and 0x0000000000000000'i64) shr 32)
    u16[1] = (uint16_t)((lo and 0x0000000000000000'i64) shr 16)
    u16[0] = (uint16_t)(lo and 0x0000000000000000'i64)
    return cast[mpd_ssize_t](_uint_from_u16(data, len, u16, 8))

proc _set_uint128_coeff_exp*(result: ptr mpd_t; hi: uint64_t; lo: uint64_t;
                            exp: mpd_ssize_t): cint =
  var data: array[5, mpd_uint_t] = [0]
  var status: uint32_t = 0
  var len: mpd_ssize_t
  when defined(CONFIG_64) and defined(__SIZEOF_INT128__):
    len = _set_coeff(data, hi, lo)
  else:
    len = _set_coeff(data, 5, hi, lo)
  if not mpd_qresize(result, len, addr(status)):
    return -1
  var i: mpd_ssize_t = 0
  while i < len:
    result.data[i] = data[i]
    inc(i)
  result.exp = exp
  result.len = len
  mpd_setdigits(result)
  return 0

proc mpd_from_uint128_triple*(result: ptr mpd_t; triple: ptr mpd_uint128_triple_t;
                             status: ptr uint32_t): cint =
  ##  c2nim TODO
  ##     static const mpd_context_t maxcontext = {
  ##      .prec=MPD_MAX_PREC,
  ##      .emax=MPD_MAX_EMAX,
  ##      .emin=MPD_MIN_EMIN,
  ##      .round=MPD_ROUND_HALF_EVEN,
  ##      .traps=MPD_Traps,
  ##      .status=0,
  ##      .newtrap=0,
  ##      .clamp=0,
  ##      .allcr=1,
  ##     };
  ##
  var tag: mpd_triple_class = triple.tag
  var sign: uint8_t = triple.sign
  var hi: uint64_t = triple.hi
  var lo: uint64_t = triple.lo
  var exp: mpd_ssize_t
  when defined(CONFIG_32):
    if triple.exp < MPD_SSIZE_MIN or triple.exp > MPD_SSIZE_MAX:
      break conversion_error
  exp = cast[mpd_ssize_t](triple.exp)
  case tag
  of MPD_TRIPLE_QNAN, MPD_TRIPLE_SNAN:
    if sign > 1 or exp != 0:
      break conversion_error
    var flags: uint8_t = if tag == MPD_TRIPLE_QNAN: MPD_NAN else: MPD_SNAN
    mpd_setspecial(result, sign, flags)
    if hi == 0 and lo == 0:
      ##  no payload
      return 0
    if _set_uint128_coeff_exp(result, hi, lo, exp) < 0:
      break malloc_error
    return 0
  of MPD_TRIPLE_INF:
    if sign > 1 or hi != 0 or lo != 0 or exp != 0:
      break conversion_error
    mpd_setspecial(result, sign, MPD_INF)
    return 0
  of MPD_TRIPLE_NORMAL:
    if sign > 1:
      break conversion_error
    var flags: uint8_t = if sign: MPD_NEG else: MPD_POS
    mpd_set_flags(result, flags)
    if exp > MPD_EXP_INF:
      exp = MPD_EXP_INF
    if exp == MPD_SSIZE_MIN:
      exp = MPD_SSIZE_MIN + 1
    if _set_uint128_coeff_exp(result, hi, lo, exp) < 0:
      break malloc_error
    var workstatus: uint32_t = 0
    mpd_qfinalize(result, addr(maxcontext), addr(workstatus))
    if workstatus and (MPD_Inexact or MPD_Rounded or MPD_Clamped):
      break conversion_error
    return 0
  else:
    break conversion_error
  mpd_seterror(result, MPD_Conversion_syntax, status)
  return -1
  mpd_seterror(result, MPD_Malloc_error, status)
  return -1

## ****************************************************************************
##                                   As triple
## ****************************************************************************

when defined(CONFIG_64) and defined(__SIZEOF_INT128__):
  proc _get_coeff*(hi: ptr uint64_t; lo: ptr uint64_t; a: ptr mpd_t) =
    var u128: __uint128_t = 0
    case a.len
    of 3:
      u128 = a.data[2]
      ##  fall through
    of 2:
      u128 = u128 * MPD_RADIX + a.data[1]
      ##  fall through
    of 1:
      u128 = u128 * MPD_RADIX + a.data[0]
    else:
      abort()
      ##  GCOV_NOT_REACHED
    hi[] = u128 shr 64
    lo[] = cast[uint64_t](u128)

else:
  proc _uint_to_u16*(w: array[8, uint16_t]; u: ptr mpd_uint_t; ulen: mpd_ssize_t): csize =
    var wbase: mpd_uint_t = 1 shl 16
    var n: csize = 0
    assert(ulen > 0)
    while true:
      if n >= 8:
        abort()
        ##  GCOV_NOT_REACHED
      w[inc(n)] = cast[uint16_t](_mpd_shortdiv(u, u, ulen, wbase))
      ##  ulen is at least 1. u[ulen-1] can only be zero if ulen == 1.
      ulen = _mpd_real_size(u, ulen)
      if not (u[ulen - 1] != 0):
        break
    return n

  proc _get_coeff*(hi: ptr uint64_t; lo: ptr uint64_t; a: ptr mpd_t) =
    var u16: array[8, uint16_t] = [0]
    var data: array[5, mpd_uint_t] = [0]
    case a.len
    of 5:
      data[4] = a.data[4]
      ##  fall through
    of 4:
      data[3] = a.data[3]
      ##  fall through
    of 3:
      data[2] = a.data[2]
      ##  fall through
    of 2:
      data[1] = a.data[1]
      ##  fall through
    of 1:
      data[0] = a.data[0]
    else:
      abort()
      ##  GCOV_NOT_REACHED
    _uint_to_u16(u16, data, a.len)
    hi[] = cast[uint64_t](u16[7]) shl 48
    hi[] = hi[] or cast[uint64_t](u16[6]) shl 32
    hi[] = hi[] or cast[uint64_t](u16[5]) shl 16
    hi[] = hi[] or cast[uint64_t](u16[4])
    lo[] = cast[uint64_t](u16[3]) shl 48
    lo[] = lo[] or cast[uint64_t](u16[2]) shl 32
    lo[] = lo[] or cast[uint64_t](u16[1]) shl 16
    lo[] = lo[] or cast[uint64_t](u16[0])

proc _coeff_as_uint128*(hi: ptr uint64_t; lo: ptr uint64_t; a: ptr mpd_t): mpd_triple_class =
  when defined(CONFIG_64):
    var uint128_max_data: array[3, mpd_uint_t] = [3374607431768211455'i64,
        4028236692093846346'i64, 3]
    var uint128_max: mpd_t = [MPD_STATIC or MPD_CONST_DATA, 0, 39, 3, 3, uint128_max_data]
  else:
    var uint128_max_data: array[5, mpd_uint_t] = [768211455, 374607431, 938463463,
        282366920, 340]
    var uint128_max: mpd_t = [MPD_STATIC or MPD_CONST_DATA, 0, 39, 5, 5, uint128_max_data]
  var ret: mpd_triple_class = MPD_TRIPLE_NORMAL
  var status: uint32_t = 0
  var coeff: mpd_t
  hi[] = lo[] = 0
  if mpd_isspecial(a):
    if mpd_isinfinite(a):
      return MPD_TRIPLE_INF
    ret = if mpd_isqnan(a): MPD_TRIPLE_QNAN else: MPD_TRIPLE_SNAN
    if a.len == 0:
      ##  no payload
      return ret
  elif mpd_iszero(a):
    return ret
  _mpd_copy_shared(addr(coeff), a)
  mpd_set_flags(addr(coeff), 0)
  coeff.exp = 0
  if mpd_qcmp(addr(coeff), addr(uint128_max), addr(status)) > 0:
    return MPD_TRIPLE_ERROR
  _get_coeff(hi, lo, addr(coeff))
  return ret

proc mpd_as_uint128_triple*(a: ptr mpd_t): mpd_uint128_triple_t =
  var triple: mpd_uint128_triple_t = [MPD_TRIPLE_ERROR, 0, 0, 0, 0]
  triple.tag = _coeff_as_uint128(addr(triple.hi), addr(triple.lo), a)
  if triple.tag == MPD_TRIPLE_ERROR:
    return triple
  triple.sign = not not mpd_isnegative(a)
  if triple.tag == MPD_TRIPLE_NORMAL:
    triple.exp = a.exp
  return triple
