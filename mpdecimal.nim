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
  when defined(msc_Ver):
  elif not defined(openBSD) and not defined(netBSD):
    ##  C99
##  Disable warning that is part of -Wextra since gcc 7.0.

when defined(gnuc) and not defined(intel_Compiler) and gnuc >= 7:
when defined(msc_Ver):
elif defined(ibmc) or defined(legacy_Compiler):
  const
    forceinline* = true
else:
  when defined(TEST_COVERAGE):
    const
      forceinline* = true
  else:
    const
      forceinline* = attribute((alwaysInline))
const
  MPD_NEWTONDIV_CUTOFF* = 1024

var dataOne*: array[1, MpdUintT] = [1]

var dataZero*: array[1, MpdUintT] = [0]

var one*: MpdT = [mpd_Static or mpd_Const_Data, 0, 1, 1, 1, dataOne]

var minusOne*: MpdT = [mpd_Neg or mpd_Static or mpd_Const_Data, 0, 1, 1, 1, dataOne]

var zero*: MpdT = [mpd_Static or mpd_Const_Data, 0, 1, 1, 1, dataZero]

proc mpdCheckExp*(dec: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
proc settriple*(result: ptr MpdT; sign: uint8T; a: MpdUintT; exp: MpdSsizeT)
proc mpdRealSize*(data: ptr MpdUintT; size: MpdSsizeT): MpdSsizeT
proc mpdCmpAbs*(a: ptr MpdT; b: ptr MpdT): cint
proc mpdQadd*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
proc mpdQmul*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
proc mpdBaseNdivmod*(q: ptr MpdT; r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; status: ptr uint32T)
proc mpdQpowUint*(result: ptr MpdT; base: ptr MpdT; exp: MpdUintT; resultsign: uint8T;
                 ctx: ptr MpdContextT; status: ptr uint32T)
proc mpdQsshiftr*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT): MpdUintT
## ****************************************************************************
##                                   Version
## ****************************************************************************

proc mpdVersion*(): cstring =
  return mpd_Version

## ****************************************************************************
##                   Performance critical inline functions
## ****************************************************************************

when defined(CONFIG_64):
  ##  Digits in a word, primarily useful for the most significant word.
  ##  ALWAYS_INLINE
  proc mpdWordDigits*(word: MpdUintT): cint =
    if word < mpdPow10[9]:
      if word < mpdPow10[4]:
        if word < mpdPow10[2]:
          return if (word < mpdPow10[1]): 1 else: 2
        return if (word < mpdPow10[3]): 3 else: 4
      if word < mpdPow10[6]:
        return if (word < mpdPow10[5]): 5 else: 6
      if word < mpdPow10[8]:
        return if (word < mpdPow10[7]): 7 else: 8
      return 9
    if word < mpdPow10[14]:
      if word < mpdPow10[11]:
        return if (word < mpdPow10[10]): 10 else: 11
      if word < mpdPow10[13]:
        return if (word < mpdPow10[12]): 12 else: 13
      return 14
    if word < mpdPow10[18]:
      if word < mpdPow10[16]:
        return if (word < mpdPow10[15]): 15 else: 16
      return if (word < mpdPow10[17]): 17 else: 18
    return if (word < mpdPow10[19]): 19 else: 20

else:
  ##  ALWAYS_INLINE
  proc mpdWordDigits*(word: MpdUintT): cint =
    if word < mpdPow10[4]:
      if word < mpdPow10[2]:
        return if (word < mpdPow10[1]): 1 else: 2
      return if (word < mpdPow10[3]): 3 else: 4
    if word < mpdPow10[6]:
      return if (word < mpdPow10[5]): 5 else: 6
    if word < mpdPow10[8]:
      return if (word < mpdPow10[7]): 7 else: 8
    return if (word < mpdPow10[9]): 9 else: 10

##  Adjusted exponent
##  ALWAYS_INLINE

proc mpdAdjexp*(dec: ptr MpdT): MpdSsizeT =
  return (dec.exp + dec.digits) - 1

##  Etiny
##  ALWAYS_INLINE

proc mpdEtiny*(ctx: ptr MpdContextT): MpdSsizeT =
  return ctx.emin - (ctx.prec - 1)

##  Etop: used for folding down in IEEE clamping
##  ALWAYS_INLINE

proc mpdEtop*(ctx: ptr MpdContextT): MpdSsizeT =
  return ctx.emax - (ctx.prec - 1)

##  Most significant word
##  ALWAYS_INLINE

proc mpdMsword*(dec: ptr MpdT): MpdUintT =
  assert(dec.len > 0)
  return dec.data[dec.len - 1]

##  Most significant digit of a word

proc mpdMsd*(word: MpdUintT): MpdUintT =
  var n: cint
  n = mpdWordDigits(word)
  return word div mpdPow10[n - 1]

##  Least significant digit of a word
##  ALWAYS_INLINE

proc mpdLsd*(word: MpdUintT): MpdUintT =
  return word mod 10

##  Coefficient size needed to store 'digits'

proc mpdDigitsToSize*(digits: MpdSsizeT): MpdSsizeT =
  var
    q: MpdSsizeT
    r: MpdSsizeT
  mpdIdivWord(addr(q), addr(r), digits, mpd_Rdigits)
  return if (r == 0): q else: q + 1

##  Number of digits in the exponent. Not defined for MPD_SSIZE_MIN.

proc mpdExpDigits*(exp: MpdSsizeT): cint =
  exp = if (exp < 0): -exp else: exp
  return mpdWordDigits(exp)

##  Canonical
##  ALWAYS_INLINE

proc mpdIscanonical*(dec: ptr MpdT): cint =
  cast[nil](dec)
  return 1

##  Finite
##  ALWAYS_INLINE

proc mpdIsfinite*(dec: ptr MpdT): cint =
  return not (dec.flags and mpd_Special)

##  Infinite
##  ALWAYS_INLINE

proc mpdIsinfinite*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Inf

##  NaN
##  ALWAYS_INLINE

proc mpdIsnan*(dec: ptr MpdT): cint =
  return dec.flags and (mpd_Nan or mpd_Snan)

##  Negative
##  ALWAYS_INLINE

proc mpdIsnegative*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Neg

##  Positive
##  ALWAYS_INLINE

proc mpdIspositive*(dec: ptr MpdT): cint =
  return not (dec.flags and mpd_Neg)

##  qNaN
##  ALWAYS_INLINE

proc mpdIsqnan*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Nan

##  Signed
##  ALWAYS_INLINE

proc mpdIssigned*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Neg

##  sNaN
##  ALWAYS_INLINE

proc mpdIssnan*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Snan

##  Special
##  ALWAYS_INLINE

proc mpdIsspecial*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Special

##  Zero
##  ALWAYS_INLINE

proc mpdIszero*(dec: ptr MpdT): cint =
  return not mpdIsspecial(dec) and mpdMsword(dec) == 0

##  Test for zero when specials have been ruled out already
##  ALWAYS_INLINE

proc mpdIszerocoeff*(dec: ptr MpdT): cint =
  return mpdMsword(dec) == 0

##  Normal

proc mpdIsnormal*(dec: ptr MpdT; ctx: ptr MpdContextT): cint =
  if mpdIsspecial(dec):
    return 0
  if mpdIszerocoeff(dec):
    return 0
  return mpdAdjexp(dec) >= ctx.emin

##  Subnormal

proc mpdIssubnormal*(dec: ptr MpdT; ctx: ptr MpdContextT): cint =
  if mpdIsspecial(dec):
    return 0
  if mpdIszerocoeff(dec):
    return 0
  return mpdAdjexp(dec) < ctx.emin

##  Odd word
##  ALWAYS_INLINE

proc mpdIsoddword*(word: MpdUintT): cint =
  return word and 1

##  Odd coefficient
##  ALWAYS_INLINE

proc mpdIsoddcoeff*(dec: ptr MpdT): cint =
  return mpdIsoddword(dec.data[0])

##  0 if dec is positive, 1 if dec is negative
##  ALWAYS_INLINE

proc mpdSign*(dec: ptr MpdT): uint8T =
  return dec.flags and mpd_Neg

##  1 if dec is positive, -1 if dec is negative
##  ALWAYS_INLINE

proc mpdArithSign*(dec: ptr MpdT): cint =
  return 1 - 2 * mpdIsnegative(dec)

##  Radix
##  ALWAYS_INLINE

proc mpdRadix*(): clong =
  return 10

##  Dynamic decimal
##  ALWAYS_INLINE

proc mpdIsdynamic*(dec: ptr MpdT): cint =
  return not (dec.flags and mpd_Static)

##  Static decimal
##  ALWAYS_INLINE

proc mpdIsstatic*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Static

##  Data of decimal is dynamic
##  ALWAYS_INLINE

proc mpdIsdynamicData*(dec: ptr MpdT): cint =
  return not (dec.flags and mpd_Dataflags)

##  Data of decimal is static
##  ALWAYS_INLINE

proc mpdIsstaticData*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Static_Data

##  Data of decimal is shared
##  ALWAYS_INLINE

proc mpdIssharedData*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Shared_Data

##  Data of decimal is const
##  ALWAYS_INLINE

proc mpdIsconstData*(dec: ptr MpdT): cint =
  return dec.flags and mpd_Const_Data

## ****************************************************************************
##                          Inline memory handling
## ****************************************************************************
##  Fill destination with zeros
##  ALWAYS_INLINE

proc mpdUintZero*(dest: ptr MpdUintT; len: MpdSizeT) =
  var i: MpdSizeT
  i = 0
  while i < len:
    dest[i] = 0
    inc(i)

##  Free a decimal
##  ALWAYS_INLINE

proc mpdDel*(dec: ptr MpdT) =
  if mpdIsdynamicData(dec):
    mpdFree(dec.data)
  if mpdIsdynamic(dec):
    mpdFree(dec)

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

proc mpdQresize*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint =
  assert(not mpdIsconstData(result))
  ##  illegal operation for a const
  assert(not mpdIssharedData(result))
  ##  illegal operation for a shared
  assert(mpd_Minalloc <= result.alloc)
  nwords = if (nwords <= mpd_Minalloc): mpd_Minalloc else: nwords
  if nwords == result.alloc:
    return 1
  if mpdIsstaticData(result):
    if nwords > result.alloc:
      return mpdSwitchToDyn(result, nwords, status)
    return 1
  return mpdReallocDyn(result, nwords, status)

##  Same as mpd_qresize, but do not set the result no NaN on failure.

proc mpdQresizeCxx*(result: ptr MpdT; nwords: MpdSsizeT): cint =
  assert(not mpdIsconstData(result))
  ##  illegal operation for a const
  assert(not mpdIssharedData(result))
  ##  illegal operation for a shared
  assert(mpd_Minalloc <= result.alloc)
  nwords = if (nwords <= mpd_Minalloc): mpd_Minalloc else: nwords
  if nwords == result.alloc:
    return 1
  if mpdIsstaticData(result):
    if nwords > result.alloc:
      return mpdSwitchToDynCxx(result, nwords)
    return 1
  return mpdReallocDynCxx(result, nwords)

##  Same as mpd_qresize, but the complete coefficient (including the old
##  memory area!) is initialized to zero.
##  ALWAYS_INLINE

proc mpdQresizeZero*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint =
  assert(not mpdIsconstData(result))
  ##  illegal operation for a const
  assert(not mpdIssharedData(result))
  ##  illegal operation for a shared
  assert(mpd_Minalloc <= result.alloc)
  nwords = if (nwords <= mpd_Minalloc): mpd_Minalloc else: nwords
  if nwords != result.alloc:
    if mpdIsstaticData(result):
      if nwords > result.alloc:
        return mpdSwitchToDynZero(result, nwords, status)
    elif not mpdReallocDyn(result, nwords, status):
      return 0
  mpdUintZero(result.data, nwords)
  return 1

##
##  Reduce memory size for the coefficient to MPD_MINALLOC. In theory,
##  realloc may fail even when reducing the memory size. But in that case
##  the old memory area is always big enough, so checking for MPD_Malloc_error
##  is not imperative.
##
##  ALWAYS_INLINE

proc mpdMinalloc*(result: ptr MpdT) =
  assert(not mpdIsconstData(result))
  ##  illegal operation for a const
  assert(not mpdIssharedData(result))
  ##  illegal operation for a shared
  if not mpdIsstaticData(result) and result.alloc > mpd_Minalloc:
    var err: uint8T = 0
    result.data = mpdRealloc(result.data, mpd_Minalloc, sizeof(result.data[]),
                           addr(err))
    if not err:
      result.alloc = mpd_Minalloc

proc mpdResize*(result: ptr MpdT; nwords: MpdSsizeT; ctx: ptr MpdContextT): cint =
  var status: uint32T = 0
  if not mpdQresize(result, nwords, addr(status)):
    mpdAddstatusRaise(ctx, status)
    return 0
  return 1

proc mpdResizeZero*(result: ptr MpdT; nwords: MpdSsizeT; ctx: ptr MpdContextT): cint =
  var status: uint32T = 0
  if not mpdQresizeZero(result, nwords, addr(status)):
    mpdAddstatusRaise(ctx, status)
    return 0
  return 1

## ****************************************************************************
##                        Set attributes of a decimal
## ****************************************************************************
##  Set digits. Assumption: result->len is initialized and > 0.

proc mpdSetdigits*(result: ptr MpdT) =
  var wdigits: MpdSsizeT = mpdWordDigits(mpdMsword(result))
  result.digits = wdigits + (result.len - 1) * mpd_Rdigits

##  Set sign
##  ALWAYS_INLINE

proc mpdSetSign*(result: ptr MpdT; sign: uint8T) =
  result.flags = result.flags and not mpd_Neg
  result.flags = result.flags or sign

##  Copy sign from another decimal
##  ALWAYS_INLINE

proc mpdSigncpy*(result: ptr MpdT; a: ptr MpdT) =
  var sign: uint8T = a.flags and mpd_Neg
  result.flags = result.flags and not mpd_Neg
  result.flags = result.flags or sign

##  Set infinity
##  ALWAYS_INLINE

proc mpdSetInfinity*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Special
  result.flags = result.flags or mpd_Inf

##  Set qNaN
##  ALWAYS_INLINE

proc mpdSetQnan*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Special
  result.flags = result.flags or mpd_Nan

##  Set sNaN
##  ALWAYS_INLINE

proc mpdSetSnan*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Special
  result.flags = result.flags or mpd_Snan

##  Set to negative
##  ALWAYS_INLINE

proc mpdSetNegative*(result: ptr MpdT) =
  result.flags = result.flags or mpd_Neg

##  Set to positive
##  ALWAYS_INLINE

proc mpdSetPositive*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Neg

##  Set to dynamic
##  ALWAYS_INLINE

proc mpdSetDynamic*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Static

##  Set to static
##  ALWAYS_INLINE

proc mpdSetStatic*(result: ptr MpdT) =
  result.flags = result.flags or mpd_Static

##  Set data to dynamic
##  ALWAYS_INLINE

proc mpdSetDynamicData*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Dataflags

##  Set data to static
##  ALWAYS_INLINE

proc mpdSetStaticData*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Dataflags
  result.flags = result.flags or mpd_Static_Data

##  Set data to shared
##  ALWAYS_INLINE

proc mpdSetSharedData*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Dataflags
  result.flags = result.flags or mpd_Shared_Data

##  Set data to const
##  ALWAYS_INLINE

proc mpdSetConstData*(result: ptr MpdT) =
  result.flags = result.flags and not mpd_Dataflags
  result.flags = result.flags or mpd_Const_Data

##  Clear flags, preserving memory attributes.
##  ALWAYS_INLINE

proc mpdClearFlags*(result: ptr MpdT) =
  result.flags = result.flags and (mpd_Static or mpd_Dataflags)

##  Set flags, preserving memory attributes.
##  ALWAYS_INLINE

proc mpdSetFlags*(result: ptr MpdT; flags: uint8T) =
  result.flags = result.flags and (mpd_Static or mpd_Dataflags)
  result.flags = result.flags or flags

##  Copy flags, preserving memory attributes of result.
##  ALWAYS_INLINE

proc mpdCopyFlags*(result: ptr MpdT; a: ptr MpdT) =
  var aflags: uint8T = a.flags
  result.flags = result.flags and (mpd_Static or mpd_Dataflags)
  result.flags = result.flags or (aflags and not (mpd_Static or mpd_Dataflags))

##  Initialize a workcontext from ctx. Set traps, flags and newtrap to 0.

proc mpdWorkcontext*(workctx: ptr MpdContextT; ctx: ptr MpdContextT) =
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

proc mpdNegate*(dec: ptr MpdT) =
  dec.flags = dec.flags xor mpd_Neg

##  Set coefficient to zero

proc mpdZerocoeff*(result: ptr MpdT) =
  mpdMinalloc(result)
  result.digits = 1
  result.len = 1
  result.data[0] = 0

##  Set the coefficient to all nines.

proc mpdQmaxcoeff*(result: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var
    len: MpdSsizeT
    r: MpdSsizeT
  mpdIdivWord(addr(len), addr(r), ctx.prec, mpd_Rdigits)
  len = if (r == 0): len else: len + 1
  if not mpdQresize(result, len, status):
    return
  result.len = len
  result.digits = ctx.prec
  dec(len)
  if r > 0:
    result.data[dec(len)] = mpdPow10[r] - 1
  while len >= 0:
    result.data[len] = mpd_Radix - 1
    dec(len)

##
##  Cut off the most significant digits so that the rest fits in ctx->prec.
##  Cannot fail.
##

proc mpdCap*(result: ptr MpdT; ctx: ptr MpdContextT) =
  var dummy: uint32T
  var
    len: MpdSsizeT
    r: MpdSsizeT
  if result.len > 0 and result.digits > ctx.prec:
    mpdIdivWord(addr(len), addr(r), ctx.prec, mpd_Rdigits)
    len = if (r == 0): len else: len + 1
    if r != 0:
      result.data[len - 1] = result.data[len - 1] mod mpdPow10[r]
    len = mpdRealSize(result.data, len)
    ##  resize to fewer words cannot fail
    mpdQresize(result, len, addr(dummy))
    result.len = len
    mpdSetdigits(result)
  if mpdIszero(result):
    settriple(result, mpdSign(result), 0, result.exp)

##
##  Cut off the most significant digits of a NaN payload so that the rest
##  fits in ctx->prec - ctx->clamp. Cannot fail.
##

proc mpdFixNan*(result: ptr MpdT; ctx: ptr MpdContextT) =
  var dummy: uint32T
  var prec: MpdSsizeT
  var
    len: MpdSsizeT
    r: MpdSsizeT
  prec = ctx.prec - ctx.clamp
  if result.len > 0 and result.digits > prec:
    if prec == 0:
      mpdMinalloc(result)
      result.len = result.digits = 0
    else:
      mpdIdivWord(addr(len), addr(r), prec, mpd_Rdigits)
      len = if (r == 0): len else: len + 1
      if r != 0:
        result.data[len - 1] = result.data[len - 1] mod mpdPow10[r]
      len = mpdRealSize(result.data, len)
      ##  resize to fewer words cannot fail
      mpdQresize(result, len, addr(dummy))
      result.len = len
      mpdSetdigits(result)
      if mpdIszerocoeff(result):
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

proc mpdGetMsdigits*(hi: ptr MpdUintT; lo: ptr MpdUintT; dec: ptr MpdT; n: cuint) =
  var
    r: MpdUintT
    tmp: MpdUintT
  assert(0 < n and n <= mpd_Rdigits + 1)
  mpdDivWord(addr(tmp), addr(r), dec.digits, mpd_Rdigits)
  r = if (r == 0): mpd_Rdigits else: r
  ##  digits in the most significant word
  hi[] = 0
  lo[] = dec.data[dec.len - 1]
  if n <= r:
    lo[] = lo[] / mpdPow10[r - n]
  elif dec.len > 1:
    ##  at this point 1 <= r < n <= MPD_RDIGITS+1
    mpdMulWords(hi, lo, lo[], mpdPow10[n - r])
    tmp = dec.data[dec.len - 2] div mpdPow10[mpd_Rdigits - (n - r)]
    lo[] = lo[] + tmp
    if lo[] < tmp:
      inc((hi[]))

## ****************************************************************************
##                    Gathering information about a decimal
## ****************************************************************************
##  The real size of the coefficient without leading zero words.

proc mpdRealSize*(data: ptr MpdUintT; size: MpdSsizeT): MpdSsizeT =
  while size > 1 and data[size - 1] == 0:
    dec(size)
  return size

##  Return number of trailing zeros. No errors are possible.

proc mpdTrailZeros*(dec: ptr MpdT): MpdSsizeT =
  var word: MpdUintT
  var
    i: MpdSsizeT
    tz: MpdSsizeT = 0
  i = 0
  while i < dec.len:
    if dec.data[i] != 0:
      word = dec.data[i]
      tz = i * mpd_Rdigits
      while word mod 10 == 0:
        word = word / 10
        inc(tz)
      break
    inc(i)
  return tz

##  Integer: Undefined for specials

proc mpdIsint*(dec: ptr MpdT): cint =
  var tz: MpdSsizeT
  if mpdIszerocoeff(dec):
    return 1
  tz = mpdTrailZeros(dec)
  return dec.exp + tz >= 0

##  Integer

proc mpdIsinteger*(dec: ptr MpdT): cint =
  if mpdIsspecial(dec):
    return 0
  return mpdIsint(dec)

##  Word is a power of 10

proc mpdWordIspow10*(word: MpdUintT): cint =
  var n: cint
  n = mpdWordDigits(word)
  if word == mpdPow10[n - 1]:
    return 1
  return 0

##  Coefficient is a power of 10

proc mpdCoeffIspow10*(dec: ptr MpdT): cint =
  if mpdWordIspow10(mpdMsword(dec)):
    if mpdIsallzero(dec.data, dec.len - 1):
      return 1
  return 0

##  All digits of a word are nines

proc mpdWordIsallnine*(word: MpdUintT): cint =
  var n: cint
  n = mpdWordDigits(word)
  if word == mpdPow10[n] - 1:
    return 1
  return 0

##  All digits of the coefficient are nines

proc mpdCoeffIsallnine*(dec: ptr MpdT): cint =
  if mpdWordIsallnine(mpdMsword(dec)):
    if mpdIsallnine(dec.data, dec.len - 1):
      return 1
  return 0

##  Odd decimal: Undefined for non-integers!

proc mpdIsodd*(dec: ptr MpdT): cint =
  var
    q: MpdUintT
    r: MpdUintT
  assert(mpdIsinteger(dec))
  if mpdIszerocoeff(dec):
    return 0
  if dec.exp < 0:
    mpdDivWord(addr(q), addr(r), -dec.exp, mpd_Rdigits)
    q = dec.data[q] div mpdPow10[r]
    return mpdIsoddword(q)
  return dec.exp == 0 and mpdIsoddword(dec.data[0])

##  Even: Undefined for non-integers!

proc mpdIseven*(dec: ptr MpdT): cint =
  return not mpdIsodd(dec)

## ****************************************************************************
##                       Getting and setting decimals
## ****************************************************************************
##  Internal function: Set a static decimal from a triple, no error checking.

proc ssettriple*(result: ptr MpdT; sign: uint8T; a: MpdUintT; exp: MpdSsizeT) =
  mpdSetFlags(result, sign)
  result.exp = exp
  mpdDivWord(addr(result.data[1]), addr(result.data[0]), a, mpd_Radix)
  result.len = if (result.data[1] == 0): 1 else: 2
  mpdSetdigits(result)

##  Internal function: Set a decimal from a triple, no error checking.

proc settriple*(result: ptr MpdT; sign: uint8T; a: MpdUintT; exp: MpdSsizeT) =
  mpdMinalloc(result)
  mpdSetFlags(result, sign)
  result.exp = exp
  mpdDivWord(addr(result.data[1]), addr(result.data[0]), a, mpd_Radix)
  result.len = if (result.data[1] == 0): 1 else: 2
  mpdSetdigits(result)

##  Set a special number from a triple

proc mpdSetspecial*(result: ptr MpdT; sign: uint8T; `type`: uint8T) =
  mpdMinalloc(result)
  result.flags = result.flags and not (mpd_Neg or mpd_Special)
  result.flags = result.flags or (sign or `type`)
  result.exp = result.digits = result.len = 0

##  Set result of NaN with an error status

proc mpdSeterror*(result: ptr MpdT; flags: uint32T; status: ptr uint32T) =
  mpdMinalloc(result)
  mpdSetQnan(result)
  mpdSetPositive(result)
  result.exp = result.digits = result.len = 0
  status[] = status[] or flags

##  quietly set a static decimal from an mpd_ssize_t

proc mpdQssetSsize*(result: ptr MpdT; a: MpdSsizeT; ctx: ptr MpdContextT;
                   status: ptr uint32T) =
  var u: MpdUintT
  var sign: uint8T = mpd_Pos
  if a < 0:
    if a == mpd_Ssize_Min:
      u = cast[MpdUintT](mpd_Ssize_Max) + (-(mpd_Ssize_Min + mpd_Ssize_Max))
    else:
      u = -a
    sign = mpd_Neg
  else:
    u = a
  ssettriple(result, sign, u, 0)
  mpdQfinalize(result, ctx, status)

##  quietly set a static decimal from an mpd_uint_t

proc mpdQssetUint*(result: ptr MpdT; a: MpdUintT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  ssettriple(result, mpd_Pos, a, 0)
  mpdQfinalize(result, ctx, status)

##  quietly set a static decimal from an int32_t

proc mpdQssetI32*(result: ptr MpdT; a: int32T; ctx: ptr MpdContextT; status: ptr uint32T) =
  mpdQssetSsize(result, a, ctx, status)

##  quietly set a static decimal from a uint32_t

proc mpdQssetU32*(result: ptr MpdT; a: uint32T; ctx: ptr MpdContextT; status: ptr uint32T) =
  mpdQssetUint(result, a, ctx, status)

when defined(CONFIG_64):
  ##  quietly set a static decimal from an int64_t
  proc mpdQssetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT;
                   status: ptr uint32T) =
    mpdQssetSsize(result, a, ctx, status)

  ##  quietly set a static decimal from a uint64_t
  proc mpdQssetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT;
                   status: ptr uint32T) =
    mpdQssetUint(result, a, ctx, status)

##  quietly set a decimal from an mpd_ssize_t

proc mpdQsetSsize*(result: ptr MpdT; a: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  mpdMinalloc(result)
  mpdQssetSsize(result, a, ctx, status)

##  quietly set a decimal from an mpd_uint_t

proc mpdQsetUint*(result: ptr MpdT; a: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  settriple(result, mpd_Pos, a, 0)
  mpdQfinalize(result, ctx, status)

##  quietly set a decimal from an int32_t

proc mpdQsetI32*(result: ptr MpdT; a: int32T; ctx: ptr MpdContextT; status: ptr uint32T) =
  mpdQsetSsize(result, a, ctx, status)

##  quietly set a decimal from a uint32_t

proc mpdQsetU32*(result: ptr MpdT; a: uint32T; ctx: ptr MpdContextT; status: ptr uint32T) =
  mpdQsetUint(result, a, ctx, status)

when defined(config_32) and not defined(legacy_Compiler):
  ##  set a decimal from a uint64_t
  proc c32setu64*(result: ptr MpdT; u: uint64T; sign: uint8T; status: ptr uint32T) =
    var w: array[3, MpdUintT]
    var q: uint64T
    var
      i: cint
      len: cint
    len = 0
    while true:
      q = u div mpd_Radix
      w[len] = (mpdUintT)(u - q * mpd_Radix)
      u = q
      inc(len)
      if not (u != 0):
        break
    if not mpdQresize(result, len, status):
      return
    i = 0
    while i < len:
      result.data[i] = w[i]
      inc(i)
    mpdSetFlags(result, sign)
    result.exp = 0
    result.len = len
    mpdSetdigits(result)

  proc c32QsetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    c32setu64(result, a, mpd_Pos, status)
    mpdQfinalize(result, ctx, status)

  ##  set a decimal from an int64_t
  proc c32QsetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT; status: ptr uint32T) =
    var u: uint64T
    var sign: uint8T = mpd_Pos
    if a < 0:
      if a == int64Min:
        u = cast[uint64T](int64Max) + (-(int64Min + int64Max))
      else:
        u = -a
      sign = mpd_Neg
    else:
      u = a
    c32setu64(result, u, sign, status)
    mpdQfinalize(result, ctx, status)

when not defined(LEGACY_COMPILER):
  ##  quietly set a decimal from an int64_t
  proc mpdQsetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT; status: ptr uint32T) =
    when defined(CONFIG_64):
      mpdQsetSsize(result, a, ctx, status)
    else:
      c32QsetI64(result, a, ctx, status)

  ##  quietly set a decimal from an int64_t, use a maxcontext for conversion
  proc mpdQsetI64Exact*(result: ptr MpdT; a: int64T; status: ptr uint32T) =
    var maxcontext: MpdContextT
    mpdMaxcontext(addr(maxcontext))
    when defined(CONFIG_64):
      mpdQsetSsize(result, a, addr(maxcontext), status)
    else:
      c32QsetI64(result, a, addr(maxcontext), status)
    if status[] and (mPD_Inexact or mPD_Rounded or mPD_Clamped):
      ##  we want exact results
      mpdSeterror(result, mPD_InvalidOperation, status)
    status[] = status[] and mPD_Errors

  ##  quietly set a decimal from a uint64_t
  proc mpdQsetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    when defined(CONFIG_64):
      mpdQsetUint(result, a, ctx, status)
    else:
      c32QsetU64(result, a, ctx, status)

  ##  quietly set a decimal from a uint64_t, use a maxcontext for conversion
  proc mpdQsetU64Exact*(result: ptr MpdT; a: uint64T; status: ptr uint32T) =
    var maxcontext: MpdContextT
    mpdMaxcontext(addr(maxcontext))
    when defined(CONFIG_64):
      mpdQsetUint(result, a, addr(maxcontext), status)
    else:
      c32QsetU64(result, a, addr(maxcontext), status)
    if status[] and (mPD_Inexact or mPD_Rounded or mPD_Clamped):
      ##  we want exact results
      mpdSeterror(result, mPD_InvalidOperation, status)
    status[] = status[] and mPD_Errors

##
##  Quietly get an mpd_uint_t from a decimal. Assumes
##  MPD_UINT_DIGITS == MPD_RDIGITS+1, which is true for
##  32 and 64 bit machines.
##
##  If the operation is impossible, MPD_Invalid_operation is set.
##

proc mpdQgetUint*(useSign: cint; a: ptr MpdT; status: ptr uint32T): MpdUintT =
  var tmp: MpdT
  var tmpData: array[2, MpdUintT]
  var
    lo: MpdUintT
    hi: MpdUintT
  if mpdIsspecial(a):
    status[] = status[] or mPD_InvalidOperation
    return mpd_Uint_Max
  if mpdIszero(a):
    return 0
  if useSign and mpdIsnegative(a):
    status[] = status[] or mPD_InvalidOperation
    return mpd_Uint_Max
  if a.digits + a.exp > mpd_Rdigits + 1:
    status[] = status[] or mPD_InvalidOperation
    return mpd_Uint_Max
  if a.exp < 0:
    if not mpdIsint(a):
      status[] = status[] or mPD_InvalidOperation
      return mpd_Uint_Max
    tmp.data = tmpData
    tmp.flags = mpd_Static or mpd_Static_Data
    tmp.alloc = 2
    mpdQsshiftr(addr(tmp), a, -a.exp)
    tmp.exp = 0
    a = addr(tmp)
  mpdGetMsdigits(addr(hi), addr(lo), a, mpd_Rdigits + 1)
  if hi:
    status[] = status[] or mPD_InvalidOperation
    return mpd_Uint_Max
  if a.exp > 0:
    mpdMulWords(addr(hi), addr(lo), lo, mpdPow10[a.exp])
    if hi:
      status[] = status[] or mPD_InvalidOperation
      return mpd_Uint_Max
  return lo

##
##  Sets Invalid_operation for:
##    - specials
##    - negative numbers (except negative zero)
##    - non-integers
##    - overflow
##

proc mpdQgetUint*(a: ptr MpdT; status: ptr uint32T): MpdUintT =
  return mpdQgetUint(1, a, status)

##  Same as above, but gets the absolute value, i.e. the sign is ignored.

proc mpdQabsUint*(a: ptr MpdT; status: ptr uint32T): MpdUintT =
  return mpdQgetUint(0, a, status)

##  quietly get an mpd_ssize_t from a decimal

proc mpdQgetSsize*(a: ptr MpdT; status: ptr uint32T): MpdSsizeT =
  var workstatus: uint32T = 0
  var u: MpdUintT
  var isneg: cint
  u = mpdQabsUint(a, addr(workstatus))
  if workstatus and mPD_InvalidOperation:
    status[] = status[] or workstatus
    return mpd_Ssize_Max
  isneg = mpdIsnegative(a)
  if u <= mpd_Ssize_Max:
    return if isneg: -(cast[MpdSsizeT](u)) else: cast[MpdSsizeT](u)
  elif isneg and u + (mpd_Ssize_Min + mpd_Ssize_Max) == mpd_Ssize_Max:
    return mpd_Ssize_Min
  status[] = status[] or mPD_InvalidOperation
  return mpd_Ssize_Max

when defined(config_32) and not defined(legacy_Compiler):
  ##
  ##  Quietly get a uint64_t from a decimal. If the operation is impossible,
  ##  MPD_Invalid_operation is set.
  ##
  proc c32QgetU64*(useSign: cint; a: ptr MpdT; status: ptr uint32T): uint64T =
    var tmpData: array[mpd_Minalloc_Max, MpdUintT]
    var tmp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 20, 3, mpd_Minalloc_Max, tmpData]
    var maxcontext: MpdContextT
    var ret: uint64T
    tmpData[0] = 709551615
    tmpData[1] = 446744073
    tmpData[2] = 18
    if mpdIsspecial(a):
      status[] = status[] or mPD_InvalidOperation
      return uint64Max
    if mpdIszero(a):
      return 0
    if useSign and mpdIsnegative(a):
      status[] = status[] or mPD_InvalidOperation
      return uint64Max
    if not mpdIsint(a):
      status[] = status[] or mPD_InvalidOperation
      return uint64Max
    if mpdCmpAbs(a, addr(tmp)) > 0:
      status[] = status[] or mPD_InvalidOperation
      return uint64Max
    mpdMaxcontext(addr(maxcontext))
    mpdQrescale(addr(tmp), a, 0, addr(maxcontext), addr(maxcontext.status))
    maxcontext.status = maxcontext.status and not mPD_Rounded
    if maxcontext.status != 0:
      status[] = status[] or (maxcontext.status or mPD_InvalidOperation)
      ##  GCOV_NOT_REACHED
      return uint64Max
      ##  GCOV_NOT_REACHED
    ret = 0
    case tmp.len
    of 3:
      inc(ret, cast[uint64T](tmpData[2] * 1000000000000000000'i64))
    of 2:
      inc(ret, cast[uint64T](tmpData[1] * 1000000000))
    of 1:
      inc(ret, tmpData[0])
    else:
      abort()
      ##  GCOV_NOT_REACHED
    return ret

  proc c32QgetI64*(a: ptr MpdT; status: ptr uint32T): int64T =
    var u: uint64T
    var isneg: cint
    u = c32QgetU64(0, a, status)
    if status[] and mPD_InvalidOperation:
      return int64Max
    isneg = mpdIsnegative(a)
    if u <= int64Max:
      return if isneg: -(cast[int64T](u)) else: cast[int64T](u)
    elif isneg and u + (int64Min + int64Max) == int64Max:
      return int64Min
    status[] = status[] or mPD_InvalidOperation
    return int64Max

when defined(CONFIG_64):
  ##  quietly get a uint64_t from a decimal
  proc mpdQgetU64*(a: ptr MpdT; status: ptr uint32T): uint64T =
    return mpdQgetUint(a, status)

  ##  quietly get an int64_t from a decimal
  proc mpdQgetI64*(a: ptr MpdT; status: ptr uint32T): int64T =
    return mpdQgetSsize(a, status)

  ##  quietly get a uint32_t from a decimal
  proc mpdQgetU32*(a: ptr MpdT; status: ptr uint32T): uint32T =
    var workstatus: uint32T = 0
    var x: uint64T = mpdQgetUint(a, addr(workstatus))
    if workstatus and mPD_InvalidOperation:
      status[] = status[] or workstatus
      return uint32Max
    if x > uint32Max:
      status[] = status[] or mPD_InvalidOperation
      return uint32Max
    return cast[uint32T](x)

  ##  quietly get an int32_t from a decimal
  proc mpdQgetI32*(a: ptr MpdT; status: ptr uint32T): int32T =
    var workstatus: uint32T = 0
    var x: int64T = mpdQgetSsize(a, addr(workstatus))
    if workstatus and mPD_InvalidOperation:
      status[] = status[] or workstatus
      return int32Max
    if x < int32Min or x > int32Max:
      status[] = status[] or mPD_InvalidOperation
      return int32Max
    return cast[int32T](x)

else:
  when not defined(LEGACY_COMPILER):
    ##  quietly get a uint64_t from a decimal
    proc mpdQgetU64*(a: ptr MpdT; status: ptr uint32T): uint64T =
      var workstatus: uint32T = 0
      var x: uint64T = c32QgetU64(1, a, addr(workstatus))
      status[] = status[] or workstatus
      return x

    ##  quietly get an int64_t from a decimal
    proc mpdQgetI64*(a: ptr MpdT; status: ptr uint32T): int64T =
      var workstatus: uint32T = 0
      var x: int64T = c32QgetI64(a, addr(workstatus))
      status[] = status[] or workstatus
      return x

  ##  quietly get a uint32_t from a decimal
  proc mpdQgetU32*(a: ptr MpdT; status: ptr uint32T): uint32T =
    return mpdQgetUint(a, status)

  ##  quietly get an int32_t from a decimal
  proc mpdQgetI32*(a: ptr MpdT; status: ptr uint32T): int32T =
    return mpdQgetSsize(a, status)

## ****************************************************************************
##          Filtering input of functions, finalizing output of functions
## ****************************************************************************
##
##  Check if the operand is NaN, copy to result and return 1 if this is
##  the case. Copying can fail since NaNs are allowed to have a payload that
##  does not fit in MPD_MINALLOC.
##

proc mpdQcheckNan*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T): cint =
  if mpdIsnan(a):
    status[] = status[] or if mpdIssnan(a): mPD_InvalidOperation else: 0
    mpdQcopy(result, a, status)
    mpdSetQnan(result)
    mpdFixNan(result, ctx)
    return 1
  return 0

##
##  Check if either operand is NaN, copy to result and return 1 if this
##  is the case. Copying can fail since NaNs are allowed to have a payload
##  that does not fit in MPD_MINALLOC.
##

proc mpdQcheckNans*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                   status: ptr uint32T): cint =
  if (a.flags or b.flags) and (mpd_Nan or mpd_Snan):
    var choice: ptr MpdT = b
    if mpdIssnan(a):
      choice = a
      status[] = status[] or mPD_InvalidOperation
    elif mpdIssnan(b):
      status[] = status[] or mPD_InvalidOperation
    elif mpdIsqnan(a):
      choice = a
    mpdQcopy(result, choice, status)
    mpdSetQnan(result)
    mpdFixNan(result, ctx)
    return 1
  return 0

##
##  Check if one of the operands is NaN, copy to result and return 1 if this
##  is the case. Copying can fail since NaNs are allowed to have a payload
##  that does not fit in MPD_MINALLOC.
##

proc mpdQcheck3nans*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; c: ptr MpdT;
                    ctx: ptr MpdContextT; status: ptr uint32T): cint =
  if (a.flags or b.flags or c.flags) and (mpd_Nan or mpd_Snan):
    var choice: ptr MpdT = c
    if mpdIssnan(a):
      choice = a
      status[] = status[] or mPD_InvalidOperation
    elif mpdIssnan(b):
      choice = b
      status[] = status[] or mPD_InvalidOperation
    elif mpdIssnan(c):
      status[] = status[] or mPD_InvalidOperation
    elif mpdIsqnan(a):
      choice = a
    elif mpdIsqnan(b):
      choice = b
    mpdQcopy(result, choice, status)
    mpdSetQnan(result)
    mpdFixNan(result, ctx)
    return 1
  return 0

##  Check if rounding digit 'rnd' leads to an increment.

proc mpdRndIncr*(dec: ptr MpdT; rnd: MpdUintT; ctx: ptr MpdContextT): cint =
  var ld: cint
  case ctx.round
  of mpd_Round_Down, mpd_Round_Trunc:
    return 0
  of mpd_Round_Half_Up:
    return rnd >= 5
  of mpd_Round_Half_Even:
    return (rnd > 5) or ((rnd == 5) and mpdIsoddcoeff(dec))
  of mpd_Round_Ceiling:
    return not (rnd == 0 or mpdIsnegative(dec))
  of mpd_Round_Floor:
    return not (rnd == 0 or mpdIspositive(dec))
  of mpd_Round_Half_Down:
    return rnd > 5
  of mpd_Round_Up:
    return not (rnd == 0)
  of mpd_Round_05up:
    ld = cast[cint](mpdLsd(dec.data[0]))
    return not (rnd == 0) and (ld == 0 or ld == 5)
  else:
    nil
  ##  Without a valid context, further results will be undefined.
  return 0
  ##  GCOV_NOT_REACHED

##
##  Apply rounding to a decimal that has been right-shifted into a full
##  precision decimal. If an increment leads to an overflow of the precision,
##  adjust the coefficient and the exponent and check the new exponent for
##  overflow.
##

proc mpdApplyRound*(dec: ptr MpdT; rnd: MpdUintT; ctx: ptr MpdContextT;
                   status: ptr uint32T) =
  if mpdRndIncr(dec, rnd, ctx):
    ##  We have a number with exactly ctx->prec digits. The increment
    ##  can only lead to an overflow if the decimal is all nines. In
    ##  that case, the result is a power of ten with prec+1 digits.
    ##
    ##  If the precision is a multiple of MPD_RDIGITS, this situation is
    ##  detected by _mpd_baseincr returning a carry.
    ##  If the precision is not a multiple of MPD_RDIGITS, we have to
    ##  check if the result has one digit too many.
    ##
    var carry: MpdUintT = mpdBaseincr(dec.data, dec.len)
    if carry:
      dec.data[dec.len - 1] = mpdPow10[mpd_Rdigits - 1]
      inc(dec.exp, 1)
      mpdCheckExp(dec, ctx, status)
      return
    mpdSetdigits(dec)
    if dec.digits > ctx.prec:
      mpdQshiftrInplace(dec, 1)
      inc(dec.exp, 1)
      dec.digits = ctx.prec
      mpdCheckExp(dec, ctx, status)

##
##  Apply rounding to a decimal. Allow overflow of the precision.
##

proc mpdApplyRoundExcess*(dec: ptr MpdT; rnd: MpdUintT; ctx: ptr MpdContextT;
                         status: ptr uint32T) =
  if mpdRndIncr(dec, rnd, ctx):
    var carry: MpdUintT = mpdBaseincr(dec.data, dec.len)
    if carry:
      if not mpdQresize(dec, dec.len + 1, status):
        return
      dec.data[dec.len] = 1
      inc(dec.len, 1)
    mpdSetdigits(dec)

##
##  Apply rounding to a decimal that has been right-shifted into a decimal
##  with full precision or less. Return failure if an increment would
##  overflow the precision.
##

proc mpdApplyRoundFit*(dec: ptr MpdT; rnd: MpdUintT; ctx: ptr MpdContextT;
                      status: ptr uint32T): cint =
  if mpdRndIncr(dec, rnd, ctx):
    var carry: MpdUintT = mpdBaseincr(dec.data, dec.len)
    if carry:
      if not mpdQresize(dec, dec.len + 1, status):
        return 0
      dec.data[dec.len] = 1
      inc(dec.len, 1)
    mpdSetdigits(dec)
    if dec.digits > ctx.prec:
      mpdSeterror(dec, mPD_InvalidOperation, status)
      return 0
  return 1

##  Check a normal number for overflow, underflow, clamping. If the operand
##    is modified, it will be zero, special or (sub)normal with a coefficient
##    that fits into the current context precision.

proc mpdCheckExp*(dec: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var
    adjexp: MpdSsizeT
    etiny: MpdSsizeT
    shift: MpdSsizeT
  var rnd: cint
  adjexp = mpdAdjexp(dec)
  if adjexp > ctx.emax:
    if mpdIszerocoeff(dec):
      dec.exp = ctx.emax
      if ctx.clamp:
        dec(dec.exp, (ctx.prec - 1))
      mpdZerocoeff(dec)
      status[] = status[] or mPD_Clamped
      return
    case ctx.round
    of mpd_Round_Half_Up, mpd_Round_Half_Even, mpd_Round_Half_Down, mpd_Round_Up,
      mpd_Round_Trunc:
      mpdSetspecial(dec, mpdSign(dec), mpd_Inf)
    of mpd_Round_Down, mpd_Round_05up:
      mpdQmaxcoeff(dec, ctx, status)
      dec.exp = ctx.emax - ctx.prec + 1
    of mpd_Round_Ceiling:
      if mpdIsnegative(dec):
        mpdQmaxcoeff(dec, ctx, status)
        dec.exp = ctx.emax - ctx.prec + 1
      else:
        mpdSetspecial(dec, mpd_Pos, mpd_Inf)
    of mpd_Round_Floor:
      if mpdIspositive(dec):
        mpdQmaxcoeff(dec, ctx, status)
        dec.exp = ctx.emax - ctx.prec + 1
      else:
        mpdSetspecial(dec, mpd_Neg, mpd_Inf)
    else:                     ##  debug
      abort()
      ##  GCOV_NOT_REACHED
    status[] = status[] or (mPD_Overflow or mPD_Inexact or mPD_Rounded)
  elif ctx.clamp and dec.exp > mpdEtop(ctx):
    ##  At this point adjexp=exp+digits-1 <= emax and exp > etop=emax-prec+1:
    ##    (1) shift = exp -emax+prec-1 > 0
    ##    (2) digits+shift = exp+digits-1 - emax + prec <= prec
    shift = dec.exp - mpdEtop(ctx)
    if not mpdQshiftl(dec, dec, shift, status):
      return
    dec(dec.exp, shift)
    status[] = status[] or mPD_Clamped
    if not mpdIszerocoeff(dec) and adjexp < ctx.emin:
      ##  Underflow is impossible, since exp < etiny=emin-prec+1
      ##  and exp > etop=emax-prec+1 would imply emax < emin.
      status[] = status[] or mPD_Subnormal
  elif adjexp < ctx.emin:
    etiny = mpdEtiny(ctx)
    if mpdIszerocoeff(dec):
      if dec.exp < etiny:
        dec.exp = etiny
        mpdZerocoeff(dec)
        status[] = status[] or mPD_Clamped
      return
    status[] = status[] or mPD_Subnormal
    if dec.exp < etiny:
      ##  At this point adjexp=exp+digits-1 < emin and exp < etiny=emin-prec+1:
      ##    (1) shift = emin-prec+1 - exp > 0
      ##    (2) digits-shift = exp+digits-1 - emin + prec < prec
      shift = etiny - dec.exp
      rnd = cast[cint](mpdQshiftrInplace(dec, shift))
      dec.exp = etiny
      ##  We always have a spare digit in case of an increment.
      mpdApplyRoundExcess(dec, rnd, ctx, status)
      status[] = status[] or mPD_Rounded
      if rnd:
        status[] = status[] or (mPD_Inexact or mPD_Underflow)
        if mpdIszerocoeff(dec):
          mpdZerocoeff(dec)
          status[] = status[] or mPD_Clamped

##  Transcendental functions do not always set Underflow reliably,
##  since they only use as much precision as is necessary for correct
##  rounding. If a result like 1.0000000000e-101 is finalized, there
##  is no rounding digit that would trigger Underflow. But we can
##  assume Inexact, so a short check suffices.

proc mpdCheckUnderflow*(dec: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  if mpdAdjexp(dec) < ctx.emin and not mpdIszero(dec) and dec.exp < mpdEtiny(ctx):
    status[] = status[] or mPD_Underflow

##  Check if a normal number must be rounded after the exponent has been checked.

proc mpdCheckRound*(dec: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var rnd: MpdUintT
  var shift: MpdSsizeT
  ##  must handle specials: _mpd_check_exp() can produce infinities or NaNs
  if mpdIsspecial(dec):
    return
  if dec.digits > ctx.prec:
    shift = dec.digits - ctx.prec
    rnd = mpdQshiftrInplace(dec, shift)
    inc(dec.exp, shift)
    mpdApplyRound(dec, rnd, ctx, status)
    status[] = status[] or mPD_Rounded
    if rnd:
      status[] = status[] or mPD_Inexact

##  Finalize all operations.

proc mpdQfinalize*(result: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  if mpdIsspecial(result):
    if mpdIsnan(result):
      mpdFixNan(result, ctx)
    return
  mpdCheckExp(result, ctx, status)
  mpdCheckRound(result, ctx, status)

## ****************************************************************************
##                                  Copying
## ****************************************************************************
##  Internal function: Copy a decimal, share data with src: USE WITH CARE!

proc mpdCopyShared*(dest: ptr MpdT; src: ptr MpdT) =
  dest.flags = src.flags
  dest.exp = src.exp
  dest.digits = src.digits
  dest.len = src.len
  dest.alloc = src.alloc
  dest.data = src.data
  mpdSetSharedData(dest)

##
##  Copy a decimal. In case of an error, status is set to MPD_Malloc_error.
##

proc mpdQcopy*(result: ptr MpdT; a: ptr MpdT; status: ptr uint32T): cint =
  if result == a:
    return 1
  if not mpdQresize(result, a.len, status):
    return 0
  mpdCopyFlags(result, a)
  result.exp = a.exp
  result.digits = a.digits
  result.len = a.len
  memcpy(result.data, a.data, a.len * (sizeof(result.data[])))
  return 1

##  Same as mpd_qcopy, but do not set the result to NaN on failure.

proc mpdQcopyCxx*(result: ptr MpdT; a: ptr MpdT): cint =
  if result == a:
    return 1
  if not mpdQresizeCxx(result, a.len):
    return 0
  mpdCopyFlags(result, a)
  result.exp = a.exp
  result.digits = a.digits
  result.len = a.len
  memcpy(result.data, a.data, a.len * (sizeof(result.data[])))
  return 1

##
##  Copy to a decimal with a static buffer. The caller has to make sure that
##  the buffer is big enough. Cannot fail.
##

proc mpdQcopyStatic*(result: ptr MpdT; a: ptr MpdT) =
  if result == a:
    return
  memcpy(result.data, a.data, a.len * (sizeof(result.data[])))
  mpdCopyFlags(result, a)
  result.exp = a.exp
  result.digits = a.digits
  result.len = a.len

##
##  Return a newly allocated copy of the operand. In case of an error,
##  status is set to MPD_Malloc_error and the return value is NULL.
##

proc mpdQncopy*(a: ptr MpdT): ptr MpdT =
  var result: ptr MpdT
  if (result = mpdQnewSize(a.len)) == nil:
    return nil
  memcpy(result.data, a.data, a.len * (sizeof(result.data[])))
  mpdCopyFlags(result, a)
  result.exp = a.exp
  result.digits = a.digits
  result.len = a.len
  return result

##
##  Copy a decimal and set the sign to positive. In case of an error, the
##  status is set to MPD_Malloc_error.
##

proc mpdQcopyAbs*(result: ptr MpdT; a: ptr MpdT; status: ptr uint32T): cint =
  if not mpdQcopy(result, a, status):
    return 0
  mpdSetPositive(result)
  return 1

##
##  Copy a decimal and negate the sign. In case of an error, the
##  status is set to MPD_Malloc_error.
##

proc mpdQcopyNegate*(result: ptr MpdT; a: ptr MpdT; status: ptr uint32T): cint =
  if not mpdQcopy(result, a, status):
    return 0
  mpdNegate(result)
  return 1

##
##  Copy a decimal, setting the sign of the first operand to the sign of the
##  second operand. In case of an error, the status is set to MPD_Malloc_error.
##

proc mpdQcopySign*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; status: ptr uint32T): cint =
  var signB: uint8T = mpdSign(b)
  ##  result may equal b!
  if not mpdQcopy(result, a, status):
    return 0
  mpdSetSign(result, signB)
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

template cmp_Equal_Or_Return*(a, b: untyped): void =
  if a != b:
    if a < b:
      return -1
    return 1

##
##  Compare the data of big and small. This function does the equivalent
##  of first shifting small to the left and then comparing the data of
##  big and small, except that no allocation for the left shift is needed.
##

proc mpdBasecmp*(big: ptr MpdUintT; small: ptr MpdUintT; n: MpdSizeT; m: MpdSizeT;
                shift: MpdSizeT): cint =
  when defined(gnuc) and not defined(intel_Compiler) and not defined(clang):
    ##  spurious uninitialized warnings
    var
      l: MpdUintT = l
      lprev: MpdUintT = lprev
      h: MpdUintT = h
  else:
    var
      l: MpdUintT
      lprev: MpdUintT
      h: MpdUintT
  var
    q: MpdUintT
    r: MpdUintT
  var
    ph: MpdUintT
    x: MpdUintT
  assert(m > 0 and n >= m and shift > 0)
  mpdDivWord(addr(q), addr(r), cast[MpdUintT](shift), mpd_Rdigits)
  if r != 0:
    ph = mpdPow10[r]
    dec(m)
    dec(n)
    mpdDivmodPow10(addr(h), addr(lprev), small[dec(m)], mpd_Rdigits - r)
    if h != 0:
      cmp_Equal_Or_Return(big[n], h)
      dec(n)
    while m != mpd_Size_Max:
      mpdDivmodPow10(addr(h), addr(l), small[m], mpd_Rdigits - r)
      x = ph * lprev + h
      cmp_Equal_Or_Return(big[n], x)
      lprev = l
      dec(m)
      dec(n)
    x = ph * lprev
    cmp_Equal_Or_Return(big[q], x)
  else:
    while dec(m) != mpd_Size_Max:
      cmp_Equal_Or_Return(big[m + q], small[m])
  return not mpdIsallzero(big, q)

##  Compare two decimals with the same adjusted exponent.

proc mpdCmpSameAdjexp*(a: ptr MpdT; b: ptr MpdT): cint =
  var
    shift: MpdSsizeT
    i: MpdSsizeT
  if a.exp != b.exp:
    ##  Cannot wrap: a->exp + a->digits = b->exp + b->digits, so
    ##  a->exp - b->exp = b->digits - a->digits.
    shift = a.exp - b.exp
    if shift > 0:
      return -(1 * mpdBasecmp(b.data, a.data, b.len, a.len, shift))
    else:
      return mpdBasecmp(a.data, b.data, a.len, b.len, -shift)
  i = a.len - 1
  while i >= 0:
    cmp_Equal_Or_Return(a.data[i], b.data[i])
    dec(i)
  return 0

##  Compare two numerical values.

proc mpdCmp*(a: ptr MpdT; b: ptr MpdT): cint =
  var
    adjexpA: MpdSsizeT
    adjexpB: MpdSsizeT
  ##  equal pointers
  if a == b:
    return 0
  if mpdIsinfinite(a):
    if mpdIsinfinite(b):
      return mpdIsnegative(b) - mpdIsnegative(a)
    return mpdArithSign(a)
  if mpdIsinfinite(b):
    return -mpdArithSign(b)
  if mpdIszerocoeff(a):
    if mpdIszerocoeff(b):
      return 0
    return -mpdArithSign(b)
  if mpdIszerocoeff(b):
    return mpdArithSign(a)
  if mpdSign(a) != mpdSign(b):
    return mpdSign(b) - mpdSign(a)
  adjexpA = mpdAdjexp(a)
  adjexpB = mpdAdjexp(b)
  if adjexpA != adjexpB:
    if adjexpA < adjexpB:
      return -(1 * mpdArithSign(a))
    return mpdArithSign(a)
  return mpdCmpSameAdjexp(a, b) * mpdArithSign(a)

##  Compare the absolutes of two numerical values.

proc mpdCmpAbs*(a: ptr MpdT; b: ptr MpdT): cint =
  var
    adjexpA: MpdSsizeT
    adjexpB: MpdSsizeT
  ##  equal pointers
  if a == b:
    return 0
  if mpdIsinfinite(a):
    if mpdIsinfinite(b):
      return 0
    return 1
  if mpdIsinfinite(b):
    return -1
  if mpdIszerocoeff(a):
    if mpdIszerocoeff(b):
      return 0
    return -1
  if mpdIszerocoeff(b):
    return 1
  adjexpA = mpdAdjexp(a)
  adjexpB = mpdAdjexp(b)
  if adjexpA != adjexpB:
    if adjexpA < adjexpB:
      return -1
    return 1
  return mpdCmpSameAdjexp(a, b)

##  Compare two values and return an integer result.

proc mpdQcmp*(a: ptr MpdT; b: ptr MpdT; status: ptr uint32T): cint =
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdIsnan(a) or mpdIsnan(b):
      status[] = status[] or mPD_InvalidOperation
      return int_Max
  return mpdCmp(a, b)

##
##  Compare a and b, convert the usual integer result to a decimal and
##  store it in 'result'. For convenience, the integer result of the comparison
##  is returned. Comparisons involving NaNs return NaN/INT_MAX.
##

proc mpdQcompare*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                 status: ptr uint32T): cint =
  var c: cint
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      return int_Max
  c = mpdCmp(a, b)
  settriple(result, (c < 0), (c != 0), 0)
  return c

##  Same as mpd_compare(), but signal for all NaNs, i.e. also for quiet NaNs.

proc mpdQcompareSignal*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                       status: ptr uint32T): cint =
  var c: cint
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      status[] = status[] or mPD_InvalidOperation
      return int_Max
  c = mpdCmp(a, b)
  settriple(result, (c < 0), (c != 0), 0)
  return c

##  Compare the operands using a total order.

proc mpdCmpTotal*(a: ptr MpdT; b: ptr MpdT): cint =
  var
    aa: MpdT
    bb: MpdT
  var
    nanA: cint
    nanB: cint
  var c: cint
  if mpdSign(a) != mpdSign(b):
    return mpdSign(b) - mpdSign(a)
  if mpdIsnan(a):
    c = 1
    if mpdIsnan(b):
      nanA = if (mpdIsqnan(a)): 1 else: 0
      nanB = if (mpdIsqnan(b)): 1 else: 0
      if nanB == nanA:
        if a.len > 0 and b.len > 0:
          mpdCopyShared(addr(aa), a)
          mpdCopyShared(addr(bb), b)
          aa.exp = bb.exp = 0
          ##  compare payload
          c = mpdCmpAbs(addr(aa), addr(bb))
        else:
          c = (a.len > 0) - (b.len > 0)
      else:
        c = nanA - nanB
  elif mpdIsnan(b):
    c = -1
  else:
    c = mpdCmpAbs(a, b)
    if c == 0 and a.exp != b.exp:
      c = if (a.exp < b.exp): -1 else: 1
  return c * mpdArithSign(a)

##
##  Compare a and b according to a total order, convert the usual integer result
##  to a decimal and store it in 'result'. For convenience, the integer result
##  of the comparison is returned.
##

proc mpdCompareTotal*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT): cint =
  var c: cint
  c = mpdCmpTotal(a, b)
  settriple(result, (c < 0), (c != 0), 0)
  return c

##  Compare the magnitude of the operands using a total order.

proc mpdCmpTotalMag*(a: ptr MpdT; b: ptr MpdT): cint =
  var
    aa: MpdT
    bb: MpdT
  mpdCopyShared(addr(aa), a)
  mpdCopyShared(addr(bb), b)
  mpdSetPositive(addr(aa))
  mpdSetPositive(addr(bb))
  return mpdCmpTotal(addr(aa), addr(bb))

##
##  Compare the magnitude of a and b according to a total order, convert the
##  the usual integer result to a decimal and store it in 'result'.
##  For convenience, the integer result of the comparison is returned.
##

proc mpdCompareTotalMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT): cint =
  var c: cint
  c = mpdCmpTotalMag(a, b)
  settriple(result, (c < 0), (c != 0), 0)
  return c

##  Determine an ordering for operands that are numerically equal.

proc mpdCmpNumequal*(a: ptr MpdT; b: ptr MpdT): cint =
  var
    signA: cint
    signB: cint
  var c: cint
  signA = mpdSign(a)
  signB = mpdSign(b)
  if signA != signB:
    c = signB - signA
  else:
    c = if (a.exp < b.exp): -1 else: 1
    c = c * mpdArithSign(a)
  return c

## ****************************************************************************
##                          Shifting the coefficient
## ****************************************************************************
##
##  Shift the coefficient of the operand to the left, no check for specials.
##  Both operands may be the same pointer. If the result length has to be
##  increased, mpd_qresize() might fail with MPD_Malloc_error.
##

proc mpdQshiftl*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; status: ptr uint32T): cint =
  var size: MpdSsizeT
  assert(not mpdIsspecial(a))
  assert(n >= 0)
  if mpdIszerocoeff(a) or n == 0:
    return mpdQcopy(result, a, status)
  size = mpdDigitsToSize(a.digits + n)
  if not mpdQresize(result, size, status):
    return 0
    ##  result is NaN
  mpdBaseshiftl(result.data, a.data, size, a.len, n)
  mpdCopyFlags(result, a)
  result.exp = a.exp
  result.digits = a.digits + n
  result.len = size
  return 1

##  Determine the rounding indicator if all digits of the coefficient are shifted
##  out of the picture.

proc mpdGetRnd*(data: ptr MpdUintT; len: MpdSsizeT; useMsd: cint): MpdUintT =
  var
    rnd: MpdUintT = 0
    rest: MpdUintT = 0
    word: MpdUintT
  word = data[len - 1]
  ##  special treatment for the most significant digit if shift == digits
  if useMsd:
    mpdDivmodPow10(addr(rnd), addr(rest), word, mpdWordDigits(word) - 1)
    if len > 1 and rest == 0:
      rest = not mpdIsallzero(data, len - 1)
  else:
    rest = not mpdIsallzero(data, len)
  return if (rnd == 0 or rnd == 5): rnd + not not rest else: rnd

##
##  Same as mpd_qshiftr(), but 'result' is an mpd_t with a static coefficient.
##  It is the caller's responsibility to ensure that the coefficient is big
##  enough. The function cannot fail.
##

proc mpdQsshiftr*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT): MpdUintT =
  var rnd: MpdUintT
  var size: MpdSsizeT
  assert(not mpdIsspecial(a))
  assert(n >= 0)
  if mpdIszerocoeff(a) or n == 0:
    mpdQcopyStatic(result, a)
    return 0
  if n >= a.digits:
    rnd = mpdGetRnd(a.data, a.len, (n == a.digits))
    mpdZerocoeff(result)
  else:
    result.digits = a.digits - n
    size = mpdDigitsToSize(result.digits)
    rnd = mpdBaseshiftr(result.data, a.data, a.len, n)
    result.len = size
  mpdCopyFlags(result, a)
  result.exp = a.exp
  return rnd

##
##  Inplace shift of the coefficient to the right, no check for specials.
##  Returns the rounding indicator for mpd_rnd_incr().
##  The function cannot fail.
##

proc mpdQshiftrInplace*(result: ptr MpdT; n: MpdSsizeT): MpdUintT =
  var dummy: uint32T
  var rnd: MpdUintT
  var size: MpdSsizeT
  assert(not mpdIsspecial(result))
  assert(n >= 0)
  if mpdIszerocoeff(result) or n == 0:
    return 0
  if n >= result.digits:
    rnd = mpdGetRnd(result.data, result.len, (n == result.digits))
    mpdZerocoeff(result)
  else:
    rnd = mpdBaseshiftr(result.data, result.data, result.len, n)
    dec(result.digits, n)
    size = mpdDigitsToSize(result.digits)
    ##  reducing the size cannot fail
    mpdQresize(result, size, addr(dummy))
    result.len = size
  return rnd

##
##  Shift the coefficient of the operand to the right, no check for specials.
##  Both operands may be the same pointer. Returns the rounding indicator to
##  be used by mpd_rnd_incr(). If the result length has to be increased,
##  mpd_qcopy() or mpd_qresize() might fail with MPD_Malloc_error. In those
##  cases, MPD_UINT_MAX is returned.
##

proc mpdQshiftr*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; status: ptr uint32T): MpdUintT =
  var rnd: MpdUintT
  var size: MpdSsizeT
  assert(not mpdIsspecial(a))
  assert(n >= 0)
  if mpdIszerocoeff(a) or n == 0:
    if not mpdQcopy(result, a, status):
      return mpd_Uint_Max
    return 0
  if n >= a.digits:
    rnd = mpdGetRnd(a.data, a.len, (n == a.digits))
    mpdZerocoeff(result)
  else:
    result.digits = a.digits - n
    size = mpdDigitsToSize(result.digits)
    if result == a:
      rnd = mpdBaseshiftr(result.data, a.data, a.len, n)
      ##  reducing the size cannot fail
      mpdQresize(result, size, status)
    else:
      if not mpdQresize(result, size, status):
        return mpd_Uint_Max
      rnd = mpdBaseshiftr(result.data, a.data, a.len, n)
    result.len = size
  mpdCopyFlags(result, a)
  result.exp = a.exp
  return rnd

## ****************************************************************************
##                          Miscellaneous operations
## ****************************************************************************
##  Logical And

proc mpdQand*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var
    big: ptr MpdT = a
    small: ptr MpdT = b
  var
    x: MpdUintT
    y: MpdUintT
    z: MpdUintT
    xbit: MpdUintT
    ybit: MpdUintT
  var
    k: cint
    mswdigits: cint
  var i: MpdSsizeT
  if mpdIsspecial(a) or mpdIsspecial(b) or mpdIsnegative(a) or mpdIsnegative(b) or
      a.exp != 0 or b.exp != 0:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if b.digits > a.digits:
    big = b
    small = a
  if not mpdQresize(result, big.len, status):
    return
  i = 0
  while i < small.len - 1:
    x = small.data[i]
    y = big.data[i]
    z = 0
    k = 0
    while k < mpd_Rdigits:
      xbit = x mod 10
      x = x / 10
      ybit = y mod 10
      y = y / 10
      if xbit > 1 or ybit > 1:
        break invalidOperation
      inc(z, if (xbit and ybit): mpdPow10[k] else: 0)
      inc(k)
    result.data[i] = z
    inc(i)
  ##  most significant word of small
  x = small.data[i]
  y = big.data[i]
  z = 0
  mswdigits = mpdWordDigits(x)
  k = 0
  while k < mswdigits:
    xbit = x mod 10
    x = x / 10
    ybit = y mod 10
    y = y / 10
    if xbit > 1 or ybit > 1:
      break invalidOperation
    inc(z, if (xbit and ybit): mpdPow10[k] else: 0)
    inc(k)
  result.data[inc(i)] = z
  ##  scan the rest of y for digits > 1
  while k < mpd_Rdigits:
    ybit = y mod 10
    y = y / 10
    if ybit > 1:
      break invalidOperation
    inc(k)
  ##  scan the rest of big for digits > 1
  while i < big.len:
    y = big.data[i]
    k = 0
    while k < mpd_Rdigits:
      ybit = y mod 10
      y = y / 10
      if ybit > 1:
        break invalidOperation
      inc(k)
    inc(i)
  mpdClearFlags(result)
  result.exp = 0
  result.len = mpdRealSize(result.data, small.len)
  mpdQresize(result, result.len, status)
  mpdSetdigits(result)
  mpdCap(result, ctx)
  return
  mpdSeterror(result, mPD_InvalidOperation, status)

##  Class of an operand. Returns a pointer to the constant name.

proc mpdClass*(a: ptr MpdT; ctx: ptr MpdContextT): cstring =
  if mpdIsnan(a):
    if mpdIsqnan(a):
      return "NaN"
    else:
      return "sNaN"
  elif mpdIspositive(a):
    if mpdIsinfinite(a):
      return "+Infinity"
    elif mpdIszero(a):
      return "+Zero"
    elif mpdIsnormal(a, ctx):
      return "+Normal"
    else:
      return "+Subnormal"
  else:
    if mpdIsinfinite(a):
      return "-Infinity"
    elif mpdIszero(a):
      return "-Zero"
    elif mpdIsnormal(a, ctx):
      return "-Normal"
    else:
      return "-Subnormal"

##  Logical Xor

proc mpdQinvert*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var
    x: MpdUintT
    z: MpdUintT
    xbit: MpdUintT
  var
    i: MpdSsizeT
    digits: MpdSsizeT
    len: MpdSsizeT
  var
    q: MpdSsizeT
    r: MpdSsizeT
  var k: cint
  if mpdIsspecial(a) or mpdIsnegative(a) or a.exp != 0:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  digits = if (a.digits < ctx.prec): ctx.prec else: a.digits
  mpdIdivWord(addr(q), addr(r), digits, mpd_Rdigits)
  len = if (r == 0): q else: q + 1
  if not mpdQresize(result, len, status):
    return
  i = 0
  while i < len:
    x = if (i < a.len): a.data[i] else: 0
    z = 0
    k = 0
    while k < mpd_Rdigits:
      xbit = x mod 10
      x = x / 10
      if xbit > 1:
        break invalidOperation
      inc(z, if not xbit: mpdPow10[k] else: 0)
      inc(k)
    result.data[i] = z
    inc(i)
  mpdClearFlags(result)
  result.exp = 0
  result.len = mpdRealSize(result.data, len)
  mpdQresize(result, result.len, status)
  mpdSetdigits(result)
  mpdCap(result, ctx)
  return
  mpdSeterror(result, mPD_InvalidOperation, status)

##  Exponent of the magnitude of the most significant digit of the operand.

proc mpdQlogb*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    mpdSetspecial(result, mpd_Pos, mpd_Inf)
  elif mpdIszerocoeff(a):
    mpdSetspecial(result, mpd_Neg, mpd_Inf)
    status[] = status[] or mPD_DivisionByZero
  else:
    mpdQsetSsize(result, mpdAdjexp(a), ctx, status)

##  Logical Or

proc mpdQor*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
            status: ptr uint32T) =
  var
    big: ptr MpdT = a
    small: ptr MpdT = b
  var
    x: MpdUintT
    y: MpdUintT
    z: MpdUintT
    xbit: MpdUintT
    ybit: MpdUintT
  var
    k: cint
    mswdigits: cint
  var i: MpdSsizeT
  if mpdIsspecial(a) or mpdIsspecial(b) or mpdIsnegative(a) or mpdIsnegative(b) or
      a.exp != 0 or b.exp != 0:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if b.digits > a.digits:
    big = b
    small = a
  if not mpdQresize(result, big.len, status):
    return
  i = 0
  while i < small.len - 1:
    x = small.data[i]
    y = big.data[i]
    z = 0
    k = 0
    while k < mpd_Rdigits:
      xbit = x mod 10
      x = x / 10
      ybit = y mod 10
      y = y / 10
      if xbit > 1 or ybit > 1:
        break invalidOperation
      inc(z, if (xbit or ybit): mpdPow10[k] else: 0)
      inc(k)
    result.data[i] = z
    inc(i)
  ##  most significant word of small
  x = small.data[i]
  y = big.data[i]
  z = 0
  mswdigits = mpdWordDigits(x)
  k = 0
  while k < mswdigits:
    xbit = x mod 10
    x = x / 10
    ybit = y mod 10
    y = y / 10
    if xbit > 1 or ybit > 1:
      break invalidOperation
    inc(z, if (xbit or ybit): mpdPow10[k] else: 0)
    inc(k)
  ##  scan for digits > 1 and copy the rest of y
  while k < mpd_Rdigits:
    ybit = y mod 10
    y = y / 10
    if ybit > 1:
      break invalidOperation
    inc(z, ybit * mpdPow10[k])
    inc(k)
  result.data[inc(i)] = z
  ##  scan for digits > 1 and copy the rest of big
  while i < big.len:
    y = big.data[i]
    k = 0
    while k < mpd_Rdigits:
      ybit = y mod 10
      y = y / 10
      if ybit > 1:
        break invalidOperation
      inc(k)
    result.data[i] = big.data[i]
    inc(i)
  mpdClearFlags(result)
  result.exp = 0
  result.len = mpdRealSize(result.data, big.len)
  mpdQresize(result, result.len, status)
  mpdSetdigits(result)
  mpdCap(result, ctx)
  return
  mpdSeterror(result, mPD_InvalidOperation, status)

##
##  Rotate the coefficient of 'a' by 'b' digits. 'b' must be an integer with
##  exponent 0.
##

proc mpdQrotate*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  var workstatus: uint32T = 0
  var tmpData: array[mpd_Minalloc_Max, MpdUintT]
  var tmp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tmpData]
  var bigData: array[mpd_Minalloc_Max, MpdUintT]
  var big: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bigData]
  var smallData: array[mpd_Minalloc_Max, MpdUintT]
  var small: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, smallData]
  var
    n: MpdSsizeT
    lshift: MpdSsizeT
    rshift: MpdSsizeT
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      return
  if b.exp != 0 or mpdIsinfinite(b):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  n = mpdQgetSsize(b, addr(workstatus))
  if workstatus and mPD_InvalidOperation:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if n > ctx.prec or n < -ctx.prec:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if mpdIsinfinite(a):
    mpdQcopy(result, a, status)
    return
  if n >= 0:
    lshift = n
    rshift = ctx.prec - n
  else:
    lshift = ctx.prec + n
    rshift = -n
  if a.digits > ctx.prec:
    if not mpdQcopy(addr(tmp), a, status):
      mpdSeterror(result, mPD_MallocError, status)
      break finish
    mpdCap(addr(tmp), ctx)
    a = addr(tmp)
  if not mpdQshiftl(addr(big), a, lshift, status):
    mpdSeterror(result, mPD_MallocError, status)
    break finish
  mpdCap(addr(big), ctx)
  if mpdQshiftr(addr(small), a, rshift, status) == mpd_Uint_Max:
    mpdSeterror(result, mPD_MallocError, status)
    break finish
  mpdQadd(result, addr(big), addr(small), ctx, status)
  mpdDel(addr(tmp))
  mpdDel(addr(big))
  mpdDel(addr(small))

##
##  b must be an integer with exponent 0 and in the range +-2*(emax + prec).
##  XXX: In my opinion +-(2*emax + prec) would be more sensible.
##  The result is a with the value of b added to its exponent.
##

proc mpdQscaleb*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  var workstatus: uint32T = 0
  var
    n: MpdUintT
    maxjump: MpdUintT
  when not defined(LEGACY_COMPILER):
    var exp: int64T
  else:
    var x: MpdUintT
    var
      xSign: cint
      nSign: cint
    var exp: MpdSsizeT
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      return
  if b.exp != 0 or mpdIsinfinite(b):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  n = mpdQabsUint(b, addr(workstatus))
  ##  the spec demands this
  maxjump = 2 * (mpdUintT)(ctx.emax + ctx.prec)
  if n > maxjump or workstatus and mPD_InvalidOperation:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if mpdIsinfinite(a):
    mpdQcopy(result, a, status)
    return
  when not defined(LEGACY_COMPILER):
    exp = a.exp + cast[int64T](n * mpdArithSign(b))
    exp = if (exp > mpd_Exp_Inf): mpd_Exp_Inf else: exp
    exp = if (exp < mpd_Exp_Clamp): mpd_Exp_Clamp else: exp
  else:
    x = if (a.exp < 0): -a.exp else: a.exp
    xSign = if (a.exp < 0): 1 else: 0
    nSign = if mpdIsnegative(b): 1 else: 0
    if xSign == nSign:
      x = x + n
      if x < n:
        x = mpd_Uint_Max
    else:
      xSign = if (x >= n): xSign else: nSign
      x = if (x >= n): x - n else: n - x
    if not xSign and x > mpd_Exp_Inf:
      x = mpd_Exp_Inf
    if xSign and x > -mpd_Exp_Clamp:
      x = -mpd_Exp_Clamp
    exp = if xSign: -(cast[MpdSsizeT](x)) else: cast[MpdSsizeT](x)
  mpdQcopy(result, a, status)
  result.exp = cast[MpdSsizeT](exp)
  mpdQfinalize(result, ctx, status)

##
##  Shift the coefficient by n digits, positive n is a left shift. In the case
##  of a left shift, the result is decapitated to fit the context precision. If
##  you don't want that, use mpd_shiftl().
##

proc mpdQshiftn*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    mpdQcopy(result, a, status)
    return
  if n >= 0 and n <= ctx.prec:
    mpdQshiftl(result, a, n, status)
    mpdCap(result, ctx)
  elif n < 0 and n >= -ctx.prec:
    if not mpdQcopy(result, a, status):
      return
    mpdCap(result, ctx)
    mpdQshiftrInplace(result, -n)
  else:
    mpdSeterror(result, mPD_InvalidOperation, status)

##
##  Same as mpd_shiftn(), but the shift is specified by the decimal b, which
##  must be an integer with a zero exponent. Infinities remain infinities.
##

proc mpdQshift*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
               status: ptr uint32T) =
  var workstatus: uint32T = 0
  var n: MpdSsizeT
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      return
  if b.exp != 0 or mpdIsinfinite(b):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  n = mpdQgetSsize(b, addr(workstatus))
  if workstatus and mPD_InvalidOperation:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if n > ctx.prec or n < -ctx.prec:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if mpdIsinfinite(a):
    mpdQcopy(result, a, status)
    return
  if n >= 0:
    mpdQshiftl(result, a, n, status)
    mpdCap(result, ctx)
  else:
    if not mpdQcopy(result, a, status):
      return
    mpdCap(result, ctx)
    mpdQshiftrInplace(result, -n)

##  Logical Xor

proc mpdQxor*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var
    big: ptr MpdT = a
    small: ptr MpdT = b
  var
    x: MpdUintT
    y: MpdUintT
    z: MpdUintT
    xbit: MpdUintT
    ybit: MpdUintT
  var
    k: cint
    mswdigits: cint
  var i: MpdSsizeT
  if mpdIsspecial(a) or mpdIsspecial(b) or mpdIsnegative(a) or mpdIsnegative(b) or
      a.exp != 0 or b.exp != 0:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if b.digits > a.digits:
    big = b
    small = a
  if not mpdQresize(result, big.len, status):
    return
  i = 0
  while i < small.len - 1:
    x = small.data[i]
    y = big.data[i]
    z = 0
    k = 0
    while k < mpd_Rdigits:
      xbit = x mod 10
      x = x / 10
      ybit = y mod 10
      y = y / 10
      if xbit > 1 or ybit > 1:
        break invalidOperation
      inc(z, if (xbit xor ybit): mpdPow10[k] else: 0)
      inc(k)
    result.data[i] = z
    inc(i)
  ##  most significant word of small
  x = small.data[i]
  y = big.data[i]
  z = 0
  mswdigits = mpdWordDigits(x)
  k = 0
  while k < mswdigits:
    xbit = x mod 10
    x = x / 10
    ybit = y mod 10
    y = y / 10
    if xbit > 1 or ybit > 1:
      break invalidOperation
    inc(z, if (xbit xor ybit): mpdPow10[k] else: 0)
    inc(k)
  ##  scan for digits > 1 and copy the rest of y
  while k < mpd_Rdigits:
    ybit = y mod 10
    y = y / 10
    if ybit > 1:
      break invalidOperation
    inc(z, ybit * mpdPow10[k])
    inc(k)
  result.data[inc(i)] = z
  ##  scan for digits > 1 and copy the rest of big
  while i < big.len:
    y = big.data[i]
    k = 0
    while k < mpd_Rdigits:
      ybit = y mod 10
      y = y / 10
      if ybit > 1:
        break invalidOperation
      inc(k)
    result.data[i] = big.data[i]
    inc(i)
  mpdClearFlags(result)
  result.exp = 0
  result.len = mpdRealSize(result.data, big.len)
  mpdQresize(result, result.len, status)
  mpdSetdigits(result)
  mpdCap(result, ctx)
  return
  mpdSeterror(result, mPD_InvalidOperation, status)

## ****************************************************************************
##                          Arithmetic operations
## ****************************************************************************
##
##  The absolute value of a. If a is negative, the result is the same
##  as the result of the minus operation. Otherwise, the result is the
##  result of the plus operation.
##

proc mpdQabs*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
  if mpdIsnegative(a):
    mpdQminus(result, a, ctx, status)
  else:
    mpdQplus(result, a, ctx, status)

proc mpdPtrswap*(a: ptr ptr MpdT; b: ptr ptr MpdT) =
  var t: ptr MpdT = a[]
  a[] = b[]
  b[] = t

##  Add or subtract infinities.

proc mpdQaddsubInf*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; signB: uint8T;
                   status: ptr uint32T) =
  if mpdIsinfinite(a):
    if mpdSign(a) != signB and mpdIsinfinite(b):
      mpdSeterror(result, mPD_InvalidOperation, status)
    else:
      mpdSetspecial(result, mpdSign(a), mpd_Inf)
    return
  assert(mpdIsinfinite(b))
  mpdSetspecial(result, signB, mpd_Inf)

##  Add or subtract non-special numbers.

proc mpdQaddsub*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; signB: uint8T;
                ctx: ptr MpdContextT; status: ptr uint32T) =
  var
    big: ptr MpdT
    small: ptr MpdT
  var bigAlignedData: array[mpd_Minalloc_Max, MpdUintT]
  var bigAligned: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max,
                      bigAlignedData]
  var tinyData: array[1, MpdUintT] = [1]
  var tiny: MpdT = [0 or mpd_Static or mpd_Const_Data, 0, 1, 1, 1, tinyData]
  var carry: MpdUintT
  var
    newsize: MpdSsizeT
    shift: MpdSsizeT
  var
    exp: MpdSsizeT
    i: MpdSsizeT
  var swap: cint = 0
  ##  compare exponents
  big = a
  small = b
  if big.exp != small.exp:
    if small.exp > big.exp:
      mpdPtrswap(addr(big), addr(small))
      inc(swap)
    if not mpdIszerocoeff(big):
      exp = big.exp - 1
      inc(exp, if (big.digits > ctx.prec): 0 else: big.digits - ctx.prec - 1)
      if mpdAdjexp(small) < exp:
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
        mpdCopyFlags(addr(tiny), small)
        tiny.exp = exp
        tiny.digits = 1
        tiny.len = 1
        tiny.data[0] = if mpdIszerocoeff(small): 0 else: 1
        small = addr(tiny)
      shift = big.exp - small.exp
      if not mpdQshiftl(addr(bigAligned), big, shift, status):
        mpdSeterror(result, mPD_MallocError, status)
        break finish
      big = addr(bigAligned)
  result.exp = small.exp
  ##  compare length of coefficients
  if big.len < small.len:
    mpdPtrswap(addr(big), addr(small))
    inc(swap)
  newsize = big.len
  if not mpdQresize(result, newsize, status):
    break finish
  if mpdSign(a) == signB:
    carry = mpdBaseadd(result.data, big.data, small.data, big.len, small.len)
    if carry:
      newsize = big.len + 1
      if not mpdQresize(result, newsize, status):
        break finish
      result.data[newsize - 1] = carry
    result.len = newsize
    mpdSetFlags(result, signB)
  else:
    if big.len == small.len:
      i = big.len - 1
      while i >= 0:
        if big.data[i] != small.data[i]:
          if big.data[i] < small.data[i]:
            mpdPtrswap(addr(big), addr(small))
            inc(swap)
          break
        dec(i)
    mpdBasesub(result.data, big.data, small.data, big.len, small.len)
    newsize = mpdRealSize(result.data, big.len)
    ##  resize to smaller cannot fail
    cast[nil](mpdQresize(result, newsize, status))
    result.len = newsize
    signB = if (swap and 1): signB else: mpdSign(a)
    mpdSetFlags(result, signB)
    if mpdIszerocoeff(result):
      mpdSetPositive(result)
      if ctx.round == mpd_Round_Floor:
        mpdSetNegative(result)
  mpdSetdigits(result)
  mpdDel(addr(bigAligned))

##  Add a and b. No specials, no finalizing.

proc mpdQadd*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  mpdQaddsub(result, a, b, mpdSign(b), ctx, status)

##  Subtract b from a. No specials, no finalizing.

proc mpdQsub*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  mpdQaddsub(result, a, b, not mpdSign(b), ctx, status)

##  Add a and b.

proc mpdQadd*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      return
    mpdQaddsubInf(result, a, b, mpdSign(b), status)
    return
  mpdQaddsub(result, a, b, mpdSign(b), ctx, status)
  mpdQfinalize(result, ctx, status)

##  Add a and b. Set NaN/Invalid_operation if the result is inexact.

proc mpdQaddExact*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var workstatus: uint32T = 0
  mpdQadd(result, a, b, ctx, addr(workstatus))
  status[] = status[] or workstatus
  if workstatus and (mPD_Inexact or mPD_Rounded or mPD_Clamped):
    mpdSeterror(result, mPD_InvalidOperation, status)

##  Subtract b from a.

proc mpdQsub*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      return
    mpdQaddsubInf(result, a, b, not mpdSign(b), status)
    return
  mpdQaddsub(result, a, b, not mpdSign(b), ctx, status)
  mpdQfinalize(result, ctx, status)

##  Subtract b from a. Set NaN/Invalid_operation if the result is inexact.

proc mpdQsubExact*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var workstatus: uint32T = 0
  mpdQsub(result, a, b, ctx, addr(workstatus))
  status[] = status[] or workstatus
  if workstatus and (mPD_Inexact or mPD_Rounded or mPD_Clamped):
    mpdSeterror(result, mPD_InvalidOperation, status)

##  Add decimal and mpd_ssize_t.

proc mpdQaddSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var maxcontext: MpdContextT
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  mpdMaxcontext(addr(maxcontext))
  mpdQssetSsize(addr(bb), b, addr(maxcontext), status)
  mpdQadd(result, a, addr(bb), ctx, status)
  mpdDel(addr(bb))

##  Add decimal and mpd_uint_t.

proc mpdQaddUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  var maxcontext: MpdContextT
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  mpdMaxcontext(addr(maxcontext))
  mpdQssetUint(addr(bb), b, addr(maxcontext), status)
  mpdQadd(result, a, addr(bb), ctx, status)
  mpdDel(addr(bb))

##  Subtract mpd_ssize_t from decimal.

proc mpdQsubSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var maxcontext: MpdContextT
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  mpdMaxcontext(addr(maxcontext))
  mpdQssetSsize(addr(bb), b, addr(maxcontext), status)
  mpdQsub(result, a, addr(bb), ctx, status)
  mpdDel(addr(bb))

##  Subtract mpd_uint_t from decimal.

proc mpdQsubUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  var maxcontext: MpdContextT
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  mpdMaxcontext(addr(maxcontext))
  mpdQssetUint(addr(bb), b, addr(maxcontext), status)
  mpdQsub(result, a, addr(bb), ctx, status)
  mpdDel(addr(bb))

##  Add decimal and int32_t.

proc mpdQaddI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  mpdQaddSsize(result, a, b, ctx, status)

##  Add decimal and uint32_t.

proc mpdQaddU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  mpdQaddUint(result, a, b, ctx, status)

when defined(CONFIG_64):
  ##  Add decimal and int64_t.
  proc mpdQaddI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    mpdQaddSsize(result, a, b, ctx, status)

  ##  Add decimal and uint64_t.
  proc mpdQaddU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    mpdQaddUint(result, a, b, ctx, status)

elif not defined(legacy_Compiler):
  ##  Add decimal and int64_t.
  proc mpdQaddI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    var maxcontext: MpdContextT
    var bbData: array[mpd_Minalloc_Max, MpdUintT]
    var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
    mpdMaxcontext(addr(maxcontext))
    mpdQsetI64(addr(bb), b, addr(maxcontext), status)
    mpdQadd(result, a, addr(bb), ctx, status)
    mpdDel(addr(bb))

  ##  Add decimal and uint64_t.
  proc mpdQaddU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    var maxcontext: MpdContextT
    var bbData: array[mpd_Minalloc_Max, MpdUintT]
    var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
    mpdMaxcontext(addr(maxcontext))
    mpdQsetU64(addr(bb), b, addr(maxcontext), status)
    mpdQadd(result, a, addr(bb), ctx, status)
    mpdDel(addr(bb))

##  Subtract int32_t from decimal.

proc mpdQsubI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  mpdQsubSsize(result, a, b, ctx, status)

##  Subtract uint32_t from decimal.

proc mpdQsubU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  mpdQsubUint(result, a, b, ctx, status)

when defined(CONFIG_64):
  ##  Subtract int64_t from decimal.
  proc mpdQsubI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    mpdQsubSsize(result, a, b, ctx, status)

  ##  Subtract uint64_t from decimal.
  proc mpdQsubU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    mpdQsubUint(result, a, b, ctx, status)

elif not defined(legacy_Compiler):
  ##  Subtract int64_t from decimal.
  proc mpdQsubI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    var maxcontext: MpdContextT
    var bbData: array[mpd_Minalloc_Max, MpdUintT]
    var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
    mpdMaxcontext(addr(maxcontext))
    mpdQsetI64(addr(bb), b, addr(maxcontext), status)
    mpdQsub(result, a, addr(bb), ctx, status)
    mpdDel(addr(bb))

  ##  Subtract uint64_t from decimal.
  proc mpdQsubU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    var maxcontext: MpdContextT
    var bbData: array[mpd_Minalloc_Max, MpdUintT]
    var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
    mpdMaxcontext(addr(maxcontext))
    mpdQsetU64(addr(bb), b, addr(maxcontext), status)
    mpdQsub(result, a, addr(bb), ctx, status)
    mpdDel(addr(bb))

##  Divide infinities.

proc mpdQdivInf*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  if mpdIsinfinite(a):
    if mpdIsinfinite(b):
      mpdSeterror(result, mPD_InvalidOperation, status)
      return
    mpdSetspecial(result, mpdSign(a) xor mpdSign(b), mpd_Inf)
    return
  assert(mpdIsinfinite(b))
  settriple(result, mpdSign(a) xor mpdSign(b), 0, mpdEtiny(ctx))
  status[] = status[] or mPD_Clamped

const
  NO_IDEAL_EXP* = 0
  SET_IDEAL_EXP* = 1

##  Divide a by b.

proc mpdQdiv*(action: cint; q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var alignedData: array[mpd_Minalloc_Max, MpdUintT]
  var aligned: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max,
                   alignedData]
  var ld: MpdUintT
  var
    shift: MpdSsizeT
    exp: MpdSsizeT
    tz: MpdSsizeT
  var newsize: MpdSsizeT
  var idealExp: MpdSsizeT
  var rem: MpdUintT
  var signA: uint8T = mpdSign(a)
  var signB: uint8T = mpdSign(b)
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(q, a, b, ctx, status):
      return
    mpdQdivInf(q, a, b, ctx, status)
    return
  if mpdIszerocoeff(b):
    if mpdIszerocoeff(a):
      mpdSeterror(q, mPD_DivisionUndefined, status)
    else:
      mpdSetspecial(q, signA xor signB, mpd_Inf)
      status[] = status[] or mPD_DivisionByZero
    return
  if mpdIszerocoeff(a):
    exp = a.exp - b.exp
    settriple(q, signA xor signB, 0, exp)
    mpdQfinalize(q, ctx, status)
    return
  shift = (b.digits - a.digits) + ctx.prec + 1
  idealExp = a.exp - b.exp
  exp = idealExp - shift
  if shift > 0:
    if not mpdQshiftl(addr(aligned), a, shift, status):
      mpdSeterror(q, mPD_MallocError, status)
      break finish
    a = addr(aligned)
  elif shift < 0:
    shift = -shift
    if not mpdQshiftl(addr(aligned), b, shift, status):
      mpdSeterror(q, mPD_MallocError, status)
      break finish
    b = addr(aligned)
  newsize = a.len - b.len + 1
  if (q != b and q != a) or (q == b and newsize > b.len):
    if not mpdQresize(q, newsize, status):
      mpdSeterror(q, mPD_MallocError, status)
      break finish
  if b.len == 1:
    rem = mpdShortdiv(q.data, a.data, a.len, b.data[0])
  elif b.len <= mpd_Newtondiv_Cutoff:
    var ret: cint = mpdBasedivmod(q.data, nil, a.data, b.data, a.len, b.len)
    if ret < 0:
      mpdSeterror(q, mPD_MallocError, status)
      break finish
    rem = ret
  else:
    var rData: array[mpd_Minalloc_Max, MpdUintT]
    var r: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, rData]
    mpdBaseNdivmod(q, addr(r), a, b, status)
    if mpdIsspecial(q) or mpdIsspecial(addr(r)):
      mpdSetspecial(q, mpd_Pos, mpd_Nan)
      mpdDel(addr(r))
      break finish
    rem = not mpdIszerocoeff(addr(r))
    mpdDel(addr(r))
    newsize = q.len
  newsize = mpdRealSize(q.data, newsize)
  ##  resize to smaller cannot fail
  mpdQresize(q, newsize, status)
  mpdSetFlags(q, signA xor signB)
  q.len = newsize
  mpdSetdigits(q)
  shift = idealExp - exp
  if rem:
    ld = mpdLsd(q.data[0])
    if ld == 0 or ld == 5:
      inc(q.data[0], 1)
  elif action == set_Ideal_Exp and shift > 0:
    tz = mpdTrailZeros(q)
    shift = if (tz > shift): shift else: tz
    mpdQshiftrInplace(q, shift)
    inc(exp, shift)
  q.exp = exp
  mpdDel(addr(aligned))
  mpdQfinalize(q, ctx, status)

##  Divide a by b.

proc mpdQdiv*(q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var aaData: array[mpd_Minalloc_Max, MpdUintT]
  var aa: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, aaData]
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  var xstatus: uint32T = 0
  if q == a:
    if not mpdQcopy(addr(aa), a, status):
      mpdSeterror(q, mPD_MallocError, status)
      break `out`
    a = addr(aa)
  if q == b:
    if not mpdQcopy(addr(bb), b, status):
      mpdSeterror(q, mPD_MallocError, status)
      break `out`
    b = addr(bb)
  mpdQdiv(set_Ideal_Exp, q, a, b, ctx, addr(xstatus))
  if xstatus and (mPD_MallocError or mPD_DivisionImpossible):
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
    var workctx: MpdContextT = ctx[]
    var ystatus: uint32T = 0
    workctx.prec = a.digits + b.digits * 4
    if workctx.prec >= ctx.prec:
      status[] = status[] or (xstatus and mPD_Errors)
      break `out`
      ##  No point in retrying, keep the original error.
    mpdQdiv(set_Ideal_Exp, q, a, b, addr(workctx), addr(ystatus))
    if ystatus != 0:
      ystatus = status[] or ((ystatus or xstatus) and mPD_Errors)
      mpdSeterror(q, ystatus, status)
  else:
    status[] = status[] or xstatus
  mpdDel(addr(aa))
  mpdDel(addr(bb))

##  Internal function.

proc mpdQdivmod*(q: ptr MpdT; r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  var alignedData: array[mpd_Minalloc_Max, MpdUintT]
  var aligned: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max,
                   alignedData]
  var
    qsize: MpdSsizeT
    rsize: MpdSsizeT
  var
    idealExp: MpdSsizeT
    expdiff: MpdSsizeT
    shift: MpdSsizeT
  var signA: uint8T = mpdSign(a)
  var signAb: uint8T = mpdSign(a) xor mpdSign(b)
  idealExp = if (a.exp > b.exp): b.exp else: a.exp
  if mpdIszerocoeff(a):
    if not mpdQcopy(r, a, status):
      break nanresult
      ##  GCOV_NOT_REACHED
    r.exp = idealExp
    settriple(q, signAb, 0, 0)
    return
  expdiff = mpdAdjexp(a) - mpdAdjexp(b)
  if expdiff < 0:
    if a.exp > b.exp:
      ##  positive and less than b->digits - a->digits
      shift = a.exp - b.exp
      if not mpdQshiftl(r, a, shift, status):
        break nanresult
      r.exp = idealExp
    else:
      if not mpdQcopy(r, a, status):
        break nanresult
    settriple(q, signAb, 0, 0)
    return
  if expdiff > ctx.prec:
    status[] = status[] or mPD_DivisionImpossible
    break nanresult
  if a.exp != b.exp:
    shift = a.exp - b.exp
    if shift > 0:
      ##  by (3), after the shift a->digits <= prec + b->digits
      if not mpdQshiftl(addr(aligned), a, shift, status):
        break nanresult
      a = addr(aligned)
    else:
      shift = -shift
      ##  by (2), after the shift b->digits <= a->digits
      if not mpdQshiftl(addr(aligned), b, shift, status):
        break nanresult
      b = addr(aligned)
  qsize = a.len - b.len + 1
  if not (q == a and qsize < a.len) and not (q == b and qsize < b.len):
    if not mpdQresize(q, qsize, status):
      break nanresult
  rsize = b.len
  if not (r == a and rsize < a.len):
    if not mpdQresize(r, rsize, status):
      break nanresult
  if b.len == 1:
    assert(b.data[0] != 0)
    ##  annotation for scan-build
    if a.len == 1:
      mpdDivWord(addr(q.data[0]), addr(r.data[0]), a.data[0], b.data[0])
    else:
      r.data[0] = mpdShortdiv(q.data, a.data, a.len, b.data[0])
  elif b.len <= mpd_Newtondiv_Cutoff:
    var ret: cint
    ret = mpdBasedivmod(q.data, r.data, a.data, b.data, a.len, b.len)
    if ret == -1:
      status[] = status[] or mPD_MallocError
      break nanresult
  else:
    mpdBaseNdivmod(q, r, a, b, status)
    if mpdIsspecial(q) or mpdIsspecial(r):
      break nanresult
    qsize = q.len
    rsize = r.len
  qsize = mpdRealSize(q.data, qsize)
  ##  resize to smaller cannot fail
  mpdQresize(q, qsize, status)
  q.len = qsize
  mpdSetdigits(q)
  mpdSetFlags(q, signAb)
  q.exp = 0
  if q.digits > ctx.prec:
    status[] = status[] or mPD_DivisionImpossible
    break nanresult
  rsize = mpdRealSize(r.data, rsize)
  ##  resize to smaller cannot fail
  mpdQresize(r, rsize, status)
  r.len = rsize
  mpdSetdigits(r)
  mpdSetFlags(r, signA)
  r.exp = idealExp
  mpdDel(addr(aligned))
  return
  mpdSetspecial(q, mpd_Pos, mpd_Nan)
  mpdSetspecial(r, mpd_Pos, mpd_Nan)
  break `out`

##  Integer division with remainder.

proc mpdQdivmod*(q: ptr MpdT; r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  var sign: uint8T = mpdSign(a) xor mpdSign(b)
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(q, a, b, ctx, status):
      mpdQcopy(r, q, status)
      return
    if mpdIsinfinite(a):
      if mpdIsinfinite(b):
        mpdSetspecial(q, mpd_Pos, mpd_Nan)
      else:
        mpdSetspecial(q, sign, mpd_Inf)
      mpdSetspecial(r, mpd_Pos, mpd_Nan)
      status[] = status[] or mPD_InvalidOperation
      return
    if mpdIsinfinite(b):
      if not mpdQcopy(r, a, status):
        mpdSeterror(q, mPD_MallocError, status)
        return
      mpdQfinalize(r, ctx, status)
      settriple(q, sign, 0, 0)
      return
    abort()
    ##  GCOV_NOT_REACHED
  if mpdIszerocoeff(b):
    if mpdIszerocoeff(a):
      mpdSetspecial(q, mpd_Pos, mpd_Nan)
      mpdSetspecial(r, mpd_Pos, mpd_Nan)
      status[] = status[] or mPD_DivisionUndefined
    else:
      mpdSetspecial(q, sign, mpd_Inf)
      mpdSetspecial(r, mpd_Pos, mpd_Nan)
      status[] = status[] or (mPD_DivisionByZero or mPD_InvalidOperation)
    return
  mpdQdivmod(q, r, a, b, ctx, status)
  mpdQfinalize(q, ctx, status)
  mpdQfinalize(r, ctx, status)

proc mpdQdivint*(q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  var rData: array[mpd_Minalloc_Max, MpdUintT]
  var r: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, rData]
  var sign: uint8T = mpdSign(a) xor mpdSign(b)
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(q, a, b, ctx, status):
      return
    if mpdIsinfinite(a) and mpdIsinfinite(b):
      mpdSeterror(q, mPD_InvalidOperation, status)
      return
    if mpdIsinfinite(a):
      mpdSetspecial(q, sign, mpd_Inf)
      return
    if mpdIsinfinite(b):
      settriple(q, sign, 0, 0)
      return
    abort()
    ##  GCOV_NOT_REACHED
  if mpdIszerocoeff(b):
    if mpdIszerocoeff(a):
      mpdSeterror(q, mPD_DivisionUndefined, status)
    else:
      mpdSetspecial(q, sign, mpd_Inf)
      status[] = status[] or mPD_DivisionByZero
    return
  mpdQdivmod(q, addr(r), a, b, ctx, status)
  mpdDel(addr(r))
  mpdQfinalize(q, ctx, status)

##  Divide decimal by mpd_ssize_t.

proc mpdQdivSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var maxcontext: MpdContextT
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  mpdMaxcontext(addr(maxcontext))
  mpdQssetSsize(addr(bb), b, addr(maxcontext), status)
  mpdQdiv(result, a, addr(bb), ctx, status)
  mpdDel(addr(bb))

##  Divide decimal by mpd_uint_t.

proc mpdQdivUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  var maxcontext: MpdContextT
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  mpdMaxcontext(addr(maxcontext))
  mpdQssetUint(addr(bb), b, addr(maxcontext), status)
  mpdQdiv(result, a, addr(bb), ctx, status)
  mpdDel(addr(bb))

##  Divide decimal by int32_t.

proc mpdQdivI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  mpdQdivSsize(result, a, b, ctx, status)

##  Divide decimal by uint32_t.

proc mpdQdivU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  mpdQdivUint(result, a, b, ctx, status)

when defined(CONFIG_64):
  ##  Divide decimal by int64_t.
  proc mpdQdivI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    mpdQdivSsize(result, a, b, ctx, status)

  ##  Divide decimal by uint64_t.
  proc mpdQdivU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    mpdQdivUint(result, a, b, ctx, status)

elif not defined(legacy_Compiler):
  ##  Divide decimal by int64_t.
  proc mpdQdivI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    var maxcontext: MpdContextT
    var bbData: array[mpd_Minalloc_Max, MpdUintT]
    var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
    mpdMaxcontext(addr(maxcontext))
    mpdQsetI64(addr(bb), b, addr(maxcontext), status)
    mpdQdiv(result, a, addr(bb), ctx, status)
    mpdDel(addr(bb))

  ##  Divide decimal by uint64_t.
  proc mpdQdivU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    var maxcontext: MpdContextT
    var bbData: array[mpd_Minalloc_Max, MpdUintT]
    var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
    mpdMaxcontext(addr(maxcontext))
    mpdQsetU64(addr(bb), b, addr(maxcontext), status)
    mpdQdiv(result, a, addr(bb), ctx, status)
    mpdDel(addr(bb))

##  Pad the result with trailing zeros if it has fewer digits than prec.

proc mpdZeropad*(result: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  if not mpdIsspecial(result) and not mpdIszero(result) and
      result.digits < ctx.prec:
    var shift: MpdSsizeT = ctx.prec - result.digits
    mpdQshiftl(result, result, shift, status)
    dec(result.exp, shift)

##  Check if the result is guaranteed to be one.

proc mpdQexpCheckOne*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                     status: ptr uint32T): cint =
  var limData: array[1, MpdUintT] = [9]
  var lim: MpdT = [0 or mpd_Static or mpd_Const_Data, -(ctx.prec + 1), 1, 1, 1, limData]
  var aa: MpdT = [(a.flags and not mpd_Dataflags) or mpd_Static or mpd_Shared_Data, a.exp,
              a.digits, a.len, a.alloc, a.data]
  mpdSetPositive(addr(aa))
  ##  abs(a) <= 9 * 10**(-prec-1)
  if mpdCmp(addr(aa), addr(lim)) <= 0:
    settriple(result, 0, 1, 0)
    status[] = status[] or (mPD_Rounded or mPD_Inexact)
    return 1
  return 0

##
##  Get the number of iterations for the Horner scheme in _mpd_qexp().
##

proc mpdGetExpIterations*(r: ptr MpdT; p: MpdSsizeT): MpdSsizeT =
  var log10pbyr: MpdSsizeT
  ##  lower bound for log10(p / abs(r))
  var n: MpdSsizeT
  assert(p >= 10)
  assert(not mpdIszero(r))
  assert(-p < mpdAdjexp(r) and mpdAdjexp(r) <= -1)
  when defined(CONFIG_64):
    if p > (mpdSsizeT)(1 shl 52):
      return mpd_Ssize_Max
  ##
  ##  Lower bound for log10(p / abs(r)): adjexp(p) - (adjexp(r) + 1)
  ##  At this point (for CONFIG_64, CONFIG_32 is not problematic):
  ##     1) 10 <= p <= 2**52
  ##     2) -p < adjexp(r) <= -1
  ##     3) 1 <= log10pbyr <= 2**52 + 14
  ##
  log10pbyr = (mpdWordDigits(p) - 1) - (mpdAdjexp(r) + 1)
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
  n = cast[MpdSsizeT](ceil((1.43503 * cast[cdouble](p) - 1.182) div
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

proc mpdQexp*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var workctx: MpdContextT
  var tmpData: array[mpd_Minalloc_Max, MpdUintT]
  var tmp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tmpData]
  var sumData: array[mpd_Minalloc_Max, MpdUintT]
  var sum: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, sumData]
  var wordData: array[1, MpdUintT] = [1]
  var word: MpdT = [0 or mpd_Static or mpd_Const_Data, 0, 1, 1, 1, wordData]
  var
    j: MpdSsizeT
    n: MpdSsizeT
    t: MpdSsizeT
  assert(not mpdIsspecial(a))
  if mpdIszerocoeff(a):
    settriple(result, mpd_Pos, 1, 0)
    return
  when defined(config_64):
    const
      MPD_EXP_MAX_T = 19
  elif defined(config_32):
    const
      MPD_EXP_MAX_T = 10
  t = a.digits + a.exp
  t = if (t > 0): t else: 0
  if t > mpd_Exp_Max_T:
    if mpdIspositive(a):
      mpdSetspecial(result, mpd_Pos, mpd_Inf)
      status[] = status[] or (mPD_Overflow or mPD_Inexact or mPD_Rounded)
    else:
      settriple(result, mpd_Pos, 0, mpdEtiny(ctx))
      status[] = status[] or
          (mPD_Inexact or mPD_Rounded or mPD_Subnormal or mPD_Underflow or
          mPD_Clamped)
    return
  if mpdQexpCheckOne(result, a, ctx, status):
    return
  mpdMaxcontext(addr(workctx))
  workctx.prec = ctx.prec + t + 2
  workctx.prec = if (workctx.prec < 10): 10 else: workctx.prec
  workctx.round = mpd_Round_Half_Even
  if not mpdQcopy(result, a, status):
    return
  dec(result.exp, t)
  ##
  ##  At this point:
  ##     1) 9 * 10**(-prec-1) < abs(a)
  ##     2) 9 * 10**(-prec-t-1) < abs(r)
  ##     3) log10(9) - prec - t - 1 < log10(abs(r)) < adjexp(abs(r)) + 1
  ##     4) - prec - t - 2 < adjexp(abs(r)) <= -1
  ##
  n = mpdGetExpIterations(result, workctx.prec)
  if n == mpd_Ssize_Max:
    mpdSeterror(result, mPD_InvalidOperation, status)
    ##  GCOV_UNLIKELY
    return
    ##  GCOV_UNLIKELY
  settriple(addr(sum), mpd_Pos, 1, 0)
  j = n - 1
  while j >= 1:
    word.data[0] = j
    mpdSetdigits(addr(word))
    mpdQdiv(addr(tmp), result, addr(word), addr(workctx), addr(workctx.status))
    mpdQfma(addr(sum), addr(sum), addr(tmp), addr(one), addr(workctx),
            addr(workctx.status))
    dec(j)
  when defined(CONFIG_64):
    mpdQpowUint(result, addr(sum), mpdPow10[t], mpd_Pos, addr(workctx), status)
  else:
    if t <= mpd_Max_Pow10:
      mpdQpowUint(result, addr(sum), mpdPow10[t], mpd_Pos, addr(workctx), status)
    else:
      dec(t, mpd_Max_Pow10)
      mpdQpowUint(addr(tmp), addr(sum), mpdPow10[mpd_Max_Pow10], mpd_Pos,
                  addr(workctx), status)
      mpdQpowUint(result, addr(tmp), mpdPow10[t], mpd_Pos, addr(workctx), status)
  mpdDel(addr(tmp))
  mpdDel(addr(sum))
  status[] = status[] or (workctx.status and mPD_Errors)
  status[] = status[] or (mPD_Inexact or mPD_Rounded)

##  exp(a)

proc mpdQexp*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var workctx: MpdContextT
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    if mpdIsnegative(a):
      settriple(result, mpd_Pos, 0, 0)
    else:
      mpdSetspecial(result, mpd_Pos, mpd_Inf)
    return
  if mpdIszerocoeff(a):
    settriple(result, mpd_Pos, 1, 0)
    return
  workctx = ctx[]
  workctx.round = mpd_Round_Half_Even
  if ctx.allcr:
    var t1Data: array[mpd_Minalloc_Max, MpdUintT]
    var t1: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, t1Data]
    var t2Data: array[mpd_Minalloc_Max, MpdUintT]
    var t2: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, t2Data]
    var ulpData: array[mpd_Minalloc_Max, MpdUintT]
    var ulp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, ulpData]
    var aaData: array[mpd_Minalloc_Max, MpdUintT]
    var aa: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, aaData]
    var prec: MpdSsizeT
    var ulpexp: MpdSsizeT
    var workstatus: uint32T
    if result == a:
      if not mpdQcopy(addr(aa), a, status):
        mpdSeterror(result, mPD_MallocError, status)
        return
      a = addr(aa)
    workctx.clamp = 0
    prec = ctx.prec + 3
    while 1:
      workctx.prec = prec
      workstatus = 0
      mpdQexp(result, a, addr(workctx), addr(workstatus))
      status[] = status[] or workstatus
      ulpexp = result.exp + result.digits - workctx.prec
      if workstatus and mPD_Underflow:
        ##  The effective work precision is result->digits.
        ulpexp = result.exp
      ssettriple(addr(ulp), mpd_Pos, 1, ulpexp)
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
      mpdQadd(addr(t1), result, addr(ulp), addr(workctx), addr(workctx.status))
      mpdQsub(addr(t2), result, addr(ulp), addr(workctx), addr(workctx.status))
      if mpdIsspecial(result) or mpdIszerocoeff(result) or
          mpdQcmp(addr(t1), addr(t2), status) == 0:
        workctx.clamp = ctx.clamp
        mpdZeropad(result, addr(workctx), status)
        mpdCheckUnderflow(result, addr(workctx), status)
        mpdQfinalize(result, addr(workctx), status)
        break
      inc(prec, mpd_Rdigits)
    mpdDel(addr(t1))
    mpdDel(addr(t2))
    mpdDel(addr(ulp))
    mpdDel(addr(aa))
  else:
    mpdQexp(result, a, addr(workctx), status)
    mpdZeropad(result, addr(workctx), status)
    mpdCheckUnderflow(result, addr(workctx), status)
    mpdQfinalize(result, addr(workctx), status)

##  Fused multiply-add: (a * b) + c, with a single final rounding.

proc mpdQfma*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; c: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var workstatus: uint32T = 0
  var cc: ptr MpdT = nil
  if result == c:
    if (cc = mpdQncopy(c)) == nil:
      mpdSeterror(result, mPD_MallocError, status)
      return
    c = cc
  mpdQmul(result, a, b, ctx, addr(workstatus))
  if not (workstatus and mPD_InvalidOperation):
    mpdQadd(result, result, c, ctx, addr(workstatus))
  if cc:
    mpdDel(cc)
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

proc lnSchedulePrec*(klist: array[mpd_Max_Prec_Log2, MpdSsizeT]; maxprec: MpdSsizeT;
                    initprec: MpdSsizeT): cint =
  var k: MpdSsizeT
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
  when mpd_Rdigits != 19:
  var mpdLn10Data*: array[mpd_Minalloc_Max, MpdUintT] = [] ##  C2NIM
                                                     ##  6983716328982174407ULL, 9089704281976336583ULL, 1515961135648465461ULL,
                                                     ##  4416816335727555703ULL, 2900988039194170265ULL, 2307925037472986509ULL,
                                                     ##   107598438319191292ULL, 3466624107184669231ULL, 4450099781311469159ULL,
                                                     ##  9807828059751193854ULL, 7713456862091670584ULL, 1492198849978748873ULL,
                                                     ##  6528728696511086257ULL, 2385392051446341972ULL, 8692180205189339507ULL,
                                                     ##  6518769751037497088ULL, 2375253577097505395ULL, 9095610299291824318ULL,
                                                     ##   982748238504564801ULL, 5438635917781170543ULL, 7547331541421808427ULL,
                                                     ##   752371033310119785ULL, 3171643095059950878ULL, 9785265383207606726ULL,
                                                     ##  2932258279850258550ULL, 5497347726624257094ULL, 2976979522110718264ULL,
                                                     ##  9221477656763693866ULL, 1979650047149510504ULL, 6674183485704422507ULL,
                                                     ##  9702766860595249671ULL, 9278096762712757753ULL, 9314848524948644871ULL,
                                                     ##  6826928280848118428ULL,  754403708474699401ULL,  230105703089634572ULL,
                                                     ##  1929203337658714166ULL, 7589402567763113569ULL, 4208241314695689016ULL,
                                                     ##  2922455440575892572ULL, 9356734206705811364ULL, 2684916746550586856ULL,
                                                     ##   644507064800027750ULL, 9476834636167921018ULL, 5659121373450747856ULL,
                                                     ##  2835522011480466371ULL, 6470806855677432162ULL, 7141748003688084012ULL,
                                                     ##  9619404400222105101ULL, 5504893431493939147ULL, 6674744042432743651ULL,
                                                     ##  2287698219886746543ULL, 7773262884616336622ULL, 1985283935053089653ULL,
                                                     ##  4680843799894826233ULL, 8168948290720832555ULL, 8067566662873690987ULL,
                                                     ##  6248633409525465082ULL, 9829834196778404228ULL, 3524802359972050895ULL,
                                                     ##  3327900967572609677ULL,  110148862877297603ULL,  179914546843642076ULL,
                                                     ##  2302585092994045684ULL
else:
  when mpd_Rdigits != 9:
  var mpdLn10Data*: array[mpd_Minalloc_Max, MpdUintT] = [401682692, 708474699,
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

var mpdLn10*: MpdT = [mpd_Static or mpd_Const_Data,
                  -(mpd_Minalloc_Max * mpd_Rdigits - 1),
                  mpd_Minalloc_Max * mpd_Rdigits, mpd_Minalloc_Max,
                  mpd_Minalloc_Max, cast[ptr MpdUintT](mpdLn10Data)]

##
##  Set 'result' to log(10).
##    Ulp error: abs(result - log(10)) < ulp(log(10))
##    Relative error: abs(result - log(10)) < 5 * 10**-prec * log(10)
##
##  NOTE: The relative error is not derived from the ulp error, but
##  calculated separately using the fact that 23/10 < log(10) < 24/10.
##

proc mpdQln10*(result: ptr MpdT; prec: MpdSsizeT; status: ptr uint32T) =
  var
    varcontext: MpdContextT
    maxcontext: MpdContextT
  var tmpData: array[mpd_Minalloc_Max, MpdUintT]
  var tmp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tmpData]
  var static10Data: array[1, MpdUintT] = [10]
  var static10: MpdT = [0 or mpd_Static or mpd_Const_Data, 0, 2, 1, 1, static10Data]
  var klist: array[mpd_Max_Prec_Log2, MpdSsizeT]
  var rnd: MpdUintT
  var shift: MpdSsizeT
  var i: cint
  assert(prec >= 1)
  shift = mpd_Minalloc_Max * mpd_Rdigits - prec
  shift = if shift < 0: 0 else: shift
  rnd = mpdQshiftr(result, addr(mpdLn10), shift, status)
  if rnd == mpd_Uint_Max:
    mpdSeterror(result, mPD_MallocError, status)
    return
  result.exp = -(result.digits - 1)
  mpdMaxcontext(addr(maxcontext))
  if prec < mpd_Minalloc_Max * mpd_Rdigits:
    maxcontext.prec = prec
    mpdApplyRoundExcess(result, rnd, addr(maxcontext), status)
    status[] = status[] or (mPD_Inexact or mPD_Rounded)
    return
  mpdMaxcontext(addr(varcontext))
  varcontext.round = mpd_Round_Trunc
  i = lnSchedulePrec(klist, prec + 2, -result.exp)
  while i >= 0:
    varcontext.prec = 2 * klist[i] + 3
    result.flags = result.flags xor mpd_Neg
    mpdQexp(addr(tmp), result, addr(varcontext), status)
    result.flags = result.flags xor mpd_Neg
    mpdQmul(addr(tmp), addr(static10), addr(tmp), addr(varcontext), status)
    mpdQsub(addr(tmp), addr(tmp), addr(one), addr(maxcontext), status)
    mpdQadd(result, result, addr(tmp), addr(maxcontext), status)
    if mpdIsspecial(result):
      break
    dec(i)
  mpdDel(addr(tmp))
  maxcontext.prec = prec
  mpdQfinalize(result, addr(maxcontext), status)

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

var lnapprox*: array[900, uint16T] = [0, 10, 20, 30, 39, 49, 58, 68, 77, 86, 95, 104, 113, 122,
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
                                 1597, 1599, 1601, 1603, 1605, 1607, 1609, 691, 689, 687,
                                 685, 683, 681, 679, 677, 675, 673, 671, 669, 668, 666,
                                 664, 662, 660, 658, 656, 654, 652, 650, 648, 646, 644,
                                 642, 641, 639, 637, 635, 633, 631, 629, 627, 626, 624,
                                 622, 620, 618, 616, 614, 612, 611, 609, 607, 605, 603,
                                 602, 600, 598, 596, 594, 592, 591, 589, 587, 585, 583,
                                 582, 580, 578, 576, 574, 573, 571, 569, 567, 566, 564,
                                 562, 560, 559, 557, 555, 553, 552, 550, 548, 546, 545,
                                 543, 541, 540, 538, 536, 534, 533, 531, 529, 528, 526,
                                 524, 523, 521, 519, 518, 516, 514, 512, 511, 509, 508,
                                 506, 504, 502, 501, 499, 498, 496, 494, 493, 491, 489,
                                 488, 486, 484, 483, 481, 480, 478, 476, 475, 473, 472,
                                 470, 468, 467, 465, 464, 462, 460, 459, 457, 456, 454,
                                 453, 451, 449, 448, 446, 445, 443, 442, 440, 438, 437,
                                 435, 434, 432, 431, 429, 428, 426, 425, 423, 422, 420,
                                 419, 417, 416, 414, 412, 411, 410, 408, 406, 405, 404,
                                 402, 400, 399, 398, 396, 394, 393, 392, 390, 389, 387,
                                 386, 384, 383, 381, 380, 378, 377, 375, 374, 372, 371,
                                 370, 368, 367, 365, 364, 362, 361, 360, 358, 357, 355,
                                 354, 352, 351, 350, 348, 347, 345, 344, 342, 341, 340,
                                 338, 337, 336, 334, 333, 331, 330, 328, 327, 326, 324,
                                 323, 322, 320, 319, 318, 316, 315, 313, 312, 311, 309,
                                 308, 306, 305, 304, 302, 301, 300, 298, 297, 296, 294,
                                 293, 292, 290, 289, 288, 286, 285, 284, 282, 281, 280,
                                 278, 277, 276, 274, 273, 272, 270, 269, 268, 267, 265,
                                 264, 263, 261, 260, 259, 258, 256, 255, 254, 252, 251,
                                 250, 248, 247, 246, 245, 243, 242, 241, 240, 238, 237,
                                 236, 234, 233, 232, 231, 229, 228, 227, 226, 224, 223,
                                 222, 221, 219, 218, 217, 216, 214, 213, 212, 211, 210,
                                 208, 207, 206, 205, 203, 202, 201, 200, 198, 197, 196,
                                 195, 194, 192, 191, 190, 189, 188, 186, 185, 184, 183,
                                 182, 180, 179, 178, 177, 176, 174, 173, 172, 171, 170,
                                 168, 167, 166, 165, 164, 162, 161, 160, 159, 158, 157,
                                 156, 154, 153, 152, 151, 150, 148, 147, 146, 145, 144,
                                 143, 142, 140, 139, 138, 137, 136, 135, 134, 132, 131,
                                 130, 129, 128, 127, 126, 124, 123, 122, 121, 120, 119,
                                 118, 116, 115, 114, 113, 112, 111, 110, 109, 108, 106,
                                 105, 104, 103, 102, 101, 100, 99, 98, 97, 95, 94, 93, 92,
                                 91, 90, 89, 88, 87, 86, 84, 83, 82, 81, 80, 79, 78, 77, 76,
                                 75, 74, 73, 72, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60,
                                 59, 58, 57, 56, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44,
                                 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 31, 30, 29, 28,
                                 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13,
                                 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

##  index 0 - 400: log((i+100)/100) * 1000
##
##  Internal ln() function that does not check for specials, zero or one.
##  Relative error: abs(result - log(a)) < 0.1 * 10**-prec * abs(log(a))
##

proc mpdQln*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var
    varcontext: MpdContextT
    maxcontext: MpdContextT
  var z: ptr MpdT = result
  var vData: array[mpd_Minalloc_Max, MpdUintT]
  var v: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, vData]
  var vtmpData: array[mpd_Minalloc_Max, MpdUintT]
  var vtmp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, vtmpData]
  var tmpData: array[mpd_Minalloc_Max, MpdUintT]
  var tmp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tmpData]
  var klist: array[mpd_Max_Prec_Log2, MpdSsizeT]
  var
    maxprec: MpdSsizeT
    shift: MpdSsizeT
    t: MpdSsizeT
  var
    aDigits: MpdSsizeT
    aExp: MpdSsizeT
  var
    dummy: MpdUintT
    x: MpdUintT
  var i: cint
  assert(not mpdIsspecial(a) and not mpdIszerocoeff(a))
  ##
  ##  We are calculating ln(a) = ln(v * 10^t) = ln(v) + t*ln(10),
  ##  where 0.5 < v <= 5.
  ##
  if not mpdQcopy(addr(v), a, status):
    mpdSeterror(result, mPD_MallocError, status)
    break finish
  mpdGetMsdigits(addr(dummy), addr(x), addr(v), 3)
  if x < 10:
    x = x * 10
  if x < 100:
    x = x * 10
  dec(x, 100)
  ##  a may equal z
  aDigits = a.digits
  aExp = a.exp
  mpdMinalloc(z)
  mpdClearFlags(z)
  z.data[0] = lnapprox[x]
  z.len = 1
  z.exp = -3
  mpdSetdigits(z)
  if x <= 400:
    ##  Reduce the input operand to 1.00 <= v <= 5.00. Let y = x + 100,
    ##  so 100 <= y <= 500. Since y contains the most significant digits
    ##  of v, y/100 <= v < (y+1)/100 and abs(z - log(v)) < 10**-2.
    v.exp = -(aDigits - 1)
    t = aExp + aDigits - 1
  else:
    ##  Reduce the input operand to 0.500 < v <= 0.999. Let y = x + 100,
    ##  so 500 < y <= 999. Since y contains the most significant digits
    ##  of v, y/1000 <= v < (y+1)/1000 and abs(z - log(v)) < 10**-2.
    v.exp = -aDigits
    t = aExp + aDigits
    mpdSetNegative(z)
  mpdMaxcontext(addr(maxcontext))
  mpdMaxcontext(addr(varcontext))
  varcontext.round = mpd_Round_Trunc
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
    var cmp: cint = mpdCmp(addr(v), addr(one))
    ##  Upper bound (assume v > 1): abs(v-1), unrounded
    mpdQsub(addr(tmp), addr(v), addr(one), addr(maxcontext), addr(maxcontext.status))
    if maxcontext.status and mPD_Errors:
      mpdSeterror(result, mPD_MallocError, status)
      break finish
    if cmp < 0:
      ##  v < 1: abs((v-1)*10)
      inc(tmp.exp, 1)
    if mpdAdjexp(addr(tmp)) < mpdEtiny(ctx):
      ##  The upper bound is less than etiny: Underflow to zero
      settriple(result, (cmp < 0), 1, mpdEtiny(ctx) - 1)
      break finish
    dec(tmp.exp, 1)
    if mpdAdjexp(addr(tmp)) < 0:
      ##  Absolute error of the loop: abs(z - log(v)) < 10**-p. If
      ##  p = ctx->prec+2-adjexp(lower), then the relative error of
      ##  the result is (using 10**adjexp(x) <= abs(x)):
      ##
      ##    abs(z - log(v)) / abs(log(v)) < 10**-p / abs(log(v))
      ##                                  <= 10**(-ctx->prec-2)
      ##
      maxprec = maxprec - mpdAdjexp(addr(tmp))
  i = lnSchedulePrec(klist, maxprec, 2)
  while i >= 0:
    varcontext.prec = 2 * klist[i] + 3
    z.flags = z.flags xor mpd_Neg
    mpdQexp(addr(tmp), z, addr(varcontext), status)
    z.flags = z.flags xor mpd_Neg
    if v.digits > varcontext.prec:
      shift = v.digits - varcontext.prec
      mpdQshiftr(addr(vtmp), addr(v), shift, status)
      inc(vtmp.exp, shift)
      mpdQmul(addr(tmp), addr(vtmp), addr(tmp), addr(varcontext), status)
    else:
      mpdQmul(addr(tmp), addr(v), addr(tmp), addr(varcontext), status)
    mpdQsub(addr(tmp), addr(tmp), addr(one), addr(maxcontext), status)
    mpdQadd(z, z, addr(tmp), addr(maxcontext), status)
    if mpdIsspecial(z):
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
  mpdQln10(addr(v), maxprec + 1, status)
  mpdQmulSsize(addr(tmp), addr(v), t, addr(maxcontext), status)
  mpdQadd(result, addr(tmp), z, addr(maxcontext), status)
  status[] = status[] or (mPD_Inexact or mPD_Rounded)
  mpdDel(addr(v))
  mpdDel(addr(vtmp))
  mpdDel(addr(tmp))

##  ln(a)

proc mpdQln*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var workctx: MpdContextT
  var
    adjexp: MpdSsizeT
    t: MpdSsizeT
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    if mpdIsnegative(a):
      mpdSeterror(result, mPD_InvalidOperation, status)
      return
    mpdSetspecial(result, mpd_Pos, mpd_Inf)
    return
  if mpdIszerocoeff(a):
    mpdSetspecial(result, mpd_Neg, mpd_Inf)
    return
  if mpdIsnegative(a):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if mpdCmp(a, addr(one)) == 0:
    settriple(result, mpd_Pos, 0, 0)
    return
  adjexp = mpdAdjexp(a)
  t = if (adjexp < 0): -adjexp - 1 else: adjexp
  t = t * 2
  if mpdExpDigits(t) - 1 > ctx.emax:
    status[] = status[] or (mPD_Overflow or mPD_Inexact or mPD_Rounded)
    mpdSetspecial(result, (adjexp < 0), mpd_Inf)
    return
  workctx = ctx[]
  workctx.round = mpd_Round_Half_Even
  if ctx.allcr:
    var t1Data: array[mpd_Minalloc_Max, MpdUintT]
    var t1: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, t1Data]
    var t2Data: array[mpd_Minalloc_Max, MpdUintT]
    var t2: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, t2Data]
    var ulpData: array[mpd_Minalloc_Max, MpdUintT]
    var ulp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, ulpData]
    var aaData: array[mpd_Minalloc_Max, MpdUintT]
    var aa: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, aaData]
    var prec: MpdSsizeT
    if result == a:
      if not mpdQcopy(addr(aa), a, status):
        mpdSeterror(result, mPD_MallocError, status)
        return
      a = addr(aa)
    workctx.clamp = 0
    prec = ctx.prec + 3
    while 1:
      workctx.prec = prec
      mpdQln(result, a, addr(workctx), status)
      ssettriple(addr(ulp), mpd_Pos, 1, result.exp + result.digits - workctx.prec)
      workctx.prec = ctx.prec
      mpdQadd(addr(t1), result, addr(ulp), addr(workctx), addr(workctx.status))
      mpdQsub(addr(t2), result, addr(ulp), addr(workctx), addr(workctx.status))
      if mpdIsspecial(result) or mpdIszerocoeff(result) or
          mpdQcmp(addr(t1), addr(t2), status) == 0:
        workctx.clamp = ctx.clamp
        mpdCheckUnderflow(result, addr(workctx), status)
        mpdQfinalize(result, addr(workctx), status)
        break
      inc(prec, mpd_Rdigits)
    mpdDel(addr(t1))
    mpdDel(addr(t2))
    mpdDel(addr(ulp))
    mpdDel(addr(aa))
  else:
    mpdQln(result, a, addr(workctx), status)
    mpdCheckUnderflow(result, addr(workctx), status)
    mpdQfinalize(result, addr(workctx), status)

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

proc mpdQlog10*(action: cint; result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
               status: ptr uint32T) =
  var workctx: MpdContextT
  var ln10Data: array[mpd_Minalloc_Max, MpdUintT]
  var ln10: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, ln10Data]
  mpdMaxcontext(addr(workctx))
  workctx.prec = ctx.prec + 3
  ##  relative error: 0.1 * 10**(-p-3). The specific underflow shortcut
  ##  in _mpd_qln() does not change the final result.
  mpdQln(result, a, addr(workctx), status)
  ##  relative error: 5 * 10**(-p-3)
  mpdQln10(addr(ln10), workctx.prec, status)
  if action == do_Finalize:
    workctx = ctx[]
    workctx.round = mpd_Round_Half_Even
  mpdQdiv(no_Ideal_Exp, result, result, addr(ln10), addr(workctx), status)
  mpdDel(addr(ln10))

##  log10(a)

proc mpdQlog10*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var workctx: MpdContextT
  var
    adjexp: MpdSsizeT
    t: MpdSsizeT
  workctx = ctx[]
  workctx.round = mpd_Round_Half_Even
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    if mpdIsnegative(a):
      mpdSeterror(result, mPD_InvalidOperation, status)
      return
    mpdSetspecial(result, mpd_Pos, mpd_Inf)
    return
  if mpdIszerocoeff(a):
    mpdSetspecial(result, mpd_Neg, mpd_Inf)
    return
  if mpdIsnegative(a):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if mpdCoeffIspow10(a):
    var sign: uint8T = 0
    adjexp = mpdAdjexp(a)
    if adjexp < 0:
      sign = 1
      adjexp = -adjexp
    settriple(result, sign, adjexp, 0)
    mpdQfinalize(result, addr(workctx), status)
    return
  adjexp = mpdAdjexp(a)
  t = if (adjexp < 0): -adjexp - 1 else: adjexp
  if mpdExpDigits(t) - 1 > ctx.emax:
    status[] = status[] or (mPD_Overflow or mPD_Inexact or mPD_Rounded)
    mpdSetspecial(result, (adjexp < 0), mpd_Inf)
    return
  if ctx.allcr:
    var t1Data: array[mpd_Minalloc_Max, MpdUintT]
    var t1: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, t1Data]
    var t2Data: array[mpd_Minalloc_Max, MpdUintT]
    var t2: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, t2Data]
    var ulpData: array[mpd_Minalloc_Max, MpdUintT]
    var ulp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, ulpData]
    var aaData: array[mpd_Minalloc_Max, MpdUintT]
    var aa: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, aaData]
    var prec: MpdSsizeT
    if result == a:
      if not mpdQcopy(addr(aa), a, status):
        mpdSeterror(result, mPD_MallocError, status)
        return
      a = addr(aa)
    workctx.clamp = 0
    prec = ctx.prec + 3
    while 1:
      workctx.prec = prec
      mpdQlog10(skip_Finalize, result, a, addr(workctx), status)
      ssettriple(addr(ulp), mpd_Pos, 1, result.exp + result.digits - workctx.prec)
      workctx.prec = ctx.prec
      mpdQadd(addr(t1), result, addr(ulp), addr(workctx), addr(workctx.status))
      mpdQsub(addr(t2), result, addr(ulp), addr(workctx), addr(workctx.status))
      if mpdIsspecial(result) or mpdIszerocoeff(result) or
          mpdQcmp(addr(t1), addr(t2), status) == 0:
        workctx.clamp = ctx.clamp
        mpdCheckUnderflow(result, addr(workctx), status)
        mpdQfinalize(result, addr(workctx), status)
        break
      inc(prec, mpd_Rdigits)
    mpdDel(addr(t1))
    mpdDel(addr(t2))
    mpdDel(addr(ulp))
    mpdDel(addr(aa))
  else:
    mpdQlog10(do_Finalize, result, a, addr(workctx), status)
    mpdCheckUnderflow(result, addr(workctx), status)

##
##  Maximum of the two operands. Attention: If one operand is a quiet NaN and the
##  other is numeric, the numeric operand is returned. This may not be what one
##  expects.
##

proc mpdQmax*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var c: cint
  if mpdIsqnan(a) and not mpdIsnan(b):
    mpdQcopy(result, b, status)
  elif mpdIsqnan(b) and not mpdIsnan(a):
    mpdQcopy(result, a, status)
  elif mpdQcheckNans(result, a, b, ctx, status):
    return
  else:
    c = mpdCmp(a, b)
    if c == 0:
      c = mpdCmpNumequal(a, b)
    if c < 0:
      mpdQcopy(result, b, status)
    else:
      mpdQcopy(result, a, status)
  mpdQfinalize(result, ctx, status)

##
##  Maximum magnitude: Same as mpd_max(), but compares the operands with their
##  sign ignored.
##

proc mpdQmaxMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  var c: cint
  if mpdIsqnan(a) and not mpdIsnan(b):
    mpdQcopy(result, b, status)
  elif mpdIsqnan(b) and not mpdIsnan(a):
    mpdQcopy(result, a, status)
  elif mpdQcheckNans(result, a, b, ctx, status):
    return
  else:
    c = mpdCmpAbs(a, b)
    if c == 0:
      c = mpdCmpNumequal(a, b)
    if c < 0:
      mpdQcopy(result, b, status)
    else:
      mpdQcopy(result, a, status)
  mpdQfinalize(result, ctx, status)

##
##  Minimum of the two operands. Attention: If one operand is a quiet NaN and the
##  other is numeric, the numeric operand is returned. This may not be what one
##  expects.
##

proc mpdQmin*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var c: cint
  if mpdIsqnan(a) and not mpdIsnan(b):
    mpdQcopy(result, b, status)
  elif mpdIsqnan(b) and not mpdIsnan(a):
    mpdQcopy(result, a, status)
  elif mpdQcheckNans(result, a, b, ctx, status):
    return
  else:
    c = mpdCmp(a, b)
    if c == 0:
      c = mpdCmpNumequal(a, b)
    if c < 0:
      mpdQcopy(result, a, status)
    else:
      mpdQcopy(result, b, status)
  mpdQfinalize(result, ctx, status)

##
##  Minimum magnitude: Same as mpd_min(), but compares the operands with their
##  sign ignored.
##

proc mpdQminMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  var c: cint
  if mpdIsqnan(a) and not mpdIsnan(b):
    mpdQcopy(result, b, status)
  elif mpdIsqnan(b) and not mpdIsnan(a):
    mpdQcopy(result, a, status)
  elif mpdQcheckNans(result, a, b, ctx, status):
    return
  else:
    c = mpdCmpAbs(a, b)
    if c == 0:
      c = mpdCmpNumequal(a, b)
    if c < 0:
      mpdQcopy(result, a, status)
    else:
      mpdQcopy(result, b, status)
  mpdQfinalize(result, ctx, status)

##  Minimum space needed for the result array in _karatsuba_rec().

proc kmulResultsize*(la: MpdSizeT; lb: MpdSizeT): MpdSizeT =
  var
    n: MpdSizeT
    m: MpdSizeT
  n = addSizeT(la, lb)
  n = addSizeT(n, 1)
  m = (la + 1) div 2 + 1
  m = mulSizeT(m, 3)
  return if (m > n): m else: n

##  Work space needed in _karatsuba_rec(). lim >= 4

proc kmulWorksize*(n: MpdSizeT; lim: MpdSizeT): MpdSizeT =
  var m: MpdSizeT
  if n <= lim:
    return 0
  m = (n + 1) div 2 + 1
  return addSizeT(mulSizeT(m, 2), kmulWorksize(m, lim))

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

proc karatsubaRec*(c: ptr MpdUintT; a: ptr MpdUintT; b: ptr MpdUintT; w: ptr MpdUintT;
                  la: MpdSizeT; lb: MpdSizeT) =
  var
    m: MpdSizeT
    lt: MpdSizeT
  assert(la >= lb and lb > 0)
  assert(la <= mpd_Karatsuba_Basecase or w != nil)
  if la <= mpd_Karatsuba_Basecase:
    mpdBasemul(c, a, b, la, lb)
    return
  m = (la + 1) div 2
  ##  ceil(la/2)
  ##  lb <= m < la
  if lb <= m:
    ##  lb can now be larger than la-m
    if lb > la - m:
      lt = lb + lb + 1
      ##  space needed for result array
      mpdUintZero(w, lt)
      ##  clear result array
      karatsubaRec(w, b, a + m, w + lt, lb, la - m)
      ##  b*ah
    else:
      lt = (la - m) + (la - m) + 1
      ##  space needed for result array
      mpdUintZero(w, lt)
      ##  clear result array
      karatsubaRec(w, a + m, b, w + lt, la - m, lb)
      ##  ah*b
    mpdBaseaddto(c + m, w, (la - m) + lb)
    ##  add ah*b*B**m
    lt = m + m + 1
    ##  space needed for the result array
    mpdUintZero(w, lt)
    ##  clear result array
    karatsubaRec(w, a, b, w + lt, m, lb)
    ##  al*b
    mpdBaseaddto(c, w, m + lb)
    ##  add al*b
    return
  memcpy(w, a, m * sizeof(w[]))
  w[m] = 0
  mpdBaseaddto(w, a + m, la - m)
  memcpy(w + (m + 1), b, m * sizeof(w[]))
  w[m + 1 + m] = 0
  mpdBaseaddto(w + (m + 1), b + m, lb - m)
  karatsubaRec(c + m, w, w + (m + 1), w + 2 * (m + 1), m + 1, m + 1)
  lt = (la - m) + (la - m) + 1
  mpdUintZero(w, lt)
  karatsubaRec(w, a + m, b + m, w + lt, la - m, lb - m)
  mpdBaseaddto(c + 2 * m, w, (la - m) + (lb - m))
  mpdBasesubfrom(c + m, w, (la - m) + (lb - m))
  lt = m + m + 1
  mpdUintZero(w, lt)
  karatsubaRec(w, a, b, w + lt, m, m)
  mpdBaseaddto(c, w, m + m)
  mpdBasesubfrom(c + m, w, m + m)
  return

##
##  Multiply u and v, using Karatsuba multiplication. Returns a pointer
##  to the result or NULL in case of failure (malloc error).
##  Conditions: ulen >= vlen, ulen >= 4
##

proc mpdKmul*(u: ptr MpdUintT; v: ptr MpdUintT; ulen: MpdSizeT; vlen: MpdSizeT;
             rsize: ptr MpdSizeT): ptr MpdUintT =
  var
    result: ptr MpdUintT = nil
    w: ptr MpdUintT = nil
  var m: MpdSizeT
  assert(ulen >= 4)
  assert(ulen >= vlen)
  rsize[] = kmulResultsize(ulen, vlen)
  if (result = mpdCalloc(rsize[], sizeof(result[]))) == nil:
    return nil
  m = kmulWorksize(ulen, mpd_Karatsuba_Basecase)
  if m and ((w = mpdCalloc(m, sizeof(w[]))) == nil):
    mpdFree(result)
    return nil
  karatsubaRec(result, u, v, w, ulen, vlen)
  if w:
    mpdFree(w)
  return result

##
##  Determine the minimum length for the number theoretic transform. Valid
##  transform lengths are 2**n or 3*2**n, where 2**n <= MPD_MAXTRANSFORM_2N.
##  The function finds the shortest length m such that rsize <= m.
##

proc mpdGetTransformLen*(rsize: MpdSizeT): MpdSizeT =
  var log2rsize: MpdSizeT
  var
    x: MpdSizeT
    step: MpdSizeT
  assert(rsize >= 4)
  log2rsize = mpdBsr(rsize)
  if rsize <= 1024:
    ##  2**n is faster in this range.
    x = (cast[MpdSizeT](1)) shl log2rsize
    return if (rsize == x): x else: x shl 1
  elif rsize <= mpd_Maxtransform_2n:
    x = (cast[MpdSizeT](1)) shl log2rsize
    if rsize == x:
      return x
    step = x shr 1
    inc(x, step)
    return if (rsize <= x): x else: x + step
  elif rsize <= mpd_Maxtransform_2n + mpd_Maxtransform_2n div 2:
    return mpd_Maxtransform_2n + mpd_Maxtransform_2n div 2
  elif rsize <= 3 * mpd_Maxtransform_2n:
    return 3 * mpd_Maxtransform_2n
  else:
    return mpd_Size_Max

when defined(PPRO):
  when not defined(_MSC_VER):
    proc mpdGetControl87*(): cushort =
      var cw: cushort
      ##  C2NIM
      ##  __asm__ __volatile__ ("fnstcw %0" : "=m" (cw));
      return cw

    proc mpdSetControl87*(cw: cushort) =
      ##  C2NIM
      ##  __asm__ __volatile__ ("fldcw %0" : : "m" (cw));

  proc mpdSetFenv*(): cuint =
    var cw: cuint
    when defined(_MSC_VER):
      var flags: cuint = em_Invalid or em_Denormal or em_Zerodivide or em_Overflow or
          em_Underflow or em_Inexact or rc_Chop or pc_64
      var mask: cuint = mcw_Em or mcw_Rc or mcw_Pc
      var dummy: cuint
      control872(0, 0, addr(cw), nil)
      control872(flags, mask, addr(dummy), nil)
    else:
      cw = mpdGetControl87()
      mpdSetControl87(cw or 0x00000F3F)
    return cw

  proc mpdRestoreFenv*(cw: cuint) =
    when defined(_MSC_VER):
      var mask: cuint = mcw_Em or mcw_Rc or mcw_Pc
      var dummy: cuint
      control872(cw, mask, addr(dummy), nil)
    else:
      mpdSetControl87(cast[cushort](cw))

##
##  Multiply u and v, using the fast number theoretic transform. Returns
##  a pointer to the result or NULL in case of failure (malloc error).
##

proc mpdFntmul*(u: ptr MpdUintT; v: ptr MpdUintT; ulen: MpdSizeT; vlen: MpdSizeT;
               rsize: ptr MpdSizeT): ptr MpdUintT =
  var
    c1: ptr MpdUintT = nil
    c2: ptr MpdUintT = nil
    c3: ptr MpdUintT = nil
    vtmp: ptr MpdUintT = nil
  var n: MpdSizeT
  when defined(PPRO):
    var cw: cuint
    cw = mpdSetFenv()
  rsize[] = addSizeT(ulen, vlen)
  if (n = mpdGetTransformLen(rsize[])) == mpd_Size_Max:
    break mallocError
  if (c1 = mpdCalloc(n, sizeof(c1[]))) == nil:
    break mallocError
  if (c2 = mpdCalloc(n, sizeof(c2[]))) == nil:
    break mallocError
  if (c3 = mpdCalloc(n, sizeof(c3[]))) == nil:
    break mallocError
  memcpy(c1, u, ulen * (sizeof(c1[])))
  memcpy(c2, u, ulen * (sizeof(c2[])))
  memcpy(c3, u, ulen * (sizeof(c3[])))
  if u == v:
    if not fntAutoconvolute(c1, n, p1) or not fntAutoconvolute(c2, n, p2) or
        not fntAutoconvolute(c3, n, p3):
      break mallocError
  else:
    if (vtmp = mpdCalloc(n, sizeof(vtmp[]))) == nil:
      break mallocError
    memcpy(vtmp, v, vlen * (sizeof(vtmp[])))
    if not fntConvolute(c1, vtmp, n, p1):
      mpdFree(vtmp)
      break mallocError
    memcpy(vtmp, v, vlen * (sizeof(vtmp[])))
    mpdUintZero(vtmp + vlen, n - vlen)
    if not fntConvolute(c2, vtmp, n, p2):
      mpdFree(vtmp)
      break mallocError
    memcpy(vtmp, v, vlen * (sizeof(vtmp[])))
    mpdUintZero(vtmp + vlen, n - vlen)
    if not fntConvolute(c3, vtmp, n, p3):
      mpdFree(vtmp)
      break mallocError
    mpdFree(vtmp)
  crt3(c1, c2, c3, rsize[])
  when defined(PPRO):
    mpdRestoreFenv(cw)
  if c2:
    mpdFree(c2)
  if c3:
    mpdFree(c3)
  return c1
  if c1:
    mpdFree(c1)
  c1 = nil
  break `out`

##
##  Karatsuba multiplication with FNT/basemul as the base case.
##

proc karatsubaRecFnt*(c: ptr MpdUintT; a: ptr MpdUintT; b: ptr MpdUintT; w: ptr MpdUintT;
                     la: MpdSizeT; lb: MpdSizeT): cint =
  var
    m: MpdSizeT
    lt: MpdSizeT
  assert(la >= lb and lb > 0)
  assert(la <= 3 * (mpd_Maxtransform_2n div 2) or w != nil)
  if la <= 3 * (mpd_Maxtransform_2n div 2):
    if lb <= 192:
      mpdBasemul(c, b, a, lb, la)
    else:
      var result: ptr MpdUintT
      var dummy: MpdSizeT
      if (result = mpdFntmul(a, b, la, lb, addr(dummy))) == nil:
        return 0
      memcpy(c, result, (la + lb) * (sizeof(result[])))
      mpdFree(result)
    return 1
  m = (la + 1) div 2
  ##  ceil(la/2)
  ##  lb <= m < la
  if lb <= m:
    ##  lb can now be larger than la-m
    if lb > la - m:
      lt = lb + lb + 1
      ##  space needed for result array
      mpdUintZero(w, lt)
      ##  clear result array
      if not karatsubaRecFnt(w, b, a + m, w + lt, lb, la - m):
        ##  b*ah
        return 0
        ##  GCOV_UNLIKELY
    else:
      lt = (la - m) + (la - m) + 1
      ##  space needed for result array
      mpdUintZero(w, lt)
      ##  clear result array
      if not karatsubaRecFnt(w, a + m, b, w + lt, la - m, lb):
        ##  ah*b
        return 0
        ##  GCOV_UNLIKELY
    mpdBaseaddto(c + m, w, (la - m) + lb)
    ##  add ah*b*B**m
    lt = m + m + 1
    ##  space needed for the result array
    mpdUintZero(w, lt)
    ##  clear result array
    if not karatsubaRecFnt(w, a, b, w + lt, m, lb):
      ##  al*b
      return 0
      ##  GCOV_UNLIKELY
    mpdBaseaddto(c, w, m + lb)
    ##  add al*b
    return 1
  memcpy(w, a, m * sizeof(w[]))
  w[m] = 0
  mpdBaseaddto(w, a + m, la - m)
  memcpy(w + (m + 1), b, m * sizeof(w[]))
  w[m + 1 + m] = 0
  mpdBaseaddto(w + (m + 1), b + m, lb - m)
  if not karatsubaRecFnt(c + m, w, w + (m + 1), w + 2 * (m + 1), m + 1, m + 1):
    return 0
    ##  GCOV_UNLIKELY
  lt = (la - m) + (la - m) + 1
  mpdUintZero(w, lt)
  if not karatsubaRecFnt(w, a + m, b + m, w + lt, la - m, lb - m):
    return 0
    ##  GCOV_UNLIKELY
  mpdBaseaddto(c + 2 * m, w, (la - m) + (lb - m))
  mpdBasesubfrom(c + m, w, (la - m) + (lb - m))
  lt = m + m + 1
  mpdUintZero(w, lt)
  if not karatsubaRecFnt(w, a, b, w + lt, m, m):
    return 0
    ##  GCOV_UNLIKELY
  mpdBaseaddto(c, w, m + m)
  mpdBasesubfrom(c + m, w, m + m)
  return 1

##
##  Multiply u and v, using Karatsuba multiplication with the FNT as the
##  base case. Returns a pointer to the result or NULL in case of failure
##  (malloc error). Conditions: ulen >= vlen, ulen >= 4.
##

proc mpdKmulFnt*(u: ptr MpdUintT; v: ptr MpdUintT; ulen: MpdSizeT; vlen: MpdSizeT;
                rsize: ptr MpdSizeT): ptr MpdUintT =
  var
    result: ptr MpdUintT = nil
    w: ptr MpdUintT = nil
  var m: MpdSizeT
  assert(ulen >= 4)
  assert(ulen >= vlen)
  rsize[] = kmulResultsize(ulen, vlen)
  if (result = mpdCalloc(rsize[], sizeof(result[]))) == nil:
    return nil
  m = kmulWorksize(ulen, 3 * (mpd_Maxtransform_2n div 2))
  if m and ((w = mpdCalloc(m, sizeof(w[]))) == nil):
    mpdFree(result)
    ##  GCOV_UNLIKELY
    return nil
    ##  GCOV_UNLIKELY
  if not karatsubaRecFnt(result, u, v, w, ulen, vlen):
    mpdFree(result)
    result = nil
  if w:
    mpdFree(w)
  return result

##  Deal with the special cases of multiplying infinities.

proc mpdQmulInf*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; status: ptr uint32T) =
  if mpdIsinfinite(a):
    if mpdIszero(b):
      mpdSeterror(result, mPD_InvalidOperation, status)
    else:
      mpdSetspecial(result, mpdSign(a) xor mpdSign(b), mpd_Inf)
    return
  assert(mpdIsinfinite(b))
  if mpdIszero(a):
    mpdSeterror(result, mPD_InvalidOperation, status)
  else:
    mpdSetspecial(result, mpdSign(a) xor mpdSign(b), mpd_Inf)

##
##  Internal function: Multiply a and b. _mpd_qmul deals with specials but
##  does NOT finalize the result. This is for use in mpd_fma().
##

proc mpdQmul*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var
    big: ptr MpdT = a
    small: ptr MpdT = b
  var rdata: ptr MpdUintT = nil
  var rbuf: array[mpd_Minalloc_Max, MpdUintT]
  var
    rsize: MpdSizeT
    i: MpdSizeT
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      return
    mpdQmulInf(result, a, b, status)
    return
  if small.len > big.len:
    mpdPtrswap(addr(big), addr(small))
  rsize = big.len + small.len
  if big.len == 1:
    mpdSinglemul(result.data, big.data[0], small.data[0])
    break finish
  if rsize <= cast[MpdSizeT](mpd_Minalloc_Max):
    if big.len == 2:
      mpdMul2Le2(rbuf, big.data, small.data, small.len)
    else:
      mpdUintZero(rbuf, rsize)
      if small.len == 1:
        mpdShortmul(rbuf, big.data, big.len, small.data[0])
      else:
        mpdBasemul(rbuf, small.data, big.data, small.len, big.len)
    if not mpdQresize(result, rsize, status):
      return
    i = 0
    while i < rsize:
      result.data[i] = rbuf[i]
      inc(i)
    break finish
  if small.len <= 256:
    rdata = mpdCalloc(rsize, sizeof(rdata[]))
    if rdata != nil:
      if small.len == 1:
        mpdShortmul(rdata, big.data, big.len, small.data[0])
      else:
        mpdBasemul(rdata, small.data, big.data, small.len, big.len)
  elif rsize <= 1024:
    rdata = mpdKmul(big.data, small.data, big.len, small.len, addr(rsize))
  elif rsize <= 3 * mpd_Maxtransform_2n:
    rdata = mpdFntmul(big.data, small.data, big.len, small.len, addr(rsize))
  else:
    rdata = mpdKmulFnt(big.data, small.data, big.len, small.len, addr(rsize))
  if rdata == nil:
    mpdSeterror(result, mPD_MallocError, status)
    return
  if mpdIsdynamicData(result):
    mpdFree(result.data)
  result.data = rdata
  result.alloc = rsize
  mpdSetDynamicData(result)
  mpdSetFlags(result, mpdSign(a) xor mpdSign(b))
  result.exp = big.exp + small.exp
  result.len = mpdRealSize(result.data, rsize)
  ##  resize to smaller cannot fail
  mpdQresize(result, result.len, status)
  mpdSetdigits(result)

##  Multiply a and b.

proc mpdQmul*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  mpdQmul(result, a, b, ctx, status)
  mpdQfinalize(result, ctx, status)

##  Multiply a and b. Set NaN/Invalid_operation if the result is inexact.

proc mpdQmulExact*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var workstatus: uint32T = 0
  mpdQmul(result, a, b, ctx, addr(workstatus))
  status[] = status[] or workstatus
  if workstatus and (mPD_Inexact or mPD_Rounded or mPD_Clamped):
    mpdSeterror(result, mPD_InvalidOperation, status)

##  Multiply decimal and mpd_ssize_t.

proc mpdQmulSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var maxcontext: MpdContextT
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  mpdMaxcontext(addr(maxcontext))
  mpdQssetSsize(addr(bb), b, addr(maxcontext), status)
  mpdQmul(result, a, addr(bb), ctx, status)
  mpdDel(addr(bb))

##  Multiply decimal and mpd_uint_t.

proc mpdQmulUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  var maxcontext: MpdContextT
  var bbData: array[mpd_Minalloc_Max, MpdUintT]
  var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
  mpdMaxcontext(addr(maxcontext))
  mpdQssetUint(addr(bb), b, addr(maxcontext), status)
  mpdQmul(result, a, addr(bb), ctx, status)
  mpdDel(addr(bb))

proc mpdQmulI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  mpdQmulSsize(result, a, b, ctx, status)

proc mpdQmulU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT;
                status: ptr uint32T) =
  mpdQmulUint(result, a, b, ctx, status)

when defined(CONFIG_64):
  proc mpdQmulI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    mpdQmulSsize(result, a, b, ctx, status)

  proc mpdQmulU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    mpdQmulUint(result, a, b, ctx, status)

elif not defined(legacy_Compiler):
  ##  Multiply decimal and int64_t.
  proc mpdQmulI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    var maxcontext: MpdContextT
    var bbData: array[mpd_Minalloc_Max, MpdUintT]
    var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
    mpdMaxcontext(addr(maxcontext))
    mpdQsetI64(addr(bb), b, addr(maxcontext), status)
    mpdQmul(result, a, addr(bb), ctx, status)
    mpdDel(addr(bb))

  ##  Multiply decimal and uint64_t.
  proc mpdQmulU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
    var maxcontext: MpdContextT
    var bbData: array[mpd_Minalloc_Max, MpdUintT]
    var bb: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, bbData]
    mpdMaxcontext(addr(maxcontext))
    mpdQsetU64(addr(bb), b, addr(maxcontext), status)
    mpdQmul(result, a, addr(bb), ctx, status)
    mpdDel(addr(bb))

##  Like the minus operator.

proc mpdQminus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
  if mpdIszero(a) and ctx.round != mpd_Round_Floor:
    mpdQcopyAbs(result, a, status)
  else:
    mpdQcopyNegate(result, a, status)
  mpdQfinalize(result, ctx, status)

##  Like the plus operator.

proc mpdQplus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
  if mpdIszero(a) and ctx.round != mpd_Round_Floor:
    mpdQcopyAbs(result, a, status)
  else:
    mpdQcopy(result, a, status)
  mpdQfinalize(result, ctx, status)

##  The largest representable number that is smaller than the operand.

proc mpdQnextMinus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                   status: ptr uint32T) =
  var workctx: MpdContextT
  var tinyData: array[1, MpdUintT] = [1]
  var tiny: MpdT = [mpd_Pos or mpd_Static or mpd_Const_Data, mpdEtiny(ctx) - 1, 1, 1, 1,
                tinyData]
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    assert(mpdIsinfinite(a))
    if mpdIsnegative(a):
      mpdQcopy(result, a, status)
      return
    else:
      mpdClearFlags(result)
      mpdQmaxcoeff(result, ctx, status)
      if mpdIsnan(result):
        return
      result.exp = mpdEtop(ctx)
      return
  mpdWorkcontext(addr(workctx), ctx)
  workctx.round = mpd_Round_Floor
  if not mpdQcopy(result, a, status):
    return
  mpdQfinalize(result, addr(workctx), addr(workctx.status))
  if workctx.status and (mPD_Inexact or mPD_Errors):
    status[] = status[] or (workctx.status and mPD_Errors)
    return
  workctx.status = 0
  mpdQsub(result, a, addr(tiny), addr(workctx), addr(workctx.status))
  status[] = status[] or (workctx.status and mPD_Errors)

##  The smallest representable number that is larger than the operand.

proc mpdQnextPlus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var workctx: MpdContextT
  var tinyData: array[1, MpdUintT] = [1]
  var tiny: MpdT = [mpd_Pos or mpd_Static or mpd_Const_Data, mpdEtiny(ctx) - 1, 1, 1, 1,
                tinyData]
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    assert(mpdIsinfinite(a))
    if mpdIspositive(a):
      mpdQcopy(result, a, status)
    else:
      mpdClearFlags(result)
      mpdQmaxcoeff(result, ctx, status)
      if mpdIsnan(result):
        return
      mpdSetFlags(result, mpd_Neg)
      result.exp = mpdEtop(ctx)
    return
  mpdWorkcontext(addr(workctx), ctx)
  workctx.round = mpd_Round_Ceiling
  if not mpdQcopy(result, a, status):
    return
  mpdQfinalize(result, addr(workctx), addr(workctx.status))
  if workctx.status and (mPD_Inexact or mPD_Errors):
    status[] = status[] or (workctx.status and mPD_Errors)
    return
  workctx.status = 0
  mpdQadd(result, a, addr(tiny), addr(workctx), addr(workctx.status))
  status[] = status[] or (workctx.status and mPD_Errors)

##
##  The number closest to the first operand that is in the direction towards
##  the second operand.
##

proc mpdQnextToward*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                    status: ptr uint32T) =
  var c: cint
  if mpdQcheckNans(result, a, b, ctx, status):
    return
  c = mpdCmp(a, b)
  if c == 0:
    mpdQcopySign(result, a, b, status)
    return
  if c < 0:
    mpdQnextPlus(result, a, ctx, status)
  else:
    mpdQnextMinus(result, a, ctx, status)
  if mpdIsinfinite(result):
    status[] = status[] or (mPD_Overflow or mPD_Rounded or mPD_Inexact)
  elif mpdAdjexp(result) < ctx.emin:
    status[] = status[] or
        (mPD_Underflow or mPD_Subnormal or mPD_Rounded or mPD_Inexact)
    if mpdIszero(result):
      status[] = status[] or mPD_Clamped

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

proc mpdQpowUint*(result: ptr MpdT; base: ptr MpdT; exp: MpdUintT; resultsign: uint8T;
                 ctx: ptr MpdContextT; status: ptr uint32T) =
  var workstatus: uint32T = 0
  var n: MpdUintT
  if exp == 0:
    settriple(result, resultsign, 1, 0)
    ##  GCOV_NOT_REACHED
    return
    ##  GCOV_NOT_REACHED
  if not mpdQcopy(result, base, status):
    return
  n = mpdBits[mpdBsr(exp)]
  while n = n shr 1:
    mpdQmul(result, result, result, ctx, addr(workstatus))
    if exp and n:
      mpdQmul(result, result, base, ctx, addr(workstatus))
    if mpdIsspecial(result) or
        (mpdIszerocoeff(result) and (workstatus and mPD_Clamped)):
      break
  status[] = status[] or workstatus
  mpdSetSign(result, resultsign)

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

proc mpdQpowMpd*(result: ptr MpdT; tbase: ptr MpdT; texp: ptr MpdT; resultsign: uint8T;
                ctx: ptr MpdContextT; status: ptr uint32T) =
  var workstatus: uint32T = 0
  var maxctx: MpdContextT
  var twoData: array[1, MpdUintT] = [2]
  var two: MpdT = [0 or mpd_Static or mpd_Const_Data, 0, 1, 1, 1, twoData]
  mpdMaxcontext(addr(maxctx))
  ##  resize to smaller cannot fail
  mpdQcopy(result, addr(one), status)
  while not mpdIszero(texp):
    if mpdIsodd(texp):
      mpdQmul(result, result, tbase, ctx, addr(workstatus))
      status[] = status[] or workstatus
      if mpdIsspecial(result) or
          (mpdIszerocoeff(result) and (workstatus and mPD_Clamped)):
        break
    mpdQmul(tbase, tbase, tbase, ctx, addr(workstatus))
    mpdQdivint(texp, texp, addr(two), addr(maxctx), addr(workstatus))
    if mpdIsnan(tbase) or mpdIsnan(texp):
      mpdSeterror(result, workstatus and mPD_Errors, status)
      return
  mpdSetSign(result, resultsign)

##
##  The power function for integer exponents. Relative error _before_ the
##  final rounding to prec:
##    abs(result - base**exp) < 0.1 * 10**-prec * abs(base**exp)
##

proc mpdQpowInt*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; resultsign: uint8T;
                ctx: ptr MpdContextT; status: ptr uint32T) =
  var workctx: MpdContextT
  var tbaseData: array[mpd_Minalloc_Max, MpdUintT]
  var tbase: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tbaseData]
  var texpData: array[mpd_Minalloc_Max, MpdUintT]
  var texp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, texpData]
  var n: MpdUintT
  mpdWorkcontext(addr(workctx), ctx)
  inc(workctx.prec, (exp.digits + exp.exp + 2))
  workctx.round = mpd_Round_Half_Even
  workctx.clamp = 0
  if mpdIsnegative(exp):
    var workstatus: uint32T = 0
    inc(workctx.prec, 1)
    mpdQdiv(addr(tbase), addr(one), base, addr(workctx), addr(workstatus))
    status[] = status[] or workstatus
    if workstatus and mPD_Errors:
      mpdSetspecial(result, mpd_Pos, mpd_Nan)
      break finish
  else:
    if not mpdQcopy(addr(tbase), base, status):
      mpdSetspecial(result, mpd_Pos, mpd_Nan)
      break finish
  n = mpdQabsUint(exp, addr(workctx.status))
  if workctx.status and mPD_InvalidOperation:
    if not mpdQcopy(addr(texp), exp, status):
      mpdSetspecial(result, mpd_Pos, mpd_Nan)
      ##  GCOV_UNLIKELY
      break finish
      ##  GCOV_UNLIKELY
    mpdQpowMpd(result, addr(tbase), addr(texp), resultsign, addr(workctx), status)
  else:
    mpdQpowUint(result, addr(tbase), n, resultsign, addr(workctx), status)
  if mpdIsinfinite(result):
    ##  for ROUND_DOWN, ROUND_FLOOR, etc.
    settriple(result, resultsign, 1, mpd_Exp_Inf)
  mpdDel(addr(tbase))
  mpdDel(addr(texp))
  mpdQfinalize(result, ctx, status)

##
##  If the exponent is infinite and base equals one, the result is one
##  with a coefficient of length prec. Otherwise, result is undefined.
##  Return the value of the comparison against one.
##

proc qcheckPowOneInf*(result: ptr MpdT; base: ptr MpdT; resultsign: uint8T;
                     ctx: ptr MpdContextT; status: ptr uint32T): cint =
  var shift: MpdSsizeT
  var cmp: cint
  if (cmp = mpdCmp(base, addr(one))) == 0:
    shift = ctx.prec - 1
    mpdQshiftl(result, addr(one), shift, status)
    result.exp = -shift
    mpdSetFlags(result, resultsign)
    status[] = status[] or (mPD_Inexact or mPD_Rounded)
  return cmp

##
##  If abs(base) equals one, calculate the correct power of one result.
##  Otherwise, result is undefined. Return the value of the comparison
##  against 1.
##
##  This is an internal function that does not check for specials.
##

proc qcheckPowOne*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; resultsign: uint8T;
                  ctx: ptr MpdContextT; status: ptr uint32T): cint =
  var workstatus: uint32T = 0
  var shift: MpdSsizeT
  var cmp: cint
  if (cmp = mpdCmpAbs(base, addr(one))) == 0:
    if mpdIsint(exp):
      if mpdIsnegative(exp):
        settriple(result, resultsign, 1, 0)
        return 0
      mpdQmulSsize(result, exp, -base.exp, ctx, addr(workstatus))
      if workstatus and mPD_Errors:
        status[] = status[] or (workstatus and mPD_Errors)
        return 0
      shift = mpdQgetSsize(result, addr(workstatus))
      ##  shift is MPD_SSIZE_MAX if result is too large
      if shift > ctx.prec - 1:
        shift = ctx.prec - 1
        status[] = status[] or mPD_Rounded
    elif mpdIspositive(base):
      shift = ctx.prec - 1
      status[] = status[] or (mPD_Inexact or mPD_Rounded)
    else:
      return -2
      ##  GCOV_NOT_REACHED
    if not mpdQshiftl(result, addr(one), shift, status):
      return 0
    result.exp = -shift
    mpdSetFlags(result, resultsign)
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

proc lowerBoundZeta*(x: ptr MpdT; status: ptr uint32T): MpdSsizeT =
  var maxctx: MpdContextT
  var scratchData: array[mpd_Minalloc_Max, MpdUintT]
  var scratch: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max,
                   scratchData]
  var
    t: MpdSsizeT
    u: MpdSsizeT
  t = mpdAdjexp(x)
  if t > 0:
    ##  x >= 10 -> floor(log10(floor(abs(log10(x)))))
    return mpdExpDigits(t) - 1
  elif t < -1:
    ##  x < 1/10 -> floor(log10(floor(abs(log10(x)))))
    return mpdExpDigits(t + 1) - 1
  else:
    mpdMaxcontext(addr(maxctx))
    mpdQsub(addr(scratch), x, addr(one), addr(maxctx), status)
    if mpdIsspecial(addr(scratch)):
      mpdDel(addr(scratch))
      return mpd_Ssize_Max
    u = mpdAdjexp(addr(scratch))
    mpdDel(addr(scratch))
    ##  t == -1, 1/10 <= x < 1 -> floor(log10(abs(x-1)/10))
    ##  t == 0,  1 < x < 10    -> floor(log10(abs(x-1)/100))
    return if (t == 0): u - 2 else: u - 1

##
##  Detect cases of certain overflow/underflow in the power function.
##  Assumptions: x != 1, y != 0. The proof above is for positive x.
##  If x is negative and y is an odd integer, x**y == -(abs(x)**y),
##  so the analysis does not change.
##

proc qcheckPowBounds*(result: ptr MpdT; x: ptr MpdT; y: ptr MpdT; resultsign: uint8T;
                     ctx: ptr MpdContextT; status: ptr uint32T): cint =
  var absX: MpdT = [(x.flags and not mpd_Dataflags) or mpd_Static or mpd_Shared_Data, x.exp,
                x.digits, x.len, x.alloc, x.data]
  var
    ubOmega: MpdSsizeT
    lbZeta: MpdSsizeT
    lbTheta: MpdSsizeT
  var sign: uint8T
  mpdSetPositive(addr(absX))
  lbTheta = mpdAdjexp(y)
  lbZeta = lowerBoundZeta(addr(absX), status)
  if lbZeta == mpd_Ssize_Max:
    mpdSeterror(result, mPD_MallocError, status)
    return 1
  sign = (mpdAdjexp(addr(absX)) < 0) xor mpdSign(y)
  if sign == 0:
    ##  (0 < |x| < 1 and y < 0) or (|x| > 1 and y > 0)
    ubOmega = mpdExpDigits(ctx.emax)
    if ubOmega < lbZeta + lbTheta:
      settriple(result, resultsign, 1, mpd_Exp_Inf)
      mpdQfinalize(result, ctx, status)
      return 1
  else:
    ##  (0 < |x| < 1 and y > 0) or (|x| > 1 and y < 0).
    ubOmega = mpdExpDigits(mpdEtiny(ctx))
    if ubOmega < lbZeta + lbTheta:
      settriple(result, resultsign, 1, mpdEtiny(ctx) - 1)
      mpdQfinalize(result, ctx, status)
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

proc mpdQpowReal*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  var workctx: MpdContextT
  var texpData: array[mpd_Minalloc_Max, MpdUintT]
  var texp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, texpData]
  if not mpdQcopy(addr(texp), exp, status):
    mpdSeterror(result, mPD_MallocError, status)
    return
  mpdMaxcontext(addr(workctx))
  workctx.prec = if (base.digits > ctx.prec): base.digits else: ctx.prec
  inc(workctx.prec, (4 + mpd_Expdigits))
  workctx.round = mpd_Round_Half_Even
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
  mpdQln(result, base, addr(workctx), addr(workctx.status))
  mpdQmul(result, result, addr(texp), addr(workctx), addr(workctx.status))
  mpdQexp(result, result, addr(workctx), status)
  mpdDel(addr(texp))
  status[] = status[] or (workctx.status and mPD_Errors)
  status[] = status[] or (mPD_Inexact or mPD_Rounded)

##  The power function: base**exp

proc mpdQpow*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var resultsign: uint8T = 0
  var intexp: cint = 0
  var cmp: cint
  if mpdIsspecial(base) or mpdIsspecial(exp):
    if mpdQcheckNans(result, base, exp, ctx, status):
      return
  if mpdIsinteger(exp):
    intexp = 1
    resultsign = mpdIsnegative(base) and mpdIsodd(exp)
  if mpdIszero(base):
    if mpdIszero(exp):
      mpdSeterror(result, mPD_InvalidOperation, status)
    elif mpdIsnegative(exp):
      mpdSetspecial(result, resultsign, mpd_Inf)
    else:
      settriple(result, resultsign, 0, 0)
    return
  if mpdIsnegative(base):
    if not intexp or mpdIsinfinite(exp):
      mpdSeterror(result, mPD_InvalidOperation, status)
      return
  if mpdIsinfinite(exp):
    ##  power of one
    cmp = qcheckPowOneInf(result, base, resultsign, ctx, status)
    if cmp == 0:
      return
    else:
      cmp = cmp * mpdArithSign(exp)
      if cmp < 0:
        settriple(result, resultsign, 0, 0)
      else:
        mpdSetspecial(result, resultsign, mpd_Inf)
    return
  if mpdIsinfinite(base):
    if mpdIszero(exp):
      settriple(result, resultsign, 1, 0)
    elif mpdIsnegative(exp):
      settriple(result, resultsign, 0, 0)
    else:
      mpdSetspecial(result, resultsign, mpd_Inf)
    return
  if mpdIszero(exp):
    settriple(result, resultsign, 1, 0)
    return
  if qcheckPowOne(result, base, exp, resultsign, ctx, status) == 0:
    return
  if qcheckPowBounds(result, base, exp, resultsign, ctx, status):
    return
  if intexp:
    mpdQpowInt(result, base, exp, resultsign, ctx, status)
  else:
    mpdQpowReal(result, base, exp, ctx, status)
    if not mpdIsspecial(result) and mpdCmp(result, addr(one)) == 0:
      var shift: MpdSsizeT = ctx.prec - 1
      mpdQshiftl(result, addr(one), shift, status)
      result.exp = -shift
    if mpdIsinfinite(result):
      ##  for ROUND_DOWN, ROUND_FLOOR, etc.
      settriple(result, mpd_Pos, 1, mpd_Exp_Inf)
    mpdQfinalize(result, ctx, status)

##
##  Internal function: Integer powmod with mpd_uint_t exponent, base is modified!
##  Function can fail with MPD_Malloc_error.
##

proc mpdQpowmodUint*(result: ptr MpdT; base: ptr MpdT; exp: MpdUintT; `mod`: ptr MpdT;
                    status: ptr uint32T) =
  var maxcontext: MpdContextT
  mpdMaxcontext(addr(maxcontext))
  ##  resize to smaller cannot fail
  mpdQcopy(result, addr(one), status)
  while exp > 0:
    if exp and 1:
      mpdQmulExact(result, result, base, addr(maxcontext), status)
      mpdQrem(result, result, `mod`, addr(maxcontext), status)
    mpdQmulExact(base, base, base, addr(maxcontext), status)
    mpdQrem(base, base, `mod`, addr(maxcontext), status)
    exp = exp shr 1

##  The powmod function: (base**exp) % mod

proc mpdQpowmod*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; `mod`: ptr MpdT;
                ctx: ptr MpdContextT; status: ptr uint32T) =
  var maxcontext: MpdContextT
  var tbaseData: array[mpd_Minalloc_Max, MpdUintT]
  var tbase: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tbaseData]
  var texpData: array[mpd_Minalloc_Max, MpdUintT]
  var texp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, texpData]
  var tmodData: array[mpd_Minalloc_Max, MpdUintT]
  var tmod: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tmodData]
  var tmpData: array[mpd_Minalloc_Max, MpdUintT]
  var tmp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tmpData]
  var twoData: array[1, MpdUintT] = [2]
  var two: MpdT = [0 or mpd_Static or mpd_Const_Data, 0, 1, 1, 1, twoData]
  var
    tbaseExp: MpdSsizeT
    texpExp: MpdSsizeT
  var i: MpdSsizeT
  var t: MpdT
  var r: MpdUintT
  var sign: uint8T
  if mpdIsspecial(base) or mpdIsspecial(exp) or mpdIsspecial(`mod`):
    if mpdQcheck3nans(result, base, exp, `mod`, ctx, status):
      return
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if not mpdIsint(base) or not mpdIsint(exp) or not mpdIsint(`mod`):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if mpdIszerocoeff(`mod`):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if `mod`.digits + `mod`.exp > ctx.prec:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  sign = (mpdIsnegative(base)) and (mpdIsodd(exp))
  if mpdIszerocoeff(exp):
    if mpdIszerocoeff(base):
      mpdSeterror(result, mPD_InvalidOperation, status)
      return
    r = if (mpdCmpAbs(`mod`, addr(one)) == 0): 0 else: 1
    settriple(result, sign, r, 0)
    return
  if mpdIsnegative(exp):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if mpdIszerocoeff(base):
    settriple(result, sign, 0, 0)
    return
  mpdMaxcontext(addr(maxcontext))
  mpdQrescale(addr(tmod), `mod`, 0, addr(maxcontext), addr(maxcontext.status))
  if maxcontext.status and mPD_Errors:
    mpdSeterror(result, maxcontext.status and mPD_Errors, status)
    break `out`
  maxcontext.status = 0
  mpdSetPositive(addr(tmod))
  mpdQroundToInt(addr(tbase), base, addr(maxcontext), status)
  mpdSetPositive(addr(tbase))
  tbaseExp = tbase.exp
  tbase.exp = 0
  mpdQroundToInt(addr(texp), exp, addr(maxcontext), status)
  texpExp = texp.exp
  texp.exp = 0
  ##  base = (base.int % modulo * pow(10, base.exp, modulo)) % modulo
  mpdQrem(addr(tbase), addr(tbase), addr(tmod), addr(maxcontext), status)
  mpdQshiftl(result, addr(one), tbaseExp, status)
  mpdQrem(result, result, addr(tmod), addr(maxcontext), status)
  mpdQmulExact(addr(tbase), addr(tbase), result, addr(maxcontext), status)
  mpdQrem(addr(tbase), addr(tbase), addr(tmod), addr(maxcontext), status)
  if mpdIsspecial(addr(tbase)) or mpdIsspecial(addr(texp)) or
      mpdIsspecial(addr(tmod)):
    break mpdErrors
  i = 0
  while i < texpExp:
    mpdQpowmodUint(addr(tmp), addr(tbase), 10, addr(tmod), status)
    t = tmp
    tmp = tbase
    tbase = t
    inc(i)
  if mpdIsspecial(addr(tbase)):
    break mpdErrors
    ##  GCOV_UNLIKELY
  mpdQcopy(result, addr(one), status)
  while mpdIsfinite(addr(texp)) and not mpdIszero(addr(texp)):
    if mpdIsodd(addr(texp)):
      mpdQmulExact(result, result, addr(tbase), addr(maxcontext), status)
      mpdQrem(result, result, addr(tmod), addr(maxcontext), status)
    mpdQmulExact(addr(tbase), addr(tbase), addr(tbase), addr(maxcontext), status)
    mpdQrem(addr(tbase), addr(tbase), addr(tmod), addr(maxcontext), status)
    mpdQdivint(addr(texp), addr(texp), addr(two), addr(maxcontext), status)
  if mpdIsspecial(addr(texp)) or mpdIsspecial(addr(tbase)) or
      mpdIsspecial(addr(tmod)) or mpdIsspecial(result):
    ##  MPD_Malloc_error
    break mpdErrors
  else:
    mpdSetSign(result, sign)
  mpdDel(addr(tbase))
  mpdDel(addr(texp))
  mpdDel(addr(tmod))
  mpdDel(addr(tmp))
  return
  mpdSetspecial(result, mpd_Pos, mpd_Nan)
  break `out`

proc mpdQquantize*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T) =
  var workstatus: uint32T = 0
  var bExp: MpdSsizeT = b.exp
  var
    expdiff: MpdSsizeT
    shift: MpdSsizeT
  var rnd: MpdUintT
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(result, a, b, ctx, status):
      return
    if mpdIsinfinite(a) and mpdIsinfinite(b):
      mpdQcopy(result, a, status)
      return
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if b.exp > ctx.emax or b.exp < mpdEtiny(ctx):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if mpdIszero(a):
    settriple(result, mpdSign(a), 0, b.exp)
    mpdQfinalize(result, ctx, status)
    return
  expdiff = a.exp - b.exp
  if a.digits + expdiff > ctx.prec:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if expdiff >= 0:
    shift = expdiff
    if not mpdQshiftl(result, a, shift, status):
      return
    result.exp = bExp
  else:
    ##  At this point expdiff < 0 and a->digits+expdiff <= prec,
    ##  so the shift before an increment will fit in prec.
    shift = -expdiff
    rnd = mpdQshiftr(result, a, shift, status)
    if rnd == mpd_Uint_Max:
      return
    result.exp = bExp
    if not mpdApplyRoundFit(result, rnd, ctx, status):
      return
    workstatus = workstatus or mPD_Rounded
    if rnd:
      workstatus = workstatus or mPD_Inexact
  if mpdAdjexp(result) > ctx.emax or mpdAdjexp(result) < mpdEtiny(ctx):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  status[] = status[] or workstatus
  mpdQfinalize(result, ctx, status)

proc mpdQreduce*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var
    shift: MpdSsizeT
    maxexp: MpdSsizeT
    maxshift: MpdSsizeT
  var signA: uint8T = mpdSign(a)
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    mpdQcopy(result, a, status)
    return
  if not mpdQcopy(result, a, status):
    return
  mpdQfinalize(result, ctx, status)
  if mpdIsspecial(result):
    return
  if mpdIszero(result):
    settriple(result, signA, 0, 0)
    return
  shift = mpdTrailZeros(result)
  maxexp = if (ctx.clamp): mpdEtop(ctx) else: ctx.emax
  ##  After the finalizing above result->exp <= maxexp.
  maxshift = maxexp - result.exp
  shift = if (shift > maxshift): maxshift else: shift
  mpdQshiftrInplace(result, shift)
  inc(result.exp, shift)

proc mpdQrem*(r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T) =
  var qData: array[mpd_Minalloc_Max, MpdUintT]
  var q: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, qData]
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(r, a, b, ctx, status):
      return
    if mpdIsinfinite(a):
      mpdSeterror(r, mPD_InvalidOperation, status)
      return
    if mpdIsinfinite(b):
      mpdQcopy(r, a, status)
      mpdQfinalize(r, ctx, status)
      return
    abort()
    ##  GCOV_NOT_REACHED
  if mpdIszerocoeff(b):
    if mpdIszerocoeff(a):
      mpdSeterror(r, mPD_DivisionUndefined, status)
    else:
      mpdSeterror(r, mPD_InvalidOperation, status)
    return
  mpdQdivmod(addr(q), r, a, b, ctx, status)
  mpdDel(addr(q))
  mpdQfinalize(r, ctx, status)

proc mpdQremNear*(r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  var workctx: MpdContextT
  var btmpData: array[mpd_Minalloc_Max, MpdUintT]
  var btmp: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, btmpData]
  var qData: array[mpd_Minalloc_Max, MpdUintT]
  var q: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, qData]
  var
    expdiff: MpdSsizeT
    qdigits: MpdSsizeT
  var
    cmp: cint
    isodd: cint
    allnine: cint
  assert(r != nil)
  ##  annotation for scan-build
  if mpdIsspecial(a) or mpdIsspecial(b):
    if mpdQcheckNans(r, a, b, ctx, status):
      return
    if mpdIsinfinite(a):
      mpdSeterror(r, mPD_InvalidOperation, status)
      return
    if mpdIsinfinite(b):
      mpdQcopy(r, a, status)
      mpdQfinalize(r, ctx, status)
      return
    abort()
    ##  GCOV_NOT_REACHED
  if mpdIszerocoeff(b):
    if mpdIszerocoeff(a):
      mpdSeterror(r, mPD_DivisionUndefined, status)
    else:
      mpdSeterror(r, mPD_InvalidOperation, status)
    return
  if r == b:
    if not mpdQcopy(addr(btmp), b, status):
      mpdSeterror(r, mPD_MallocError, status)
      return
    b = addr(btmp)
  mpdQdivmod(addr(q), r, a, b, ctx, status)
  if mpdIsnan(addr(q)) or mpdIsnan(r):
    break finish
  if mpdIszerocoeff(r):
    break finish
  expdiff = mpdAdjexp(b) - mpdAdjexp(r)
  if -1 <= expdiff and expdiff <= 1:
    allnine = mpdCoeffIsallnine(addr(q))
    qdigits = q.digits
    isodd = mpdIsodd(addr(q))
    mpdMaxcontext(addr(workctx))
    if mpdSign(a) == mpdSign(b):
      ##  sign(r) == sign(b)
      mpdQsub(addr(q), r, b, addr(workctx), addr(workctx.status))
    else:
      ##  sign(r) != sign(b)
      mpdQadd(addr(q), r, b, addr(workctx), addr(workctx.status))
    if workctx.status and mPD_Errors:
      mpdSeterror(r, workctx.status and mPD_Errors, status)
      break finish
    cmp = mpdCmpAbs(addr(q), r)
    if cmp < 0 or (cmp == 0 and isodd):
      ##  abs(r) > abs(b)/2 or abs(r) == abs(b)/2 and isodd(quotient)
      if allnine and qdigits == ctx.prec:
        ##  abs(quotient) + 1 == 10**prec
        mpdSeterror(r, mPD_DivisionImpossible, status)
        break finish
      mpdQcopy(r, addr(q), status)
  mpdDel(addr(btmp))
  mpdDel(addr(q))
  mpdQfinalize(r, ctx, status)

proc mpdQrescale*(result: ptr MpdT; a: ptr MpdT; exp: MpdSsizeT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  var
    expdiff: MpdSsizeT
    shift: MpdSsizeT
  var rnd: MpdUintT
  if mpdIsspecial(a):
    mpdQcopy(result, a, status)
    return
  if mpdIszero(a):
    settriple(result, mpdSign(a), 0, exp)
    return
  expdiff = a.exp - exp
  if expdiff >= 0:
    shift = expdiff
    if a.digits + shift > mpd_Max_Prec + 1:
      mpdSeterror(result, mPD_InvalidOperation, status)
      return
    if not mpdQshiftl(result, a, shift, status):
      return
    result.exp = exp
  else:
    shift = -expdiff
    rnd = mpdQshiftr(result, a, shift, status)
    if rnd == mpd_Uint_Max:
      return
    result.exp = exp
    mpdApplyRoundExcess(result, rnd, ctx, status)
    status[] = status[] or mPD_Rounded
    if rnd:
      status[] = status[] or mPD_Inexact
  if mpdIssubnormal(result, ctx):
    status[] = status[] or mPD_Subnormal

##
##  Rescale a number so that it has exponent 'exp'. Does not regard context
##  precision, emax, emin, but uses the rounding mode. Special numbers are
##  quietly copied. Restrictions:
##
##      MPD_MIN_ETINY <= exp <= MPD_MAX_EMAX+1
##      result->digits <= MPD_MAX_PREC+1
##

proc mpdQrescale*(result: ptr MpdT; a: ptr MpdT; exp: MpdSsizeT; ctx: ptr MpdContextT;
                 status: ptr uint32T) =
  if exp > mpd_Max_Emax + 1 or exp < mpd_Min_Etiny:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  mpdQrescale(result, a, exp, ctx, status)

##
##  Same as mpd_qrescale, but with relaxed restrictions. The result of this
##  function should only be used for formatting a number and never as input
##  for other operations.
##
##      MPD_MIN_ETINY-MPD_MAX_PREC <= exp <= MPD_MAX_EMAX+1
##      result->digits <= MPD_MAX_PREC+1
##

proc mpdQrescaleFmt*(result: ptr MpdT; a: ptr MpdT; exp: MpdSsizeT; ctx: ptr MpdContextT;
                    status: ptr uint32T) =
  if exp > mpd_Max_Emax + 1 or exp < mpd_Min_Etiny - mpd_Max_Prec:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  mpdQrescale(result, a, exp, ctx, status)

##  Round to an integer according to 'action' and ctx->round.

const
  TO_INT_EXACT* = 0
  TO_INT_SILENT* = 1
  TO_INT_TRUNC* = 2

proc mpdQroundToIntegral*(action: cint; result: ptr MpdT; a: ptr MpdT;
                         ctx: ptr MpdContextT; status: ptr uint32T) =
  var rnd: MpdUintT
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    mpdQcopy(result, a, status)
    return
  if a.exp >= 0:
    mpdQcopy(result, a, status)
    return
  if mpdIszerocoeff(a):
    settriple(result, mpdSign(a), 0, 0)
    return
  rnd = mpdQshiftr(result, a, -a.exp, status)
  if rnd == mpd_Uint_Max:
    return
  result.exp = 0
  if action == to_Int_Exact or action == to_Int_Silent:
    mpdApplyRoundExcess(result, rnd, ctx, status)
    if action == to_Int_Exact:
      status[] = status[] or mPD_Rounded
      if rnd:
        status[] = status[] or mPD_Inexact

proc mpdQroundToIntx*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                     status: ptr uint32T) =
  cast[nil](mpdQroundToIntegral(to_Int_Exact, result, a, ctx, status))

proc mpdQroundToInt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                    status: ptr uint32T) =
  cast[nil](mpdQroundToIntegral(to_Int_Silent, result, a, ctx, status))

proc mpdQtrunc*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  if mpdIsspecial(a):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  cast[nil](mpdQroundToIntegral(to_Int_Trunc, result, a, ctx, status))

proc mpdQfloor*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var workctx: MpdContextT = ctx[]
  if mpdIsspecial(a):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  workctx.round = mpd_Round_Floor
  cast[nil](mpdQroundToIntegral(to_Int_Silent, result, a, addr(workctx), status))

proc mpdQceil*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var workctx: MpdContextT = ctx[]
  if mpdIsspecial(a):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  workctx.round = mpd_Round_Ceiling
  cast[nil](mpdQroundToIntegral(to_Int_Silent, result, a, addr(workctx), status))

proc mpdSameQuantum*(a: ptr MpdT; b: ptr MpdT): cint =
  if mpdIsspecial(a) or mpdIsspecial(b):
    return (mpdIsnan(a) and mpdIsnan(b)) or
        (mpdIsinfinite(a) and mpdIsinfinite(b))
  return a.exp == b.exp

##  Schedule the increase in precision for the Newton iteration.

proc recprSchedulePrec*(klist: array[mpd_Max_Prec_Log2, MpdSsizeT];
                       maxprec: MpdSsizeT; initprec: MpdSsizeT): cint =
  var k: MpdSsizeT
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

proc mpdQreciprocalApprox*(z: ptr MpdT; v: ptr MpdT; status: ptr uint32T) =
  var p10data: array[2, MpdUintT] = [0, mpdPow10[mpd_Rdigits - 2]]
  var
    dummy: MpdUintT
    word: MpdUintT
  var n: cint
  assert(v.exp == -v.digits)
  mpdGetMsdigits(addr(dummy), addr(word), v, mpd_Rdigits)
  n = mpdWordDigits(word)
  word = word * mpdPow10[mpd_Rdigits - n]
  mpdQresize(z, 2, status)
  cast[nil](mpdShortdiv(z.data, p10data, 2, word))
  mpdClearFlags(z)
  z.exp = -(mpd_Rdigits - 2)
  z.len = if (z.data[1] == 0): 1 else: 2
  mpdSetdigits(z)

##
##  Reciprocal, calculated with Newton's Method. Assumption: result != a.
##  NOTE: The comments in the function show that certain operations are
##  exact. The proof for the maximum error is too long to fit in here.
##  ACL2 proof: maxerror-inverse-complete
##

proc mpdQreciprocal*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                    status: ptr uint32T) =
  var
    varcontext: MpdContextT
    maxcontext: MpdContextT
  var z: ptr MpdT = result
  ##  current approximation
  var v: ptr MpdT
  ##  a, normalized to a number between 0.1 and 1
  var vtmp: MpdT = [(a.flags and not mpd_Dataflags) or mpd_Static or mpd_Shared_Data, a.exp,
                a.digits, a.len, a.alloc, a.data]
  ##  v shares data with a
  var sData: array[mpd_Minalloc_Max, MpdUintT]
  var s: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, sData]
  ##  temporary variable
  var tData: array[mpd_Minalloc_Max, MpdUintT]
  var t: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tData]
  ##  temporary variable
  var twoData: array[1, MpdUintT] = [2]
  var two: MpdT = [0 or mpd_Static or mpd_Const_Data, 0, 1, 1, 1, twoData]
  ##  const 2
  var klist: array[mpd_Max_Prec_Log2, MpdSsizeT]
  var
    adj: MpdSsizeT
    maxprec: MpdSsizeT
    initprec: MpdSsizeT
  var sign: uint8T = mpdSign(a)
  var i: cint
  assert(result != a)
  v = addr(vtmp)
  mpdClearFlags(v)
  adj = v.digits + v.exp
  v.exp = -v.digits
  ##  Initial approximation
  mpdQreciprocalApprox(z, v, status)
  mpdMaxcontext(addr(varcontext))
  mpdMaxcontext(addr(maxcontext))
  varcontext.round = maxcontext.round = mpd_Round_Trunc
  varcontext.emax = maxcontext.emax = mpd_Max_Emax + 100
  varcontext.emin = maxcontext.emin = mpd_Min_Emin - 100
  maxcontext.prec = mpd_Max_Prec + 100
  maxprec = ctx.prec
  inc(maxprec, 2)
  initprec = mpd_Rdigits - 3
  i = recprSchedulePrec(klist, maxprec, initprec)
  while i >= 0:
    ##  Loop invariant: z->digits <= klist[i]+7
    ##  Let s := z**2, exact result
    mpdQmulExact(addr(s), z, z, addr(maxcontext), status)
    varcontext.prec = 2 * klist[i] + 5
    if v.digits > varcontext.prec:
      ##  Let t := v, truncated to n >= 2*k+5 fraction digits
      mpdQshiftr(addr(t), v, v.digits - varcontext.prec, status)
      t.exp = -varcontext.prec
      ##  Let t := trunc(v)*s, truncated to n >= 2*k+1 fraction digits
      mpdQmul(addr(t), addr(t), addr(s), addr(varcontext), status)
    else:
      ##  v->digits <= 2*k+5
      ##  Let t := v*s, truncated to n >= 2*k+1 fraction digits
      mpdQmul(addr(t), v, addr(s), addr(varcontext), status)
    ##  Let s := 2*z, exact result
    mpdQmulExact(addr(s), z, addr(two), addr(maxcontext), status)
    ##  s.digits < t.digits <= 2*k+5, |adjexp(s)-adjexp(t)| <= 1,
    ##  so the subtraction generates at most 2*k+6 <= klist[i+1]+7
    ##  digits. The loop invariant is preserved.
    mpdQsubExact(z, addr(s), addr(t), addr(maxcontext), status)
    dec(i)
  if not mpdIsspecial(z):
    dec(z.exp, adj)
    mpdSetFlags(z, sign)
  mpdDel(addr(s))
  mpdDel(addr(t))
  mpdQfinalize(z, ctx, status)

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

proc mpdBaseNdivmod*(q: ptr MpdT; r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; status: ptr uint32T) =
  var workctx: MpdContextT
  var
    qq: ptr MpdT = q
    rr: ptr MpdT = r
  var
    aa: MpdT
    bb: MpdT
  var k: cint
  mpdCopyShared(addr(aa), a)
  mpdCopyShared(addr(bb), b)
  mpdSetPositive(addr(aa))
  mpdSetPositive(addr(bb))
  aa.exp = 0
  bb.exp = 0
  if q == a or q == b:
    if (qq = mpdQnew()) == nil:
      status[] = status[] or mPD_MallocError
      break nanresult
  if r == a or r == b:
    if (rr = mpdQnew()) == nil:
      status[] = status[] or mPD_MallocError
      break nanresult
  mpdMaxcontext(addr(workctx))
  ##  Let prec := adigits - bdigits + 4
  workctx.prec = a.digits - b.digits + 1 + 3
  if a.digits > mpd_Max_Prec or workctx.prec > mpd_Max_Prec:
    status[] = status[] or mPD_DivisionImpossible
    break nanresult
  mpdQreciprocal(rr, addr(bb), addr(workctx), addr(workctx.status))
  ##  Get an estimate for the quotient. Let q := a * x
  ##  Then q is bounded by:
  ##     3) a/b - 10**-4 < q < a/b + 10**-4
  ##
  mpdQmul(qq, addr(aa), rr, addr(workctx), addr(workctx.status))
  ##  Truncate q to an integer:
  ##     4) a/b - 2 < trunc(q) < a/b + 1
  ##
  mpdQtrunc(qq, qq, addr(workctx), addr(workctx.status))
  workctx.prec = aa.digits + 3
  workctx.emax = mpd_Max_Emax + 3
  workctx.emin = mpd_Min_Emin - 3
  ##  Multiply the estimate for q by b:
  ##     5) a - 2 * b < trunc(q) * b < a + b
  ##
  mpdQmul(rr, addr(bb), qq, addr(workctx), addr(workctx.status))
  ##  Get the estimate for r such that a = q * b + r.
  mpdQsubExact(rr, addr(aa), rr, addr(workctx), addr(workctx.status))
  ##  Fix the result. At this point -b < r < 2*b, so the correction loop
  ##        takes at most one iteration.
  k = 0
  while true:
    if mpdIsspecial(qq) or mpdIsspecial(rr):
      status[] = status[] or (workctx.status and mPD_Errors)
      break nanresult
    if k > 2:
      ##  Allow two iterations despite the proof.
      mpdErrWarn("libmpdec: internal error in _mpd_base_ndivmod: please report")
      ##  GCOV_NOT_REACHED
      status[] = status[] or mPD_InvalidOperation
      ##  GCOV_NOT_REACHED
      break nanresult
      ##  GCOV_NOT_REACHED
    elif mpdCmp(addr(zero), rr) == 1: ##  0 <= r < b
      mpdQaddExact(rr, rr, addr(bb), addr(workctx), addr(workctx.status))
      mpdQaddExact(qq, qq, addr(minusOne), addr(workctx), addr(workctx.status))
    elif mpdCmp(rr, addr(bb)) == -1: ##  r >= b
      break
    else:
      mpdQsubExact(rr, rr, addr(bb), addr(workctx), addr(workctx.status))
      mpdQaddExact(qq, qq, addr(one), addr(workctx), addr(workctx.status))
    inc(k)
  if qq != q:
    if not mpdQcopy(q, qq, status):
      break nanresult
      ##  GCOV_UNLIKELY
    mpdDel(qq)
  if rr != r:
    if not mpdQcopy(r, rr, status):
      break nanresult
      ##  GCOV_UNLIKELY
    mpdDel(rr)
  status[] = status[] or (workctx.status and mPD_Errors)
  return
  if qq and qq != q:
    mpdDel(qq)
  if rr and rr != r:
    mpdDel(rr)
  mpdSetspecial(q, mpd_Pos, mpd_Nan)
  mpdSetspecial(r, mpd_Pos, mpd_Nan)

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

proc invrootSchedulePrec*(klist: array[mpd_Max_Prec_Log2, MpdSsizeT];
                         maxprec: MpdSsizeT; initprec: MpdSsizeT): cint =
  var k: MpdSsizeT
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

proc invrootInitApprox*(z: ptr MpdT; vhat: MpdUintT) =
  var lo: MpdUintT = 1000
  var hi: MpdUintT = 10000
  var
    a: MpdUintT
    sq: MpdUintT
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
  mpdMinalloc(z)
  mpdClearFlags(z)
  z.data[0] = 1000000000 div a
  z.len = 1
  z.exp = -6
  mpdSetdigits(z)

##
##  Set 'result' to 1/sqrt(a).
##    Relative error: abs(result - 1/sqrt(a)) < 10**-prec * 1/sqrt(a)
##

proc mpdQinvroot*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var workstatus: uint32T = 0
  var
    varcontext: MpdContextT
    maxcontext: MpdContextT
  var z: ptr MpdT = result
  ##  current approximation
  var v: ptr MpdT
  ##  a, normalized to a number between 1 and 100
  var vtmp: MpdT = [(a.flags and not mpd_Dataflags) or mpd_Static or mpd_Shared_Data, a.exp,
                a.digits, a.len, a.alloc, a.data]
  ##  by default v will share data with a
  var sData: array[mpd_Minalloc_Max, MpdUintT]
  var s: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, sData]
  ##  temporary variable
  var tData: array[mpd_Minalloc_Max, MpdUintT]
  var t: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tData]
  ##  temporary variable
  var oneHalfData: array[1, MpdUintT] = [5]
  var oneHalf: MpdT = [0 or mpd_Static or mpd_Const_Data, -1, 1, 1, 1, oneHalfData]
  var threeData: array[1, MpdUintT] = [3]
  var three: MpdT = [0 or mpd_Static or mpd_Const_Data, 0, 1, 1, 1, threeData]
  var klist: array[mpd_Max_Prec_Log2, MpdSsizeT]
  var
    idealExp: MpdSsizeT
    shift: MpdSsizeT
  var
    adj: MpdSsizeT
    tz: MpdSsizeT
  var
    maxprec: MpdSsizeT
    fracdigits: MpdSsizeT
  var
    vhat: MpdUintT
    dummy: MpdUintT
  var
    i: cint
    n: cint
  idealExp = -((a.exp - (a.exp and 1)) div 2)
  v = addr(vtmp)
  if result == a:
    if (v = mpdQncopy(a)) == nil:
      mpdSeterror(result, mPD_MallocError, status)
      return
  if (v.digits + v.exp) and 1:
    fracdigits = v.digits - 1
    v.exp = -fracdigits
    n = if (v.digits > 7): 7 else: cast[cint](v.digits)
    ##  Let vhat := floor(v * 10**(2*initprec))
    mpdGetMsdigits(addr(dummy), addr(vhat), v, n)
    if n < 7:
      vhat = vhat * mpdPow10[7 - n]
  else:
    fracdigits = v.digits - 2
    v.exp = -fracdigits
    n = if (v.digits > 8): 8 else: cast[cint](v.digits)
    ##  Let vhat := floor(v * 10**(2*initprec))
    mpdGetMsdigits(addr(dummy), addr(vhat), v, n)
    if n < 8:
      vhat = vhat * mpdPow10[8 - n]
  adj = (a.exp - v.exp) div 2
  ##  initial approximation
  invrootInitApprox(z, vhat)
  mpdMaxcontext(addr(maxcontext))
  mpdMaxcontext(addr(varcontext))
  varcontext.round = mpd_Round_Trunc
  maxprec = ctx.prec + 1
  ##  initprec == 3
  i = invrootSchedulePrec(klist, maxprec, 3)
  while i >= 0:
    varcontext.prec = 2 * klist[i] + 2
    mpdQmul(addr(s), z, z, addr(maxcontext), addr(workstatus))
    if v.digits > varcontext.prec:
      shift = v.digits - varcontext.prec
      mpdQshiftr(addr(t), v, shift, addr(workstatus))
      inc(t.exp, shift)
      mpdQmul(addr(t), addr(t), addr(s), addr(varcontext), addr(workstatus))
    else:
      mpdQmul(addr(t), v, addr(s), addr(varcontext), addr(workstatus))
    mpdQsub(addr(t), addr(three), addr(t), addr(maxcontext), addr(workstatus))
    mpdQmul(z, z, addr(t), addr(varcontext), addr(workstatus))
    mpdQmul(z, z, addr(oneHalf), addr(maxcontext), addr(workstatus))
    dec(i)
  dec(z.exp, adj)
  tz = mpdTrailZeros(result)
  shift = idealExp - result.exp
  shift = if (tz > shift): shift else: tz
  if shift > 0:
    mpdQshiftrInplace(result, shift)
    inc(result.exp, shift)
  mpdDel(addr(s))
  mpdDel(addr(t))
  if v != addr(vtmp):
    mpdDel(v)
  status[] = status[] or (workstatus and mPD_Errors)
  status[] = status[] or (mPD_Rounded or mPD_Inexact)

proc mpdQinvroot*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var workctx: MpdContextT
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    if mpdIsnegative(a):
      mpdSeterror(result, mPD_InvalidOperation, status)
      return
    settriple(result, mpd_Pos, 0, mpdEtiny(ctx))
    status[] = status[] or mPD_Clamped
    return
  if mpdIszero(a):
    mpdSetspecial(result, mpdSign(a), mpd_Inf)
    status[] = status[] or mPD_DivisionByZero
    return
  if mpdIsnegative(a):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  workctx = ctx[]
  inc(workctx.prec, 2)
  workctx.round = mpd_Round_Half_Even
  mpdQinvroot(result, a, addr(workctx), status)
  mpdQfinalize(result, ctx, status)

##  END LIBMPDEC_ONLY
##  Algorithm from decimal.py

proc mpdQsqrt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var maxcontext: MpdContextT
  var cData: array[mpd_Minalloc_Max, MpdUintT]
  var c: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, cData]
  var qData: array[mpd_Minalloc_Max, MpdUintT]
  var q: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, qData]
  var rData: array[mpd_Minalloc_Max, MpdUintT]
  var r: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, rData]
  var twoData: array[1, MpdUintT] = [2]
  var two: MpdT = [0 or mpd_Static or mpd_Const_Data, 0, 1, 1, 1, twoData]
  var
    prec: MpdSsizeT
    idealExp: MpdSsizeT
  var
    l: MpdSsizeT
    shift: MpdSsizeT
  var exact: cint = 0
  idealExp = (a.exp - (a.exp and 1)) div 2
  if mpdIsspecial(a):
    if mpdQcheckNan(result, a, ctx, status):
      return
    if mpdIsnegative(a):
      mpdSeterror(result, mPD_InvalidOperation, status)
      return
    mpdSetspecial(result, mpd_Pos, mpd_Inf)
    return
  if mpdIszero(a):
    settriple(result, mpdSign(a), 0, idealExp)
    mpdQfinalize(result, ctx, status)
    return
  if mpdIsnegative(a):
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  mpdMaxcontext(addr(maxcontext))
  prec = ctx.prec + 1
  if not mpdQcopy(addr(c), a, status):
    break mallocError
  c.exp = 0
  if a.exp and 1:
    if not mpdQshiftl(addr(c), addr(c), 1, status):
      break mallocError
    l = (a.digits shr 1) + 1
  else:
    l = (a.digits + 1) shr 1
  shift = prec - l
  if shift >= 0:
    if not mpdQshiftl(addr(c), addr(c), 2 * shift, status):
      break mallocError
    exact = 1
  else:
    exact = not mpdQshiftrInplace(addr(c), -(2 * shift))
  dec(idealExp, shift)
  ##  find result = floor(sqrt(c)) using Newton's method
  if not mpdQshiftl(result, addr(one), prec, status):
    break mallocError
  while 1:
    mpdQdivmod(addr(q), addr(r), addr(c), result, addr(maxcontext),
               addr(maxcontext.status))
    if mpdIsspecial(result) or mpdIsspecial(addr(q)):
      mpdSeterror(result, maxcontext.status and mPD_Errors, status)
      break `out`
    if mpdCmp(result, addr(q)) <= 0:
      break
    mpdQaddExact(result, result, addr(q), addr(maxcontext), addr(maxcontext.status))
    if mpdIsspecial(result):
      mpdSeterror(result, maxcontext.status and mPD_Errors, status)
      break `out`
    mpdQdivmod(result, addr(r), result, addr(two), addr(maxcontext),
               addr(maxcontext.status))
  if exact:
    mpdQmulExact(addr(r), result, result, addr(maxcontext), addr(maxcontext.status))
    if mpdIsspecial(addr(r)):
      mpdSeterror(result, maxcontext.status and mPD_Errors, status)
      break `out`
    exact = (mpdCmp(addr(r), addr(c)) == 0)
  if exact:
    if shift >= 0:
      mpdQshiftrInplace(result, shift)
    else:
      if not mpdQshiftl(result, result, -shift, status):
        break mallocError
    inc(idealExp, shift)
  else:
    var lsd: cint = cast[cint](mpdLsd(result.data[0]))
    if lsd == 0 or lsd == 5:
      inc(result.data[0], 1)
  result.exp = idealExp
  mpdDel(addr(c))
  mpdDel(addr(q))
  mpdDel(addr(r))
  maxcontext = ctx[]
  maxcontext.round = mpd_Round_Half_Even
  mpdQfinalize(result, addr(maxcontext), status)
  return
  mpdSeterror(result, mPD_MallocError, status)
  break `out`

proc mpdQsqrt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T) =
  var aaData: array[mpd_Minalloc_Max, MpdUintT]
  var aa: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, aaData]
  var xstatus: uint32T = 0
  if result == a:
    if not mpdQcopy(addr(aa), a, status):
      mpdSeterror(result, mPD_MallocError, status)
      break `out`
    a = addr(aa)
  mpdQsqrt(result, a, ctx, addr(xstatus))
  if xstatus and (mPD_MallocError or mPD_DivisionImpossible):
    ##  The above conditions can occur at very high context precisions
    ##  if intermediate values get too large. Retry the operation with
    ##  a lower context precision in case the result is exact.
    ##
    ##  If the result is exact, an upper bound for the number of digits
    ##  is the number of digits in the input.
    ##
    ##  NOTE: sqrt(40e9) = 2.0e+5 /\ digits(40e9) = digits(2.0e+5) = 2
    ##
    var ystatus: uint32T = 0
    var workctx: MpdContextT = ctx[]
    workctx.prec = a.digits
    if workctx.prec >= ctx.prec:
      status[] = status[] or (xstatus or mPD_Errors)
      break `out`
      ##  No point in repeating this, keep the original error.
    mpdQsqrt(result, a, addr(workctx), addr(ystatus))
    if ystatus != 0:
      ystatus = status[] or ((xstatus or ystatus) and mPD_Errors)
      mpdSeterror(result, ystatus, status)
  else:
    status[] = status[] or xstatus
  mpdDel(addr(aa))

## ****************************************************************************
##                               Base conversions
## ****************************************************************************
##  Space needed to represent an integer mpd_t in base 'base'.

proc mpdSizeinbase*(a: ptr MpdT; base: uint32T): csize =
  var x: cdouble
  var digits: csize
  var upperBound: cdouble
  assert(mpdIsinteger(a))
  assert(base >= 2)
  if mpdIszero(a):
    return 1
  digits = a.digits + a.exp
  when defined(CONFIG_64):
    ##  ceil(2711437152599294 / log10(2)) + 4 == 2**53
    if digits > 2711437152599294'i64:
      return size_Max
    upperBound = (double)((1 shl 53) - 1)
  else:
    upperBound = (double)(size_Max - 1)
  x = cast[cdouble](digits div log10(base))
  return if (x > upperBound): size_Max else: cast[csize](x) + 1

##  Space needed to import a base 'base' integer of length 'srclen'.

proc mpdImportsize*(srclen: csize; base: uint32T): MpdSsizeT =
  var x: cdouble
  var upperBound: cdouble
  assert(srclen > 0)
  assert(base >= 2)
  when size_Max == uint64Max:
    if srclen > (1 shl 53):
      return mpd_Ssize_Max
    assert((1 shl 53) <= mpd_Maximport)
    upperBound = (double)((1 shl 53) - 1)
  else:
    upperBound = mpd_Maximport - 1
  x = cast[cdouble](srclen * (log10(base) div mpd_Rdigits))
  return if (x > upperBound): mpd_Ssize_Max else: cast[MpdSsizeT](x) + 1

proc mpdResizeU16*(w: ptr ptr uint16T; nmemb: csize): uint8T =
  var err: uint8T = 0
  w[] = mpdRealloc(w[], nmemb, sizeof(w[][]), addr(err))
  return not err

proc mpdResizeU32*(w: ptr ptr uint32T; nmemb: csize): uint8T =
  var err: uint8T = 0
  w[] = mpdRealloc(w[], nmemb, sizeof(w[][]), addr(err))
  return not err

proc baseconvToU16*(w: ptr ptr uint16T; wlen: csize; wbase: MpdUintT; u: ptr MpdUintT;
                   ulen: MpdSsizeT): csize =
  var n: csize = 0
  assert(wlen > 0 and ulen > 0)
  assert(wbase <= (1 shl 16))
  while true:
    if n >= wlen:
      if not mpdResizeU16(w, n + 1):
        return size_Max
      wlen = n + 1
    (w[])[inc(n)] = cast[uint16T](mpdShortdiv(u, u, ulen, wbase))
    ##  ulen is at least 1. u[ulen-1] can only be zero if ulen == 1.
    ulen = mpdRealSize(u, ulen)
    if not (u[ulen - 1] != 0):
      break
  return n

proc coeffFromU16*(w: ptr MpdT; wlen: MpdSsizeT; u: ptr MpdUintT; ulen: csize;
                  ubase: uint32T; status: ptr uint32T): csize =
  var n: MpdSsizeT = 0
  var carry: MpdUintT
  assert(wlen > 0 and ulen > 0)
  assert(ubase <= (1 shl 16))
  w.data[inc(n)] = u[dec(ulen)]
  while dec(ulen) != size_Max:
    carry = mpdShortmulC(w.data, w.data, n, ubase)
    if carry:
      if n >= wlen:
        if not mpdQresize(w, n + 1, status):
          return size_Max
        wlen = n + 1
      w.data[inc(n)] = carry
    carry = mpdShortadd(w.data, n, u[ulen])
    if carry:
      if n >= wlen:
        if not mpdQresize(w, n + 1, status):
          return size_Max
        wlen = n + 1
      w.data[inc(n)] = carry
  return n

##  target base wbase < source base ubase

proc baseconvToSmaller*(w: ptr ptr uint32T; wlen: csize; wbase: uint32T; u: ptr MpdUintT;
                       ulen: MpdSsizeT; ubase: MpdUintT): csize =
  var n: csize = 0
  assert(wlen > 0 and ulen > 0)
  assert(wbase < ubase)
  while true:
    if n >= wlen:
      if not mpdResizeU32(w, n + 1):
        return size_Max
      wlen = n + 1
    (w[])[inc(n)] = cast[uint32T](mpdShortdivB(u, u, ulen, wbase, ubase))
    ##  ulen is at least 1. u[ulen-1] can only be zero if ulen == 1.
    ulen = mpdRealSize(u, ulen)
    if not (u[ulen - 1] != 0):
      break
  return n

when defined(CONFIG_32):
  ##  target base 'wbase' == source base 'ubase'
  proc copyEqualBase*(w: ptr ptr uint32T; wlen: csize; u: ptr uint32T; ulen: csize): csize =
    if wlen < ulen:
      if not mpdResizeU32(w, ulen):
        return size_Max
    memcpy(w[], u, ulen * (sizeof(w[][])))
    return ulen

  ##  target base 'wbase' > source base 'ubase'
  proc baseconvToLarger*(w: ptr ptr uint32T; wlen: csize; wbase: MpdUintT;
                        u: ptr MpdUintT; ulen: csize; ubase: MpdUintT): csize =
    var n: csize = 0
    var carry: MpdUintT
    assert(wlen > 0 and ulen > 0)
    assert(ubase < wbase)
    (w[])[inc(n)] = u[dec(ulen)]
    while dec(ulen) != size_Max:
      carry = mpdShortmulB(w[], w[], n, ubase, wbase)
      if carry:
        if n >= wlen:
          if not mpdResizeU32(w, n + 1):
            return size_Max
          wlen = n + 1
        (w[])[inc(n)] = carry
      carry = mpdShortaddB(w[], n, u[ulen], wbase)
      if carry:
        if n >= wlen:
          if not mpdResizeU32(w, n + 1):
            return size_Max
          wlen = n + 1
        (w[])[inc(n)] = carry
    return n

  ##  target base wbase < source base ubase
  proc coeffFromLargerBase*(w: ptr MpdT; wlen: csize; wbase: MpdUintT; u: ptr MpdUintT;
                           ulen: MpdSsizeT; ubase: MpdUintT; status: ptr uint32T): csize =
    var n: csize = 0
    assert(wlen > 0 and ulen > 0)
    assert(wbase < ubase)
    while true:
      if n >= wlen:
        if not mpdQresize(w, n + 1, status):
          return size_Max
        wlen = n + 1
      w.data[inc(n)] = cast[uint32T](mpdShortdivB(u, u, ulen, wbase, ubase))
      ##  ulen is at least 1. u[ulen-1] can only be zero if ulen == 1.
      ulen = mpdRealSize(u, ulen)
      if not (u[ulen - 1] != 0):
        break
    return n

##  target base 'wbase' > source base 'ubase'

proc coeffFromSmallerBase*(w: ptr MpdT; wlen: MpdSsizeT; wbase: MpdUintT;
                          u: ptr uint32T; ulen: csize; ubase: MpdUintT;
                          status: ptr uint32T): csize =
  var n: MpdSsizeT = 0
  var carry: MpdUintT
  assert(wlen > 0 and ulen > 0)
  assert(wbase > ubase)
  w.data[inc(n)] = u[dec(ulen)]
  while dec(ulen) != size_Max:
    carry = mpdShortmulB(w.data, w.data, n, ubase, wbase)
    if carry:
      if n >= wlen:
        if not mpdQresize(w, n + 1, status):
          return size_Max
        wlen = n + 1
      w.data[inc(n)] = carry
    carry = mpdShortaddB(w.data, n, u[ulen], wbase)
    if carry:
      if n >= wlen:
        if not mpdQresize(w, n + 1, status):
          return size_Max
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

proc mpdQexportU16*(rdata: ptr ptr uint16T; rlen: csize; rbase: uint32T; src: ptr MpdT;
                   status: ptr uint32T): csize =
  var tsrcData: array[mpd_Minalloc_Max, MpdUintT]
  var tsrc: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tsrcData]
  var alloc: cint = 0
  ##  rdata == NULL
  var n: csize
  assert(rbase <= (1 shl 16))
  if mpdIsspecial(src) or not mpdIsint(src):
    status[] = status[] or mPD_InvalidOperation
    return size_Max
  if rdata[] == nil:
    rlen = mpdSizeinbase(src, rbase)
    if rlen == size_Max:
      status[] = status[] or mPD_InvalidOperation
      return size_Max
    rdata[] = mpdAlloc(rlen, sizeof(rdata[][]))
    if rdata[] == nil:
      break mallocError
    alloc = 1
  if mpdIszero(src):
    rdata[][] = 0
    return 1
  if src.exp >= 0:
    if not mpdQshiftl(addr(tsrc), src, src.exp, status):
      break mallocError
  else:
    if mpdQshiftr(addr(tsrc), src, -src.exp, status) == mpd_Uint_Max:
      break mallocError
  n = baseconvToU16(rdata, rlen, rbase, tsrc.data, tsrc.len)
  if n == size_Max:
    break mallocError
  mpdDel(addr(tsrc))
  return n
  if alloc:
    mpdFree(rdata[])
    rdata[] = nil
  n = size_Max
  status[] = status[] or mPD_MallocError
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

proc mpdQexportU32*(rdata: ptr ptr uint32T; rlen: csize; rbase: uint32T; src: ptr MpdT;
                   status: ptr uint32T): csize =
  var tsrcData: array[mpd_Minalloc_Max, MpdUintT]
  var tsrc: MpdT = [0 or mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, tsrcData]
  var alloc: cint = 0
  ##  rdata == NULL
  var n: csize
  if mpdIsspecial(src) or not mpdIsint(src):
    status[] = status[] or mPD_InvalidOperation
    return size_Max
  if rdata[] == nil:
    rlen = mpdSizeinbase(src, rbase)
    if rlen == size_Max:
      status[] = status[] or mPD_InvalidOperation
      return size_Max
    rdata[] = mpdAlloc(rlen, sizeof(rdata[][]))
    if rdata[] == nil:
      break mallocError
    alloc = 1
  if mpdIszero(src):
    rdata[][] = 0
    return 1
  if src.exp >= 0:
    if not mpdQshiftl(addr(tsrc), src, src.exp, status):
      break mallocError
  else:
    if mpdQshiftr(addr(tsrc), src, -src.exp, status) == mpd_Uint_Max:
      break mallocError
  when defined(CONFIG_64):
    n = baseconvToSmaller(rdata, rlen, rbase, tsrc.data, tsrc.len, mpd_Radix)
  else:
    if rbase == mpd_Radix:
      n = copyEqualBase(rdata, rlen, tsrc.data, tsrc.len)
    elif rbase < mpd_Radix:
      n = baseconvToSmaller(rdata, rlen, rbase, tsrc.data, tsrc.len, mpd_Radix)
    else:
      n = baseconvToLarger(rdata, rlen, rbase, tsrc.data, tsrc.len, mpd_Radix)
  if n == size_Max:
    break mallocError
  mpdDel(addr(tsrc))
  return n
  if alloc:
    mpdFree(rdata[])
    rdata[] = nil
  n = size_Max
  status[] = status[] or mPD_MallocError
  break `out`

##
##  Converts a multiprecision integer with base <= UINT16_MAX+1 to an mpd_t.
##  The least significant word of the source is srcdata[0].
##

proc mpdQimportU16*(result: ptr MpdT; srcdata: ptr uint16T; srclen: csize;
                   srcsign: uint8T; srcbase: uint32T; ctx: ptr MpdContextT;
                   status: ptr uint32T) =
  var usrc: ptr MpdUintT
  ##  uint16_t src copied to an mpd_uint_t array
  var rlen: MpdSsizeT
  ##  length of the result
  var n: csize
  assert(srclen > 0)
  assert(srcbase <= (1 shl 16))
  rlen = mpdImportsize(srclen, srcbase)
  if rlen == mpd_Ssize_Max:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  usrc = mpdAlloc(cast[MpdSizeT](srclen), sizeof(usrc[]))
  if usrc == nil:
    mpdSeterror(result, mPD_MallocError, status)
    return
  n = 0
  while n < srclen:
    usrc[n] = srcdata[n]
    inc(n)
  if not mpdQresize(result, rlen, status):
    break finish
  n = coeffFromU16(result, rlen, usrc, srclen, srcbase, status)
  if n == size_Max:
    break finish
  mpdSetFlags(result, srcsign)
  result.exp = 0
  result.len = n
  mpdSetdigits(result)
  mpdQresize(result, result.len, status)
  mpdQfinalize(result, ctx, status)
  mpdFree(usrc)

##
##  Converts a multiprecision integer with base <= UINT32_MAX to an mpd_t.
##  The least significant word of the source is srcdata[0].
##

proc mpdQimportU32*(result: ptr MpdT; srcdata: ptr uint32T; srclen: csize;
                   srcsign: uint8T; srcbase: uint32T; ctx: ptr MpdContextT;
                   status: ptr uint32T) =
  var rlen: MpdSsizeT
  ##  length of the result
  var n: csize
  assert(srclen > 0)
  rlen = mpdImportsize(srclen, srcbase)
  if rlen == mpd_Ssize_Max:
    mpdSeterror(result, mPD_InvalidOperation, status)
    return
  if not mpdQresize(result, rlen, status):
    return
  when defined(CONFIG_64):
    n = coeffFromSmallerBase(result, rlen, mpd_Radix, srcdata, srclen, srcbase, status)
  else:
    if srcbase == mpd_Radix:
      if not mpdQresize(result, srclen, status):
        return
      memcpy(result.data, srcdata, srclen * (sizeof(srcdata[])))
      n = srclen
    elif srcbase < mpd_Radix:
      n = coeffFromSmallerBase(result, rlen, mpd_Radix, srcdata, srclen, srcbase, status)
    else:
      var usrc: ptr MpdUintT = mpdAlloc(cast[MpdSizeT](srclen), sizeof(usrc[]))
      if usrc == nil:
        mpdSeterror(result, mPD_MallocError, status)
        return
      n = 0
      while n < srclen:
        usrc[n] = srcdata[n]
        inc(n)
      n = coeffFromLargerBase(result, rlen, mpd_Radix, usrc, cast[MpdSsizeT](srclen),
                            srcbase, status)
      mpdFree(usrc)
  if n == size_Max:
    return
  mpdSetFlags(result, srcsign)
  result.exp = 0
  result.len = n
  mpdSetdigits(result)
  mpdQresize(result, result.len, status)
  mpdQfinalize(result, ctx, status)

## ****************************************************************************
##                                 From triple
## ****************************************************************************

when defined(config_64) and defined(sizeof_Int128):
  proc setCoeff*(data: array[3, uint64T]; hi: uint64T; lo: uint64T): MpdSsizeT =
    var d: Uint128T = (cast[Uint128T](hi) shl 64) + lo
    var
      q: Uint128T
      r: Uint128T
    q = d div mpd_Radix
    r = d mod mpd_Radix
    data[0] = cast[uint64T](r)
    d = q
    q = d div mpd_Radix
    r = d mod mpd_Radix
    data[1] = cast[uint64T](r)
    d = q
    q = d div mpd_Radix
    r = d mod mpd_Radix
    data[2] = cast[uint64T](r)
    if q != 0:
      abort()
      ##  GCOV_NOT_REACHED
    return if data[2] != 0: 3 else: (if data[1] != 0: 2 else: 1)

else:
  proc uintFromU16*(w: ptr MpdUintT; wlen: MpdSsizeT; u: ptr uint16T; ulen: csize): csize =
    var ubase: MpdUintT = 1 shl 16
    var n: MpdSsizeT = 0
    var carry: MpdUintT
    assert(wlen > 0 and ulen > 0)
    w[inc(n)] = u[dec(ulen)]
    while dec(ulen) != size_Max:
      carry = mpdShortmulC(w, w, n, ubase)
      if carry:
        if n >= wlen:
          abort()
          ##  GCOV_NOT_REACHED
        w[inc(n)] = carry
      carry = mpdShortadd(w, n, u[ulen])
      if carry:
        if n >= wlen:
          abort()
          ##  GCOV_NOT_REACHED
        w[inc(n)] = carry
    return n

  proc setCoeff*(data: ptr MpdUintT; len: MpdSsizeT; hi: uint64T; lo: uint64T): MpdSsizeT =
    var u16: array[8, uint16T] = [0]
    u16[7] = (uint16T)((hi and 0x0000000000000000'i64) shr 48)
    u16[6] = (uint16T)((hi and 0x0000000000000000'i64) shr 32)
    u16[5] = (uint16T)((hi and 0x0000000000000000'i64) shr 16)
    u16[4] = (uint16T)(hi and 0x0000000000000000'i64)
    u16[3] = (uint16T)((lo and 0x0000000000000000'i64) shr 48)
    u16[2] = (uint16T)((lo and 0x0000000000000000'i64) shr 32)
    u16[1] = (uint16T)((lo and 0x0000000000000000'i64) shr 16)
    u16[0] = (uint16T)(lo and 0x0000000000000000'i64)
    return cast[MpdSsizeT](uintFromU16(data, len, u16, 8))

proc setUint128CoeffExp*(result: ptr MpdT; hi: uint64T; lo: uint64T; exp: MpdSsizeT): cint =
  var data: array[5, MpdUintT] = [0]
  var status: uint32T = 0
  var len: MpdSsizeT
  when defined(config_64) and defined(sizeof_Int128):
    len = setCoeff(data, hi, lo)
  else:
    len = setCoeff(data, 5, hi, lo)
  if not mpdQresize(result, len, addr(status)):
    return -1
  var i: MpdSsizeT = 0
  while i < len:
    result.data[i] = data[i]
    inc(i)
  result.exp = exp
  result.len = len
  mpdSetdigits(result)
  return 0

proc mpdFromUint128Triple*(result: ptr MpdT; triple: ptr MpdUint128TripleT;
                          status: ptr uint32T): cint =
  var maxcontext: MpdContextT = [] ##  C2NIM
                              ##  .prec=MPD_MAX_PREC,
                              ##  .emax=MPD_MAX_EMAX,
                              ##  .emin=MPD_MIN_EMIN,
                              ##  .round=MPD_ROUND_HALF_EVEN,
                              ##  .traps=MPD_Traps,
                              ##  .status=0,
                              ##  .newtrap=0,
                              ##  .clamp=0,
                              ##  .allcr=1,
  var tag: MpdTripleClass = triple.tag
  var sign: uint8T = triple.sign
  var hi: uint64T = triple.hi
  var lo: uint64T = triple.lo
  var exp: MpdSsizeT
  when defined(CONFIG_32):
    if triple.exp < mpd_Ssize_Min or triple.exp > mpd_Ssize_Max:
      break conversionError
  exp = cast[MpdSsizeT](triple.exp)
  case tag
  of mpd_Triple_Qnan, mpd_Triple_Snan:
    if sign > 1 or exp != 0:
      break conversionError
    var flags: uint8T = if tag == mpd_Triple_Qnan: mpd_Nan else: mpd_Snan
    mpdSetspecial(result, sign, flags)
    if hi == 0 and lo == 0:
      ##  no payload
      return 0
    if setUint128CoeffExp(result, hi, lo, exp) < 0:
      break mallocError
    return 0
  of mpd_Triple_Inf:
    if sign > 1 or hi != 0 or lo != 0 or exp != 0:
      break conversionError
    mpdSetspecial(result, sign, mpd_Inf)
    return 0
  of mpd_Triple_Normal:
    if sign > 1:
      break conversionError
    var flags: uint8T = if sign: mpd_Neg else: mpd_Pos
    mpdSetFlags(result, flags)
    if exp > mpd_Exp_Inf:
      exp = mpd_Exp_Inf
    if exp == mpd_Ssize_Min:
      exp = mpd_Ssize_Min + 1
    if setUint128CoeffExp(result, hi, lo, exp) < 0:
      break mallocError
    var workstatus: uint32T = 0
    mpdQfinalize(result, addr(maxcontext), addr(workstatus))
    if workstatus and (mPD_Inexact or mPD_Rounded or mPD_Clamped):
      break conversionError
    return 0
  else:
    break conversionError
  mpdSeterror(result, mPD_ConversionSyntax, status)
  return -1
  mpdSeterror(result, mPD_MallocError, status)
  return -1

## ****************************************************************************
##                                   As triple
## ****************************************************************************

when defined(config_64) and defined(sizeof_Int128):
  proc getCoeff*(hi: ptr uint64T; lo: ptr uint64T; a: ptr MpdT) =
    var u128: Uint128T = 0
    case a.len
    of 3:
      u128 = a.data[2]
      ##  fall through
    of 2:
      u128 = u128 * mpd_Radix + a.data[1]
      ##  fall through
    of 1:
      u128 = u128 * mpd_Radix + a.data[0]
    else:
      abort()
      ##  GCOV_NOT_REACHED
    hi[] = u128 shr 64
    lo[] = cast[uint64T](u128)

else:
  proc uintToU16*(w: array[8, uint16T]; u: ptr MpdUintT; ulen: MpdSsizeT): csize =
    var wbase: MpdUintT = 1 shl 16
    var n: csize = 0
    assert(ulen > 0)
    while true:
      if n >= 8:
        abort()
        ##  GCOV_NOT_REACHED
      w[inc(n)] = cast[uint16T](mpdShortdiv(u, u, ulen, wbase))
      ##  ulen is at least 1. u[ulen-1] can only be zero if ulen == 1.
      ulen = mpdRealSize(u, ulen)
      if not (u[ulen - 1] != 0):
        break
    return n

  proc getCoeff*(hi: ptr uint64T; lo: ptr uint64T; a: ptr MpdT) =
    var u16: array[8, uint16T] = [0]
    var data: array[5, MpdUintT] = [0]
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
    uintToU16(u16, data, a.len)
    hi[] = cast[uint64T](u16[7]) shl 48
    hi[] = hi[] or cast[uint64T](u16[6]) shl 32
    hi[] = hi[] or cast[uint64T](u16[5]) shl 16
    hi[] = hi[] or cast[uint64T](u16[4])
    lo[] = cast[uint64T](u16[3]) shl 48
    lo[] = lo[] or cast[uint64T](u16[2]) shl 32
    lo[] = lo[] or cast[uint64T](u16[1]) shl 16
    lo[] = lo[] or cast[uint64T](u16[0])

proc coeffAsUint128*(hi: ptr uint64T; lo: ptr uint64T; a: ptr MpdT): MpdTripleClass =
  when defined(CONFIG_64):
    var uint128MaxData: array[3, MpdUintT] = [3374607431768211455'i64,
        4028236692093846346'i64, 3]
    var uint128Max: MpdT = [mpd_Static or mpd_Const_Data, 0, 39, 3, 3, uint128MaxData]
  else:
    var uint128MaxData: array[5, MpdUintT] = [768211455, 374607431, 938463463,
        282366920, 340]
    var uint128Max: MpdT = [mpd_Static or mpd_Const_Data, 0, 39, 5, 5, uint128MaxData]
  var ret: MpdTripleClass = mpd_Triple_Normal
  var status: uint32T = 0
  var coeff: MpdT
  hi[] = lo[] = 0
  if mpdIsspecial(a):
    if mpdIsinfinite(a):
      return mpd_Triple_Inf
    ret = if mpdIsqnan(a): mpd_Triple_Qnan else: mpd_Triple_Snan
    if a.len == 0:
      ##  no payload
      return ret
  elif mpdIszero(a):
    return ret
  mpdCopyShared(addr(coeff), a)
  mpdSetFlags(addr(coeff), 0)
  coeff.exp = 0
  if mpdQcmp(addr(coeff), addr(uint128Max), addr(status)) > 0:
    return mpd_Triple_Error
  getCoeff(hi, lo, addr(coeff))
  return ret

proc mpdAsUint128Triple*(a: ptr MpdT): MpdUint128TripleT =
  var triple: MpdUint128TripleT = [mpd_Triple_Error, 0, 0, 0, 0]
  triple.tag = coeffAsUint128(addr(triple.hi), addr(triple.lo), a)
  if triple.tag == mpd_Triple_Error:
    return triple
  triple.sign = not not mpdIsnegative(a)
  if triple.tag == mpd_Triple_Normal:
    triple.exp = a.exp
  return triple
