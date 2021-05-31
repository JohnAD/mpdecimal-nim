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
  mpdecimal, io, typearith

##  This file contains functions for decimal <-> string conversions, including
##    PEP-3101 formatting for numeric types.

when defined(gnuc) and not defined(intel_Compiler) and gnuc >= 7:
##
##  Work around the behavior of tolower() and strcasecmp() in certain
##  locales. For example, in tr_TR.utf8:
##
##  tolower((unsigned char)'I') == 'I'
##
##  u is the exact uppercase version of l; n is strlen(l) or strlen(l)+1
##

proc mpdStrneq*(s: cstring; l: cstring; u: cstring; n: csize): cint {.inline.} =
  while dec(n) != size_Max:
    if s[] != l[] and s[] != u[]:
      return 0
    inc(s)
    inc(u)
    inc(l)
  return 1

proc strtoexp*(s: cstring): MpdSsizeT =
  var `end`: cstring
  var retval: MpdSsizeT
  errno = 0
  retval = mpdStrtossize(s, addr(`end`), 10)
  if errno == 0 and not (s[] != '\x00' and `end`[] == '\x00'):
    errno = einval
  return retval

##
##  Scan 'len' words. The most significant word contains 'r' digits,
##  the remaining words are full words. Skip dpoint. The string 's' must
##  consist of digits and an optional single decimal point at 'dpoint'.
##

proc stringToCoeff*(data: ptr MpdUintT; s: cstring; dpoint: cstring; r: cint; len: csize) =
  var j: cint
  if r > 0:
    data[dec(len)] = 0
    j = 0
    while j < r:
      if s == dpoint:
        inc(s)
      data[len] = 10 * data[len] + (s[] - '0')
      inc(j)
      inc(s)
  while dec(len) != size_Max:
    data[len] = 0
    j = 0
    while j < mpd_Rdigits:
      if s == dpoint:
        inc(s)
      data[len] = 10 * data[len] + (s[] - '0')
      inc(j)
      inc(s)

##
##  Partially verify a numeric string of the form:
##
##      [cdigits][.][cdigits][eE][+-][edigits]
##
##  If successful, return a pointer to the location of the first
##  relevant coefficient digit. This digit is either non-zero or
##  part of one of the following patterns:
##
##      ["0\x00", "0.\x00", "0.E", "0.e", "0E", "0e"]
##
##  The locations of a single optional dot or indicator are stored
##  in 'dpoint' and 'exp'.
##
##  The end of the string is stored in 'end'. If an indicator [eE]
##  occurs without trailing [edigits], the condition is caught
##  later by strtoexp().
##

proc scanDpointExp*(s: cstring; dpoint: cstringArray; exp: cstringArray;
                   `end`: cstringArray): cstring =
  var coeff: cstring = nil
  dpoint[] = nil
  exp[] = nil
  while s[] != '\x00':
    case s[]
    of '.':
      if dpoint[] != nil or exp[] != nil:
        return nil
      dpoint[] = s
    of 'E', 'e':
      if exp[] != nil:
        return nil
      exp[] = s
      if (s + 1)[] == '+' or (s + 1)[] == '-':
        inc(s)
    else:
      if not isdigit(cast[cuchar](s[])):
        return nil
      if coeff == nil and exp[] == nil:
        if s[] == '0':
          if not isdigit(cast[cuchar]((s + 1)[])):
            if not ((s + 1)[] == '.' and isdigit(cast[cuchar]((s + 2)[]))):
              coeff = s
        else:
          coeff = s
    inc(s)
  `end`[] = s
  return coeff

##  scan the payload of a NaN

proc scanPayload*(s: cstring; `end`: cstringArray): cstring =
  var coeff: cstring
  while s[] == '0':
    inc(s)
  coeff = s
  while isdigit(cast[cuchar](s[])):
    inc(s)
  `end`[] = s
  return if (s[] == '\x00'): coeff else: nil

##  convert a character string to a decimal

proc mpdQsetString*(dec: ptr MpdT; s: cstring; ctx: ptr MpdContextT; status: ptr uint32T) =
  var
    q: MpdSsizeT
    r: MpdSsizeT
    len: MpdSsizeT
  var
    coeff: cstring
    `end`: cstring
  var
    dpoint: cstring = nil
    exp: cstring = nil
  var digits: csize
  var sign: uint8T = mpd_Pos
  mpdSetFlags(dec, 0)
  dec.len = 0
  dec.exp = 0
  ##  sign
  if s[] == '+':
    inc(s)
  elif s[] == '-':
    mpdSetNegative(dec)
    sign = mpd_Neg
    inc(s)
  if mpdStrneq(s, "nan", "NAN", 3):
    ##  NaN
    inc(s, 3)
    mpdSetspecial(dec, sign, mpd_Nan)
    if s[] == '\x00':
      return
    if (coeff = scanPayload(s, addr(`end`))) == nil:
      break conversionError
    if coeff[] == '\x00':
      return
    digits = `end` - coeff
    ##  prec >= 1, clamp is 0 or 1
    if digits > (sizeT)(ctx.prec - ctx.clamp):
      break conversionError
  elif mpdStrneq(s, "snan", "SNAN", 4):
    inc(s, 4)
    mpdSetspecial(dec, sign, mpd_Snan)
    if s[] == '\x00':
      return
    if (coeff = scanPayload(s, addr(`end`))) == nil:
      break conversionError
    if coeff[] == '\x00':
      return
    digits = `end` - coeff
    if digits > (sizeT)(ctx.prec - ctx.clamp):
      break conversionError
  elif mpdStrneq(s, "inf", "INF", 3):
    inc(s, 3)
    if s[] == '\x00' or mpdStrneq(s, "inity", "INITY", 6):
      ##  numeric-value: infinity
      mpdSetspecial(dec, sign, mpd_Inf)
      return
    break conversionError
  else:
    ##  scan for start of coefficient, decimal point, indicator, end
    if (coeff = scanDpointExp(s, addr(dpoint), addr(exp), addr(`end`))) == nil:
      break conversionError
    if exp:
      ##  exponent-part
      `end` = exp
      inc(exp)
      dec.exp = strtoexp(exp)
      if errno:
        if not (errno == erange and
            (dec.exp == mpd_Ssize_Max or dec.exp == mpd_Ssize_Min)):
          break conversionError
    digits = `end` - coeff
    if dpoint:
      var fracdigits: csize = `end` - dpoint - 1
      if dpoint > coeff:
        dec(digits)
      if fracdigits > mpd_Max_Prec:
        break conversionError
      if dec.exp < mpd_Ssize_Min + cast[MpdSsizeT](fracdigits):
        dec.exp = mpd_Ssize_Min
      else:
        dec(dec.exp, cast[MpdSsizeT](fracdigits))
    if digits > mpd_Max_Prec:
      break conversionError
    if dec.exp > mpd_Exp_Inf:
      dec.exp = mpd_Exp_Inf
    if dec.exp == mpd_Ssize_Min:
      dec.exp = mpd_Ssize_Min + 1
  mpdIdivWord(addr(q), addr(r), cast[MpdSsizeT](digits), mpd_Rdigits)
  len = if (r == 0): q else: q + 1
  if len == 0:
    break conversionError
    ##  GCOV_NOT_REACHED
  if not mpdQresize(dec, len, status):
    mpdSeterror(dec, mPD_MallocError, status)
    return
  dec.len = len
  stringToCoeff(dec.data, coeff, dpoint, cast[cint](r), len)
  mpdSetdigits(dec)
  mpdQfinalize(dec, ctx, status)
  return
  ##  standard wants a positive NaN
  mpdSeterror(dec, mPD_ConversionSyntax, status)

##  convert a character string to a decimal, use a maxcontext for conversion

proc mpdQsetStringExact*(dec: ptr MpdT; s: cstring; status: ptr uint32T) =
  var maxcontext: MpdContextT
  mpdMaxcontext(addr(maxcontext))
  mpdQsetString(dec, s, addr(maxcontext), status)
  if status[] and (mPD_Inexact or mPD_Rounded or mPD_Clamped):
    ##  we want exact results
    mpdSeterror(dec, mPD_InvalidOperation, status)
  status[] = status[] and mPD_Errors

##  Print word x with n decimal digits to string s. dot is either NULL
##    or the location of a decimal point.

proc wordToString*(s: cstring; x: MpdUintT; n: cint; dot: cstring): cstring {.inline.} =
  case n ##  #ifdef CONFIG_64
       ##      case 20: EXTRACT_DIGIT(s, x, 10000000000000000000ULL, dot); /* GCOV_NOT_REACHED */
       ##      case 19: EXTRACT_DIGIT(s, x, 1000000000000000000ULL, dot);
       ##      case 18: EXTRACT_DIGIT(s, x, 100000000000000000ULL, dot);
       ##      case 17: EXTRACT_DIGIT(s, x, 10000000000000000ULL, dot);
       ##      case 16: EXTRACT_DIGIT(s, x, 1000000000000000ULL, dot);
       ##      case 15: EXTRACT_DIGIT(s, x, 100000000000000ULL, dot);
       ##      case 14: EXTRACT_DIGIT(s, x, 10000000000000ULL, dot);
       ##      case 13: EXTRACT_DIGIT(s, x, 1000000000000ULL, dot);
       ##      case 12: EXTRACT_DIGIT(s, x, 100000000000ULL, dot);
       ##      case 11: EXTRACT_DIGIT(s, x, 10000000000ULL, dot);
       ##  #endif
  of 10:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 1000000000)
    x = x mod 1000000000
  of 9:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 100000000)
    x = x mod 100000000
  of 8:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 10000000)
    x = x mod 10000000
  of 7:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 1000000)
    x = x mod 1000000
  of 6:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 100000)
    x = x mod 100000
  of 5:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 10000)
    x = x mod 10000
  of 4:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 1000)
    x = x mod 1000
  of 3:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 100)
    x = x mod 100
  of 2:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + (char)(x div 10)
    x = x mod 10
  else:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + cast[char](x)
  s[] = '\x00'
  return s

##  Print exponent x to string s. Undefined for MPD_SSIZE_MIN.

proc expToString*(s: cstring; x: MpdSsizeT): cstring {.inline.} =
  var sign: char = '+'
  if x < 0:
    sign = '-'
    x = -x
  inc(s)[] = sign
  return wordToString(s, x, mpdWordDigits(x), nil)

##  Print the coefficient of dec to string s. len(dec) > 0.

proc coeffToString*(s: cstring; dec: ptr MpdT): cstring {.inline.} =
  var x: MpdUintT
  var i: MpdSsizeT
  ##  most significant word
  x = mpdMsword(dec)
  s = wordToString(s, x, mpdWordDigits(x), nil)
  ##  remaining full words
  i = dec.len - 2
  while i >= 0:
    x = dec.data[i]
    s = wordToString(s, x, mpd_Rdigits, nil)
    dec(i)
  return s

##  Print the coefficient of dec to string s. len(dec) > 0. dot is either
##    NULL or a pointer to the location of a decimal point.

proc coeffToStringDot*(s: cstring; dot: cstring; dec: ptr MpdT): cstring {.inline.} =
  var x: MpdUintT
  var i: MpdSsizeT
  ##  most significant word
  x = mpdMsword(dec)
  s = wordToString(s, x, mpdWordDigits(x), dot)
  ##  remaining full words
  i = dec.len - 2
  while i >= 0:
    x = dec.data[i]
    s = wordToString(s, x, mpd_Rdigits, dot)
    dec(i)
  return s

##  Format type

const
  MPD_FMT_LOWER* = 0x00000000
  MPD_FMT_UPPER* = 0x00000001
  MPD_FMT_TOSCI* = 0x00000002
  MPD_FMT_TOENG* = 0x00000004
  MPD_FMT_EXP* = 0x00000008
  MPD_FMT_FIXED* = 0x00000010
  MPD_FMT_PERCENT* = 0x00000020
  MPD_FMT_SIGN_SPACE* = 0x00000040
  MPD_FMT_SIGN_PLUS* = 0x00000080

##  Default place of the decimal point for MPD_FMT_TOSCI, MPD_FMT_EXP

const
  MPD_DEFAULT_DOTPLACE* = 1

##
##  Set *result to the string representation of a decimal. Return the length
##  of *result, not including the terminating '\0' character.
##
##  Formatting is done according to 'flags'. A return value of -1 with *result
##  set to NULL indicates MPD_Malloc_error.
##
##  'dplace' is the default place of the decimal point. It is always set to
##  MPD_DEFAULT_DOTPLACE except for zeros in combination with MPD_FMT_EXP.
##

proc mpdToString*(result: cstringArray; dec: ptr MpdT; flags: cint; dplace: MpdSsizeT): MpdSsizeT =
  var
    decstring: cstring = nil
    cp: cstring = nil
  var ldigits: MpdSsizeT
  var
    mem: MpdSsizeT = 0
    k: MpdSsizeT
  if mpdIsspecial(dec):
    mem = sizeof("-Infinity%")
    if mpdIsnan(dec) and dec.len > 0:
      ##  diagnostic code
      inc(mem, dec.digits)
    cp = decstring = mpdAlloc(mem, sizeof(decstring[]))
    if cp == nil:
      result[] = nil
      return -1
    if mpdIsnegative(dec):
      inc(cp)[] = '-'
    elif flags and mpd_Fmt_Sign_Space:
      inc(cp)[] = ' '
    elif flags and mpd_Fmt_Sign_Plus:
      inc(cp)[] = '+'
    if mpdIsnan(dec):
      if mpdIsqnan(dec):
        strcpy(cp, "NaN")
        inc(cp, 3)
      else:
        strcpy(cp, "sNaN")
        inc(cp, 4)
      if dec.len > 0:
        ##  diagnostic code
        cp = coeffToString(cp, dec)
    elif mpdIsinfinite(dec):
      strcpy(cp, "Infinity")
      inc(cp, 8)
    else:
      ##  debug
      abort()
      ##  GCOV_NOT_REACHED
  else:
    assert(dec.len > 0)
    ##
    ##  For easier manipulation of the decimal point's location
    ##  and the exponent that is finally printed, the number is
    ##  rescaled to a virtual representation with exp = 0. Here
    ##  ldigits denotes the number of decimal digits to the left
    ##  of the decimal point and remains constant once initialized.
    ##
    ##  dplace is the location of the decimal point relative to
    ##  the start of the coefficient. Note that 3) always holds
    ##  when dplace is shifted.
    ##
    ##    1) ldigits := dec->digits - dec->exp
    ##    2) dplace  := ldigits            (initially)
    ##    3) exp     := ldigits - dplace   (initially exp = 0)
    ##
    ##    0.00000_.____._____000000.
    ##     ^      ^    ^           ^
    ##     |      |    |           |
    ##     |      |    |           `- dplace >= digits
    ##     |      |    `- dplace in the middle of the coefficient
    ##     |      ` dplace = 1 (after the first coefficient digit)
    ##     `- dplace <= 0
    ##
    ldigits = dec.digits + dec.exp
    if flags and mpd_Fmt_Exp:
      nil
    elif flags and mpd_Fmt_Fixed or (dec.exp <= 0 and ldigits > -6):
      ##  MPD_FMT_FIXED: always use fixed point notation.
      ##  MPD_FMT_TOSCI, MPD_FMT_TOENG: for a certain range,
      ##  override exponent notation.
      dplace = ldigits
    elif flags and mpd_Fmt_Toeng: ##
                              ##  Basic space requirements:
                              ##
                              ##  [-][.][coeffdigits][E][-][expdigits+1][%]['\0']
                              ##
                              ##  If the decimal point lies outside of the coefficient digits,
                              ##  space is adjusted accordingly.
                              ##
      if mpdIszero(dec):
        ##  If the exponent is divisible by three,
        ##  dplace = 1. Otherwise, move dplace one
        ##  or two places to the left.
        dplace = -1 + modMpdSsizeT(dec.exp + 2, 3)
      else:
        ##  ldigits-1 is the adjusted exponent, which
        ##  should be divisible by three. If not, move
        ##  dplace one or two places to the right.
        inc(dplace, modMpdSsizeT(ldigits - 1, 3))
    if dplace <= 0:
      mem = -dplace + dec.digits + 2
    elif dplace >= dec.digits:
      mem = dplace
    else:
      mem = dec.digits
    inc(mem, (mpd_Expdigits + 1 + 6))
    cp = decstring = mpdAlloc(mem, sizeof(decstring[]))
    if cp == nil:
      result[] = nil
      return -1
    if mpdIsnegative(dec):
      inc(cp)[] = '-'
    elif flags and mpd_Fmt_Sign_Space:
      inc(cp)[] = ' '
    elif flags and mpd_Fmt_Sign_Plus:
      inc(cp)[] = '+'
    if dplace <= 0:
      ##  space: -dplace+dec->digits+2
      inc(cp)[] = '0'
      inc(cp)[] = '.'
      k = 0
      while k < -dplace:
        inc(cp)[] = '0'
        inc(k)
      cp = coeffToString(cp, dec)
    elif dplace >= dec.digits:
      ##  space: dplace
      cp = coeffToString(cp, dec)
      k = 0
      while k < dplace - dec.digits:
        inc(cp)[] = '0'
        inc(k)
    else:
      ##  space: dec->digits+1
      cp = coeffToStringDot(cp, cp + dplace, dec)
    ##
    ##  Conditions for printing an exponent:
    ##
    ##    MPD_FMT_TOSCI, MPD_FMT_TOENG: only if ldigits != dplace
    ##    MPD_FMT_FIXED:                never (ldigits == dplace)
    ##    MPD_FMT_EXP:                  always
    ##
    if ldigits != dplace or flags and mpd_Fmt_Exp:
      ##  space: expdigits+2
      inc(cp)[] = if (flags and mpd_Fmt_Upper): 'E' else: 'e'
      cp = expToString(cp, ldigits - dplace)
  if flags and mpd_Fmt_Percent:
    inc(cp)[] = '%'
  assert(cp < decstring + mem)
  assert(cp - decstring < mpd_Ssize_Max)
  cp[] = '\x00'
  result[] = decstring
  return (mpdSsizeT)(cp - decstring)

proc mpdToSci*(dec: ptr MpdT; fmt: cint): cstring =
  var res: cstring
  var flags: cint = mpd_Fmt_Tosci
  flags = flags or if fmt: mpd_Fmt_Upper else: mpd_Fmt_Lower
  cast[nil](mpdToString(addr(res), dec, flags, mpd_Default_Dotplace))
  return res

proc mpdToEng*(dec: ptr MpdT; fmt: cint): cstring =
  var res: cstring
  var flags: cint = mpd_Fmt_Toeng
  flags = flags or if fmt: mpd_Fmt_Upper else: mpd_Fmt_Lower
  cast[nil](mpdToString(addr(res), dec, flags, mpd_Default_Dotplace))
  return res

proc mpdToSciSize*(res: cstringArray; dec: ptr MpdT; fmt: cint): MpdSsizeT =
  var flags: cint = mpd_Fmt_Tosci
  flags = flags or if fmt: mpd_Fmt_Upper else: mpd_Fmt_Lower
  return mpdToString(res, dec, flags, mpd_Default_Dotplace)

proc mpdToEngSize*(res: cstringArray; dec: ptr MpdT; fmt: cint): MpdSsizeT =
  var flags: cint = mpd_Fmt_Toeng
  flags = flags or if fmt: mpd_Fmt_Upper else: mpd_Fmt_Lower
  return mpdToString(res, dec, flags, mpd_Default_Dotplace)

##  Copy a single UTF-8 char to dest. See: The Unicode Standard, version 5.2,
##    chapter 3.9: Well-formed UTF-8 byte sequences.

proc mpdCopyUtf8*(dest: array[5, char]; s: cstring): cint =
  var cp: ptr cuchar = cast[ptr cuchar](s)
  var
    lb: cuchar
    ub: cuchar
  var
    count: cint
    i: cint
  if cp[] == 0:
    ##  empty string
    dest[0] = '\x00'
    return 0
  elif cp[] <= 0x0000007F:
    ##  ascii
    dest[0] = cp[]
    dest[1] = '\x00'
    return 1
  elif 0x000000C2 <= cp[] and cp[] <= 0x000000DF:
    lb = 0x00000080
    ub = 0x000000BF
    count = 2
  elif cp[] == 0x000000E0:
    lb = 0x000000A0
    ub = 0x000000BF
    count = 3
  elif cp[] <= 0x000000EC:
    lb = 0x00000080
    ub = 0x000000BF
    count = 3
  elif cp[] == 0x000000ED:
    lb = 0x00000080
    ub = 0x0000009F
    count = 3
  elif cp[] <= 0x000000EF:
    lb = 0x00000080
    ub = 0x000000BF
    count = 3
  elif cp[] == 0x000000F0:
    lb = 0x00000090
    ub = 0x000000BF
    count = 4
  elif cp[] <= 0x000000F3:
    lb = 0x00000080
    ub = 0x000000BF
    count = 4
  elif cp[] == 0x000000F4:
    lb = 0x00000080
    ub = 0x0000008F
    count = 4
  else:
    ##  invalid
    break error
  dest[0] = inc(cp)[]
  if cp[] < lb or ub < cp[]:
    break error
  dest[1] = inc(cp)[]
  i = 2
  while i < count:
    if cp[] < 0x00000080 or 0x000000BF < cp[]:
      break error
    dest[i] = inc(cp)[]
    inc(i)
  dest[i] = '\x00'
  return count
  dest[0] = '\x00'
  return -1

proc mpdValidateLconv*(spec: ptr MpdSpecT): cint =
  var n: csize
  when char_Max == schar_Max:
    var cp: cstring = spec.grouping
    while cp[] != '\x00':
      if inc(cp)[] < 0:
        return -1
  n = strlen(spec.dot)
  if n == 0 or n > 4:
    return -1
  if strlen(spec.sep) > 4:
    return -1
  return 0

proc mpdParseFmtStr*(spec: ptr MpdSpecT; fmt: cstring; caps: cint): cint =
  var cp: cstring = cast[cstring](fmt)
  var
    haveAlign: cint = 0
    n: cint
  ##  defaults
  spec.minWidth = 0
  spec.prec = -1
  spec.`type` = if caps: 'G' else: 'g'
  spec.align = '>'
  spec.sign = '-'
  spec.dot = ""
  spec.sep = ""
  spec.grouping = ""
  ##  presume that the first character is a UTF-8 fill character
  if (n = mpdCopyUtf8(spec.fill, cp)) < 0:
    return 0
  if cp[] and
      ((cp + n)[] == '<' or (cp + n)[] == '>' or (cp + n)[] == '=' or (cp + n)[] == '^'):
    inc(cp, n)
    spec.align = inc(cp)[]
    haveAlign = 1
  else:
    ##  default fill character
    spec.fill[0] = ' '
    spec.fill[1] = '\x00'
    if cp[] == '<' or cp[] == '>' or cp[] == '=' or cp[] == '^':
      spec.align = inc(cp)[]
      haveAlign = 1
  ##  sign formatting
  if cp[] == '+' or cp[] == '-' or cp[] == ' ':
    spec.sign = inc(cp)[]
  if cp[] == '0':
    ##  zero padding implies alignment, which should not be
    ##  specified twice.
    if haveAlign:
      return 0
    spec.align = 'z'
    spec.fill[0] = inc(cp)[]
    spec.fill[1] = '\x00'
  if isdigit(cast[cuchar](cp[])):
    if cp[] == '0':
      return 0
    errno = 0
    spec.minWidth = mpdStrtossize(cp, addr(cp), 10)
    if errno == erange or errno == einval:
      return 0
  if cp[] == ',':
    spec.dot = "."
    spec.sep = ","
    spec.grouping = "\x03\x03"
    inc(cp)
  if cp[] == '.':
    inc(cp)
    if not isdigit(cast[cuchar](cp[])):
      return 0
    errno = 0
    spec.prec = mpdStrtossize(cp, addr(cp), 10)
    if errno == erange or errno == einval:
      return 0
  if cp[] == 'E' or cp[] == 'e' or cp[] == 'F' or cp[] == 'f' or cp[] == 'G' or cp[] == 'g' or
      cp[] == '%':
    spec.`type` = inc(cp)[]
  elif cp[] == 'N' or cp[] == 'n':   ##  check correctness
    ##  locale specific conversion
    var lc: ptr Lconv
    ##  separator has already been specified
    if spec.sep[]:
      return 0
    spec.`type` = inc(cp)[]
    spec.`type` = if (spec.`type` == 'N'): 'G' else: 'g'
    lc = localeconv()
    spec.dot = lc.decimalPoint
    spec.sep = lc.thousandsSep
    spec.grouping = lc.grouping
    if mpdValidateLconv(spec) < 0:
      return 0
      ##  GCOV_NOT_REACHED
  if cp[] != '\x00':
    return 0
  return 1

##
##  The following functions assume that spec->min_width <= MPD_MAX_PREC, which
##  is made sure in mpd_qformat_spec. Then, even with a spec that inserts a
##  four-byte separator after each digit, nbytes in the following struct
##  cannot overflow.
##
##  Multibyte string

type
  MpdMbstrT* {.bycopy.} = object
    nbytes*: MpdSsizeT         ##  length in bytes
    nchars*: MpdSsizeT         ##  length in chars
    cur*: MpdSsizeT            ##  current write index
    data*: cstring


proc mpdBcopy*(dest: cstring; src: cstring; n: MpdSsizeT) {.inline.} =
  while dec(n) >= 0:
    dest[n] = src[n]

proc mbstrCopyChar*(dest: ptr MpdMbstrT; src: cstring; n: MpdSsizeT) {.inline.} =
  inc(dest.nbytes, n)
  inc(dest.nchars, (if n > 0: 1 else: 0))
  dec(dest.cur, n)
  if dest.data != nil:
    mpdBcopy(dest.data + dest.cur, src, n)

proc mbstrCopyAscii*(dest: ptr MpdMbstrT; src: cstring; n: MpdSsizeT) {.inline.} =
  inc(dest.nbytes, n)
  inc(dest.nchars, n)
  dec(dest.cur, n)
  if dest.data != nil:
    mpdBcopy(dest.data + dest.cur, src, n)

proc mbstrCopyPad*(dest: ptr MpdMbstrT; n: MpdSsizeT) {.inline.} =
  inc(dest.nbytes, n)
  inc(dest.nchars, n)
  dec(dest.cur, n)
  if dest.data != nil:
    var cp: cstring = dest.data + dest.cur
    while dec(n) >= 0:
      cp[n] = '0'

##
##  Copy a numeric string to dest->data, adding separators in the integer
##  part according to spec->grouping. If leading zero padding is enabled
##  and the result is smaller than spec->min_width, continue adding zeros
##  and separators until the minimum width is reached.
##
##  The final length of dest->data is stored in dest->nbytes. The number
##  of UTF-8 characters is stored in dest->nchars.
##
##  First run (dest->data == NULL): determine the length of the result
##  string and store it in dest->nbytes.
##
##  Second run (write to dest->data): data is written in chunks and in
##  reverse order, starting with the rest of the numeric string.
##

proc mpdAddSepDot*(dest: ptr MpdMbstrT; sign: cstring; src: cstring; nSrc: MpdSsizeT;
                  dot: cstring; rest: cstring; nRest: MpdSsizeT; spec: ptr MpdSpecT) =
  ##  location of optional sign
  ##  integer part and length
  ##  location of optional decimal point
  ##  remaining part and length
  var
    nSep: MpdSsizeT
    nSign: MpdSsizeT
    consume: MpdSsizeT
  var g: cstring
  var pad: cint = 0
  nSign = if sign: 1 else: 0
  nSep = cast[MpdSsizeT](strlen(spec.sep))
  ##  Initial write index: set to location of '\0' in the output string.
  ##  Irrelevant for the first run.
  dest.cur = dest.nbytes
  dest.nbytes = dest.nchars = 0
  mbstrCopyAscii(dest, rest, nRest)
  if dot:
    mbstrCopyChar(dest, dot, cast[MpdSsizeT](strlen(dot)))
  g = spec.grouping
  consume = g[]
  while 1:
    ##  If the group length is 0 or CHAR_MAX or greater than the
    ##  number of source bytes, consume all remaining bytes.
    if g[] == 0 or g[] == char_Max or consume > nSrc:
      consume = nSrc
    dec(nSrc, consume)
    if pad:
      mbstrCopyPad(dest, consume)
    else:
      mbstrCopyAscii(dest, src + nSrc, consume)
    if nSrc == 0:
      ##  Either the real source of intpart digits or the virtual
      ##  source of padding zeros is exhausted.
      if spec.align == 'z' and dest.nchars + nSign < spec.minWidth:
        ##  Zero padding is set and length < min_width:
        ##  Generate n_src additional characters.
        nSrc = spec.minWidth - (dest.nchars + nSign)
        ##  Next iteration:
        ##    case *g == 0 || *g == CHAR_MAX:
        ##       consume all padding characters
        ##    case consume < g*:
        ##       fill remainder of current group
        ##    case consume == g*
        ##       copying is a no-op
        consume = g[] - consume
        ##  Switch on virtual source of zeros.
        pad = 1
        continue
      break
    if nSep > 0:
      ##  If padding is switched on, separators are counted
      ##  as padding characters. This rule does not apply if
      ##  the separator would be the first character of the
      ##  result string.
      if pad and nSrc > 1:
        dec(nSrc, 1)
      mbstrCopyChar(dest, spec.sep, nSep)
    if g[] and (g + 1)[]:
      inc(g)
    consume = g[]
  if sign:
    mbstrCopyAscii(dest, sign, 1)
  if dest.data:
    dest.data[dest.nbytes] = '\x00'

##
##  Convert a numeric-string to its locale-specific appearance.
##  The string must have one of these forms:
##
##      1) [sign] digits [exponent-part]
##      2) [sign] digits '.' [digits] [exponent-part]
##
##  Not allowed, since _mpd_to_string() never returns this form:
##
##      3) [sign] '.' digits [exponent-part]
##
##  Input: result->data := original numeric string (ASCII)
##         result->bytes := strlen(result->data)
##         result->nchars := strlen(result->data)
##
##  Output: result->data := modified or original string
##          result->bytes := strlen(result->data)
##          result->nchars := number of characters (possibly UTF-8)
##

proc mpdApplyLconv*(result: ptr MpdMbstrT; spec: ptr MpdSpecT; status: ptr uint32T): cint =
  var
    sign: cstring = nil
    intpart: cstring = nil
    dot: cstring = nil
  var
    rest: cstring
    dp: cstring
  var decstring: cstring
  var
    nInt: MpdSsizeT
    nRest: MpdSsizeT
  ##  original numeric string
  dp = result.data
  ##  sign
  if dp[] == '+' or dp[] == '-' or dp[] == ' ':
    sign = inc(dp)
  assert(isdigit(cast[cuchar](dp[])))
  intpart = inc(dp)
  while isdigit(cast[cuchar](dp[])):
    inc(dp)
  nInt = (mpdSsizeT)(dp - intpart)
  ##  decimal point
  if dp[] == '.':
    inc(dp)
    dot = spec.dot
  rest = dp
  nRest = result.nbytes - (mpdSsizeT)(dp - result.data)
  if dot == nil and (spec.sep[] == '\x00' or spec.grouping[] == '\x00'):
    ##  _mpd_add_sep_dot() would not change anything
    return 1
  decstring = result.data
  result.data = nil
  mpdAddSepDot(result, sign, intpart, nInt, dot, rest, nRest, spec)
  result.data = mpdAlloc(result.nbytes + 1, 1)
  if result.data == nil:
    status[] = status[] or mPD_MallocError
    mpdFree(decstring)
    return 0
  mpdAddSepDot(result, sign, intpart, nInt, dot, rest, nRest, spec)
  mpdFree(decstring)
  return 1

##  Add padding to the formatted string if necessary.

proc mpdAddPad*(result: ptr MpdMbstrT; spec: ptr MpdSpecT; status: ptr uint32T): cint =
  if result.nchars < spec.minWidth:
    var
      addChars: MpdSsizeT
      addBytes: MpdSsizeT
    var
      lpad: csize = 0
      rpad: csize = 0
    var
      nFill: csize
      len: csize
      i: csize
      j: csize
    var align: char = spec.align
    var err: uint8T = 0
    var cp: cstring
    nFill = strlen(spec.fill)
    addChars = (spec.minWidth - result.nchars)
    ##  max value: MPD_MAX_PREC * 4
    addBytes = addChars * cast[MpdSsizeT](nFill)
    cp = result.data = mpdRealloc(result.data, result.nbytes + addBytes + 1,
                              sizeof(result.data[]), addr(err))
    if err:
      status[] = status[] or mPD_MallocError
      mpdFree(result.data)
      return 0
    if align == 'z':
      align = '='
    if align == '<':
      rpad = addChars
    elif align == '>' or align == '=':
      lpad = addChars
    else:
      ##  align == '^'
      lpad = addChars div 2
      rpad = addChars - lpad
    len = result.nbytes
    if align == '=' and (cp[] == '-' or cp[] == '+' or cp[] == ' '):
      ##  leave sign in the leading position
      inc(cp)
      dec(len)
    memmove(cp + nFill * lpad, cp, len)
    i = 0
    while i < lpad:
      j = 0
      while j < nFill:
        cp[i * nFill + j] = spec.fill[j]
        inc(j)
      inc(i)
    inc(cp, (nFill * lpad + len))
    i = 0
    while i < rpad:
      j = 0
      while j < nFill:
        cp[i * nFill + j] = spec.fill[j]
        inc(j)
      inc(i)
    inc(result.nbytes, addBytes)
    inc(result.nchars, addChars)
    result.data[result.nbytes] = '\x00'
  return 1

##  Round a number to prec digits. The adjusted exponent stays the same
##    or increases by one if rounding up crosses a power of ten boundary.
##    If result->digits would exceed MPD_MAX_PREC+1, MPD_Invalid_operation
##    is set and the result is NaN.

proc mpdRound*(result: ptr MpdT; a: ptr MpdT; prec: MpdSsizeT; ctx: ptr MpdContextT;
              status: ptr uint32T) {.inline.} =
  var exp: MpdSsizeT = a.exp + a.digits - prec
  if prec <= 0:
    mpdSeterror(result, mPD_InvalidOperation, status)
    ##  GCOV_NOT_REACHED
    return
    ##  GCOV_NOT_REACHED
  if mpdIsspecial(a) or mpdIszero(a):
    mpdQcopy(result, a, status)
    var `return`: Gcov_Not_Reached
    ##  GCOV_NOT_REACHED
  mpdQrescaleFmt(result, a, exp, ctx, status)
  if result.digits > prec:
    mpdQrescaleFmt(result, result, exp + 1, ctx, status)

##
##  Return the string representation of an mpd_t, formatted according to 'spec'.
##  The format specification is assumed to be valid. Memory errors are indicated
##  as usual. This function is quiet.
##

proc mpdQformatSpec*(dec: ptr MpdT; spec: ptr MpdSpecT; ctx: ptr MpdContextT;
                    status: ptr uint32T): cstring =
  var dt: array[mpd_Minalloc_Max, MpdUintT]
  var tmp: MpdT = [mpd_Static or mpd_Static_Data, 0, 0, 0, mpd_Minalloc_Max, dt]
  var dplace: MpdSsizeT = mpd_Default_Dotplace
  var result: MpdMbstrT
  var stackspec: MpdSpecT
  var `type`: char = spec.`type`
  var flags: cint = 0
  if spec.minWidth > mpd_Max_Prec:
    status[] = status[] or mPD_InvalidOperation
    return nil
  if isupper(cast[cuchar](`type`)):
    `type` = cast[char](tolower(cast[cuchar](`type`)))
    flags = flags or mpd_Fmt_Upper
  if spec.sign == ' ':
    flags = flags or mpd_Fmt_Sign_Space
  elif spec.sign == '+':
    flags = flags or mpd_Fmt_Sign_Plus
  if mpdIsspecial(dec):
    if spec.align == 'z':
      stackspec = spec[]
      stackspec.fill[0] = ' '
      stackspec.fill[1] = '\x00'
      stackspec.align = '>'
      spec = addr(stackspec)
    assert(strlen(spec.fill) == 1)
    ##  annotation for scan-build
    if `type` == '%':
      flags = flags or mpd_Fmt_Percent
  else:
    var workstatus: uint32T = 0
    var prec: MpdSsizeT
    case `type`
    of 'g':
      flags = flags or mpd_Fmt_Tosci
    of 'e':
      flags = flags or mpd_Fmt_Exp
    of '%':
      flags = flags or mpd_Fmt_Percent
      if not mpdQcopy(addr(tmp), dec, status):
        return nil
      inc(tmp.exp, 2)
      dec = addr(tmp)
      `type` = 'f'
      ##  fall through
    of 'f':
      flags = flags or mpd_Fmt_Fixed
    else:
      abort()
      ##  debug: GCOV_NOT_REACHED
    if spec.prec >= 0:
      if spec.prec > mpd_Max_Prec:
        status[] = status[] or mPD_InvalidOperation
        break error
      case `type`
      of 'g':
        prec = if (spec.prec == 0): 1 else: spec.prec
        if dec.digits > prec:
          mpdRound(addr(tmp), dec, prec, ctx, addr(workstatus))
          dec = addr(tmp)
      of 'e':
        if mpdIszero(dec):
          dplace = 1 - spec.prec
        else:
          mpdRound(addr(tmp), dec, spec.prec + 1, ctx, addr(workstatus))
          dec = addr(tmp)
      of 'f':
        mpdQrescale(addr(tmp), dec, -spec.prec, ctx, addr(workstatus))
        dec = addr(tmp)
    if `type` == 'f':
      if mpdIszero(dec) and dec.exp > 0:
        mpdQrescale(addr(tmp), dec, 0, ctx, addr(workstatus))
        dec = addr(tmp)
    if workstatus and mPD_Errors:
      status[] = status[] or (workstatus and mPD_Errors)
      break error
  ##
  ##  At this point, for all scaled or non-scaled decimals:
  ##    1) 1 <= digits <= MAX_PREC+1
  ##    2) adjexp(scaled) = adjexp(orig) [+1]
  ##    3)   case 'g': MIN_ETINY <= exp <= MAX_EMAX+1
  ##         case 'e': MIN_ETINY-MAX_PREC <= exp <= MAX_EMAX+1
  ##         case 'f': MIN_ETINY <= exp <= MAX_EMAX+1
  ##    4) max memory alloc in _mpd_to_string:
  ##         case 'g': MAX_PREC+36
  ##         case 'e': MAX_PREC+36
  ##         case 'f': 2*MPD_MAX_PREC+30
  ##
  result.nbytes = mpdToString(addr(result.data), dec, flags, dplace)
  result.nchars = result.nbytes
  if result.nbytes < 0:
    status[] = status[] or mPD_MallocError
    break error
  if spec.dot[] != '\x00' and not mpdIsspecial(dec):
    if result.nchars > mpd_Max_Prec + 36:
      ##  Since a group length of one is not explicitly
      ##  disallowed, ensure that it is always possible to
      ##  insert a four byte separator after each digit.
      status[] = status[] or mPD_InvalidOperation
      mpdFree(result.data)
      break error
    if not mpdApplyLconv(addr(result), spec, status):
      break error
  if spec.minWidth:
    if not mpdAddPad(addr(result), spec, status):
      break error
  mpdDel(addr(tmp))
  return result.data
  mpdDel(addr(tmp))
  return nil

proc mpdQformat*(dec: ptr MpdT; fmt: cstring; ctx: ptr MpdContextT; status: ptr uint32T): cstring =
  var spec: MpdSpecT
  if not mpdParseFmtStr(addr(spec), fmt, 1):
    status[] = status[] or mPD_InvalidOperation
    return nil
  return mpdQformatSpec(dec, addr(spec), ctx, status)

##
##  The specification has a *condition* called Invalid_operation and an
##  IEEE *signal* called Invalid_operation. The former corresponds to
##  MPD_Invalid_operation, the latter to MPD_IEEE_Invalid_operation.
##  MPD_IEEE_Invalid_operation comprises the following conditions:
##
##  [MPD_Conversion_syntax, MPD_Division_impossible, MPD_Division_undefined,
##   MPD_Fpu_error, MPD_Invalid_context, MPD_Invalid_operation,
##   MPD_Malloc_error]
##
##  In the following functions, 'flag' denotes the condition, 'signal'
##  denotes the IEEE signal.
##

var mpdFlagString*: array[mpd_Num_Flags, cstring] = ["Clamped", "Conversion_syntax",
    "Division_by_zero", "Division_impossible", "Division_undefined", "Fpu_error",
    "Inexact", "Invalid_context", "Invalid_operation", "Malloc_error",
    "Not_implemented", "Overflow", "Rounded", "Subnormal", "Underflow"]

var mpdSignalString*: array[mpd_Num_Flags, cstring] = ["Clamped",
    "IEEE_Invalid_operation", "Division_by_zero", "IEEE_Invalid_operation",
    "IEEE_Invalid_operation", "IEEE_Invalid_operation", "Inexact",
    "IEEE_Invalid_operation", "IEEE_Invalid_operation", "IEEE_Invalid_operation",
    "Not_implemented", "Overflow", "Rounded", "Subnormal", "Underflow"]

##  print conditions to buffer, separated by spaces

proc mpdSnprintFlags*(dest: cstring; nmemb: cint; flags: uint32T): cint =
  var cp: cstring
  var
    n: cint
    j: cint
  assert(nmemb >= mpd_Max_Flag_String)
  dest[] = '\x00'
  cp = dest
  j = 0
  while j < mpd_Num_Flags:
    if flags and (1 shl j):
      n = snprintf(cp, nmemb, "%s ", mpdFlagString[j])
      if n < 0 or n >= nmemb:
        return -1
      inc(cp, n)
      dec(nmemb, n)
    inc(j)
  if cp != dest:
    (dec(cp))[] = '\x00'
  return (int)(cp - dest)

##  print conditions to buffer, in list form

proc mpdLsnprintFlags*(dest: cstring; nmemb: cint; flags: uint32T;
                      flagString: ptr cstring): cint =
  var cp: cstring
  var
    n: cint
    j: cint
  assert(nmemb >= mpd_Max_Flag_List)
  if flagString == nil:
    flagString = mpdFlagString
  dest[] = '['
  (dest + 1)[] = '\x00'
  cp = dest + 1
  dec(nmemb)
  j = 0
  while j < mpd_Num_Flags:
    if flags and (1 shl j):
      n = snprintf(cp, nmemb, "%s, ", flagString[j])
      if n < 0 or n >= nmemb:
        return -1
      inc(cp, n)
      dec(nmemb, n)
    inc(j)
  ##  erase the last ", "
  if cp != dest + 1:
    dec(cp, 2)
  inc(cp)[] = ']'
  cp[] = '\x00'
  return (int)(cp - dest)
  ##  strlen, without NUL terminator

##  print signals to buffer, in list form

proc mpdLsnprintSignals*(dest: cstring; nmemb: cint; flags: uint32T;
                        signalString: ptr cstring): cint =
  var cp: cstring
  var
    n: cint
    j: cint
  var ieeeInvalidDone: cint = 0
  assert(nmemb >= mpd_Max_Signal_List)
  if signalString == nil:
    signalString = mpdSignalString
  dest[] = '['
  (dest + 1)[] = '\x00'
  cp = dest + 1
  dec(nmemb)
  j = 0
  while j < mpd_Num_Flags:
    var f: uint32T = flags and (1 shl j)
    if f:
      if f and mPD_IEEE_InvalidOperation:
        if ieeeInvalidDone:
          continue
        ieeeInvalidDone = 1
      n = snprintf(cp, nmemb, "%s, ", signalString[j])
      if n < 0 or n >= nmemb:
        return -1
      inc(cp, n)
      dec(nmemb, n)
    inc(j)
  ##  erase the last ", "
  if cp != dest + 1:
    dec(cp, 2)
  inc(cp)[] = ']'
  cp[] = '\x00'
  return (int)(cp - dest)
  ##  strlen, without NUL terminator

##  The following two functions are mainly intended for debugging.

proc mpdFprint*(file: ptr File; dec: ptr MpdT) =
  var decstring: cstring
  decstring = mpdToSci(dec, 1)
  if decstring != nil:
    fprintf(file, "%s\n", decstring)
    mpdFree(decstring)
  else:
    fputs("mpd_fprint: output error\n", file)
    ##  GCOV_NOT_REACHED

##  C2NIM crashed on the next function:
##
##  void
##  mpd_print(const mpd_t *dec)
##  {
##      char *decstring;
##      decstring = mpd_to_sci(dec, 1);
##      if (decstring != NULL) {
##          printf("%s\n", decstring);
##          mpd_free(decstring);
##      }
##      else {
##          fputs("mpd_fprint: output error\n", stderr); /* GCOV_NOT_REACHED */
##      }
##  }
