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

when defined(__GNUC__) and not defined(__INTEL_COMPILER) and __GNUC__ >= 7:
##
##  Work around the behavior of tolower() and strcasecmp() in certain
##  locales. For example, in tr_TR.utf8:
##
##  tolower((unsigned char)'I') == 'I'
##
##  u is the exact uppercase version of l; n is strlen(l) or strlen(l)+1
##

proc _mpd_strneq*(s: cstring; l: cstring; u: cstring; n: csize): cint {.inline.} =
  while dec(n) != SIZE_MAX:
    if s[] != l[] and s[] != u[]:
      return 0
    inc(s)
    inc(u)
    inc(l)
  return 1

proc strtoexp*(s: cstring): mpd_ssize_t =
  var `end`: cstring
  var retval: mpd_ssize_t
  errno = 0
  retval = mpd_strtossize(s, addr(`end`), 10)
  if errno == 0 and not (s[] != '\x00' and `end`[] == '\x00'):
    errno = EINVAL
  return retval

##
##  Scan 'len' words. The most significant word contains 'r' digits,
##  the remaining words are full words. Skip dpoint. The string 's' must
##  consist of digits and an optional single decimal point at 'dpoint'.
##

proc string_to_coeff*(data: ptr mpd_uint_t; s: cstring; dpoint: cstring; r: cint;
                     len: csize) =
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
  while dec(len) != SIZE_MAX:
    data[len] = 0
    j = 0
    while j < MPD_RDIGITS:
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

proc scan_dpoint_exp*(s: cstring; dpoint: cstringArray; exp: cstringArray;
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

proc scan_payload*(s: cstring; `end`: cstringArray): cstring =
  var coeff: cstring
  while s[] == '0':
    inc(s)
  coeff = s
  while isdigit(cast[cuchar](s[])):
    inc(s)
  `end`[] = s
  return if (s[] == '\x00'): coeff else: nil

##  convert a character string to a decimal

proc mpd_qset_string*(dec: ptr mpd_t; s: cstring; ctx: ptr mpd_context_t;
                     status: ptr uint32_t) =
  var
    q: mpd_ssize_t
    r: mpd_ssize_t
    len: mpd_ssize_t
  var
    coeff: cstring
    `end`: cstring
  var
    dpoint: cstring = nil
    exp: cstring = nil
  var digits: csize
  var sign: uint8_t = MPD_POS
  mpd_set_flags(dec, 0)
  dec.len = 0
  dec.exp = 0
  ##  sign
  if s[] == '+':
    inc(s)
  elif s[] == '-':
    mpd_set_negative(dec)
    sign = MPD_NEG
    inc(s)
  if _mpd_strneq(s, "nan", "NAN", 3):
    ##  NaN
    inc(s, 3)
    mpd_setspecial(dec, sign, MPD_NAN)
    if s[] == '\x00':
      return
    if (coeff = scan_payload(s, addr(`end`))) == nil:
      break conversion_error
    if coeff[] == '\x00':
      return
    digits = `end` - coeff
    ##  prec >= 1, clamp is 0 or 1
    if digits > (size_t)(ctx.prec - ctx.clamp):
      break conversion_error
  elif _mpd_strneq(s, "snan", "SNAN", 4):
    inc(s, 4)
    mpd_setspecial(dec, sign, MPD_SNAN)
    if s[] == '\x00':
      return
    if (coeff = scan_payload(s, addr(`end`))) == nil:
      break conversion_error
    if coeff[] == '\x00':
      return
    digits = `end` - coeff
    if digits > (size_t)(ctx.prec - ctx.clamp):
      break conversion_error
  elif _mpd_strneq(s, "inf", "INF", 3):
    inc(s, 3)
    if s[] == '\x00' or _mpd_strneq(s, "inity", "INITY", 6):
      ##  numeric-value: infinity
      mpd_setspecial(dec, sign, MPD_INF)
      return
    break conversion_error
  else:
    ##  scan for start of coefficient, decimal point, indicator, end
    if (coeff = scan_dpoint_exp(s, addr(dpoint), addr(exp), addr(`end`))) == nil:
      break conversion_error
    if exp:
      ##  exponent-part
      `end` = exp
      inc(exp)
      dec.exp = strtoexp(exp)
      if errno:
        if not (errno == ERANGE and
            (dec.exp == MPD_SSIZE_MAX or dec.exp == MPD_SSIZE_MIN)):
          break conversion_error
    digits = `end` - coeff
    if dpoint:
      var fracdigits: csize = `end` - dpoint - 1
      if dpoint > coeff:
        dec(digits)
      if fracdigits > MPD_MAX_PREC:
        break conversion_error
      if dec.exp < MPD_SSIZE_MIN + cast[mpd_ssize_t](fracdigits):
        dec.exp = MPD_SSIZE_MIN
      else:
        dec(dec.exp, cast[mpd_ssize_t](fracdigits))
    if digits > MPD_MAX_PREC:
      break conversion_error
    if dec.exp > MPD_EXP_INF:
      dec.exp = MPD_EXP_INF
    if dec.exp == MPD_SSIZE_MIN:
      dec.exp = MPD_SSIZE_MIN + 1
  _mpd_idiv_word(addr(q), addr(r), cast[mpd_ssize_t](digits), MPD_RDIGITS)
  len = if (r == 0): q else: q + 1
  if len == 0:
    break conversion_error
    ##  GCOV_NOT_REACHED
  if not mpd_qresize(dec, len, status):
    mpd_seterror(dec, MPD_Malloc_error, status)
    return
  dec.len = len
  string_to_coeff(dec.data, coeff, dpoint, cast[cint](r), len)
  mpd_setdigits(dec)
  mpd_qfinalize(dec, ctx, status)
  return
  ##  standard wants a positive NaN
  mpd_seterror(dec, MPD_Conversion_syntax, status)

##  convert a character string to a decimal, use a maxcontext for conversion

proc mpd_qset_string_exact*(dec: ptr mpd_t; s: cstring; status: ptr uint32_t) =
  var maxcontext: mpd_context_t
  mpd_maxcontext(addr(maxcontext))
  mpd_qset_string(dec, s, addr(maxcontext), status)
  if status[] and (MPD_Inexact or MPD_Rounded or MPD_Clamped):
    ##  we want exact results
    mpd_seterror(dec, MPD_Invalid_operation, status)
  status[] = status[] and MPD_Errors

##  Print word x with n decimal digits to string s. dot is either NULL
##    or the location of a decimal point.
##  c2nim TODO
## #define EXTRACT_DIGIT(s, x, d, dot) \
##         if (s == dot) *s++ = '.'; *s++ = '0' + (char)(x / d); x %= d
##

proc word_to_string*(s: cstring; x: mpd_uint_t; n: cint; dot: cstring): cstring {.inline.} =
  case n ##  c2nim TODO
       ## #ifdef CONFIG_64
       ##     case 20: EXTRACT_DIGIT(s, x, 10000000000000000000ULL, dot);
       ##     case 19: EXTRACT_DIGIT(s, x, 1000000000000000000ULL, dot);
       ##     case 18: EXTRACT_DIGIT(s, x, 100000000000000000ULL, dot);
       ##     case 17: EXTRACT_DIGIT(s, x, 10000000000000000ULL, dot);
       ##     case 16: EXTRACT_DIGIT(s, x, 1000000000000000ULL, dot);
       ##     case 15: EXTRACT_DIGIT(s, x, 100000000000000ULL, dot);
       ##     case 14: EXTRACT_DIGIT(s, x, 10000000000000ULL, dot);
       ##     case 13: EXTRACT_DIGIT(s, x, 1000000000000ULL, dot);
       ##     case 12: EXTRACT_DIGIT(s, x, 100000000000ULL, dot);
       ##     case 11: EXTRACT_DIGIT(s, x, 10000000000ULL, dot);
       ## #endif
       ##
  of 10:
    EXTRACT_DIGIT(s, x, 1000000000, dot)
  of 9:
    EXTRACT_DIGIT(s, x, 100000000, dot)
  of 8:
    EXTRACT_DIGIT(s, x, 10000000, dot)
  of 7:
    EXTRACT_DIGIT(s, x, 1000000, dot)
  of 6:
    EXTRACT_DIGIT(s, x, 100000, dot)
  of 5:
    EXTRACT_DIGIT(s, x, 10000, dot)
  of 4:
    EXTRACT_DIGIT(s, x, 1000, dot)
  of 3:
    EXTRACT_DIGIT(s, x, 100, dot)
  of 2:
    EXTRACT_DIGIT(s, x, 10, dot)
  else:
    if s == dot:
      inc(s)[] = '.'
    inc(s)[] = '0' + cast[char](x)
  s[] = '\x00'
  return s

##  Print exponent x to string s. Undefined for MPD_SSIZE_MIN.

proc exp_to_string*(s: cstring; x: mpd_ssize_t): cstring {.inline.} =
  var sign: char = '+'
  if x < 0:
    sign = '-'
    x = -x
  inc(s)[] = sign
  return word_to_string(s, x, mpd_word_digits(x), nil)

##  Print the coefficient of dec to string s. len(dec) > 0.

proc coeff_to_string*(s: cstring; dec: ptr mpd_t): cstring {.inline.} =
  var x: mpd_uint_t
  var i: mpd_ssize_t
  ##  most significant word
  x = mpd_msword(dec)
  s = word_to_string(s, x, mpd_word_digits(x), nil)
  ##  remaining full words
  i = dec.len - 2
  while i >= 0:
    x = dec.data[i]
    s = word_to_string(s, x, MPD_RDIGITS, nil)
    dec(i)
  return s

##  Print the coefficient of dec to string s. len(dec) > 0. dot is either
##    NULL or a pointer to the location of a decimal point.

proc coeff_to_string_dot*(s: cstring; dot: cstring; dec: ptr mpd_t): cstring {.inline.} =
  var x: mpd_uint_t
  var i: mpd_ssize_t
  ##  most significant word
  x = mpd_msword(dec)
  s = word_to_string(s, x, mpd_word_digits(x), dot)
  ##  remaining full words
  i = dec.len - 2
  while i >= 0:
    x = dec.data[i]
    s = word_to_string(s, x, MPD_RDIGITS, dot)
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

proc _mpd_to_string*(result: cstringArray; dec: ptr mpd_t; flags: cint;
                    dplace: mpd_ssize_t): mpd_ssize_t =
  var
    decstring: cstring = nil
    cp: cstring = nil
  var ldigits: mpd_ssize_t
  var
    mem: mpd_ssize_t = 0
    k: mpd_ssize_t
  if mpd_isspecial(dec):
    mem = sizeof("-Infinity%")
    if mpd_isnan(dec) and dec.len > 0:
      ##  diagnostic code
      inc(mem, dec.digits)
    cp = decstring = mpd_alloc(mem, sizeof(decstring[]))
    if cp == nil:
      result[] = nil
      return -1
    if mpd_isnegative(dec):
      inc(cp)[] = '-'
    elif flags and MPD_FMT_SIGN_SPACE:
      inc(cp)[] = ' '
    elif flags and MPD_FMT_SIGN_PLUS:
      inc(cp)[] = '+'
    if mpd_isnan(dec):
      if mpd_isqnan(dec):
        strcpy(cp, "NaN")
        inc(cp, 3)
      else:
        strcpy(cp, "sNaN")
        inc(cp, 4)
      if dec.len > 0:
        ##  diagnostic code
        cp = coeff_to_string(cp, dec)
    elif mpd_isinfinite(dec):
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
    if flags and MPD_FMT_EXP:
      nil
    elif flags and MPD_FMT_FIXED or (dec.exp <= 0 and ldigits > -6):
      ##  MPD_FMT_FIXED: always use fixed point notation.
      ##  MPD_FMT_TOSCI, MPD_FMT_TOENG: for a certain range,
      ##  override exponent notation.
      dplace = ldigits
    elif flags and MPD_FMT_TOENG: ##
                              ##  Basic space requirements:
                              ##
                              ##  [-][.][coeffdigits][E][-][expdigits+1][%]['\0']
                              ##
                              ##  If the decimal point lies outside of the coefficient digits,
                              ##  space is adjusted accordingly.
                              ##
      if mpd_iszero(dec):
        ##  If the exponent is divisible by three,
        ##  dplace = 1. Otherwise, move dplace one
        ##  or two places to the left.
        dplace = -1 + mod_mpd_ssize_t(dec.exp + 2, 3)
      else:
        ##  ldigits-1 is the adjusted exponent, which
        ##  should be divisible by three. If not, move
        ##  dplace one or two places to the right.
        inc(dplace, mod_mpd_ssize_t(ldigits - 1, 3))
    if dplace <= 0:
      mem = -dplace + dec.digits + 2
    elif dplace >= dec.digits:
      mem = dplace
    else:
      mem = dec.digits
    inc(mem, (MPD_EXPDIGITS + 1 + 6))
    cp = decstring = mpd_alloc(mem, sizeof(decstring[]))
    if cp == nil:
      result[] = nil
      return -1
    if mpd_isnegative(dec):
      inc(cp)[] = '-'
    elif flags and MPD_FMT_SIGN_SPACE:
      inc(cp)[] = ' '
    elif flags and MPD_FMT_SIGN_PLUS:
      inc(cp)[] = '+'
    if dplace <= 0:
      ##  space: -dplace+dec->digits+2
      inc(cp)[] = '0'
      inc(cp)[] = '.'
      k = 0
      while k < -dplace:
        inc(cp)[] = '0'
        inc(k)
      cp = coeff_to_string(cp, dec)
    elif dplace >= dec.digits:
      ##  space: dplace
      cp = coeff_to_string(cp, dec)
      k = 0
      while k < dplace - dec.digits:
        inc(cp)[] = '0'
        inc(k)
    else:
      ##  space: dec->digits+1
      cp = coeff_to_string_dot(cp, cp + dplace, dec)
    ##
    ##  Conditions for printing an exponent:
    ##
    ##    MPD_FMT_TOSCI, MPD_FMT_TOENG: only if ldigits != dplace
    ##    MPD_FMT_FIXED:                never (ldigits == dplace)
    ##    MPD_FMT_EXP:                  always
    ##
    if ldigits != dplace or flags and MPD_FMT_EXP:
      ##  space: expdigits+2
      inc(cp)[] = if (flags and MPD_FMT_UPPER): 'E' else: 'e'
      cp = exp_to_string(cp, ldigits - dplace)
  if flags and MPD_FMT_PERCENT:
    inc(cp)[] = '%'
  assert(cp < decstring + mem)
  assert(cp - decstring < MPD_SSIZE_MAX)
  cp[] = '\x00'
  result[] = decstring
  return (mpd_ssize_t)(cp - decstring)

proc mpd_to_sci*(dec: ptr mpd_t; fmt: cint): cstring =
  var res: cstring
  var flags: cint = MPD_FMT_TOSCI
  flags = flags or if fmt: MPD_FMT_UPPER else: MPD_FMT_LOWER
  cast[nil](_mpd_to_string(addr(res), dec, flags, MPD_DEFAULT_DOTPLACE))
  return res

proc mpd_to_eng*(dec: ptr mpd_t; fmt: cint): cstring =
  var res: cstring
  var flags: cint = MPD_FMT_TOENG
  flags = flags or if fmt: MPD_FMT_UPPER else: MPD_FMT_LOWER
  cast[nil](_mpd_to_string(addr(res), dec, flags, MPD_DEFAULT_DOTPLACE))
  return res

proc mpd_to_sci_size*(res: cstringArray; dec: ptr mpd_t; fmt: cint): mpd_ssize_t =
  var flags: cint = MPD_FMT_TOSCI
  flags = flags or if fmt: MPD_FMT_UPPER else: MPD_FMT_LOWER
  return _mpd_to_string(res, dec, flags, MPD_DEFAULT_DOTPLACE)

proc mpd_to_eng_size*(res: cstringArray; dec: ptr mpd_t; fmt: cint): mpd_ssize_t =
  var flags: cint = MPD_FMT_TOENG
  flags = flags or if fmt: MPD_FMT_UPPER else: MPD_FMT_LOWER
  return _mpd_to_string(res, dec, flags, MPD_DEFAULT_DOTPLACE)

##  Copy a single UTF-8 char to dest. See: The Unicode Standard, version 5.2,
##    chapter 3.9: Well-formed UTF-8 byte sequences.

proc _mpd_copy_utf8*(dest: array[5, char]; s: cstring): cint =
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

proc mpd_validate_lconv*(spec: ptr mpd_spec_t): cint =
  var n: csize
  when CHAR_MAX == SCHAR_MAX:
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

proc mpd_parse_fmt_str*(spec: ptr mpd_spec_t; fmt: cstring; caps: cint): cint =
  var cp: cstring = cast[cstring](fmt)
  var
    have_align: cint = 0
    n: cint
  ##  defaults
  spec.min_width = 0
  spec.prec = -1
  spec.`type` = if caps: 'G' else: 'g'
  spec.align = '>'
  spec.sign = '-'
  spec.dot = ""
  spec.sep = ""
  spec.grouping = ""
  ##  presume that the first character is a UTF-8 fill character
  if (n = _mpd_copy_utf8(spec.fill, cp)) < 0:
    return 0
  if cp[] and
      ((cp + n)[] == '<' or (cp + n)[] == '>' or (cp + n)[] == '=' or (cp + n)[] == '^'):
    inc(cp, n)
    spec.align = inc(cp)[]
    have_align = 1
  else:
    ##  default fill character
    spec.fill[0] = ' '
    spec.fill[1] = '\x00'
    if cp[] == '<' or cp[] == '>' or cp[] == '=' or cp[] == '^':
      spec.align = inc(cp)[]
      have_align = 1
  ##  sign formatting
  if cp[] == '+' or cp[] == '-' or cp[] == ' ':
    spec.sign = inc(cp)[]
  if cp[] == '0':
    ##  zero padding implies alignment, which should not be
    ##  specified twice.
    if have_align:
      return 0
    spec.align = 'z'
    spec.fill[0] = inc(cp)[]
    spec.fill[1] = '\x00'
  if isdigit(cast[cuchar](cp[])):
    if cp[] == '0':
      return 0
    errno = 0
    spec.min_width = mpd_strtossize(cp, addr(cp), 10)
    if errno == ERANGE or errno == EINVAL:
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
    spec.prec = mpd_strtossize(cp, addr(cp), 10)
    if errno == ERANGE or errno == EINVAL:
      return 0
  if cp[] == 'E' or cp[] == 'e' or cp[] == 'F' or cp[] == 'f' or cp[] == 'G' or cp[] == 'g' or
      cp[] == '%':
    spec.`type` = inc(cp)[]
  elif cp[] == 'N' or cp[] == 'n':   ##  check correctness
    ##  locale specific conversion
    var lc: ptr lconv
    ##  separator has already been specified
    if spec.sep[]:
      return 0
    spec.`type` = inc(cp)[]
    spec.`type` = if (spec.`type` == 'N'): 'G' else: 'g'
    lc = localeconv()
    spec.dot = lc.decimal_point
    spec.sep = lc.thousands_sep
    spec.grouping = lc.grouping
    if mpd_validate_lconv(spec) < 0:
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
  mpd_mbstr_t* {.bycopy.} = object
    nbytes*: mpd_ssize_t       ##  length in bytes
    nchars*: mpd_ssize_t       ##  length in chars
    cur*: mpd_ssize_t          ##  current write index
    data*: cstring


proc _mpd_bcopy*(dest: cstring; src: cstring; n: mpd_ssize_t) {.inline.} =
  while dec(n) >= 0:
    dest[n] = src[n]

proc _mbstr_copy_char*(dest: ptr mpd_mbstr_t; src: cstring; n: mpd_ssize_t) {.inline.} =
  inc(dest.nbytes, n)
  inc(dest.nchars, (if n > 0: 1 else: 0))
  dec(dest.cur, n)
  if dest.data != nil:
    _mpd_bcopy(dest.data + dest.cur, src, n)

proc _mbstr_copy_ascii*(dest: ptr mpd_mbstr_t; src: cstring; n: mpd_ssize_t) {.inline.} =
  inc(dest.nbytes, n)
  inc(dest.nchars, n)
  dec(dest.cur, n)
  if dest.data != nil:
    _mpd_bcopy(dest.data + dest.cur, src, n)

proc _mbstr_copy_pad*(dest: ptr mpd_mbstr_t; n: mpd_ssize_t) {.inline.} =
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

proc _mpd_add_sep_dot*(dest: ptr mpd_mbstr_t; sign: cstring; src: cstring;
                      n_src: mpd_ssize_t; dot: cstring; rest: cstring;
                      n_rest: mpd_ssize_t; spec: ptr mpd_spec_t) =
  ##  location of optional sign
  ##  integer part and length
  ##  location of optional decimal point
  ##  remaining part and length
  var
    n_sep: mpd_ssize_t
    n_sign: mpd_ssize_t
    consume: mpd_ssize_t
  var g: cstring
  var pad: cint = 0
  n_sign = if sign: 1 else: 0
  n_sep = cast[mpd_ssize_t](strlen(spec.sep))
  ##  Initial write index: set to location of '\0' in the output string.
  ##  Irrelevant for the first run.
  dest.cur = dest.nbytes
  dest.nbytes = dest.nchars = 0
  _mbstr_copy_ascii(dest, rest, n_rest)
  if dot:
    _mbstr_copy_char(dest, dot, cast[mpd_ssize_t](strlen(dot)))
  g = spec.grouping
  consume = g[]
  while 1:
    ##  If the group length is 0 or CHAR_MAX or greater than the
    ##  number of source bytes, consume all remaining bytes.
    if g[] == 0 or g[] == CHAR_MAX or consume > n_src:
      consume = n_src
    dec(n_src, consume)
    if pad:
      _mbstr_copy_pad(dest, consume)
    else:
      _mbstr_copy_ascii(dest, src + n_src, consume)
    if n_src == 0:
      ##  Either the real source of intpart digits or the virtual
      ##  source of padding zeros is exhausted.
      if spec.align == 'z' and dest.nchars + n_sign < spec.min_width:
        ##  Zero padding is set and length < min_width:
        ##  Generate n_src additional characters.
        n_src = spec.min_width - (dest.nchars + n_sign)
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
    if n_sep > 0:
      ##  If padding is switched on, separators are counted
      ##  as padding characters. This rule does not apply if
      ##  the separator would be the first character of the
      ##  result string.
      if pad and n_src > 1:
        dec(n_src, 1)
      _mbstr_copy_char(dest, spec.sep, n_sep)
    if g[] and (g + 1)[]:
      inc(g)
    consume = g[]
  if sign:
    _mbstr_copy_ascii(dest, sign, 1)
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

proc _mpd_apply_lconv*(result: ptr mpd_mbstr_t; spec: ptr mpd_spec_t;
                      status: ptr uint32_t): cint =
  var
    sign: cstring = nil
    intpart: cstring = nil
    dot: cstring = nil
  var
    rest: cstring
    dp: cstring
  var decstring: cstring
  var
    n_int: mpd_ssize_t
    n_rest: mpd_ssize_t
  ##  original numeric string
  dp = result.data
  ##  sign
  if dp[] == '+' or dp[] == '-' or dp[] == ' ':
    sign = inc(dp)
  assert(isdigit(cast[cuchar](dp[])))
  intpart = inc(dp)
  while isdigit(cast[cuchar](dp[])):
    inc(dp)
  n_int = (mpd_ssize_t)(dp - intpart)
  ##  decimal point
  if dp[] == '.':
    inc(dp)
    dot = spec.dot
  rest = dp
  n_rest = result.nbytes - (mpd_ssize_t)(dp - result.data)
  if dot == nil and (spec.sep[] == '\x00' or spec.grouping[] == '\x00'):
    ##  _mpd_add_sep_dot() would not change anything
    return 1
  decstring = result.data
  result.data = nil
  _mpd_add_sep_dot(result, sign, intpart, n_int, dot, rest, n_rest, spec)
  result.data = mpd_alloc(result.nbytes + 1, 1)
  if result.data == nil:
    status[] = status[] or MPD_Malloc_error
    mpd_free(decstring)
    return 0
  _mpd_add_sep_dot(result, sign, intpart, n_int, dot, rest, n_rest, spec)
  mpd_free(decstring)
  return 1

##  Add padding to the formatted string if necessary.

proc _mpd_add_pad*(result: ptr mpd_mbstr_t; spec: ptr mpd_spec_t; status: ptr uint32_t): cint =
  if result.nchars < spec.min_width:
    var
      add_chars: mpd_ssize_t
      add_bytes: mpd_ssize_t
    var
      lpad: csize = 0
      rpad: csize = 0
    var
      n_fill: csize
      len: csize
      i: csize
      j: csize
    var align: char = spec.align
    var err: uint8_t = 0
    var cp: cstring
    n_fill = strlen(spec.fill)
    add_chars = (spec.min_width - result.nchars)
    ##  max value: MPD_MAX_PREC * 4
    add_bytes = add_chars * cast[mpd_ssize_t](n_fill)
    cp = result.data = mpd_realloc(result.data, result.nbytes + add_bytes + 1,
                               sizeof(result.data[]), addr(err))
    if err:
      status[] = status[] or MPD_Malloc_error
      mpd_free(result.data)
      return 0
    if align == 'z':
      align = '='
    if align == '<':
      rpad = add_chars
    elif align == '>' or align == '=':
      lpad = add_chars
    else:
      ##  align == '^'
      lpad = add_chars div 2
      rpad = add_chars - lpad
    len = result.nbytes
    if align == '=' and (cp[] == '-' or cp[] == '+' or cp[] == ' '):
      ##  leave sign in the leading position
      inc(cp)
      dec(len)
    memmove(cp + n_fill * lpad, cp, len)
    i = 0
    while i < lpad:
      j = 0
      while j < n_fill:
        cp[i * n_fill + j] = spec.fill[j]
        inc(j)
      inc(i)
    inc(cp, (n_fill * lpad + len))
    i = 0
    while i < rpad:
      j = 0
      while j < n_fill:
        cp[i * n_fill + j] = spec.fill[j]
        inc(j)
      inc(i)
    inc(result.nbytes, add_bytes)
    inc(result.nchars, add_chars)
    result.data[result.nbytes] = '\x00'
  return 1

##  Round a number to prec digits. The adjusted exponent stays the same
##    or increases by one if rounding up crosses a power of ten boundary.
##    If result->digits would exceed MPD_MAX_PREC+1, MPD_Invalid_operation
##    is set and the result is NaN.

proc _mpd_round*(result: ptr mpd_t; a: ptr mpd_t; prec: mpd_ssize_t;
                ctx: ptr mpd_context_t; status: ptr uint32_t) {.inline.} =
  var exp: mpd_ssize_t = a.exp + a.digits - prec
  if prec <= 0:
    mpd_seterror(result, MPD_Invalid_operation, status)
    ##  GCOV_NOT_REACHED
    return
    ##  GCOV_NOT_REACHED
  if mpd_isspecial(a) or mpd_iszero(a):
    mpd_qcopy(result, a, status)
    ##  GCOV_NOT_REACHED
    return
    ##  GCOV_NOT_REACHED
  mpd_qrescale_fmt(result, a, exp, ctx, status)
  if result.digits > prec:
    mpd_qrescale_fmt(result, result, exp + 1, ctx, status)

##
##  Return the string representation of an mpd_t, formatted according to 'spec'.
##  The format specification is assumed to be valid. Memory errors are indicated
##  as usual. This function is quiet.
##

proc mpd_qformat_spec*(dec: ptr mpd_t; spec: ptr mpd_spec_t; ctx: ptr mpd_context_t;
                      status: ptr uint32_t): cstring =
  var dt: array[MPD_MINALLOC_MAX, mpd_uint_t]
  var tmp: mpd_t = [MPD_STATIC or MPD_STATIC_DATA, 0, 0, 0, MPD_MINALLOC_MAX, dt]
  var dplace: mpd_ssize_t = MPD_DEFAULT_DOTPLACE
  var result: mpd_mbstr_t
  var stackspec: mpd_spec_t
  var `type`: char = spec.`type`
  var flags: cint = 0
  if spec.min_width > MPD_MAX_PREC:
    status[] = status[] or MPD_Invalid_operation
    return nil
  if isupper(cast[cuchar](`type`)):
    `type` = cast[char](tolower(cast[cuchar](`type`)))
    flags = flags or MPD_FMT_UPPER
  if spec.sign == ' ':
    flags = flags or MPD_FMT_SIGN_SPACE
  elif spec.sign == '+':
    flags = flags or MPD_FMT_SIGN_PLUS
  if mpd_isspecial(dec):
    if spec.align == 'z':
      stackspec = spec[]
      stackspec.fill[0] = ' '
      stackspec.fill[1] = '\x00'
      stackspec.align = '>'
      spec = addr(stackspec)
    assert(strlen(spec.fill) == 1)
    ##  annotation for scan-build
    if `type` == '%':
      flags = flags or MPD_FMT_PERCENT
  else:
    var workstatus: uint32_t = 0
    var prec: mpd_ssize_t
    case `type`
    of 'g':
      flags = flags or MPD_FMT_TOSCI
    of 'e':
      flags = flags or MPD_FMT_EXP
    of '%':
      flags = flags or MPD_FMT_PERCENT
      if not mpd_qcopy(addr(tmp), dec, status):
        return nil
      inc(tmp.exp, 2)
      dec = addr(tmp)
      `type` = 'f'
      ##  fall through
    of 'f':
      flags = flags or MPD_FMT_FIXED
    else:
      abort()
      ##  debug: GCOV_NOT_REACHED
    if spec.prec >= 0:
      if spec.prec > MPD_MAX_PREC:
        status[] = status[] or MPD_Invalid_operation
        break error
      case `type`
      of 'g':
        prec = if (spec.prec == 0): 1 else: spec.prec
        if dec.digits > prec:
          _mpd_round(addr(tmp), dec, prec, ctx, addr(workstatus))
          dec = addr(tmp)
      of 'e':
        if mpd_iszero(dec):
          dplace = 1 - spec.prec
        else:
          _mpd_round(addr(tmp), dec, spec.prec + 1, ctx, addr(workstatus))
          dec = addr(tmp)
      of 'f':
        mpd_qrescale(addr(tmp), dec, -spec.prec, ctx, addr(workstatus))
        dec = addr(tmp)
    if `type` == 'f':
      if mpd_iszero(dec) and dec.exp > 0:
        mpd_qrescale(addr(tmp), dec, 0, ctx, addr(workstatus))
        dec = addr(tmp)
    if workstatus and MPD_Errors:
      status[] = status[] or (workstatus and MPD_Errors)
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
  result.nbytes = _mpd_to_string(addr(result.data), dec, flags, dplace)
  result.nchars = result.nbytes
  if result.nbytes < 0:
    status[] = status[] or MPD_Malloc_error
    break error
  if spec.dot[] != '\x00' and not mpd_isspecial(dec):
    if result.nchars > MPD_MAX_PREC + 36:
      ##  Since a group length of one is not explicitly
      ##  disallowed, ensure that it is always possible to
      ##  insert a four byte separator after each digit.
      status[] = status[] or MPD_Invalid_operation
      mpd_free(result.data)
      break error
    if not _mpd_apply_lconv(addr(result), spec, status):
      break error
  if spec.min_width:
    if not _mpd_add_pad(addr(result), spec, status):
      break error
  mpd_del(addr(tmp))
  return result.data
  mpd_del(addr(tmp))
  return nil

proc mpd_qformat*(dec: ptr mpd_t; fmt: cstring; ctx: ptr mpd_context_t;
                 status: ptr uint32_t): cstring =
  var spec: mpd_spec_t
  if not mpd_parse_fmt_str(addr(spec), fmt, 1):
    status[] = status[] or MPD_Invalid_operation
    return nil
  return mpd_qformat_spec(dec, addr(spec), ctx, status)

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

var mpd_flag_string*: array[MPD_NUM_FLAGS, cstring] = ["Clamped", "Conversion_syntax",
    "Division_by_zero", "Division_impossible", "Division_undefined", "Fpu_error",
    "Inexact", "Invalid_context", "Invalid_operation", "Malloc_error",
    "Not_implemented", "Overflow", "Rounded", "Subnormal", "Underflow"]

var mpd_signal_string*: array[MPD_NUM_FLAGS, cstring] = ["Clamped",
    "IEEE_Invalid_operation", "Division_by_zero", "IEEE_Invalid_operation",
    "IEEE_Invalid_operation", "IEEE_Invalid_operation", "Inexact",
    "IEEE_Invalid_operation", "IEEE_Invalid_operation", "IEEE_Invalid_operation",
    "Not_implemented", "Overflow", "Rounded", "Subnormal", "Underflow"]

##  print conditions to buffer, separated by spaces

proc mpd_snprint_flags*(dest: cstring; nmemb: cint; flags: uint32_t): cint =
  var cp: cstring
  var
    n: cint
    j: cint
  assert(nmemb >= MPD_MAX_FLAG_STRING)
  dest[] = '\x00'
  cp = dest
  j = 0
  while j < MPD_NUM_FLAGS:
    if flags and (1 shl j):
      n = snprintf(cp, nmemb, "%s ", mpd_flag_string[j])
      if n < 0 or n >= nmemb:
        return -1
      inc(cp, n)
      dec(nmemb, n)
    inc(j)
  if cp != dest:
    (dec(cp))[] = '\x00'
  return (int)(cp - dest)

##  print conditions to buffer, in list form

proc mpd_lsnprint_flags*(dest: cstring; nmemb: cint; flags: uint32_t;
                        flag_string: ptr cstring): cint =
  var cp: cstring
  var
    n: cint
    j: cint
  assert(nmemb >= MPD_MAX_FLAG_LIST)
  if flag_string == nil:
    flag_string = mpd_flag_string
  dest[] = '['
  (dest + 1)[] = '\x00'
  cp = dest + 1
  dec(nmemb)
  j = 0
  while j < MPD_NUM_FLAGS:
    if flags and (1 shl j):
      n = snprintf(cp, nmemb, "%s, ", flag_string[j])
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

proc mpd_lsnprint_signals*(dest: cstring; nmemb: cint; flags: uint32_t;
                          signal_string: ptr cstring): cint =
  var cp: cstring
  var
    n: cint
    j: cint
  var ieee_invalid_done: cint = 0
  assert(nmemb >= MPD_MAX_SIGNAL_LIST)
  if signal_string == nil:
    signal_string = mpd_signal_string
  dest[] = '['
  (dest + 1)[] = '\x00'
  cp = dest + 1
  dec(nmemb)
  j = 0
  while j < MPD_NUM_FLAGS:
    var f: uint32_t = flags and (1 shl j)
    if f:
      if f and MPD_IEEE_Invalid_operation:
        if ieee_invalid_done:
          continue
        ieee_invalid_done = 1
      n = snprintf(cp, nmemb, "%s, ", signal_string[j])
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

proc mpd_fprint*(file: ptr FILE; dec: ptr mpd_t) =
  var decstring: cstring
  decstring = mpd_to_sci(dec, 1)
  if decstring != nil:
    fprintf(file, "%s\n", decstring)
    mpd_free(decstring)
  else:
    fputs("mpd_fprint: output error\n", file)
    ##  GCOV_NOT_REACHED

proc mpd_print*(dec: ptr mpd_t) =
  var decstring: cstring
  decstring = mpd_to_sci(dec, 1)
  if decstring != nil:
    printf("%s\n", decstring)
    mpd_free(decstring)
  else:
    fputs("mpd_fprint: output error\n", stderr)
    ##  GCOV_NOT_REACHED
