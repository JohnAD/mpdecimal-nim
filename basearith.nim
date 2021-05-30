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
  mpdecimal, constants, typearith

## *******************************************************************
##                    Calculations in base MPD_RADIX
## *******************************************************************
##
##  Knuth, TAOCP, Volume 2, 4.3.1:
##     w := sum of u (len m) and v (len n)
##     n > 0 and m >= n
##  The calling function has to handle a possible final carry.
##

proc _mpd_baseadd*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; v: ptr mpd_uint_t; m: mpd_size_t;
                  n: mpd_size_t): mpd_uint_t =
  var s: mpd_uint_t
  var carry: mpd_uint_t = 0
  var i: mpd_size_t
  assert(n > 0 and m >= n)
  ##  add n members of u and v
  i = 0
  while i < n:
    s = u[i] + (v[i] + carry)
    carry = (s < u[i]) or (s >= MPD_RADIX)
    w[i] = if carry: s - MPD_RADIX else: s
    inc(i)
  ##  if there is a carry, propagate it
  while carry and i < m:
    s = u[i] + carry
    carry = (s == MPD_RADIX)
    w[i] = if carry: 0 else: s
    inc(i)
  ##  copy the rest of u
  while i < m:
    w[i] = u[i]
    inc(i)
  return carry

##
##  Add the contents of u to w. Carries are propagated further. The caller
##  has to make sure that w is big enough.
##

proc _mpd_baseaddto*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; n: mpd_size_t) =
  var s: mpd_uint_t
  var carry: mpd_uint_t = 0
  var i: mpd_size_t
  if n == 0:
    return
  i = 0
  while i < n:
    s = w[i] + (u[i] + carry)
    carry = (s < w[i]) or (s >= MPD_RADIX)
    w[i] = if carry: s - MPD_RADIX else: s
    inc(i)
  ##  if there is a carry, propagate it
  while carry:
    s = w[i] + carry
    carry = (s == MPD_RADIX)
    w[i] = if carry: 0 else: s
    inc(i)

##
##  Add v to w (len m). The calling function has to handle a possible
##  final carry. Assumption: m > 0.
##

proc _mpd_shortadd*(w: ptr mpd_uint_t; m: mpd_size_t; v: mpd_uint_t): mpd_uint_t =
  var s: mpd_uint_t
  var carry: mpd_uint_t
  var i: mpd_size_t
  assert(m > 0)
  ##  add v to w
  s = w[0] + v
  carry = (s < v) or (s >= MPD_RADIX)
  w[0] = if carry: s - MPD_RADIX else: s
  ##  if there is a carry, propagate it
  i = 1
  while carry and i < m:
    s = w[i] + carry
    carry = (s == MPD_RADIX)
    w[i] = if carry: 0 else: s
    inc(i)
  return carry

##  Increment u. The calling function has to handle a possible carry.

proc _mpd_baseincr*(u: ptr mpd_uint_t; n: mpd_size_t): mpd_uint_t =
  var s: mpd_uint_t
  var carry: mpd_uint_t = 1
  var i: mpd_size_t
  assert(n > 0)
  ##  if there is a carry, propagate it
  i = 0
  while carry and i < n:
    s = u[i] + carry
    carry = (s == MPD_RADIX)
    u[i] = if carry: 0 else: s
    inc(i)
  return carry

##
##  Knuth, TAOCP, Volume 2, 4.3.1:
##      w := difference of u (len m) and v (len n).
##      number in u >= number in v;
##

proc _mpd_basesub*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; v: ptr mpd_uint_t; m: mpd_size_t;
                  n: mpd_size_t) =
  var d: mpd_uint_t
  var borrow: mpd_uint_t = 0
  var i: mpd_size_t
  assert(m > 0 and n > 0)
  ##  subtract n members of v from u
  i = 0
  while i < n:
    d = u[i] - (v[i] + borrow)
    borrow = (u[i] < d)
    w[i] = if borrow: d + MPD_RADIX else: d
    inc(i)
  ##  if there is a borrow, propagate it
  while borrow and i < m:
    d = u[i] - borrow
    borrow = (u[i] == 0)
    w[i] = if borrow: MPD_RADIX - 1 else: d
    inc(i)
  ##  copy the rest of u
  while i < m:
    w[i] = u[i]
    inc(i)

##
##  Subtract the contents of u from w. w is larger than u. Borrows are
##  propagated further, but eventually w can absorb the final borrow.
##

proc _mpd_basesubfrom*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; n: mpd_size_t) =
  var d: mpd_uint_t
  var borrow: mpd_uint_t = 0
  var i: mpd_size_t
  if n == 0:
    return
  i = 0
  while i < n:
    d = w[i] - (u[i] + borrow)
    borrow = (w[i] < d)
    w[i] = if borrow: d + MPD_RADIX else: d
    inc(i)
  ##  if there is a borrow, propagate it
  while borrow:
    d = w[i] - borrow
    borrow = (w[i] == 0)
    w[i] = if borrow: MPD_RADIX - 1 else: d
    inc(i)

##  w := product of u (len n) and v (single word)

proc _mpd_shortmul*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; n: mpd_size_t; v: mpd_uint_t) =
  var
    hi: mpd_uint_t
    lo: mpd_uint_t
  var carry: mpd_uint_t = 0
  var i: mpd_size_t
  assert(n > 0)
  i = 0
  while i < n:
    _mpd_mul_words(addr(hi), addr(lo), u[i], v)
    lo = carry + lo
    if lo < carry:
      inc(hi)
    _mpd_div_words_r(addr(carry), addr(w[i]), hi, lo)
    inc(i)
  w[i] = carry

##
##  Knuth, TAOCP, Volume 2, 4.3.1:
##      w := product of u (len m) and v (len n)
##      w must be initialized to zero
##

proc _mpd_basemul*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; v: ptr mpd_uint_t; m: mpd_size_t;
                  n: mpd_size_t) =
  var
    hi: mpd_uint_t
    lo: mpd_uint_t
  var carry: mpd_uint_t
  var
    i: mpd_size_t
    j: mpd_size_t
  assert(m > 0 and n > 0)
  j = 0
  while j < n:
    carry = 0
    i = 0
    while i < m:
      _mpd_mul_words(addr(hi), addr(lo), u[i], v[j])
      lo = w[i + j] + lo
      if lo < w[i + j]:
        inc(hi)
      lo = carry + lo
      if lo < carry:
        inc(hi)
      _mpd_div_words_r(addr(carry), addr(w[i + j]), hi, lo)
      inc(i)
    w[j + m] = carry
    inc(j)

##
##  Knuth, TAOCP Volume 2, 4.3.1, exercise 16:
##      w := quotient of u (len n) divided by a single word v
##

proc _mpd_shortdiv*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; n: mpd_size_t; v: mpd_uint_t): mpd_uint_t =
  var
    hi: mpd_uint_t
    lo: mpd_uint_t
  var rem: mpd_uint_t = 0
  var i: mpd_size_t
  assert(n > 0)
  i = n - 1
  while i != MPD_SIZE_MAX:
    _mpd_mul_words(addr(hi), addr(lo), rem, MPD_RADIX)
    lo = u[i] + lo
    if lo < u[i]:
      inc(hi)
    _mpd_div_words(addr(w[i]), addr(rem), hi, lo, v)
    dec(i)
  return rem

##
##  Knuth, TAOCP Volume 2, 4.3.1:
##      q, r := quotient and remainder of uconst (len nplusm)
##              divided by vconst (len n)
##      nplusm >= n
##
##  If r is not NULL, r will contain the remainder. If r is NULL, the
##  return value indicates if there is a remainder: 1 for true, 0 for
##  false.  A return value of -1 indicates an error.
##

proc _mpd_basedivmod*(q: ptr mpd_uint_t; r: ptr mpd_uint_t; uconst: ptr mpd_uint_t;
                     vconst: ptr mpd_uint_t; nplusm: mpd_size_t; n: mpd_size_t): cint =
  var ustatic: array[MPD_MINALLOC_MAX, mpd_uint_t]
  var vstatic: array[MPD_MINALLOC_MAX, mpd_uint_t]
  var u: ptr mpd_uint_t = ustatic
  var v: ptr mpd_uint_t = vstatic
  var
    d: mpd_uint_t
    qhat: mpd_uint_t
    rhat: mpd_uint_t
    w2: array[2, mpd_uint_t]
  var
    hi: mpd_uint_t
    lo: mpd_uint_t
    x: mpd_uint_t
  var carry: mpd_uint_t
  var
    i: mpd_size_t
    j: mpd_size_t
    m: mpd_size_t
  var retval: cint = 0
  assert(n > 1 and nplusm >= n)
  m = sub_size_t(nplusm, n)
  ##  D1: normalize
  d = MPD_RADIX div (vconst[n - 1] + 1)
  if nplusm >= MPD_MINALLOC_MAX:
    if (u = mpd_alloc(nplusm + 1, sizeof(u[]))) == nil:
      return -1
  if n >= MPD_MINALLOC_MAX:
    if (v = mpd_alloc(n + 1, sizeof(v[]))) == nil:
      mpd_free(u)
      return -1
  _mpd_shortmul(u, uconst, nplusm, d)
  _mpd_shortmul(v, vconst, n, d)
  ##  D2: loop
  j = m
  while j != MPD_SIZE_MAX:
    assert(2 <= j + n and j + n <= nplusm)
    ##  annotation for scan-build
    ##  D3: calculate qhat and rhat
    rhat = _mpd_shortdiv(w2, u + j + n - 1, 2, v[n - 1])
    qhat = w2[1] * MPD_RADIX + w2[0]
    while 1:
      if qhat < MPD_RADIX:
        _mpd_singlemul(w2, qhat, v[n - 2])
        if w2[1] <= rhat:
          if w2[1] != rhat or w2[0] <= u[j + n - 2]:
            break
      dec(qhat, 1)
      inc(rhat, v[n - 1])
      if rhat < v[n - 1] or rhat >= MPD_RADIX:
        break
    ##  D4: multiply and subtract
    carry = 0
    i = 0
    while i <= n:
      _mpd_mul_words(addr(hi), addr(lo), qhat, v[i])
      lo = carry + lo
      if lo < carry:
        inc(hi)
      _mpd_div_words_r(addr(hi), addr(lo), hi, lo)
      x = u[i + j] - lo
      carry = (u[i + j] < x)
      u[i + j] = if carry: x + MPD_RADIX else: x
      inc(carry, hi)
      inc(i)
    q[j] = qhat
    ##  D5: test remainder
    if carry:
      dec(q[j], 1)
      ##  D6: add back
      cast[nil](_mpd_baseadd(u + j, u + j, v, n + 1, n))
    dec(j)
  ##  D8: unnormalize
  if r != nil:
    _mpd_shortdiv(r, u, n, d)
    ##  we are not interested in the return value here
    retval = 0
  else:
    retval = not _mpd_isallzero(u, n)
  if u != ustatic:
    mpd_free(u)
  if v != vstatic:
    mpd_free(v)
  return retval

##
##  Left shift of src by 'shift' digits; src may equal dest.
##
##   dest := area of n mpd_uint_t with space for srcdigits+shift digits.
##   src  := coefficient with length m.
##
##  The case splits in the function are non-obvious. The following
##  equations might help:
##
##   Let msdigits denote the number of digits in the most significant
##   word of src. Then 1 <= msdigits <= rdigits.
##
##    1) shift = q * rdigits + r
##    2) srcdigits = qsrc * rdigits + msdigits
##    3) destdigits = shift + srcdigits
##                  = q * rdigits + r + qsrc * rdigits + msdigits
##                  = q * rdigits + (qsrc * rdigits + (r + msdigits))
##
##   The result has q zero words, followed by the coefficient that
##   is left-shifted by r. The case r == 0 is trivial. For r > 0, it
##   is important to keep in mind that we always read m source words,
##   but write m+1 destination words if r + msdigits > rdigits, m words
##   otherwise.
##

proc _mpd_baseshiftl*(dest: ptr mpd_uint_t; src: ptr mpd_uint_t; n: mpd_size_t;
                     m: mpd_size_t; shift: mpd_size_t) =
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
  var ph: mpd_uint_t
  assert(m > 0 and n >= m)
  _mpd_div_word(addr(q), addr(r), cast[mpd_uint_t](shift), MPD_RDIGITS)
  if r != 0:
    ph = mpd_pow10[r]
    dec(m)
    dec(n)
    _mpd_divmod_pow10(addr(h), addr(lprev), src[dec(m)], MPD_RDIGITS - r)
    if h != 0:
      ##  r + msdigits > rdigits <==> h != 0
      dest[dec(n)] = h
    while m != MPD_SIZE_MAX:
      _mpd_divmod_pow10(addr(h), addr(l), src[m], MPD_RDIGITS - r)
      dest[n] = ph * lprev + h
      lprev = l
      dec(m)
      dec(n)
    ##  write least significant word
    dest[q] = ph * lprev
  else:
    while dec(m) != MPD_SIZE_MAX:
      dest[m + q] = src[m]
  mpd_uint_zero(dest, q)

##
##  Right shift of src by 'shift' digits; src may equal dest.
##  Assumption: srcdigits-shift > 0.
##
##   dest := area with space for srcdigits-shift digits.
##   src  := coefficient with length 'slen'.
##
##  The case splits in the function rely on the following equations:
##
##   Let msdigits denote the number of digits in the most significant
##   word of src. Then 1 <= msdigits <= rdigits.
##
##   1) shift = q * rdigits + r
##   2) srcdigits = qsrc * rdigits + msdigits
##   3) destdigits = srcdigits - shift
##                 = qsrc * rdigits + msdigits - (q * rdigits + r)
##                 = (qsrc - q) * rdigits + msdigits - r
##
##  Since destdigits > 0 and 1 <= msdigits <= rdigits:
##
##   4) qsrc >= q
##   5) qsrc == q  ==>  msdigits > r
##
##  The result has slen-q words if msdigits > r, slen-q-1 words otherwise.
##

proc _mpd_baseshiftr*(dest: ptr mpd_uint_t; src: ptr mpd_uint_t; slen: mpd_size_t;
                     shift: mpd_size_t): mpd_uint_t =
  when defined(__GNUC__) and not defined(__INTEL_COMPILER) and
      not defined(__clang__):
    ##  spurious uninitialized warnings
    var
      l: mpd_uint_t = l
      h: mpd_uint_t = h
      hprev: mpd_uint_t = hprev
    ##  low, high, previous high
  else:
    var
      l: mpd_uint_t
      h: mpd_uint_t
      hprev: mpd_uint_t
    ##  low, high, previous high
  var
    rnd: mpd_uint_t
    rest: mpd_uint_t
  ##  rounding digit, rest
  var
    q: mpd_uint_t
    r: mpd_uint_t
  var
    i: mpd_size_t
    j: mpd_size_t
  var ph: mpd_uint_t
  assert(slen > 0)
  _mpd_div_word(addr(q), addr(r), cast[mpd_uint_t](shift), MPD_RDIGITS)
  rnd = rest = 0
  if r != 0:
    ph = mpd_pow10[MPD_RDIGITS - r]
    _mpd_divmod_pow10(addr(hprev), addr(rest), src[q], r)
    _mpd_divmod_pow10(addr(rnd), addr(rest), rest, r - 1)
    if rest == 0 and q > 0:
      rest = not _mpd_isallzero(src, q)
    j = 0
    i = q + 1
    while i < slen:
      _mpd_divmod_pow10(addr(h), addr(l), src[i], r)
      dest[j] = ph * l + hprev
      hprev = h
      inc(i)
      inc(j)
    ##  write most significant word
    if hprev != 0:
      ##  always the case if slen==q-1
      dest[j] = hprev
  else:
    if q > 0:
      _mpd_divmod_pow10(addr(rnd), addr(rest), src[q - 1], MPD_RDIGITS - 1)
      ##  is there any non-zero digit below rnd?
      if rest == 0:
        rest = not _mpd_isallzero(src, q - 1)
    j = 0
    while j < slen - q:
      dest[j] = src[q + j]
      inc(j)
  ##  0-4  ==> rnd+rest < 0.5
  ##  5    ==> rnd+rest == 0.5
  ##  6-9  ==> rnd+rest > 0.5
  return if (rnd == 0 or rnd == 5): rnd + not not rest else: rnd

## *******************************************************************
##                       Calculations in base b
## *******************************************************************
##
##  Add v to w (len m). The calling function has to handle a possible
##  final carry. Assumption: m > 0.
##

proc _mpd_shortadd_b*(w: ptr mpd_uint_t; m: mpd_size_t; v: mpd_uint_t; b: mpd_uint_t): mpd_uint_t =
  var s: mpd_uint_t
  var carry: mpd_uint_t
  var i: mpd_size_t
  assert(m > 0)
  ##  add v to w
  s = w[0] + v
  carry = (s < v) or (s >= b)
  w[0] = if carry: s - b else: s
  ##  if there is a carry, propagate it
  i = 1
  while carry and i < m:
    s = w[i] + carry
    carry = (s == b)
    w[i] = if carry: 0 else: s
    inc(i)
  return carry

##  w := product of u (len n) and v (single word). Return carry.

proc _mpd_shortmul_c*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; n: mpd_size_t; v: mpd_uint_t): mpd_uint_t =
  var
    hi: mpd_uint_t
    lo: mpd_uint_t
  var carry: mpd_uint_t = 0
  var i: mpd_size_t
  assert(n > 0)
  i = 0
  while i < n:
    _mpd_mul_words(addr(hi), addr(lo), u[i], v)
    lo = carry + lo
    if lo < carry:
      inc(hi)
    _mpd_div_words_r(addr(carry), addr(w[i]), hi, lo)
    inc(i)
  return carry

##  w := product of u (len n) and v (single word)

proc _mpd_shortmul_b*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; n: mpd_size_t; v: mpd_uint_t;
                     b: mpd_uint_t): mpd_uint_t =
  var
    hi: mpd_uint_t
    lo: mpd_uint_t
  var carry: mpd_uint_t = 0
  var i: mpd_size_t
  assert(n > 0)
  i = 0
  while i < n:
    _mpd_mul_words(addr(hi), addr(lo), u[i], v)
    lo = carry + lo
    if lo < carry:
      inc(hi)
    _mpd_div_words(addr(carry), addr(w[i]), hi, lo, b)
    inc(i)
  return carry

##
##  Knuth, TAOCP Volume 2, 4.3.1, exercise 16:
##      w := quotient of u (len n) divided by a single word v
##

proc _mpd_shortdiv_b*(w: ptr mpd_uint_t; u: ptr mpd_uint_t; n: mpd_size_t; v: mpd_uint_t;
                     b: mpd_uint_t): mpd_uint_t =
  var
    hi: mpd_uint_t
    lo: mpd_uint_t
  var rem: mpd_uint_t = 0
  var i: mpd_size_t
  assert(n > 0)
  i = n - 1
  while i != MPD_SIZE_MAX:
    _mpd_mul_words(addr(hi), addr(lo), rem, b)
    lo = u[i] + lo
    if lo < u[i]:
      inc(hi)
    _mpd_div_words(addr(w[i]), addr(rem), hi, lo, v)
    dec(i)
  return rem
