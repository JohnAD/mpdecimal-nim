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
  mpdecimal, basearith, constants, typearith

## *******************************************************************
##                    Calculations in base MPD_RADIX
## *******************************************************************
##
##  Knuth, TAOCP, Volume 2, 4.3.1:
##     w := sum of u (len m) and v (len n)
##     n > 0 and m >= n
##  The calling function has to handle a possible final carry.
##

proc mpdBaseadd*(w: ptr MpdUintT; u: ptr MpdUintT; v: ptr MpdUintT; m: MpdSizeT; n: MpdSizeT): MpdUintT =
  var s: MpdUintT
  var carry: MpdUintT = 0
  var i: MpdSizeT
  assert(n > 0 and m >= n)
  ##  add n members of u and v
  i = 0
  while i < n:
    s = u[i] + (v[i] + carry)
    carry = (s < u[i]) or (s >= mpd_Radix)
    w[i] = if carry: s - mpd_Radix else: s
    inc(i)
  ##  if there is a carry, propagate it
  while carry and i < m:
    s = u[i] + carry
    carry = (s == mpd_Radix)
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

proc mpdBaseaddto*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT) =
  var s: MpdUintT
  var carry: MpdUintT = 0
  var i: MpdSizeT
  if n == 0:
    return
  i = 0
  while i < n:
    s = w[i] + (u[i] + carry)
    carry = (s < w[i]) or (s >= mpd_Radix)
    w[i] = if carry: s - mpd_Radix else: s
    inc(i)
  ##  if there is a carry, propagate it
  while carry:
    s = w[i] + carry
    carry = (s == mpd_Radix)
    w[i] = if carry: 0 else: s
    inc(i)

##
##  Add v to w (len m). The calling function has to handle a possible
##  final carry. Assumption: m > 0.
##

proc mpdShortadd*(w: ptr MpdUintT; m: MpdSizeT; v: MpdUintT): MpdUintT =
  var s: MpdUintT
  var carry: MpdUintT
  var i: MpdSizeT
  assert(m > 0)
  ##  add v to w
  s = w[0] + v
  carry = (s < v) or (s >= mpd_Radix)
  w[0] = if carry: s - mpd_Radix else: s
  ##  if there is a carry, propagate it
  i = 1
  while carry and i < m:
    s = w[i] + carry
    carry = (s == mpd_Radix)
    w[i] = if carry: 0 else: s
    inc(i)
  return carry

##  Increment u. The calling function has to handle a possible carry.

proc mpdBaseincr*(u: ptr MpdUintT; n: MpdSizeT): MpdUintT =
  var s: MpdUintT
  var carry: MpdUintT = 1
  var i: MpdSizeT
  assert(n > 0)
  ##  if there is a carry, propagate it
  i = 0
  while carry and i < n:
    s = u[i] + carry
    carry = (s == mpd_Radix)
    u[i] = if carry: 0 else: s
    inc(i)
  return carry

##
##  Knuth, TAOCP, Volume 2, 4.3.1:
##      w := difference of u (len m) and v (len n).
##      number in u >= number in v;
##

proc mpdBasesub*(w: ptr MpdUintT; u: ptr MpdUintT; v: ptr MpdUintT; m: MpdSizeT; n: MpdSizeT) =
  var d: MpdUintT
  var borrow: MpdUintT = 0
  var i: MpdSizeT
  assert(m > 0 and n > 0)
  ##  subtract n members of v from u
  i = 0
  while i < n:
    d = u[i] - (v[i] + borrow)
    borrow = (u[i] < d)
    w[i] = if borrow: d + mpd_Radix else: d
    inc(i)
  ##  if there is a borrow, propagate it
  while borrow and i < m:
    d = u[i] - borrow
    borrow = (u[i] == 0)
    w[i] = if borrow: mpd_Radix - 1 else: d
    inc(i)
  ##  copy the rest of u
  while i < m:
    w[i] = u[i]
    inc(i)

##
##  Subtract the contents of u from w. w is larger than u. Borrows are
##  propagated further, but eventually w can absorb the final borrow.
##

proc mpdBasesubfrom*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT) =
  var d: MpdUintT
  var borrow: MpdUintT = 0
  var i: MpdSizeT
  if n == 0:
    return
  i = 0
  while i < n:
    d = w[i] - (u[i] + borrow)
    borrow = (w[i] < d)
    w[i] = if borrow: d + mpd_Radix else: d
    inc(i)
  ##  if there is a borrow, propagate it
  while borrow:
    d = w[i] - borrow
    borrow = (w[i] == 0)
    w[i] = if borrow: mpd_Radix - 1 else: d
    inc(i)

##  w := product of u (len n) and v (single word)

proc mpdShortmul*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT) =
  var
    hi: MpdUintT
    lo: MpdUintT
  var carry: MpdUintT = 0
  var i: MpdSizeT
  assert(n > 0)
  i = 0
  while i < n:
    mpdMulWords(addr(hi), addr(lo), u[i], v)
    lo = carry + lo
    if lo < carry:
      inc(hi)
    mpdDivWordsR(addr(carry), addr(w[i]), hi, lo)
    inc(i)
  w[i] = carry

##
##  Knuth, TAOCP, Volume 2, 4.3.1:
##      w := product of u (len m) and v (len n)
##      w must be initialized to zero
##

proc mpdBasemul*(w: ptr MpdUintT; u: ptr MpdUintT; v: ptr MpdUintT; m: MpdSizeT; n: MpdSizeT) =
  var
    hi: MpdUintT
    lo: MpdUintT
  var carry: MpdUintT
  var
    i: MpdSizeT
    j: MpdSizeT
  assert(m > 0 and n > 0)
  j = 0
  while j < n:
    carry = 0
    i = 0
    while i < m:
      mpdMulWords(addr(hi), addr(lo), u[i], v[j])
      lo = w[i + j] + lo
      if lo < w[i + j]:
        inc(hi)
      lo = carry + lo
      if lo < carry:
        inc(hi)
      mpdDivWordsR(addr(carry), addr(w[i + j]), hi, lo)
      inc(i)
    w[j + m] = carry
    inc(j)

##
##  Knuth, TAOCP Volume 2, 4.3.1, exercise 16:
##      w := quotient of u (len n) divided by a single word v
##

proc mpdShortdiv*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT): MpdUintT =
  var
    hi: MpdUintT
    lo: MpdUintT
  var rem: MpdUintT = 0
  var i: MpdSizeT
  assert(n > 0)
  i = n - 1
  while i != mpd_Size_Max:
    mpdMulWords(addr(hi), addr(lo), rem, mpd_Radix)
    lo = u[i] + lo
    if lo < u[i]:
      inc(hi)
    mpdDivWords(addr(w[i]), addr(rem), hi, lo, v)
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

proc mpdBasedivmod*(q: ptr MpdUintT; r: ptr MpdUintT; uconst: ptr MpdUintT;
                   vconst: ptr MpdUintT; nplusm: MpdSizeT; n: MpdSizeT): cint =
  var ustatic: array[mpd_Minalloc_Max, MpdUintT]
  var vstatic: array[mpd_Minalloc_Max, MpdUintT]
  var u: ptr MpdUintT = ustatic
  var v: ptr MpdUintT = vstatic
  var
    d: MpdUintT
    qhat: MpdUintT
    rhat: MpdUintT
    w2: array[2, MpdUintT]
  var
    hi: MpdUintT
    lo: MpdUintT
    x: MpdUintT
  var carry: MpdUintT
  var
    i: MpdSizeT
    j: MpdSizeT
    m: MpdSizeT
  var retval: cint = 0
  assert(n > 1 and nplusm >= n)
  m = subSizeT(nplusm, n)
  ##  D1: normalize
  d = mpd_Radix div (vconst[n - 1] + 1)
  if nplusm >= mpd_Minalloc_Max:
    if (u = mpdAlloc(nplusm + 1, sizeof(u[]))) == nil:
      return -1
  if n >= mpd_Minalloc_Max:
    if (v = mpdAlloc(n + 1, sizeof(v[]))) == nil:
      mpdFree(u)
      return -1
  mpdShortmul(u, uconst, nplusm, d)
  mpdShortmul(v, vconst, n, d)
  ##  D2: loop
  j = m
  while j != mpd_Size_Max:
    assert(2 <= j + n and j + n <= nplusm)
    ##  annotation for scan-build
    ##  D3: calculate qhat and rhat
    rhat = mpdShortdiv(w2, u + j + n - 1, 2, v[n - 1])
    qhat = w2[1] * mpd_Radix + w2[0]
    while 1:
      if qhat < mpd_Radix:
        mpdSinglemul(w2, qhat, v[n - 2])
        if w2[1] <= rhat:
          if w2[1] != rhat or w2[0] <= u[j + n - 2]:
            break
      dec(qhat, 1)
      inc(rhat, v[n - 1])
      if rhat < v[n - 1] or rhat >= mpd_Radix:
        break
    ##  D4: multiply and subtract
    carry = 0
    i = 0
    while i <= n:
      mpdMulWords(addr(hi), addr(lo), qhat, v[i])
      lo = carry + lo
      if lo < carry:
        inc(hi)
      mpdDivWordsR(addr(hi), addr(lo), hi, lo)
      x = u[i + j] - lo
      carry = (u[i + j] < x)
      u[i + j] = if carry: x + mpd_Radix else: x
      inc(carry, hi)
      inc(i)
    q[j] = qhat
    ##  D5: test remainder
    if carry:
      dec(q[j], 1)
      ##  D6: add back
      cast[nil](mpdBaseadd(u + j, u + j, v, n + 1, n))
    dec(j)
  ##  D8: unnormalize
  if r != nil:
    mpdShortdiv(r, u, n, d)
    ##  we are not interested in the return value here
    retval = 0
  else:
    retval = not mpdIsallzero(u, n)
  if u != ustatic:
    mpdFree(u)
  if v != vstatic:
    mpdFree(v)
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

proc mpdBaseshiftl*(dest: ptr MpdUintT; src: ptr MpdUintT; n: MpdSizeT; m: MpdSizeT;
                   shift: MpdSizeT) =
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
  var ph: MpdUintT
  assert(m > 0 and n >= m)
  mpdDivWord(addr(q), addr(r), cast[MpdUintT](shift), mpd_Rdigits)
  if r != 0:
    ph = mpdPow10[r]
    dec(m)
    dec(n)
    mpdDivmodPow10(addr(h), addr(lprev), src[dec(m)], mpd_Rdigits - r)
    if h != 0:
      ##  r + msdigits > rdigits <==> h != 0
      dest[dec(n)] = h
    while m != mpd_Size_Max:
      mpdDivmodPow10(addr(h), addr(l), src[m], mpd_Rdigits - r)
      dest[n] = ph * lprev + h
      lprev = l
      dec(m)
      dec(n)
    ##  write least significant word
    dest[q] = ph * lprev
  else:
    while dec(m) != mpd_Size_Max:
      dest[m + q] = src[m]
  mpdUintZero(dest, q)

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

proc mpdBaseshiftr*(dest: ptr MpdUintT; src: ptr MpdUintT; slen: MpdSizeT;
                   shift: MpdSizeT): MpdUintT =
  when defined(gnuc) and not defined(intel_Compiler) and not defined(clang):
    ##  spurious uninitialized warnings
    var
      l: MpdUintT = l
      h: MpdUintT = h
      hprev: MpdUintT = hprev
    ##  low, high, previous high
  else:
    var
      l: MpdUintT
      h: MpdUintT
      hprev: MpdUintT
    ##  low, high, previous high
  var
    rnd: MpdUintT
    rest: MpdUintT
  ##  rounding digit, rest
  var
    q: MpdUintT
    r: MpdUintT
  var
    i: MpdSizeT
    j: MpdSizeT
  var ph: MpdUintT
  assert(slen > 0)
  mpdDivWord(addr(q), addr(r), cast[MpdUintT](shift), mpd_Rdigits)
  rnd = rest = 0
  if r != 0:
    ph = mpdPow10[mpd_Rdigits - r]
    mpdDivmodPow10(addr(hprev), addr(rest), src[q], r)
    mpdDivmodPow10(addr(rnd), addr(rest), rest, r - 1)
    if rest == 0 and q > 0:
      rest = not mpdIsallzero(src, q)
    j = 0
    i = q + 1
    while i < slen:
      mpdDivmodPow10(addr(h), addr(l), src[i], r)
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
      mpdDivmodPow10(addr(rnd), addr(rest), src[q - 1], mpd_Rdigits - 1)
      ##  is there any non-zero digit below rnd?
      if rest == 0:
        rest = not mpdIsallzero(src, q - 1)
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

proc mpdShortaddB*(w: ptr MpdUintT; m: MpdSizeT; v: MpdUintT; b: MpdUintT): MpdUintT =
  var s: MpdUintT
  var carry: MpdUintT
  var i: MpdSizeT
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

proc mpdShortmulC*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT): MpdUintT =
  var
    hi: MpdUintT
    lo: MpdUintT
  var carry: MpdUintT = 0
  var i: MpdSizeT
  assert(n > 0)
  i = 0
  while i < n:
    mpdMulWords(addr(hi), addr(lo), u[i], v)
    lo = carry + lo
    if lo < carry:
      inc(hi)
    mpdDivWordsR(addr(carry), addr(w[i]), hi, lo)
    inc(i)
  return carry

##  w := product of u (len n) and v (single word)

proc mpdShortmulB*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT; b: MpdUintT): MpdUintT =
  var
    hi: MpdUintT
    lo: MpdUintT
  var carry: MpdUintT = 0
  var i: MpdSizeT
  assert(n > 0)
  i = 0
  while i < n:
    mpdMulWords(addr(hi), addr(lo), u[i], v)
    lo = carry + lo
    if lo < carry:
      inc(hi)
    mpdDivWords(addr(carry), addr(w[i]), hi, lo, b)
    inc(i)
  return carry

##
##  Knuth, TAOCP Volume 2, 4.3.1, exercise 16:
##      w := quotient of u (len n) divided by a single word v
##

proc mpdShortdivB*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT; b: MpdUintT): MpdUintT =
  var
    hi: MpdUintT
    lo: MpdUintT
  var rem: MpdUintT = 0
  var i: MpdSizeT
  assert(n > 0)
  i = n - 1
  while i != mpd_Size_Max:
    mpdMulWords(addr(hi), addr(lo), rem, b)
    lo = u[i] + lo
    if lo < u[i]:
      inc(hi)
    mpdDivWords(addr(w[i]), addr(rem), hi, lo, v)
    dec(i)
  return rem
