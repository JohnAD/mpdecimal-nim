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
  mpdecimal, constants, crt, numbertheory, typearith, umodarith

##  Bignum: Chinese Remainder Theorem, extends the maximum transform length.
##  Multiply P1P2 by v, store result in w.

proc crtMulP1P23*(w: array[3, MpdUintT]; v: MpdUintT) {.inline.} =
  var
    hi1: MpdUintT
    hi2: MpdUintT
    lo: MpdUintT
  mpdMulWords(addr(hi1), addr(lo), lh_P1p2, v)
  w[0] = lo
  mpdMulWords(addr(hi2), addr(lo), uh_P1p2, v)
  lo = hi1 + lo
  if lo < hi1:
    inc(hi2)
  w[1] = lo
  w[2] = hi2

##  Add 3 words from v to w. The result is known to fit in w.

proc crtAdd3*(w: array[3, MpdUintT]; v: array[3, MpdUintT]) {.inline.} =
  var carry: MpdUintT
  w[0] = w[0] + v[0]
  carry = (w[0] < v[0])
  w[1] = w[1] + v[1]
  if w[1] < v[1]:
    inc(w[2])
  w[1] = w[1] + carry
  if w[1] < carry:
    inc(w[2])
  inc(w[2], v[2])

##  Divide 3 words in u by v, store result in w, return remainder.

proc crtDiv3*(w: ptr MpdUintT; u: ptr MpdUintT; v: MpdUintT): MpdUintT {.inline.} =
  var r1: MpdUintT = u[2]
  var r2: MpdUintT
  if r1 < v:
    w[2] = 0
  else:
    mpdDivWord(addr(w[2]), addr(r1), u[2], v)
    ##  GCOV_NOT_REACHED
  mpdDivWords(addr(w[1]), addr(r2), r1, u[1], v)
  mpdDivWords(addr(w[0]), addr(r1), r2, u[0], v)
  return r1

##
##  Chinese Remainder Theorem:
##  Algorithm from Joerg Arndt, "Matters Computational",
##  Chapter 37.4.1 [http://www.jjj.de/fxt/]
##
##  See also Knuth, TAOCP, Volume 2, 4.3.2, exercise 7.
##
##
##  CRT with carry: x1, x2, x3 contain numbers modulo p1, p2, p3. For each
##  triple of members of the arrays, find the unique z modulo p1*p2*p3, with
##  zmax = p1*p2*p3 - 1.
##
##  In each iteration of the loop, split z into result[i] = z % MPD_RADIX
##  and carry = z / MPD_RADIX. Let N be the size of carry[] and cmax the
##  maximum carry.
##
##  Limits for the 32-bit build:
##
##    N    = 2**96
##    cmax = 7711435591312380274
##
##  Limits for the 64 bit build:
##
##    N    = 2**192
##    cmax = 627710135393475385904124401220046371710
##
##  The following statements hold for both versions:
##
##    1) cmax + zmax < N, so the addition does not overflow.
##
##    2) (cmax + zmax) / MPD_RADIX == cmax.
##
##    3) If c <= cmax, then c_next = (c + zmax) / MPD_RADIX <= cmax.
##

proc crt3*(x1: ptr MpdUintT; x2: ptr MpdUintT; x3: ptr MpdUintT; rsize: MpdSizeT) =
  var p1: MpdUintT = mpdModuli[p1]
  var umod: MpdUintT
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32T]
  var
    a1: MpdUintT
    a2: MpdUintT
    a3: MpdUintT
  var s: MpdUintT
  var
    z: array[3, MpdUintT]
    t: array[3, MpdUintT]
  var carry: array[3, MpdUintT] = [0, 0, 0]
  var
    hi: MpdUintT
    lo: MpdUintT
  var i: MpdSizeT
  i = 0
  while i < rsize:
    a1 = x1[i]
    a2 = x2[i]
    a3 = x3[i]
    setmodulus(p2)
    s = extSubmod(a2, a1, umod)
    s = mulmod(s, inv_P1Mod_P2)
    mpdMulWords(addr(hi), addr(lo), s, p1)
    lo = lo + a1
    if lo < a1:
      inc(hi)
    setmodulus(p3)
    s = dwSubmod(a3, hi, lo, umod)
    s = mulmod(s, inv_P1p2Mod_P3)
    z[0] = lo
    z[1] = hi
    z[2] = 0
    crtMulP1P23(t, s)
    crtAdd3(z, t)
    crtAdd3(carry, z)
    x1[i] = crtDiv3(carry, carry, mpd_Radix)
    inc(i)
  assert(carry[0] == 0 and carry[1] == 0 and carry[2] == 0)
