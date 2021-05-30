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
  mpdecimal, bits, constants, difradix2, numbertheory, umodarith

##  Bignum: The actual transform routine (decimation in frequency).
##
##  Generate index pairs (x, bitreverse(x)) and carry out the permutation.
##  n must be a power of two.
##  Algorithm due to Brent/Lehmann, see Joerg Arndt, "Matters Computational",
##  Chapter 1.14.4. [http://www.jjj.de/fxt/]
##

proc bitreverse_permute*(a: ptr mpd_uint_t; n: mpd_size_t) {.inline.} =
  var x: mpd_size_t = 0
  var r: mpd_size_t = 0
  var t: mpd_uint_t
  while true:
    ##  Invariant: r = bitreverse(x)
    if r > x:
      t = a[x]
      a[x] = a[r]
      a[r] = t
    inc(x, 1)
    ##  Mirror the operation on r: Flip n_trailing_zeros(x)+1
    ##            high bits of r.
    r = r xor (n - (n shr (mpd_bsf(x) + 1)))
    ##  The loop invariant is preserved.
    if not (x < n):
      break

##  Fast Number Theoretic Transform, decimation in frequency.

proc fnt_dif2*(a: ptr mpd_uint_t; n: mpd_size_t; tparams: ptr fnt_params) =
  var wtable: ptr mpd_uint_t = tparams.wtable
  var umod: mpd_uint_t
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var
    u0: mpd_uint_t
    u1: mpd_uint_t
    v0: mpd_uint_t
    v1: mpd_uint_t
  var
    w: mpd_uint_t
    w0: mpd_uint_t
    w1: mpd_uint_t
    wstep: mpd_uint_t
  var
    m: mpd_size_t
    mhalf: mpd_size_t
  var
    j: mpd_size_t
    r: mpd_size_t
  assert(ispower2(n))
  assert(n >= 4)
  SETMODULUS(tparams.modnum)
  ##  m == n
  mhalf = n div 2
  j = 0
  while j < mhalf:
    w0 = wtable[j]
    w1 = wtable[j + 1]
    u0 = a[j]
    v0 = a[j + mhalf]
    u1 = a[j + 1]
    v1 = a[j + 1 + mhalf]
    a[j] = addmod(u0, v0, umod)
    v0 = submod(u0, v0, umod)
    a[j + 1] = addmod(u1, v1, umod)
    v1 = submod(u1, v1, umod)
    MULMOD2(addr(v0), w0, addr(v1), w1)
    a[j + mhalf] = v0
    a[j + 1 + mhalf] = v1
    inc(j, 2)
  wstep = 2
  m = n div 2
  while m >= 2:
    mhalf = m div 2
    ##  j == 0
    r = 0
    while r < n:
      u0 = a[r]
      v0 = a[r + mhalf]
      u1 = a[m + r]
      v1 = a[m + r + mhalf]
      a[r] = addmod(u0, v0, umod)
      v0 = submod(u0, v0, umod)
      a[m + r] = addmod(u1, v1, umod)
      v1 = submod(u1, v1, umod)
      a[r + mhalf] = v0
      a[m + r + mhalf] = v1
      inc(r, 2 * m)
    j = 1
    while j < mhalf:
      w = wtable[j * wstep]
      r = 0
      while r < n:
        u0 = a[r + j]
        v0 = a[r + j + mhalf]
        u1 = a[m + r + j]
        v1 = a[m + r + j + mhalf]
        a[r + j] = addmod(u0, v0, umod)
        v0 = submod(u0, v0, umod)
        a[m + r + j] = addmod(u1, v1, umod)
        v1 = submod(u1, v1, umod)
        MULMOD2C(addr(v0), addr(v1), w)
        a[r + j + mhalf] = v0
        a[m + r + j + mhalf] = v1
        inc(r, 2 * m)
      inc(j)
    m = m shr 1
    wstep = wstep shl 1
  bitreverse_permute(a, n)
