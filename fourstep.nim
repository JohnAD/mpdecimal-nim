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
  mpdecimal, constants, fourstep, numbertheory, sixstep, umodarith

##  Bignum: Cache efficient Matrix Fourier Transform for arrays of the
##    form 3 * 2**n (See literature/matrix-transform.txt).

when not defined(PPRO):
  proc std_size3_ntt*(x1: ptr mpd_uint_t; x2: ptr mpd_uint_t; x3: ptr mpd_uint_t;
                     w3table: array[3, mpd_uint_t]; umod: mpd_uint_t) {.inline.} =
    var
      r1: mpd_uint_t
      r2: mpd_uint_t
    var w: mpd_uint_t
    var
      s: mpd_uint_t
      tmp: mpd_uint_t
    ##  k = 0 -> w = 1
    s = x1[]
    s = addmod(s, x2[], umod)
    s = addmod(s, x3[], umod)
    r1 = s
    ##  k = 1
    s = x1[]
    w = w3table[1]
    tmp = MULMOD(x2[], w)
    s = addmod(s, tmp, umod)
    w = w3table[2]
    tmp = MULMOD(x3[], w)
    s = addmod(s, tmp, umod)
    r2 = s
    ##  k = 2
    s = x1[]
    w = w3table[2]
    tmp = MULMOD(x2[], w)
    s = addmod(s, tmp, umod)
    w = w3table[1]
    tmp = MULMOD(x3[], w)
    s = addmod(s, tmp, umod)
    x3[] = s
    x2[] = r2
    x1[] = r1

else:
  proc ppro_size3_ntt*(x1: ptr mpd_uint_t; x2: ptr mpd_uint_t; x3: ptr mpd_uint_t;
                      w3table: array[3, mpd_uint_t]; umod: mpd_uint_t;
                      dmod: ptr cdouble; dinvmod: array[3, uint32_t]) {.inline.} =
    var
      r1: mpd_uint_t
      r2: mpd_uint_t
    var w: mpd_uint_t
    var
      s: mpd_uint_t
      tmp: mpd_uint_t
    ##  k = 0 -> w = 1
    s = x1[]
    s = addmod(s, x2[], umod)
    s = addmod(s, x3[], umod)
    r1 = s
    ##  k = 1
    s = x1[]
    w = w3table[1]
    tmp = ppro_mulmod(x2[], w, dmod, dinvmod)
    s = addmod(s, tmp, umod)
    w = w3table[2]
    tmp = ppro_mulmod(x3[], w, dmod, dinvmod)
    s = addmod(s, tmp, umod)
    r2 = s
    ##  k = 2
    s = x1[]
    w = w3table[2]
    tmp = ppro_mulmod(x2[], w, dmod, dinvmod)
    s = addmod(s, tmp, umod)
    w = w3table[1]
    tmp = ppro_mulmod(x3[], w, dmod, dinvmod)
    s = addmod(s, tmp, umod)
    x3[] = s
    x2[] = r2
    x1[] = r1

##  forward transform, sign = -1; transform length = 3 * 2**n

proc four_step_fnt*(a: ptr mpd_uint_t; n: mpd_size_t; modnum: cint): cint =
  var R: mpd_size_t = 3
  ##  number of rows
  var C: mpd_size_t = n div 3
  ##  number of columns
  var w3table: array[3, mpd_uint_t]
  var
    kernel: mpd_uint_t
    w0: mpd_uint_t
    w1: mpd_uint_t
    wstep: mpd_uint_t
  var
    s: ptr mpd_uint_t
    p0: ptr mpd_uint_t
    p1: ptr mpd_uint_t
    p2: ptr mpd_uint_t
  var umod: mpd_uint_t
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var
    i: mpd_size_t
    k: mpd_size_t
  assert(n >= 48)
  assert(n <= 3 * MPD_MAXTRANSFORM_2N)
  ##  Length R transform on the columns.
  SETMODULUS(modnum)
  _mpd_init_w3table(w3table, -1, modnum)
  p0 = a
  p1 = p0 + C
  p2 = p0 + 2 * C
  while p0 < a + C:
    SIZE3_NTT(p0, p1, p2, w3table)
    inc(p0)
    inc(p1)
    inc(p2)
  ##  Multiply each matrix element (addressed by i*C+k) by r**(i*k).
  kernel = _mpd_getkernel(n, -1, modnum)
  i = 1
  while i < R:
    w0 = 1
    ##  r**(i*0): initial value for k=0
    w1 = POWMOD(kernel, i)
    ##  r**(i*1): initial value for k=1
    wstep = MULMOD(w1, w1)
    ##  r**(2*i)
    k = 0
    while k < C - 1:
      var x0: mpd_uint_t = a[i * C + k]
      var x1: mpd_uint_t = a[i * C + k + 1]
      MULMOD2(addr(x0), w0, addr(x1), w1)
      MULMOD2C(addr(w0), addr(w1), wstep)
      ##  r**(i*(k+2)) = r**(i*k) * r**(2*i)
      a[i * C + k] = x0
      a[i * C + k + 1] = x1
      inc(k, 2)
    inc(i)
  ##  Length C transform on the rows.
  s = a
  while s < a + n:
    if not six_step_fnt(s, C, modnum):
      return 0
    inc(s, C)
  when 0:
    ##  An unordered transform is sufficient for convolution.
    ##  Transpose the matrix.
    import
      transpose

    transpose_3xpow2(a, R, C)
  return 1

##  backward transform, sign = 1; transform length = 3 * 2**n

proc inv_four_step_fnt*(a: ptr mpd_uint_t; n: mpd_size_t; modnum: cint): cint =
  var R: mpd_size_t = 3
  ##  number of rows
  var C: mpd_size_t = n div 3
  ##  number of columns
  var w3table: array[3, mpd_uint_t]
  var
    kernel: mpd_uint_t
    w0: mpd_uint_t
    w1: mpd_uint_t
    wstep: mpd_uint_t
  var
    s: ptr mpd_uint_t
    p0: ptr mpd_uint_t
    p1: ptr mpd_uint_t
    p2: ptr mpd_uint_t
  var umod: mpd_uint_t
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var
    i: mpd_size_t
    k: mpd_size_t
  assert(n >= 48)
  assert(n <= 3 * MPD_MAXTRANSFORM_2N)
  when 0:
    ##  An unordered transform is sufficient for convolution.
    ##  Transpose the matrix, producing an R*C matrix.
    import
      transpose

    transpose_3xpow2(a, C, R)
  ##  Length C transform on the rows.
  s = a
  while s < a + n:
    if not inv_six_step_fnt(s, C, modnum):
      return 0
    inc(s, C)
  ##  Multiply each matrix element (addressed by i*C+k) by r**(i*k).
  SETMODULUS(modnum)
  kernel = _mpd_getkernel(n, 1, modnum)
  i = 1
  while i < R:
    w0 = 1
    w1 = POWMOD(kernel, i)
    wstep = MULMOD(w1, w1)
    k = 0
    while k < C:
      var x0: mpd_uint_t = a[i * C + k]
      var x1: mpd_uint_t = a[i * C + k + 1]
      MULMOD2(addr(x0), w0, addr(x1), w1)
      MULMOD2C(addr(w0), addr(w1), wstep)
      a[i * C + k] = x0
      a[i * C + k + 1] = x1
      inc(k, 2)
    inc(i)
  ##  Length R transform on the columns.
  _mpd_init_w3table(w3table, 1, modnum)
  p0 = a
  p1 = p0 + C
  p2 = p0 + 2 * C
  while p0 < a + C:
    SIZE3_NTT(p0, p1, p2, w3table)
    inc(p0)
    inc(p1)
    inc(p2)
  return 1
