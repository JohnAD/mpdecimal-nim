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
  mpdecimal, bits, constants, difradix2, numbertheory, sixstep, transpose, umodarith

##  Bignum: Cache efficient Matrix Fourier Transform for arrays of the
##    form 2**n (See literature/six-step.txt).
##  forward transform with sign = -1

proc six_step_fnt*(a: ptr mpd_uint_t; n: mpd_size_t; modnum: cint): cint =
  var tparams: ptr fnt_params
  var
    log2n: mpd_size_t
    C: mpd_size_t
    R: mpd_size_t
  var kernel: mpd_uint_t
  var umod: mpd_uint_t
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var
    x: ptr mpd_uint_t
    w0: mpd_uint_t
    w1: mpd_uint_t
    wstep: mpd_uint_t
  var
    i: mpd_size_t
    k: mpd_size_t
  assert(ispower2(n))
  assert(n >= 16)
  assert(n <= MPD_MAXTRANSFORM_2N)
  log2n = mpd_bsr(n)
  C = (cast[mpd_size_t](1)) shl (log2n div 2)
  ##  number of columns
  R = (cast[mpd_size_t](1)) shl (log2n - (log2n div 2))
  ##  number of rows
  ##  Transpose the matrix.
  if not transpose_pow2(a, R, C):
    return 0
  if (tparams = _mpd_init_fnt_params(R, -1, modnum)) == nil:
    return 0
  x = a
  while x < a + n:
    fnt_dif2(x, R, tparams)
    inc(x, R)
  ##  Transpose the matrix.
  if not transpose_pow2(a, C, R):
    mpd_free(tparams)
    return 0
  SETMODULUS(modnum)
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
    while k < C:
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
  if C != R:
    mpd_free(tparams)
    if (tparams = _mpd_init_fnt_params(C, -1, modnum)) == nil:
      return 0
  x = a
  while x < a + n:
    fnt_dif2(x, C, tparams)
    inc(x, C)
  mpd_free(tparams)
  when 0:
    ##  An unordered transform is sufficient for convolution.
    ##  Transpose the matrix.
    if not transpose_pow2(a, R, C):
      return 0
  return 1

##  reverse transform, sign = 1

proc inv_six_step_fnt*(a: ptr mpd_uint_t; n: mpd_size_t; modnum: cint): cint =
  var tparams: ptr fnt_params
  var
    log2n: mpd_size_t
    C: mpd_size_t
    R: mpd_size_t
  var kernel: mpd_uint_t
  var umod: mpd_uint_t
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var
    x: ptr mpd_uint_t
    w0: mpd_uint_t
    w1: mpd_uint_t
    wstep: mpd_uint_t
  var
    i: mpd_size_t
    k: mpd_size_t
  assert(ispower2(n))
  assert(n >= 16)
  assert(n <= MPD_MAXTRANSFORM_2N)
  log2n = mpd_bsr(n)
  C = (cast[mpd_size_t](1)) shl (log2n div 2)
  ##  number of columns
  R = (cast[mpd_size_t](1)) shl (log2n - (log2n div 2))
  ##  number of rows
  when 0:
    ##  An unordered transform is sufficient for convolution.
    ##  Transpose the matrix, producing an R*C matrix.
    if not transpose_pow2(a, C, R):
      return 0
  ##  Length C transform on the rows.
  if (tparams = _mpd_init_fnt_params(C, 1, modnum)) == nil:
    return 0
  x = a
  while x < a + n:
    fnt_dif2(x, C, tparams)
    inc(x, C)
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
  ##  Transpose the matrix.
  if not transpose_pow2(a, R, C):
    mpd_free(tparams)
    return 0
  if R != C:
    mpd_free(tparams)
    if (tparams = _mpd_init_fnt_params(R, 1, modnum)) == nil:
      return 0
  x = a
  while x < a + n:
    fnt_dif2(x, R, tparams)
    inc(x, R)
  mpd_free(tparams)
  ##  Transpose the matrix.
  if not transpose_pow2(a, C, R):
    return 0
  return 1
