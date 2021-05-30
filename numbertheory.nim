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
  mpdecimal, bits, numbertheory, umodarith

##  Bignum: Initialize the Number Theoretic Transform.
##
##  Return the nth root of unity in F(p). This corresponds to e**((2*pi*i)/n)
##  in the Fourier transform. We have w**n == 1 (mod p).
##     n := transform length.
##     sign := -1 for forward transform, 1 for backward transform.
##     modnum := one of {P1, P2, P3}.
##

proc _mpd_getkernel*(n: mpd_uint_t; sign: cint; modnum: cint): mpd_uint_t =
  var
    umod: mpd_uint_t
    p: mpd_uint_t
    r: mpd_uint_t
    xi: mpd_uint_t
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  SETMODULUS(modnum)
  r = mpd_roots[modnum]
  ##  primitive root of F(p)
  p = umod
  xi = (p - 1) div n
  if sign == -1:
    return POWMOD(r, (p - 1 - xi))
  else:
    return POWMOD(r, xi)

##
##  Initialize and return transform parameters.
##     n := transform length.
##     sign := -1 for forward transform, 1 for backward transform.
##     modnum := one of {P1, P2, P3}.
##

proc _mpd_init_fnt_params*(n: mpd_size_t; sign: cint; modnum: cint): ptr fnt_params =
  var tparams: ptr fnt_params
  var umod: mpd_uint_t
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var
    kernel: mpd_uint_t
    w: mpd_uint_t
  var i: mpd_uint_t
  var nhalf: mpd_size_t
  assert(ispower2(n))
  assert(sign == -1 or sign == 1)
  assert(P1 <= modnum and modnum <= P3)
  nhalf = n div 2
  tparams = mpd_sh_alloc(sizeof(tparams[]), nhalf, sizeof((mpd_uint_t)))
  if tparams == nil:
    return nil
  SETMODULUS(modnum)
  kernel = _mpd_getkernel(n, sign, modnum)
  tparams.modnum = modnum
  tparams.modulus = umod
  tparams.kernel = kernel
  ##  wtable[] := w**0, w**1, ..., w**(nhalf-1)
  w = 1
  i = 0
  while i < nhalf:
    tparams.wtable[i] = w
    w = MULMOD(w, kernel)
    inc(i)
  return tparams

##  Initialize wtable of size three.

proc _mpd_init_w3table*(w3table: array[3, mpd_uint_t]; sign: cint; modnum: cint) =
  var umod: mpd_uint_t
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var kernel: mpd_uint_t
  SETMODULUS(modnum)
  kernel = _mpd_getkernel(3, sign, modnum)
  w3table[0] = 1
  w3table[1] = kernel
  w3table[2] = POWMOD(kernel, 2)
