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
  mpdecimal, bits, constants, convolute, fnt, fourstep, numbertheory, sixstep, umodarith

##  Bignum: Fast convolution using the Number Theoretic Transform. Used for
##    the multiplication of very large coefficients.
##  Convolute the data in c1 and c2. Result is in c1.

proc fnt_convolute*(c1: ptr mpd_uint_t; c2: ptr mpd_uint_t; n: mpd_size_t; modnum: cint): cint =
  var fnt: proc (a1: ptr mpd_uint_t; a2: mpd_size_t; a3: cint): cint
  var inv_fnt: proc (a1: ptr mpd_uint_t; a2: mpd_size_t; a3: cint): cint
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var
    n_inv: mpd_uint_t
    umod: mpd_uint_t
  var i: mpd_size_t
  SETMODULUS(modnum)
  n_inv = POWMOD(n, (umod - 2))
  if ispower2(n):
    if n > SIX_STEP_THRESHOLD:
      fnt = six_step_fnt
      inv_fnt = inv_six_step_fnt
    else:
      fnt = std_fnt
      inv_fnt = std_inv_fnt
  else:
    fnt = four_step_fnt
    inv_fnt = inv_four_step_fnt
  if not fnt(c1, n, modnum):
    return 0
  if not fnt(c2, n, modnum):
    return 0
  i = 0
  while i < n - 1:
    var x0: mpd_uint_t = c1[i]
    var y0: mpd_uint_t = c2[i]
    var x1: mpd_uint_t = c1[i + 1]
    var y1: mpd_uint_t = c2[i + 1]
    MULMOD2(addr(x0), y0, addr(x1), y1)
    c1[i] = x0
    c1[i + 1] = x1
    inc(i, 2)
  if not inv_fnt(c1, n, modnum):
    return 0
  i = 0
  while i < n - 3:
    var x0: mpd_uint_t = c1[i]
    var x1: mpd_uint_t = c1[i + 1]
    var x2: mpd_uint_t = c1[i + 2]
    var x3: mpd_uint_t = c1[i + 3]
    MULMOD2C(addr(x0), addr(x1), n_inv)
    MULMOD2C(addr(x2), addr(x3), n_inv)
    c1[i] = x0
    c1[i + 1] = x1
    c1[i + 2] = x2
    c1[i + 3] = x3
    inc(i, 4)
  return 1

##  Autoconvolute the data in c1. Result is in c1.

proc fnt_autoconvolute*(c1: ptr mpd_uint_t; n: mpd_size_t; modnum: cint): cint =
  var fnt: proc (a1: ptr mpd_uint_t; a2: mpd_size_t; a3: cint): cint
  var inv_fnt: proc (a1: ptr mpd_uint_t; a2: mpd_size_t; a3: cint): cint
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32_t]
  var
    n_inv: mpd_uint_t
    umod: mpd_uint_t
  var i: mpd_size_t
  SETMODULUS(modnum)
  n_inv = POWMOD(n, (umod - 2))
  if ispower2(n):
    if n > SIX_STEP_THRESHOLD:
      fnt = six_step_fnt
      inv_fnt = inv_six_step_fnt
    else:
      fnt = std_fnt
      inv_fnt = std_inv_fnt
  else:
    fnt = four_step_fnt
    inv_fnt = inv_four_step_fnt
  if not fnt(c1, n, modnum):
    return 0
  i = 0
  while i < n - 1:
    var x0: mpd_uint_t = c1[i]
    var x1: mpd_uint_t = c1[i + 1]
    MULMOD2(addr(x0), x0, addr(x1), x1)
    c1[i] = x0
    c1[i + 1] = x1
    inc(i, 2)
  if not inv_fnt(c1, n, modnum):
    return 0
  i = 0
  while i < n - 3:
    var x0: mpd_uint_t = c1[i]
    var x1: mpd_uint_t = c1[i + 1]
    var x2: mpd_uint_t = c1[i + 2]
    var x3: mpd_uint_t = c1[i + 3]
    MULMOD2C(addr(x0), addr(x1), n_inv)
    MULMOD2C(addr(x2), addr(x3), n_inv)
    c1[i] = x0
    c1[i + 1] = x1
    c1[i + 2] = x2
    c1[i + 3] = x3
    inc(i, 4)
  return 1
