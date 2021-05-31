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
  mpdecimal, bits, difradix2, fnt, numbertheory

##  Bignum: Fast transform for medium-sized coefficients.
##  forward transform, sign = -1

proc stdFnt*(a: ptr MpdUintT; n: MpdSizeT; modnum: cint): cint =
  var tparams: ptr FntParams
  assert(ispower2(n))
  assert(n >= 4)
  assert(n <= 3 * mpd_Maxtransform_2n)
  if (tparams = mpdInitFntParams(n, -1, modnum)) == nil:
    return 0
  fntDif2(a, n, tparams)
  mpdFree(tparams)
  return 1

##  reverse transform, sign = 1

proc stdInvFnt*(a: ptr MpdUintT; n: MpdSizeT; modnum: cint): cint =
  var tparams: ptr FntParams
  assert(ispower2(n))
  assert(n >= 4)
  assert(n <= 3 * mpd_Maxtransform_2n)
  if (tparams = mpdInitFntParams(n, 1, modnum)) == nil:
    return 0
  fntDif2(a, n, tparams)
  mpdFree(tparams)
  return 1
