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
  proc stdSize3Ntt*(x1: ptr MpdUintT; x2: ptr MpdUintT; x3: ptr MpdUintT;
                   w3table: array[3, MpdUintT]; umod: MpdUintT) {.inline.} =
    var
      r1: MpdUintT
      r2: MpdUintT
    var w: MpdUintT
    var
      s: MpdUintT
      tmp: MpdUintT
    ##  k = 0 -> w = 1
    s = x1[]
    s = addmod(s, x2[], umod)
    s = addmod(s, x3[], umod)
    r1 = s
    ##  k = 1
    s = x1[]
    w = w3table[1]
    tmp = mulmod(x2[], w)
    s = addmod(s, tmp, umod)
    w = w3table[2]
    tmp = mulmod(x3[], w)
    s = addmod(s, tmp, umod)
    r2 = s
    ##  k = 2
    s = x1[]
    w = w3table[2]
    tmp = mulmod(x2[], w)
    s = addmod(s, tmp, umod)
    w = w3table[1]
    tmp = mulmod(x3[], w)
    s = addmod(s, tmp, umod)
    x3[] = s
    x2[] = r2
    x1[] = r1

else:
  proc pproSize3Ntt*(x1: ptr MpdUintT; x2: ptr MpdUintT; x3: ptr MpdUintT;
                    w3table: array[3, MpdUintT]; umod: MpdUintT; dmod: ptr cdouble;
                    dinvmod: array[3, uint32T]) {.inline.} =
    var
      r1: MpdUintT
      r2: MpdUintT
    var w: MpdUintT
    var
      s: MpdUintT
      tmp: MpdUintT
    ##  k = 0 -> w = 1
    s = x1[]
    s = addmod(s, x2[], umod)
    s = addmod(s, x3[], umod)
    r1 = s
    ##  k = 1
    s = x1[]
    w = w3table[1]
    tmp = pproMulmod(x2[], w, dmod, dinvmod)
    s = addmod(s, tmp, umod)
    w = w3table[2]
    tmp = pproMulmod(x3[], w, dmod, dinvmod)
    s = addmod(s, tmp, umod)
    r2 = s
    ##  k = 2
    s = x1[]
    w = w3table[2]
    tmp = pproMulmod(x2[], w, dmod, dinvmod)
    s = addmod(s, tmp, umod)
    w = w3table[1]
    tmp = pproMulmod(x3[], w, dmod, dinvmod)
    s = addmod(s, tmp, umod)
    x3[] = s
    x2[] = r2
    x1[] = r1

##  forward transform, sign = -1; transform length = 3 * 2**n

proc fourStepFnt*(a: ptr MpdUintT; n: MpdSizeT; modnum: cint): cint =
  var R: MpdSizeT = 3
  ##  number of rows
  var C: MpdSizeT = n div 3
  ##  number of columns
  var w3table: array[3, MpdUintT]
  var
    kernel: MpdUintT
    w0: MpdUintT
    w1: MpdUintT
    wstep: MpdUintT
  var
    s: ptr MpdUintT
    p0: ptr MpdUintT
    p1: ptr MpdUintT
    p2: ptr MpdUintT
  var umod: MpdUintT
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32T]
  var
    i: MpdSizeT
    k: MpdSizeT
  assert(n >= 48)
  assert(n <= 3 * mpd_Maxtransform_2n)
  ##  Length R transform on the columns.
  setmodulus(modnum)
  mpdInitW3table(w3table, -1, modnum)
  p0 = a
  p1 = p0 + c
  p2 = p0 + 2 * c
  while p0 < a + c:
    size3Ntt(p0, p1, p2, w3table)
    inc(p0)
    inc(p1)
    inc(p2)
  ##  Multiply each matrix element (addressed by i*C+k) by r**(i*k).
  kernel = mpdGetkernel(n, -1, modnum)
  i = 1
  while i < r:
    w0 = 1
    ##  r**(i*0): initial value for k=0
    w1 = powmod(kernel, i)
    ##  r**(i*1): initial value for k=1
    wstep = mulmod(w1, w1)
    ##  r**(2*i)
    k = 0
    while k < c - 1:
      var x0: MpdUintT = a[i * c + k]
      var x1: MpdUintT = a[i * c + k + 1]
      mulmod2(addr(x0), w0, addr(x1), w1)
      mulmod2c(addr(w0), addr(w1), wstep)
      ##  r**(i*(k+2)) = r**(i*k) * r**(2*i)
      a[i * c + k] = x0
      a[i * c + k + 1] = x1
      inc(k, 2)
    inc(i)
  ##  Length C transform on the rows.
  s = a
  while s < a + n:
    if not sixStepFnt(s, c, modnum):
      return 0
    inc(s, c)
  when 0:
    ##  An unordered transform is sufficient for convolution.
    ##  Transpose the matrix.
    import
      transpose

    transpose3xpow2(a, r, c)
  return 1

##  backward transform, sign = 1; transform length = 3 * 2**n

proc invFourStepFnt*(a: ptr MpdUintT; n: MpdSizeT; modnum: cint): cint =
  var R: MpdSizeT = 3
  ##  number of rows
  var C: MpdSizeT = n div 3
  ##  number of columns
  var w3table: array[3, MpdUintT]
  var
    kernel: MpdUintT
    w0: MpdUintT
    w1: MpdUintT
    wstep: MpdUintT
  var
    s: ptr MpdUintT
    p0: ptr MpdUintT
    p1: ptr MpdUintT
    p2: ptr MpdUintT
  var umod: MpdUintT
  when defined(PPRO):
    var dmod: cdouble
    var dinvmod: array[3, uint32T]
  var
    i: MpdSizeT
    k: MpdSizeT
  assert(n >= 48)
  assert(n <= 3 * mpd_Maxtransform_2n)
  when 0:
    ##  An unordered transform is sufficient for convolution.
    ##  Transpose the matrix, producing an R*C matrix.
    import
      transpose

    transpose3xpow2(a, c, r)
  ##  Length C transform on the rows.
  s = a
  while s < a + n:
    if not invSixStepFnt(s, c, modnum):
      return 0
    inc(s, c)
  ##  Multiply each matrix element (addressed by i*C+k) by r**(i*k).
  setmodulus(modnum)
  kernel = mpdGetkernel(n, 1, modnum)
  i = 1
  while i < r:
    w0 = 1
    w1 = powmod(kernel, i)
    wstep = mulmod(w1, w1)
    k = 0
    while k < c:
      var x0: MpdUintT = a[i * c + k]
      var x1: MpdUintT = a[i * c + k + 1]
      mulmod2(addr(x0), w0, addr(x1), w1)
      mulmod2c(addr(w0), addr(w1), wstep)
      a[i * c + k] = x0
      a[i * c + k + 1] = x1
      inc(k, 2)
    inc(i)
  ##  Length R transform on the columns.
  mpdInitW3table(w3table, 1, modnum)
  p0 = a
  p1 = p0 + c
  p2 = p0 + 2 * c
  while p0 < a + c:
    size3Ntt(p0, p1, p2, w3table)
    inc(p0)
    inc(p1)
    inc(p2)
  return 1
