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

# import
#   mpdecimal

##  Internal header file: all symbols have local scope in the DSO
##  MPD_PRAGMA(MPD_HIDE_SYMBOLS_START)
##  choice of optimized functions

when defined(config_64):
  ##  x64
  template mulmod*(a, b: untyped): untyped =
    x64Mulmod(a, b, umod)

  template mulmod2c*(a0, a1, w: untyped): untyped =
    x64Mulmod2c(a0, a1, w, umod)

  template mulmod2*(a0, b0, a1, b1: untyped): untyped =
    x64Mulmod2(a0, b0, a1, b1, umod)

  template powmod*(base, exp: untyped): untyped =
    x64Powmod(base, exp, umod)

  template setmodulus*(modnum: untyped): untyped =
    stdSetmodulus(modnum, addr(umod))

  template size3Ntt*(x0, x1, x2, w3table: untyped): untyped =
    stdSize3Ntt(x0, x1, x2, w3table, umod)

elif defined(ppro):
  ##  PentiumPro (or later) gcc inline asm
  template mulmod*(a, b: untyped): untyped =
    pproMulmod(a, b, addr(dmod), dinvmod)

  template mulmod2c*(a0, a1, w: untyped): untyped =
    pproMulmod2c(a0, a1, w, addr(dmod), dinvmod)

  template mulmod2*(a0, b0, a1, b1: untyped): untyped =
    pproMulmod2(a0, b0, a1, b1, addr(dmod), dinvmod)

  template powmod*(base, exp: untyped): untyped =
    pproPowmod(base, exp, addr(dmod), dinvmod)

  template setmodulus*(modnum: untyped): untyped =
    pproSetmodulus(modnum, addr(umod), addr(dmod), dinvmod)

  template size3Ntt*(x0, x1, x2, w3table: untyped): untyped =
    pproSize3Ntt(x0, x1, x2, w3table, umod, addr(dmod), dinvmod)

else:
  ##  ANSI C99
  template mulmod*(a, b: untyped): untyped =
    stdMulmod(a, b, umod)

  template mulmod2c*(a0, a1, w: untyped): untyped =
    stdMulmod2c(a0, a1, w, umod)

  template mulmod2*(a0, b0, a1, b1: untyped): untyped =
    stdMulmod2(a0, b0, a1, b1, umod)

  template powmod*(base, exp: untyped): untyped =
    stdPowmod(base, exp, umod)

  template setmodulus*(modnum: untyped): untyped =
    stdSetmodulus(modnum, addr(umod))

  template size3Ntt*(x0, x1, x2, w3table: untyped): untyped =
    stdSize3Ntt(x0, x1, x2, w3table, umod)

##  PentiumPro (or later) gcc inline asm

var MPD_TWO63* {.importc: "MPD_TWO63", header: "constants.h".}: cfloat

var mpdInvmoduli* {.importc: "mpd_invmoduli", header: "constants.h".}: array[3,
    array[3, uint32T]]

const
  P1* = 0
  P2* = 1
  P3* = 2

var mpdModuli* {.importc: "mpd_moduli", header: "constants.h".}: UncheckedArray[
    MpdUintT]

var mpdRoots* {.importc: "mpd_roots", header: "constants.h".}: UncheckedArray[MpdUintT]

var mpdBits* {.importc: "mpd_bits", header: "constants.h".}: UncheckedArray[MpdSizeT]

var mpdPow10* {.importc: "mpd_pow10", header: "constants.h".}: UncheckedArray[MpdUintT]

var INV_P1_MOD_P2* {.importc: "INV_P1_MOD_P2", header: "constants.h".}: MpdUintT

var INV_P1P2_MOD_P3* {.importc: "INV_P1P2_MOD_P3", header: "constants.h".}: MpdUintT

var LH_P1P2* {.importc: "LH_P1P2", header: "constants.h".}: MpdUintT

var UH_P1P2* {.importc: "UH_P1P2", header: "constants.h".}: MpdUintT

##  MPD_PRAGMA(MPD_HIDE_SYMBOLS_END) /* restore previous scope rules */
