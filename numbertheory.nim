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
  mpdecimal, constants

##  Internal header file: all symbols have local scope in the DSO
##  MPD_PRAGMA(MPD_HIDE_SYMBOLS_START)
##  transform parameters

type
  FntParams* {.importc: "fnt_params", header: "numbertheory.h", bycopy.} = object
    modnum* {.importc: "modnum".}: cint
    modulus* {.importc: "modulus".}: MpdUintT
    kernel* {.importc: "kernel".}: MpdUintT
    wtable* {.importc: "wtable".}: UncheckedArray[MpdUintT]


proc mpdGetkernel*(n: MpdUintT; sign: cint; modnum: cint): MpdUintT {.
    importc: "_mpd_getkernel", header: "numbertheory.h".}
proc mpdInitFntParams*(n: MpdSizeT; sign: cint; modnum: cint): ptr FntParams {.
    importc: "_mpd_init_fnt_params", header: "numbertheory.h".}
proc mpdInitW3table*(w3table: array[3, MpdUintT]; sign: cint; modnum: cint) {.
    importc: "_mpd_init_w3table", header: "numbertheory.h".}
when defined(PPRO):
  proc pproSetmodulus*(modnum: cint; umod: ptr MpdUintT; dmod: ptr cdouble;
                      dinvmod: array[3, uint32T]) {.inline.} =
    dmod[] = umod[] = mpdModuli[modnum]
    dinvmod[0] = mpdInvmoduli[modnum][0]
    dinvmod[1] = mpdInvmoduli[modnum][1]
    dinvmod[2] = mpdInvmoduli[modnum][2]

else:
  proc stdSetmodulus*(modnum: cint; umod: ptr MpdUintT) {.inline.} =
    umod[] = mpdModuli[modnum]

##  MPD_PRAGMA(MPD_HIDE_SYMBOLS_END) /* restore previous scope rules */
