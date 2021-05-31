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
  mpdecimal

##  Internal header file: all symbols have local scope in the DSO
##  MPD_PRAGMA(MPD_HIDE_SYMBOLS_START)

const
  FORWARD_CYCLE* = 0
  BACKWARD_CYCLE* = 1

proc stdTrans*(dest: ptr MpdUintT; src: ptr MpdUintT; rows: MpdSizeT; cols: MpdSizeT) {.
    importc: "std_trans", header: "transpose.h".}
proc transposePow2*(matrix: ptr MpdUintT; rows: MpdSizeT; cols: MpdSizeT): cint {.
    importc: "transpose_pow2", header: "transpose.h".}
proc transpose3xpow2*(matrix: ptr MpdUintT; rows: MpdSizeT; cols: MpdSizeT) {.
    importc: "transpose_3xpow2", header: "transpose.h".}
proc pointerswap*(a: ptr ptr MpdUintT; b: ptr ptr MpdUintT) {.inline.} =
  var tmp: ptr MpdUintT
  tmp = b[]
  b[] = a[]
  a[] = tmp

##  MPD_PRAGMA(MPD_HIDE_SYMBOLS_END) /* restore previous scope rules */
