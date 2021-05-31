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
  # mpdecimal, 
  typearith

##  Internal header file: all symbols have local scope in the DSO

mpd_Pragma(mpd_Hide_Symbols_Start)
proc mpdBaseadd*(w: ptr MpdUintT; u: ptr MpdUintT; v: ptr MpdUintT; m: MpdSizeT; n: MpdSizeT): MpdUintT {.
    importc: "_mpd_baseadd", header: "basearith.h".}
proc mpdBaseaddto*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT) {.
    importc: "_mpd_baseaddto", header: "basearith.h".}
proc mpdShortadd*(w: ptr MpdUintT; m: MpdSizeT; v: MpdUintT): MpdUintT {.
    importc: "_mpd_shortadd", header: "basearith.h".}
proc mpdShortaddB*(w: ptr MpdUintT; m: MpdSizeT; v: MpdUintT; b: MpdUintT): MpdUintT {.
    importc: "_mpd_shortadd_b", header: "basearith.h".}
proc mpdBaseincr*(u: ptr MpdUintT; n: MpdSizeT): MpdUintT {.importc: "_mpd_baseincr",
    header: "basearith.h".}
proc mpdBasesub*(w: ptr MpdUintT; u: ptr MpdUintT; v: ptr MpdUintT; m: MpdSizeT; n: MpdSizeT) {.
    importc: "_mpd_basesub", header: "basearith.h".}
proc mpdBasesubfrom*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT) {.
    importc: "_mpd_basesubfrom", header: "basearith.h".}
proc mpdBasemul*(w: ptr MpdUintT; u: ptr MpdUintT; v: ptr MpdUintT; m: MpdSizeT; n: MpdSizeT) {.
    importc: "_mpd_basemul", header: "basearith.h".}
proc mpdShortmul*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT) {.
    importc: "_mpd_shortmul", header: "basearith.h".}
proc mpdShortmulC*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT): MpdUintT {.
    importc: "_mpd_shortmul_c", header: "basearith.h".}
proc mpdShortmulB*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT; b: MpdUintT): MpdUintT {.
    importc: "_mpd_shortmul_b", header: "basearith.h".}
proc mpdShortdiv*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT): MpdUintT {.
    importc: "_mpd_shortdiv", header: "basearith.h".}
proc mpdShortdivB*(w: ptr MpdUintT; u: ptr MpdUintT; n: MpdSizeT; v: MpdUintT; b: MpdUintT): MpdUintT {.
    importc: "_mpd_shortdiv_b", header: "basearith.h".}
proc mpdBasedivmod*(q: ptr MpdUintT; r: ptr MpdUintT; uconst: ptr MpdUintT;
                   vconst: ptr MpdUintT; nplusm: MpdSizeT; n: MpdSizeT): cint {.
    importc: "_mpd_basedivmod", header: "basearith.h".}
proc mpdBaseshiftl*(dest: ptr MpdUintT; src: ptr MpdUintT; n: MpdSizeT; m: MpdSizeT;
                   shift: MpdSizeT) {.importc: "_mpd_baseshiftl",
                                    header: "basearith.h".}
proc mpdBaseshiftr*(dest: ptr MpdUintT; src: ptr MpdUintT; slen: MpdSizeT;
                   shift: MpdSizeT): MpdUintT {.importc: "_mpd_baseshiftr",
    header: "basearith.h".}
when defined(CONFIG_64):
  var mprimeRdx* {.importc: "mprime_rdx", header: "basearith.h".}: MpdUintT
  ##
  ##  Algorithm from: Division by Invariant Integers using Multiplication,
  ##  T. Granlund and P. L. Montgomery, Proceedings of the SIGPLAN '94
  ##  Conference on Programming Language Design and Implementation.
  ##
  ##  http://gmplib.org/~tege/divcnst-pldi94.pdf
  ##
  ##  Variables from the paper and their translations (See section 8):
  ##
  ##   N := 64
  ##   d := MPD_RADIX
  ##   l := 64
  ##   m' := floor((2**(64+64) - 1)/MPD_RADIX) - 2**64
  ##
  ##  Since N-l == 0:
  ##
  ##   dnorm := d
  ##   n2 := hi
  ##   n10 := lo
  ##
  ##  ACL2 proof: mpd-div-words-r-correct
  ##
  proc mpdDivWordsR*(q: ptr MpdUintT; r: ptr MpdUintT; hi: MpdUintT; lo: MpdUintT) {.inline.} =
    var
      nAdj: MpdUintT
      h: MpdUintT
      l: MpdUintT
      t: MpdUintT
    var n1Neg: MpdUintT
    ##  n1_neg = if lo >= 2**63 then MPD_UINT_MAX else 0
    n1Neg = if (lo and (1 shl 63)): mpd_Uint_Max else: 0
    ##  n_adj = if lo >= 2**63 then lo+MPD_RADIX else lo
    nAdj = lo + (n1Neg and mpd_Radix)
    ##  (h, l) = if lo >= 2**63 then m'*(hi+1) else m'*hi
    mpdMulWords(addr(h), addr(l), mprimeRdx, hi - n1Neg)
    l = l + nAdj
    if l < nAdj:
      inc(h)
    t = h + hi
    ##  At this point t == qest, with q == qest or q == qest+1:
    ##    1) 0 <= 2**64*hi + lo - qest*MPD_RADIX < 2*MPD_RADIX
    ##
    ##  t = 2**64-1 - qest = 2**64 - (qest+1)
    t = mpd_Uint_Max - t
    ##  (h, l) = 2**64*MPD_RADIX - (qest+1)*MPD_RADIX
    mpdMulWords(addr(h), addr(l), t, mpd_Radix)
    l = l + lo
    if l < lo:
      inc(h)
    inc(h, hi)
    dec(h, mpd_Radix)
    ##  (h, l) = 2**64*hi + lo - (qest+1)*MPD_RADIX (mod 2**128)
    ##  Case q == qest+1:
    ##      a) h == 0, l == r
    ##      b) q := h - t == qest+1
    ##      c) r := l
    ##  Case q == qest:
    ##      a) h == MPD_UINT_MAX, l == 2**64-(MPD_RADIX-r)
    ##      b) q := h - t == qest
    ##      c) r := l + MPD_RADIX = r
    ##
    q[] = (h - t)
    r[] = l + (mpd_Radix and h)

else:
  proc mpdDivWordsR*(q: ptr MpdUintT; r: ptr MpdUintT; hi: MpdUintT; lo: MpdUintT) {.inline.} =
    mpdDivWords(q, r, hi, lo, mpd_Radix)

##  Multiply two single base MPD_RADIX words, store result in array w[2].

proc mpdSinglemul*(w: array[2, MpdUintT]; u: MpdUintT; v: MpdUintT) {.inline.} =
  var
    hi: MpdUintT
    lo: MpdUintT
  mpdMulWords(addr(hi), addr(lo), u, v)
  mpdDivWordsR(addr(w[1]), addr(w[0]), hi, lo)

##  Multiply u (len 2) and v (len m, 1 <= m <= 2).

proc mpdMul2Le2*(w: array[4, MpdUintT]; u: array[2, MpdUintT]; v: array[2, MpdUintT];
                m: MpdSsizeT) {.inline.} =
  var
    hi: MpdUintT
    lo: MpdUintT
  mpdMulWords(addr(hi), addr(lo), u[0], v[0])
  mpdDivWordsR(addr(w[1]), addr(w[0]), hi, lo)
  mpdMulWords(addr(hi), addr(lo), u[1], v[0])
  lo = w[1] + lo
  if lo < w[1]:
    inc(hi)
  mpdDivWordsR(addr(w[2]), addr(w[1]), hi, lo)
  if m == 1:
    return
  mpdMulWords(addr(hi), addr(lo), u[0], v[1])
  lo = w[1] + lo
  if lo < w[1]:
    inc(hi)
  mpdDivWordsR(addr(w[3]), addr(w[1]), hi, lo)
  mpdMulWords(addr(hi), addr(lo), u[1], v[1])
  lo = w[2] + lo
  if lo < w[2]:
    inc(hi)
  lo = w[3] + lo
  if lo < w[3]:
    inc(hi)
  mpdDivWordsR(addr(w[3]), addr(w[2]), hi, lo)

##
##  Test if all words from data[len-1] to data[0] are zero. If len is 0, nothing
##  is tested and the coefficient is regarded as "all zero".
##

proc mpdIsallzero*(data: ptr MpdUintT; len: MpdSsizeT): cint {.inline.} =
  while dec(len) >= 0:
    if data[len] != 0:
      return 0
  return 1

##
##  Test if all full words from data[len-1] to data[0] are MPD_RADIX-1
##  (all nines). Return true if len == 0.
##

proc mpdIsallnine*(data: ptr MpdUintT; len: MpdSsizeT): cint {.inline.} =
  while dec(len) >= 0:
    if data[len] != mpd_Radix - 1:
      return 0
  return 1

mpd_Pragma(mpd_Hide_Symbols_End)
##  restore previous scope rules
