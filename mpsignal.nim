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

##  Signaling wrappers for the quiet functions in mpdecimal.c.

proc mpdFormat*(dec: ptr MpdT; fmt: cstring; ctx: ptr MpdContextT): cstring =
  var ret: cstring
  var status: uint32T = 0
  ret = mpdQformat(dec, fmt, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)
  return ret

proc mpdImportU16*(result: ptr MpdT; srcdata: ptr uint16T; srclen: csize;
                  srcsign: uint8T; base: uint32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQimportU16(result, srcdata, srclen, srcsign, base, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdImportU32*(result: ptr MpdT; srcdata: ptr uint32T; srclen: csize;
                  srcsign: uint8T; base: uint32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQimportU32(result, srcdata, srclen, srcsign, base, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdExportU16*(rdata: ptr ptr uint16T; rlen: csize; base: uint32T; src: ptr MpdT;
                  ctx: ptr MpdContextT): csize =
  var n: csize
  var status: uint32T = 0
  n = mpdQexportU16(rdata, rlen, base, src, addr(status))
  mpdAddstatusRaise(ctx, status)
  return n

proc mpdExportU32*(rdata: ptr ptr uint32T; rlen: csize; base: uint32T; src: ptr MpdT;
                  ctx: ptr MpdContextT): csize =
  var n: csize
  var status: uint32T = 0
  n = mpdQexportU32(rdata, rlen, base, src, addr(status))
  mpdAddstatusRaise(ctx, status)
  return n

proc mpdFinalize*(result: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQfinalize(result, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdCheckNan*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT): cint =
  var status: uint32T = 0
  if mpdQcheckNan(result, a, ctx, addr(status)):
    mpdAddstatusRaise(ctx, status)
    return 1
  return 0

proc mpdCheckNans*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT): cint =
  var status: uint32T = 0
  if mpdQcheckNans(result, a, b, ctx, addr(status)):
    mpdAddstatusRaise(ctx, status)
    return 1
  return 0

proc mpdSetString*(result: ptr MpdT; s: cstring; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsetString(result, s, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMaxcoeff*(result: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmaxcoeff(result, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

##  set static mpd from signed integer

proc mpdSsetSsize*(result: ptr MpdT; a: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQssetSsize(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdSsetI32*(result: ptr MpdT; a: int32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQssetI32(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when defined(CONFIG_64):
  proc mpdSsetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQssetI64(result, a, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

##  set static mpd from unsigned integer

proc mpdSsetUint*(result: ptr MpdT; a: MpdUintT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQssetUint(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdSsetU32*(result: ptr MpdT; a: uint32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQssetU32(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when defined(CONFIG_64):
  proc mpdSsetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQssetU64(result, a, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

##  set mpd from signed integer

proc mpdSetSsize*(result: ptr MpdT; a: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsetSsize(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdSetI32*(result: ptr MpdT; a: int32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsetI32(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdSetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQsetI64(result, a, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

##  set mpd from unsigned integer

proc mpdSetUint*(result: ptr MpdT; a: MpdUintT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsetUint(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdSetU32*(result: ptr MpdT; a: uint32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsetU32(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdSetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQsetU64(result, a, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

##  convert mpd to signed integer

proc mpdGetSsize*(a: ptr MpdT; ctx: ptr MpdContextT): MpdSsizeT =
  var status: uint32T = 0
  var ret: MpdSsizeT
  ret = mpdQgetSsize(a, addr(status))
  mpdAddstatusRaise(ctx, status)
  return ret

proc mpdGetI32*(a: ptr MpdT; ctx: ptr MpdContextT): int32T =
  var status: uint32T = 0
  var ret: int32T
  ret = mpdQgetI32(a, addr(status))
  mpdAddstatusRaise(ctx, status)
  return ret

when not defined(LEGACY_COMPILER):
  proc mpdGetI64*(a: ptr MpdT; ctx: ptr MpdContextT): int64T =
    var status: uint32T = 0
    var ret: int64T
    ret = mpdQgetI64(a, addr(status))
    mpdAddstatusRaise(ctx, status)
    return ret

proc mpdGetUint*(a: ptr MpdT; ctx: ptr MpdContextT): MpdUintT =
  var status: uint32T = 0
  var ret: MpdUintT
  ret = mpdQgetUint(a, addr(status))
  mpdAddstatusRaise(ctx, status)
  return ret

proc mpdAbsUint*(a: ptr MpdT; ctx: ptr MpdContextT): MpdUintT =
  var status: uint32T = 0
  var ret: MpdUintT
  ret = mpdQabsUint(a, addr(status))
  mpdAddstatusRaise(ctx, status)
  return ret

proc mpdGetU32*(a: ptr MpdT; ctx: ptr MpdContextT): uint32T =
  var status: uint32T = 0
  var ret: uint32T
  ret = mpdQgetU32(a, addr(status))
  mpdAddstatusRaise(ctx, status)
  return ret

when not defined(LEGACY_COMPILER):
  proc mpdGetU64*(a: ptr MpdT; ctx: ptr MpdContextT): uint64T =
    var status: uint32T = 0
    var ret: uint64T
    ret = mpdQgetU64(a, addr(status))
    mpdAddstatusRaise(ctx, status)
    return ret

proc mpdAnd*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQand(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdCopy*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  if not mpdQcopy(result, a, addr(status)):
    mpdAddstatusRaise(ctx, status)

proc mpdCanonical*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  mpdCopy(result, a, ctx)

proc mpdCopyAbs*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  if not mpdQcopyAbs(result, a, addr(status)):
    mpdAddstatusRaise(ctx, status)

proc mpdCopyNegate*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  if not mpdQcopyNegate(result, a, addr(status)):
    mpdAddstatusRaise(ctx, status)

proc mpdCopySign*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  if not mpdQcopySign(result, a, b, addr(status)):
    mpdAddstatusRaise(ctx, status)

proc mpdInvert*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQinvert(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdLogb*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQlogb(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdOr*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQor(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdRotate*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQrotate(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdScaleb*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQscaleb(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdShiftl*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQshiftl(result, a, n, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdShiftr*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; ctx: ptr MpdContextT): MpdUintT =
  var status: uint32T = 0
  var rnd: MpdUintT
  rnd = mpdQshiftr(result, a, n, addr(status))
  mpdAddstatusRaise(ctx, status)
  return rnd

proc mpdShiftn*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQshiftn(result, a, n, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdShift*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQshift(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdXor*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQxor(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdAbs*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQabs(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdCmp*(a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT): cint =
  var status: uint32T = 0
  var c: cint
  c = mpdQcmp(a, b, addr(status))
  mpdAddstatusRaise(ctx, status)
  return c

proc mpdCompare*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT): cint =
  var status: uint32T = 0
  var c: cint
  c = mpdQcompare(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)
  return c

proc mpdCompareSignal*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT): cint =
  var status: uint32T = 0
  var c: cint
  c = mpdQcompareSignal(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)
  return c

proc mpdAdd*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQadd(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdSub*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsub(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdAddSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQaddSsize(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdAddI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQaddI32(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdAddI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQaddI64(result, a, b, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

proc mpdAddUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQaddUint(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdAddU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQaddU32(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdAddU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQaddU64(result, a, b, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

proc mpdSubSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsubSsize(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdSubI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsubI32(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdSubI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQsubI64(result, a, b, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

proc mpdSubUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsubUint(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdSubU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsubU32(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdSubU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQsubU64(result, a, b, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

proc mpdDiv*(q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQdiv(q, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdDivSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQdivSsize(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdDivI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQdivI32(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdDivI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQdivI64(result, a, b, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

proc mpdDivUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQdivUint(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdDivU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQdivU32(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdDivU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQdivU64(result, a, b, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

proc mpdDivmod*(q: ptr MpdT; r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQdivmod(q, r, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdDivint*(q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQdivint(q, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdExp*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQexp(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdFma*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; c: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQfma(result, a, b, c, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdLn*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQln(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdLog10*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQlog10(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMax*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmax(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMaxMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmaxMag(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMin*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmin(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMinMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQminMag(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMinus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQminus(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMul*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmul(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMulSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmulSsize(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMulI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmulI32(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdMulI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQmulI64(result, a, b, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

proc mpdMulUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmulUint(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdMulU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQmulU32(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpdMulU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT) =
    var status: uint32T = 0
    mpdQmulU64(result, a, b, ctx, addr(status))
    mpdAddstatusRaise(ctx, status)

proc mpdNextMinus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQnextMinus(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdNextPlus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQnextPlus(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdNextToward*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQnextToward(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdPlus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQplus(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdPow*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQpow(result, base, exp, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdPowmod*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; `mod`: ptr MpdT;
               ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQpowmod(result, base, exp, `mod`, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdQuantize*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQquantize(result, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdRescale*(result: ptr MpdT; a: ptr MpdT; exp: MpdSsizeT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQrescale(result, a, exp, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdReduce*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQreduce(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdRem*(r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQrem(r, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdRemNear*(r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQremNear(r, a, b, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdRoundToIntx*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQroundToIntx(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdRoundToInt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQroundToInt(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdTrunc*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQtrunc(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdFloor*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQfloor(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdCeil*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQceil(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdSqrt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQsqrt(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)

proc mpdInvroot*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT) =
  var status: uint32T = 0
  mpdQinvroot(result, a, ctx, addr(status))
  mpdAddstatusRaise(ctx, status)
