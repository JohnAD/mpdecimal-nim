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

proc mpdDfltTraphandler*(ctx: ptr MpdContextT) =
  cast[nil](ctx)
  `raise`(sigfpe)

##  void (* mpd_traphandler)(mpd_context_t *) = mpd_dflt_traphandler;
##  Set guaranteed minimum number of coefficient words. The function may
##    be used once at program start. Setting MPD_MINALLOC to out-of-bounds
##    values is a catastrophic error, so in that case the function exits rather
##    than relying on the user to check a return value.

proc mpdSetminalloc*(n: MpdSsizeT) =
  var minallocIsSet: cint = 0
  if minallocIsSet:
    mpdErrWarn("mpd_setminalloc: ignoring request to set MPD_MINALLOC a second time\n")
    return
  if n < mpd_Minalloc_Min or n > mpd_Minalloc_Max:
    mpdErrFatal("illegal value for MPD_MINALLOC")
    ##  GCOV_NOT_REACHED
  mpd_Minalloc = n
  minallocIsSet = 1

proc mpdInit*(ctx: ptr MpdContextT; prec: MpdSsizeT) =
  var idealMinalloc: MpdSsizeT
  mpdDefaultcontext(ctx)
  if not mpdQsetprec(ctx, prec):
    mpdAddstatusRaise(ctx, mPD_InvalidContext)
    return
  idealMinalloc = 2 * ((prec + mpd_Rdigits - 1) div mpd_Rdigits)
  if idealMinalloc < mpd_Minalloc_Min:
    idealMinalloc = mpd_Minalloc_Min
  if idealMinalloc > mpd_Minalloc_Max:
    idealMinalloc = mpd_Minalloc_Max
  mpdSetminalloc(idealMinalloc)

proc mpdMaxcontext*(ctx: ptr MpdContextT) =
  ctx.prec = mpd_Max_Prec
  ctx.emax = mpd_Max_Emax
  ctx.emin = mpd_Min_Emin
  ctx.round = mpd_Round_Half_Even
  ctx.traps = mPD_Traps
  ctx.status = 0
  ctx.newtrap = 0
  ctx.clamp = 0
  ctx.allcr = 1

proc mpdDefaultcontext*(ctx: ptr MpdContextT) =
  ctx.prec = 2 * mpd_Rdigits
  ctx.emax = mpd_Max_Emax
  ctx.emin = mpd_Min_Emin
  ctx.round = mpd_Round_Half_Up
  ctx.traps = mPD_Traps
  ctx.status = 0
  ctx.newtrap = 0
  ctx.clamp = 0
  ctx.allcr = 1

proc mpdBasiccontext*(ctx: ptr MpdContextT) =
  ctx.prec = 9
  ctx.emax = mpd_Max_Emax
  ctx.emin = mpd_Min_Emin
  ctx.round = mpd_Round_Half_Up
  ctx.traps = mPD_Traps or mPD_Clamped
  ctx.status = 0
  ctx.newtrap = 0
  ctx.clamp = 0
  ctx.allcr = 1

proc mpdIeeeContext*(ctx: ptr MpdContextT; bits: cint): cint =
  if bits <= 0 or bits > mpd_Ieee_Context_Max_Bits or bits mod 32:
    return -1
  ctx.prec = 9 * (bits div 32) - 2
  ctx.emax = 3 * (cast[MpdSsizeT](1) shl (bits div 16 + 3))
  ctx.emin = 1 - ctx.emax
  ctx.round = mpd_Round_Half_Even
  ctx.traps = 0
  ctx.status = 0
  ctx.newtrap = 0
  ctx.clamp = 1
  ctx.allcr = 1
  return 0

proc mpdGetprec*(ctx: ptr MpdContextT): MpdSsizeT =
  return ctx.prec

proc mpdGetemax*(ctx: ptr MpdContextT): MpdSsizeT =
  return ctx.emax

proc mpdGetemin*(ctx: ptr MpdContextT): MpdSsizeT =
  return ctx.emin

proc mpdGetround*(ctx: ptr MpdContextT): cint =
  return ctx.round

proc mpdGettraps*(ctx: ptr MpdContextT): uint32T =
  return ctx.traps

proc mpdGetstatus*(ctx: ptr MpdContextT): uint32T =
  return ctx.status

proc mpdGetclamp*(ctx: ptr MpdContextT): cint =
  return ctx.clamp

proc mpdGetcr*(ctx: ptr MpdContextT): cint =
  return ctx.allcr

proc mpdQsetprec*(ctx: ptr MpdContextT; prec: MpdSsizeT): cint =
  if prec <= 0 or prec > mpd_Max_Prec:
    return 0
  ctx.prec = prec
  return 1

proc mpdQsetemax*(ctx: ptr MpdContextT; emax: MpdSsizeT): cint =
  if emax < 0 or emax > mpd_Max_Emax:
    return 0
  ctx.emax = emax
  return 1

proc mpdQsetemin*(ctx: ptr MpdContextT; emin: MpdSsizeT): cint =
  if emin > 0 or emin < mpd_Min_Emin:
    return 0
  ctx.emin = emin
  return 1

proc mpdQsetround*(ctx: ptr MpdContextT; round: cint): cint =
  if not (0 <= round and round < mpd_Round_Guard):
    return 0
  ctx.round = round
  return 1

proc mpdQsettraps*(ctx: ptr MpdContextT; flags: uint32T): cint =
  if flags > mPD_MaxStatus:
    return 0
  ctx.traps = flags
  return 1

proc mpdQsetstatus*(ctx: ptr MpdContextT; flags: uint32T): cint =
  if flags > mPD_MaxStatus:
    return 0
  ctx.status = flags
  return 1

proc mpdQsetclamp*(ctx: ptr MpdContextT; c: cint): cint =
  if c != 0 and c != 1:
    return 0
  ctx.clamp = c
  return 1

proc mpdQsetcr*(ctx: ptr MpdContextT; c: cint): cint =
  if c != 0 and c != 1:
    return 0
  ctx.allcr = c
  return 1

proc mpdAddstatusRaise*(ctx: ptr MpdContextT; flags: uint32T) =
  ctx.status = ctx.status or flags
  if flags and ctx.traps:
    ctx.newtrap = (flags and ctx.traps)
    mpdTraphandler(ctx)
