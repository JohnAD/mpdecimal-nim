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

proc mpd_dflt_traphandler*(ctx: ptr mpd_context_t) =
  cast[nil](ctx)
  `raise`(SIGFPE)

##  c2nim TODO void (* mpd_traphandler)(mpd_context_t *) = mpd_dflt_traphandler;
##  Set guaranteed minimum number of coefficient words. The function may
##    be used once at program start. Setting MPD_MINALLOC to out-of-bounds
##    values is a catastrophic error, so in that case the function exits rather
##    than relying on the user to check a return value.

proc mpd_setminalloc*(n: mpd_ssize_t) =
  var minalloc_is_set: cint = 0
  if minalloc_is_set:
    mpd_err_warn("mpd_setminalloc: ignoring request to set MPD_MINALLOC a second time\n")
    return
  if n < MPD_MINALLOC_MIN or n > MPD_MINALLOC_MAX:
    mpd_err_fatal("illegal value for MPD_MINALLOC")
    ##  GCOV_NOT_REACHED
  MPD_MINALLOC = n
  minalloc_is_set = 1

proc mpd_init*(ctx: ptr mpd_context_t; prec: mpd_ssize_t) =
  var ideal_minalloc: mpd_ssize_t
  mpd_defaultcontext(ctx)
  if not mpd_qsetprec(ctx, prec):
    mpd_addstatus_raise(ctx, MPD_Invalid_context)
    return
  ideal_minalloc = 2 * ((prec + MPD_RDIGITS - 1) div MPD_RDIGITS)
  if ideal_minalloc < MPD_MINALLOC_MIN:
    ideal_minalloc = MPD_MINALLOC_MIN
  if ideal_minalloc > MPD_MINALLOC_MAX:
    ideal_minalloc = MPD_MINALLOC_MAX
  mpd_setminalloc(ideal_minalloc)

proc mpd_maxcontext*(ctx: ptr mpd_context_t) =
  ctx.prec = MPD_MAX_PREC
  ctx.emax = MPD_MAX_EMAX
  ctx.emin = MPD_MIN_EMIN
  ctx.round = MPD_ROUND_HALF_EVEN
  ctx.traps = MPD_Traps
  ctx.status = 0
  ctx.newtrap = 0
  ctx.clamp = 0
  ctx.allcr = 1

proc mpd_defaultcontext*(ctx: ptr mpd_context_t) =
  ctx.prec = 2 * MPD_RDIGITS
  ctx.emax = MPD_MAX_EMAX
  ctx.emin = MPD_MIN_EMIN
  ctx.round = MPD_ROUND_HALF_UP
  ctx.traps = MPD_Traps
  ctx.status = 0
  ctx.newtrap = 0
  ctx.clamp = 0
  ctx.allcr = 1

proc mpd_basiccontext*(ctx: ptr mpd_context_t) =
  ctx.prec = 9
  ctx.emax = MPD_MAX_EMAX
  ctx.emin = MPD_MIN_EMIN
  ctx.round = MPD_ROUND_HALF_UP
  ctx.traps = MPD_Traps or MPD_Clamped
  ctx.status = 0
  ctx.newtrap = 0
  ctx.clamp = 0
  ctx.allcr = 1

proc mpd_ieee_context*(ctx: ptr mpd_context_t; bits: cint): cint =
  if bits <= 0 or bits > MPD_IEEE_CONTEXT_MAX_BITS or bits mod 32:
    return -1
  ctx.prec = 9 * (bits div 32) - 2
  ctx.emax = 3 * (cast[mpd_ssize_t](1) shl (bits div 16 + 3))
  ctx.emin = 1 - ctx.emax
  ctx.round = MPD_ROUND_HALF_EVEN
  ctx.traps = 0
  ctx.status = 0
  ctx.newtrap = 0
  ctx.clamp = 1
  ctx.allcr = 1
  return 0

proc mpd_getprec*(ctx: ptr mpd_context_t): mpd_ssize_t =
  return ctx.prec

proc mpd_getemax*(ctx: ptr mpd_context_t): mpd_ssize_t =
  return ctx.emax

proc mpd_getemin*(ctx: ptr mpd_context_t): mpd_ssize_t =
  return ctx.emin

proc mpd_getround*(ctx: ptr mpd_context_t): cint =
  return ctx.round

proc mpd_gettraps*(ctx: ptr mpd_context_t): uint32_t =
  return ctx.traps

proc mpd_getstatus*(ctx: ptr mpd_context_t): uint32_t =
  return ctx.status

proc mpd_getclamp*(ctx: ptr mpd_context_t): cint =
  return ctx.clamp

proc mpd_getcr*(ctx: ptr mpd_context_t): cint =
  return ctx.allcr

proc mpd_qsetprec*(ctx: ptr mpd_context_t; prec: mpd_ssize_t): cint =
  if prec <= 0 or prec > MPD_MAX_PREC:
    return 0
  ctx.prec = prec
  return 1

proc mpd_qsetemax*(ctx: ptr mpd_context_t; emax: mpd_ssize_t): cint =
  if emax < 0 or emax > MPD_MAX_EMAX:
    return 0
  ctx.emax = emax
  return 1

proc mpd_qsetemin*(ctx: ptr mpd_context_t; emin: mpd_ssize_t): cint =
  if emin > 0 or emin < MPD_MIN_EMIN:
    return 0
  ctx.emin = emin
  return 1

proc mpd_qsetround*(ctx: ptr mpd_context_t; round: cint): cint =
  if not (0 <= round and round < MPD_ROUND_GUARD):
    return 0
  ctx.round = round
  return 1

proc mpd_qsettraps*(ctx: ptr mpd_context_t; flags: uint32_t): cint =
  if flags > MPD_Max_status:
    return 0
  ctx.traps = flags
  return 1

proc mpd_qsetstatus*(ctx: ptr mpd_context_t; flags: uint32_t): cint =
  if flags > MPD_Max_status:
    return 0
  ctx.status = flags
  return 1

proc mpd_qsetclamp*(ctx: ptr mpd_context_t; c: cint): cint =
  if c != 0 and c != 1:
    return 0
  ctx.clamp = c
  return 1

proc mpd_qsetcr*(ctx: ptr mpd_context_t; c: cint): cint =
  if c != 0 and c != 1:
    return 0
  ctx.allcr = c
  return 1

proc mpd_addstatus_raise*(ctx: ptr mpd_context_t; flags: uint32_t) =
  ctx.status = ctx.status or flags
  if flags and ctx.traps:
    ctx.newtrap = (flags and ctx.traps)
    mpd_traphandler(ctx)
