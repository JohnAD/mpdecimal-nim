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

proc mpd_format*(dec: ptr mpd_t; fmt: cstring; ctx: ptr mpd_context_t): cstring =
  var ret: cstring
  var status: uint32_t = 0
  ret = mpd_qformat(dec, fmt, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)
  return ret

proc mpd_import_u16*(result: ptr mpd_t; srcdata: ptr uint16_t; srclen: csize;
                    srcsign: uint8_t; base: uint32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qimport_u16(result, srcdata, srclen, srcsign, base, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_import_u32*(result: ptr mpd_t; srcdata: ptr uint32_t; srclen: csize;
                    srcsign: uint8_t; base: uint32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qimport_u32(result, srcdata, srclen, srcsign, base, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_export_u16*(rdata: ptr ptr uint16_t; rlen: csize; base: uint32_t;
                    src: ptr mpd_t; ctx: ptr mpd_context_t): csize =
  var n: csize
  var status: uint32_t = 0
  n = mpd_qexport_u16(rdata, rlen, base, src, addr(status))
  mpd_addstatus_raise(ctx, status)
  return n

proc mpd_export_u32*(rdata: ptr ptr uint32_t; rlen: csize; base: uint32_t;
                    src: ptr mpd_t; ctx: ptr mpd_context_t): csize =
  var n: csize
  var status: uint32_t = 0
  n = mpd_qexport_u32(rdata, rlen, base, src, addr(status))
  mpd_addstatus_raise(ctx, status)
  return n

proc mpd_finalize*(result: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qfinalize(result, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_check_nan*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t): cint =
  var status: uint32_t = 0
  if mpd_qcheck_nan(result, a, ctx, addr(status)):
    mpd_addstatus_raise(ctx, status)
    return 1
  return 0

proc mpd_check_nans*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t): cint =
  var status: uint32_t = 0
  if mpd_qcheck_nans(result, a, b, ctx, addr(status)):
    mpd_addstatus_raise(ctx, status)
    return 1
  return 0

proc mpd_set_string*(result: ptr mpd_t; s: cstring; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qset_string(result, s, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_maxcoeff*(result: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmaxcoeff(result, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

##  set static mpd from signed integer

proc mpd_sset_ssize*(result: ptr mpd_t; a: mpd_ssize_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsset_ssize(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_sset_i32*(result: ptr mpd_t; a: int32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsset_i32(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when defined(CONFIG_64):
  proc mpd_sset_i64*(result: ptr mpd_t; a: int64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qsset_i64(result, a, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

##  set static mpd from unsigned integer

proc mpd_sset_uint*(result: ptr mpd_t; a: mpd_uint_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsset_uint(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_sset_u32*(result: ptr mpd_t; a: uint32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsset_u32(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when defined(CONFIG_64):
  proc mpd_sset_u64*(result: ptr mpd_t; a: uint64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qsset_u64(result, a, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

##  set mpd from signed integer

proc mpd_set_ssize*(result: ptr mpd_t; a: mpd_ssize_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qset_ssize(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_set_i32*(result: ptr mpd_t; a: int32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qset_i32(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_set_i64*(result: ptr mpd_t; a: int64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qset_i64(result, a, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

##  set mpd from unsigned integer

proc mpd_set_uint*(result: ptr mpd_t; a: mpd_uint_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qset_uint(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_set_u32*(result: ptr mpd_t; a: uint32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qset_u32(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_set_u64*(result: ptr mpd_t; a: uint64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qset_u64(result, a, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

##  convert mpd to signed integer

proc mpd_get_ssize*(a: ptr mpd_t; ctx: ptr mpd_context_t): mpd_ssize_t =
  var status: uint32_t = 0
  var ret: mpd_ssize_t
  ret = mpd_qget_ssize(a, addr(status))
  mpd_addstatus_raise(ctx, status)
  return ret

proc mpd_get_i32*(a: ptr mpd_t; ctx: ptr mpd_context_t): int32_t =
  var status: uint32_t = 0
  var ret: int32_t
  ret = mpd_qget_i32(a, addr(status))
  mpd_addstatus_raise(ctx, status)
  return ret

when not defined(LEGACY_COMPILER):
  proc mpd_get_i64*(a: ptr mpd_t; ctx: ptr mpd_context_t): int64_t =
    var status: uint32_t = 0
    var ret: int64_t
    ret = mpd_qget_i64(a, addr(status))
    mpd_addstatus_raise(ctx, status)
    return ret

proc mpd_get_uint*(a: ptr mpd_t; ctx: ptr mpd_context_t): mpd_uint_t =
  var status: uint32_t = 0
  var ret: mpd_uint_t
  ret = mpd_qget_uint(a, addr(status))
  mpd_addstatus_raise(ctx, status)
  return ret

proc mpd_abs_uint*(a: ptr mpd_t; ctx: ptr mpd_context_t): mpd_uint_t =
  var status: uint32_t = 0
  var ret: mpd_uint_t
  ret = mpd_qabs_uint(a, addr(status))
  mpd_addstatus_raise(ctx, status)
  return ret

proc mpd_get_u32*(a: ptr mpd_t; ctx: ptr mpd_context_t): uint32_t =
  var status: uint32_t = 0
  var ret: uint32_t
  ret = mpd_qget_u32(a, addr(status))
  mpd_addstatus_raise(ctx, status)
  return ret

when not defined(LEGACY_COMPILER):
  proc mpd_get_u64*(a: ptr mpd_t; ctx: ptr mpd_context_t): uint64_t =
    var status: uint32_t = 0
    var ret: uint64_t
    ret = mpd_qget_u64(a, addr(status))
    mpd_addstatus_raise(ctx, status)
    return ret

proc mpd_and*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qand(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_copy*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  if not mpd_qcopy(result, a, addr(status)):
    mpd_addstatus_raise(ctx, status)

proc mpd_canonical*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  mpd_copy(result, a, ctx)

proc mpd_copy_abs*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  if not mpd_qcopy_abs(result, a, addr(status)):
    mpd_addstatus_raise(ctx, status)

proc mpd_copy_negate*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  if not mpd_qcopy_negate(result, a, addr(status)):
    mpd_addstatus_raise(ctx, status)

proc mpd_copy_sign*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  if not mpd_qcopy_sign(result, a, b, addr(status)):
    mpd_addstatus_raise(ctx, status)

proc mpd_invert*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qinvert(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_logb*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qlogb(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_or*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qor(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_rotate*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qrotate(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_scaleb*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qscaleb(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_shiftl*(result: ptr mpd_t; a: ptr mpd_t; n: mpd_ssize_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qshiftl(result, a, n, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_shiftr*(result: ptr mpd_t; a: ptr mpd_t; n: mpd_ssize_t; ctx: ptr mpd_context_t): mpd_uint_t =
  var status: uint32_t = 0
  var rnd: mpd_uint_t
  rnd = mpd_qshiftr(result, a, n, addr(status))
  mpd_addstatus_raise(ctx, status)
  return rnd

proc mpd_shiftn*(result: ptr mpd_t; a: ptr mpd_t; n: mpd_ssize_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qshiftn(result, a, n, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_shift*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qshift(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_xor*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qxor(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_abs*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qabs(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_cmp*(a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t): cint =
  var status: uint32_t = 0
  var c: cint
  c = mpd_qcmp(a, b, addr(status))
  mpd_addstatus_raise(ctx, status)
  return c

proc mpd_compare*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t): cint =
  var status: uint32_t = 0
  var c: cint
  c = mpd_qcompare(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)
  return c

proc mpd_compare_signal*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                        ctx: ptr mpd_context_t): cint =
  var status: uint32_t = 0
  var c: cint
  c = mpd_qcompare_signal(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)
  return c

proc mpd_add*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qadd(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_sub*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsub(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_add_ssize*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_ssize_t;
                   ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qadd_ssize(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_add_i32*(result: ptr mpd_t; a: ptr mpd_t; b: int32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qadd_i32(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_add_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qadd_i64(result, a, b, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

proc mpd_add_uint*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_uint_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qadd_uint(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_add_u32*(result: ptr mpd_t; a: ptr mpd_t; b: uint32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qadd_u32(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_add_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qadd_u64(result, a, b, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

proc mpd_sub_ssize*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_ssize_t;
                   ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsub_ssize(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_sub_i32*(result: ptr mpd_t; a: ptr mpd_t; b: int32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsub_i32(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_sub_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qsub_i64(result, a, b, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

proc mpd_sub_uint*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_uint_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsub_uint(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_sub_u32*(result: ptr mpd_t; a: ptr mpd_t; b: uint32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsub_u32(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_sub_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qsub_u64(result, a, b, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

proc mpd_div*(q: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qdiv(q, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_div_ssize*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_ssize_t;
                   ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qdiv_ssize(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_div_i32*(result: ptr mpd_t; a: ptr mpd_t; b: int32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qdiv_i32(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_div_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qdiv_i64(result, a, b, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

proc mpd_div_uint*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_uint_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qdiv_uint(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_div_u32*(result: ptr mpd_t; a: ptr mpd_t; b: uint32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qdiv_u32(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_div_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qdiv_u64(result, a, b, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

proc mpd_divmod*(q: ptr mpd_t; r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qdivmod(q, r, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_divint*(q: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qdivint(q, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_exp*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qexp(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_fma*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; c: ptr mpd_t;
             ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qfma(result, a, b, c, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_ln*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qln(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_log10*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qlog10(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_max*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmax(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_max_mag*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmax_mag(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_min*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmin(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_min_mag*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmin_mag(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_minus*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qminus(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_mul*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmul(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_mul_ssize*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_ssize_t;
                   ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmul_ssize(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_mul_i32*(result: ptr mpd_t; a: ptr mpd_t; b: int32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmul_i32(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_mul_i64*(result: ptr mpd_t; a: ptr mpd_t; b: int64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qmul_i64(result, a, b, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

proc mpd_mul_uint*(result: ptr mpd_t; a: ptr mpd_t; b: mpd_uint_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmul_uint(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_mul_u32*(result: ptr mpd_t; a: ptr mpd_t; b: uint32_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qmul_u32(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

when not defined(LEGACY_COMPILER):
  proc mpd_mul_u64*(result: ptr mpd_t; a: ptr mpd_t; b: uint64_t; ctx: ptr mpd_context_t) =
    var status: uint32_t = 0
    mpd_qmul_u64(result, a, b, ctx, addr(status))
    mpd_addstatus_raise(ctx, status)

proc mpd_next_minus*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qnext_minus(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_next_plus*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qnext_plus(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_next_toward*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t;
                     ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qnext_toward(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_plus*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qplus(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_pow*(result: ptr mpd_t; base: ptr mpd_t; exp: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qpow(result, base, exp, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_powmod*(result: ptr mpd_t; base: ptr mpd_t; exp: ptr mpd_t; `mod`: ptr mpd_t;
                ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qpowmod(result, base, exp, `mod`, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_quantize*(result: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qquantize(result, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_rescale*(result: ptr mpd_t; a: ptr mpd_t; exp: mpd_ssize_t;
                 ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qrescale(result, a, exp, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_reduce*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qreduce(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_rem*(r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qrem(r, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_rem_near*(r: ptr mpd_t; a: ptr mpd_t; b: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qrem_near(r, a, b, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_round_to_intx*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qround_to_intx(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_round_to_int*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qround_to_int(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_trunc*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qtrunc(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_floor*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qfloor(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_ceil*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qceil(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_sqrt*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qsqrt(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)

proc mpd_invroot*(result: ptr mpd_t; a: ptr mpd_t; ctx: ptr mpd_context_t) =
  var status: uint32_t = 0
  mpd_qinvroot(result, a, ctx, addr(status))
  mpd_addstatus_raise(ctx, status)
