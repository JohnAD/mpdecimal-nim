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

## ***************************************************************************
##                  Low level native arithmetic on basic types
## ***************************************************************************
## * ------------------------------------------------------------
## *           Double width multiplication and division
## * ------------------------------------------------------------
##

when defined(config_64):
  when defined(ansi):
    when defined(have_Uint128T):
      proc mpdMulWords*(hi: ptr MpdUintT; lo: ptr MpdUintT; a: MpdUintT; b: MpdUintT) {.
          inline.} =
        var hl: Uint128T
        hl = cast[Uint128T](a * b)
        hi[] = hl shr 64
        lo[] = cast[MpdUintT](hl)

      proc mpdDivWords*(q: ptr MpdUintT; r: ptr MpdUintT; hi: MpdUintT; lo: MpdUintT;
                       d: MpdUintT) {.inline.} =
        var hl: Uint128T
        hl = (cast[Uint128T](hi) shl 64) + lo
        q[] = (mpdUintT)(hl div d)
        ##  quotient is known to fit
        r[] = (mpdUintT)(hl - (uint128T)(q[]) * d)

    else:
      proc mpdMulWords*(hi: ptr MpdUintT; lo: ptr MpdUintT; a: MpdUintT; b: MpdUintT) {.
          inline.} =
        var
          w: array[4, uint32T]
          carry: uint32T
        var
          ah: uint32T
          al: uint32T
          bh: uint32T
          bl: uint32T
        var hl: uint64T
        ah = (uint32T)(a shr 32)
        al = cast[uint32T](a)
        bh = (uint32T)(b shr 32)
        bl = cast[uint32T](b)
        hl = cast[uint64T](al * bl)
        w[0] = cast[uint32T](hl)
        carry = (uint32T)(hl shr 32)
        hl = cast[uint64T](ah * bl) + carry
        w[1] = cast[uint32T](hl)
        w[2] = (uint32T)(hl shr 32)
        hl = cast[uint64T](al * bh) + w[1]
        w[1] = cast[uint32T](hl)
        carry = (uint32T)(hl shr 32)
        hl = (cast[uint64T](ah * bh) + w[2]) + carry
        w[2] = cast[uint32T](hl)
        w[3] = (uint32T)(hl shr 32)
        hi[] = (cast[uint64T](w[3]) shl 32) + w[2]
        lo[] = (cast[uint64T](w[1]) shl 32) + w[0]

      ##
      ##  By Henry S. Warren: http://www.hackersdelight.org/HDcode/divlu.c.txt
      ##  http://www.hackersdelight.org/permissions.htm:
      ##  "You are free to use, copy, and distribute any of the code on this web
      ##   site, whether modified by you or not. You need not give attribution."
      ##
      ##  Slightly modified, comments are mine.
      ##
      proc nlz*(x: uint64T): cint {.inline.} =
        var n: cint
        if x == 0:
          return 64
        n = 0
        if x <= 0x0000000000000000'i64:
          n = n + 32
          x = x shl 32
        if x <= 0x0000000000000000'i64:
          n = n + 16
          x = x shl 16
        if x <= 0x0000000000000000'i64:
          n = n + 8
          x = x shl 8
        if x <= 0x0000000000000000'i64:
          n = n + 4
          x = x shl 4
        if x <= 0x0000000000000000'i64:
          n = n + 2
          x = x shl 2
        if x <= 0x0000000000000000'i64:
          n = n + 1
        return n

      proc mpdDivWords*(q: ptr MpdUintT; r: ptr MpdUintT; u1: MpdUintT; u0: MpdUintT;
                       v: MpdUintT) {.inline.} =
        var b: MpdUintT = 4294967296'i64
        var
          un1: MpdUintT
          un0: MpdUintT
          vn1: MpdUintT
          vn0: MpdUintT
          q1: MpdUintT
          q0: MpdUintT
          un32: MpdUintT
          un21: MpdUintT
          un10: MpdUintT
          rhat: MpdUintT
          t: MpdUintT
        var s: cint
        assert(u1 < v)
        s = nlz(v)
        v = v shl s
        vn1 = v shr 32
        vn0 = v and 0xFFFFFFFF
        t = if (s == 0): 0 else: u0 shr (64 - s)
        un32 = (u1 shl s) or t
        un10 = u0 shl s
        un1 = un10 shr 32
        un0 = un10 and 0xFFFFFFFF
        q1 = un32 div vn1
        rhat = un32 - q1 * vn1
        if q1 >= b or q1 * vn0 > b * rhat + un1:
          q1 = q1 - 1
          rhat = rhat + vn1
          if rhat < b:
            break again1
        un21 = un32 * b + un1 - q1 * v
        q0 = un21 div vn1
        rhat = un21 - q0 * vn1
        if q0 >= b or q0 * vn0 > b * rhat + un0:
          q0 = q0 - 1
          rhat = rhat + vn1
          if rhat < b:
            break again2
        q[] = q1 * b + q0
        r[] = (un21 * b + un0 - q0 * v) shr s

    ##  END ANSI
  elif defined(`asm`):
    proc mpdMulWords*(hi: ptr MpdUintT; lo: ptr MpdUintT; a: MpdUintT; b: MpdUintT) {.
        inline.} =
      var
        h: MpdUintT
        l: MpdUintT
      ##  C2NIM
      ##  __asm__ ( "mulq %3\n\t"
      ##            : "=d" (h), "=a" (l)
      ##            : "%a" (a), "rm" (b)
      ##            : "cc"
      ##  );
      hi[] = h
      lo[] = l

    proc mpdDivWords*(q: ptr MpdUintT; r: ptr MpdUintT; hi: MpdUintT; lo: MpdUintT;
                     d: MpdUintT) {.inline.} =
      var
        qq: MpdUintT
        rr: MpdUintT
      ##  C2NIM
      ##  __asm__ ( "divq %4\n\t"
      ##            : "=a" (qq), "=d" (rr)
      ##            : "a" (lo), "d" (hi), "rm" (d)
      ##            : "cc"
      ##  );
      q[] = qq
      r[] = rr

    ##  END GCC ASM
  elif defined(masm):
    proc mpdMulWords*(hi: ptr MpdUintT; lo: ptr MpdUintT; a: MpdUintT; b: MpdUintT) {.
        inline.} =
      lo[] = umul128(a, b, hi)

    proc mpdDivWords*(q: ptr MpdUintT; r: ptr MpdUintT; hi: MpdUintT; lo: MpdUintT;
                     d: MpdUintT)
    ##  END MASM (_MSC_VER)
  else:
    proc mpdDivmodPow10*(q: ptr MpdUintT; r: ptr MpdUintT; v: MpdUintT; exp: MpdUintT) {.
        inline.} =
      assert(exp <= 19)
      if exp <= 9:
        if exp <= 4:
          case exp
          of 0:
            q[] = v
            r[] = 0
          of 1:
            q[] = v div 10
            r[] = v - q * 10[]
          of 2:
            q[] = v div 100
            r[] = v - q * 100[]
          of 3:
            q[] = v div 1000
            r[] = v - q * 1000[]
          of 4:
            q[] = v div 10000
            r[] = v - q * 10000[]
        else:
          case exp
          of 5:
            q[] = v div 100000
            r[] = v - q * 100000[]
          of 6:
            q[] = v div 1000000
            r[] = v - q * 1000000[]
          of 7:
            q[] = v div 10000000
            r[] = v - q * 10000000[]
          of 8:
            q[] = v div 100000000
            r[] = v - q * 100000000[]
          of 9:
            q[] = v div 1000000000
            r[] = v - q * 1000000000[]
      else:
        if exp <= 14:
          case exp
          of 10:
            q[] = v div 10000000000'i64
            r[] = v - q * 10000000000'i64[]
          of 11:
            q[] = v div 100000000000'i64
            r[] = v - q * 100000000000'i64[]
          of 12:
            q[] = v div 1000000000000'i64
            r[] = v - q * 1000000000000'i64[]
          of 13:
            q[] = v div 10000000000000'i64
            r[] = v - q * 10000000000000'i64[]
          of 14:
            q[] = v div 100000000000000'i64
            r[] = v - q * 100000000000000'i64[]
        else:
          case exp
          of 15:
            q[] = v div 1000000000000000'i64
            r[] = v - q * 1000000000000000'i64[]
          of 16:
            q[] = v div 10000000000000000'i64
            r[] = v - q * 10000000000000000'i64[]
          of 17:
            q[] = v div 100000000000000000'i64
            r[] = v - q * 100000000000000000'i64[]
          of 18:
            q[] = v div 1000000000000000000'i64
            r[] = v - q * 1000000000000000000'i64[]

  ##  END CONFIG_64
elif defined(config_32):
  when defined(ansi):
    when not defined(legacy_Compiler):
      proc mpdMulWords*(hi: ptr MpdUintT; lo: ptr MpdUintT; a: MpdUintT; b: MpdUintT) {.
          inline.} =
        var hl: MpdUuintT
        hl = cast[MpdUuintT](a * b)
        hi[] = hl shr 32
        lo[] = cast[MpdUintT](hl)

      proc mpdDivWords*(q: ptr MpdUintT; r: ptr MpdUintT; hi: MpdUintT; lo: MpdUintT;
                       d: MpdUintT) {.inline.} =
        var hl: MpdUuintT
        hl = (cast[MpdUuintT](hi) shl 32) + lo
        q[] = (mpdUintT)(hl div d)
        ##  quotient is known to fit
        r[] = (mpdUintT)(hl - (mpdUuintT)(q[]) * d)

      ##  END ANSI + uint64_t
    else:
      proc mpdMulWords*(hi: ptr MpdUintT; lo: ptr MpdUintT; a: MpdUintT; b: MpdUintT) {.
          inline.} =
        var
          w: array[4, uint16T]
          carry: uint16T
        var
          ah: uint16T
          al: uint16T
          bh: uint16T
          bl: uint16T
        var hl: uint32T
        ah = (uint16T)(a shr 16)
        al = cast[uint16T](a)
        bh = (uint16T)(b shr 16)
        bl = cast[uint16T](b)
        hl = cast[uint32T](al * bl)
        w[0] = cast[uint16T](hl)
        carry = (uint16T)(hl shr 16)
        hl = cast[uint32T](ah * bl) + carry
        w[1] = cast[uint16T](hl)
        w[2] = (uint16T)(hl shr 16)
        hl = cast[uint32T](al * bh) + w[1]
        w[1] = cast[uint16T](hl)
        carry = (uint16T)(hl shr 16)
        hl = (cast[uint32T](ah * bh) + w[2]) + carry
        w[2] = cast[uint16T](hl)
        w[3] = (uint16T)(hl shr 16)
        hi[] = (cast[uint32T](w[3]) shl 16) + w[2]
        lo[] = (cast[uint32T](w[1]) shl 16) + w[0]

      ##
      ##  By Henry S. Warren: http://www.hackersdelight.org/HDcode/divlu.c.txt
      ##  http://www.hackersdelight.org/permissions.htm:
      ##  "You are free to use, copy, and distribute any of the code on this web
      ##   site, whether modified by you or not. You need not give attribution."
      ##
      ##  Slightly modified, comments are mine.
      ##
      proc nlz*(x: uint32T): cint {.inline.} =
        var n: cint
        if x == 0:
          return 32
        n = 0
        if x <= 0x0000FFFF:
          n = n + 16
          x = x shl 16
        if x <= 0x00FFFFFF:
          n = n + 8
          x = x shl 8
        if x <= 0x0FFFFFFF:
          n = n + 4
          x = x shl 4
        if x <= 0x3FFFFFFF:
          n = n + 2
          x = x shl 2
        if x <= 0x7FFFFFFF:
          n = n + 1
        return n

      proc mpdDivWords*(q: ptr MpdUintT; r: ptr MpdUintT; u1: MpdUintT; u0: MpdUintT;
                       v: MpdUintT) {.inline.} =
        var b: MpdUintT = 65536
        var
          un1: MpdUintT
          un0: MpdUintT
          vn1: MpdUintT
          vn0: MpdUintT
          q1: MpdUintT
          q0: MpdUintT
          un32: MpdUintT
          un21: MpdUintT
          un10: MpdUintT
          rhat: MpdUintT
          t: MpdUintT
        var s: cint
        assert(u1 < v)
        s = nlz(v)
        v = v shl s
        vn1 = v shr 16
        vn0 = v and 0x0000FFFF
        t = if (s == 0): 0 else: u0 shr (32 - s)
        un32 = (u1 shl s) or t
        un10 = u0 shl s
        un1 = un10 shr 16
        un0 = un10 and 0x0000FFFF
        q1 = un32 div vn1
        rhat = un32 - q1 * vn1
        if q1 >= b or q1 * vn0 > b * rhat + un1:
          q1 = q1 - 1
          rhat = rhat + vn1
          if rhat < b:
            break again1
        un21 = un32 * b + un1 - q1 * v
        q0 = un21 div vn1
        rhat = un21 - q0 * vn1
        if q0 >= b or q0 * vn0 > b * rhat + un0:
          q0 = q0 - 1
          rhat = rhat + vn1
          if rhat < b:
            break again2
        q[] = q1 * b + q0
        r[] = (un21 * b + un0 - q0 * v) shr s

    ##  END ANSI
  elif defined(`asm`):
    proc mpdMulWords*(hi: ptr MpdUintT; lo: ptr MpdUintT; a: MpdUintT; b: MpdUintT) {.
        inline.} =
      var
        h: MpdUintT
        l: MpdUintT
      ##  C2NIM
      ##  __asm__ ( "mull %3\n\t"
      ##            : "=d" (h), "=a" (l)
      ##            : "%a" (a), "rm" (b)
      ##            : "cc"
      ##  );
      hi[] = h
      lo[] = l

    proc mpdDivWords*(q: ptr MpdUintT; r: ptr MpdUintT; hi: MpdUintT; lo: MpdUintT;
                     d: MpdUintT) {.inline.} =
      var
        qq: MpdUintT
        rr: MpdUintT
      ##  C2NIM
      ##  __asm__ ( "divl %4\n\t"
      ##            : "=a" (qq), "=d" (rr)
      ##            : "a" (lo), "d" (hi), "rm" (d)
      ##            : "cc"
      ##  );
      q[] = qq
      r[] = rr

    ##  END GCC ASM
  elif defined(masm):
    proc mpdMulWords*(hi: ptr MpdUintT; lo: ptr MpdUintT; a: MpdUintT; b: MpdUintT) {.
        inline, cdecl.} =
      var
        h: MpdUintT
        l: MpdUintT
      ##  C2NIM
      ##  __asm {
      ##      mov eax, a
      ##      mul b
      ##      mov h, edx
      ##      mov l, eax
      ##  }
      hi[] = h
      lo[] = l

    proc mpdDivWords*(q: ptr MpdUintT; r: ptr MpdUintT; hi: MpdUintT; lo: MpdUintT;
                     d: MpdUintT) {.inline, cdecl.} =
      var
        qq: MpdUintT
        rr: MpdUintT
      ##  C2NIM
      ##  __asm {
      ##      mov eax, lo
      ##      mov edx, hi
      ##      div d
      ##      mov qq, eax
      ##      mov rr, edx
      ##  }
      q[] = qq
      r[] = rr

    ##  END MASM (_MSC_VER)
  ##  C2NIM
  ##  #define DIVMOD(q, r, v, d) *q = v / d; *r = v - *q * d
  proc mpdDivmodPow10*(q: ptr MpdUintT; r: ptr MpdUintT; v: MpdUintT; exp: MpdUintT) {.
      inline.} =
    assert(exp <= 9)
    if exp <= 4:
      case exp
      of 0:
        q[] = v
        r[] = 0
      of 1:
        q[] = v div 10
        r[] = v - q * 10[]
      of 2:
        q[] = v div 100
        r[] = v - q * 100[]
      of 3:
        q[] = v div 1000
        r[] = v - q * 1000[]
      of 4:
        q[] = v div 10000
        r[] = v - q * 10000[]
    else:
      case exp
      of 5:
        q[] = v div 100000
        r[] = v - q * 100000[]
      of 6:
        q[] = v div 1000000
        r[] = v - q * 1000000[]
      of 7:
        q[] = v div 10000000
        r[] = v - q * 10000000[]
      of 8:
        q[] = v div 100000000
        r[] = v - q * 100000000[]
      of 9:
        q[] = v div 1000000000
        r[] = v - q * 1000000000[]

  ##  END CONFIG_32
  ##  NO CONFIG
proc mpdDivWord*(q: ptr MpdUintT; r: ptr MpdUintT; v: MpdUintT; d: MpdUintT) {.inline.} =
  q[] = v div d
  r[] = v - q * d[]

proc mpdIdivWord*(q: ptr MpdSsizeT; r: ptr MpdSsizeT; v: MpdSsizeT; d: MpdSsizeT) {.inline.} =
  q[] = v div d
  r[] = v - q * d[]

## * ------------------------------------------------------------
## *              Arithmetic with overflow checking
## * ------------------------------------------------------------
##
##  The following macros do call exit() in case of an overflow.
##    If the library is used correctly (i.e. with valid context
##    parameters), such overflows cannot occur. The macros are used
##    as sanity checks in a couple of strategic places and should
##    be viewed as a handwritten version of gcc's -ftrapv option.

proc addSizeT*(a: MpdSizeT; b: MpdSizeT): MpdSizeT {.inline.} =
  if a > mpd_Size_Max - b:
    mpdErrFatal("add_size_t(): overflow: check the context")
    ##  GCOV_NOT_REACHED
  return a + b

proc subSizeT*(a: MpdSizeT; b: MpdSizeT): MpdSizeT {.inline.} =
  if b > a:
    mpdErrFatal("sub_size_t(): overflow: check the context")
    ##  GCOV_NOT_REACHED
  return a - b

when mpd_Size_Max != mpd_Uint_Max:
proc mulSizeT*(a: MpdSizeT; b: MpdSizeT): MpdSizeT {.inline.} =
  var
    hi: MpdUintT
    lo: MpdUintT
  mpdMulWords(addr(hi), addr(lo), cast[MpdUintT](a), cast[MpdUintT](b))
  if hi:
    mpdErrFatal("mul_size_t(): overflow: check the context")
    ##  GCOV_NOT_REACHED
  return lo

proc addSizeTOverflow*(a: MpdSizeT; b: MpdSizeT; overflow: ptr MpdSizeT): MpdSizeT {.
    inline.} =
  var ret: MpdSizeT
  overflow[] = 0
  ret = a + b
  if ret < a:
    overflow[] = 1
  return ret

proc mulSizeTOverflow*(a: MpdSizeT; b: MpdSizeT; overflow: ptr MpdSizeT): MpdSizeT {.
    inline.} =
  var
    hi: MpdUintT
    lo: MpdUintT
  mpdMulWords(addr(hi), addr(lo), cast[MpdUintT](a), cast[MpdUintT](b))
  overflow[] = cast[MpdSizeT](hi)
  return lo

proc modMpdSsizeT*(a: MpdSsizeT; m: MpdSsizeT): MpdSsizeT {.inline.} =
  var r: MpdSsizeT = a mod m
  return if (r < 0): r + m else: r

proc mulmodSizeT*(a: MpdSizeT; b: MpdSizeT; m: MpdSizeT): MpdSizeT {.inline.} =
  var
    hi: MpdUintT
    lo: MpdUintT
  var
    q: MpdUintT
    r: MpdUintT
  mpdMulWords(addr(hi), addr(lo), cast[MpdUintT](a), cast[MpdUintT](b))
  mpdDivWords(addr(q), addr(r), hi, lo, cast[MpdUintT](m))
  return r
