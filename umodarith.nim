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
  mpdecimal, constants, typearith

##  Bignum: Low level routines for unsigned modular arithmetic. These are
##    used in the fast convolution functions for very large coefficients.
## ************************************************************************
##                         ANSI modular arithmetic
## ************************************************************************
##
##  Restrictions: a < m and b < m
##  ACL2 proof: umodarith.lisp: addmod-correct
##

proc addmod*(a: MpdUintT; b: MpdUintT; m: MpdUintT): MpdUintT {.inline.} =
  var s: MpdUintT
  s = a + b
  s = if (s < a): s - m else: s
  s = if (s >= m): s - m else: s
  return s

##
##  Restrictions: a < m and b < m
##  ACL2 proof: umodarith.lisp: submod-2-correct
##

proc submod*(a: MpdUintT; b: MpdUintT; m: MpdUintT): MpdUintT {.inline.} =
  var d: MpdUintT
  d = a - b
  d = if (a < b): d + m else: d
  return d

##
##  Restrictions: a < 2m and b < 2m
##  ACL2 proof: umodarith.lisp: section ext-submod
##

proc extSubmod*(a: MpdUintT; b: MpdUintT; m: MpdUintT): MpdUintT {.inline.} =
  var d: MpdUintT
  a = if (a >= m): a - m else: a
  b = if (b >= m): b - m else: b
  d = a - b
  d = if (a < b): d + m else: d
  return d

##
##  Reduce double word modulo m.
##  Restrictions: m != 0
##  ACL2 proof: umodarith.lisp: section dw-reduce
##

proc dwReduce*(hi: MpdUintT; lo: MpdUintT; m: MpdUintT): MpdUintT {.inline.} =
  var
    r1: MpdUintT
    r2: MpdUintT
    w: MpdUintT
  mpdDivWord(addr(w), addr(r1), hi, m)
  mpdDivWords(addr(w), addr(r2), r1, lo, m)
  return r2

##
##  Subtract double word from a.
##  Restrictions: a < m
##  ACL2 proof: umodarith.lisp: section dw-submod
##

proc dwSubmod*(a: MpdUintT; hi: MpdUintT; lo: MpdUintT; m: MpdUintT): MpdUintT {.inline.} =
  var
    d: MpdUintT
    r: MpdUintT
  r = dwReduce(hi, lo, m)
  d = a - r
  d = if (a < r): d + m else: d
  return d

when defined(CONFIG_64):
  ## ************************************************************************
  ##                         64-bit modular arithmetic
  ## ************************************************************************
  ##
  ##  A proof of the algorithm is in literature/mulmod-64.txt. An ACL2
  ##  proof is in umodarith.lisp: section "Fast modular reduction".
  ##
  ##  Algorithm: calculate (a * b) % p:
  ##
  ##    a) hi, lo <- a * b       # Calculate a * b.
  ##
  ##    b) hi, lo <-  R(hi, lo)  # Reduce modulo p.
  ##
  ##    c) Repeat step b) until 0 <= hi * 2**64 + lo < 2*p.
  ##
  ##    d) If the result is less than p, return lo. Otherwise return lo - p.
  ##
  proc x64Mulmod*(a: MpdUintT; b: MpdUintT; m: MpdUintT): MpdUintT {.inline.} =
    var
      hi: MpdUintT
      lo: MpdUintT
      x: MpdUintT
      y: MpdUintT
    mpdMulWords(addr(hi), addr(lo), a, b)
    if m and (1 shl 32):
      ##  P1
      ##  first reduction
      x = y = hi
      hi = hi shr 32
      x = lo - x
      if x > lo:
        dec(hi)
      y = y shl 32
      lo = y + x
      if lo < y:
        inc(hi)
      x = y = hi
      hi = hi shr 32
      x = lo - x
      if x > lo:
        dec(hi)
      y = y shl 32
      lo = y + x
      if lo < y:
        inc(hi)
      return if hi or lo >= m: lo - m else: lo
    elif m and (1 shl 34):
      ##  P2
      ##  first reduction
      x = y = hi
      hi = hi shr 30
      x = lo - x
      if x > lo:
        dec(hi)
      y = y shl 34
      lo = y + x
      if lo < y:
        inc(hi)
      x = y = hi
      hi = hi shr 30
      x = lo - x
      if x > lo:
        dec(hi)
      y = y shl 34
      lo = y + x
      if lo < y:
        inc(hi)
      x = y = hi
      hi = hi shr 30
      x = lo - x
      if x > lo:
        dec(hi)
      y = y shl 34
      lo = y + x
      if lo < y:
        inc(hi)
      return if hi or lo >= m: lo - m else: lo
    else:
      ##  P3
      ##  first reduction
      x = y = hi
      hi = hi shr 24
      x = lo - x
      if x > lo:
        dec(hi)
      y = y shl 40
      lo = y + x
      if lo < y:
        inc(hi)
      x = y = hi
      hi = hi shr 24
      x = lo - x
      if x > lo:
        dec(hi)
      y = y shl 40
      lo = y + x
      if lo < y:
        inc(hi)
      x = y = hi
      hi = hi shr 24
      x = lo - x
      if x > lo:
        dec(hi)
      y = y shl 40
      lo = y + x
      if lo < y:
        inc(hi)
      return if hi or lo >= m: lo - m else: lo

  proc x64Mulmod2c*(a: ptr MpdUintT; b: ptr MpdUintT; w: MpdUintT; m: MpdUintT) {.inline.} =
    a[] = x64Mulmod(a[], w, m)
    b[] = x64Mulmod(b[], w, m)

  proc x64Mulmod2*(a0: ptr MpdUintT; b0: MpdUintT; a1: ptr MpdUintT; b1: MpdUintT;
                  m: MpdUintT) {.inline.} =
    a0[] = x64Mulmod(a0[], b0, m)
    a1[] = x64Mulmod(a1[], b1, m)

  proc x64Powmod*(base: MpdUintT; exp: MpdUintT; umod: MpdUintT): MpdUintT {.inline.} =
    var r: MpdUintT = 1
    while exp > 0:
      if exp and 1:
        r = x64Mulmod(r, base, umod)
      base = x64Mulmod(base, base, umod)
      exp = exp shr 1
    return r

  ##  END CONFIG_64
else:
  ## ************************************************************************
  ##                         32-bit modular arithmetic
  ## ************************************************************************
  when defined(ansi):
    when not defined(legacy_Compiler):
      ##  HAVE_UINT64_T
      proc stdMulmod*(a: MpdUintT; b: MpdUintT; m: MpdUintT): MpdUintT {.inline.} =
        return (cast[MpdUuintT](a * b)) mod m

      proc stdMulmod2c*(a: ptr MpdUintT; b: ptr MpdUintT; w: MpdUintT; m: MpdUintT) {.
          inline.} =
        a[] = ((mpdUuintT) * a * w) mod m
        b[] = ((mpdUuintT) * b * w) mod m

      proc stdMulmod2*(a0: ptr MpdUintT; b0: MpdUintT; a1: ptr MpdUintT; b1: MpdUintT;
                      m: MpdUintT) {.inline.} =
        a0[] = ((mpdUuintT) * a0 * b0) mod m
        a1[] = ((mpdUuintT) * a1 * b1) mod m

      ##  END HAVE_UINT64_T
    else:
      ##  LEGACY_COMPILER
      proc stdMulmod*(a: MpdUintT; b: MpdUintT; m: MpdUintT): MpdUintT {.inline.} =
        var
          hi: MpdUintT
          lo: MpdUintT
          q: MpdUintT
          r: MpdUintT
        mpdMulWords(addr(hi), addr(lo), a, b)
        mpdDivWords(addr(q), addr(r), hi, lo, m)
        return r

      proc stdMulmod2c*(a: ptr MpdUintT; b: ptr MpdUintT; w: MpdUintT; m: MpdUintT) {.
          inline.} =
        a[] = stdMulmod(a[], w, m)
        b[] = stdMulmod(b[], w, m)

      proc stdMulmod2*(a0: ptr MpdUintT; b0: MpdUintT; a1: ptr MpdUintT; b1: MpdUintT;
                      m: MpdUintT) {.inline.} =
        a0[] = stdMulmod(a0[], b0, m)
        a1[] = stdMulmod(a1[], b1, m)

      ##  END LEGACY_COMPILER
    proc stdPowmod*(base: MpdUintT; exp: MpdUintT; umod: MpdUintT): MpdUintT {.inline.} =
      var r: MpdUintT = 1
      while exp > 0:
        if exp and 1:
          r = stdMulmod(r, base, umod)
        base = stdMulmod(base, base, umod)
        exp = exp shr 1
      return r

  ## ************************************************************************
  ##                     Pentium Pro modular arithmetic
  ## ************************************************************************
  ##
  ##  A proof of the algorithm is in literature/mulmod-ppro.txt. The FPU
  ##  control word must be set to 64-bit precision and truncation mode
  ##  prior to using these functions.
  ##
  ##  Algorithm: calculate (a * b) % p:
  ##
  ##    p    := prime < 2**31
  ##    pinv := (long double)1.0 / p (precalculated)
  ##
  ##    a) n = a * b              # Calculate exact product.
  ##    b) qest = n * pinv        # Calculate estimate for q = n / p.
  ##    c) q = (qest+2**63)-2**63 # Truncate qest to the exact quotient.
  ##    d) r = n - q * p          # Calculate remainder.
  ##
  ##  Remarks:
  ##
  ##    - p = dmod and pinv = dinvmod.
  ##    - dinvmod points to an array of three uint32_t, which is interpreted
  ##      as an 80 bit long double by fldt.
  ##    - Intel compilers prior to version 11 do not seem to handle the
  ##      __GNUC__ inline assembly correctly.
  ##    - random tests are provided in tests/extended/ppro_mulmod.c
  ##
  when defined(ppro):
    when defined(`asm`):
      ##  Return (a * b) % dmod
      proc pproMulmod*(a: MpdUintT; b: MpdUintT; dmod: ptr cdouble; dinvmod: ptr uint32T): MpdUintT {.
          inline.} =
        var retval: MpdUintT
        ##  C2NIM
        ##  __asm__ (
        ##          "fildl  %2\n\t"
        ##          "fildl  %1\n\t"
        ##          "fmulp  %%st, %%st(1)\n\t"
        ##          "fldt   (%4)\n\t"
        ##          "fmul   %%st(1), %%st\n\t"
        ##          "flds   %5\n\t"
        ##          "fadd   %%st, %%st(1)\n\t"
        ##          "fsubrp %%st, %%st(1)\n\t"
        ##          "fldl   (%3)\n\t"
        ##          "fmulp  %%st, %%st(1)\n\t"
        ##          "fsubrp %%st, %%st(1)\n\t"
        ##          "fistpl %0\n\t"
        ##          : "=m" (retval)
        ##          : "m" (a), "m" (b), "r" (dmod), "r" (dinvmod), "m" (MPD_TWO63)
        ##          : "st", "memory"
        ##  );
        return retval

      ##
      ##  Two modular multiplications in parallel:
      ##       *a0 = (*a0 * w) % dmod
      ##       *a1 = (*a1 * w) % dmod
      ##
      proc pproMulmod2c*(a0: ptr MpdUintT; a1: ptr MpdUintT; w: MpdUintT;
                        dmod: ptr cdouble; dinvmod: ptr uint32T) {.inline.} =
        ##  C2NIM
        ##  __asm__ (
        ##          "fildl  %2\n\t"
        ##          "fildl  (%1)\n\t"
        ##          "fmul   %%st(1), %%st\n\t"
        ##          "fxch   %%st(1)\n\t"
        ##          "fildl  (%0)\n\t"
        ##          "fmulp  %%st, %%st(1) \n\t"
        ##          "fldt   (%4)\n\t"
        ##          "flds   %5\n\t"
        ##          "fld    %%st(2)\n\t"
        ##          "fmul   %%st(2)\n\t"
        ##          "fadd   %%st(1)\n\t"
        ##          "fsub   %%st(1)\n\t"
        ##          "fmull  (%3)\n\t"
        ##          "fsubrp %%st, %%st(3)\n\t"
        ##          "fxch   %%st(2)\n\t"
        ##          "fistpl (%0)\n\t"
        ##          "fmul   %%st(2)\n\t"
        ##          "fadd   %%st(1)\n\t"
        ##          "fsubp  %%st, %%st(1)\n\t"
        ##          "fmull  (%3)\n\t"
        ##          "fsubrp %%st, %%st(1)\n\t"
        ##          "fistpl (%1)\n\t"
        ##          : : "r" (a0), "r" (a1), "m" (w),
        ##              "r" (dmod), "r" (dinvmod),
        ##              "m" (MPD_TWO63)
        ##          : "st", "memory"
        ##  );

      ##
      ##  Two modular multiplications in parallel:
      ##       *a0 = (*a0 * b0) % dmod
      ##       *a1 = (*a1 * b1) % dmod
      ##
      proc pproMulmod2*(a0: ptr MpdUintT; b0: MpdUintT; a1: ptr MpdUintT; b1: MpdUintT;
                       dmod: ptr cdouble; dinvmod: ptr uint32T) {.inline.} =
        ##  C2NIM
        ##  __asm__ (
        ##          "fildl  %3\n\t"
        ##          "fildl  (%2)\n\t"
        ##          "fmulp  %%st, %%st(1)\n\t"
        ##          "fildl  %1\n\t"
        ##          "fildl  (%0)\n\t"
        ##          "fmulp  %%st, %%st(1)\n\t"
        ##          "fldt   (%5)\n\t"
        ##          "fld    %%st(2)\n\t"
        ##          "fmul   %%st(1), %%st\n\t"
        ##          "fxch   %%st(1)\n\t"
        ##          "fmul   %%st(2), %%st\n\t"
        ##          "flds   %6\n\t"
        ##          "fldl   (%4)\n\t"
        ##          "fxch   %%st(3)\n\t"
        ##          "fadd   %%st(1), %%st\n\t"
        ##          "fxch   %%st(2)\n\t"
        ##          "fadd   %%st(1), %%st\n\t"
        ##          "fxch   %%st(2)\n\t"
        ##          "fsub   %%st(1), %%st\n\t"
        ##          "fxch   %%st(2)\n\t"
        ##          "fsubp  %%st, %%st(1)\n\t"
        ##          "fxch   %%st(1)\n\t"
        ##          "fmul   %%st(2), %%st\n\t"
        ##          "fxch   %%st(1)\n\t"
        ##          "fmulp  %%st, %%st(2)\n\t"
        ##          "fsubrp %%st, %%st(3)\n\t"
        ##          "fsubrp %%st, %%st(1)\n\t"
        ##          "fxch   %%st(1)\n\t"
        ##          "fistpl (%2)\n\t"
        ##          "fistpl (%0)\n\t"
        ##          : : "r" (a0), "m" (b0), "r" (a1), "m" (b1),
        ##              "r" (dmod), "r" (dinvmod),
        ##              "m" (MPD_TWO63)
        ##          : "st", "memory"
        ##  );

      ##  END PPRO GCC ASM
    elif defined(masm):
      ##  Return (a * b) % dmod
      proc pproMulmod*(a: MpdUintT; b: MpdUintT; dmod: ptr cdouble; dinvmod: ptr uint32T): MpdUintT {.
          inline, cdecl.} =
        var retval: MpdUintT
        ##  C2NIM
        ##  __asm {
        ##      mov     eax, dinvmod
        ##      mov     edx, dmod
        ##      fild    b
        ##      fild    a
        ##      fmulp   st(1), st
        ##      fld     TBYTE PTR [eax]
        ##      fmul    st, st(1)
        ##      fld     MPD_TWO63
        ##      fadd    st(1), st
        ##      fsubp   st(1), st
        ##      fld     QWORD PTR [edx]
        ##      fmulp   st(1), st
        ##      fsubp   st(1), st
        ##      fistp   retval
        ##  }
        return retval

      ##
      ##  Two modular multiplications in parallel:
      ##       *a0 = (*a0 * w) % dmod
      ##       *a1 = (*a1 * w) % dmod
      ##
      proc pproMulmod2c*(a0: ptr MpdUintT; a1: ptr MpdUintT; w: MpdUintT;
                        dmod: ptr cdouble; dinvmod: ptr uint32T): MpdUintT {.inline,
          cdecl.} =
        ##  C2NIM
        ##  __asm {
        ##      mov     ecx, dmod
        ##      mov     edx, a1
        ##      mov     ebx, dinvmod
        ##      mov     eax, a0
        ##      fild    w
        ##      fild    DWORD PTR [edx]
        ##      fmul    st, st(1)
        ##      fxch    st(1)
        ##      fild    DWORD PTR [eax]
        ##      fmulp   st(1), st
        ##      fld     TBYTE PTR [ebx]
        ##      fld     MPD_TWO63
        ##      fld     st(2)
        ##      fmul    st, st(2)
        ##      fadd    st, st(1)
        ##      fsub    st, st(1)
        ##      fmul    QWORD PTR [ecx]
        ##      fsubp   st(3), st
        ##      fxch    st(2)
        ##      fistp   DWORD PTR [eax]
        ##      fmul    st, st(2)
        ##      fadd    st, st(1)
        ##      fsubrp  st(1), st
        ##      fmul    QWORD PTR [ecx]
        ##      fsubp   st(1), st
        ##      fistp   DWORD PTR [edx]
        ##  }

      ##
      ##  Two modular multiplications in parallel:
      ##       *a0 = (*a0 * b0) % dmod
      ##       *a1 = (*a1 * b1) % dmod
      ##
      proc pproMulmod2*(a0: ptr MpdUintT; b0: MpdUintT; a1: ptr MpdUintT; b1: MpdUintT;
                       dmod: ptr cdouble; dinvmod: ptr uint32T) {.inline, cdecl.} =
        ##  C2NIM
        ##  __asm {
        ##      mov     ecx, dmod
        ##      mov     edx, a1
        ##      mov     ebx, dinvmod
        ##      mov     eax, a0
        ##      fild    b1
        ##      fild    DWORD PTR [edx]
        ##      fmulp   st(1), st
        ##      fild    b0
        ##      fild    DWORD PTR [eax]
        ##      fmulp   st(1), st
        ##      fld     TBYTE PTR [ebx]
        ##      fld     st(2)
        ##      fmul    st, st(1)
        ##      fxch    st(1)
        ##      fmul    st, st(2)
        ##      fld     DWORD PTR MPD_TWO63
        ##      fld     QWORD PTR [ecx]
        ##      fxch    st(3)
        ##      fadd    st, st(1)
        ##      fxch    st(2)
        ##      fadd    st, st(1)
        ##      fxch    st(2)
        ##      fsub    st, st(1)
        ##      fxch    st(2)
        ##      fsubrp  st(1), st
        ##      fxch    st(1)
        ##      fmul    st, st(2)
        ##      fxch    st(1)
        ##      fmulp   st(2), st
        ##      fsubp   st(3), st
        ##      fsubp   st(1), st
        ##      fxch    st(1)
        ##      fistp   DWORD PTR [edx]
        ##      fistp   DWORD PTR [eax]
        ##  }

    ##  Return (base ** exp) % dmod
    proc pproPowmod*(base: MpdUintT; exp: MpdUintT; dmod: ptr cdouble;
                    dinvmod: ptr uint32T): MpdUintT {.inline.} =
      var r: MpdUintT = 1
      while exp > 0:
        if exp and 1:
          r = pproMulmod(r, base, dmod, dinvmod)
        base = pproMulmod(base, base, dmod, dinvmod)
        exp = exp shr 1
      return r
