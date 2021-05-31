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

##  Check if n is a power of 2.

proc ispower2*(n: MpdSizeT): cint {.inline.} =
  return n != 0 and (n and (n - 1)) == 0

when defined(ansi):
  ##
  ##  Return the most significant bit position of n from 0 to 31 (63).
  ##  Assumptions: n != 0.
  ##
  proc mpdBsr*(n: MpdSizeT): cint {.inline.} =
    var pos: cint = 0
    var tmp: MpdSizeT
    when defined(CONFIG_64):
      tmp = n shr 32
      if tmp != 0:
        n = tmp
        inc(pos, 32)
    tmp = n shr 16
    if tmp != 0:
      n = tmp
      inc(pos, 16)
    tmp = n shr 8
    if tmp != 0:
      n = tmp
      inc(pos, 8)
    tmp = n shr 4
    if tmp != 0:
      n = tmp
      inc(pos, 4)
    tmp = n shr 2
    if tmp != 0:
      n = tmp
      inc(pos, 2)
    tmp = n shr 1
    if tmp != 0:
      n = tmp
      inc(pos, 1)
    return pos + cast[cint](n) - 1

  ##
  ##  Return the least significant bit position of n from 0 to 31 (63).
  ##  Assumptions: n != 0.
  ##
  proc mpdBsf*(n: MpdSizeT): cint {.inline.} =
    var pos: cint
    ##  C2NIM
    ##  #ifdef CONFIG_64
    ##      pos = 63;
    ##      if (n & 0x00000000FFFFFFFFULL) { pos -= 32; } else { n >>= 32; }
    ##      if (n & 0x000000000000FFFFULL) { pos -= 16; } else { n >>= 16; }
    ##      if (n & 0x00000000000000FFULL) { pos -=  8; } else { n >>=  8; }
    ##      if (n & 0x000000000000000FULL) { pos -=  4; } else { n >>=  4; }
    ##      if (n & 0x0000000000000003ULL) { pos -=  2; } else { n >>=  2; }
    ##      if (n & 0x0000000000000001ULL) { pos -=  1; }
    ##  #else
    pos = 31
    if n and 0x0000000000000000'i64:
      dec(pos, 16)
    else:
      n = n shr 16
    if n and 0x0000000000000000'i64:
      dec(pos, 8)
    else:
      n = n shr 8
    if n and 0x0000000000000000'i64:
      dec(pos, 4)
    else:
      n = n shr 4
    if n and 0x0000000000000000'i64:
      dec(pos, 2)
    else:
      n = n shr 2
    if n and 0x0000000000000000'i64:
      dec(pos, 1)
    return pos

  ##  END ANSI
elif defined(`asm`):
  ##
  ##  Bit scan reverse. Assumptions: a != 0.
  ##
  proc mpdBsr*(a: MpdSizeT): cint {.inline.} =
    var retval: MpdSizeT
    ##  C2NIM
    ##      __asm__ (
    ##  #ifdef CONFIG_64
    ##          "bsrq %1, %0\n\t"
    ##  #else
    ##          "bsr %1, %0\n\t"
    ##  #endif
    ##          :"=r" (retval)
    ##          :"r" (a)
    ##          :"cc"
    ##      );
    return cast[cint](retval)

  ##
  ##  Bit scan forward. Assumptions: a != 0.
  ##
  proc mpdBsf*(a: MpdSizeT): cint {.inline.} =
    var retval: MpdSizeT
    ##  C2NIM
    ##      __asm__ (
    ##  #ifdef CONFIG_64
    ##          "bsfq %1, %0\n\t"
    ##  #else
    ##          "bsf %1, %0\n\t"
    ##  #endif
    ##          :"=r" (retval)
    ##          :"r" (a)
    ##          :"cc"
    ##      );
    return cast[cint](retval)

  ##  END ASM
elif defined(masm):
  ##
  ##  Bit scan reverse. Assumptions: a != 0.
  ##
  proc mpdBsr*(a: MpdSizeT): cint {.inline, cdecl.} =
    var retval: culong
    when defined(CONFIG_64):
      bitScanReverse64(addr(retval), a)
    else:
      bitScanReverse(addr(retval), a)
    return cast[cint](retval)

  ##
  ##  Bit scan forward. Assumptions: a != 0.
  ##
  proc mpdBsf*(a: MpdSizeT): cint {.inline, cdecl.} =
    var retval: culong
    when defined(CONFIG_64):
      bitScanForward64(addr(retval), a)
    else:
      bitScanForward(addr(retval), a)
    return cast[cint](retval)

  ##  END MASM (_MSC_VER)
else: