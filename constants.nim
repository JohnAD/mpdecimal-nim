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
  mpdecimal, basearith

when defined(CONFIG_64):
  ##  number-theory.c
  var mpd_moduli*: array[3, mpd_uint_t] = [] ##   c2nim TODO  18446744069414584321ULL, 18446744056529682433ULL, 18446742974197923841ULL
  var mpd_roots*: array[3, mpd_uint_t] = [7, 10, 19]
  ##  crt.c
  ##  c2nim TODO
  ##   const mpd_uint_t INV_P1_MOD_P2   = 18446744055098026669ULL;
  ##   const mpd_uint_t INV_P1P2_MOD_P3 = 287064143708160ULL;
  ##   const mpd_uint_t LH_P1P2 = 18446744052234715137ULL;
  ##   const mpd_uint_t UH_P1P2 = 18446744052234715141ULL;
  ##
  ##  transpose.c
  ##  c2nim TODO
  ##   const mpd_size_t mpd_bits[64] = {
  ##     1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024,  2048, 4096, 8192, 16384,
  ##     32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608,
  ##     16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824,
  ##     2147483648ULL, 4294967296ULL, 8589934592ULL, 17179869184ULL, 34359738368ULL,
  ##     68719476736ULL, 137438953472ULL, 274877906944ULL, 549755813888ULL,
  ##     1099511627776ULL, 2199023255552ULL, 4398046511104, 8796093022208ULL,
  ##     17592186044416ULL, 35184372088832ULL, 70368744177664ULL, 140737488355328ULL,
  ##     281474976710656ULL, 562949953421312ULL, 1125899906842624ULL,
  ##     2251799813685248ULL, 4503599627370496ULL, 9007199254740992ULL,
  ##     18014398509481984ULL, 36028797018963968ULL, 72057594037927936ULL,
  ##     144115188075855872ULL, 288230376151711744ULL, 576460752303423488ULL,
  ##     1152921504606846976ULL, 2305843009213693952ULL, 4611686018427387904ULL,
  ##     9223372036854775808ULL
  ##   };
  ##
  ##  mpdecimal.c
  ##  c2nim TODO
  ##   const mpd_uint_t mpd_pow10[MPD_RDIGITS+1] = {
  ##     1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000,
  ##     10000000000ULL,100000000000ULL,1000000000000ULL,10000000000000ULL,
  ##     100000000000000ULL,1000000000000000ULL,10000000000000000ULL,
  ##     100000000000000000ULL,1000000000000000000ULL,10000000000000000000ULL
  ##   };
  ##
  ##  magic number for constant division by MPD_RADIX
  ##  c2nim TODO
  ##   const mpd_uint_t mprime_rdx = 15581492618384294730ULL;
  ##
elif defined(CONFIG_32):
  ##  number-theory.c
  var mpd_moduli*: array[3, mpd_uint_t] = [2113929217, 2013265921, 1811939329]
  var mpd_roots*: array[3, mpd_uint_t] = [5, 31, 13]
  ##  PentiumPro modular multiplication: These constants have to be loaded as
  ##  80 bit long doubles, which are not supported by certain compilers.
  var mpd_invmoduli*: array[3, array[3, uint32_t]] = [
      [4293885170'i64, 2181570688'i64, 16352], [1698898177, 2290649223'i64, 16352], [
      2716021846'i64, 2545165803'i64, 16352]] ##  ((long double) 1 / 1811939329UL)
  var MPD_TWO63*: cfloat = 9.223372036854776e+18
  ##  2^63
  ##  crt.c
  var INV_P1_MOD_P2*: mpd_uint_t = 2013265901
  var INV_P1P2_MOD_P3*: mpd_uint_t = 54
  var LH_P1P2*: mpd_uint_t = 4127195137'i64
  ##  (P1*P2) % 2^32
  var UH_P1P2*: mpd_uint_t = 990904320
  ##  (P1*P2) / 2^32
  ##  transpose.c
  var mpd_bits*: array[32, mpd_size_t] = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048,
                                     4096, 8192, 16384, 32768, 65536, 131072, 262144,
                                     524288, 1048576, 2097152, 4194304, 8388608,
                                     16777216, 33554432, 67108864, 134217728,
                                     268435456, 536870912, 1073741824,
                                     2147483648'i64]
  ##  mpdecimal.c
  var mpd_pow10*: array[MPD_RDIGITS + 1, mpd_uint_t] = [1, 10, 100, 1000, 10000, 100000,
      1000000, 10000000, 100000000, 1000000000]

var mpd_round_string*: array[MPD_ROUND_GUARD, cstring] = ["ROUND_UP", "ROUND_DOWN",
    "ROUND_CEILING", "ROUND_FLOOR", "ROUND_HALF_UP", "ROUND_HALF_DOWN",
    "ROUND_HALF_EVEN", "ROUND_05UP", "ROUND_TRUNC"]

var mpd_clamp_string*: array[MPD_CLAMP_GUARD, cstring] = ["CLAMP_DEFAULT",
    "CLAMP_IEEE_754"]
