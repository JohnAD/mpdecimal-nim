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

template mpd_Pragma*(x: untyped): void =
  nil

const
  MPD_HIDE_SYMBOLS_START* = true
  MPD_HIDE_SYMBOLS_END* = true

##  C2NIM
##  #define /* EXTINLINE */ extern inline
##  #define /* IMPORTEXPORT */
##  #if defined (BUILD_LIBMPDEC)
##    #undef /* IMPORTEXPORT */
##    #define /* IMPORTEXPORT */ __declspec(dllexport)
##  #elif defined(_DLL)
##    #undef /* IMPORTEXPORT */
##    #define /* IMPORTEXPORT */ __declspec(dllimport)
##  #endif
## ****************************************************************************
##                                   Version
## ****************************************************************************

const
  MPD_MAJOR_VERSION* = 2
  MPD_MINOR_VERSION* = 5
  MPD_MICRO_VERSION* = 1
  MPD_VERSION* = "2.5.1"
  MPD_VERSION_HEX* = ((mpd_Major_Version shl 24) or (mpd_Minor_Version shl 16) or
      (mpd_Micro_Version shl 8))

##  IMPORTEXPORT

proc mpdVersion*(): cstring
## ****************************************************************************
##                       Types for 64 bit architectures
## ****************************************************************************
##  ABI: 64-bit

const
  MPD_CONFIG_64* = 1

when defined(MPD_CONFIG_32):
when defined(CONFIG_32):
##  types for modular and base arithmetic

const
  MPD_UINT_MAX* = uint64Max
  MPD_BITS_PER_UINT* = 64

type
  MpdUintT* = uint64T

##  unsigned mod type

const
  MPD_SIZE_MAX* = size_Max

type
  MpdSizeT* = csize

##  unsigned size type
##  type for exp, digits, len, prec

const
  MPD_SSIZE_MAX* = int64Max
  MPD_SSIZE_MIN* = int64Min

type
  MpdSsizeT* = int64T

const
  mpdStrtossize* = strtoll

##  decimal arithmetic
##  #define MPD_RADIX 10000000000000000000ULL  /* 10**19 */

const
  MPD_RDIGITS* = 19
  MPD_MAX_POW10* = 19
  MPD_EXPDIGITS* = 19
  MPD_MAXTRANSFORM_2N* = 4294967296'i64

##  #define MPD_MAX_PREC 999999999999999999LL

const
  MPD_MAX_PREC_LOG2* = 64

##  #define MPD_ELIMIT  1000000000000000000LL
##  #define MPD_MAX_EMAX   999999999999999999LL    /* ELIMIT-1 */
##  #define MPD_MIN_EMIN  (-999999999999999999LL)  /* -EMAX */

const
  MPD_MIN_ETINY* = (mpd_Min_Emin - (mpd_Max_Prec - 1))

##  #define MPD_EXP_INF 2000000000000000001LL
##  #define MPD_EXP_CLAMP (-4000000000000000001LL)

const
  MPD_MAXIMPORT* = 105263157894736842'i64
  MPD_IEEE_CONTEXT_MAX_BITS* = 512

##  conversion specifiers

const
  PRI_mpdUintT* = pRIu64
  PRI_mpdSsizeT* = pRIi64

when mpd_Size_Max != mpd_Uint_Max:
## ****************************************************************************
##                                 Context
## ****************************************************************************

const
  MPD_ROUND_UP* = 0             ##  round away from 0
  MPD_ROUND_DOWN* = 1           ##  round toward 0 (truncate)
  MPD_ROUND_CEILING* = 2        ##  round toward +infinity
  MPD_ROUND_FLOOR* = 3          ##  round toward -infinity
  MPD_ROUND_HALF_UP* = 4        ##  0.5 is rounded up
  MPD_ROUND_HALF_DOWN* = 5      ##  0.5 is rounded down
  MPD_ROUND_HALF_EVEN* = 6      ##  0.5 is rounded to even
  MPD_ROUND_05UP* = 7           ##  round zero or five away from 0
  MPD_ROUND_TRUNC* = 8          ##  truncate, but set infinity
  MPD_ROUND_GUARD* = 9

const
  MPD_CLAMP_DEFAULT* = 0
  MPD_CLAMP_IEEE_754* = 1
  MPD_CLAMP_GUARD* = 2

##  IMPORTEXPORT

var mpdRoundString*: array[mpd_Round_Guard, cstring]

##  IMPORTEXPORT

var mpdClampString*: array[mpd_Clamp_Guard, cstring]

type
  MpdContextT* {.bycopy.} = object
    prec*: MpdSsizeT           ##  precision
    emax*: MpdSsizeT           ##  max positive exp
    emin*: MpdSsizeT           ##  min negative exp
    traps*: uint32T            ##  status events that should be trapped
    status*: uint32T           ##  status flags
    newtrap*: uint32T          ##  set by mpd_addstatus_raise()
    round*: cint               ##  rounding mode
    clamp*: cint               ##  clamp mode
    allcr*: cint               ##  all functions correctly rounded


##  Status flags

const
  MPD_Clamped* = 0x00000001
  MPD_ConversionSyntax* = 0x00000002
  MPD_DivisionByZero* = 0x00000004
  MPD_DivisionImpossible* = 0x00000008
  MPD_DivisionUndefined* = 0x00000010
  MPD_FpuError* = 0x00000020
  MPD_Inexact* = 0x00000040
  MPD_InvalidContext* = 0x00000080
  MPD_InvalidOperation* = 0x00000100
  MPD_MallocError* = 0x00000200
  MPD_NotImplemented* = 0x00000400
  MPD_Overflow* = 0x00000800
  MPD_Rounded* = 0x00001000
  MPD_Subnormal* = 0x00002000
  MPD_Underflow* = 0x00004000
  MPD_MaxStatus* = (0x00008000 - 1)

##  Conditions that result in an IEEE 754 exception

const
  MPD_IEEE_InvalidOperation* = (mPD_ConversionSyntax or mPD_DivisionImpossible or
      mPD_DivisionUndefined or mPD_FpuError or mPD_InvalidContext or
      mPD_InvalidOperation or mPD_MallocError)

##  Errors that require the result of an operation to be set to NaN

const
  MPD_Errors* = (mPD_IEEE_InvalidOperation or mPD_DivisionByZero)

##  Default traps

const
  MPD_Traps* = (mPD_IEEE_InvalidOperation or mPD_DivisionByZero or mPD_Overflow or
      mPD_Underflow)

##  Official name

const
  MPD_InsufficientStorage* = mPD_MallocError

##  IEEE 754 interchange format contexts

const
  MPD_DECIMAL32* = 32
  MPD_DECIMAL64* = 64
  MPD_DECIMAL128* = 128
  MPD_MINALLOC_MIN* = 2
  MPD_MINALLOC_MAX* = 64

##  IMPORTEXPORT

var MPD_MINALLOC*: MpdSsizeT

##  IMPORTEXPORT

var mpdTraphandler*: proc (a1: ptr MpdContextT)

##  IMPORTEXPORT

proc mpdDfltTraphandler*(a1: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSetminalloc*(n: MpdSsizeT)
##  IMPORTEXPORT

proc mpdInit*(ctx: ptr MpdContextT; prec: MpdSsizeT)
##  IMPORTEXPORT

proc mpdMaxcontext*(ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdDefaultcontext*(ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdBasiccontext*(ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdIeeeContext*(ctx: ptr MpdContextT; bits: cint): cint
##  IMPORTEXPORT

proc mpdGetprec*(ctx: ptr MpdContextT): MpdSsizeT
##  IMPORTEXPORT

proc mpdGetemax*(ctx: ptr MpdContextT): MpdSsizeT
##  IMPORTEXPORT

proc mpdGetemin*(ctx: ptr MpdContextT): MpdSsizeT
##  IMPORTEXPORT

proc mpdGetround*(ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdGettraps*(ctx: ptr MpdContextT): uint32T
##  IMPORTEXPORT

proc mpdGetstatus*(ctx: ptr MpdContextT): uint32T
##  IMPORTEXPORT

proc mpdGetclamp*(ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdGetcr*(ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdQsetprec*(ctx: ptr MpdContextT; prec: MpdSsizeT): cint
##  IMPORTEXPORT

proc mpdQsetemax*(ctx: ptr MpdContextT; emax: MpdSsizeT): cint
##  IMPORTEXPORT

proc mpdQsetemin*(ctx: ptr MpdContextT; emin: MpdSsizeT): cint
##  IMPORTEXPORT

proc mpdQsetround*(ctx: ptr MpdContextT; newround: cint): cint
##  IMPORTEXPORT

proc mpdQsettraps*(ctx: ptr MpdContextT; flags: uint32T): cint
##  IMPORTEXPORT

proc mpdQsetstatus*(ctx: ptr MpdContextT; flags: uint32T): cint
##  IMPORTEXPORT

proc mpdQsetclamp*(ctx: ptr MpdContextT; c: cint): cint
##  IMPORTEXPORT

proc mpdQsetcr*(ctx: ptr MpdContextT; c: cint): cint
##  IMPORTEXPORT

proc mpdAddstatusRaise*(ctx: ptr MpdContextT; flags: uint32T)
## ****************************************************************************
##                            Decimal Arithmetic
## ****************************************************************************
##  mpd_t flags

const
  MPD_POS* = mpd_Uint8C(0)
  MPD_NEG* = mpd_Uint8C(1)
  MPD_INF* = mpd_Uint8C(2)
  MPD_NAN* = mpd_Uint8C(4)
  MPD_SNAN* = mpd_Uint8C(8)
  MPD_SPECIAL* = (mpd_Inf or mpd_Nan or mpd_Snan)
  MPD_STATIC* = mpd_Uint8C(16)
  MPD_STATIC_DATA* = mpd_Uint8C(32)
  MPD_SHARED_DATA* = mpd_Uint8C(64)
  MPD_CONST_DATA* = mpd_Uint8C(128)
  MPD_DATAFLAGS* = (mpd_Static_Data or mpd_Shared_Data or mpd_Const_Data)

##  mpd_t

type
  MpdT* {.bycopy.} = object
    flags*: uint8T
    exp*: MpdSsizeT
    digits*: MpdSsizeT
    len*: MpdSsizeT
    alloc*: MpdSsizeT
    data*: ptr MpdUintT


## ****************************************************************************
##                                     Triple
## ****************************************************************************
##  status cases for getting a triple

type
  MpdTripleClass* = enum
    MPD_TRIPLE_NORMAL, MPD_TRIPLE_INF, MPD_TRIPLE_QNAN, MPD_TRIPLE_SNAN,
    MPD_TRIPLE_ERROR


type
  MpdUint128TripleT* {.bycopy.} = object
    tag*: MpdTripleClass
    sign*: uint8T
    hi*: uint64T
    lo*: uint64T
    exp*: int64T


##  IMPORTEXPORT

proc mpdFromUint128Triple*(result: ptr MpdT; triple: ptr MpdUint128TripleT;
                          status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdAsUint128Triple*(a: ptr MpdT): MpdUint128TripleT
## ****************************************************************************
##                        Quiet, thread-safe functions
## ****************************************************************************
##  format specification

type
  MpdSpecT* {.bycopy.} = object
    minWidth*: MpdSsizeT       ##  minimum field width
    prec*: MpdSsizeT           ##  fraction digits or significant digits
    `type`*: char              ##  conversion specifier
    align*: char               ##  alignment
    sign*: char                ##  sign printing/alignment
    fill*: array[5, char]       ##  fill character
    dot*: cstring              ##  decimal point
    sep*: cstring              ##  thousands separator
    grouping*: cstring         ##  grouping of digits


##  output to a string
##  IMPORTEXPORT

proc mpdToSci*(dec: ptr MpdT; fmt: cint): cstring
##  IMPORTEXPORT

proc mpdToEng*(dec: ptr MpdT; fmt: cint): cstring
##  IMPORTEXPORT

proc mpdToSciSize*(res: cstringArray; dec: ptr MpdT; fmt: cint): MpdSsizeT
##  IMPORTEXPORT

proc mpdToEngSize*(res: cstringArray; dec: ptr MpdT; fmt: cint): MpdSsizeT
##  IMPORTEXPORT

proc mpdValidateLconv*(spec: ptr MpdSpecT): cint
##  IMPORTEXPORT

proc mpdParseFmtStr*(spec: ptr MpdSpecT; fmt: cstring; caps: cint): cint
##  IMPORTEXPORT

proc mpdQformatSpec*(dec: ptr MpdT; spec: ptr MpdSpecT; ctx: ptr MpdContextT;
                    status: ptr uint32T): cstring
##  IMPORTEXPORT

proc mpdQformat*(dec: ptr MpdT; fmt: cstring; ctx: ptr MpdContextT; status: ptr uint32T): cstring
const
  MPD_NUM_FLAGS* = 15
  MPD_MAX_FLAG_STRING* = 208
  MPD_MAX_FLAG_LIST* = (mpd_Max_Flag_String + 18)
  MPD_MAX_SIGNAL_LIST* = 121

##  IMPORTEXPORT

proc mpdSnprintFlags*(dest: cstring; nmemb: cint; flags: uint32T): cint
##  IMPORTEXPORT

proc mpdLsnprintFlags*(dest: cstring; nmemb: cint; flags: uint32T;
                      flagString: ptr cstring): cint
##  IMPORTEXPORT

proc mpdLsnprintSignals*(dest: cstring; nmemb: cint; flags: uint32T;
                        signalString: ptr cstring): cint
##  output to a file
##  IMPORTEXPORT

proc mpdFprint*(file: ptr File; dec: ptr MpdT)
##  IMPORTEXPORT

proc mpdPrint*(dec: ptr MpdT)
##  assignment from a string
##  IMPORTEXPORT

proc mpdQsetString*(dec: ptr MpdT; s: cstring; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsetStringExact*(dec: ptr MpdT; s: cstring; status: ptr uint32T)
##  set to NaN with error flags
##  IMPORTEXPORT

proc mpdSeterror*(result: ptr MpdT; flags: uint32T; status: ptr uint32T)
##  set a special with sign and type
##  IMPORTEXPORT

proc mpdSetspecial*(result: ptr MpdT; sign: uint8T; `type`: uint8T)
##  set coefficient to zero or all nines
##  IMPORTEXPORT

proc mpdZerocoeff*(result: ptr MpdT)
##  IMPORTEXPORT

proc mpdQmaxcoeff*(result: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  quietly assign a C integer type to an mpd_t
##  IMPORTEXPORT

proc mpdQsetSsize*(result: ptr MpdT; a: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsetI32*(result: ptr MpdT; a: int32T; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsetUint*(result: ptr MpdT; a: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsetU32*(result: ptr MpdT; a: uint32T; ctx: ptr MpdContextT; status: ptr uint32T)
when not defined(MPD_LEGACY_COMPILER):
  ##  IMPORTEXPORT
  proc mpdQsetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT; status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQsetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQsetI64Exact*(result: ptr MpdT; a: int64T; status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQsetU64Exact*(result: ptr MpdT; a: uint64T; status: ptr uint32T)
##  quietly assign a C integer type to an mpd_t with a static coefficient
##  IMPORTEXPORT

proc mpdQssetSsize*(result: ptr MpdT; a: MpdSsizeT; ctx: ptr MpdContextT;
                   status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQssetI32*(result: ptr MpdT; a: int32T; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQssetUint*(result: ptr MpdT; a: MpdUintT; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQssetU32*(result: ptr MpdT; a: uint32T; ctx: ptr MpdContextT; status: ptr uint32T)
##  quietly get a C integer type from an mpd_t
##  IMPORTEXPORT

proc mpdQgetSsize*(dec: ptr MpdT; status: ptr uint32T): MpdSsizeT
##  IMPORTEXPORT

proc mpdQgetUint*(dec: ptr MpdT; status: ptr uint32T): MpdUintT
##  IMPORTEXPORT

proc mpdQabsUint*(dec: ptr MpdT; status: ptr uint32T): MpdUintT
##  IMPORTEXPORT

proc mpdQgetI32*(dec: ptr MpdT; status: ptr uint32T): int32T
##  IMPORTEXPORT

proc mpdQgetU32*(dec: ptr MpdT; status: ptr uint32T): uint32T
when not defined(MPD_LEGACY_COMPILER):
  ##  IMPORTEXPORT
  proc mpdQgetI64*(dec: ptr MpdT; status: ptr uint32T): int64T
  ##  IMPORTEXPORT
  proc mpdQgetU64*(dec: ptr MpdT; status: ptr uint32T): uint64T
##  quiet functions
##  IMPORTEXPORT

proc mpdQcheckNan*(nanresult: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQcheckNans*(nanresult: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                   status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQfinalize*(result: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdClass*(a: ptr MpdT; ctx: ptr MpdContextT): cstring
##  IMPORTEXPORT

proc mpdQcopy*(result: ptr MpdT; a: ptr MpdT; status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQcopyCxx*(result: ptr MpdT; a: ptr MpdT): cint
##  IMPORTEXPORT

proc mpdQncopy*(a: ptr MpdT): ptr MpdT
##  IMPORTEXPORT

proc mpdQcopyAbs*(result: ptr MpdT; a: ptr MpdT; status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQcopyNegate*(result: ptr MpdT; a: ptr MpdT; status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQcopySign*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQand*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQinvert*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQlogb*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQor*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
            status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQscaleb*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQxor*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdSameQuantum*(a: ptr MpdT; b: ptr MpdT): cint
##  IMPORTEXPORT

proc mpdQrotate*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQshiftl*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQshiftr*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; status: ptr uint32T): MpdUintT
##  IMPORTEXPORT

proc mpdQshiftrInplace*(result: ptr MpdT; n: MpdSsizeT): MpdUintT
##  IMPORTEXPORT

proc mpdQshift*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
               status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQshiftn*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQcmp*(a: ptr MpdT; b: ptr MpdT; status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQcompare*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                 status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdQcompareSignal*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                       status: ptr uint32T): cint
##  IMPORTEXPORT

proc mpdCmpTotal*(a: ptr MpdT; b: ptr MpdT): cint
##  IMPORTEXPORT

proc mpdCmpTotalMag*(a: ptr MpdT; b: ptr MpdT): cint
##  IMPORTEXPORT

proc mpdCompareTotal*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT): cint
##  IMPORTEXPORT

proc mpdCompareTotalMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT): cint
##  IMPORTEXPORT

proc mpdQroundToIntx*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                     status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQroundToInt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                    status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQtrunc*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQfloor*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQceil*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQabs*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQmax*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQmaxMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQmin*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQminMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQminus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQplus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQnextMinus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                   status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQnextPlus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQnextToward*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                    status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQquantize*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQrescale*(result: ptr MpdT; a: ptr MpdT; exp: MpdSsizeT; ctx: ptr MpdContextT;
                 status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQrescaleFmt*(result: ptr MpdT; a: ptr MpdT; exp: MpdSsizeT; ctx: ptr MpdContextT;
                    status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQreduce*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQadd*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQaddSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQaddI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQaddUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQaddU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsub*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsubSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsubI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsubUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsubU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQmul*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQmulSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQmulI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQmulUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQmulU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQfma*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; c: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQdiv*(q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQdivSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQdivI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQdivUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT;
                 status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQdivU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQdivint*(q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQrem*(r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQremNear*(r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                 status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQdivmod*(q: ptr MpdT; r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT;
                status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQpow*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; ctx: ptr MpdContextT;
             status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQpowmod*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; `mod`: ptr MpdT;
                ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQexp*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQln10*(result: ptr MpdT; prec: MpdSsizeT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQln*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQlog10*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQsqrt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQinvroot*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT; status: ptr uint32T)
when not defined(MPD_LEGACY_COMPILER):
  ##  IMPORTEXPORT
  proc mpdQaddI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQaddU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQsubI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQsubU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQmulI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQmulU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQdivI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
  ##  IMPORTEXPORT
  proc mpdQdivU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT;
                  status: ptr uint32T)
##  IMPORTEXPORT

proc mpdSizeinbase*(a: ptr MpdT; base: uint32T): csize
##  IMPORTEXPORT

proc mpdQimportU16*(result: ptr MpdT; srcdata: ptr uint16T; srclen: csize;
                   srcsign: uint8T; srcbase: uint32T; ctx: ptr MpdContextT;
                   status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQimportU32*(result: ptr MpdT; srcdata: ptr uint32T; srclen: csize;
                   srcsign: uint8T; srcbase: uint32T; ctx: ptr MpdContextT;
                   status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQexportU16*(rdata: ptr ptr uint16T; rlen: csize; base: uint32T; src: ptr MpdT;
                   status: ptr uint32T): csize
##  IMPORTEXPORT

proc mpdQexportU32*(rdata: ptr ptr uint32T; rlen: csize; base: uint32T; src: ptr MpdT;
                   status: ptr uint32T): csize
## ****************************************************************************
##                            Signalling functions
## ****************************************************************************
##  IMPORTEXPORT

proc mpdFormat*(dec: ptr MpdT; fmt: cstring; ctx: ptr MpdContextT): cstring
##  IMPORTEXPORT

proc mpdImportU16*(result: ptr MpdT; srcdata: ptr uint16T; srclen: csize;
                  srcsign: uint8T; base: uint32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdImportU32*(result: ptr MpdT; srcdata: ptr uint32T; srclen: csize;
                  srcsign: uint8T; base: uint32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdExportU16*(rdata: ptr ptr uint16T; rlen: csize; base: uint32T; src: ptr MpdT;
                  ctx: ptr MpdContextT): csize
##  IMPORTEXPORT

proc mpdExportU32*(rdata: ptr ptr uint32T; rlen: csize; base: uint32T; src: ptr MpdT;
                  ctx: ptr MpdContextT): csize
##  IMPORTEXPORT

proc mpdFinalize*(result: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdCheckNan*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdCheckNans*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdSetString*(result: ptr MpdT; s: cstring; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMaxcoeff*(result: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSsetSsize*(result: ptr MpdT; a: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSsetI32*(result: ptr MpdT; a: int32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSsetUint*(result: ptr MpdT; a: MpdUintT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSsetU32*(result: ptr MpdT; a: uint32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSetSsize*(result: ptr MpdT; a: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSetI32*(result: ptr MpdT; a: int32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSetUint*(result: ptr MpdT; a: MpdUintT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSetU32*(result: ptr MpdT; a: uint32T; ctx: ptr MpdContextT)
when not defined(MPD_LEGACY_COMPILER):
  ##  IMPORTEXPORT
  proc mpdSetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT)
  ##  IMPORTEXPORT
  proc mpdSetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdGetSsize*(a: ptr MpdT; ctx: ptr MpdContextT): MpdSsizeT
##  IMPORTEXPORT

proc mpdGetUint*(a: ptr MpdT; ctx: ptr MpdContextT): MpdUintT
##  IMPORTEXPORT

proc mpdAbsUint*(a: ptr MpdT; ctx: ptr MpdContextT): MpdUintT
##  IMPORTEXPORT

proc mpdGetI32*(a: ptr MpdT; ctx: ptr MpdContextT): int32T
##  IMPORTEXPORT

proc mpdGetU32*(a: ptr MpdT; ctx: ptr MpdContextT): uint32T
when not defined(MPD_LEGACY_COMPILER):
  ##  IMPORTEXPORT
  proc mpdGetI64*(a: ptr MpdT; ctx: ptr MpdContextT): int64T
  ##  IMPORTEXPORT
  proc mpdGetU64*(a: ptr MpdT; ctx: ptr MpdContextT): uint64T
##  IMPORTEXPORT

proc mpdAnd*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdCopy*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdCanonical*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdCopyAbs*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdCopyNegate*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdCopySign*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdInvert*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdLogb*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdOr*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdRotate*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdScaleb*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdShiftl*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdShiftr*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; ctx: ptr MpdContextT): MpdUintT
##  IMPORTEXPORT

proc mpdShiftn*(result: ptr MpdT; a: ptr MpdT; n: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdShift*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdXor*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdAbs*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdCmp*(a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdCompare*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdCompareSignal*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdAdd*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdAddSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdAddI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdAddUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdAddU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSub*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSubSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSubI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSubUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSubU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdDiv*(q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdDivSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdDivI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdDivUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdDivU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdDivmod*(q: ptr MpdT; r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdDivint*(q: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdExp*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdFma*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; c: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdLn*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdLog10*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMax*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMaxMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMin*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMinMag*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMinus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMul*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMulSsize*(result: ptr MpdT; a: ptr MpdT; b: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMulI32*(result: ptr MpdT; a: ptr MpdT; b: int32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMulUint*(result: ptr MpdT; a: ptr MpdT; b: MpdUintT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdMulU32*(result: ptr MpdT; a: ptr MpdT; b: uint32T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdNextMinus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdNextPlus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdNextToward*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdPlus*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdPow*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdPowmod*(result: ptr MpdT; base: ptr MpdT; exp: ptr MpdT; `mod`: ptr MpdT;
               ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdQuantize*(result: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdRescale*(result: ptr MpdT; a: ptr MpdT; exp: MpdSsizeT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdReduce*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdRem*(r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdRemNear*(r: ptr MpdT; a: ptr MpdT; b: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdRoundToIntx*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdRoundToInt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdTrunc*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdFloor*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdCeil*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSqrt*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdInvroot*(result: ptr MpdT; a: ptr MpdT; ctx: ptr MpdContextT)
when not defined(MPD_LEGACY_COMPILER):
  ##  IMPORTEXPORT
  proc mpdAddI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT)
  ##  IMPORTEXPORT
  proc mpdAddU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT)
  ##  IMPORTEXPORT
  proc mpdSubI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT)
  ##  IMPORTEXPORT
  proc mpdSubU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT)
  ##  IMPORTEXPORT
  proc mpdDivI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT)
  ##  IMPORTEXPORT
  proc mpdDivU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT)
  ##  IMPORTEXPORT
  proc mpdMulI64*(result: ptr MpdT; a: ptr MpdT; b: int64T; ctx: ptr MpdContextT)
  ##  IMPORTEXPORT
  proc mpdMulU64*(result: ptr MpdT; a: ptr MpdT; b: uint64T; ctx: ptr MpdContextT)
## ****************************************************************************
##                           Configuration specific
## ****************************************************************************
##  IMPORTEXPORT

proc mpdQssetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdQssetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT; status: ptr uint32T)
##  IMPORTEXPORT

proc mpdSsetI64*(result: ptr MpdT; a: int64T; ctx: ptr MpdContextT)
##  IMPORTEXPORT

proc mpdSsetU64*(result: ptr MpdT; a: uint64T; ctx: ptr MpdContextT)
## ****************************************************************************
##                        Get attributes of a decimal
## ****************************************************************************
##  IMPORTEXPORT
##  EXTINLINE

proc mpdAdjexp*(dec: ptr MpdT): MpdSsizeT
##  IMPORTEXPORT
##  EXTINLINE

proc mpdEtiny*(ctx: ptr MpdContextT): MpdSsizeT
##  IMPORTEXPORT
##  EXTINLINE

proc mpdEtop*(ctx: ptr MpdContextT): MpdSsizeT
##  IMPORTEXPORT
##  EXTINLINE

proc mpdMsword*(dec: ptr MpdT): MpdUintT
##  IMPORTEXPORT
##  EXTINLINE

proc mpdWordDigits*(word: MpdUintT): cint
##  most significant digit of a word
##  IMPORTEXPORT
##  EXTINLINE

proc mpdMsd*(word: MpdUintT): MpdUintT
##  least significant digit of a word
##  IMPORTEXPORT
##  EXTINLINE

proc mpdLsd*(word: MpdUintT): MpdUintT
##  coefficient size needed to store 'digits'
##  IMPORTEXPORT
##  EXTINLINE

proc mpdDigitsToSize*(digits: MpdSsizeT): MpdSsizeT
##  number of digits in the exponent, undefined for MPD_SSIZE_MIN
##  IMPORTEXPORT
##  EXTINLINE

proc mpdExpDigits*(exp: MpdSsizeT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIscanonical*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsfinite*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsinfinite*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsinteger*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsnan*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsnegative*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIspositive*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsqnan*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIssigned*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIssnan*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsspecial*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIszero*(dec: ptr MpdT): cint
##  undefined for special numbers
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIszerocoeff*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsnormal*(dec: ptr MpdT; ctx: ptr MpdContextT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIssubnormal*(dec: ptr MpdT; ctx: ptr MpdContextT): cint
##  odd word
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsoddword*(word: MpdUintT): cint
##  odd coefficient
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsoddcoeff*(dec: ptr MpdT): cint
##  odd decimal, only defined for integers
##  IMPORTEXPORT

proc mpdIsodd*(dec: ptr MpdT): cint
##  even decimal, only defined for integers
##  IMPORTEXPORT

proc mpdIseven*(dec: ptr MpdT): cint
##  0 if dec is positive, 1 if dec is negative
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSign*(dec: ptr MpdT): uint8T
##  1 if dec is positive, -1 if dec is negative
##  IMPORTEXPORT
##  EXTINLINE

proc mpdArithSign*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdRadix*(): clong
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsdynamic*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsstatic*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsdynamicData*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsstaticData*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIssharedData*(dec: ptr MpdT): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdIsconstData*(dec: ptr MpdT): cint
##  IMPORTEXPORT

proc mpdTrailZeros*(dec: ptr MpdT): MpdSsizeT
## ****************************************************************************
##                        Set attributes of a decimal
## ****************************************************************************
##  set number of decimal digits in the coefficient
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetdigits*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetSign*(result: ptr MpdT; sign: uint8T)
##  copy sign from another decimal
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSigncpy*(result: ptr MpdT; a: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetInfinity*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetQnan*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetSnan*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetNegative*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetPositive*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetDynamic*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetStatic*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetDynamicData*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetStaticData*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetSharedData*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetConstData*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdClearFlags*(result: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdSetFlags*(result: ptr MpdT; flags: uint8T)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdCopyFlags*(result: ptr MpdT; a: ptr MpdT)
## ****************************************************************************
##                               Error Macros
## ****************************************************************************
##  C2NIM
##
##  #define mpd_err_fatal(...) \
##      do {fprintf(stderr, "%s:%d: error: ", __FILE__, __LINE__); \
##          fprintf(stderr, __VA_ARGS__);  fputc('\n', stderr);    \
##          abort();                                               \
##      } while (0)
##  #define mpd_err_warn(...) \
##      do {fprintf(stderr, "%s:%d: warning: ", __FILE__, __LINE__); \
##          fprintf(stderr, __VA_ARGS__); fputc('\n', stderr);       \
##      } while (0)
## ****************************************************************************
##                             Memory handling
## ****************************************************************************
##  IMPORTEXPORT

var mpdMallocfunc*: proc (size: csize): pointer

##  IMPORTEXPORT

var mpdCallocfunc*: proc (nmemb: csize; size: csize): pointer

##  IMPORTEXPORT

var mpdReallocfunc*: proc (`ptr`: pointer; size: csize): pointer

##  IMPORTEXPORT

var mpdFree*: proc (`ptr`: pointer)

##  IMPORTEXPORT

proc mpdCallocfuncEm*(nmemb: csize; size: csize): pointer
##  IMPORTEXPORT

proc mpdAlloc*(nmemb: MpdSizeT; size: MpdSizeT): pointer
##  IMPORTEXPORT

proc mpdCalloc*(nmemb: MpdSizeT; size: MpdSizeT): pointer
##  IMPORTEXPORT

proc mpdRealloc*(`ptr`: pointer; nmemb: MpdSizeT; size: MpdSizeT; err: ptr uint8T): pointer
##  IMPORTEXPORT

proc mpdShAlloc*(structSize: MpdSizeT; nmemb: MpdSizeT; size: MpdSizeT): pointer
##  IMPORTEXPORT

proc mpdQnew*(): ptr MpdT
##  IMPORTEXPORT

proc mpdNew*(ctx: ptr MpdContextT): ptr MpdT
##  IMPORTEXPORT

proc mpdQnewSize*(nwords: MpdSsizeT): ptr MpdT
##  IMPORTEXPORT
##  EXTINLINE

proc mpdDel*(dec: ptr MpdT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdUintZero*(dest: ptr MpdUintT; len: MpdSizeT)
##  IMPORTEXPORT
##  EXTINLINE

proc mpdQresize*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdQresizeZero*(result: ptr MpdT; nwords: MpdSsizeT; status: ptr uint32T): cint
##  IMPORTEXPORT
##  EXTINLINE

proc mpdMinalloc*(result: ptr MpdT)
##  IMPORTEXPORT

proc mpdResize*(result: ptr MpdT; nwords: MpdSsizeT; ctx: ptr MpdContextT): cint
##  IMPORTEXPORT

proc mpdResizeZero*(result: ptr MpdT; nwords: MpdSsizeT; ctx: ptr MpdContextT): cint