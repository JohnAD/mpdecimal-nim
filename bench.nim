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

proc errExit*(msg: cstring) =
  fprintf(stderr, "%s\n", msg)
  exit(1)

proc newMpd*(): ptr MpdT =
  var x: ptr MpdT = mpdQnew()
  if x == nil:
    errExit("out of memory")
  return x

##  Nonsense version of escape-time algorithm for calculating a mandelbrot
##  set. Just for benchmarking.

proc colorPoint*(x0: ptr MpdT; y0: ptr MpdT; maxiter: clong; ctx: ptr MpdContextT) =
  var
    x: ptr MpdT
    y: ptr MpdT
    sqX: ptr MpdT
    sqY: ptr MpdT
  var two: ptr MpdT
  x = newMpd()
  y = newMpd()
  mpdSetU32(x, 0, ctx)
  mpdSetU32(y, 0, ctx)
  sqX = newMpd()
  sqY = newMpd()
  mpdSetU32(sqX, 0, ctx)
  mpdSetU32(sqY, 0, ctx)
  two = newMpd()
  mpdSetU32(two, 2, ctx)
  var i: clong = 0
  while i < maxiter:
    mpdMul(y, x, y, ctx)
    mpdMul(y, y, two, ctx)
    mpdAdd(y, y, y0, ctx)
    mpdSub(x, sqX, sqY, ctx)
    mpdAdd(x, x, x0, ctx)
    mpdMul(sqX, x, x, ctx)
    mpdMul(sqY, y, y, ctx)
    inc(i)
  mpdCopy(x0, x, ctx)
  mpdDel(two)
  mpdDel(sqY)
  mpdDel(sqX)
  mpdDel(y)
  mpdDel(x)

proc main*(argc: cint; argv: cstringArray): cint =
  var ctx: MpdContextT
  var
    x0: ptr MpdT
    y0: ptr MpdT
  var prec: uint32T = 19
  var iter: clong = 10000000
  var
    startClock: ClockT
    endClock: ClockT
  if argc != 3:
    errExit("usage: bench prec iter\n")
  prec = strtoul(argv[1], nil, 10)
  iter = strtol(argv[2], nil, 10)
  mpdInit(addr(ctx), prec)
  ##  no more MPD_MINALLOC changes after here
  x0 = newMpd()
  y0 = newMpd()
  mpdSetString(x0, "0.222", addr(ctx))
  mpdSetString(y0, "0.333", addr(ctx))
  if ctx.status and mPD_Errors:
    mpdDel(y0)
    mpdDel(x0)
    errExit("unexpected error during conversion")
  startClock = clock()
  colorPoint(x0, y0, iter, addr(ctx))
  endClock = clock()
  mpdPrint(x0)
  fprintf(stderr, "time: %f\n\n",
          (double)(endClock - startClock) div cast[cdouble](clocks_Per_Sec))
  mpdDel(y0)
  mpdDel(x0)
  return 0
