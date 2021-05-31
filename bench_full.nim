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

##
##  Example from: http://en.wikipedia.org/wiki/Mandelbrot_set
##
##  Escape time algorithm for drawing the set:
##
##  Point x0, y0 is deemed to be in the Mandelbrot set if the return
##  value is maxiter. Lower return values indicate how quickly points
##  escaped and can be used for coloring.
##

proc colorPoint*(x0: ptr MpdT; y0: ptr MpdT; maxiter: clong; ctx: ptr MpdContextT): cint =
  var
    x: ptr MpdT
    y: ptr MpdT
    sqX: ptr MpdT
    sqY: ptr MpdT
  var
    two: ptr MpdT
    four: ptr MpdT
    c: ptr MpdT
  var i: clong
  x = newMpd()
  y = newMpd()
  mpdSetU32(x, 0, ctx)
  mpdSetU32(y, 0, ctx)
  sqX = newMpd()
  sqY = newMpd()
  mpdSetU32(sqX, 0, ctx)
  mpdSetU32(sqY, 0, ctx)
  two = newMpd()
  four = newMpd()
  mpdSetU32(two, 2, ctx)
  mpdSetU32(four, 4, ctx)
  c = newMpd()
  mpdSetU32(c, 0, ctx)
  i = 0
  while i < maxiter and mpdCmp(c, four, ctx) <= 0:
    mpdMul(y, x, y, ctx)
    mpdMul(y, y, two, ctx)
    mpdAdd(y, y, y0, ctx)
    mpdSub(x, sqX, sqY, ctx)
    mpdAdd(x, x, x0, ctx)
    mpdMul(sqX, x, x, ctx)
    mpdMul(sqY, y, y, ctx)
    mpdAdd(c, sqX, sqY, ctx)
    inc(i)
  mpdDel(c)
  mpdDel(four)
  mpdDel(two)
  mpdDel(sqY)
  mpdDel(sqX)
  mpdDel(y)
  mpdDel(x)
  return i

proc main*(argc: cint; argv: cstringArray): cint =
  var ctx: MpdContextT
  var
    x0: ptr MpdT
    y0: ptr MpdT
  var
    sqrt2: ptr MpdT
    xstep: ptr MpdT
    ystep: ptr MpdT
  var prec: MpdSsizeT = 19
  var iter: clong = 1000
  var points: array[40, array[80, cint]]
  var
    i: cint
    j: cint
  var
    startClock: ClockT
    endClock: ClockT
  if argc != 3:
    fprintf(stderr, "usage: ./bench prec iter\n")
    exit(1)
  prec = strtoll(argv[1], nil, 10)
  iter = strtol(argv[2], nil, 10)
  mpdInit(addr(ctx), prec)
  ##  no more MPD_MINALLOC changes after here
  sqrt2 = newMpd()
  xstep = newMpd()
  ystep = newMpd()
  x0 = newMpd()
  y0 = newMpd()
  mpdSetU32(sqrt2, 2, addr(ctx))
  mpdSqrt(sqrt2, sqrt2, addr(ctx))
  mpdDivU32(xstep, sqrt2, 40, addr(ctx))
  mpdDivU32(ystep, sqrt2, 20, addr(ctx))
  startClock = clock()
  mpdCopy(y0, sqrt2, addr(ctx))
  i = 0
  while i < 40:
    mpdCopy(x0, sqrt2, addr(ctx))
    mpdSetNegative(x0)
    j = 0
    while j < 80:
      points[i][j] = colorPoint(x0, y0, iter, addr(ctx))
      mpdAdd(x0, x0, xstep, addr(ctx))
      inc(j)
    mpdSub(y0, y0, ystep, addr(ctx))
    inc(i)
  endClock = clock()
  when defined(BENCH_VERBOSE):
    i = 0
    while i < 40:
      j = 0
      while j < 80:
        if points[i][j] == iter:
          putchar('*')
        elif points[i][j] >= 10:
          putchar('+')
        elif points[i][j] >= 5:
          putchar('.')
        else:
          putchar(' ')
        inc(j)
      putchar('\n')
      inc(i)
    putchar('\n')
  else:
    cast[nil](points)
    ##  suppress gcc warning
  printf("time: %f\n\n",
         (double)(endClock - startClock) div cast[cdouble](clocks_Per_Sec))
  mpdDel(y0)
  mpdDel(x0)
  mpdDel(ystep)
  mpdDel(xstep)
  mpdDel(sqrt2)
  return 0
