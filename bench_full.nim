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

proc err_exit*(msg: cstring) =
  fprintf(stderr, "%s\n", msg)
  exit(1)

proc new_mpd*(): ptr mpd_t =
  var x: ptr mpd_t = mpd_qnew()
  if x == nil:
    err_exit("out of memory")
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

proc color_point*(x0: ptr mpd_t; y0: ptr mpd_t; maxiter: clong; ctx: ptr mpd_context_t): cint =
  var
    x: ptr mpd_t
    y: ptr mpd_t
    sq_x: ptr mpd_t
    sq_y: ptr mpd_t
  var
    two: ptr mpd_t
    four: ptr mpd_t
    c: ptr mpd_t
  var i: clong
  x = new_mpd()
  y = new_mpd()
  mpd_set_u32(x, 0, ctx)
  mpd_set_u32(y, 0, ctx)
  sq_x = new_mpd()
  sq_y = new_mpd()
  mpd_set_u32(sq_x, 0, ctx)
  mpd_set_u32(sq_y, 0, ctx)
  two = new_mpd()
  four = new_mpd()
  mpd_set_u32(two, 2, ctx)
  mpd_set_u32(four, 4, ctx)
  c = new_mpd()
  mpd_set_u32(c, 0, ctx)
  i = 0
  while i < maxiter and mpd_cmp(c, four, ctx) <= 0:
    mpd_mul(y, x, y, ctx)
    mpd_mul(y, y, two, ctx)
    mpd_add(y, y, y0, ctx)
    mpd_sub(x, sq_x, sq_y, ctx)
    mpd_add(x, x, x0, ctx)
    mpd_mul(sq_x, x, x, ctx)
    mpd_mul(sq_y, y, y, ctx)
    mpd_add(c, sq_x, sq_y, ctx)
    inc(i)
  mpd_del(c)
  mpd_del(four)
  mpd_del(two)
  mpd_del(sq_y)
  mpd_del(sq_x)
  mpd_del(y)
  mpd_del(x)
  return i

proc main*(argc: cint; argv: cstringArray): cint =
  var ctx: mpd_context_t
  var
    x0: ptr mpd_t
    y0: ptr mpd_t
  var
    sqrt_2: ptr mpd_t
    xstep: ptr mpd_t
    ystep: ptr mpd_t
  var prec: mpd_ssize_t = 19
  var iter: clong = 1000
  var points: array[40, array[80, cint]]
  var
    i: cint
    j: cint
  var
    start_clock: clock_t
    end_clock: clock_t
  if argc != 3:
    fprintf(stderr, "usage: ./bench prec iter\n")
    exit(1)
  prec = strtoll(argv[1], nil, 10)
  iter = strtol(argv[2], nil, 10)
  mpd_init(addr(ctx), prec)
  ##  no more MPD_MINALLOC changes after here
  sqrt_2 = new_mpd()
  xstep = new_mpd()
  ystep = new_mpd()
  x0 = new_mpd()
  y0 = new_mpd()
  mpd_set_u32(sqrt_2, 2, addr(ctx))
  mpd_sqrt(sqrt_2, sqrt_2, addr(ctx))
  mpd_div_u32(xstep, sqrt_2, 40, addr(ctx))
  mpd_div_u32(ystep, sqrt_2, 20, addr(ctx))
  start_clock = clock()
  mpd_copy(y0, sqrt_2, addr(ctx))
  i = 0
  while i < 40:
    mpd_copy(x0, sqrt_2, addr(ctx))
    mpd_set_negative(x0)
    j = 0
    while j < 80:
      points[i][j] = color_point(x0, y0, iter, addr(ctx))
      mpd_add(x0, x0, xstep, addr(ctx))
      inc(j)
    mpd_sub(y0, y0, ystep, addr(ctx))
    inc(i)
  end_clock = clock()
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
         (double)(end_clock - start_clock) div cast[cdouble](CLOCKS_PER_SEC))
  mpd_del(y0)
  mpd_del(x0)
  mpd_del(ystep)
  mpd_del(xstep)
  mpd_del(sqrt_2)
  return 0
