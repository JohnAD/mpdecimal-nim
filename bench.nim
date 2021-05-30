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

##  Nonsense version of escape-time algorithm for calculating a mandelbrot
##  set. Just for benchmarking.

proc color_point*(x0: ptr mpd_t; y0: ptr mpd_t; maxiter: clong; ctx: ptr mpd_context_t) =
  var
    x: ptr mpd_t
    y: ptr mpd_t
    sq_x: ptr mpd_t
    sq_y: ptr mpd_t
  var two: ptr mpd_t
  x = new_mpd()
  y = new_mpd()
  mpd_set_u32(x, 0, ctx)
  mpd_set_u32(y, 0, ctx)
  sq_x = new_mpd()
  sq_y = new_mpd()
  mpd_set_u32(sq_x, 0, ctx)
  mpd_set_u32(sq_y, 0, ctx)
  two = new_mpd()
  mpd_set_u32(two, 2, ctx)
  var i: clong = 0
  while i < maxiter:
    mpd_mul(y, x, y, ctx)
    mpd_mul(y, y, two, ctx)
    mpd_add(y, y, y0, ctx)
    mpd_sub(x, sq_x, sq_y, ctx)
    mpd_add(x, x, x0, ctx)
    mpd_mul(sq_x, x, x, ctx)
    mpd_mul(sq_y, y, y, ctx)
    inc(i)
  mpd_copy(x0, x, ctx)
  mpd_del(two)
  mpd_del(sq_y)
  mpd_del(sq_x)
  mpd_del(y)
  mpd_del(x)

proc main*(argc: cint; argv: cstringArray): cint =
  var ctx: mpd_context_t
  var
    x0: ptr mpd_t
    y0: ptr mpd_t
  var prec: uint32_t = 19
  var iter: clong = 10000000
  var
    start_clock: clock_t
    end_clock: clock_t
  if argc != 3:
    err_exit("usage: bench prec iter\n")
  prec = strtoul(argv[1], nil, 10)
  iter = strtol(argv[2], nil, 10)
  mpd_init(addr(ctx), prec)
  ##  no more MPD_MINALLOC changes after here
  x0 = new_mpd()
  y0 = new_mpd()
  mpd_set_string(x0, "0.222", addr(ctx))
  mpd_set_string(y0, "0.333", addr(ctx))
  if ctx.status and MPD_Errors:
    mpd_del(y0)
    mpd_del(x0)
    err_exit("unexpected error during conversion")
  start_clock = clock()
  color_point(x0, y0, iter, addr(ctx))
  end_clock = clock()
  mpd_print(x0)
  fprintf(stderr, "time: %f\n\n",
          (double)(end_clock - start_clock) div cast[cdouble](CLOCKS_PER_SEC))
  mpd_del(y0)
  mpd_del(x0)
  return 0
