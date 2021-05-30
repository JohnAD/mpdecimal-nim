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
  mpdecimal, bits, constants, transpose, typearith

const
  BUFSIZE* = 4096
  SIDE* = 128

##  Bignum: The transpose functions are used for very large transforms
##    in sixstep.c and fourstep.c.
##  Definition of the matrix transpose

proc std_trans*(dest: ptr mpd_uint_t; src: ptr mpd_uint_t; rows: mpd_size_t;
               cols: mpd_size_t) =
  var
    idest: mpd_size_t
    isrc: mpd_size_t
  var
    r: mpd_size_t
    c: mpd_size_t
  r = 0
  while r < rows:
    isrc = r * cols
    idest = r
    c = 0
    while c < cols:
      dest[idest] = src[isrc]
      inc(isrc, 1)
      inc(idest, rows)
      inc(c)
    inc(r)

##
##  Swap half-rows of 2^n * (2*2^n) matrix.
##  FORWARD_CYCLE: even/odd permutation of the halfrows.
##  BACKWARD_CYCLE: reverse the even/odd permutation.
##

proc swap_halfrows_pow2*(matrix: ptr mpd_uint_t; rows: mpd_size_t; cols: mpd_size_t;
                        dir: cint): cint =
  var buf1: array[BUFSIZE, mpd_uint_t]
  var buf2: array[BUFSIZE, mpd_uint_t]
  var
    readbuf: ptr mpd_uint_t
    writebuf: ptr mpd_uint_t
    hp: ptr mpd_uint_t
  var
    done: ptr mpd_size_t
    dbits: mpd_size_t
  var
    b: mpd_size_t = BUFSIZE
    stride: mpd_size_t
  var
    hn: mpd_size_t
    hmax: mpd_size_t
  ##  halfrow number
  var
    m: mpd_size_t
    r: mpd_size_t = 0
  var offset: mpd_size_t
  var next: mpd_size_t
  assert(cols == mul_size_t(2, rows))
  if dir == FORWARD_CYCLE:
    r = rows
  elif dir == BACKWARD_CYCLE:
    r = 2
  else:
    abort()
    ##  GCOV_NOT_REACHED
  m = cols - 1
  hmax = rows
  ##  cycles start at odd halfrows
  dbits = 8 * sizeof(done[])
  if (done = mpd_calloc(hmax div (sizeof(done[])) + 1, sizeof(done[]))) == nil:
    return 0
  hn = 1
  while hn <= hmax:
    if done[hn div dbits] and mpd_bits[hn mod dbits]:
      continue
    readbuf = buf1
    writebuf = buf2
    offset = 0
    while offset < cols div 2:
      stride = if (offset + b < cols div 2): b else: cols div 2 - offset
      hp = matrix + hn * cols div 2
      memcpy(readbuf, hp + offset, stride * (sizeof(readbuf[])))
      pointerswap(addr(readbuf), addr(writebuf))
      next = mulmod_size_t(hn, r, m)
      hp = matrix + next * cols div 2
      while next != hn:
        memcpy(readbuf, hp + offset, stride * (sizeof(readbuf[])))
        memcpy(hp + offset, writebuf, stride * (sizeof(writebuf[])))
        pointerswap(addr(readbuf), addr(writebuf))
        done[next div dbits] = done[next div dbits] or mpd_bits[next mod dbits]
        next = mulmod_size_t(next, r, m)
        hp = matrix + next * cols div 2
      memcpy(hp + offset, writebuf, stride * (sizeof(writebuf[])))
      done[hn div dbits] = done[hn div dbits] or mpd_bits[hn mod dbits]
      inc(offset, b)
    inc(hn, 2)
  mpd_free(done)
  return 1

##  In-place transpose of a square matrix

proc squaretrans*(buf: ptr mpd_uint_t; cols: mpd_size_t) {.inline.} =
  var tmp: mpd_uint_t
  var
    idest: mpd_size_t
    isrc: mpd_size_t
  var
    r: mpd_size_t
    c: mpd_size_t
  r = 0
  while r < cols:
    c = r + 1
    isrc = r * cols + c
    idest = c * cols + r
    c = r + 1
    while c < cols:
      tmp = buf[isrc]
      buf[isrc] = buf[idest]
      buf[idest] = tmp
      inc(isrc, 1)
      inc(idest, cols)
      inc(c)
    inc(r)

##
##  Transpose 2^n * 2^n matrix. For cache efficiency, the matrix is split into
##  square blocks with side length 'SIDE'. First, the blocks are transposed,
##  then a square transposition is done on each individual block.
##

proc squaretrans_pow2*(matrix: ptr mpd_uint_t; size: mpd_size_t) =
  var buf1: array[SIDE * SIDE, mpd_uint_t]
  var buf2: array[SIDE * SIDE, mpd_uint_t]
  var
    to: ptr mpd_uint_t
    `from`: ptr mpd_uint_t
  var b: mpd_size_t = size
  var
    r: mpd_size_t
    c: mpd_size_t
  var i: mpd_size_t
  while b > SIDE:
    b = b shr 1
  r = 0
  while r < size:
    c = r
    while c < size:
      `from` = matrix + r * size + c
      to = buf1
      i = 0
      while i < b:
        memcpy(to, `from`, b * (sizeof(to[])))
        inc(`from`, size)
        inc(to, b)
        inc(i)
      squaretrans(buf1, b)
      if r == c:
        to = matrix + r * size + c
        `from` = buf1
        i = 0
        while i < b:
          memcpy(to, `from`, b * (sizeof(to[])))
          inc(`from`, b)
          inc(to, size)
          inc(i)
        continue
      else:
        `from` = matrix + c * size + r
        to = buf2
        i = 0
        while i < b:
          memcpy(to, `from`, b * (sizeof(to[])))
          inc(`from`, size)
          inc(to, b)
          inc(i)
        squaretrans(buf2, b)
        to = matrix + c * size + r
        `from` = buf1
        i = 0
        while i < b:
          memcpy(to, `from`, b * (sizeof(to[])))
          inc(`from`, b)
          inc(to, size)
          inc(i)
        to = matrix + r * size + c
        `from` = buf2
        i = 0
        while i < b:
          memcpy(to, `from`, b * (sizeof(to[])))
          inc(`from`, b)
          inc(to, size)
          inc(i)
      inc(c, b)
    inc(r, b)

##
##  In-place transposition of a 2^n x 2^n or a 2^n x (2*2^n)
##  or a (2*2^n) x 2^n matrix.
##

proc transpose_pow2*(matrix: ptr mpd_uint_t; rows: mpd_size_t; cols: mpd_size_t): cint =
  var size: mpd_size_t = mul_size_t(rows, cols)
  assert(ispower2(rows))
  assert(ispower2(cols))
  if cols == rows:
    squaretrans_pow2(matrix, rows)
  elif cols == mul_size_t(2, rows):
    if not swap_halfrows_pow2(matrix, rows, cols, FORWARD_CYCLE):
      return 0
    squaretrans_pow2(matrix, rows)
    squaretrans_pow2(matrix + (size div 2), rows)
  elif rows == mul_size_t(2, cols):
    squaretrans_pow2(matrix, cols)
    squaretrans_pow2(matrix + (size div 2), cols)
    if not swap_halfrows_pow2(matrix, cols, rows, BACKWARD_CYCLE):
      return 0
  else:
    abort()
    ##  GCOV_NOT_REACHED
  return 1
