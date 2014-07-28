// C implementation of 128-bit Xorshift RNG, taken with minor modifications
// from capnproto-C++ benchmark sources (MIT license follows).  OCaml generates
// kind of shitty code for this RNG, so it's faster to call out to a C extension.

// Copyright (c) 2013-2014 Sandstorm Development Group, Inc. and contributors
// Licensed under the MIT License:
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.


#define __STDC_LIMIT_MACROS
#include "stdint.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"

// This code is designed for use on a 64-bit platform, where a uint32_t
// always fits inside an OCaml int.
static char requires_sixtyfour_bit_platform[sizeof(long) == 8 ? 1 : -1];

// Use a 128-bit Xorshift algorithm.
static inline uint32_t nextFastRand() {
  // These values are arbitrary. Any seed other than all zeroes is OK.
  static uint32_t x = 0x1d2acd47;
  static uint32_t y = 0x58ca3e14;
  static uint32_t z = 0xf563f232;
  static uint32_t w = 0x0bc76199;

  uint32_t tmp = x ^ (x << 11);
  x = y;
  y = z;
  z = w;
  w = w ^ (w >> 19) ^ tmp ^ (tmp >> 8);
  return w;
}


// GC can never be triggered in this function, so we'll avoid
// the usual CAMLParam/CAMLLocal/CAMLReturn business.
value capnp_bench_nextFastRand(value unit) {
  uint32_t result32 = nextFastRand();
  return Val_long(result32);
}


// GC can never be triggered in this function, so we'll avoid
// the usual CAMLParam/CAMLLocal/CAMLReturn business.
value capnp_bench_fastRand(value range) {
  uint32_t range32  = Long_val(range);
  uint32_t result32 = nextFastRand() % range32;
  return Val_long(result32);
}


double capnp_bench_unboxed_fastRandDouble(double range)
{
  return nextFastRand() * range / UINT32_MAX;
}


value capnp_bench_fastRandDouble(value range) {
  CAMLparam1(range);
  CAMLlocal1(result);
  double range_dbl = Double_val(range);
  double result_dbl = capnp_bench_unboxed_fastRandDouble(range_dbl);
  result = caml_copy_double(result_dbl);
  CAMLreturn(result);
}


