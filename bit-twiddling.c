/*
 * CS:APP Data Lab
 *
 * Sammy Potter (spott14@u.rochester.edu)
 * Justin Lee (jlee363@u.rochester.edu)
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:

  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code
  must conform to the following style:

  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>

  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.


  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function.
     The max operator count is checked by dlc. Note that '=' is not
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 *
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* We do support the IEC 559 math functionality, real and complex.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/*
 * bitXor - x^y using only ~ and &
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y) {
  /* Truth table logic */
  return ~(~x & ~y) & ~(x & y);
}
/*
 * oddBits - return word with all odd-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int oddBits(void) {
  /* Copy 10101010 to all four bytes */
  int b = 0xaa;
  return b|(b<<8)|(b<<16)|(b<<24);
}
/*
 * reverseBytes - reverse the bytes of x
 *   Example: reverseBytes(0x01020304) = 0x04030201
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 3
 */
int reverseBytes(int x) {
  /* Each byte is masked and swapped symetrically */
  int z = 0xFF;
  int out = (x >> 24 & z) | ((x >> 16 & z) << 8) | ((x >> 8 & z) << 16) | ((x & z) << 24);
  return out;
}
/*
 * rotateRight - Rotate x to the right by n
 *   Can assume that 0 <= n <= 31
 *   Examples: rotateRight(0x87654321,4) = 0x18765432
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 3
 */
int rotateRight(int x, int n) {
  /* Split into two chunks, which are swapped
   * and masked together */
  int left = x << (32 + (~n + 1));
  int right = x >> n;
  int mask = ~(1 << 31);
  mask = mask >> (n + (~1 + 1));
  return (right & mask) | left;
}
/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x) {
  /* Uses a series of masks to count up 1s in
   * areas of increasing size */

  // Pairs (0101 0101)
  int a1 = 0x55 | 0x55 << 8;
  int a = a1 | a1 << 16;

  // Nibbles (0011 0011)
  int b1 = 0x33 | 0x33 << 8;
  int b = b1 | b1 << 16;

  // Bytes (0000 1111)
  int c1 = 0x0F | 0x0f << 8;
  int c = c1 | c1 << 16;

  // Word (1111 1111)
  int d = 0xFF | (0x00 << 8) | (0xFF << 16) | (0x00 << 24);
  int e = 0xFF | (0xFF << 8);

  x = (x & a) + ((x >> 1) & a);
  x = (x & b) + ((x >> 2) & b);
  x = (x & c) + ((x >> 4) & c);
  x = (x & d) + ((x >> 8) & d);
  x = (x & e) + ((x >> 16) & e);
  return x;
}
/*
 * TMax - return maximum two's complement integer
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmax(void) {
  /* Left shift 1 all the way, then invert */
  return ~(1 << 31);
}
/*
 * addOK - Determine if can compute x+y without overflow
 *   Example: addOK(0x80000000,0x80000000) = 0,
 *            addOK(0x80000000,0x70000000) = 1,
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int addOK(int x, int y) {
  /* 2s Complement overflow rule:
   *   - If x and y have the same sign, then result must
   *     also have that sign
   * t = 1 if x and y have different signs
   * z = 1 if the sum, x, and y all have the same sign. */
  int t = (x & ~y) | (~x & y);
  int res = x + y;
  int z = (res & x & y) | (~res & ~x & ~y);
  int out = t | z;
  return (out >> 31) & 1;
}
/*
 * rempwr2 - Compute x%(2^n), for 0 <= n <= 30
 *   Negative arguments should yield negative remainders
 *   Examples: rempwr2(15,2) = 3, rempwr2(-35,3) = -3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int rempwr2(int x, int n) {
  /* Mask the 32 - n MSBs in order to "chop off" n-number of
   * bits from the right side of x, then accounting for
   * 2s complement if the original number was negative. */
  int mask = ~(~0 << n);
  int div = ((!!(x & mask)) << 31) >> 31;
  int rem = (x & mask) + (((x >> 31) & div) & ((~(1 << n)) + 1));
  return rem;
}
/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0xe0000000) = 0x80000000 (saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {
  /* If we multiply a negative number by a positive number,
   * and the result is positive, we have overflow. If
   * we multiply two positive numbers and get a negative
   * result, overflow has occurred.
   *   - Right-shift by 1 to multiply, store the result
   *   - Look at the second bit from the left to check overflow */
  int sign = x >> 31;
  int msb = (x << 1) >> 31; // Store the 2nd bit from the left
  int res = x << 1; // x * 2 (may overflow)
  int n = 1 << 31;
  int out = res ^ ((sign ^ msb) & (res ^ msb ^ n));
  return out;
}
/*
 * isGreater - if x > y  then return 1, else return 0
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y) {
  /* Compares x and y by checking their signs
   *   - If the signs are the same, check
   *     if x - y is positive. (t)
   *   - If the signs of x and y are different,
   *     we return 1 if x is positive. (z) */
  int sx = x >> 31;
  int sy = y >> 31;

  int t = (~(sx ^ sy)) & ((~y + x) >> 31);
  int z = sx & ~sy;

  return !(t | z);
}
/*
 * multFiveEighths - multiplies by 5/8 rounding toward 0.
 *   Should exactly duplicate effect of C expression (x*5/8),
 *   including overflow behavior.
 *   Examples: multFiveEighths(77) = 48
 *             multFiveEighths(-22) = -13
 *             multFiveEighths(1073741824) = 13421728 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
int multFiveEighths(int x) {
  /* Multiply by 5 by left shifting and
   * adding 1*x, divide by 8 using right
   * shift.
   * 5 = 2^2 + 1
   * 8 = 2^3 */
  int f = (x << 2) + x;
  int e = (f + (7 & (f >> 31))) >> 3; // Round toward 0, even if x is negative
  return e;
}
/*
 * isNonZero - Check whether x is nonzero using
 *              the legal operators except !
 *   Examples: isNonZero(3) = 1, isNonZero(0) = 0
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4
 */
int isNonZero(int x) {
  /* All 2s complement numbers have a negative representation
   * except zero.
   *   - Store the sign, negate the number, then check
   *     if the sign has changed. */
  int s = (x >> 31) & 1;
  x = ~x + 1;
  return s | ((x >> 31) & 1);
}
/*
 * float_abs - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_abs(unsigned uf) {
  /* Mask off each part of the float.
   *   - If the sign is already positive, or if the
   *     exponent is all 1s and the mantissa
   *     is 1 (NaN), return the number as is.
   *   - Otherwise flip the sign bit */
  unsigned int sign = uf >> 31;
  unsigned int exp = uf & 0x7F800000;
  unsigned int mant = uf & 0x007fffff;
  if(!sign || (exp == 0x7F800000 && mant)) {
    return uf;
  } else {
    return uf & ~(1 << 31);
  }
}
/*
 * float_f2i - Return bit-level equivalent of expression (int) f
 *   for floating point argument f.
 *   Argument is passed as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point value.
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
int float_f2i(unsigned uf) {
  /* Isolates the float elements, checks for denorm or NaN,
   * then combines the mantissa with the implied 1 and shifts it.
   * Converts to 2s complement if negative */
  unsigned sign = uf >> 31;
  unsigned exp = (uf >> 23) & 0xFF;
  unsigned mant = uf & 0x7FFFFF;

  int out;
  int exp_biased;

  if(exp == 0) return 0; // Denormalized value
  if(exp == 0xFF) return 0x80000000u; // NaN case

  exp_biased = exp - 127;
  if(exp_biased < 0) return 0; // Number is too precise for int
  if(exp_biased > 30) return 0x80000000u; // Number is too big for int
  out = 1 << exp_biased; // MSB
  // Shift mantissa to align with the above MSB, and combine with out
  if(exp_biased < 23) {
    out = out | mant >> (23 - exp_biased);
  } else {
    out = out | mant << (exp_biased - 23);
  }
  if(sign) out = ~out + 1; // Convert to 2s complement (if negative)
  return out;
}
/*
 * float_half - Return bit-level equivalent of expression 0.5*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_half(unsigned uf) {
  /* Isolate float elements, check for NaN case and
   * 0 or 1 exponent, otherwise subtract 1 from the
   * exponent to divide by 2 */
  unsigned int exp = uf & 0x7F800000;
  unsigned int sign = uf & 0x80000000;
  unsigned int mant = uf & 0x007FFFFF;
  if(exp == 0x7F800000) return uf; // NaN case
  if((!exp) || (exp == 0x00800000)) { // exp = 0 or 1
    mant = (uf & 0x00FFFFFF) >> 1;
    mant += ((uf & 2) >> 1) & (uf & 1); // Recover the LSB lost during the last step
    return sign | mant;
  }
  return sign | ((exp - 1) & 0x7F800000) | mant; // Subtract 1 from exponent to divide by 2
}