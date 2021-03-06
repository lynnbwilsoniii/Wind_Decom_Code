/* megaint2.c - works with a char array as a string of unsigned bytes
   to manipulate 64-bit unsigned integers

   Note: these routines only work for arrays in which the bits of individual
         bytes index from lo to hi in the same direction as the bytes index
         in the char array. (this is equivalent to vms 64-bit integer 
         manipulation)

   Add, subtract, and multiply work like base 256 long hand two-operand
   arithmetic, while division is 64-bit binary
*/
#include <stdio.h>
#define MAX_SZ 8

/*
   Copies 8 bytes from a to b
*/
#ifdef USE_MACOSX
void ui8_cpy_( a, b )
#else
void ui8_cpy( a, b )
#endif
   unsigned char *a;
   unsigned char *b;
{
   int i;
   for (i=0; i<MAX_SZ; i++) b[i] = a[i];
   return;
}

/*
  Sets an arbitrary bit in byte array
*/
static void set_bit( in, n )
   unsigned char *in;
   int n;
{
   int i, j, x;
   unsigned char u;

   i = n/8;
   j = n - (i*8);
   x = 1;
   x = x << j;
   u = x;
   in[i] = in[i] | u;
   return;
}

/*
  Returns the number of the most significant bit set in byte array
  (the array is considered an 8-byte unsigned integer, little end first)
*/
static int highest_bit_set( in )
   unsigned char *in;
{
   int i, k;
   unsigned char j;

   for (i=MAX_SZ-1; i >= 0; i--) if (in[i] != 0) break;
   if (i<0) return -1;

   for (j=0x80, k=7; (j & in[i]) == 0; j = j >> 1, k--);
   return (i*8) + k;
}

/* 
   cpy_bits copies arbitrary number of bits from char array source
   to char array destination
*/
void cpy_bits( in, inbitbeg, inbitend, out, outbitbeg, outbitend )
   unsigned char *in;
   int inbitbeg;
   int inbitend;
   unsigned char *out;
   int outbitbeg;
   int outbitend;
{
   int ib, ob;
   int it, ot;
   int i,j,k,n,m;
   unsigned char x, y;

   ib = inbitbeg/8;
   it = inbitbeg - (ib*8);

   ob = outbitbeg/8;
   ot = outbitbeg - (ob*8);

   i = inbitend - inbitbeg + 1;
   j = outbitend - outbitbeg + 1;
   if (i > j)
   {
      puts("Illegal range in cpy_bits.");
      return;
   }
   m = j;
   k = inbitbeg;


   for (i=0; i<m; i++, k++)
   {
       x = ( (k <= inbitend) ? 1 : 0 );
       x = x << it;
       y = 1;
       y = y << ot;
/*
printf(" in by/bit=%d,%d    out by/bit=%d,%d m=%d    x,y=%u,%u\n",
ib, it, ob, ot, m, x, y);
*/
       if ((x & in[ib]) != 0) out[ob] = out[ob] | y;
       else                   out[ob] = out[ob] & ~y;

       ++it;
       ++ot;
       if (it == 8) {it = 0; ++ib;}
       if (ot == 8) {ot = 0; ++ob;}
   }
   return;
}

/*
   cpy_bits2 - is used for copying bits from a little-end-first bitstream
   to a big-end-first bit stream, where the target's bits are in the range
   0 to 31.  This routine is for converting vax bits to sun bits.
*/
int cpy_bits2( in, inbitbeg, length, out, outbitbeg, switch_bits)
   unsigned char *in;
   int inbitbeg;
   int length;   
   unsigned char *out;
   int outbitbeg;
   int switch_bits;
{
   int i,j,k;
   unsigned char b[8];

   i = inbitbeg + length - 1;
   j = outbitbeg + length - 1;
   if (switch_bits)
   {
      for (k=0; k<8; k++) b[k] = 0;
      cpy_bits( in, inbitbeg, i, b, outbitbeg, j);
      for (i=0, j=3; i < 4; i++, j--) out[i] = out[i] | b[j];
   }
   else
      cpy_bits( in, inbitbeg, i, out, outbitbeg, j);

   return 1;
}

/*
  Adds two 8-byte little-end-first integers
*/
#ifdef USE_MACOSX
void ui8_add_( op1, op2, out, err )
#else
void ui8_add( op1, op2, out, err )
#endif
   unsigned char *op1;
   unsigned char *op2; 
   unsigned char *out; 
   int *err;
{
   int sz = MAX_SZ;
   unsigned int carry=0;
   int i;
   unsigned int w=0;

   *err = 1;
   for (i=0; i < sz; i++)
   {
      w = (unsigned int) op1[i] + (unsigned int) op2[i] + carry;
      carry = w >> 8;
      w = w & 0xff;
      out[i] = w;
   }
   if (carry != 0)
   {
      printf("UI8_ADD: arithmetic overflow!\n");
      *err = 2;
   }
}

/*
  Tests for equality between two byte arrays.
  Returns 1 if op1 > op2, 0 if op1==op2, -1 if op1<op2 
*/
#ifdef USE_MACOSX
int ui8_is_eq_( op1, op2, sz )
#else
int ui8_is_eq( op1, op2, sz )
#endif
   unsigned char *op1; 
   unsigned char *op2; 
   int sz;
{
   int i = sz-1;

   while (i >= 0 && op1[i]==op2[i]) --i;
   if (i<0) return 0;
   if (op1[i] > op2[i]) return 1;
   return -1;
}

/*
  Tests for equality between two byte arrays of length 8.
  Returns 1 if op1 > op2, 0 if op1==op2, -1 if op1<op2 
*/
#ifdef USE_MACOSX
int ui8_is_eq2_( op1, op2 )
#else
int ui8_is_eq2( op1, op2 )
#endif
   unsigned char *op1; 
   unsigned char *op2;
{
   int i = 7;

   while (i >= 0 && op1[i]==op2[i]) --i;
   if (i<0) return 0;
   if (op1[i] > op2[i]) return 1;
   return -1;
}

/*
  Subtracts two 8-byte little-end-first integers.
  Operand 1 should be larger than operand 2 for a positive result 
*/
#ifdef USE_MACOSX
void ui8_sub_( op1, op2, out, err )
#else
void ui8_sub( op1, op2, out, err )
#endif
   unsigned char *op1; 
   unsigned char *op2; 
   unsigned char *out; 
   int *err;
{
   int sz=MAX_SZ;
   int carry=0;
   int i=0;
   unsigned int w=0;
   int x;
   unsigned char *a, *b;
   int sign_value;

#ifdef USE_MACOSX
   sign_value = ui8_is_eq_(op1, op2, sz);
#else
   sign_value = ui8_is_eq(op1, op2, sz);
#endif
   *err = sign_value;
   switch(sign_value)
   {
      case  1: a = op1; b = op2; break;
      case -1: a = op2; b = op1; break;
      default: for (i=0; i<sz; out[i]=0, i++); return;
   }

   for (i=0; i < sz; i++)
   {
      x = (a[i] - b[i]) - carry;
      if (x < 0)
      {
         w = (0x100 + (unsigned int) a[i] ) - b[i] - carry;
         carry = 1;
         out[i] = w;
      }
      else
      {
         carry = 0;
         out[i] = x;
      }
/*
printf("i=%d, a=%u, b=%u, carry=%d sign=%d\n", i, a[i], b[i], carry, sign_value);
*/
   }
   if (carry != 0)
   {
      printf("UI8_SUB: arithmetic underflow!\n");
      *err = 2;
   }
}

/*
  Divides two 8-byte little-end-first integers.
  Operand 1 should be larger than operand 2 for a non-zero result.
  This one works only for 64-bit=8-byte size inputs.
*/
#ifdef USE_MACOSX
void ui8_divide_( op1, op2, out, r, err )
#else
void ui8_divide( op1, op2, out, r, err )
#endif
   unsigned char *op1;       /* dividend */
   unsigned char *op2;       /* divisor  */
   unsigned char *out;       /* quotient */
   unsigned char *r;         /* remainder*/
   int *err;
{
   int sz=MAX_SZ;
   int h1, h2, cb;
   unsigned char z[MAX_SZ];
   unsigned char v[MAX_SZ];
   int i;
   int sign;

   for (i=0; i < sz; i++) { out[i] = 0; z[i] = op1[i]; v[i] = 0; r[i] = 0;}

   /* if divisor >= dividend the result is zero, the remainder is the 
      dividend, so return early
   */
#ifdef USE_MACOSX
   *err = ui8_is_eq_(op1, op2, sz);
#else
   *err = ui8_is_eq(op1, op2, sz);
#endif
   if (*err < 0) { for (i=0; i<sz; i++) r[i] = op1[i]; return; }
   if (*err == 0) { out[0] = 1; return; }

   h1 = highest_bit_set(z);
   h2 = highest_bit_set(op2);
   cb = h1 - h2;

   while(cb >= 0 && h1 >= 0)
   {
      cpy_bits(op2,0,h2,v,cb,h1);
#ifdef USE_MACOSX
      ui8_sub_(z,v,r,&sign);
#else
      ui8_sub(z,v,r,&sign);
#endif
      switch(sign)
      {
         case 1: 
            set_bit(out,cb);
            for (i=0; i<MAX_SZ; i++) { z[i] = r[i]; v[i] = 0; }
            break;
         case 0:
            set_bit(out,cb);
            return;
            break;
         case -1:
            for (i=0; i<MAX_SZ; i++) { v[i] = 0; r[i] = z[i]; }
            break;
         default:
            puts("Error in ui8_divide!");
            *err = -3;
            break;
      }
      h1 = highest_bit_set(z);
      cb--;
      while( (cb>=0) && ((h1-cb) < h2) ) cb--;
   }
   return;
}

/*
  Multiplies two 8-byte little-end-first integers.
  This version works for 64-bit values only 
*/
#ifdef USE_MACOSX
void ui8_multiply_( op1, op2, out, err )
#else
void ui8_multiply( op1, op2, out, err )
#endif
   unsigned char *op1; 
   unsigned char *op2; 
   unsigned char *out; 
   int *err;
{
   char *rn="ui8_multiply";
   int sz=MAX_SZ;
   unsigned int carry=0;
   int i,j,k,m,n;
   unsigned int x;
   unsigned int y[MAX_SZ];
   unsigned char *a, *b;
   int sign_value;

   for (i=0; i < sz; y[i]=0, out[i]=0, i++); 
#ifdef USE_MACOSX
   sign_value = ui8_is_eq_(op1, op2, sz);
#else
   sign_value = ui8_is_eq(op1, op2, sz);
#endif
   *err = 0;
   switch(sign_value)
   {
      case  1: a = op1; b = op2; break;
      case -1: a = op2; b = op1; break;
      default: a = op1; b = op2; break;
   }

   for (m=sz-1; m >= 0 && b[m] == 0; m--); /* digit count of smaller op */
   if (m < 0) return;
   for (n=sz-1; n >= 0 && a[n] == 0; n--); /* digit count of larger op */

   for (i=0; i <= m; i++)       /* smaller and bottom op */
   {
      for (j=0; j <= n; j++)   /* larger and top op */
      {
         x = ( (unsigned int) a[j] * (unsigned int) b[i]) + carry;
/*
printf("i,j=%d,%d   b[i],a[j]=%X,%X   x=%X, carry=%X \n",
i,j, b[i],a[j], x,carry);
*/

         if (x > 0xff)
         {
            carry = x >> 8;
            x = x & 0xff;
         }
         else
         {
            carry = 0;
         }
         k = i + j;
         if (k < sz) y[k] = y[k] + x;
         else 
         {
            --*err;
            puts("UI8_MULTIPLY: overflow error (a)!");
            printf("%s: %s %2x %2x %2x %2x   %2x %2x %2x %2x\n", rn, "op1",
            op1[0], op1[1], op1[2], op1[3], op1[4], op1[5], op1[6], op1[7]);
            printf("%s: %s %2x %2x %2x %2x   %2x %2x %2x %2x\n", rn, "op2",
            op2[0], op2[1], op2[2], op2[3], op2[4], op2[5], op2[6], op2[7]);
         }
      }
      if (carry != 0)
      {
         k++;
         if ( k < sz )
         {
            y[k] = y[k] + carry;
            carry = 0;
         }
         else 
         {
            --*err;
            puts("UI8_MULTIPLY: overflow error (b)!");
         }
      }
   }

   carry = 0;
   for (i=0; i < sz; i++)
   {
      x = y[i] + carry;
      if (x > 0xff)
      {
         carry = x >> 8;
         out[i] = x & 0xff;
      }
      else
      {
         carry = 0;
         out[i] = x;
      }
   }
   if (carry != 0)
   {
      puts("UI8_MULTIPLY: overflow error (c)!");
      --*err;
   }
}
