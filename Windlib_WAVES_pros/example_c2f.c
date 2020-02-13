#include <stdio.h>

void sum_array(int argc, void *argv[])
{
  extern void sum_array1_();	/* FORTRAN routine */
  int *n;
  float *s, *f;
  
  f = (float *) argv[0];    /* Array pntr */
  n = (int *) argv[1];      /* Get # of elements */
  s = (float *) argv[2];    /* Pass back result a parameter */
  
  sum_array1_(f, n, s);     /* Compute sum */
}
