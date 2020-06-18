/* carc.c - links with car.for to test character array passing between
   fortran and c
*/

int c_str ( s , len)
   char *s;
   int len;
{
   int i,j;

   printf("Here is the string:\n");
   printf("s: %s, len=%d.\n", s, len);

   return 1;
}
