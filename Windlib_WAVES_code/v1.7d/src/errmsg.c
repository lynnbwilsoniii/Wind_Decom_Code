/* errmsg.c - general purpose error message handling routine
*/
#include <stdio.h>

#define YES 1
#define NO 0
#define OK 1
#define ERR 0

static int print_it=YES;

int wc_msg(s)
   char *s;
{
   if (print_it == YES) printf("%s\n", s);
   return OK;
}

int wc_msg_set_print()
{
   print_it = YES;
   return OK;
}

int wc_msg_set_noprint()
{
   print_it = NO;
   return OK;
}
