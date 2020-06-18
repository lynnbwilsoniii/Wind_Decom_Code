/* string.c - misc c string routines

!!!!#include <stdio.h>
*/
#include <string.h>

/*
  Compares an actual file specification with a wild card specification.

  % = match one character
  ? = match one character
  * = match zero or more characters

  Returns 1 for match, 0 for non-match.
*/
int wc_match_spec( b, a )
   char *b; /* file spec containing wild cards *, ?, and/or % */
   char *a; /* real file name to test against "b" */
{
   char s[256];
   char *c, *d;
   int match=1;

/*
printf("arg #1: %s.\n", b);
printf("arg #2: %s.\n", a);
*/
   if (*b == '\0') return 0;

   for (; match == 1 && *b != '\0'; )
   {
      switch(*b) /* b points to a char in the file selection mask */
      {
      case '?':
      case '%':
         while (*b == '%' || *b == '?')
         {
            if (*a == '\0') {match = 0; break;}
            a++;
            b++;
         }
         break;
      case '*':
         ++b;
         if (*b == '\0' ) break;
         for (c=s; 
              *b != '*' && *b != '%' && *b != '?' && *b != '\0';
              ++b, ++c) *c = *b;
         *c = '\0';
         if ( (d=strstr(a,s)) == NULL) /* is s in a? */
         {
            match = 0;
            break;
         }
         for (a=d, c=s; *c != '\0'; ++c, ++a);
         break;
      default:
         for (; *b == *a && *b != '\0' && *a != '\0'; ++b, ++a);
         if (*a != *b)
         {
            if (!(*b == '*' || *b == '%' || *b == '?' || *b == '\0')) match = 0;
         }
         break;
      }
   }

   if (*b == '\0' && *a != '\0')
      if (b[-1] != '*') match = 0;

   return match;
}

/* file name string compare, finds last '/' in path portion of file specs
   and calls strcmp to compare file names independant of path spec
*/
int fnstrcmp(a, b)
   char *a;
   char *b;
{
   char *u, *v, *x, *y;

   for (u=NULL, x=a; *x != '\0'; ++x) if (*x == '/') u = x;
   for (v=NULL, y=b; *y != '\0'; ++y) if (*y == '/') v = y;

   if (u == NULL)  u = a;  else u++;
   if (v == NULL)  v = b;  else v++;

   return strcmp(u,v);
}

short i2fncmpasc(a, b)
{
   return (short) fnstrcmp(a, b);
}

short i2fncmpdec(a, b)
{
   return - (short) fnstrcmp(a, b);
}

int i4fncmpasc(a, b)
{
   return fnstrcmp(a, b);
}

int i4fncmpdec(a, b)
{
   return - fnstrcmp(a, b);
}
