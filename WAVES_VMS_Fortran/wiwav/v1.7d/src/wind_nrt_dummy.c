/* wind_nrt_dummy.c - a module to resolve linker references on systems
   without a tcp/ip package (ucx, multinet, ...) or programs linking to
   wind_lib.olb (non-shareable link).
*/
#include <stdio.h>

#define W_OK 1
#define W_ERR 0


int w_nrt_is_available() { return 0; }

int w_nrt_open(s)
char *s;
{
   char *rn="w_nrt_open";
   printf("%s: NRT option is not available.\n", rn);
   return W_ERR;
}

int w_nrt_get_rec(r)
void *r;
{
   char *rn="w_nrt_get_rec";
   printf("%s: NRT option is not available.\n", rn);
   return W_ERR;
}
