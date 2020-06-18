/* file_info.c - uses RMS to get basic file information like size, creation
   date, uic, protection, and file_size.
*/
#include <rms>
#include <stdio>
#include <ssdef>
#include <string.h>
#include <stdio.h>

extern int sys$open();
extern int sys$close();

int file_info( filename, ldate, lprot, luic1, luic2, file_size)
   char *filename;
   int *ldate;
   int *lprot;
   int *luic1;
   int *luic2;
   int *file_size;
{
   struct FAB fab;
   struct XABPRO xabpro;
   struct XABDAT xabdat;
   struct XABFHC xabfhc;
   int i,j;
   int ok;
   int frst_free;

   fab = cc$rms_fab;
   xabpro = cc$rms_xabpro;
   xabdat = cc$rms_xabdat;
   xabfhc = cc$rms_xabfhc;

   fab.fab$b_fac = FAB$M_GET;
   fab.fab$b_shr = FAB$M_GET | FAB$M_PUT | FAB$M_UPD | FAB$M_UPI;
   fab.fab$l_fna = filename;
   fab.fab$b_fns = (unsigned char) strlen(filename);

   fab.fab$l_xab    = (char *) &xabpro;
   xabpro.xab$l_nxt = (char *) &xabdat;
   xabdat.xab$l_nxt = (char *) &xabfhc;
   xabfhc.xab$l_nxt = NULL;

   ok = sys$open(&fab);
   if ((ok & 0x00000001) == 0) return 0;

   memcpy(ldate, &xabdat.xab$q_cdt, 8);
   *lprot = 0;
   *lprot = xabpro.xab$w_pro;
   *luic1 = xabpro.xab$w_grp;
   *luic2 = xabpro.xab$w_mbm;
   *file_size = xabfhc.xab$l_ebk;
   frst_free = xabfhc.xab$w_ffb;
   
   ok = sys$close(&fab);

   return 1;
}
