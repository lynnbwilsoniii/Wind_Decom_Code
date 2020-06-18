/* block_file_size.c - determines the number of 512-byte chunks in a file */
#include "wind_os_def.h"

#ifdef USE_MACOSX

#include <sys/stat.h>

#else

#if SYSTEM == SUNOS
/* old, changed for SunOS 5.4 port 
 #include </sys/sys/stat.h>
*/
#include <sys/stat.h>
#elif SYSTEM == VMS
#include <stat.h>
#endif

#endif


int block_file_size_(name)
char *name;   /* file name */
{
  struct stat b;
  int i, j;

  if (stat(name, &b) == -1)
  {
      return -2;
  }
  i = b.st_size / 512;
  j = b.st_size % 512;
  if (j > 0) ++i;
  return i;
}
