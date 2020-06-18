/* sun_win_sz.c - gets the size of an "xterm" under SunOS
*/
/* added BSD_COMP for SunOS 5.4 port, 22AUG96, jk */
#define BSD_COMP
#include <sys/ioctl.h>

int get_screen_size( page_size )
   int *page_size;
{
   struct winsize ws;
   ioctl(0, TIOCGWINSZ, &ws);
   *page_size = ws.ws_row;
   return 1;
}
