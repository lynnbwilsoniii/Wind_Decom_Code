/* wcfo.c - tests wild card file name opening
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "windlib.h"

#define OS_ERR 2 
#define OS_SUCCESS 0  /* 0 for unix, 1 for vms */

static void initstr(str, len, fill)
   char *str;
   int len;
   char fill;
{
   int i;
   for (i=0; i < (len-1); i++, str++) *str = fill;
   *str = '\0';
   return;
}

static void i_sho_i(pch, name)
   int  *pch;
   char *name;
{
   int ok;
   int size=10;
   int buf[10];
   int ret_size;

   ok = w_item_i4(pch, name, buf, &size, &ret_size);
   if (ok == W_OK)
      printf("  Item %32s (%d): %d\n", name, ret_size, buf[0]);
   else
      printf("  Cannot get item %s, ok=%d\n", name, ok);
   return;
}

static void i_sho_f(pch, name)
   int  *pch;
   char *name;
{
   int ok;
   int size=10;
   float buf[10];
   int ret_size;
   int max_to_show=5;
   int i,j;

   ok = w_item_r4(pch, name, buf, &size, &ret_size);
   if (ok == W_OK)
   {
      printf("  Item %32s (%d): %f\n", name, ret_size, (double) buf[0]);
      j = (max_to_show > ret_size) ? ret_size : max_to_show;
      for (i=1; i < j; i++)
         printf("  %42.42s %f\n", " ", buf[i]);
   }
   else
      printf("  Cannot get item %s, ok=%d\n", name, ok);
   return;
}

static void i_sho_d(pch, name)
   int  *pch;
   char *name;
{
   int ok;
   int size=10;
   double buf[10];
   int ret_size;
   char c_ur8[32];

   ok = w_item_r8(pch, name, buf, &size, &ret_size);
   if (ok == W_OK)
   {
      printf("  Item %32s (%d): %f\n", name, ret_size, buf[0]);
      if (NULL != strstr(name, "SCET"))
      {
         initstr(c_ur8, sizeof(c_ur8), ' ');
         ok = w_ur8_to_string(&buf[0], c_ur8);
         printf("  %42.42s %26.26s.\n", " ", c_ur8);
      }
   }
   else
      printf("  Cannot get item %s, ok=%d\n", name, ok);
   return;
}

static void i_sho_c(pch, name)
   int  *pch;
   char *name;
{
   int ok;
   int ret_size;
   char str[64];
   char buf[8][12]; /* 8 strings of length 12 */
   int size_one_element=12;
   int number_of_elements=8;
   int i;

   /* initialize an array of character strings */
   for (i=0; i < number_of_elements; i++) 
      initstr(&(buf[i][0]), size_one_element, ' ');

   ok = w_item_char(pch, name, buf, &number_of_elements, &ret_size);
   if (ok == W_OK)
   {
      printf("  Item %32s (%d): %12.12s.\n", name, ret_size, &(buf[0][0]));
      for (i=1; i < ret_size; i++)
      {
         printf("  %42.42s %12.12s.\n", " ", &(buf[i][0]));
      }
   }
   else
      printf("  Cannot get item %s, ok=%d\n", name, ok);
   return;
}

main()
{
   int i,j;
   int ok;
   int size, ret_size;
   int ch, *pch=&ch;
   char *event="RAD2";
   int ev_count=0;
   char file[128];
   char ver[32];
   char *fmt1="*199604%02d*";

   strcpy(ver,"12345678901234567890");
   ok = w_version(ver);
   printf("Using wind_lib version %12s\n", ver);

   for (i=1; i < 4; i++)
   {
      initstr(file, sizeof(file), ' ');
      sprintf(file, fmt1, i);
      printf("Calling for file %s...\n", file);
      ok = w_channel_open(pch, file);
      if (ok != W_OK) {printf("cannot open ch, ok=%d\n", ok); return OS_ERR;};

      initstr(file, sizeof(file), ' ');
      ok = w_channel_filename(pch, file);
      printf("Opened file: %s\n", file);

      ev_count = 0;
      do
      {
         ok = w_event(pch, event);
         if (ok == W_OK)
         {
            ev_count++;
            printf("----%d----------------------------------------\n", ev_count);
            i_sho_i(pch, "DPU_MAJOR_FRAME");
            i_sho_i(pch, "DPU_MINOR_FRAME");
            i_sho_i(pch, "SC_MODE");
            i_sho_d(pch, "EVENT_SCET_R8");
            i_sho_c(pch, "PACKET_ID_ARRAY");
            i_sho_f(pch, "FREQUENCIES_HZ_R4");
         }
      } while (ok == W_OK && ev_count < 1);

      ok = w_channel_close(pch);
      if (ok != W_OK) {printf("cannot close ch, ok=%d\n", ok); }
   }

   return OS_SUCCESS;
}
