/* usage.c - get a bunch of wind_lib events and print the system usage
*/
#include </sys/sys/types.h>
#include </sys/sys/time.h>
#include </sys/sys/resource.h>
#include <stdio.h>
#include <stdlib.h>


int show_usage_()
{
   struct rusage r;
   char s1[26], s2[26];
   int i;

   i = RUSAGE_SELF;
   getrusage(i, &r);

   printf("User time used (sec.usec)....: %d.%4.4d\n", 
      r.ru_utime.tv_sec, r.ru_utime.tv_usec);
   printf("System time used (sec.usec)..: %d.%4.4d\n", 
      r.ru_stime.tv_sec, r.ru_stime.tv_usec);
   printf("Page faults not requiring I/O: %d\n", r.ru_minflt);
   printf("Page faults requiring I/O....: %d\n", r.ru_majflt);
   printf("Swaps........................: %d\n", r.ru_nswap);
   printf("Block input operations.......: %d\n", r.ru_inblock);
   printf("Block output operations......: %d\n", r.ru_oublock);
   printf("Messages sent................: %d\n", r.ru_msgsnd);
   printf("Messages received............: %d\n", r.ru_msgrcv);
   printf("Signals received.............: %d\n", r.ru_nsignals);
   printf("Voluntary context switches...: %d\n", r.ru_nvcsw);
   printf("Involuntary context switches.: %d\n", r.ru_nivcsw);

   return 0;
}
