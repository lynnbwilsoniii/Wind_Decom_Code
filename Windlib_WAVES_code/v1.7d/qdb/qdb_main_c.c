/* qdb_main_c.c - C version (portable) of qdb main program module
*/

#include "qdb.h"

extern int qdb_args();
extern int write_report();

main(argc, argv)
   int argc;
   void *argv[];
{
   int ok;
   Select sel;

   sel.n_ev   = 0;
   sel.flags  = 0;
   sel.flags2 = 0;
   sel.flags3 = 0;
   sel.flags4 = 0;

   ok = qdb_args(argc, argv, &sel);
   if (ok == OK) ok = write_report(&sel);
}
