/* qdb.h - query database definitions file
*/
#define OK 1
#define ERR 0
#define YES 1
#define NO 0

#define MAX_N_EV 16
#define MAX_EV_LEN 32

#define V_AUTHOR_DATE 0x0001
#define V_DPU_VERSION 0x0002
#define V_FFT_VERSION 0x0004
#define V_TDS_VERSION 0x0008
#define V_AREA        0x0010
#define V_SUBTYPE     0x0020
#define V_DATE        0x0040
#define V_ITEM_NAME   0x0080
#define V_WILD_NAME   0x0100

typedef struct qdb_validation {
   int dpu_ver;
   int fft_ver;
   int tds_ver;
   int subtype;
   int area;
   int ymdhms[2];
   int proc;
   char auth_date[12];
   char item_name[64];
} Validation ;

typedef struct qdb_select {
   int flags;
   int flags2;
   int flags3;
   int flags4;
   Validation v;
   int n_ev;
   char ev[MAX_N_EV] [MAX_EV_LEN];
   char f[256];
} Select;

#include "wdir:item.h"
#include "wdir:wind_os_def.h"
