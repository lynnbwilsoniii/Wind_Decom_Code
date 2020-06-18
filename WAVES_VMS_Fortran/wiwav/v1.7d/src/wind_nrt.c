/* wind_nrt.c
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "wind_os_def.h"
#include "wind_nrt_def.h"
/*
#include <curses.h>
*/

extern int Rtc_Connect();
extern int Rtc_GetPacket();
extern int Rtc_LoginUser();
extern int Rtc_SetPacket();

#if SYSTEM == VMS
extern int set_job_log_name_c();
#endif

#define W_OK 1
#define W_ERR 0
#define BUF_SIZE 24+15552+8
#define ENV_USERNAME    "WIND_NRT_USERNAME"
#define ENV_PASSWORD    "WIND_NRT_PASSWORD"
#define ENV_SERVER      "WIND_NRT_SERVER"
#define ENV_SERVER_PORT "WIND_NRT_SERVER_PORT"
#define DEFAULT_PORT_NUMBER 500
#define DEFAULT_SERVER_NAME "istp6.gsfc.nasa.gov"

#ifdef USE_MULTINET
#define TCPIP_PKG "Multinet"
#endif

#ifdef USE_UCX
#define TCPIP_PKG "UCX"
#endif

#ifdef USE_SUN
#define TCPIP_PKG "Sun"
#endif

#ifdef USE_MACOSX
#define TCPIP_PKG "MacOSX"
#endif

static int first_time=1;

/* must check the telem_mode to determine the record internal structure */
struct science_tm_data {
   unsigned char tm[45*250];
};
struct maneuver_tm_data {
   unsigned char tm[61*250];
};

struct wind_cdhf_rec {
   unsigned long   inst_num; /* 300 byte LZ data record hdr */
   unsigned long   rec_num;
   unsigned long   maf_count;
   unsigned long   sc_time[2];
   unsigned long   year;
   unsigned long   doy;
   unsigned long   msoday;
   unsigned long   usec;   
   unsigned long   fill_mif;
   unsigned long   sync_err_mif;
   unsigned long   telem_mode;
   char      quality[252];
   union {
      struct science_tm_data tms;
      struct maneuver_tm_data tmm;
   } u;
};

struct wav_nrt_buf {
   char   startblk[8];      /* 24 bytes of packet header */
   char   instrument[4];
   char   filler[12];
   struct wind_cdhf_rec r;
   char   trailer[8];       /* 8-byte packet trailer */
};

/* buffer to hold full WI_LZ_WAV mjr fr rec plus nrt head/tail bytes*/
union waves_buf {
   double dummy; /* just to assure 8-byte alignment, jk */
   char   buf[BUF_SIZE];
   struct wav_nrt_buf b;
};

#ifdef USE_SUN
int w_nrt_is_available_()
#else
#ifdef USE_MACOSX
int w_nrt_is_available_()
#else
int w_nrt_is_available()
#endif
#endif
{ return 1; }

static int w_nrt_get_login_params(s, user, pw, server, port)
char *s; /* eg: "nrt username password servername portnumber_on_server" */
char *user;
char *pw;
char *server;
int  *port;
{
   char *rn="w_nrt_get_login_params";
   char *ps;
   char env[256];
   char *a, *b, *c, *d;
   int i,n;

   /* get the server name from environment, channel_open string */
   *server = '\0';
   n = sscanf(s,"%*s %*s %*s %128s", server);
   if (n == 1)
   {
      ps = getenv(server);
      if (ps != NULL) strncpy(server, ps, 128);
   }
   else
   {
      ps = getenv(ENV_SERVER); /* does default environ var exist? */
      if (ps != NULL) strncpy(server, ps, 128);
      else
      {
         strcpy(server, DEFAULT_SERVER_NAME);
      }
   }
   printf("WIND/WAVES NRT remote login to %s\n\n", server);

   /* get the username from environment, channel_open string, or prompt */
   /* user name is first positional parameter, if present */
   *user = '\0';
   n = sscanf(s,"%*s %32s", user);
   if (n == 1)
   {
      ps = getenv(user); /* is this string a named environment variable? */
      if (ps != NULL) strncpy(user, ps, 32);
   }
   else
   {
      ps = getenv(ENV_USERNAME); /* does default environ var exist? */
      if (ps != NULL) strncpy(user, ps, 32);
      else
      {
         printf("NRT Username: ");
         a = fgets(user,32,stdin);
         if (a==NULL) return W_ERR;
         if (*a < ' ') return W_ERR;
         for (i=0; *a > ' ' && i < 32; ++a, ++i);
         *a = '\0';
      }
   }

   /* get the password from environment, channel_open string, or prompt */
   *pw = '\0';
   n = sscanf(s,"%*s %*s %32s", pw);
   if (n == 1)
   {
      ps = getenv(pw);
      if (ps != NULL) strncpy(pw, ps, 32);
   }
   else
   {
      ps = getenv(ENV_PASSWORD);
      if (ps != NULL) strncpy(pw, ps, 32);
      else
      {
/*
         a = env;
         *a = '\0';
         initscr();
         box(stdscr, 'x','q');
         echo();
         move(4,4);
         printw(" Enter Password: ");
         noecho();
         i = getstr(a);
         strcpy(pw,a);
         echo();
         endwin();
*/
/*
*/
         printf(" [PW echoes]\n");
         printf("NRT Password: ");
         a = fgets(pw,32,stdin);
         if (a==NULL) return W_ERR;
         if (*a < ' ') return W_ERR;
         for (i=0; *a > ' ' && i < 32; ++a, ++i);
         *a = '\0';
      }
   }

   /* get the server port from environment, channel_open string, or prompt */
   *env = '\0';
   *port = 0;
   n = sscanf(s,"%*s %*s %*s %*s %16s", env);
   if (n == 1)
   {
      ps = getenv(env);
      if (ps != NULL) n = sscanf(ps,"%d", port);
      else n = sscanf(env,"%d", port);
   }
   else
   {
      ps = getenv(ENV_SERVER_PORT); /* does default environ var exist? */
      if (ps != NULL) n = sscanf(ps,"%d", port);
      else { n = 1; *port = DEFAULT_PORT_NUMBER; }
   }
   if (n != 1)
   {
      printf("%s: bad specified port number, using default instead.\n", rn);
      *port = DEFAULT_PORT_NUMBER;
   }
/*
   printf("Port is %d\n", *port);
*/

   return W_OK;
}

#ifdef USE_SUN
int w_nrt_open_
#else
#ifdef USE_MACOSX
int w_nrt_open_
#else
int w_nrt_open
#endif
#endif
(s)
char *s;
{
   char *rn="w_nrt_open";
   int  ok;
   char usrnam[64];
   char passwd[64];
   char server[256];
   char packet_status[64];
   int  port;

   ok = w_nrt_get_login_params(s, usrnam, passwd, server, &port);
/*
   printf("%s: user=%s pw=%s server=%s port=%d %s\n",
      rn, usrnam, passwd, server, port, TCPIP_PKG);
*/
   if (ok != W_OK)
   {
       printf("%s: cannot determine logon parameters.\n", rn);
       return W_ERR;  
   }

   if (Rtc_Connect(server, port) < 0) {
      printf("Connect to server failed\n");
      return W_ERR;
   }

   /* send server username and password */
   if (Rtc_LoginUser(usrnam, passwd) < 0) {
      printf( "Login Failed\n");
      return W_ERR;
   }

#if SYSTEM == VMS
   /* define logical name equivalences for username and password */
   /* [this will make subsequent same-process logins automatic]  */
   set_job_log_name_c(ENV_USERNAME, usrnam);
   set_job_log_name_c(ENV_PASSWORD, passwd);
#endif

   /* Select WAVES LZ telemetry packets */
   if (Rtc_SetPacket("WI_LZ_WAV",packet_status) < 0) {
      printf("Packet Type Error - %s\n",packet_status);
      return W_ERR;
   }

   /* Signal end of packet type selection */
   if (Rtc_SetPacket("END",packet_status) < 0) {
      printf("Packet Type Error - On END command!??? - %s\n",
         packet_status);
      return W_ERR;
   }

   printf("%s: connected to server %s as %s [%s]\n",
           rn, server, usrnam, TCPIP_PKG);
   return W_OK;
}

#ifdef USE_SUN
int w_nrt_get_rec_
#else
#ifdef USE_MACOSX
int w_nrt_get_rec_
#else
int w_nrt_get_rec
#endif
#endif
(r)
struct wind_cdhf_rec *r;
{
   char *rn="w_nrt_get_rec";
   int  n;
   char txtbuf[256];
   union waves_buf w;

   if (first_time == 1)
   {
      first_time = 0;
      printf("%s: waiting for first NRT record..\n", rn);
   }

   /* routine rtc_getpacket ignores the size argument */
   n = Rtc_GetPacket(&w.dummy, (long *) BUF_SIZE);
   if (n <= 0) {
      perror("RTC_CLIENT: socket_read error reading data");
      sprintf (txtbuf,"W_ERROR READING BUFFER n=%d=%x (hex) \n",n,n);
      printf (txtbuf);
      return W_ERR;
   } 

/*
printf("%s: just received a nrt record, ins=%.4s fill=%.12s trail=%.12s\n", rn, 
  w.b.instrument, w.b.filler, w.b.trailer);
printf("%s: lz hdr yr doy msec %d %d %d, #fill=%d #syerr=%d md=%d\n", rn,
  w.b.r.year, w.b.r.doy, w.b.r.msoday,
  w.b.r.fill_mif, w.b.r.sync_err_mif, w.b.r.telem_mode);
*/

   /* the end of pass record is 41 bytes long, 
      we should really check for the ENDOFPASS string in the data block
   */
   if (n == 41) return 82; /* wind/waves end of file code */

   /* copy the data record to caller's buffer */
   *r = w.b.r;

   return W_OK;
}
