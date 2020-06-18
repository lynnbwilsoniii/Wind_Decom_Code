/* nrt_rtc_client.c - a client application, origianally written for a
   VMS/Multinet environment by John M. Barker around 03/01/95.
   Significant modifications since then by Jon Kappler.

   Certain preprocessor definitions are provided via the compliation command:
	USE_UCX - VMS systems with DEC TCP/IP
	USE_MULTINET - VMS systems Multinet TCP/IP
	USE_SUN - for standard BSD-Unix type TCP/IP socket stuff
*/
#include "wind_nrt_def.h"

#ifdef USE_MULTINET
/* this USE_MULTINET_V34_ARGS is for the connect() function, which seems
   to use the standard argument list now in multinet version 4.0
*/
#undef USE_MULTINET_V34_ARGS
/* These are the original includes, jk */
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include]netdb.h"
/*_*/
#define OK_EXIT 1
#define ERR_EXIT 0
#endif

#include <stdio.h>

#ifdef USE_UCX
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <types.h>
#include <socket.h>
#include <in.h>
#include <netdb.h>
#include <unixio.h>
#include <inet.h>
#include <ucx$inetdef.h>
#define socket_perror perror
#define socket_read read
#define socket_write write
#define OK_EXIT 1
#define ERR_EXIT 0
#endif

#ifdef USE_SUN
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#define socket_perror perror
#define socket_read read
#define socket_write write
#define OK_EXIT 0
#define ERR_EXIT 1
#endif

/* States: */
#define stateWAITFORREPLY 1
#define stateSUCCESS      2
#define stateFAILURE      3
#define stateERROR        4
#define stateSENDUSER    10
#define stateWAITUSER    11
#define stateSENDPASS    20
#define stateWAITPASS    21
#define stateWAITDB      22
#define stateWAITLOGIN   23
#define stateSENDPACK    31
#define stateWAITPACK    32
#define stateDENIED      33
#define stateWAITSYS     34
#define stateSYSTEM      35
#define stateSYSDOWN     36

static int nsocket;

#ifdef USE_UCX
void bcopy(src, trg, n)
   char *src;
   char *trg;
   int n;
{
   int i;

   for (i=0; i<n; i++) *trg++ = *src++;
}
#endif

/***********************************************************/
void Reverse_bytes(data, num_of_bytes)
   char data[];
   int num_of_bytes;
{
   int i,j, end_of_loop;
   char temp;

   end_of_loop = (num_of_bytes/2) - 1;
   j = num_of_bytes - 1;
   for (i=0; i<=end_of_loop; i+=1)
   {   
      temp = data[i];
      data[i] = data[j];
      data[j] = temp;
      j -= 1;
   }
}

int Rtc_Connect(server, portnumber)
   char *server;
   int portnumber;
{
	int  n;
	int status;
	char buf[256], state[80], txtbuf[256];
	struct sockaddr_in sin;
	struct hostent *hp;
	struct servent *sp;

/************************************************************************
 *               Get the IP address of the rtc_ server host              *
 ************************************************************************/

	hp = gethostbyname(server);
	if (hp == NULL) {
		printf("[RTC_Connect] host unknown: %s\n", server);
		return(-1);
	}

/************************************************************************
 *  Create an IP-family socket on which to make the connection          *
 ************************************************************************/


	nsocket = socket(hp->h_addrtype, SOCK_STREAM, 0);
	if (nsocket < 0) {
		printf ("[RTC_Connect] error issuing socket command\n");
		socket_perror("rtc_client: socket");
		return(-1);
	}

/************************************************************************
 *  Create a "sockaddr_in" structure which describes the remote         *
 *  IP address we want to connect to (from gethostbyname()) and         *
 *  the remote TCP port number (from getservbyname()).                  *
 ************************************************************************/

	sin.sin_family = hp->h_addrtype;
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
/*	sin.sin_port = htons(7032);  */
/*	sin.sin_port = htons(500);   */
	sin.sin_port = htons(portnumber);  

/************************************************************************
 *  Connect to that address...                                          *
 ************************************************************************/

   printf ("[RTC_Connect] Connecting to host\n");
#ifdef USE_MULTINET_V34_ARGS
   if (connect(nsocket, &sin, sizeof (sin), 0) < 0) {
#else
   if (connect(nsocket, (struct sockaddr *) &sin, sizeof (sin)) < 0) {
#endif
      printf("[RTC_Connect] error issueing connect command, port=%d\n", portnumber);
      socket_perror ("[RTC_Connect] unable to connect error");
      return(-1);
   }

/************************************************************************
 * Get response from server                                             *
 ************************************************************************/


	n = socket_read(nsocket, buf, sizeof(buf));
	if (n>0)
	{
		status = 0;
 
	}
        if (n == 0) {
		status = -1;
		socket_perror("rtc_client: socket read responce");
                printf("rtc_client: n=%d nsocket=%8.8Xx\n", n, nsocket);
	}
	if (n < 0)
        {
		status = 0;
                printf("rtc_client: Trouble? n=%d nsocket=%8.8Xx\n", n, nsocket);
	}

   return(status);
}

int Rtc_GetPacket(databuf, size)
   char *databuf;
   long *size;
{
/* NOTE:  this routine ignores the "size" argument! */
	long nbytes;
	long count;
	unsigned long total;
	long readcount, remain_tot;
	char *txtbuf[132];
	char *puc;
  
  	count = total = 0;
	nbytes = 0;
	nbytes = socket_read(nsocket, &total, sizeof(total));
#ifndef USE_SUN
	puc = (char *) &total;
        Reverse_bytes(puc, nbytes); /* convert from IEEE to VAX */
#endif
	if (nbytes <= 0) 
	{
		socket_perror("rtc_getpacket: socket_read (1)\n");
		return(nbytes);
	}


	remain_tot = total;
        nbytes = 0;
	do 
	{
		if (remain_tot >= 6144)
		{
			readcount = 6144;
		} else {
			readcount = remain_tot;
		}
		count = socket_read(nsocket, databuf+nbytes, readcount);
		if (count <= 0)
		{
		   socket_perror("rtc_getpacket: socket_read (2)\n");
		}
		nbytes += count;
		remain_tot = total - nbytes;
	} while (nbytes < total);
 	return (nbytes);
}

int Rtc_LoginUser(username, password)
     char *username;    /* remote user name */
     char *password;    /* remote user password */
{
  int state = stateSENDUSER;
  int done = 0;
  int code,  n, status;
  char command[256],buf[256];
  char txtbuf[256];
  
  memset(txtbuf, '\0', sizeof(txtbuf));


/***********************************************************************
 *                       Repeat until done:                            *
 ***********************************************************************/

  while (!done) 
  {
       switch (state) 
       {
            case stateSENDUSER:      /* send USER command */
                 sprintf(command, "USER:%s\0", username);
                 if (socket_write(nsocket, command, sizeof(command)) <= 0) 
                 {
		      socket_perror("rtc_loginuser: write (1)\n");
        	      memset(command, '\0', sizeof(command));
        	      state = stateERROR;
			printf ("[RTC_LoginUser] error sending username\n");
        	      continue;
                 } else {
        	      memset(command, '\0', sizeof(command));
        	      state = stateWAITUSER;
        	      continue;
                 }
                  
            case stateSENDPASS:      /* send PASS command */
                 sprintf(command, "PASS:%s\0", password);
                 if (socket_write(nsocket, command, sizeof(command)) <= 0) 
                 {
		      socket_perror("rtc_loginuser: write (2)\n");
        	      state = stateERROR;
			printf ("[RTC_LoginUser] error sending password\n");
        	      continue;
                 } else {                          
        	      state = stateWAITPASS;
        	      continue;
                 }
              
            case stateWAITUSER:      /* wait for reply to USER command */
		 memset (buf, '\0', sizeof(buf));
                 n = socket_read(nsocket, buf, sizeof(buf));
                 if (n <= 0) 
                 {
		      socket_perror("rtc_loginuser: read (3)\n");
		      sprintf (txtbuf, "[RTC_LoginUser] error from socket_read\n");
		      printf ( "%s\n",txtbuf);
        	      state = stateERROR;
               	      continue;
                 } else {
                      state = stateSENDPASS;
                      continue;
                 }
        
            case stateWAITPASS:      /* wait for reply to PASS command */
		 memset (buf, '\0', sizeof(buf));
                 n = socket_read(nsocket, buf, sizeof(buf));
                 if (n <= 0) 
                 {
		      socket_perror("rtc_loginuser: read (4)\n");
		      sprintf (txtbuf, "[RTC_LoginUser] error from socket_read\n");
        	      state = stateERROR;
               	      continue;
                 } else {
        	      memset(txtbuf, '\0', sizeof(txtbuf));
                      state = stateWAITLOGIN;
                      continue;
                 }
        
            case stateWAITLOGIN:
                 sprintf(command, "WTG\0");

                 if (socket_write(nsocket, command, sizeof(command)) <= 0)
                 {
		      socket_perror("rtc_loginuser: write (5)\n");
		      sprintf (txtbuf, "[RTC_LoginUser] error from socket_write\n");
                      state = stateERROR;
        	      continue;
                 }
		 memset (buf, '\0', sizeof(buf));
                 n = socket_read(nsocket, buf, sizeof(buf));
                 if (n <= 0) 
                 {
		      socket_perror("rtc_loginuser: read (6)\n");
		      sprintf (txtbuf, "[RTC_LoginUser] error from socket_read\n");
        	      state = stateERROR;
               	      continue;
                 } else {

        	      memset(txtbuf, '\0', sizeof(txtbuf));
		      if (buf[0] == 'A') {
                           state = stateSUCCESS;
                      } else {
                           state = stateERROR;
                      }
                      continue;
                 }
        
            case stateSUCCESS:
                 done = 1;
                 break;

            case stateFAILURE:
                 done = 1;
                 break;
              
            case stateERROR:
                 done = 1;
                 break;
       }                 /* switch state */
  }                /* while not done */
     
/***********************************************************************
 *                     Done... return appropriately:                   *
 ***********************************************************************/

  if (state == stateSUCCESS) 
  {
       status = 0;
  } else {
       status = -1;
  }

  return (status);
}
                                               	
int Rtc_SetPacket(packet_types, Rtc_PacketStatus)
     char *packet_types;       /* packet types to receive  */
     char *Rtc_PacketStatus;    /* status of packet request */
{
  int state = stateSENDPACK;
  int done = 0;
  int code,n;
  char command[80],buf[256], txtbuf[256];
  
  
  memset (Rtc_PacketStatus, '\0', sizeof(Rtc_PacketStatus));

/*************************************************************************
 *                       REPEAT UNTIL DONE                               *
 *************************************************************************/

  while (!done) 
  {
    switch (state) 
    {

       case stateSENDPACK:      /* send PACK command */
     
            sprintf(command, "SETP %s\0", packet_types);
            if (socket_write(nsocket, command, strlen(command)) <= 0) 
	    {
		 socket_perror("rtc_setpacket: write (1)\n");
                 memset(command, '\0', sizeof(command));
	         state = stateERROR;
	         continue;
            } else {
	         memset(command, '\0', sizeof(command));
	         state = stateWAITPACK;
	         continue;
            }
      
      
      
       case stateWAITPACK:      /* wait for reply to PACK command */
            memset(buf, '\0', sizeof(buf));
            n = socket_read(nsocket, buf, sizeof(buf));
            if (n <= 0) {
		 socket_perror("rtc_setpacket: read (2)\n");
	        state = stateERROR;
       	        continue;
            } else {
		sprintf (txtbuf, "[RTC_SETPACKET] stateWAITPACK: %s\n", buf);
		memset (txtbuf, '\0', sizeof(txtbuf));
                state = stateWAITDB;
                continue;
            }

      
       case stateWAITDB:
            sprintf(command, "WTG\0");
            if (socket_write(nsocket, command, strlen(command)) <= 0) {
		 socket_perror("rtc_setpacket: write (3)\n");
	          memset(command, '\0', sizeof(command));
	          state = stateERROR;
	          continue;
            }
	          memset(buf, '\0', sizeof(buf));
                  n = socket_read(nsocket, buf, sizeof(buf));

                  if (n == 0) {
		        socket_perror("rtc_setpacket: read (4)\n");
	                state = stateERROR;
       	                continue;
                  } else {                                    
			sprintf(txtbuf,"[RTC_SETPACKET] stateWAITDB: %s\n",buf);
			memset (txtbuf, '\0', sizeof(txtbuf));
                        if (strncmp(buf,"ACK",3)==0) 
                        {
                              state = stateSUCCESS; /* SUCCESS; */
                        }
                    else {
     
                        state = stateDENIED;
                  }
                  continue;
            } 

       case stateWAITSYS:
            sprintf(command, "SYS\0");
            if (socket_write(nsocket, command, strlen(command)) <= 0) {
		 socket_perror("rtc_setpacket: write (5)\n");
	          memset(command, '\0', sizeof(command));
	          state = stateERROR;
	          continue;
            }
                  n = socket_read(nsocket, buf, sizeof(buf));

                  if (n <= 0) {
		        socket_perror("rtc_setpacket: read (6)\n");
	                state = stateERROR;
       	                continue;
                  } else {
			sprintf (txtbuf, "[RTC_SETPACKET] stateWAITSYS: %s\n", buf);
		        fwrite(txtbuf, strlen(txtbuf), 1, stdout);
			memset (txtbuf, '\0', sizeof(txtbuf));
                        if (strncmp(buf,"ACK",3)==0) 
                        {
                             state = stateSUCCESS; 
                        }
                    else {
     
                        state = stateDENIED;
                  }
                  continue;
            } 
      
       case stateSYSTEM:
            sprintf(command, "SYSACK\0");

            if (socket_write(nsocket, command, strlen(command)) <= 0) 
            {
		 socket_perror("rtc_setpacket: write (7)\n");
	          memset(command, '\0', sizeof(command));
	          state = stateERROR;
	          continue;
            }
                  n = socket_read(nsocket, buf, sizeof(buf));

                  if (n <= 0) 
                  {
		 	socket_perror("rtc_setpacket: read (8)\n");
	                state = stateERROR;
       	                continue;
                  } else {                                    
			sprintf (txtbuf, "[RTC_SETPACKET] stateSYSTEM: %s\n", buf);
		        fwrite(txtbuf, strlen(txtbuf), 1, stdout);
			memset (txtbuf, '\0', sizeof(txtbuf));
                        if (strncmp(buf,"ACK",3)==0) 
                        {
                             state = stateSUCCESS; 
                        } 
                  else {
                             state = stateSYSDOWN;
                  }
                  continue;
            } 
      
       case stateSYSDOWN:
            sprintf(Rtc_PacketStatus,"Remote System Not operational");
            done = 1;
            break;

       case stateSUCCESS:
            sprintf(Rtc_PacketStatus,"Accepted");
            done = 1;
            break;

       case stateDENIED:
            sprintf(Rtc_PacketStatus,"Denied");
            done = 1;
            break;

       case stateFAILURE:
      
       case stateERROR:
            done = 1;
            break;
    }                 /* switch state */
  }                /* while not done */
  
/*************************************************************************
 *              DONE... RETURN APPROPRIATELY                             *
 *************************************************************************/

  if (state == stateSUCCESS)
  {
        return 0;
  }

  if (state == stateDENIED) 
  {
        return -1;
  }

  return -1;
}
