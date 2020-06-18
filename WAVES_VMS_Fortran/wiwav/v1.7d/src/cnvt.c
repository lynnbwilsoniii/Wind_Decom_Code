/* cnvt.c - converts old form of wind item database to new format
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

static FILE *widf;
static FILE *newf;
static int  data_file_line_number;
static int access_mode;

#define wid_file_name "wind_items.dat"
#define new_file_name "wind_items.new"

#define  sizeof_line       512
static char line[sizeof_line];

static void remove_whitespace( s )
   char *s;
{
   char *a, *b, *e;
   a = b = s;
   e = &s[strlen(s)];
   while (*b != '\0')
   {
      if (*b <= ' ') {b++; continue;}
      if (b != a) *a = *b;
      a++; b++;
   }
   *a = '\0';
   while (a < e) {a++; *a = '\0';}
   return;
}

main()
{
   char *ok;
   int line_count;
   int line_count2;
   int area='$';
   char *a, *b, *c, *d;
   char s[80];
   int i,j,k,n;
   int x1, x2, x3, x4, x5;

   /* open the existing item datafile */
   if ( (widf = fopen(wid_file_name,"r")) == NULL)
   {
      perror("FOPEN");
      printf("Cannot open %s.\n", wid_file_name);
      exit(2);
   }

   /* open the new format item datafile */
   if ( (newf = fopen(new_file_name,"w")) == NULL)
   {
      perror("FOPEN");
      printf("Cannot open %s.\n", new_file_name);
      exit(2);
   }


   line_count = 0;
   line_count2 = 0;
   while (NULL != (ok = fgets(line, sizeof_line, widf)))
   {
      line_count++;
      if (line[0] < 'A' || line[0] > 'Z')
      {
         if ( NULL != (a = strstr(line,"EXTRACT=")) && area != '$')
         {
            /* assume that only the EXTRACT= clause is on this line */
            remove_whitespace(a);
/*
printf("line:%d  a=%s$\n", line_count, a);
*/
            while (*a != '=') a++;
            a++;
            /* 1st arg */
            n = sscanf(a,"%d%n", &x1, &i);
            if (n != 1) printf(" 1ERR at line %d:%s\n", line_count, line);
            a = &a[i+1];
            /* 2nd arg */
            n = sscanf(a,"%d%n", &x2, &i);
            if (n != 1) printf(" 2ERR at line %d:%s\n", line_count, line);
            a = &a[i+1];
            /* 3rd arg */
            n = sscanf(a,"%d%n", &x3, &i);
            if (n != 1) printf(" 3ERR at line %d:%s\n", line_count, line);
            a = &a[i+1];
            /* 4th arg */
            n = sscanf(a,"%d%n", &x4, &i);
            if (n != 1) printf(" 4ERR at line %d:%s\n", line_count, line);
            /* 5th arg, if present */
            b = strstr(a,",");
            x5 = 0;
            if (b != NULL)
            {
               a = &b[1];
               if (*a != '\0')
               {
                  n = sscanf(a,"%d", &x5);
                  if (n != 1) printf(" 5ERR at line %d:%s\n", line_count, line);
               }
            }
/*
            printf( "\tEXTRACT=%d,%d,%d,%d,%d,AREA:%c\n",
                 x1, x2, x3, x4, x5, area);
*/
            fprintf(newf,"\t\tEXTRACT=%d,%d,%d,%d,%d,AREA:%c\n",
                 x1, x2, x3, x4, x5, area);
         }
         else
         if ( NULL != (a = strstr(line,"FIXED_VALUE=")) )
         {
            /* separate the count from the value list with a colon */
            b = strstr(a,",");
	    *b = ':';
            /* trim off the trailing commas */
            while (*b > ' ') b++;
            b--;
            while (*b == ',') {*b = ' '; b--;}
            /* output the new line */
            fprintf(newf,"\t");
            fputs(line, newf);
         }
         else
         {
            if ((area != '$') && (strlen(line) > 4)) fprintf(newf,"\t");
            fputs(line, newf);
         }
         line_count2++;
         continue;
      }


      fprintf(newf, "EVENT=%4.4s\n", &line[3]);
      fprintf(newf, "\tITEM=%16.16s\n", &line[8]);
      area = line[1];
      line_count2 = line_count2 + 3;

      /* validations */

      /* event subtype field */
      if (0 != strncmp("1111111111111111", &line[25], 16))
      {
         a = &line[25];
         line[25+16] = '\0';
         while (*a == '0') a++;
         fprintf(newf, "\t\tVALIDATION=PACKET_SUBTYPE,MO,b%s\n", a);
         line_count2++;
      }

      /* date start time */
      if (0 != strncmp("19910101", &line[42], 8))
      {
         fprintf(newf, "\t\tVALIDATION=EVENT_SCET,GT,DATE:%8.8s-%6.6s\n", 
             &line[42], &line[51]);
         line_count2++;
      }

      /* date end time */
      if (line[58] != '9')
      {
         fprintf(newf, "\t\tVALIDATION=EVENT_SCET,LT,DATE:%8.8s-%6.6s\n", 
             &line[58], &line[67]);
         line_count2++;
      }

      /* dpu version */
      if (line[77]=='F') line[77] = 'f';
      if (line[78]=='F') line[78] = 'f';
      if (!(line[74]=='0' && line[75]=='0' && line[77]=='f' && line[78]=='f'))
      {
         n = sscanf(&line[74],"%x %x", &i, &j);
         if (n != 2) printf("DPU ver read err, line=%d\n", line_count);
         if (i == j)
            fprintf(newf, "\t\tVALIDATION=DPU_VERSION,EQ,%d\n", i);
         else
         if (i == 0)
            fprintf(newf, "\t\tVALIDATION=DPU_VERSION,LE,%d\n", j);
         else
         if (j == 255)
            fprintf(newf, "\t\tVALIDATION=DPU_VERSION,GE,%d\n", i);
         else
            fprintf(newf, "\t\tVALIDATION=DPU_VERSION,GE,%d,LE,%d\n", i, j);

         line_count2++;
      }

      /* fft version */
      if (line[83]=='F') line[83] = 'f';
      if (line[84]=='F') line[84] = 'f';
      if (!(line[80]=='0' && line[81]=='0' && line[83]=='f' && line[84]=='f'))
      {
         n = sscanf(&line[80],"%x %x", &i, &j);
         if (n != 2) printf("FFT ver read err, line=%d\n", line_count);
         if (i == j)
            fprintf(newf, "\t\tVALIDATION=FFT_VERSION,EQ,%d\n", i);
         else
         if (i == 0)
            fprintf(newf, "\t\tVALIDATION=FFT_VERSION,LE,%d\n", j);
         else
         if (j == 255)
            fprintf(newf, "\t\tVALIDATION=FFT_VERSION,GE,%d\n", i);
         else
            fprintf(newf, "\t\tVALIDATION=FFT_VERSION,GE,%d,LE,%d\n", i, j);

         line_count2++;
      }

      /* tds version */
      if (line[89]=='F') line[89] = 'f';
      if (line[90]=='F') line[90] = 'f';
      if (!(line[86]=='0' && line[87]=='0' && line[89]=='f' && line[90]=='f'))
      {
         n = sscanf(&line[86],"%x %x", &i, &j);
         if (n != 2) printf("TDS ver read err, line=%d\n", line_count);
         if (i == j)
            fprintf(newf, "\t\tVALIDATION=TDS_VERSION,EQ,%d\n", i);
         else
         if (i == 0)
            fprintf(newf, "\t\tVALIDATION=TDS_VERSION,LE,%d\n", j);
         else
         if (j == 255)
            fprintf(newf, "\t\tVALIDATION=TDS_VERSION,GE,%d\n", i);
         else
            fprintf(newf, "\t\tVALIDATION=TDS_VERSION,GE,%d,LE,%d\n", i, j);

         line_count2++;
      }

   } /* while */

   printf("%d lines read from %s.\n", line_count, wid_file_name);
   printf("%d lines written to %s.\n", line_count2, new_file_name);
   exit(0);
} /* main */
