c	modification history:
c		2/5/02	replace obsolete version of sampex_timcon subr
c			with include statement pointing to version
c			on ULEIS workstation    /gm
c
c
c       The following are standard functions and subroutines used 
c       frequently by STEP data analysis programs
c
c       Contents:
c             subroutine convert_time(ptime,sktime,fktime)
c
c             integer function day_of_year(yy,mm,dd)
c
c             integer function IDAYS_PER_YEAR(IYEAR)
c
c             subroutine convert_to_sampextime(iyear,idoy,ihour,imin,iseconds,stime)
c
c             subroutine sampex_timcon(stime,yyyy,mo,da,hr,mn,se,doy,isotime)
c
c
c
c***************************************************
c
      subroutine convert_time(ptime,sktime,fktime)
c
c***************************************************
c     Takes the string ptime, containing the start and finish date and time
c     of the major frame, and creates two strings:
c     sktime is the start date and time
c     fk is the finish date and time
c
c     Revisions:           
c              10/6/95 by J. Dwyer  commented code
c
      character *40 ptime   ! input string
      character *24 sktime,fktime  ! output strings
c
c      iso_time_c24(1:24)='    -  -  T  :  :       '
c                          123456789012345678901234
c
      sktime(1:4)='19'//ptime(2:3)
      sktime(5:7)='-'//ptime(5:6)
      sktime(8:10)='-'//ptime(8:9)
      sktime(11:19)='T'//ptime(11:18)
      sktime(20:24)='     '
c
      fktime(1:4)='19'//ptime(21:22)
      fktime(5:7)='-'//ptime(24:25)
      fktime(8:10)='-'//ptime(27:28)
      fktime(11:19)='T'//ptime(30:37)
      fktime(20:24)='     '
c
      return
      end   ! end convert_time
c
c
c
c
c***************************************************
c
       integer function day_of_year(yy,mm,dd)
c
c***************************************************
c
c    Takes the date and returns the number of the day (1-365) in the year  
c    yy = year (e.g. 1995)
c    mm = month (1-12)
c    dd = day
c
c     Revisions:
c                10/6/95 by J. Dwyer  Changed from subroutine to function and
c                                                  commented code.
c    
c
      integer yy, mm, dd, mm0
      integer dm(12)/31,28,31,30,31,30,31,31,30,31,30,31/  ! days per month
c
      mm0 = mm-1  
      day_of_year = dd
      if (mm0.eq.0) return
      do 4000 i=1,mm0
        day_of_year = day_of_year + dm(i)
4000    continue
      if (mm0.ge.2 .and. mod(yy,4).eq.0)
     1           day_of_year = day_of_year + 1      
c
      return
      end  ! end day_of_year
c
c
c
c
c
c
c****************************************************
c
        integer function idays_per_year(iyear)
c
c****************************************************
c
c    	Function to return the number of days per year for the 
c	add mod test 5/8/08   /gm
c      iyear = year (e.g. 1995)
c
c      Revisions:
c
c
	      integer	iyear
c
   	   if (mod(iyear,4).eq.0) then
	     	 idays_per_year=366
   	  else
   	  	idays_per_year=365
	     end if
c
       return
       end   ! end idays_per_year
c
c
c
c
c*****************************************************
c	    
       subroutine convert_to_sampextime(iyear,
     1  idoy,ihour,imin,iseconds,stime)
c
c*****************************************************
c
c   	This subroutine accepts a year, day of year, hour of day,
c	   minutes, and seconds of day and converts these to a 
c	   sampex time (seconds since midnight 1 January 1992).
c
c					J. Mazur  3/25/94
c
c   	Revisions:
c                 10/6/95 by J. Dwyer added line  integer idays_per_year
c
c
	      INTEGER	IYEAR	! year
        INTEGER	IDOY	! day of year
	      INTEGER	IHOUR	! hour of day
	      INTEGER	IMIN	! minutes of day
        integer    idays_per_year
	      INTEGER*4	ISECONDS ! seconds of day
	      INTEGER*4	STIME	! sampex time
c
	      INTEGER*4	IYDUM	! year counter
	      INTEGER*4	IDAYS	! day counter
c			
c	First zero the day counter (for multiple
c	calls to convert_to_sampextime)
	      IDAYS=0
c
c	Find number of days since midnight 1 Jan 1992
	      IYDUM=1992
	      DO WHILE(IYDUM.LT.IYEAR)
	         IDAYS = IDAYS + IDAYS_PER_YEAR(IYDUM)
	         IYDUM = IYDUM + 1
	      END DO
c	Add up the days, but subtract 1 since the first day 
c	of 1992 was day 1, not day 0.
	      IDAYS = IDAYS + IDOY - 1
c
c	Get the number of seconds since midnight 1 Jan 1992
      	STIME = (IDAYS*86400) + (IHOUR*3600) + (IMIN*60)+ ISECONDS
c
	      RETURN
	      END  ! end convert_to_sampextime
	


