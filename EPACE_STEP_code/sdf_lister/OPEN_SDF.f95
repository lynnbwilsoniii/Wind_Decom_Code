c
      subroutine open_sdf(IYEAR,IDAY,IUNIT,FILE_EXTENT_PREFIX,IFLAG)
C	
C	Routine to open a STEP Data File (SDF) that resides on any
C	SDF volume in the VMS search list "sdfdir".
C
C	This subroutine reads through this listing, finds the requested
C	SDF (if it exists) and volume name, and opens the SDF using the
C	unit number IUNIT.
C
C					J. Mazur  6/1/93
C                                    Modified for STEP
C                                       E. Salter 2/1/95
C				     Modified to use VMS search list
C					J. Dwyer 9/29/95
c          J. Dwyer replaced write with print 11/21/95
C	
C	Input:
C		IYEAR	year of the SDF to open (e.g., 1992)
C		IDAY	day number of SDF to open
C		IUNIT	unit number for SDF
C		IFLAG	t/f flag that tells if this SDF exists
C		FILE_EXTENT_PREFIX  for input file 
C
C	Revisions
C	
C	6/8/93		Close ldf_locations.dat before each return.
C	6/24/93		Add 'action=¹read¹' to open statement.
C	9/29/95		No longer use sdf_locations.dat to find file
C			Use VMS search list.
c	28-Jul-2008	comment out open statements to keep down lenght of batch output files /gm
C
C________________________________________________________________________
c Local variables			
      CHARACTER*120	VOLUME_NAME	! full sdf volume name
      CHARACTER*4	YEAR		! character form of iyear
	    CHARACTER*3	DAY		! character form of iday
	    CHARACTER*3	FILE_EXTENT_PREFIX      ! (hex vs. xdr)
	    LOGICAL		IFLAG		! flag for sucessful file open 
	    LOGICAL 	ifile		! test for SDF inquire
	include 'include_paths.inc'

C				SEARCH SETUP
C				************
C	Write the requested day and year into character strings; check the day 
C	number to insure it has three characters (e.g., '003' instead of '  3')

	  do 200 i_version=9,1,-1
	write(volume_name, 100) adjustl(trim(data_path)), iyear, iday, file_extent_prefix,i_version
100	format(a,'sdf',i4.4,'_',i3.3,'.'a3,';',i1)
c	type *, ' attempting to open: ', adjustl(trim(VOLUME_NAME))

	         OPEN(UNIT=IUNIT, file=adjustl(trim(VOLUME_NAME)),
     *                status='old',action='read',err=200)
c           type *, 'OPEN_SDF: file ', adjustl(trim(VOLUME_NAME))
		       IFLAG=.TRUE.
 		      RETURN

200	continue

C	We get to here if we've read through the entire SDF listing
C	and never found a file that matched the year and day of the 
C	one we requested.


1000	  continue
c	type *,'SDF_OPEN: SDF for ',
c     1           IYEAR,' day ',IDAY,' is not in the '
c      type *,' folder ', data_path,  '. No file opened.'
      IFLAG=.FALSE.
      RETURN
	    END  ! end open_sdf_sl
c
