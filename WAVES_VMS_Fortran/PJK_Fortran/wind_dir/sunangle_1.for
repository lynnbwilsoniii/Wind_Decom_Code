C
c	on 16 nov 1999 it was found that the sunclock is read 1024
c	samples after the end of the event, plus about 10.6 msec or
c	about 14 spin clock counts for message passing time
c
	print*,'spinrate,sunclock,sps',spinrate,sunclock,sps
	END_ANGLE =  -360.*(SUNCLOCK-14.)/4096. - 45. ! ANGLE SUN TO +EX AT END
	IF(END_ANGLE.LT.-180.) END_ANGLE = END_ANGLE + 360.
	IF(END_ANGLE.GT.180.)  END_ANGLE = END_ANGLE - 360.
	DANG = SPINRATE*360./SPS/TWOPI
	ST_ANGLE = END_ANGLE + 3072.*DANG  ! ANGLE SUN TO +EX AT START 16nov99
	END_ANGLE = END_ANGLE + 1024.*DANG
c	ST_ANGLE = END_ANGLE + 2048.*DANG	  ! ANGLE SUN TO +EX AT START
	print*,'start angle, end angle, dang',st_angle,end_angle,dang
C

