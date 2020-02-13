C
c********** histogram of low freq powers
C
	IHIST = 1
C	IHIST = 0
	IF(IHIST.NE.1) GO TO 175
C
	histint = 1.
	nhistst = -10
	do irx = 1,4
	  do ifri = 1,14
	    ifr = fhist(ifri)
	    nhist = -dbspec(ifr+1,irx)
	    nhist = max0(nhist,nhistst)
	    nhist = min0(nhist,89)
C		range is -10 to 90 dB
	    nhist = nhist - nhistst + 1
	    hist(ifri,irx,nhist) = hist(ifri,irx,nhist) + 1.
          enddo
	enddo
c	write(69,69) no_evt(1),edist,sps,(dbspec(i,1),i=1,3),
c     1	 (dbspec(i,2),i=1,3),(dbspec(i,3),i=1,3),(dbspec(i,4),i=1,3)
c 69	format(i10,f6.1,f6.0,12f6.1) 
 175	CONTINUE
ct	IF(1) GO TO 100
C
c********** end histogram
C

