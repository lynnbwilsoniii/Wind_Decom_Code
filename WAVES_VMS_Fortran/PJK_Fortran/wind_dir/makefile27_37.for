	subroutine makefile(ch)
c
C	THIS IS MAKEFILE27
c	copied from makefile4, to try to find ion acoustic waves
c		like Thejappa found, in FFTH data.
c		Examples are: 	2000/11/08 23:58:58
c				2001/08/12 18:07:55
c				2000/03/04 after 1200
c
	integer*4 ch,ok,okt,OK2,SCETI4(2),NDATA(1025)
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	INTEGER*4 NHICOUNT(2)
	REAL*8 SCET8
	REAL DBSPEC(1024),VDATA(1024),POWERHI(20,2),THRESH(20,2)
	REAL WSPEC(1024),SPECSAVE(10,2),SIGMAX(2),WT(513)
	character*32 ITEM
	character*4 event
c
c
c	    type*,'type name of data file, eg: makefile20.results'
c	    read(5,1002) eventfile
c	    open(unit=44,file=eventfile,status='old',readonly)
c	    read(44,1044) scetr,nevtr,evtr,junk
C		FOR FOR070.DAT, RESULTS OF MAKEFILE
C	    read(44,1044) scetr,nevtr,ntmday,evtr,junk
C		FOR MAKEFILE18.RESULTS
C	READ(44,1111,END=200) SCETR,NEVTR,EVTR,ISPS,IZCNT,FREQHZ
C     1	 ,FAVR,FREQ,BWAVR,FRBW,FP_3DP,EMAX,XRE,YRE,ZRE,XYPH,PANGLE,RATIO
C	1111 IS NOW 114 CHARACTERS
 1111	FORMAT(I10,I8,I10,A2,I2,I4,3F7.0,F7.2,F6.2,F7.0,F6.1,3F7.1,2F7.1
     1		,F6.3)
C
C		FOR MAKEFILE24.RESULTS, AND FOR058 FROM PROCMK18
C
c	READ(44,1114,END=200) SCETR,NEVTR,NTMDAY,EVTR
C	READ(56,1114,END=200) SCETR,NEVTR,NTMDAY,EVTR,ISPS,IZCNT,FREQHZ
C     1	 ,FAVR,FREQ,BWAVR,FRBW,FP_3DP,EMAX,XRE,YRE,ZRE,XYPH,PANGLE,RATIO
C	1114 IS NOW 113 CHARACTERS
 1114	FORMAT(I10,I7,I10,I3,A2,3F7.0,F7.2,F6.2,F7.0,F6.1,3F7.1,2F7.1
     1		,F6.3)
c
C	these thresholds are from 2000/03/04 120442, and are for 	
C	   nch=10, nlowlim = 22
	DATA THRESH /-154., -158., -159., -159., -160., -161., -162.,
     1     -162., -162., -163., 10*0.,
     2     -141., -143., -146., -147., -150., -151., -151., -152.,
     2	   -153., -153., 10*0./
	DATA WT /0.,512*1./
	data event /'FFTH'/
C
C	ELIMINATE SOME HARMONICS OF 600 HZ INTERFERENCE
	WT(30) = 0.
	WT(31) = 0.
	WT(32) = 0.
	WT(59) = 0.
	WT(60) = 0.
	WT(61) = 0.
	WT(90) = 0.
	WT(91) = 0.
	WT(92) = 0.
C
C	SEARCH FOR SIGNAL, USING FREQUENCIES FROM NLOWLIM UP.  AS THE 
C	EXPECTED SIGNALS ARE FAIRLY BAND, SUM OVER NCH FFT FREQUENCIES.
C
	NLOWLIM = 22
	NCH = 10
	NCHWDTH = (513-NLOWLIM)/NCH
	NCHWDTH = NCHWDTH/2 + 1
C	NCH AND NCHWDTH ARE DOUBLED LATER, TO GET 50% OVERLAP BETWEEN 
C	CHANNELS.  THEN AN EVENT HAS BEEN FOUND IF THE SIGNAL IS
C
 100	ok = w_event(ch,event)
	if (ok.ne.1) then
		if(ok.eq.82) then
		   ok = w_channel_close(ch)
		   return
	        endif
		write(6,*) 'cannot open ',event, ', ok=', ok
	endif
C
c	OKT = W_MESSAGES_OFF(ch)
C
C	SOME CHECKING STUFF
C	ITEM = 'EX_LENGTH_EFF'
C	okt = W_ITEM_R4(ch, item, EFFLEN, 1, ret_size)
C	PRINT*,'X LEN',EFFLEN
C	ITEM = 'EY_LENGTH_EFF'
C	okt = W_ITEM_R4(ch, item, EFFLEN, 1, ret_size)







C	PRINT*,'Y LEN',EFFLEN
C	ITEM = 'EZ_LENGTH_EFF'
C	okt = W_ITEM_R4(ch, item, EFFLEN, 1, ret_size)
C	PRINT*,'Z LEN',EFFLEN
C
c	ITEM = 'DATA'
c	ok2 = W_ITEM_I4(ch, item, NDATA, 1024, ret_size)
c
        call w_item_i4(ch,'PACKET_SUBTYPE',my_subtype,1,n)
c        if (my_subtype .eq. 1) then
c            raw = .true.
c        else
c            raw = .false.
c        endif
C
	IF(my_subtype.EQ.1) THEN
	  IRAW = 1
	ELSE
	  IRAW = 0
	ENDIF
C
	item = 'EVENT_SCET'
	ok = W_ITEM_I4(ch, item, scetI4, 2, ret_size)
	ITEM = 'SOURCE'
	okt = W_ITEM_I4(ch, item, ISRC, 1, ret_size)
C	ITEM = 'SPECTRUM_DB'
C	ok2 = W_ITEM_R4(ch, item, WSPEC, 513, ret_size)
	IF(ISRCSV.EQ.1.AND.ISRC.NE.2) GO TO 100
	ISRCSV = ISRC
	call fft_phys(ch,iraw,vdata,wspec)
C
C	print*,'WSPEC,size,1st vals',ret_size,wspec(2),wspec(3),wspec(30)
c	IF(ISRC.LT.4.OR.ISRC.GT.7) GO TO 100		! SHOULD NOT HAPPEN
	ITEM = 'CHANNEL_NUMBER'
	okt = W_ITEM_I4(ch, item, ICHNNL, 1, ret_size)
C	print*,'src,ch,raw',isrc,ichnnl,iraw
C
C	print*,'dbSPEC,iraw,1st vals',iraw,dbspec(2),dbspec(3),dbspec(30)
C
C	print*,'source,channel,iraw',isrc,ichnnl,iraw
C
	 N1 = NLOWLIM
	 DO N = 1,2*NCH
	     N2 = N1 + 2*NCHWDTH
	     N2 = MIN0(N2,513)
	     COUNT = 1.E-10
	     POWERHI(N,ISRC) = 0.
	     DO NF = N1,N2
	       IF(NF.LE.513) THEN
	         POWERHI(N,ISRC) = POWERHI(N,ISRC) + WT(NF)*WSPEC(NF)
	         COUNT = COUNT + WT(NF)
	       ENDIF
	     ENDDO
	     POWERHI(N,ISRC) = POWERHI(N,ISRC)/COUNT
	     N1 = N1 + NCHWDTH
	 ENDDO
C
C	PRINT*,ISRC,SCETI4,N2,(POWERHI(N,ISRC),N=1,NCH)
	NHICOUNT(ISRC) = 0
	SIGMAX(ISRC) = 0.
	DO N = 1,NCH
	  SIG = POWERHI(N,ISRC) - THRESH(N,ISRC)
	  IF(SIG.GT.5.) THEN
	    NHICOUNT(ISRC) = NHICOUNT(ISRC)+1
	    SIGMAX(ISRC) = AMAX1(SIG,SIGMAX(ISRC))
	  ENDIF
	ENDDO
	IF(ISRC.EQ.2) THEN
	  IF(NHICOUNT(1).GE.1.AND.NHICOUNT(2).GE.1.AND.NHICOUNT(1).
     1		LT.6.AND.NHICOUNT(2).LT.6) THEN
	   PRINT*,SCETI4,NHICOUNT(1),NHICOUNT(2),SIGMAX(1),SIGMAX(2)
	  ENDIF
	ENDIF
	IF(OK.NE.82) GO TO 100
C
C
C	IF(ICHNNL.EQ.6) THEN
C	IF(POWERHI.GT.-1000.) THEN
	
C	   write(s,'(i8.8,i6.6)',iostat=ios) s_scet(1), s_scet(2)
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)

C	  WRITE(88,1011) sceti4,(POWERHI(J),J=4,6)
C	  WRITE(90,1011) sceti4,(SPECSAVE(J,1),SPECSAVE(J,2),J=4,6)
C	  IF(POWERHI(4).GT.-136..AND.POWERHI(5).GT.-114.) THEN
C	    print 1011,sceti4,POWERHI(J),J=4,6)
C	    WRITE(91,1011) sceti4,(POWERHI(J),J=4,6)
C	  ENDIF
C	ENDIF
 1011	format(1X,I10,I8.6,6F10.3)
	IF(OK.NE.82) GO TO 100
	return
	end

