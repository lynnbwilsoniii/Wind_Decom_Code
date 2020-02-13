	SUBROUTINE BZGLITCH(CH,N1,NAS,N2,DATA)
C
	CHARACTER*32 ITEM
	INTEGER*4 IRX,ISPS,IFIL,CH
	REAL ANGTBL(710),BGDATA(710),ANG(2050),DATA(2050),BGNEW(710)
	integer*4 NTMDAT(710),NANG(710)
C
	DATA IBGDATA /0/
	DATA DA_STAND /.063958012/
	DATA TWOPI /6.28318531/
C
C	print*,'bzglitch called'
	item = 'SOURCE'
	ok = W_ITEM_i4(ch, item, IRX, 1, return_size)
	ITEM = 'RX_SPEED'
	OK = W_ITEM_I4(CH,ITEM,ISPS,1,RETURN_SIZE)
	ITEM = 'RX_SPEED_R4'
	OK = W_ITEM_R4(CH,ITEM,SPS,1,RETURN_SIZE)
	ITEM = 'RX_FILTER'
	OK = W_ITEM_I4(CH,ITEM,IFIL,1,RETURN_SIZE)
C	ITEM = 'RX_FILTER_R4'
C	OK = W_ITEM_R4(CH,ITEM,FILTER,1,RETURN_SIZE)
C
C	PRINT*,'in bzglitch, IRX,ISPS,IFIL=',IRX,ISPS,IFIL
	IF(IRX.NE.9.OR.ISPS.NE.3.OR.IFIL.NE.3) THEN
		PRINT*,'IN BZGLITCH, IRX,ISPS OR IFIL =', IRX,ISPS,IFIL
		RETURN
	ENDIF
C
	item = 'WIND_SPIN_RATE_R4'
	ok = w_item_R4(ch, item, SPINRATE, 1, return_size)
	item = 'SUN_ANGLE'
	ok = w_item_R4(ch, item, sunclock, 1, return_size)
C
C	  SEARCH FOR AND REMOVE BZ GLITCH
C
C	  print*,'in BZGLITCH spinrate,sunclock,sps',spinrate,sunclock,sps

C	print*,'spinrate,sunclock,sps',spinrate,sunclock,sps
	END_ANGLE =  -360.*(SUNCLOCK-14.)/4096. - 45. ! ANGLE SUN TO +EX AT END
	IF(END_ANGLE.LT.-180.) END_ANGLE = END_ANGLE + 360.
	IF(END_ANGLE.GT.180.)  END_ANGLE = END_ANGLE - 360.
	DANG = SPINRATE*360./SPS/TWOPI
	ST_ANGLE = END_ANGLE + 3072.*DANG  ! ANGLE SUN TO +EX AT START 16nov99
	END_ANGLE = END_ANGLE + 1024.*DANG
C	print*,'start angle, end angle, dang',st_angle,end_angle,dang
C
C	  END_ANGLE =  -360.*SUNCLOCK/4096. - 45.  ! ANGLE SUN TO +EX AT END
C	  DANG = SPINRATE*360./SPS/TWOPI
C	  ST_ANGLE = END_ANGLE + 2048.*DANG	 ! ANGLE SUN TO +EX AT START
C
	  IF(ST_ANGLE.LT.360.) ST_ANGLE = ST_ANGLE + 360.
	  IF(ST_ANGLE.GT.360.) ST_ANGLE = ST_ANGLE - 360.
C
C	  PKANG = 328.3     ! OLD VALUE BEFORE SUNCLOCK WAS UNDERSTOOD
	  PKANG = 35.45 
	  PKANG = 34.846 
C
C	NANG = MEAS IS AN INTEGER WHICH IS ANGLE/(ANGLE INCREMENT)
C		I.E. AN INTEGER WHICH DECREASES BY 1 PER SAMPLE
C
	  IF(IBGDATA.EQ.0) THEN
	    OPEN(UNIT=62,FILE='BZGLITCH.DAT',STATUS='OLD')
	    READ(62,*) NTIMES
	    PRINT*,'NO. OF GLITCHES AVERAGED',NTIMES
	    DO N = 1,703
	      READ(62,*),ANGT,MEAS,NFFDT,TDS,DIFF
	      ANGTBL(N) = ANGT
	      BGDATA(N) = TDS
	      BGNEW(N) = TDS
	      NANG(N) = MEAS
	      NTMDAT(N) = NFFDT
C	      IF(ABS(ANGT-PKANG).LE.(.5*DA_STAND)) NASBG = N
	    ENDDO
	    IBGDATA=1
	    CLOSE(UNIT=62)
	  ENDIF
C
C	  OPEN(UNIT=62,FILE='BZGLITCH.DAT',STATUS='new')
C	    NTIMES = NTIMES + 1
C	    write(62,*) ntimes,sunclock
C	    PRINT*,'BGDATA READ IN, LAST',N-1,ANGT,TDS
C	    print*,'peak in bgdata at sample no.',nasbg,' ang',angtbl(nasbg)
C
	  NAS = 0
	  PEAK = 0.
	  DO IK = 1,2048
	    ANGT = ST_ANGLE - (IK-1)*DANG
	    ANG(IK) = ANGT
	    IF(DATA(IK).GT.PEAK) THEN
	      PEAK = AMAX1(PEAK,DATA(IK))
	      NPK = IK  
	    ENDIF
	    IF(ANGT.LT.0.) ANGT = ANGT + 360.	
C		FIND SAMPLE NO OF MAIN PEAK FROM ANGLE
	    IF(ABS(ANGT-PKANG).LE.(.5*DANG)) NAS = IK
C	    WRITE(89,*) IK,NAS,ANGT,DATA(IK)
	  ENDDO
c
c	  if(nas.ne.0) then
c	    print*,'peak angle at sample no.',nas,' ang,val',
c     1		ang(nas),data(nas),peak
c	    print*,'peak value at sample no.',npk,' ang,val',
c     1		ang(npk),data(npk),peak
c	  else
c	    print*,'no peak, bzglitch'
c	  endif
C
C	RETURN IF THE DATA DO NOT INCLUDE THE GLITCH ANGLE
C
	IF(NAS.EQ.0) RETURN
C
C	  DETERMINE A MORE ACCURATE VALUE OF NAS = SAMPLE NO. OF PEAK
C
	NLSIDE = -1
	LUSIDE = -1
	TLEVEL = .5*PEAK
C
	DO IK = 1,2047
	  IF(DATA(IK+1).GT.TLEVEL.AND.DATA(IK).LE.TLEVEL) NLSIDE = IK
	  IF(DATA(IK+1).LT.TLEVEL.AND.DATA(IK).GE.TLEVEL) NUSIDE = IK+1
	ENDDO
	IK = NLSIDE
	ADDL = (TLEVEL-DATA(IK))/(DATA(IK+1)-DATA(IK))
	IK = NUSIDE
	ADDU = (TLEVEL-DATA(IK))/(DATA(IK-1)-DATA(IK))
	XMIDPT = .5*(NLSIDE+NUSIDE+ADDL-ADDU)
	NAS1 = XMIDPT + .5	
	IOFFS = NAS1-NAS
	NAS = NAS1
C	print*,'bg',nlside,addl,nuside,addu,xmidpt
C
C	print*,'bg',nlside,ang(nlside),nuside,ang(nuside),peak,nas
c	write(99,*)'bg',nlside,ang(nlside),nuside,ang(nuside),peak,nas
C
C	CALCULATE NEW AVERAGED DATA AND SUBTRACT FROM PRESENT DATA
C	  AS THERE MAY BE SOME ERROR IN ST_ANGLE, IT IS ASSUMED THAT
C	  THE PEAK OCCURS AT PKANG = 328.3 DEG., AND THAT NANG = MEAS
C	  IS 5133 AT THAT POINT, WHICH IS LINE 261 IN THE TABLE
C	  Nov 99 correction, peak occurs at 35. deg.
C
	  FACT = (NTIMES-1)/FLOAT(NTIMES)
	  CTRANG = ANG(NAS)
	  DO N = 1,2048
C	    NTBL = -(ANG(N) - CTRANG)/DA_STAND + 300.5
	    NTBL = -(ANG(N) - CTRANG)/DA_STAND + 260.5
	    IF(NTBL.GE.1.AND.NTBL.LE.703) THEN
	        BGNEW(NTBL) = DATA(N)/FLOAT(NTIMES) + 
     1			FACT*BGDATA(NTBL)
C		DATA(N) = DATA(N) - BGDATA(NTBL)
C		write(64,*) n,bgdata(ntbl),data(n),bgnew(ntbl),
C     1		data(n)-bgnew(ntbl)
		DATA(N) = DATA(N) - BGNEW(NTBL)
	    ENDIF
Cold	    IF(ANG(N).GT.300.AND.ANG(N).LT.345.) THEN
	    IF(NTBL.GE.1.AND.NTBL.LE.703) THEN
	      NW = NTBL
C	      WRITE(62,*) ANG(N),NTBL,ntmdat(nW),BGNEW(NW),DATA(N)
	    ENDIF
	  ENDDO
C	CLOSE(UNIT=62)
C
C	ENDIF
C
C	THE SUBTRACTION ABOVE STILL DID NOT REMOVE THE WORST PEAK, SO
C		DO AN INTERPOLATION ON THE VALUES FROM 39.5 DEG TO 32 DEG.
C		ANGLES CHANGED ON 23 FEB 2002		
C
C	NSTRT = -57
C	NNEND = 24
C	NSTRT = -63
C	NNEND = 54
C	NSTRT = -80
	NSTRT = -100
	NNEND = 24
	N1 = NAS+NSTRT
	N2 = NAS+NNEND
	IF(NAS.GT.-NSTRT.AND.NAS.LT.2048-NNEND) THEN
	  DSTART = DATA(NAS+NSTRT)
	  DEND = DATA(NAS+NNEND)
	  NTOT = NNEND - NSTRT
c	  WRITE(88,*) IOFFS,NAS,DSTART,DEND
	  DO N = 0,NTOT
	    DATATMP = DATA(NAS+NSTRT+N)            
C	    DATA(NAS+NSTRT+N) = DSTART + N*(DEND-DSTART)/NTOT
	    DATA(NAS+NSTRT+N) = 0.
C	    WRITE(88,*) N,NAS+NSTRT+N,DATATMP,DATA(NAS+NSTRT+N)
	  ENDDO
	ELSE
	    PRINT*,'GLITCH NEAR END'
	    N1 = MAX0(N1,1)
	    N2 = MIN0(N2,2048)
	ENDIF
	RETURN
	END