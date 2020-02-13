	subroutine makefile(ch)
c
C	MAKEFILE 14, A PROGRAM TO WRITE ORBIT PARAMETERS AND HOURLY AVR.
C
C	TO EXCLUDE UPSTREAM WAVES, USE PERIODS WHEN WIND IS MORE THAN
C		100 RE UPSTREAM
C		1996 MAY 19 TO AUG 12   =   5252  to  5336
C		1997 JAN 12 TO JUL 4    =   5490  to  5663
C		1997 NOV 2  TO FEB 20   =   5784  to  5893
C	OR, IF CLOSER, WHEN NO CONNECTION TO BOW SHOCK  (NOT DONE YET)
C
	integer*4 ch,ok,okt,OK1,OKE,SCETI4(2)	
	INTEGER*4 W_CHANNEL_CLOSE,W_EVENT,RET_SIZE,W_MESSAGES_OFF
	INTEGER*4 W_ITEM_I4,W_ITEM_R4,W_ITEM_R8
	REAL*8 SCET8,RGSE,DOY,SCET8SV
	REAL JY,JZ
	character*32 ITEM
	character*4 event
	DATA NERR /0/
	DATA RE /6.378E3/
	DATA TWOPI /6.2831853/
	data event /'CDF'/
C
C
	OKT = W_MESSAGES_OFF(ch)
	XMU0 = 2.*TWOPI*1.E-7
	MINSV = 0
	NBMAG = 0
	NDENS = 0
	NV = 0
	NTIME = 0
	BMAGT = 0.
	AZ = 0.
	DENST = 0.
	VXSW = 0.
	BXT = 0.
	BYT = 0.
	BZT = 0.
	BXSTD = 0.
	BYSTD = 0.
	BZSTD = 0.
	CUR2 = 0.
	DOY = 0.D00
C
 100	OKE = w_event(ch,event)
C
C	CHECK FOR END OF RECORD
C
	if (oke.eq.82) then
	  go to 200
	endif
C
C	CHECK FOR ERRORS
C
	if (oke.ne.1) then
		write(6,*) 'cannot open ',event, ', ok=', oke
		NERR = NERR + 1
	        IF(NERR.LT.10) GO TO 100
		RETURN
        ELSE
	   	NERR = 0
	endif
C
	ITEM = 'EVENT_SCET'
	ok = W_ITEM_I4(ch, item, SCETI4, 2, ret_size)
C
C	CHECK THAT IT IS NOT LEFT OVER FROM PREVIOUS DAY
C	IF(SCETI4(2).GT.120000) GO TO 100
C
	ITEM = 'WIND_ORBIT_X(GSE)_R8'
	ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	XGSE = RGSE/RE
	ITEM = 'WIND_ORBIT_Y(GSE)_R8'
	ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	YGSE = RGSE/RE
	ITEM = 'WIND_ORBIT_Z(GSE)_R8'
	ok = W_ITEM_R8(ch, item, RGSE, 1, ret_size)
	ZGSE = RGSE/RE
	ITEM = 'EVENT_SCET_R8'
	OK = W_ITEM_R8(ch, item, SCET8, 1, ret_size)
C
	RRE = SQRT(XGSE**2 + YGSE**2 + ZGSE**2)
C
	ITEM = 'WIND_MFI_BX(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BX, 1, ret_size)
	ITEM = 'WIND_MFI_BY(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BY, 1, ret_size)
	ITEM = 'WIND_MFI_BZ(GSE)_R4'
	ok = W_ITEM_R4(ch, item, BZ, 1, ret_size)
C	ITEM = 'WIND_MFI_BPHI(GSE)_R4'
C	ok = W_ITEM_R4(ch, item, AZMAG, 1, ret_size)
C	ITEM = 'MAG_ELEVATION'
C	ok = W_ITEM_I4(ch, item, MAGEL, 1, ret_size)
C	ITEM = 'SUN_ANGLE'
C	ok = W_ITEM_I4(ch, item, SUNCLOCK, 1, ret_size)
C	ITEM = 'WIND_3DP_E_TEMP_R4'
C	ok = W_ITEM_R4(ch, item, TEMPE, 1, ret_size)
C	ITEM = 'WIND_3DP_ION_TEMP_R4'
C	ok = W_ITEM_R4(ch, item, TEMPI, 1, ret_size)
C
	ITEM = 'WIND_3DP_ION_DENSITY_R4'
	ok = W_ITEM_R4(ch, item, DENS, 1, ret_size)
	ITEM = 'WIND_3DP_ION_VX(GSE)_R4'
	ok = W_ITEM_R4(ch, item, VX, 1, ret_size)
C
C	ITEM = 'WIND_3DP_ION_VY(GSE)_R4'
C	ok = W_ITEM_R4(ch, item, VY, 1, ret_size)
	ITEM = 'WIND_MFI_BMAG_R4'
	ok = W_ITEM_R4(ch, item, BMAG, 1, ret_size)
C
C	CHECK FOR END OF HOUR
C
	  MINUTE = MOD(SCETI4(2),10000)
	  IF(MINUTE.LT.MINSV) GO TO 200
	  MINSV = MINUTE
C
	  NBMAG = NBMAG + 1
	  NDENS = NDENS + 1
	  NV = NV + 1
	  NTIME = NTIME + 1
	  BMAGT = BMAGT + BMAG
	  DENST = DENST + DENS
	  VXSW = VXSW + VX
	  BXT = BXT + BX
	  BYT = BYT + BY
	  BZT = BZT + BZ
	  IF(NBMAG.GT.1) THEN
	    DELTA_T = 8.64D04*(SCET8-SCET8SV)
	    DELTA_X = -(VX*1000.)*DELTA_T
C	    JY = -(BZ-BZSV)/XMU0/DELTA_X
C	    JZ = (BY-BYSV)/XMU0/DELTA_X
	    JY = -1.E-9*(BZ-BZSV)/XMU0/DELTA_X
	    JZ =  1.E-9*(BY-BYSV)/XMU0/DELTA_X
	  ENDIF
	  BXSTD = BXSTD + BX**2
	  BYSTD = BYSTD + BY**2
	  BZSTD = BZSTD + BZ**2
	  CUR2 = CUR2 + JY**2 + JZ**2
	  SCET8SV = SCET8
	  BYSV = BY
	  BZSV = BZ
	  DOY = DOY + SCET8 
	  GO TO 100
C
 200	continue
C
C	CALCULATE HOURLY AVERAGES
C
	  BMAGT = BMAGT/(NBMAG + 1.E-8)
	  DENST = DENST/(NDENS + 1.E-8)
	  VXSW = VXSW/(NV + 1.E-8)
	  VXSW = -VXSW
	  BXT = BXT/(NBMAG + 1.E-8)
	  BYT = BYT/(NBMAG + 1.E-8)
	  BZT = BZT/(NBMAG + 1.E-8)
	  BXSTD = BXSTD/(NBMAG + 1.E-8) - BXT
	  BYSTD = BYSTD/(NBMAG + 1.E-8) - BYT
	  BXSTD = BXSTD/(NBMAG + 1.E-8) - BZT
	  CUR = 0.
	  IF(NBMAG.GT.1) CUR = SQRT(CUR2/(NBMAG-1))
	  VARB = BXSTD + BYSTD + BZSTD
	  VARB = SQRT(AMAX1(VARB,0.))
C	  ESTIMATED CURRENT DENSITY
	  ESTJ = 0.
	  DOY = DOY/(NTIME+1.D-08) - 6208.D00		! DOY 1999
	  AZ = ATAN2D(BYT,BXT)
C	  PRINT 1078, DOY,SCETI4,XGSE,YGSE,ZGSE,BMAGT,AZ,DENST,VXSW,VARB
	  WRITE(78,1078) DOY,SCETI4,XGSE,YGSE,ZGSE,BMAGT,AZ,DENST,VXSW,VARB
     1		,CUR
 1078	  FORMAT(F8.3,I10,I8,3F8.2,F6.1,F6.0,F7.2,F7.1,F7.1,E11.3)
	  MINSV = MINUTE
	  NBMAG = 1
	  NDENS = 1
	  NV = 1
	  NTIME = 1
	  BMAGT = BMAG
	  DENST = DENS
	  VXSW = VX
	  BXT = BX
	  BYT = BY
	  BZT = BZ
	  BXSTD = BX**2
	  BYSTD = BY**2
	  BZSTD = BZ**2
	  CUR2 = 0.
	  DOY = SCET8
	  if(oke.eq.82) then
	    ok = w_channel_close(ch)
	    return
	  else
	    go to 100
	  endif
	end
