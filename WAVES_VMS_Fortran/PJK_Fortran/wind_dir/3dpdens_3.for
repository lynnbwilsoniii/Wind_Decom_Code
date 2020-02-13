	SUBROUTINE TDPDENS(YYYYMMDD,FDAY,FLUX,DENS,ETEMP)
C
C	RETURN ACCURATE DENSITIES FROM STUART BY INTERPOLATING ON HIS TABLE 
C
C	THERE ARE TWO VERSIONS OF THIS( NO ONLY  FLUX, NOT DENS). 
C	 THIS ONE RETURNS DENS AT
C		A CERTAIN TIME.  DENS_3DP (DOESNT  EXIST) RETURNS AN AVERAGE 
C		DENS AND VARIANCE FOR THE RESISTANCE PAPERS
C
	CHARACTER*50 FILENAME
	INTEGER DATE,YYYYMMDD,ANGLE
	DATA IOPEN/0/
	DATA FDAYSV /0./
C
C	DENS = 1.68E7*DENS
C	
C
	IF(IOPEN.NE.1) THEN
	  WRITE(FILENAME,123) YYYYMMDD
C 123	  FORMAT('MONTHLY:[KELLOGG.EDENS]DENS_',I8.8,'.ASC')	
 123	  FORMAT('USER_A:[KELLOGG.WIND.STUFF]DENS_',I8.8,'.ASC')	
	  OPEN(UNIT=83,FILE=FILENAME,STATUS='OLD',READONLY)
C	  READ(83,*,END=141,ERR=141) NYR,MO,NDAY,N4,N5,N6,EDENSSV
	  READ(83,*,END=141,ERR=141) SEC,EDENSV
	  FDAYSV = SEC/86400.
	  FDAYF = FDAYSV
	  IOPEN = 1
	ENDIF
C
	IF(FDAY.GE.FDAYF) THEN
 140	  CONTINUE 
c	  READ(83,*,END=141,ERR=140) NYR,MO,NDAY,N4,N5,N6,EDENS
c	  print*,'nyr,n4,n5,eDENS',nyr,n4,n5,eDENS
	  EDENSSV = EDENS
	  FDAYSV = FDAYF
	  READ(83,*,END=140,ERR=140) SEC,EDENS
c	  FDAYF = N4/24. + N5/1440. + N6/86400.
	  FDAYF = SEC/86400.
	  IF(FDAYF.LT.FDAY) GO TO 140
	ENDIF
 141	CONTINUE
	DENS = EDENS + (FDAY-FDAYF)*(EDENS-EDENSSV)/(FDAYF-FDAYSV)
	TE = 0.
	DENS = 0.
C	WRITE(72,1072) YYYYMMDD,DENS,SDENS,TE,STE,DENS,TEDENS,SEDENS,
C     1		DENS-TEDENS
C 1072	FORMAT(I9,F7.2,F7.3,F6.1,F6.2,E11.3,E11.3,E11.3,e11.3)
	print*,'tdpDENS',fday,sec,eDENSsv,DENS
	RETURN
 200	CONTINUE
C	PRINT*,'END OF DENS TABLE'
	RETURN
	END

