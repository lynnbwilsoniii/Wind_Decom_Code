	PROGRAM TDSCHECK
C
	COMPLEX FCOEF,CGAIN,CCORR
	COMMON /GAINBLK/ PHASE,CGAIN
	character*4 pa(9)
	REAL SPSF(4)
	data pa /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',' BX '
     1    ,' BY ',' BZ '/
	DATA SPSF /120.,30.,7.5,1.875/
	
C
 100	READ(5,*,END=200) ICH,IRX,FREQ,ISPS,IFIL,NDATA
	    DATA = TDSCAL(ICH,ISPS,NDATA+128)
C	print*,'data',data
		FCOEF = DATA
	        EJUNK = PREAMP(IRX,FREQ)
C	print*,ejunk,cgain
		FCOEF = FCOEF/CGAIN
		CCORR = CGAIN
		EJUNK = TDSDET(ICH,FREQ)
		FCOEF = FCOEF/CGAIN
		CCORR = CGAIN*CCORR
C		EJUNK = TDS_FILTER(TDS_CHANNEL,IFIL+1,FREQ)
C		FCOEFT = FCOEFT/CGAIN
C		CCORR = CGAIN*CCORR
	write(25,*) ICH,PA(IRX),FREQ,NDATA,FCOEF
	print*, ICH,PA(IRX),FREQ,NDATA,cabs(FCOEF)
	GO TO 100
 200	STOP
	END
