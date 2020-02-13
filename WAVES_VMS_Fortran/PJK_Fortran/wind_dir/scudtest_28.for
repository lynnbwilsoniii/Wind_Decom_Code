	PROGRAM CALCARES
C
C	CALCULATES ANTENNA POTENTIAL AND RESISTANCE, TO BALANCE 
C		PHOTOEMISSION AGAINST ANTCURR.  Can be used with
C	        VARIOUS PHOTOEMISSION FORMULAE.  
C
	DATA FFACT /.137/  !FUDGE FACTOR TO GIVE PHOTEMITTING AREA,STEREO S/C
C	DATA FFACT /.261/  !FUDGE FACTOR TO GIVE PHOTEMITTING AREA,-Y ANT 35
C	DATA FFACT /.174/  !FUDGE FACTOR TO GIVE PHOTEMITTING AREA,-Z ANT 35
C	DATA FFACT /.318/  !FUDGE FACTOR TO GIVE PHOTEMITTING AREA, WIND Z
C	DATA FFACT /.177/  !FUDGE FACTOR TO GIVE PHOTEMITTING AREA, WIND S/C
C	DATA FFACT /2./	    !FUDGE FACTOR TO GIVE PHOTEMITTING AREA
C
C
	DATA PI /3.14159265/
	DATA EVME /.51E6/
	DATA QE /4.8E-10/            ! ELECTRON CHARGE IN ESU
	DATA QESI /1.60206E-19/        ! ELECTRON CHARGE IN SI
	DATA EOMSI /-1.75882E11/        ! E OVER M FOR ELECTRONS, SI UNITS
	DATA BOLTZK /1.38044E-23/       ! BOLTZMANN CONST IN SI
	DATA AION /1./                  ! ION MASS in AMU
	DATA CLIGHT /2.9979E10/
	DATA PHYSLEN,DIA /1000.,1./
	DATA DENSE,TE /7., 10./
C
C	ICS = 1 IS CYLINDER, 2 IS SPHERE
C
	ICS = 1
C
	VMTE = CLIGHT*SQRT(8.*TE/PI/EVME) 
	ANTAREA = PI*DIA*PHYSLEN		! IN CM^2
C
C	CALCULATE THERMAL ELECTRON CURRENT
C
	ANTAREA = 14.7E4		! STEREO SPACECRAFT
C
	FLUX = .25*DENSE*ANTAREA*VMTE       ! IN ELECTRONS/SEC, INDEP OF UNITS
C
C	ABOVE IN CGS, NOW DO IN MKS
C
C	  ANTENNA CAPACITANCE AND RESISTANCE
C
	ANTCURR = FLUX*QESI
C	ANTRES = TPHI1/ANTCURR
C	CALL CALCARES(ANTCURR,ANTAREA,ANTPOT,ANTRES)
	ANTCAP = (.241E-12)*PHYSLEN/(ALOG10(2.*PHYSLEN/DIA) - .4)
C
	ANTPOT = 0.
	print*,'dense,te,antcurr',dense,te,antcurr
	CALL SCUDDER(ANTPOT,CURDENS)
C	CALL PEDERSEN(ANTPOT,CURDENS)
C	CALL HINTER(ANTPOT,CURDENS)
	PCUR = FFACT*1.E-4*ANTAREA*CURDENS	
	DIFFSV = PCUR - ANTCURR
	DELTAV = .1
C
	DO N = 1,300
	  ANTPOT = DELTAV*N
	  CALL SCUDDER(ANTPOT,CURDENS)
	write(55,*) n, antpot, curdens
c	  CALL PEDERSEN(ANTPOT,CURDENS)
C	  CALL HINTER(ANTPOT,CURDENS)
	  PCUR = FFACT*1.E-4*ANTAREA*CURDENS	
	  DIFF = PCUR - ANTCURR
	print*,'antpot,pcur,diff',antpot,pcur,diff
	  IF(DIFF*DIFFSV.LE.0.) THEN		!ROOT FOUND
	    DIDV = (DIFF-DIFFSV)/DELTAV
	    ANTPOTT = ANTPOT 
	    IF(DIDV.NE.0) ANTRES = -1./DIDV
	    ANTPOTT = ANTPOT + DIFF*ANTRES
	    ANTPOT = ANTPOTT
	print*,'antpot,antres',antpot,antres
c	    STOP
	  ENDIF
	  DIFFSV = DIFF
	ENDDO
	STOP
	END
	SUBROUTINE HINTER(ANTPOT,CURDENS)
C
C	FROM HINTEREGGER, DAMON AND HALL, JGR 64, 961, 1959
C
C	DATA TPHI /3./		! HINTEREGGER GAVE ABOUT 1 EV
	DATA TPHI /1.3/		! HINTEREGGER GAVE ABOUT 1 EV
C
	CURDENS = 4.E-5*EXP(-ANTPOT/TPHI)		! IN AMP/M**2
	RETURN
	END
	SUBROUTINE PEDERSEN(ANTPOT,CURDENS)
C
C	FROM FORREST, QUOTING FROM SCUDDER ET AL
C
C	DATA TPHI /3./		! HINTEREGGER GAVE ABOUT 1 EV
	DATA TPHIA,TPHIB /2.,4./		! 
C
	CURDENS = 8.E-5*EXP(-ANTPOT/TPHIA) + 
     1		.3E-5*EXP(-ANTPOT/TPHIB)   !  IN AMP/M**2
	RETURN
	END
	SUBROUTINE CUBE(ANTPOT,CURDENS)
C
C	AN ATTEMPT TO MATCH ULYSSES AND WIND MEASUREMENTS
C
	DATA TPHI /3./		! HINTEREGGER GAVE ABOUT 1 EV
C
	CURDENS = 4.E-5*EXP(-(ANTPOT/TPHI)**2)		! IN AMP/M**2
	RETURN
	END
