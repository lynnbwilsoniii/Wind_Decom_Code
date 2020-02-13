	program shockfig
C
C	MAKES A FIGURE SHOWING SHOCK AND DIRECTION OF E
C
	DIMENSION PLDATA(200,2)
	DATA TWOPI /6.2831853/
	DATA RTA,RTB,RTC /  25.6, 25.6, 14.6/
	DATA ALPHA,BETA,GAMMA /0.,1.,0./
C	DATA X0,Y0,Z0 /14.,6.,0./
C	DATA X0,Y0,Z0 /-1.02,-24.03,-2.39/		! 1996 OCT 5
	DATA X1,Y1,Z1 /.31, 21.81,.34/			! 1996 NOV 16
	DATA X0,Y0,Z0 /.31,-21.81,.34/			! 1996 NOV 16 TOP V
C	DATA X0,Y0,Z0 /5.68,20.77,.38/			! 1996 NOV 15
	DATA RE /6.378E3/
C
C
	R02 = Y0**2 + Z0**2
	EXRATIO = (.5/RTC)*(X0 + SQRT(X0**2 + 
     1		4.*R02*(RTC/RTA)**2))
	print*,'inbound orb',x0,y0,z0,exratio
	RC = EXRATIO*RTC
	RA = EXRATIO*RTA
	RB = EXRATIO*RTB
C
	DELR = RA/20.
	DO N = 0,25
		RR = N*DELR
		XX = RC*(1. - (RR/RA)**2)
		PLDATA(N+1,1) = XX
		PLDATA(N+1,2) = SIGN(RR,Y0)
	ENDDO
C
C	CALCULATE MODEL SHOCK NORMAL
C
c	      SNX = 1./RTC
c	      SNY = 2.*Y0/EXRATIO/RTB**2
c	      SNZ = 2.*Z0/EXRATIO/RTA**2
c	      SNORM = SQRT(SNX**2 + SNY**2 + SNZ**2)
C	PRINT*,N,orbtime,SNORM,bMAG(N),BX(N)
c	      BDOTN = (BX(N)*SNX + BY(N)*SNY + BZ(N)*SNZ)/SNORM/BMAG(N)
C	PRINT*,N,RR(N),snx/SNORM,sny/SNORM,snz/SNORM
C	PRINT*,N,orbtime,bdotn,bx(n),by(n),bz(n)
C
	R02 = Y1**2 + Z1**2
	EXRATIO = (.5/RTC)*(X1 + SQRT(X1**2 + 
     1		4.*R02*(RTC/RTA)**2))
	print*,'outbound orb',x1,y1,z1,exratio
	RC = EXRATIO*RTC
	RA = EXRATIO*RTA
	RB = EXRATIO*RTB
C
	DELR = RA/20.
	NP = 0
	DO N = 26,51
		RR = NP*DELR
		XX = RC*(1. - (RR/RA)**2)
		PLDATA(N+1,1) = XX
		PLDATA(N+1,2) = SIGN(RR,Y1)
		NP = NP+1
	ENDDO
C
C	CALL MGPLOT(0,200,2,PLDATA)
	CALL MGPLOT(1,200,2,PLDATA)
	STOP
	END
	SUBROUTINE MGPLOT(IHARD,NR,NL,PDATA)
C
	DIMENSION PDATA(200,2)
	CHARACTER*40 CH(30)
C
	CH(1) = 'TERMINAL 3'
	IF(IHARD.EQ.1) CH(1) = 'PRINTER 1'
	CH(2) = 'ERASE'
	CH(3) = 'LINES 1 26'
	CH(4) = 'XCOLUMN 1'
	CH(5) = 'YCOLUMN 2'
	IF(IHARD.EQ.0) THEN
	  CH(6) = 'LOCATION 100. 1000. 80. 750.'
	ELSE
 	  CH(6) = 'LOCATION 950. 1839. 500. 3000.'
	ENDIF
	CH(7) = 'LIMITS -5. 20. -35. 35.'
	CH(8) = 'CONNECT'
	CH(9) = 'LINES 27 52'
	CH(10) = 'XCOLUMN 1'
	CH(11) = 'YCOLUMN 2'
	CH(12) = 'CONNECT'
	CH(13) = 'BOX'
	CH(14) = 'XLABEL X (RE, GSE)'
	CH(15) = 'YLABEL Y (RE, GSE)'
	CH(16) = 'RELOCATE -5. 0.'
	CH(17) = 'DRAW 20. 0.'
	CH(18) = 'RELOCATE 0. -35.'
	CH(19) = 'DRAW 0. 35.'
	CH(20) = 'ID'
	CH(21) = 'END'
      IF(IHARD.EQ.1) THEN
	CH(21) = 'HARDCOPY'
	CH(22) = 'END'
      ENDIF
	CALL MONGO(21+IHARD,CH,NR,NL,PDATA)
      RETURN
      END