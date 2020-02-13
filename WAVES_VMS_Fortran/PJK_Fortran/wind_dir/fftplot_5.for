	SUBROUTINE SHPLOT
C
	COMMON /SHBLK/ DRVFRQ(2048),PWRL(256,511)
	COMMON /PLOTDT/ ITERM,NPT,FMIN,FMAX,NX1,NX2,NSIZE,AMP
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	CHARACTER*6 TITLE(4)
	CHARACTER*120 STR
C
C
	IF(NPT.LT.1) NPT = 1             	  ! protection
	PRINT*,' IN SHPLOT,FREQS',DRVFRQ(1),DRVFRQ(NPT),NPT
C	CALCULATE BOX SIZE
	PIXSIZ = .1
	IF(NPTH.GT.1) PIXSIZ = (DRVFRQ(NPT) - DRVFRQ(1))/(NPT-1)
	HSTART = DRVFRQ(1) - .5*PIXSIZ
	HEND =   DRVFRQ(NPT) + .5*PIXSIZ
	PRINT*,' IN SHPLOT',HSTART,HEND,NPT
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
C
C	CALCULATE MAX AND MIN, FOR GREY SCALING
C
	IF(NX1.EQ.1) THEN
	  DELX = 500.
	  XSTART = 500.
	  XEND = XSTART + DELX
	  YMIN = PWRL(1,1)
	  YMAX = YMIN
	  DO N = 1,NPT
	    DO J = 1,511
	        YMIN = AMIN1(YMIN,PWRL(N,J))
	        YMAX = AMAX1(YMAX,PWRL(N,J))
	    ENDDO
	  ENDDO
	ENDIF
C
	
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(XSTART,280.,XEND,1587.)
	ENDIF
	PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX
C	RAISING YMIN MAKES THE BACKGROUND LIGHTER
C	LOWERING YMAX MAKES THE SIGNAL DARKER
	PRINT*,'LO, YMIN,MAX SET TO',YMIN,YMAX
	CALL MGOHALFTONE(PWRL,NPT,511,YMIN,YMAX,1.E7)
	CALL MGOSETEXPAND(.7)
	CALL MGOSETLIM(FSTART,1.,FEND,511.)
	IF(NX2.EQ.NSIZE) THEN
          CALL MGOXLABEL(15,'INPUT HARMONIC')	
	  CALL MGOYLABEL(15,'OUTPUT HARMONIC')	
	  CALL MGOBOX(1,2)
	  CALL MGOSETEXPAND(1.)
	  CALL MGOPLOTID('FFTWIN','SHPLOT')
	  CALL MGOSETEXPAND(1.)
C
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                      ! maxch, crt
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
	ENDIF
C
	CALL MGOSETEXPAND(1.)
	IF(ITERM.LT.0) THEN
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	  NPLOTS = NPLOTS + 1
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
	END
	SUBROUTINE TPLOT(IFSTART)
C
	COMMON /TBLK/ DRVFRQ(1250),THL(300),PWRL(300,64),AVSH(1250,12)
	COMMON /PLOTDT/ ITERM,NPTH,NPTL,NFCLK,NFCLKS,HRFR,DELT,FLOLIM
     1    ,NRESOL
	COMMON /RADMODE/ LISDRVFRQ,LISTLO,LOSTRT,IHISTR,LOLAST,IHILST,NMODE
	COMMON /LL/LOLIST(16,16)            ! 1ST INDEX = FREQ, 2ND IS LIST NO.
	INTEGER*4 ITERM,NPTL,NPTH,NFCLK,NFCLKS,NVEC,JL4,JH4
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	CHARACTER*6 TITLE(4)
	CHARACTER*2 INPT(4),MAXCH
	CHARACTER*120 STR
	DIMENSION IML4(16),IML5(16),YY(1250),FREQLO(64),FREQHI(12)
C
	DATA FREQHI /52.,63.,81.,100.,120.,148.,196.,272.,387.,
     1     540.,740.,940./
	DATA NPLOTS /0/
	DATA INPT /'EX','EZ','BY','BZ'/
C
C
	TSTART = DRVFRQ(1) - .05
	TEND =   DRVFRQ(NPTH) + .05
	PRINT*,' IN TPLOT',TSTART,TEND,NPTH
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500.,330.,2200.,3000.)
	ENDIF
C
	  CALL GMTCLOCK(NFCLKS,YEAR,DOY,NHRMIN,SEC)
	  NYR = YEAR+.1
	  NDOY = DOY + .1
	  WRITE(STR,703) NYR,NDOY,NHRMIN,NFCLKS
 703	  FORMAT('\\t',I5,' DAY',I4,1X,I4.4,'  S/C CLK=',I12)
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                   
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
	  CALL MGOPUTLABEL(53,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	IF(IFSTART.LE.0) GO TO 100
C
	DO JH = 1,4
	  JH4 = JH
C	  JHP = 3*(JH-1) + IFSTART
	  JHP = JH + 4*IFSTART
	  CALL MGOWINDOW(1,4,JH4)                          ! HI FREQ
	  YMIN = 0.
	  YMAX = 0.
	  DO JP = 1,NPTH
	    YY(JP) = AVSH(JP,JHP)
	    YMAX = AMAX1(YY(JP),YMAX)
	  ENDDO
	  PRINT*,'FREQ,YMAX',12-JHP,YMAX
	  CALL MGOSETLIM(TSTART,YMIN,TEND,1.1*YMAX)
	  CALL MGOCONNECT(DRVFRQ,YY,NPTH)
	  CALL MGOBOX(1,2)
C	  WRITE(STR,704) (12-JHP)
C 704	  FORMAT('\\tFREQ',I3)
	  WRITE(STR,704) FREQHI(13-JHP)
 704	  FORMAT('\\t',F4.0,' kHz')
	  CALL MGOYLABEL(11,STR)
	ENDDO
	GO TO 200
C
 100	CONTINUE
C
	LFSTART = IFSTART
	LFSTART = IABS(LFSTART)
C
	DO JF = 1,64
	  FREQLO(JF) = .5 + .75*JF
	ENDDO
C
	TSTART = THL(1) - .05
	TEND =   THL(NPTL) + .05
	PRINT*,' IN TPLOT, LOW',TSTART,TEND,NPTL
C
	DO JL = 1,4
	  JL4 = JL
          IF(NMODE.EQ.3) THEN                            ! LINEAR SWEEP
	    JLP = 16*(JL-1) + LFSTART + 1
	  ELSE
	    JLP = LOLIST(2*JL-1,LISTLO+1) + 1
	  ENDIF
	  CALL MGOWINDOW(1,4,JL4)                          ! LO FREQ
	  YMIN = 0.
	  YMAX = 0.
	  DO JP = 1,NPTL
	    YY(JP) = PWRL(JP,JLP)
	    YMAX = AMAX1(YY(JP),YMAX)
	  ENDDO
	  PRINT*,'FREQ,YMAX',JLP-1,YMAX
	  YMAX = AMAX1(YMAX,9.09091)
	  CALL MGOSETLIM(TSTART,YMIN,TEND,1.1*YMAX)
	  CALL MGOCONNECT(THL,YY,NPTL)
	  CALL MGOBOX(1,2)
	  WRITE(STR,705) FREQLO(JLP)
 705	  FORMAT('\\t',F4.1,' kHz')
	  CALL MGOYLABEL(11,STR)
	ENDDO
C
C
 200	CONTINUE
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID('FREQINV','TPLOT')
	CALL MGOSETEXPAND(1.)
	IF(ITERM.LT.0) THEN
C	  CALL MGOTIDLE
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	  NPLOTS = NPLOTS + 1
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
	END
	SUBROUTINE SPPLOT
C
	COMMON /TBLK/ DRVFRQ(1250),THL(300),PWRL(300,64),AVSH(1250,12)
	COMMON /PLOTDT/ ITERM,NPTH,NPTL,NFCLK,NFCLKS,HRFR,DELT,FLOLIM
     1    ,NRESOL
	COMMON /RADMODE/ LISDRVFRQ,LISTLO,LOSTRT,IHISTR,LOLAST,IHILST,NMODE
C
	COMMON /MONGOPAR/
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,
     1  GX,GY,CX,CY,
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,
     1  TERMOUT,XYSWAPPED,NUMDEV,
     1  PI,USERVAR(10),AUTODOT
	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV
C
	CHARACTER*6 TITLE(4)
	CHARACTER*120 STR
	INTEGER*4 ITERM,NPTL,NPTH,NFCLK,NFCLKS,NVEC,JHS,J4
	DIMENSION FREQHI(12),FREQLO(64)
	DIMENSION IML4(16),IML5(16),YY(500),XX(64),JTPS(2)
	DATA FREQHI /52.,63.,81.,100.,120.,148.,196.,272.,387.,
     1     540.,740.,940./
	DATA JTPS /4,75/
C
	PRINT*,' IN SPPLOT',NPTL,NPTH
	DO JF = 1,64
	  FREQLO(JF) = .5 + .75*JF
	ENDDO
C
	  CALL MGOINIT
	  CALL MGOSETUP(ITERM)
	  CALL MGOERASE
	IF(ITERM.LT.0) THEN
	  CALL MGOSETLOC(500.,330.,2200.,3000.)
	ENDIF
C
	  CALL GMTCLOCK(NFCLKS,YEAR,DOY,NHRMIN,SEC)
	  NYR = YEAR+.1
	  NDOY = DOY + .1
	  WRITE(STR,703) NYR,NDOY,NHRMIN,NFCLKS
 703	  FORMAT('\\t',I5,' DAY',I4,1X,I4.4,'  S/C CLK=',I12)
	  CALL MGOSETEXPAND(.85)
	  IF(ITERM.GT.0) THEN
	    CALL MGOGRELOCATE(10.,0.)                    
	  ELSE
	    CALL MGOGRELOCATE(400.,50.)                      ! hardcopy
	  ENDIF
	  CALL MGOPUTLABEL(53,STR,9)
c	  WRITE(STR,704) SAA()
c 704	  FORMAT('\\tSOLAR ASPECT',F6.1,' DEG.')
c	  CALL MGOPUTLABEL(55,STR,9)
	  CALL MGOSETEXPAND(1.)
C
	DO JT = 1,2
	  JTP = JTPS(JT)
	  JHS = 2*JT - 1
	  CALL MGOWINDOW(1,4,JHS)                          ! LO FREQ
	  YMIN = 40.
	  YMAX = YMIN+10.
	  JP = 1
	  DO J = 1,64
	    YY(JP) = PWRL(JTP,J)
	    XX(JP) = FREQLO(J)
	    IF(YY(JP).GT.1.) THEN
	      YMAX = AMAX1(YY(JP),YMAX)
	      JP = JP+1
	    ENDIF
	  ENDDO
	  JP = JP-1
	PRINT*,'SPEC #,YMAX,JP',JTP,YMAX,JP
	  CALL MGOSETLIM(0.,YMIN,50.,1.2*YMAX)
	  J4 = JP
	  CALL MGOCONNECT(XX,YY,J4)
	  CALL MGOBOX(1,2)
	  WRITE(STR,704) THL(JTP)
C 704	  FORMAT('\\tSPEC #',I3)
 704	  FORMAT('\\tAT',F6.2,' HRS')
	  CALL MGOYLABEL(15,STR)
C
	  CALL MGOWINDOW(1,4,JHS+1)                          ! HI FREQ
	  YMIN = 0.
	  YMAX = 0.
	  DO JP = 1,12
	    YY(JP) = AVSH(JTP,JP)
	    YMAX = AMAX1(YY(JP),YMAX)
	  ENDDO
	PRINT*,'SPEC #,YMAX',JTP,YMAX
	  CALL MGOSETLIM(0.,YMIN,1000.,1.1*YMAX)
	  CALL MGOCONNECT(FREQHI,YY,12)
	  CALL MGOBOX(1,2)
	  WRITE(STR,704) DRVFRQ(JTP)
	  CALL MGOYLABEL(15,STR)
c	  CALL MGOPUTLABEL(55,STR,9)
	ENDDO
C
C
	CALL MGOSETEXPAND(.8)
	CALL MGOPLOTID('FFTWIN','SPPLOT')
	CALL MGOSETEXPAND(1.)
	IF(ITERM.LT.0) THEN
	  CALL MGOTIDLE
	  CALL MGOPRNTPLOT(NVEC)
	  PRINT*,' NO. VECTORS PLOTTED',NVEC
	  NPLOTS = NPLOTS + 1
	ELSE
	  CALL MGOTCLOSE
	ENDIF
C
	RETURN
C
	END
