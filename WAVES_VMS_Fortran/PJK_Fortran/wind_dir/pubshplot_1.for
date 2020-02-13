        SUBROUTINE PUBSHPLOT(NCHGP,ICH)
C
C       THIS IS LIKE SHADE, BUT MAKES LANDSCAPE PLOTS, PREFERABLY
C               FFTH, AND A WHOLE DAY ON A PAGE, OR MORE.
C
        character*32 s_scet
        CHARACTER*1 DISPOSE
        integer*2 NPWRL
        CHARACTER*10 TITLE(15)
        CHARACTER*120 STR
        REAL FUNDF(3)
        real*8 scet8
        integer*4 fft_channel
C       COMMON /SHBLK/ PTIME(1000),NPWRL(1000,511,4)
        COMMON /SHBLK/ PTIME(1000),NPWRL(500,511,4)
        COMMON /PLTPAR/ ITERM,NDAY,IPNT(4),ITMCORR,IDOY
        common /headblk/ major,minor,fft_channel,isrc,s_scet,title,scet8     
  *
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
C       DIMENSION PWRL(511000)
C       DIMENSION PWRL(102200)
        DIMENSION PWRL(255500)
        DATA FUNDF /.021348,.005337, .33356/
C
C
        NWNDOW = 2
        IF(NCHGP.NE.1) NWNDOW = 4
        ITERM = -3              ! SAVE
C       ITERM = 3
        XSTART = 400.
        XEND = 2300.
        XEND = 2175.            ! to match kyoto AE plots
        IW = 1
        NPT = IPNT(IW)
        IF(NPT.LT.1) NPT = 1                      ! protection
        PRINT*,' IN SHPLOT,NPT',NPT
        PRINT*,' IN SHPLOT,TIMES',PTIME(1),PTIME(NPT),NPT
C       CALCULATE BOX SIZE
        PIXSIZ = .1
        IF(NPT.GT.1) PIXSIZ = (PTIME(NPT) - PTIME(1))/(NPT-1)
        HSTART = PTIME(1) - .5*PIXSIZ
        HEND =   PTIME(NPT) + .5*PIXSIZ
        PRINT*,' IN SHPLOT',HSTART,HEND,NPT
          CALL MGOINIT
          CALL MGOSETUP(ITERM)
          CALL MGOERASE
C

        IF(ITERM.LT.0) THEN
C         CALL MGOSETLOC(XSTART,280.,XEND,3100.)
          CALL MGOSETLOC(XSTART,985.,XEND,2300.)
        ENDIF
C
        IW = ICH
C       DO IW = 1,NWNDOW
C         CALL MGOWINDOW(1,NWNDOW,IW)                          !
          YMIN = 1.E6
          YMAX = -YMIN
          CALL HISTOG(2,TJUNK,256,-250.,0.,.01,TOTAL,RET)     ! CLEAR AND 
INIT *
          DO M = 1,NPT
          DO N = 1,511
            NM = M + (N-1)*NPT
            PWRL(NM) = NPWRL(M,N,IW)
            CALL HISTOG(1,PWRL(NM),256,-250.,0.,.5,TOTAL,RET)    ! LOAD 
ARRAY  *
C
            YMIN = AMIN1(YMIN,PWRL(NM))
            YMAX = AMAX1(YMAX,PWRL(NM))
          ENDDO
          ENDDO
          PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX
C
C         CALL HISTOG(0,TJUNK,256,-250.,0.,.03,TOTAL,YHMIN)   !DETERMINE 3 
PCTI*
C         CALL HISTOG(0,TJUNK,256,-250.,0.,.97,TOTAL,YHMAX)  !DETERMINE 97 
PCTI*
          CALL HISTOG(0,TJUNK,256,-250.,0.,.02,TOTAL,YHMIN)   !DETERMINE 2 
PCTI*
          CALL HISTOG(0,TJUNK,256,-250.,0.,.98,TOTAL,YHMAX)  !DETERMINE 98 
PCTI*
C         RAISING YMIN MAKES THE BACKGROUND LIGHTER
C         LOWERING YMAX MAKES THE SIGNAL DARKER
C
          YAMIN = YHMIN
          YAMAX = YHMAX
C         YAMIN = YHMIN - 5.
C         YAMAX = YHMAX + 10.
          PRINT*,'YMIN,MAX SET TO',YAMIN,YAMAX
c         arguments are: array(m,n),m,n,white,black,linearity
c         m is the x direction
          CALL MGOHALFTONE(PWRL,NPT,511,YAMIN,YAMAX,1.E7)
          CALL MGOSETEXPAND(.7)
          FMIN = FUNDF(NCHGP)
          FMAX = 511.*FMIN
          CALL MGOSETLIM(HSTART,FMIN,HEND,FMAX)
          IF(NCHGP.EQ.3) THEN
            CALL MGOYLABEL(13,'FREQUENCY, Hz')
          ELSE
            CALL MGOYLABEL(14,'FREQUENCY, kHz')
          ENDIF
          CALL MGOTICKSIZE( .16667, 1., 0., 0.)
          CALL MGOBOX(1,2)
          CALL MGOGRELOCATE(GX1,GY2)
          CALL MGOPUTLABEL(10,TITLE(IW),9)
        print*,'title in shplot  ',title(iw)
C
          WRITE(STR,704) .5*YAMIN,.5*YAMAX
 704      FORMAT('WHITE,BLACK ',2F6.1,' DB ')
          CALL MGOSETANGLE(90.)
          CALL MGOSETEXPAND(.5)
          XPR = GX2 + .005*(GX2-GX1)
          YPR = .5*(GY1+GY2)
          CALL MGOGRELOCATE(XPR,YPR)
          CALL MGOPUTLABEL(28,STR,2)
          CALL MGOSETANGLE(0.)
          CALL MGOSETEXPAND(1.)
          IF(IW.EQ.1) THEN
C          s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//
C       1       s(9:10)//':'//s(11:12)//':'//s(13:14)
            WRITE(STR,1001) S_SCET(1:10),IDOY
 1001       FORMAT('HOURS OF ',A10,' DOY ',I3)
            CALL MGOSETEXPAND(.8)
            CALL MGOXLABEL(28,STR)
            CALL MGOSETEXPAND(.5)
            IF(ITMCORR.EQ.0) THEN
                CALL MGOPUTLABEL(23,'    consecutive spectra',6)
            ELSE
                CALL MGOPUTLABEL(18,'    corrected time',6)
            ENDIF
            CALL MGOSETEXPAND(1.)
          ENDIF
C
C         IF(IW.EQ.NWNDOW) THEN
            CALL MGOSETEXPAND(.8)
            CALL MGOPLOTID('FFTPRO 3.1','SHPLOT')
            CALL MGOSETEXPAND(1.)
C         ENDIF
C       ENDDO
C
          CALL MGOSETEXPAND(1.)
          IF(ITERM.LT.0) THEN
            CALL MGOPRNTPLOT(NVEC)
            PRINT*,' NO. VECTORS PLOTTED',NVEC
          ELSE
            READ(5,1234) DISPOSE
 1234       FORMAT(A)
            CALL MGOTCLOSE
          ENDIF
C
        RETURN
        END           
