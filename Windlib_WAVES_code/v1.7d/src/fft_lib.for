	SUBROUTINE FWINDW(my_window,my_normal,window)
C
$IF ABSOFT_FORTRAN
	character *(*)	rn
	parameter	(rn='FFT FWINDOW:')
$ELSE
	parameter	rn='FFT FWINDOW:'
$ENDIF
	integer*4	my_window	!1 for Hamming, 2 for Hanning,
					!otherwise, square
	integer*4	my_normal	!1 for RMS normalized, otherwise not
	real*4		WINDOW(1024)
!
	DATA TWOPI /6.2831853/
C
	if (my_window .eq. 1) then
	    ALPHA = .54				!Hamming
	    SUMSQ = 0.
	    DO N = 1,1024
		WINDOW(N) = ALPHA + (ALPHA-1.)*COS(TWOPI*(N-1)/1024.)
		SUMSQ = SUMSQ + WINDOW(N)**2 
	    ENDDO
	elseif (my_window .eq. 2) then
	    ALPHA = .5				!Hanning
	    SUMSQ = 0.
	    DO N = 1,1024
		WINDOW(N) = ALPHA + (ALPHA-1.)*COS(TWOPI*(N-1)/1024.)
		SUMSQ = SUMSQ + WINDOW(N)**2 
	    ENDDO
	else
	    call w_msg_put_always(rn, 'REQUESTED WINDOW DOES NOT EXIST')
	    call w_msg_put_always2(rn,'NO WINDOWING, IWINDW=', my_window)
	    DO N = 1,1024
		WINDOW(N) = 1.
	    ENDDO
	endif

	if (my_normal .eq. 1) then
	    RMS = SQRT(SUMSQ/1024.)
	    DO N = 1,1024
		WINDOW(N) = WINDOW(N)/RMS
	    ENDDO
	endif
	
	RETURN
	END

      SUBROUTINE REALFT(DATA,N,ISIGN)
C
C	Calculates the Fourier Transform of a set of 2*N real-valued data
C	points.  Replaces this data (which is stored in array DATA) by the
C	positive frequency half of its complex Fourier Transform.  The real-
C	valued first and last components of the complex transform are 
C	returned as DATA(1) and DATA(2) respectively.  
C
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
C
C
      THETA=6.28318530717959D0/2.0D0/DBLE(N)
      WR=1.0D0
      WI=0.0D0
      C1=0.5
C
C
      IF (ISIGN.EQ.1) THEN
        C2=-0.5
        CALL FOUR1(DATA,N,+1)
        DATA(2*N+1)=DATA(1)
        DATA(2*N+2)=DATA(2)
      ELSE
        C2=0.5
        THETA=-THETA
        DATA(2*N+1)=DATA(2)
        DATA(2*N+2)=0.0
        DATA(2)=0.0
      ENDIF
      WPR=-2.0D0*DSIN(0.5D0*THETA)**2
      WPI=DSIN(THETA)
      N2P3=2*N+3
      DO 11 I=1,N/2+1
        I1=2*I-1
        I2=I1+1
        I3=N2P3-I2
        I4=I3+1
        WRS=SNGL(WR)
        WIS=SNGL(WI)
        H1R=C1*(DATA(I1)+DATA(I3))
        H1I=C1*(DATA(I2)-DATA(I4))
        H2R=-C2*(DATA(I2)+DATA(I4))
        H2I=C2*(DATA(I1)-DATA(I3))
        DATA(I1)=H1R+WRS*H2R-WIS*H2I
        DATA(I2)=H1I+WRS*H2I+WIS*H2R
        DATA(I3)=H1R-WRS*H2R+WIS*H2I
        DATA(I4)=-H1I+WRS*H2I+WIS*H2R
        WTEMP=WR
        WR=WR*WPR-WI*WPI+WR
        WI=WI*WPR+WTEMP*WPI+WI
11    CONTINUE
      IF (ISIGN.EQ.1) THEN
        DATA(2)=DATA(2*N+1)
      ELSE
        CALL FOUR1(DATA,N,-1)
      ENDIF
C
C
      RETURN
      END

      SUBROUTINE FOUR1(DATA,NN,ISIGN)
C
C
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
C
C
      N=2*NN
      J=1
      DO 11 I=1,N,2
        IF(J.GT.I)THEN
          TEMPR=DATA(J)
          TEMPI=DATA(J+1)
          DATA(J)=DATA(I)
          DATA(J+1)=DATA(I+1)
          DATA(I)=TEMPR
          DATA(I+1)=TEMPI
        ENDIF
        M=N/2
1       IF ((M.GE.2).AND.(J.GT.M)) THEN
          J=J-M
          M=M/2
        GO TO 1
        ENDIF
        J=J+M
11    CONTINUE
      MMAX=2
2     IF (N.GT.MMAX) THEN
        ISTEP=2*MMAX
        THETA=6.28318530717959D0/(ISIGN*MMAX)
        WPR=-2.D0*DSIN(0.5D0*THETA)**2
        WPI=DSIN(THETA)
        WR=1.D0
        WI=0.D0
        DO 13 M=1,MMAX,2
          DO 12 I=M,N,ISTEP
            J=I+MMAX
            TEMPR=SNGL(WR)*DATA(J)-SNGL(WI)*DATA(J+1)
            TEMPI=SNGL(WR)*DATA(J+1)+SNGL(WI)*DATA(J)
            DATA(J)=DATA(I)-TEMPR
            DATA(J+1)=DATA(I+1)-TEMPI
            DATA(I)=DATA(I)+TEMPR
            DATA(I+1)=DATA(I+1)+TEMPI
12        CONTINUE
          WTEMP=WR
          WR=WR*WPR-WI*WPI+WR
          WI=WI*WPR+WTEMP*WPI+WI
13      CONTINUE
        MMAX=ISTEP
      GO TO 2
      ENDIF
C
C
      RETURN
      END
