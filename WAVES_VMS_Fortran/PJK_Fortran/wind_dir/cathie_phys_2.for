*	Program to make a simple test of the FFT physical routine

	implicit none

	integer*4	ok, aok
	integer*4	ch
	integer*4	in_len/1024/
	integer*4	out_len
	integer*4	w_channel_open
	integer*4	w_event
	character*132	item
	character*4	event_type
	character*40	outname
	character*8	date
	real*4		output_data(1024)
	integer*4	fft_phys_r4, w_item_i4,i, w_messages_off
	integer*4	channel,out_num,count,antenna

	ok = w_channel_open(ch,'offline')
	ok = w_messages_off(ch)
	type *,'Enter the event type (FFTH, FFTM, FFTL)'
	read(5,'(a)')event_type
	type *,'Enter item name '
	read(5,'(a)')item

	type*,'date should be 1995xxyy for consistency'
	type *,'Enter date of data : ',date
  	read(5,'(a)')date
	outname = event_type //'_' //date// '.' //item
	count = 0
	do while (ok .ne. 82 .and. count .lt. 100)
	  ok = w_event(ch,event_type)	
	  open(unit=1,name=outname,form='formatted',type='new')
	  if (ok .eq. 82) stop
	  if (ok .eq. 1) then
	     aok = fft_phys_r4(ch,item,output_data,in_len,out_len)
	     count = count + 1
	     if (aok .eq. 1) then
	       ok = w_item_i4(ch,'channel_number',channel,1,out_num)
	       ok = w_item_i4(ch,'source',antenna,1,out_num)
	       write(1,*)count,channel,antenna,(output_data(i),i=1,out_len)		
	    endif
	  endif
	enddo

	close(1)
	stop
	end

	Integer*4		Function	fft_phys_r4(
	1		ch,
	1		item,
	1		output,
	1		in_len,
	1		out_len)

*	Purpose:  To convert FFT data into physical units
*	The items available during a particular event depend on the antenna
*	selection and whether the data is raw or has been processed on board
*	the spacecraft.  If it is raw, an FFT is applied in this function
*	to mimic the on-board algorithm.  In the raw case, voltages
*	and a spectrum are available.  If the data has been processed on
*	board, no voltages are available, only the corrected spectrum.
*	A future step may include voltages derived from the inverse FFT of
*	the frequency-response-corrected spectrum.
*	
*	The ITEMS covered by this routine apply to FFTH, L AND M.
*	VOLTS_METER		Any valid voltage data is transmitted
*	E_VOLTS_METER		Any E-field data is transmitted
*	EXAC_VOLTS_METER	Only the ExAC component is transmitted
*	EXDC_VOLTS_METER	Only the ExDC component is transmitted
*	EYAC_VOLTS_METER	Only the EyAC component is transmitted
*	EYDC_VOLTS_METER	Only the EyDC component is transmitted
*	EZAC_VOLTS_METER	Only the EzAC component is transmitted
*	EZDC_VOLTS_METER	Only the EzDC component is transmitted
*	B_NT2_HZ		Any B-field data is transmitted
*	BX_NT2_HZ		Only the Bx component is transmitted
*	BY_NT2_HZ		Only the By component is transmitted
*	BZ_NT2_HZ		Only the Bz component is transmitted
*	SPECTRUM_DB		Any Power spectrum - B or E
*	SPECTRUM_E_DB           Follows the same sort of convention as above
*	SPECTRUM_B_DB
*	SPECTRUM_EXAC_DB
*	SPECTRUM_EYAC_DB
*	SPECTRUM_EZAC_DB
*	SPECTRUM_EXDC_DB
*	SPECTRUM_EYDC_DB
*	SPECTRUM_EZDC_DB
*	SPECTRUM_BX_DB
*	SPECTRUM_BY_DB
*	SPECTRUM_BZ_DB

*	To select and process the data we need some fundamental information
*	about the current state of the instrument.

	IMPLICIT NONE

	integer*4	spectrum(1024)
	integer*4	n
	integer*4	antenna_int
	integer*4	chan_gap
	integer*4	size1/1/
	integer*4	size1024/1024/
*	integer*4	size40/40/
	integer*4	size_out
	integer*4	mantissa(1024)
	integer*4	exponent(1024)
	integer*4	ok
	integer*4	ch
	integer*4	fft_chan
	integer*4	out_len
	integer*4	in_len

	real*4		output(*)
	real*4		volts_per_step/.24e-6/  ! 
	real*4		ffund(3)		!Fundamental frequencies for
*						  H,M,L receivers, respect..
	real*4		fftamp		
	real*4		raw_voltages(1024)
	real*4		windowed_voltages(1024)
	real*4		pgain
	real*4		const
	real*4		freq
	real*4		fund_freq	

	character *(*)	item
	character *132  copy_item
	character *132	antenna

	logical*1	raw
	logical*1	spectral
	logical*1	proceed

*	FUNCTIONS
	integer*4	w_item_i4,w_item_char
	real*4		preamp

*	Steve's FFT Cals, average over channel and preamp, give 1.03 
*	counts for 3mVpp input, at unity preamp gain.  3mVpp is 1.5mV 
*	pk, or 1.06 mVrms, or (1.06/1.03) 10**-3 volts 
*	rms/step.  This is at gainstep 3, so divide by 16.3**3

	data Volts_per_step/.24e-6/

	data ffund/21.34766,5.336914,.3335571/

	out_len = 0

*	FIND OUT WHETHER THE DATA IS RAW OR NOT.  PRIMITIVE MEANS...
	raw = .false.
	ok = w_item_i4(ch,'MANTISSA',mantissa,size1024,size_out)
	if (ok .eq. 1) raw = .true.

*	DETERMINE WHICH ANTENNA IS IN USE
	ok = w_item_char(ch,'SOURCE',antenna,    size1,size_out)
	ok = w_item_i4  (ch,'SOURCE',antenna_int,size1,size_out)

*	ENSURE THAT BOTH ITEM AND ANTENNA ARE UPPERCASE
	copy_item = item
	call strupcase(copy_item)
	call strupcase(antenna)

*	ASSUME THAT THE ITEM IS INVALID
	proceed = .false.

*	VOLTAGE ITEMS ARE ONLY VALID WHEN THE DATA IS RAW
        if (raw) then
*	  Check for source validity when a spectrum has NOT been selected
          if (copy_item(1:1) .ne. 'S') then
	    spectral = .false.

*	    CHECK FOR ANTENNA
	    if (copy_item(1:1) .eq. antenna(1:1) ) then
              if (copy_item(2:2) .eq. '_' ) then
	        proceed = .true.
	      else if (copy_item(2:3) .eq. antenna(2:3) ) then 
	        proceed = .true.
              endif
*	    THE VOLTS/METER ITEM IS ALWAYS AVAILABLE FOR RAW DATA	  
	    else if (copy_item(1:1) .eq. 'V') then
	      proceed = .true.
	    endif
	  endif
	endif

*	THE SPECTRUM_DB ITEM IS ALWAYS VALID
*	CHECK WHETHER THERE IS AN ANTENNA SELECTION CONSTRAINT
	if (copy_item(1:1) .eq. 'S') then
	    if (copy_item(10:10) .eq. antenna(1:1) ) then
              if (copy_item(11:11) .eq. '_' ) then
	        proceed = .true.
	      else if (copy_item(11:12) .eq. antenna(2:3) ) then 
	        proceed = .true.
              endif
*	    THE SPECTRAL ITEM IS ALWAYS AVAILABLE IF ANTENNA IS UNSPECIFIED
	    else if (copy_item(10:10) .eq. 'D') then
	      proceed = .true.
	    endif
	  spectral = .true.
	endif

*	ONLY DEAL WITH THE DATA IF IT IS SOMETHING WE WANT
	if (proceed) then
*	  DETERMINE THE CHANNEL  
	  ok = w_item_i4(ch,'CHANNEL_NUMBER',fft_chan,size1,size_out)

	  if (raw) then
	    out_len = 1024
*	    GET THE MANTISSA AND EXPONENT FOR THE VOLTAGES
	    ok = w_item_i4(ch,'MANTISSA',mantissa,size1024,size_out)
	    ok = w_item_i4(ch,'EXPONENT',exponent,size1024,size_out)

*	    REMOVE BIAS, AND WINDOW 
	    call unbias(fft_chan,mantissa,exponent,raw_voltages)

	    if (spectral) then
*	      PERFORM AN FFT ON THE INCOMING RAW DATA
	      call window(raw_voltages,windowed_voltages)
	      call windfft(windowed_voltages,spectrum)
	      out_len = 513
	    endif
	  endif                     	!raw

	  if (spectral) then	
	    if (.not. raw) then
*	      GET THE INCOMING SPECTRAL DATA
	      ok = w_item_i4(ch,'DATA',spectrum,size1024,size_out)
	    endif

*	    FIXUP SPECTRUM (WHETHER CALCULATED OR FROM TELEMETRY) AND PLACE
*	    IN OUTPUT ARRAY
	    out_len   = 513
	    chan_gap  = int((fft_chan + 1)/4) + 1
	    fund_freq = ffund(chan_gap)

	    const     = 40. *(alog10(512./volts_per_step)) + 
	1               20. * alog10(fund_freq) - 124
*	    
	    if (in_len .lt. out_len) then
	      write(6,*)'fft_phys_r4 - truncating output to ',in_len
	      out_len = in_len
	    endif

	    do n = 2,out_len
	      freq        = (n-1)*fund_freq
	      pgain       = preamp(antenna_int,freq)
	      output(n)   = .5 * (spectrum(n) - const) - 
	1                20.*alog10(pgain*fftamp(fft_chan,freq))
	    enddo
	  else
	    
*	    PUT VOLTAGES IN OUTPUT ARRAY, CHECKING LENGTH...
	    if (in_len .lt. out_len) then
	      write(6,*)'fft_phys_r4 - truncating output to ',in_len
	      out_len = in_len
	    endif
	        
	    do n = 1,out_len
	      output(n) = raw_voltages(n)
	    enddo
	  endif

*	  SET FUNCTION FLAG TO SUCCESSFUL COMPLETION
	  fft_phys_r4 = 1
	else			!proceed
*	  SET FUNCTION FLAG TO UNSUCCESSFUL COMPLETION	  
	  fft_phys_r4 = 0
	endif

	return 
	end	 


	subroutine window(raw_voltages,windowed_voltages)
*	TO APPLY THE APPROPRIATE WINDOWING FUNCTION TO THE RAW DATA

	implicit none

	integer*4	npros, N
	integer*4	iwindw
	real*4		window_factors(1024)
	real*4		raw_voltages(1024)
	real*4		windowed_voltages(1024)
	real*4		fwindw
	
	NPROS = 1024
	IWINDW = 1
	CALL FWINDW(IWINDW,window_factors)
C
	DO N = 1,NPROS
	   windowed_voltages(N) = window_factors(N)*raw_voltages(N)
	ENDDO

	return
	end



	subroutine unbias(fft_chan,mantissa,exponent,raw_voltages)
*	Purpose;
*	To remove bias from raw data that is not 32-bit integers

	implicit none
 
	real*4		offsets(40)
	real*4		temp
	real*4		raw_voltages(1024)
	integer*4	mantissa(1024)
	integer*4	fft_chan
	integer*4	exponent(1024)
*	integer*4	w_item_r4
	integer*4	iloop
	integer*4	nex
	integer*4	ntemp


	data offsets/	2048., 2050., 2046., 2048.,
	1		2048., 2050., 2046., 2051.,
	1		2048., 2082., 2041., 2043.,
	1		2048., 2078., 2042., 2046.,
	1		2048., 2048., 2048., 2047.,
	1		2048., 2075., 2043., 2045.,
	1		2048., 2048., 2048., 2037.,
	1		2048., 2048., 2048., 2040.,
	1		2048., 2048., 2048., 2036.,
	1		2048., 2048., 2287., 2048./

*	USE THESE TO CALCULATE THE RAW VOLTAGES
*	ok       = w_item_r4(ch,'OFFSETS',offsets,size40,size_out)
	   
	do iloop = 1,1024
	  ntemp  = mantissa(iloop) .and. 4095
	  nex    = exponent(iloop) .and. 3
	  temp   = -(ntemp - offsets((fft_chan-1)*4 + nex+1))
	  raw_voltages(iloop) = temp * (-16.3**nex)
	enddo
	return
	end



	SUBROUTINE WINDFFT(pwrw,spectrum)
C
C	THIS WINDFFT DOES FFT AS IN FLIGHT SOFTWARE
C

	implicit none

	integer*4	nfrq
	integer*4	npros
	integer*4	n
	integer*4	spectrum(1024)

	REAL*4 		PWRW(1024)
	real*4		avr		

	NPROS = 1024
	NFRQ = NPROS/2

        CALL REALFT(pwrw,NFRQ,1)
	AVR = pwrw(1)
C	
	IF(AVR.GT.0.) THEN
	    PWRW(1) = 40.*ALOG10(AVR) - 124.
	ELSE
	    PWRW(1) = 0.
	ENDIF
	spectrum(1) = int(pwrw(1))

C	CALCULATE POWER 
C
c	114.4 is 20.*log10(1024**2/2), to make fasfor normalization agree
c	with the flight norm which subtracts 124.

	DO N = 2,NFRQ+1
	  IF(PWRW(N).GT.0.) THEN
	    PWRW(N) = 20.*ALOG10(pwrw(N)) - 124.   ! .5 db steps,fl bias
	  ELSE
	    PWRW(N) = 0.
	  ENDIF
	  spectrum(N) = int(pwrw(n))
	ENDDO
	RETURN
	END


	Real*4	FUNCTION 	PREAMP(IRX,F)
C
C	THIS FUNCTION CALLS THE APPROPRIATE PREAMP, PUTS THE
C	COMPLEX GAIN AND PHASE SHIFT IN CGAIN AND PHASE, AND RETURNS
C	THE MAGNITUDE OF THE PREAMP GAIN
C
C
	implicit none
	integer*4	irx
	real*4	f

*	FUNCTIONS CALLED
	real*4		exacpa
	real*4		eyacpa
	real*4		ezacpa
	real*4		exdcpa
	real*4		eydcpa
	real*4		ezdcpa
	real*4		bxpa,bypa,bzpa
C
	IF(IRX.EQ.1) PREAMP = EXACPA(F)
	IF(IRX.EQ.2) PREAMP = EYACPA(F)
	IF(IRX.EQ.3) PREAMP = EZACPA(F)
	IF(IRX.EQ.4) PREAMP = EXDCPA(F)
	IF(IRX.EQ.5) PREAMP = EYDCPA(F)
	IF(IRX.EQ.6) PREAMP = EZDCPA(F)
	IF(IRX.EQ.7) PREAMP = BXPA(F)
	IF(IRX.EQ.8) PREAMP = BYPA(F)
	IF(IRX.EQ.9) PREAMP = BZPA(F)
	RETURN
	END


      SUBROUTINE REALFT(DATA,N,ISIGN)
C	Calculates the Fourier Transform of a set of 2*N real-valued data
C	points.  Replaces this data (which is stored in array DATA) by the
C	positive frequency half of its complex Fourier Transform.  The real-
C	valued first and last components of the complex transform are 
C	returned as DATA(1) and DATA(2) respectively.  
C
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      THETA=6.28318530717959D0/2.0D0/DBLE(N)
      WR=1.0D0
      WI=0.0D0
      C1=0.5
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
      RETURN
      END

      SUBROUTINE FOUR1(DATA,NN,ISIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
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
      RETURN
      END


	FUNCTION FFTAMP(CHANNEL,F)

C	THIS IS THE GAIN OF A CIRCUIT JUST BEFORE THE FFT D/A CONVERTER
C
	INTEGER*4 CHANNEL
	COMPLEX Z1,Y2,GAIN
	REAL RSW(6),C2MUF(6),F
	COMMON /GAINBLK/ PHASE,GAIN
	DATA RSW /50.,400.,400.,0.,0.,0./
	DATA C2MUF /1.961,1.896,1.949,1.981,2.011,1.943/
	DATA C1,R2,C2 /2.E-6,1.E4,1.E-9/
	DATA TWOPI /6.2831853/
C
	GAIN = CMPLX(1.,0.)
	PHASE = 0.
	FFTAMP = 1.
	IF(CHANNEL.GT.6) RETURN
	W = TWOPI*F
	C1 = C2MUF(CHANNEL)*1.E-6
	RSWT = RSW(CHANNEL)
	Z1 = CMPLX(RSWT,-1./W/C1)
	Y2 = CMPLX(1./R2,W*C2)
	GAIN = 1./(1. + Y2*Z1)

C
	GNIP = AIMAG(GAIN)
	GNRP = GAIN
	PHASE = ATAN2(GNIP,GNRP)
	FFTAMP = CABS(GAIN)
	RETURN
	END


	SUBROUTINE FWINDW(IWINDW,window)
C
	real*4		WINDOW(1024)
	DATA TWOPI /6.2831853/
C
	GO TO (100,200,300) IWINDW+1
	PRINT*,'REQUESTED WINDOW DOES NOT EXIST'
 100	CONTINUE
	PRINT*,'IWINDW =',IWINDW,'  NO WINDOWING'
	DO N = 1,1024
	  WINDOW(N) = 1.
	ENDDO
	RETURN
 200	CONTINUE
C	PRINT*,'IWINDW =',IWINDW,'  HAMMING'
	ALPHA = .54
	GO TO 310
 300	CONTINUE
	ALPHA = .5
	PRINT*,'IWINDW =',IWINDW,'  HANNING'
 310	CONTINUE
	SUMSQ = 0.
	DO N = 1,1024
	  WINDOW(N) = ALPHA + (ALPHA-1.)*COS(TWOPI*(N-1)/1024.)
	  SUMSQ = SUMSQ + WINDOW(N)**2 
	ENDDO
	RMS = SQRT(SUMSQ/1024.)
	DO N = 1,1024
	  WINDOW(N) = WINDOW(N)/RMS
	ENDDO
	RETURN
	END

	FUNCTION EXDCPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND X PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /5.E8, 23.6E-12, 5.E8, 4.37E-12/
	DATA R33,C16 /2.2E+03, 1.203E-9/
	DATA R1C,R2C,CCOMP / 0., 4.64E3, .947E-6/    ! as shown on drawing
C	DATA R1C,R2C,CCOMP / 0., 5.E3, .947E-6/
	DATA R5 /22.1E3/
	DATA TWOPI /6.2831853/
C
C	C16 = CVAR(1)
C	C9 = CVAR(2)
C	CINP = CVAR(3)
C
	CGAIN = 0.
	EXDCPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C	EFFECT OF OUTPUT RC
	WC16 = 1./(W*C16)
	CGAIN = CGAIN*CMPLX(0.,-WC16)/CMPLX(R33,-WC16)
C
C	EFFECT OF INPUT (211) BOARD
C
	WCC = W*CCOMP
	Z2 = CMPLX(R2C,-1./WCC)	
	Y1 = 1./R5 + 1./(R1C + Z2) + CMPLX(0.,W*C16)	
	CGAIN = CGAIN*(Z2/(R1C+Z2))/(1. + R33*Y1)
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EXDCPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EYDCPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND Y PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C
C	DONE AND CHECKED
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /5.E9,24.5E-12,5.E9,4.757E-12/
	DATA R33,C16 /2.2E+03,1.185E-9/
	DATA R1C,R2C,CCOMP /37.4E3,39.2E3,1.982E-6/
	DATA R19 /1.E5/
	DATA TWOPI /6.2831853/
C
C	C16 = 1.185E-9                           
C	C9  = 24.5E-12                           
C	CINP = 4.757E-12                         
C
C
C	C16 = CVAR(1)                             ! ADJUSTING EY
C	C9  = CVAR(2)                             ! ADJUSTING EY
C	CINP = CVAR(3)                            ! ADJUSTING EY
C
	CGAIN = 0.
	EYDCPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C	EFFECT OF OUTPUT RC
	WC16 = 1./(W*C16)
	CGAIN = CGAIN*CMPLX(0.,-WC16)/CMPLX(R33,-WC16)
C
C	EFFECT OF INPUT (211) BOARD
C
	WCC = W*CCOMP
	Z2 = CMPLX(R2C,-1./WCC)	
	Y2 = 1./Z2 + 1./R19
	Z1 = R1C + 1./Y2
	Y1 = 1./Z1 + CMPLX(0.,W*C16)	
	CGAIN = CGAIN/(1. + R33*Y1)/(1. + R1C*Y2)
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EYDCPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EZDCPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE DC CHANNEL OF THE WIND Z PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C	DONE AND TESTED
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA R9,C9,R10,CINP /18.E6,84.E-12,2.E8,4.99E-12/
	DATA R33,C16 /2.2E+03,1.202E-9/
	DATA RCOMP,CCOMP /22.1E3,47.E-12/
	DATA R29 /22.1E3/
	DATA TWOPI /6.2831853/
C
C	C16 = 1.203E-9                             ! ADJUSTING EX
C	C9  = 23.6E-12                            ! ADJUSTING EX
C	CINP = 4.99E-12                          ! ADJUSTING EZ
C
C	C16 = CVAR(1)
C	C9 = CVAR(2)
C	CINP = CVAR(3)
C
	CGAIN = 0.
	EZDCPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
	CGAIN = 1.
C	DIVISION OF SIGNAL BEFORE FOLLOWER
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C	EFFECT OF OUTPUT RC
	WC16 = 1./(W*C16)
	CGAIN = CGAIN*CMPLX(0.,-WC16)/CMPLX(R33,-WC16)
C
C	EFFECT OF INPUT (211) BOARD
C
	Y1 = 1./R29 + CMPLX(0.,W*C16)	
	CGAIN = CGAIN/(1. + R33*Y1)
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EZDCPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EXACPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND X PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
C	COMPONENT DESIGNATIONS ARE SAME AS SCHEMATIC
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN,ZMEU
	DATA R9,C9,R10,CINP /5.E8,23.6E-12,5.E8,4.37E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.49E-9,10.E3,86.9E-9/
	DATA C15,R32,C27,R11/ 0.,10.E3,470.E-12,1.5E3/
	DATA C25,R39,C30/829.E-9,330.,29.9E-9/ 
	DATA CMEU,RMEU /20.E-9,300./
	DATA TWOPI /6.2831853/
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
C
C	C30 = 30.9E-9                     ! ADJUSTING EX
C	C13 = 3.7E-9                     ! ADJUSTING EX
C
C	C30 = CVAR(1)
C	C26 = CVAR(2)
C	C25 = CVAR(3)
C	C13 = CVAR(4)
C
	CGAIN = 0.
	EXACPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
	CGAIN = 1.
C
C	DIVISION OF SIGNAL BEFORE FOLLOWER
C
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C
C	CGAIN = 3.2
C
C	EFFECT OF OUTPUT OP AMP
C	
C	FILTERING BEFORE INPUT
	Z1 = R28                 	! LAST LOAD BEFORE OP AMP
	WC12 = W*C12
	Z2 = CMPLX(R28,-1./WC12)        !  LAST RC
	Y1 = 1./CMPLX(R43,0.) + 1./Z2   !  LAST RC IN PARALLEL WITH R43
	WC29 = W*C29                    
	Z3 = 1./Y1 + CMPLX(0.,-1./WC29) !  LOAD ON FOLLOWER
	OPAMPIN = CGAIN*(Z1/Z2)*(1./Y1/Z3)
C
C	OP AMP GAIN
C
C	Z3 FROM NEG. INPUT TO GROUND
	WC13 = W*C13
	Z1 = CMPLX(R29,-1./WC13)
	WC26 = W*C26
	Z2 = CMPLX(R12,-1./WC26)
	Y1 = 1./Z1 + 1./Z2                  ! Y FROM NEG INPUT TO GROUND
	Z3 = 1./Y1                          ! Z FROM NEG INPUT TO GROUND
C	Z2 FROM OUTPUT TO NEG INPUT
	WC27 = W*C27
	Z1 = CMPLX(R11,-1./WC27)
	WC15 = W*C15
	Y1 = CMPLX(1./R32,WC15)
	Y2 = Y1 + 1./Z1 
	Z2 = 1./Y2
	CGAIN = OPAMPIN*(Z3 + Z2)/Z3
C
C	FILTERING AFTER OP AMP OUTPUT
C
	WC30 = W*C30
	RMINN = 22.1E3                ! input resistance on 210 board
	Y1 = CMPLX(1./RMINN,WC30)
	Z1 = R39 + 1./Y1
	WC25 = W*C25
C	RTNR = .42e5		      ! total TNR load
	ZMEU = 47. + CMPLX(RMEU,-1./W/CMEU)
	Y3 = 1./ZMEU + 1./Z1
	Z2 = CMPLX(0.,-1./WC25)
	CGAIN = CGAIN/(1.+Y3*Z2)/Y1/Z1	
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EXACPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EYACPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND Y PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C
C	COMPONENT DESIGNATIONS ARE SAME AS SCHEMATIC
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN,ZMEU
	DATA R9,C9,R10,CINP /5.E9,24.5E-12,5.E9,4.757E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.437E-9,10.E3,98.23E-9/
	DATA C15,R32,C27,R11/ 0.,33.E3,150.E-12,4.7E3/
	DATA C25,R39,C30/1401.E-9,330.,30.61E-9/
 	DATA CMEU,RMEU /20.E-9,300./
	DATA TWOPI /6.2831853/
C
C	TO BE CHANGED FOR Y,Z:  R11,R32,C27,R9,C9,R10
C
C	DONE AND CHECKED
C
C	C30 = CVAR(1) = 30.61 E-9
C	C26 = CVAR(2) = 98.23 E-9
C	C25 = CVAR(3) = 1401. E-6
C	C13 = CVAR(4) = 3.437 E-9
C
	CGAIN = 0.
	EYACPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
	CGAIN = 1.
C
C	DIVISION OF SIGNAL BEFORE FOLLOWER
C
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C
C	CGAIN = 3.2
C
C	EFFECT OF OUTPUT OP AMP
C	
C	FILTERING BEFORE INPUT
	Z1 = R28                 	! LAST LOAD BEFORE OP AMP
	WC12 = W*C12
	Z2 = CMPLX(R28,-1./WC12)        !  LAST RC
	Y1 = 1./CMPLX(R43,0.) + 1./Z2   !  LAST RC IN PARALLEL WITH R43
	WC29 = W*C29                    
	Z3 = 1./Y1 + CMPLX(0.,-1./WC29) !  LOAD ON FOLLOWER
	OPAMPIN = CGAIN*(Z1/Z2)*(1./Y1/Z3)
C
C	OP AMP GAIN
C
C	Z3 FROM NEG. INPUT TO GROUND
	WC13 = W*C13
	Z1 = CMPLX(R29,-1./WC13)
	WC26 = W*C26
	Z2 = CMPLX(R12,-1./WC26)
	Y1 = 1./Z1 + 1./Z2                  ! Y FROM NEG INPUT TO GROUND
	Z3 = 1./Y1                          ! Z FROM NEG INPUT TO GROUND
C	Z2 FROM OUTPUT TO NEG INPUT
	WC27 = W*C27
	Z1 = CMPLX(R11,-1./WC27)
	WC15 = W*C15
	Y1 = CMPLX(1./R32,WC15)
	Y2 = Y1 + 1./Z1 
	Z2 = 1./Y2
	CGAIN = OPAMPIN*(Z3 + Z2)/Z3
C
C	FILTERING AFTER OP AMP OUTPUT
C
	WC30 = W*C30
	RMINN = 22.1E3                ! input resistance on 210 board
	Y1 = CMPLX(1./RMINN,WC30)
	Z1 = R39 + 1./Y1
	WC25 = W*C25
C	RTNR = .42e5		      ! total TNR load
	ZMEU = 47. + CMPLX(RMEU,-1./W/CMEU)
	Y3 = 1./ZMEU + 1./Z1
	Z2 = CMPLX(0.,-1./WC25)
	CGAIN = CGAIN/(1.+Y3*Z2)/Y1/Z1	
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EYACPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION EZACPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE AC CHANNEL OF THE WIND Z PREAMP
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C	COMMON /COMPBLK/ CVAR(5)
C
C	COMPONENT DESIGNATIONS ARE SAME AS SCHEMATIC
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2,Y3,Z3,OPAMPIN,ZMEU
	DATA R9,C9,R10,CINP /18.E6,84.E-12,2.E8,4.99E-12/
	DATA C29,R43,C12,R28/ 1.E-7,47.E+3,15.E-9,100.E+3/
	DATA R29,C13,R12,C26/ 470.,3.448E-9,10.E3,92.E-9/
	DATA C15,R32,C27,R11/ 0.,33.E3,150.E-12,4.7E3/
	DATA C25,R39,C30/703.E-9,330.,32.89E-9/ 
	DATA CMEU,RMEU /20.E-9,300./
	DATA TWOPI /6.2831853/
C
C	DONE AND CHECKED
C
C 	C30 = CVAR(1)
C	C26 = CVAR(2)
C	C25 = CVAR(3)
C	C13 = CVAR(4)
C
	CGAIN = 0.
	EZACPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
	CGAIN = 1.
C
C	DIVISION OF SIGNAL BEFORE FOLLOWER
C
	Y1 = CMPLX(1./R9,W*C9)
	Y2 = CMPLX(1./R10,W*CINP)
	Z1 = 1./Y1
	Z2 = 1./Y2
	CGAIN = CGAIN*Z2/(Z1+Z2)
C
C	CGAIN = 3.2
C
C	EFFECT OF OUTPUT OP AMP
C	
C	FILTERING BEFORE INPUT
	Z1 = R28                 	! LAST LOAD BEFORE OP AMP
	WC12 = W*C12
	Z2 = CMPLX(R28,-1./WC12)        !  LAST RC
	Y1 = 1./CMPLX(R43,0.) + 1./Z2   !  LAST RC IN PARALLEL WITH R43
	WC29 = W*C29                    
	Z3 = 1./Y1 + CMPLX(0.,-1./WC29) !  LOAD ON FOLLOWER
	OPAMPIN = CGAIN*(Z1/Z2)*(1./Y1/Z3)
C
C	OP AMP GAIN
C
C	Z3 FROM NEG. INPUT TO GROUND
	WC13 = W*C13
	Z1 = CMPLX(R29,-1./WC13)
	WC26 = W*C26
	Z2 = CMPLX(R12,-1./WC26)
	Y1 = 1./Z1 + 1./Z2                  ! Y FROM NEG INPUT TO GROUND
	Z3 = 1./Y1                          ! Z FROM NEG INPUT TO GROUND
C	Z2 FROM OUTPUT TO NEG INPUT
	WC27 = W*C27
	Z1 = CMPLX(R11,-1./WC27)
	WC15 = W*C15
	Y1 = CMPLX(1./R32,WC15)
	Y2 = Y1 + 1./Z1 
	Z2 = 1./Y2
	CGAIN = OPAMPIN*(Z3 + Z2)/Z3
C
C	FILTERING AFTER OP AMP OUTPUT
C
	WC30 = W*C30
	RMINN = 22.1E3                ! input resistance on 210 board
	Y1 = CMPLX(1./RMINN,WC30)
	Z1 = R39 + 1./Y1
	WC25 = W*C25
C	RTNR = .42e5		      ! total TNR load
	ZMEU = 47. + CMPLX(RMEU,-1./W/CMEU)
	Y3 = 1./ZMEU + 1./Z1
	Z2 = CMPLX(0.,-1./WC25)
	CGAIN = CGAIN/(1.+Y3*Z2)/Y1/Z1	
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	EZACPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION BXPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE WIND X SEARCH COIL,PREAMP AND
C	INPUT STAGE ON THE 212 BOARD.
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
C	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA Q,T0,F0 / 1.47,.749,1650./     		! T0 is in Volts/nT
	DATA C10,R1,R2,C1/ .952E-6, 49.9E3, 1.E6, 1.E-9/
	DATA TWOPI /6.2831853/
C
C
	CGAIN = 0.
	BXPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
C	OUTPUT OF SEARCH COIL PLUS PREAMP, VOLTS/nT
	CGAIN = T0/CMPLX(1.,Q*(F/F0 - F0/F))
C
C	EFFECT OF INPUT ON 212 BOARD
C
	Y2 = CMPLX(1./R2,W*C1)
	Z1 = CMPLX(R1, -1./W/C10)
	CGAIN = CGAIN/(Z1*Y2)
C
C	NORMALIZE TO E PREAMPS
C
c	CGAIN = .852*CGAIN
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	BXPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION BYPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE WIND Y SEARCH COIL,PREAMP AND
C	INPUT STAGE ON THE 212 BOARD.
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA Q,T0,F0 / 1.49,.759,1560./     		! T0 is in Volts/nT
	DATA C10,R1,R2,C1/ .992E-6, 49.9E3, 1.E6, 1.002E-9/
	DATA TWOPI /6.2831853/
C
C
	CGAIN = 0.
	BYPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
C	OUTPUT OF SEARCH COIL PLUS PREAMP, VOLTS/nT
	CGAIN = T0/CMPLX(1.,Q*(F/F0 - F0/F))         ! Volts/nT
C
C	EFFECT OF INPUT ON 212 BOARD
C
	Y2 = CMPLX(1./R2,W*C1)
	Z1 = CMPLX(R1, -1./W/C10)
	CGAIN = CGAIN/(Z1*Y2)
C
C	NORMALIZE TO E PREAMPS
C
c	CGAIN = .852*CGAIN
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	BYPA = CABS(CGAIN)
	RETURN
	END
	FUNCTION BZPA(F)
C
C	THIS FUNCTION RETURNS THE GAIN OF THE WIND Z SEARCH COIL,PREAMP AND
C	INPUT STAGE ON THE 212 BOARD.
C	THE FUNCTION ITSELF RETURNS THE MAGNITUDE OF THE GAIN, AND THE PHASE
C	AND THE COMPLEX GAIN ARE RETURNED IN A COMMON BLOCK
C
	COMMON /GAINBLK/ PHASE,CGAIN        		  ! PHASE IS IN RADIANS
	COMMON /COMPBLK/ CVAR(5)
C
	COMPLEX CGAIN,Y1,Z1,Y2,Z2
	DATA Q,T0,F0 / 1.43,.763,1700./     		! T0 is in Volts/nT
	DATA C10,R1,R2,C1/ .959E-6, 49.9E3, 1.E6, 1.005E-9/
	DATA TWOPI /6.2831853/
C
C
	CGAIN = 0.
	BZPA=0.
	IF(F.EQ.0.) RETURN
	W = TWOPI*F
C	OUTPUT OF SEARCH COIL PLUS PREAMP, VOLTS/nT
	CGAIN = T0/CMPLX(1.,Q*(F/F0 - F0/F))
C
C	EFFECT OF INPUT ON 212 BOARD
C
	Y2 = CMPLX(1./R2,W*C1)
	Z1 = CMPLX(R1, -1./W/C10)
	CGAIN = CGAIN/(Z1*Y2)
C
C	NORMALIZE TO E PREAMPS
C
c	CGAIN = .852*CGAIN
C
	GNIP = AIMAG(CGAIN)
	GNRP = CGAIN
	PHASE = ATAN2(GNIP,GNRP)
	BZPA = CABS(CGAIN)
	RETURN
	END


	subroutine strupcase(in)

*	function to turn a string of characters into upper case
*	it changes only letters and not either numbers or punctuation.

	implicit none
	integer *4 length, i,raw
	character *(*) in

	length = len(in)
	do i = 1,length
	  raw = ichar(in(i:i))
	  if (raw.ge.97.and.raw.le.122) then
	    in(i:i) = char(raw - 32)
	  endif
	enddo

	return
	end
