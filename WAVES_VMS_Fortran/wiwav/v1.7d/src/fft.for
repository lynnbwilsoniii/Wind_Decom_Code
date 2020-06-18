! fft.for - WIND/WAVES FFT Instrument's Physical Units Routines
!
	Integer*4		Function	w_physical_fft_r4(
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
*	VOLTS/METER		Any valid voltage data is returned
*	E_VOLTS/METER		Any E-field data is returned
*	EXAC_VOLTS/METER	Only the ExAC component is returned
*	EXDC_VOLTS/METER	Only the ExDC component is returned
*	EYAC_VOLTS/METER	Only the EyAC component is returned
*	EYDC_VOLTS/METER	Only the EyDC component is returned
*	EZAC_VOLTS/METER	Only the EzAC component is returned
*	EZDC_VOLTS/METER	Only the EzDC component is returned
*	B_NT2/HZ		Any B-field data is returned
*	BX_NT2/HZ		Only the Bx component is returned
*	BY_NT2/HZ		Only the By component is returned
*	BZ_NT2/HZ		Only the Bz component is returned
*	SPECTRUM_DB		Any Power spectrum - B or E
*	SPECTRUM_E_DBVOLTS      Follows the same sort of convention as above
*	SPECTRUM_B_DBNT
*	SPECTRUM_EXAC_DBVOLTS
*	SPECTRUM_EYAC_DBVOLTS
*	SPECTRUM_EZAC_DBVOLTS
*	SPECTRUM_EXDC_DBVOLTS
*	SPECTRUM_EYDC_DBVOLTS
*	SPECTRUM_EZDC_DBVOLTS
*	SPECTRUM_BX_DBNT
*	SPECTRUM_BY_DBNT
*	SPECTRUM_BZ_DBNT

*	To select and process the data we need some fundamental information
*	about the current state of the instrument.

	IMPLICIT NONE

	character	rn*11
	parameter	(rn='W_PHYSICAL_FFT_R4') ! routine name for messages
C	EFFECTIVE LENGTHS IN METERS, 1 = EX, 2 = EY, 3 = EZ, 4 = SEARCH COIL
	REAL*4 EFFLEN(4)
	logical*4	need_antenna_lengths /.true./

	integer*4	spectrum(1024)
	integer*4	n
	integer*4	antenna_int
	integer*4	chan_grp
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
	integer*4	len_index
	integer*4	pkt_subtype	! used to determine raw vs. not_raw
	integer*4	offsets(40)	! used to calculate raw voltages

	real*4		output(*)
	real*4		volts_per_step
	real*4		ffund(3)		!Fundamental frequencies for
*						  H,M,L receivers, respect..
	real*4		fftamp		
	real*4		raw_voltages(1024)
	real*4		windowed_voltages(1024)
	real*4		pgain
	real*4		const
	real*4		freq
	real*4		fund_freq

	character*(*)	item
	character*64	copy_item
	character*16	antenna

	logical*1	raw
	logical*1	spectral
	logical*1	proceed
	logical*1	need_offsets /.true./

*	FUNCTIONS
	integer*4	w_item_i4, w_item_char, w_item_r4
	real*4		preamp

*	Steve's FFT Cals, average over channel and preamp, give 1.03 
*	counts for 3mVpp input, at unity preamp gain.  3mVpp is 1.5mV 
*	pk, or 1.06 mVrms, or (1.06/1.03) 10**-3 volts 
*	rms/step.  This is at gainstep 3, so divide by 16.3**3

C	data Volts_per_step /.24e-6/		!old value
	data Volts_per_step /.66e-6/		!latest value from PJK 27.2.96

	data ffund /0.0, 0.0, 0.0/		! obtained via w_item call

	out_len = 0

*	FIND OUT WHETHER THE DATA IS RAW OR NOT.  SOMEWHAT PRIMITIVE MEANS...
	ok = w_item_i4(ch,'PACKET_SUBTYPE',pkt_subtype,1,n)
	if (ok .ne. 1) then
	   call w_msg_put(ch,rn, 'cannot get PACKET_SUBTYPE (raw/not-raw).')
	end if
	if (pkt_subtype .eq. 1) then
	    raw = .true.
	else
	    raw = .false.
	endif

*	DETERMINE WHICH ANTENNA IS IN USE
	ok = w_item_char(ch,'SOURCE',antenna,    size1,size_out)
	if (ok .ne. 1) then
	   call w_msg_put(ch,rn, 'cannot get SOURCE antenna (char)')
	end if
	ok = w_item_i4  (ch,'SOURCE',antenna_int,size1,size_out)
	if (ok .ne. 1) then
	   call w_msg_put(ch,rn, 'cannot get SOURCE antenna')
	end if
c
	IF(antenna_int.ge.7) THEN			! SEARCH COILS
		len_index = 4
	ELSEIF(antenna_int.ge.4) then
		len_index = antenna_int - 3
	ELSE
		len_index = antenna_int
	ENDIF
c
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
	   ! assume the item name is of the form SPECTRUM_*
	   spectral = .true.
	   if (copy_item(10:10) .eq. antenna(1:1) ) then
              if (copy_item(11:11) .eq. '_' ) then
	         ! gets SPECTRUM_D_* and SPECTRUM_E_*
	         proceed = .true.
	      else if (copy_item(10:13) .eq. antenna) then
	         ! gets SPECTRUM_E%%C* 
	         proceed = .true.
	      else if (copy_item(10:11) .eq. antenna) then
	         ! gets SPECTRUM_B%*
	         proceed = .true.
              endif
	   else if (copy_item(10:10) .eq. 'D') then
	      ! gets SPECTRUM_DB
	      ! THE SPECTRAL ITEM IS ALWAYS AVAILABLE IF ANTENNA IS UNSPECIFIED
	      proceed = .true.
	   endif
	endif

*	ONLY DEAL WITH THE DATA IF IT IS SOMETHING WE WANT
	if (proceed) then
*	   DETERMINE THE CHANNEL  
	   ok = w_item_i4(ch,'CHANNEL_NUMBER',fft_chan,size1,size_out)
	   if (ok .ne. 1) then
	      call w_msg_put(ch,rn, 'cannot get CHANNEL_NUMBER')
	   end if

	   ! get the antenna lengths
	   if (need_antenna_lengths) then
	      need_antenna_lengths = .false.
	      ok = w_item_r4(ch,'EX_LENGTH_EFF',efflen(1),1,n)
	      if (ok .ne. 1) then
	         call w_msg_put(ch,rn, 'Cannot get effective length of Ex.')
	      end if
	      ok = w_item_r4(ch,'EY_LENGTH_EFF',efflen(2),1,n)
	      if (ok .ne. 1) then
	         call w_msg_put(ch,rn, 'Cannot get effective length of Ey.')
	      end if
	      ok = w_item_r4(ch,'EZ_LENGTH_EFF',efflen(3),1,n)
	      if (ok .ne. 1) then
	         call w_msg_put(ch,rn, 'Cannot get effective length of Ez.')
	      end if
	      ! use Bx for search coil
	      ok = w_item_r4(ch,'BX_LENGTH_EFF',efflen(4),1,n)
	      if (ok .ne. 1) then
	         call w_msg_put(ch,rn, 'Cannot get effective length of Bx.')
	      end if
	   end if
!xxxx
	  if (raw) then
	    out_len = 1024
*	    GET THE MANTISSA AND EXPONENT FOR THE VOLTAGES
	    ok = w_item_i4(ch,'MANTISSA',mantissa,size1024,size_out)
	    if (ok .ne. 1) then
	       call w_msg_put(ch,rn, ' cannot get MANTISSA')
	    end if
	    ok = w_item_i4(ch,'EXPONENT',exponent,size1024,size_out)
	    if (ok .ne. 1) then
	       call w_msg_put(ch,rn, ' cannot get EXPONENT')
	    end if

*	    USE THESE TO CALCULATE THE RAW VOLTAGES
	    if (need_offsets) then
	       need_offsets = .false.
	       ok = w_item_i4(ch,'UNBIASING_OFFSETS', offsets, 40, n)
	       if (ok .ne. 1) then
	          call w_msg_put(ch,rn, 'Cannot get offsets.')
	       end if
	    end if

*	    REMOVE BIAS, AND WINDOW 
	    call unbias(fft_chan,mantissa,exponent,raw_voltages, offsets)

	    if (spectral) then
*	      PERFORM AN FFT ON THE INCOMING RAW DATA
	      call window(raw_voltages,windowed_voltages)
*	      GET THE "FLIGHT" TYPE INTEGER SPECTRUM
	      call windfft(windowed_voltages,spectrum)
	      out_len = 513
	    endif
	  endif                     	!raw

	  if (spectral) then	
	    if (.not. raw) then
*	       GET THE INCOMING SPECTRAL DATA
	       ok = w_item_i4(ch,'DATA',spectrum,size1024,size_out)
	       if (ok .ne. 1) then
	          call w_msg_put(ch,rn, ' cannot get item DATA')
	       end if
	    endif

*	    FIXUP SPECTRUM (WHETHER CALCULATED OR FROM TELEMETRY) AND PLACE
*	    IN OUTPUT ARRAY
	    out_len   = 513
	    chan_grp  = int((fft_chan + 1)/4) + 1
	    if (ffund(chan_grp) .eq. 0.0) then
	       ok = w_item_r4(ch,'FUNDAMENTAL_FREQUENCY_R4',
	1         ffund(chan_grp),1,n)
	       if (ok .ne. 1) then
	          call w_msg_put(ch,rn, 'cannot get FUNDAMENTAL_FREQUENCY_R4.')
	       end if
	    end if
	    fund_freq = ffund(chan_grp)

	    const     = 40. *(alog10(512./volts_per_step)) 
	1	        + 40.*alog10(efflen(len_index))
	1               + 20. * alog10(fund_freq) - 124
*	    
	    if (in_len .lt. out_len) then
	      call w_msg_put2(ch,rn, ' - truncating output to ',in_len)
	      out_len = in_len
	    endif

C	    output(1) = ?? = f ( spectrum(1) )
	    do n = 2,out_len
	      freq      = (n-1)*fund_freq
	      pgain     = preamp(antenna_int,freq)
	      output(n) = .5 * (spectrum(n) - const) - 
	1                 20.*alog10(pgain*fftamp(fft_chan,freq))
	    enddo
	  else
	    
*	    PUT VOLTAGES IN OUTPUT ARRAY, CHECKING LENGTH...
	    if (in_len .lt. out_len) then
	      call w_msg_put2(ch,rn, ' - truncating output to ',in_len)
	      out_len = in_len
	    endif

!	type *, 'len_index=', len_index
!	type *, 'efflen=', efflen(1), efflen(2), efflen(3), efflen(4)
	        
	    do n = 1,out_len
	      output(n) = raw_voltages(n)*volts_per_step/efflen(len_index)
	    enddo
	  endif

*	  SET FUNCTION FLAG TO SUCCESSFUL COMPLETION
	  w_physical_fft_r4 = 1
	else			!proceed
*	  SET FUNCTION FLAG TO UNSUCCESSFUL COMPLETION	  
	  w_physical_fft_r4 = 0
	endif

	return 
	end	 

	subroutine unbias(fft_chan,mantissa,exponent,raw_samples,offsets)
*	Purpose;
*	To remove bias from raw data that is not 32-bit integers

	implicit none

	character	rn*6
	parameter	(rn='UNBIAS')
	integer*4	fft_chan
	integer*4	mantissa(1024)
	integer*4	exponent(1024)
	real*4		raw_samples(1024)
	integer*4	offsets(40)	! used to calculate raw voltages

	integer*4	temp
*	integer*4	w_item_r4
	integer*4	iloop
	integer*4	nex
	integer*4	ntemp

	do iloop = 1,1024
	  ntemp  = mantissa(iloop) .and. 4095
	  nex    = exponent(iloop) .and. 3
	  temp   = -(ntemp - offsets((fft_chan-1)*4 + nex+1))
	  raw_samples(iloop) = float(temp) * (-16.3)**nex
!	  raw_samples(iloop) = float(temp) * (-16)**nex		!flight style
	enddo
	return
	end

	subroutine window(raw_voltages,windowed_voltages)
*	TO APPLY THE APPROPRIATE WINDOWING FUNCTION TO THE RAW DATA

	implicit none

	real*4		raw_voltages(1024)
	real*4		windowed_voltages(1024)

	real*4		window_factors(1024)
	integer*4	iwindow,inormal
	integer*4	i
	
	IWINDOW = 1				!get the Hamming window
	INORMAL = 1				!with the Kellogg normalization
	CALL FWINDW(IWINDOW,INORMAL,window_factors)
C
	DO i = 1,1024
	   windowed_voltages(i) = window_factors(i) * raw_voltages(i)
	ENDDO

	return
	end

	SUBROUTINE WINDFFT(VOLTIN,integer_spectrum)
C
C	THIS WINDFFT DOES FFT AS IN FLIGHT SOFTWARE
C

	implicit none

	integer*4	nfrq
$IF ABSOFT_FORTRAN
!	integer*4	npros
$ELSE
	integer*4	npros
$ENDIF
	integer*4	n
	integer*4	integer_spectrum(1024)

	REAL*4 		PWRW(1024),VOLTIN(1024),VOLT(1030)
	real*4		avr		

	NFRQ = 512
c	this is done because realft requires an array of size(2*n+2)
	do n = 1,1024
	  volt(n) = voltin(n)
	enddo
        CALL REALFT(VOLT,NFRQ,1)
	AVR = VOLT(1)
C	
	IF(AVR.GT.0.) THEN
	    PWRW(1) = 40.*ALOG10(AVR) - 124.
	ELSE
	    PWRW(1) = 0.
	ENDIF
	integer_spectrum(1) = int(pwrw(1))

C	CALCULATE POWER 
C
c	114.4 is 20.*log10(1024**2/2), to make fasfor normalization agree
c	with the flight norm which subtracts 124.

	DO N = 2,NFRQ+1
	  PWRW(N) = VOLT(2*N-1)**2 + VOLT(2*N)**2
	  IF(PWRW(N).GT.0.) THEN
	    PWRW(N) = 20.*ALOG10(pwrw(N)) - 124.   ! .5 db steps,fl bias
	  ELSE
	    PWRW(N) = 0.
	  ENDIF
	  integer_spectrum(N) = int(pwrw(n))
	ENDDO

	RETURN
	END

	FUNCTION FFTAMP(CHANNEL,F)

C	THIS IS THE GAIN OF A CIRCUIT JUST BEFORE THE FFT D/A CONVERTER
C
C	PHASE IS IN RADIANS

	INTEGER*4 CHANNEL
	COMPLEX Z1, Y2, GAIN
	REAL RSW(6), C2MUF(6), F
	DATA RSW        /50., 400., 400., 0., 0., 0./
	DATA C2MUF      /1.961, 1.896, 1.949, 1.981, 2.011, 1.943/
	DATA C1, R2, C2 /2.E-6, 1.E4, 1.E-9/
	DATA TWOPI      /6.2831853/
C
	GAIN   = CMPLX(1.,0.)
	PHASE  = 0.
	FFTAMP = 1.
	IF(CHANNEL.GT.6) RETURN

	W    = TWOPI*F
	C1   = C2MUF(CHANNEL)*1.E-6
	RSWT = RSW(CHANNEL)
	Z1   = CMPLX(RSWT,-1./W/C1)
	Y2   = CMPLX(1./R2,W*C2)
	GAIN = 1./(1. + Y2*Z1)
C
	GNIP   = AIMAG(GAIN)
	GNRP   = GAIN
	PHASE  = ATAN2(GNIP,GNRP)
	FFTAMP = CABS(GAIN)

	RETURN
	END
