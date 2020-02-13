	subroutine offscheck(ch,iraw,vdata,dbspec)
c
c	include 'efflen.for'                     ! file giving effective lengths
c	
c	this routine is to be called when an FFT (FFTH,FFTM or FFTL) event
c	has been found.  
c	ch is the channel number, from 'w_channel_open'in the calling prog
c	It returns iraw, vdata, dbspec, and tmspec  
c	iraw = 0 for an fft spectrum, iraw =
c		1 for "raw" = time series data, 
c	if iraw = 1, vdata contains the time series in volts/m or nT,
c		corrected for the frequency response of the system.
c	dbspec is spectrum, corrected for the frequency response, in dB 
c		with respect to  1. (volt/m)**2/hz rms, 
c		or 1. nT**2/Hz in the case of the search coils.
c	dbspec(1) is the DC term, (not logarithmic) in volt/m or nT, 
c	dbspec(2) for is the power at the fundamental frequency
c	in dbspec(n), n ranges from 1 to 513
c	tmspec is equivalent to the item returned when on-board fft's
c	are returned, i.e. 1/2 dB steps above an arbitrary reference.
c	not corrected for frequency response
c
c	for my own use, two data sets are returned in common blocks:
c		in common /rawvolts/ are returned the set of voltage
c		samples (at preamp input) not corrected for freq. response
c		in common /telem/ tmspec is the spectrum equivalent to
c		the uncorrected telemetry spectrum (an integer)
c
	common /rawvolts/ DATA(1026)
	common /telem/ tmspec(1024)
	integer*4 ch,iraw,ifftch,ipa,ok,okl,ichgp,size,return_size
	integer*4 mantissa,gain,tmspec,pwrw
	integer*4 len_index
	integer*4 w_item_i4,w_item_r4
	complex preamp,fftamp,fft_filter
	real ffund(3),dbspec(513),vdata(1024)
	character*32 item
	data size /1024/
	data ffund /21.34766,5.336914,.3335571/
c
c	This is  (3/1.05) 10**-3 volts /step.  
c	This is at gain step 3, so divide by 16.3**3
c	DATA VPSTEP/ .24E-6/              ! used to 17 FEb 1996
c	The above was in error.  
C	REDONE 30 JAN 1997.  STEVES COUNTS ARE ALSO P-P, AND FOR
C	FLIGHT PREAMPS, IT IS AVERAGE OF 1.077 COUNTS PER 3 mV
C	SEE N.B. VOL II P.80.  VOLTS/COUNT IS THEN (3 10^-3/1.077)
C	DIVIDED BY (16.3)^3 = .643E-6

C	data Volts_per_step /.24e-6/		!old value
C	data Volts_per_step /.66e-6/		!latest value from PJK 27.2.96
	data Vpstep /.643e-6/		!latest value from PJK 30.1.97
c
c	   ! now get the items
c
	   item = 'channel_number'
	   ok = w_item_i4(ch, item, ifftch, 1, return_size)
	   item = 'source'
	   ok = w_item_i4(ch, item, ipa, 1, return_size)
c
	   ichgp = (ifftch+1)/4 + 1               ! 1 = hi, 2 = mid, 3 = lo
	   fundfr = ffund(ichgp)
c
c
	IF(ipa.ge.7) THEN			! SEARCH COILS
	   item = 'BZ_LENGTH_EFF'
	   okl = w_item_R4(ch, item, efflen, 1, return_size)
	ELSEIF(ipa.eq.1.or.ipa.eq.4) then
	   item = 'EX_LENGTH_EFF'
	   okl = w_item_R4(ch, item, efflen, 1, return_size)
	ELSEIF(ipa.eq.2.or.ipa.eq.5) then
	   item = 'EY_LENGTH_EFF'
	   okl = w_item_R4(ch, item, efflen, 1, return_size)
	ELSE
	   item = 'EZ_LENGTH_EFF'
	   okl = w_item_R4(ch, item, efflen, 1, return_size)
	ENDIF
C	print*, 'ipa,efflen',ipa,efflen
C	write(37,*) 'ipa,efflen',ipa,efflen
C
	   ok = w_item_i4(ch, 'PACKET_SUBTYPE', iraw, 1, return_size)
C	 print*,'packet subtype, iraw =', iraw
	   if(iraw.ne.1) iraw = 0
	
c
	   if(iraw.eq.0) then
	     item = 'DATA'
	     ok = w_item_i4(ch, item, tmspec, size, return_size)
c
	     dbspec(1) = .5*tmspec(1)
c
	     const   =    40.*(alog10(512./vpstep)) 
     1			+ 40.*alog10(efflen)
     1			+ 20.*alog10(fundfr) - 124.	    ! half dB steps
c
	     do n = 2,513
	  	f = (n-1)*fundfr
	  	pgain = CABS(preamp(ipa,f))
	 	pgain = PGAIN*CABS(fftamp(ifftch,f))*cabs(fft_filter(ifftch,f))
	  	dbspec(n) = .5*(tmspec(n) - const)-20.*ALOG10(PGAIN)
	     enddo
c
	   endif
C
	if (iraw.eq.1) then
C
C	  WINDFFT DOES FFT AS IN FLIGHT SOFTWARE
C
	      CALL CHECKFFT(CH,IFFTCH,IPA,VDATA,TMSPEC,DBSPEC)
C
c	  print*,'ch no.,etc',ifftch,fmax,dbmax,dbspec(maxn-1),dbspec(maxn+1)
	
c     1    ,PREAMP(IPA,FMAX),FFTAMP(IFFTCH,FMAX),SPMAX
	endif
c
	return
	end
	SUBROUTINE CHECKFFT(TMCH,CHAN,IRX,VDATA,TMSPEC,SPECT)
C
C	THIS WINDFFT DOES FFT AS IN FLIGHT SOFTWARE
C	ON RETURN, DATA IS A TIME SERIES, IN TELEMETRY COUNTS BUT CORRECTED
C	FOR GAINS, TMSPEC IS IN HALF DB STEPS, IDENTICAL TO THE RETURN
C	FROM ON-BOARD FFT, AND SPECT IS THE SPECTRUM IN DB, CORRECTED FOR
C	GAINS BUT STILL RELATIVE TO AN ARBITRARY LEVEL BASED ON TELEMETRY NUMBERS
C	THE MAIN PROGRAM CONVERTS THESE TO VOLTS/M OR nT
C
	REAL*4	 WINDOW(1024)
	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),CHDATA(1024)         
	common /rawvolts/ DATA(1026)
	integer*4	gain(1024),mantissa(1024),TMSPEC(1024)
c
	COMPLEX 	PREAMP
	COMPLEX 	FFTAMP
	COMPLEX 	FFT_FILTER
	COMPLEX		CCORR,FCOEF,FCOEFT
	character*32	item
	REAL*4 		SPECT(513),DATA,VOLT(1030),OFFS(4,10)
	REAL*4		SPHASE(1024),VDATA(1024)
	REAL*4		FREQMAX,FFUND(3)
	REAL*4		HIPASS(10)
	INTEGER*4 	TMCH,size,return_size,w_item_i4
	integer*4	IRX
	integer*4	CHAN
	data size /1024/
	data ffund /21.34766,5.336914,.3335571/
	DATA HIPASS /2*85.,4*30.,4*.45/
C	DATA HIPASS /2*150.,4*8.,4*.45/     ! fftm 3 dB pt.  
C
	DATA TWOPI /6.2831853/
	DATA WINDOW /1024*1./		! PLACEHOLDER, NOT USED
C
C	OFFSETS IN FLIGHT PROMS, ACCORDING TO RED NOTEBOOK
C
	DATA OFFS /2048. ,2050. ,2046. ,2048.,   ! CH 1 GN STP 0-3
     2             2048. ,2050. ,2046. ,2051.,   ! CH 2 GN STP 0-3
     3             2048. ,2082. ,2041. ,2043.,   ! etc.
     4             2048. ,2078. ,2042. ,2046.,
     5             2048. ,2048. ,2048. ,2047.,
     6             2048. ,2075. ,2043. ,2045.,
     7             2048. ,2048. ,2048. ,2037.,
     8             2048. ,2048. ,2048. ,2040.,
     9             2048. ,2048. ,2048. ,2036.,
     X             2048. ,2048. ,2287. ,2048./
C
	NPROS = 1024
	NFRQ = NPROS/2
C******
	OFFS(1,3) = 1400.
	      item = 'MANTISSA'
	      ok = w_item_i4(tmch,item,mantissa, size, return_size)

	      item = 'EXPONENT'
	      ok = w_item_i4(tmch, item, gain, size, return_size)

C
	datamax = 0.
	DO IK = 1,1024
	  NTEMP = MANTISSA(IK).AND.4095
	  NEX = GAIN(IK).AND.3
	  TEMP = -(NTEMP-OFFS(NEX+1,CHAN))
	  CHDATA(IK) = TEMP*(-16.3)**NEX
	  datamax = amax1(abs(chdata(ik)),datamax)
	ENDDO
C	print*,'max chdata',datamax
C
	CALL TDPLOT(TMCH)
C
	RETURN
	END
