! tnr.for - physical units routines for WIND/WAVES TNR instrument
!
 	integer*4 function w_physical_tnr_r4(channel,argitem,tnr_dbVolts,
	1        in_size,out_size)

!		..this function is called by the w_physical_r4 routine
!		..this function should also be a shareable routine in WINDlib

!	Updated March 1996 to include a set of DBVOLTS items - that will 
!	need to be included in the database.  


	implicit none

$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='W_PHYSICAL_TNR_R4: ')
$ELSE
	parameter	rn='W_PHYSICAL_TNR_R4: '
$ENDIF
	character*(*)	argitem
	real*4		tnr_dbVolts(*)
	real*4		tnr_phys(32)
	
	integer*4	channel_num(32)
	integer*4	s_buffer(512)
! 	integer*4       f_buffer(512)
	integer*4	in_size
	integer*4	out_size
	integer*4	channel
!	integer*4	cal
	integer*4	s_retsize
	integer*4	f_retsize
	integer*4	ret_size
	integer*4	c_retsize
!	integer*4	t_retsize
	integer*4	band
	integer*4	nchan
!	integer*4	chans
	integer*4	nspec
	integer*4	antenna
	integer*4	ant
	integer*4	s_size/512/
	integer*4	t_size/512/
	integer*4	f_size/1/
	integer*4	n_size/1/
	integer*4	event_state/-1/
	integer*4	tnr_cal
	integer*4	mode/0/
	integer*4	nn_chan(100)
	integer*4	agc(100)
	integer*4	c_size/100/
	integer*4	ii,jj
	integer*4	agc_tm
	integer*4	ok
	integer*4	counter,o_counter
	integer*4	s_temp(32)
	integer*4	f_chan(100)
	integer*4	rcvr
	integer*4	offset(4)
	logical*4	first_time /.true./
	structure /c_string_256/
	   union
	   map
	   character*256	plt_file
	   end map
	   map
	   byte		b_plt_file(256)
	   end map
	   end union
	end structure
	record /c_string_256/ cstr, cstr2
	character*32	myitem
!	integer*4	ios
	integer*4	w_cnvrt_c_fn_by_os !$pragma C (w_cnvrt_c_fn_by_os)
	integer*4	i
	integer*4	k3len
	integer*4	ch

	character*80	channel_item
	character*80	chan_item
	character*80	num_item
	character*80	time_item
	character*80	agc_item
	character*64	item

	integer*4	w_item_char !-xragma C (w_item_char)
	integer*4	w_item_i4   !-xragma C (w_item_i4)

	logical*1	mixed_mode  !set true if we get B spectra from A/B
	logical*1	short_mode  !set true if we should get short spectra
	logical*1	return_dBs  !set true if we should give back dBs

	data offset/-2,0,1,3/

	w_physical_tnr_r4 = 0
	ch = channel

	if (first_time) then
	   myitem = 'PHYSICAL_LOOKUP_TABLE'
	   ok = w_item_char(channel,myitem,cstr.plt_file,1,ret_size)
	   if (ok .ne. 1) then
	      call w_msg_put(ch,rn, 'cannot get item '//myitem)
	      return
	   end if
	   i = k3len(cstr.plt_file)
	   call null_terminate(cstr.plt_file)
	   ok = w_cnvrt_c_fn_by_os(cstr.b_plt_file, cstr2.b_plt_file, 256)
	   if (ok .ne. 1) then
	      call w_msg_put(ch, rn, 'cannot convert file specification.')
	      return
	   end if
	   call tnr_set_coeff_filename(cstr2.plt_file)
	   first_time = .false.
	end if
  1	format(1x,'W_PHYSICAL_TNR_R4: ', a, a)

	item = argitem
	call strupcase(item)

	if (item .eq. 'SPECTRA_1_MICROVOLTS_R4') then
	  item         = 'SPECTRA_1'
	  chan_item    = 'CHANS_PER_BAND_1'
	  agc_item     = 'AGC_1'
	  time_item    = 'SPECTRA_1_SCET_R8'
	  num_item     = 'NUMBER_OF_SPECTRA_1'
	  mixed_mode   = .false.
	  return_dBs   = .false.
	  short_mode   = .false.
	elseif (item .eq. 'SPECTRA_1_DBVOLTS_R4') then
	  item         = 'SPECTRA_1'
	  chan_item    = 'CHANS_PER_BAND_1'
	  agc_item     = 'AGC_1'
	  time_item    = 'SPECTRA_1_SCET_R8'
	  num_item     = 'NUMBER_OF_SPECTRA_1'
	  mixed_mode   = .false.
	  return_dBs   = .true.
	  short_mode   = .false.
	elseif (item .eq. 'SPECTRA_2_MICROVOLTS_R4') then
	  item         = 'SPECTRA_2'
	  chan_item    = 'CHANS_PER_BAND_2'
	  agc_item     = 'AGC_2'
	  time_item    = 'SPECTRA_2_SCET_R8'
	  num_item     =  'NUMBER_OF_SPECTRA_2'
	  mixed_mode   = .true.
	  return_dBs   = .false.
	  short_mode   = .false.
	elseif (item .eq. 'SPECTRA_2_DBVOLTS_R4') then
	  item         = 'SPECTRA_2'
	  chan_item    = 'CHANS_PER_BAND_2'
	  agc_item     = 'AGC_2'
	  time_item    = 'SPECTRA_2_SCET_R8'
	  num_item     =  'NUMBER_OF_SPECTRA_2'
	  mixed_mode   = .true.
	  return_dBs   = .true.
	  short_mode   = .false.
	elseif (item .eq. 'SPECTRA_SHORT_MICROVOLTS_R4') then
	  item         = 'SPECTRA_SHORT'
	  chan_item    = 'CHANS_PER_BAND_SHORT'
	  agc_item     = 'AGC_SHORT'
	  time_item    = 'SPECTRA_SHORT_SCET_R8'
	  num_item     = 'NUMBER_OF_SPECTRA_SHORT'
	  channel_item = 'NN_CHANNEL_SHORT'
	  mixed_mode   = .false.
	  return_dBs   = .false.
	  short_mode   = .true.
	elseif (item .eq. 'SPECTRA_SHORT_DBVOLTS_R4') then
	  item         = 'SPECTRA_SHORT'
	  chan_item    = 'CHANS_PER_BAND_SHORT'
	  agc_item     = 'AGC_SHORT'
	  time_item    = 'SPECTRA_SHORT_SCET_R8'
	  num_item     = 'NUMBER_OF_SPECTRA_SHORT'
	  channel_item = 'NN_CHANNEL_SHORT'
	  mixed_mode   = .false.
	  return_dBs   = .true.
	  short_mode   = .true.
	else
	   call w_msg_put(ch,rn, 'invalid item name: '//item)
	   return
	endif

!	TRY TO GET THE ITEM.  IF IT ISN'T THERE, DON'T DO THE REST.	
 	if (ok .eq. 1 ) then
	  ok = w_item_i4(channel,item,             s_buffer,s_size,s_retsize)
	  if (ok .eq. 1) then
	    ok = w_item_i4(channel,'ANTENNA',      antenna,n_size, ret_size)
	    ok = w_item_i4(channel,'EVENT_MODE',   mode,   n_size, ret_size)
	    ok = w_item_i4(channel,chan_item,      nchan,  n_size, ret_size)
	    ok = w_item_i4(channel,'EVENT_STATE',  event_state,n_size,ret_size)
	    ok = w_item_i4(channel,agc_item,       agc,    c_size, ret_size)
	    ok = w_item_i4(channel,'EVENT_BAND',   band,   f_size, f_retsize)
	    ok = w_item_i4(channel,num_item,       nspec,  n_size, c_retsize)

	    if (short_mode) then
	      ok = w_item_i4(channel,'NN_CHANNEL_SHORT', nn_chan,
	1                  c_size, c_retsize)
	    endif

!	    FILL BAND VECTOR FOR FIXED TUNE
	    if (event_state .eq. 1 ) then
	      do ii = 1,nspec
	        f_chan(ii) = band		        !fixed tune or NN
	      enddo

!	    FILL BAND VECTOR FOR ABCDE MODE
	    else if (event_state .eq. 2) then
	      do ii = 1,nspec
c*	        CORRECTION HERE CAM 3/24/95
	        f_chan(ii) = mod(ii-1,5)		!ABCDE = 01234
	      enddo

!	    FILL BAND VECTOR FOR ACE MODE
	    else if (event_state .eq. 3) then
	      do ii = 1,nspec
	          f_chan(ii) = mod((ii-1) * 2,6)
	      enddo
	    endif

!	    IDENTIFY RECEIVER
	    rcvr = 0
!	    CORRECTIONS HERE, CAM 24/3/95
	    if ( mode .eq. 1 .or. mode .eq. 3 .or.
	1       (mode .eq.4 .and. mixed_mode)) then
	      rcvr = 1
	    endif

!	    ANTENNA IS RETURNED AS A COMBINATION OF EX/EY AND MUST
!	    BE SEPARATED OUT ACCORDING TO THE RECEIVER CHOICE
	    if (rcvr .eq. 0) then 
	      ant = 1
	      if ((antenna .eq. 0) .or. (antenna .eq. 1)) ant = 0
	    else
!	      CORRECTIONS HERE, CAM 24/03/95
	      ant = 1
              if ((antenna .eq. 1) .or. (antenna .eq. 2)) ant = 2
	      if (antenna .eq. 3) ant = 0
            endif

!	    SET COUNTERS 
	    counter = 1
	    o_counter = 1

!	    WORK WITH EACH SPECTRUM IN EVENT IN TURN
	    do ii = 1,nspec 

!	      THE CHANNEL NUMBERS ARE JUST SEQUENTIAL IF THIS ISN'T NN/short 
!	      MODE, IF THEY ARE NN MODE, THEY ARE PICKED WRT THE 'CHOSEN'
!	      CHANNEL
	      do jj = 1,nchan
	        if (short_mode) then
	          channel_num(jj) = nn_chan(ii) + offset(jj)
	        else
	          channel_num(jj) = jj - 1
	        endif
	      enddo

!	      PICK UP A SINGLE SPECTRUM
	      do jj = 1,nchan
	        s_temp(jj) = s_buffer(counter)
	        counter    = counter + 1
	      enddo

!	      PICK UP STATUS VARIABLES FOR THIS SPECTRUM	   
	      band     = f_chan(ii)
	      agc_tm   = agc(ii)

!	      CALL CONVERSION ROUTINE FOR THIS SPECTRUM	
	      ok = tnr_cal(ant,rcvr,band,nchan,mode,channel_num,s_temp,agc_tm,
	1       	       tnr_phys,return_dBs)

!	      PUT CONVERTED SPECTRUM INTO OUTPUT ARRAY - CHECK SIZE!	   
	      do jj = 1,nchan
	        if (o_counter .gt. in_size) then
	          call w_msg_put2(ch,rn,
	1	  'Output array tnr_dbVolts truncated to ',in_size)
	          w_physical_tnr_r4 = 1
	          return
	        else
	          tnr_dBVolts(o_counter) = tnr_phys(jj)
	          out_size = o_counter
	        endif
	        o_counter = o_counter + 1
	      enddo
	    enddo
	  else
	    call w_msg_put(ch,rn,
	1	'Requested quantity not available for this event')
	    out_size = 0
	    w_physical_tnr_r4 = 0
	    return
	  endif
	else
	  call w_msg_put(ch,rn,
	1	'Calibration in progress - physical values invalid')
	  out_size = 0
	  w_physical_tnr_r4 = 0
	  return
	endif

!	SIGNAL SUCCESS	    
	w_physical_tnr_r4 = 1

	return
	end
	integer*4 function tnr_cal(antenna,rcvr,band,num_chans,mode,channels,
	1                  tnr_tm,agc_tm,tnr_phys,return_dBs)

!		..this function might also be a shareable routine in WINDlib

!	Written by:  C. Meetre, HSTX, January 1995
!
!	Purpose:
!	To transform a TNR spectrum in TM counts into physical units of
!	dbVolts.
!
!	Usage notes:
!	The routine is designed to be called for each spectrum, and not for
!	an array of spectra.
!	The incoming arrays will have a minimum length that of the spectra
!	they contain - i.e. 4, 16 or 32 elements depending on the mode of
!	the instrument.  Longer than the minimum is ok.
!	The returned array MUST be dimensioned by the calling program to 
!	have the same length as the incoming arrays, using the same rules.
!	
!	METHOD
!	The numerical routine is based on material supplied by Bob Manning 
!	and Claude Perche, Observatoire de Paris.
!	When the subroutine is first used, it reads in the necessary 
!	coefficient tables, and creates a lookup table that is appropriate
!	for the current configuration of the instrument.  This lookup table
!	is used until the instrument configuration changes - when a new one
!	is made.
!
!	Channel numbers are used to index the lookup table, rather than a
!	straight progression of values 0-15 or 0-31, because in the NN mode
!	the channels will be non-standard.
!
	implicit none

	integer*4	antenna     	!0=x,1=y,2=z
	integer*4	num_chans	!number of channels in spectrum(16,32,4)
	integer*4	rcvr		!0=TNR-A, 1 = TNR-B
	integer*4	band		!0..4 = A..E
	integer*4	channels(*)	!a channel # per spectral element
	integer*4	agc_tm		!an AGC value per spectrum
	integer*4	tnr_tm(*)	!a count per spectral element
!	integer*4	old_antenna	!prior state of antenna
	integer*4	mode		!0,1,2,3,4
!	integer*4	old_mode	!prior mode
	integer*4	sixteen_32	! 0 for 16 channels, 1 for 32
	
	real*4		tnr_phys(*)     !array of physical values to return

	logical*1	return_dBs

!	USE THE MODE TO SET THE SIXTEEN_32 FLAG
	sixteen_32 = 1
  	if (mode .le. 1 .or. mode .eq. 4) sixteen_32 = 0

	call tnr_calculate   (antenna,rcvr,num_chans,
	1                       sixteen_32,channels,agc_tm,tnr_tm,
	1                       band,tnr_phys,return_dBs)

	tnr_cal = 1
	
	return
	end
	subroutine tnr_calculate(antenna,rcvr,num_chans,
	1                           sixteen_32,channels,agc_tm,tnr_tm,
	1                           band,tnr_phys,return_dBs)

!	UPDATED TO PROTECT AGAINST FLOATING OVERFLOWS ESPECIALLY LIKELY
!	DURING CALIBRATIONS - BUT POSSIBLY OCCURRING AT OTHER TIMES.
!	
	implicit none
	integer*4	channels(*)
	integer*4	agc_tm
	integer*4	tnr_tm(*)
!	integer*4	index
	integer*4	band
	integer*4	i
	integer*4	antenna
	integer*4	rcvr
	integer*4	sixteen_32
	integer*4	exponent(0:255)
	integer*4	mantissa(0:255)
	integer*4	num_chans
	integer*4	a_choice
	integer*4	b_choice
!	integer*4	c_choice
	integer*4	r_choice
	integer*4	counter
	integer*4	step

	real*4		dbv0(0:4)
	real*4		dBch(0:255)
	real*4		dbcalib
	real*4		a1,a2,a3
	real*4		tnr_phys(*)

	logical*1	first
	logical*1	return_dBs

!	DIMENSIONS of DBCAL = antenna,receiver,band,16 or 32,channels
	real*4		dbcal(0:2,0:1,0:4,0:1,0:31)

!	DIMENSIONS of DBV = antenna,band,receiver,count
	real*4		dBV(0:2,0:4,0:1,0:255)

!	DIMENSIONS of A123 = antenna,receiver,band,coefficient (a1,a2,a3)
	real*4		a123(0:2,0:1,0:4,0:2)

	save dbcal,a123,exponent,mantissa

!	data dbv0/-46.36,-49.47,-52.62,-55.63,-58.64/		!old
	data dbv0/-40.36,-43.47,-46.62,-49.63,-52.64/		!9.4.96 KAG

	data first/.true./

	if (first) then
	  call tnr_read_coefficients(dbcal,a123,exponent,mantissa)

!	  FILL THE LOOKUP TABLES FOR AGC AND TNR_TM
	  do counter = 0,255
	    do a_choice = 0,2
	      do b_choice=0,4
	        do r_choice = 0,1
c*		  CORRECTION HERE, CAM 24/3/95
	          a1 = a123(a_choice,r_choice,b_choice,0)
	          a2 = a123(a_choice,r_choice,b_choice,1)
	          a3 = a123(a_choice,r_choice,b_choice,2)
	          dBV(a_choice,b_choice,r_choice,counter) = 
	1               a1 - 40.* log10(10**((counter - a3)/a2) + 1)
	        enddo
	      enddo
	    enddo
	    dBch(counter) = 
	1        10.* log10(2.**exponent(counter) * (mantissa(counter) + 8.))
	  enddo

	  first = .false.
	endif

	step = 1
	if (num_chans .eq. 16) step = 2
	do i = 0,num_chans-1
	  dbcalib = dbcal(antenna,rcvr,band,sixteen_32,channels(i+1)*step)
	  tnr_phys(i+1) = dbv0(band) + dbch(tnr_tm(i+1)) - dbcalib
	1                 - dbV(antenna,band,rcvr,agc_tm)
!	  GUARD AGAINS FLOATING OVERFLOWS - INSERTING FILL VALUES.
	  if (.not. return_dBs) then
  	    if (tnr_phys(i+1) .lt. 480) then
	      tnr_phys(i+1) = 10**((tnr_phys(i+1) + 120.)/20.)  !microVolts/(Hz**.5)
	    else
	      tnr_phys(i+1) = -99.9
	    endif
	  endif
	enddo
	
	return
	end
	subroutine tnr_read_coefficients(dbcal,a123,exponent,mantissa)

	implicit none

$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='TNR_READ_COEFF: ')
$ELSE
	parameter	rn='TNR_READ_COEFF: '
$ENDIF
	integer*4	antenna
	integer*4	receiver
	integer*4	channels
	integer*4	band
!	integer*4	num_chans
	integer*4	coeff
	integer*4	i
	integer*4	nchans
	integer*4	exponent(0:255),mantissa(0:255)
	integer*4	lun
	character*256	name
	character*(*)	file
	integer*4	ios
	integer*4	k3len

!	DIMENSIONS of DBCAL = antenna,receiver,band,16 or 32,channels
	real *4		dbcal(0:2,0:1,0:4,0:1,0:31)

!	DIMENSIONS of A123 = antenna,receiver,band,coefficient (a1,a2,a3)
	real *4		a123(0:2,0:1,0:4,0:2)

	i = k3len(name)
	if (i .le. 1) then
	   call w_msg_put_always(rn,'call tnr_set_coeff_filename first')
	   stop
	end if

	call lib$get_lun(lun)

$IF ABSOFT_FORTRAN
!
! (2007/07/16):  Absoft Fortran must replace "type='old'" with
!  "status='old'".
!
	open(unit=lun,name=name(1:i),status='old',readonly,iostat=ios)
$ELSE
	open(unit=lun,name=name(1:i),type='old',readonly,iostat=ios)
$ENDIF

	if (ios .ne. 0) then
	   call w_msg_put_always2(rn, 'cannot open '//name(1:i), ios)
	   call lib$free_lun(lun)
	   lun = 0
	   stop
	end if

!	READ IN DBCAL LOOKUP TABLE
	do receiver = 0,1                       !(TNR A, TNR B)
	  do antenna = 0,2                      !(X,Y,Z)
            do nchans = 0,1
	      do channels = 0,31                !(0,31)
                read(lun,*)
	1         (dbcal(antenna,receiver,band,nchans,channels),band=0,4)
	      enddo
	    enddo
          enddo
        enddo


!	READ IN COEFFICIENT LOOKUP TABLE
	do receiver = 0,1	                 !(TNR A, TNR B)
          do antenna = 0,2                       !(X,Y,Z)
            do band = 0,4                        !(A,B,C,D,E)
              read(lun,*)(a123(antenna,receiver,band,coeff),coeff=0,2)
	    enddo
          enddo
        enddo

	CLOSE(UNIT=lun)
	call lib$free_lun(lun)
	lun = 0

!	create table of m and e
	do i = 0,255 
          exponent(i) = i/8
          mantissa(i) = i - 8*exponent(i)
	enddo

	return

	!----------------------------------------------------------------------
	entry	tnr_set_coeff_filename(file)
	name = file
	return
	end
	subroutine strupcase(in)

!	function to turn a string of characters into upper case
!	it does only letters and not either numbers or punctuation.

	implicit none
	integer *4 length, i,raw
	character *(*) in

	length = len(in)

	do i = 1,length
	  raw = ichar(in(i:i))
	  if (raw.ge.97.and.raw.le.122) 
	1                 in(i:i) = char(raw - 32)
	enddo
	return
	end
