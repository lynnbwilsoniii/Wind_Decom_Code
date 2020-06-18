! wind_misc_lib.for - misc. less important (in the universal use of sense)
! routines supporting wind_lib
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

! a dummy routine to satisfy compiler/linker references from item_get.c
	integer*4       function	w_cdf_dummy()
	w_cdf_dummy = 0
	type *, 'W_CDF_DUMMY: Your system''s CDF library was not '
	type *, '	      present or could not be used when '
	type *, '             this version of WIND_LIB was compiled.'
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_simple_time_chooser(t1,t2)
	implicit	none
	real*8		t1,t2
	character*32	s
	integer*4	i,j,k,o
	integer*4	ok
	integer*4	dbms_to_ur8

  10	format(1x,a,$)
  20	format(a)
  30	format(1x,'Cannot read two integers from this: ', a)
  40	format(1x,'Cannot convert this to UR8 time: ', a)

 100	write(6,10) 'Enter start date and time [YYYYMMDD HHMMSS]: '
	read(5,20,iostat=o,err=100,end=900) s
	read(s,*,iostat=o) i, j
	if (o .ne. 0) then
	   write(6,30) s
	   goto 100
	end if
	k = 1
	ok = dbms_to_ur8(i,j,k,t1)
	if (ok .ne. 1) then
	   write(6,40) s
	   goto 100
	end if

 200	write(6,10) 'Enter stop date and time  [YYYYMMDD HHMMSS]: '
	read(5,20,iostat=o,err=200,end=900) s
	read(s,*,iostat=o) i, j
	if (o .ne. 0) then
	   write(6,30) s
	   goto 200
	end if
	k = 1
	ok = dbms_to_ur8(i,j,k,t2)
	if (ok .ne. 1) then
	   write(6,40) s
	   goto 200
	end if

	w_simple_time_chooser = 1
	return
 900	continue
	w_simple_time_chooser = 0
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_to_x_filename(t,x,f)
	implicit	none
	include		'parm_def.for'
	real*8		t
	integer*4	x
	character*(*)	f
	integer*4	i,j,k,o
	character*8	c8
	character*256	g
	integer*4	ret_size
	integer*4	ok
	integer*4	ur8_to_dbms
	integer*4	w_get_file_list_from_dir !$pragma C (w_get_file_list_from_dir)
	integer*4	file_type
	integer*4	w_ur8_to_filename
	integer*4	flags /'03'x/

	file_type = x
	goto 1000

	!------------------------------------------------------------------
	entry	w_ur8_to_filename(t, f)
	file_type = cdhf_lz_stream
	goto 1000

 1000	continue

	f = ' '
	if (file_type .eq. cdhf_lz_stream) then
	   ok = ur8_to_dbms(t,i,j,k)
	   if (ok .ne. 1) goto 900
	   write(c8,'(i8.8)',iostat=o,err=900) i
	   g = 'wi_lz_wav_'//c8//'_v*.dat'
$IF ABSOFT_FORTRAN
	   ok = w_get_file_list_from_dir(trim('WIND_DATA')//char(0),
	1	trim(g)//char(0), f, 1, len(f), ret_size, flags)
$ELSE
	   ok = w_get_file_list_from_dir('WIND_DATA', g, 
	1	f, 1, len(f), ret_size, flags)
$ENDIF
	   if (ok .ne. 1) goto 900
	else if (file_type .eq. cdhf_cdrom_stream) then
	   ! EDR's from CD-ROM:  yymmddvv.dat
	   ok = ur8_to_dbms(t,i,j,k)
	   if (ok .ne. 1) goto 900
	   if (i .lt. 20000000) then
	       i = i - 20000000
	   else
	       i = i - 19000000
	   end if
	   write(c8(1:6),'(i6.6)',iostat=o,err=900) i
	   g = c8(1:6)//'*.dat'
$IF ABSOFT_FORTRAN
	   ok = w_get_file_list_from_dir(trim('WIND_DATA')//char(0),
	1	trim(g)//char(0), f, 1, len(f), ret_size, flags)
$ELSE
	   ok = w_get_file_list_from_dir('WIND_DATA', g, 
	1	f, 1, len(f), ret_size, flags)
$ENDIF
	   if (ok .ne. 1) goto 900
	else
	   goto 900
	end if

	w_ur8_to_filename = 1
	return
 900	continue
	w_ur8_to_filename = 0
	return
	end

!------------------------------------------------------------------------------
! Get an environment variable value (logical name translation)
!------------------------------------------------------------------------------
	integer*4	function	w_get_env(env, str)
	implicit	none
	character*(*)	env
	character*(*)	str
	include		'wind_os_def.for'
	integer*4	get_file_dev_logical_name_str

	if (vms) then
	   w_get_env = get_file_dev_logical_name_str(env, str)
	else if (unixos) then
	   w_get_env = 1
$IF ABSOFT_FORTRAN
!  2008/03/03:  our usage of the Absoft compiler libraries was
!  causing the "C" version of "getenv" to be invoked, instead of the
!  Fortran version.   To get everything to work, I had to use the
!  "YEXT_LCS" compiler option (otherwise linkage lead to unresolved
!  externals).  This option appears to be what caused the "C"
!  version of "getenv" to be called, instead of the "getenv" from
!  the Absoft "U77" library.   The fix for this was to directly
!  line $ABSOFT/lib/libU77.a before the other libraries, and
!  refer, here to "getenv" as "getenv_".
	   call getenv(env, str)
$ELSE
	   call getenv(env, str)
$ENDIF
	else
	   str = ' '
	   w_get_env = 0
	end if

	return
	end


!------------------------------------------------------------------------------
! A statistical routine to compute the min, max, mean, and standard deviation
! for an array of real*4 numbers.
!------------------------------------------------------------------------------
	integer*4	function	basic_stats(a,n,rmin,rmax,mean,stdev)
	implicit	none
	real*4		a(*)		! array of real numbers
	integer*4	n		! size of array a
	real*4		rmin, rmax, mean, stdev
	real*4		sum, sumsq, count
	integer*4	i

	basic_stats = 0
	rmin  = 0.0
	rmax  = 0.0
	mean  = 0.0
	stdev = 0.0
	if (n.le.0) return
	count = float(n)
	sum   = 0.0
	sumsq = 0.0
	mean  = 0.0
	stdev = 0.0
	rmin  = +1.7e37
	rmax  = -1.7e37
	do i=1,n
	   sum = sum + a(i)
	   sumsq = sumsq + (a(i)**2.0)
	   if (a(i) .gt. rmax) rmax = a(i)
	   if (a(i) .lt. rmin) rmin = a(i)
	end do
	mean = sum/count
	stdev = (sumsq/count) - (mean**2.0)
	stdev = sqrt(abs(stdev))

	basic_stats = 1
	return
	end

!------------------------------------------------------------------------------
! A statistical routine to compute the min, max, mean, and standard deviation
! for an array of real*8 numbers.
!------------------------------------------------------------------------------
	integer*4	function	basic_stats2(a,n,rmin,rmax,mean,stdev)
	implicit	none
	real*8		a(*)		! array of real numbers
	integer*4	n		! size of array a
	real*8		rmin, rmax, mean, stdev
	real*8		sum, sumsq, count
	integer*4	i

	basic_stats2 = 0
	rmin  = 0.0
	rmax  = 0.0
	mean  = 0.0
	stdev = 0.0
	if (n.le.0) return
	count = float(n)
	sum   = 0.0
	sumsq = 0.0
	mean  = 0.0
	stdev = 0.0
	rmin  = +1.7e37
	rmax  = -1.7e37
	do i=1,n
	   sum = sum + a(i)
	   sumsq = sumsq + (a(i)**2.0)
	   if (a(i) .gt. rmax) rmax = a(i)
	   if (a(i) .lt. rmin) rmin = a(i)
	end do
	mean = sum/count
	stdev = (sumsq/count) - (mean**2.0)
	stdev = sqrt(abs(stdev))

	basic_stats2 = 1
	return
	end

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!-- Internal Analysis Routines
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! iar = internal analysis routine
! eei = event extract information
! This routine prints to standard output the final state of the event extraction
! data structure for the most recently extracted event for the given channel.
! This routine is intended primarily as an analysis aid for
! the wind_lib developer, but may also be useful for users.
!------------------------------------------------------------------------------
	integer*4	function	wind_iar_show_eei(ch)
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number

	wind_iar_show_eei = 0

  1	format(1x,a,i2)
  2	format(1x,a,a)
  3	format(1x,a,'0x',z4.4)
  4	format(1x,a,i8,4x,a,i6)
  5	format(1x,t12,6(2x,a6))
  6	format(1x,a,t12,6(3x,i4,1x))
  7	format(1x,a,i3)
  8	format(1x,a,'0x',z1)

	if (ch .lt. 1 .or. ch .gt. max_channels) then
	   type *, 'WIND_IAR_SHOW_EEI:  Invalid channel argument:', ch
	   return
	end if
	type *, ' '
	type *, 'WIND/WAVES Wind_lib Event Extraction Information Summary'
	type *, '--------------------------------------------------------'
	type 1, '              Channel: ', ch
	type 2, '                Event: ', eei(ch).event_type
	type 3, '        Event Subytpe: ', eei(ch).event_subtype
	type 4, ' Event ERT Start Date: ', eei(ch).dbms_date_time(1), 
	1       ' Event ERT Start Time: ', eei(ch).dbms_date_time(2)
	type 4, ' Event ERT Stop Date : ', eei(ch).last_date_time(1), 
	1       ' Event ERT Stop Time : ', eei(ch).last_date_time(2)
	type 8, '      Packet ID Value: ', eei(ch).packet_id
	type 7, '  Packet ID Start Bit: ', eei(ch).packet_id_start_bit
	type 7, '   Packet ID Bit Size: ', eei(ch).packet_id_bit_size
	type 7, '    First Packet Mask: ', eei(ch).first_packet_mask
	type 7, '     Last Packet Mask: ', eei(ch).last_packet_mask
	type 1, '   Got a first packet? ', eei(ch).got_a_first_packet
	type 1, '    Got a last packet? ', eei(ch).got_a_last_packet
	type *, ' '
	type 5, ' 1st  ',' 2nd  ',' 3rd  ',' 4th  ','Instrm',' TDS  '
	type 5, 'Header','Header','Header','Header',' Data ',' Fill '
	type 5, '------','------','------','------','------','------'
	type 6, 'Start Bit__:', eei(ch).h1_start_bit
	type 6, 'Bit Size___:', eei(ch).h1_bit_size,
	1		        eei(ch).h2_bit_size,
	1		        eei(ch).h3_bit_size,
	1		        eei(ch).h4_bit_size
	type 6, 'Start Byte_:', eei(ch).h1_start_byte,
	1		        eei(ch).h2_start_byte,
	1		        eei(ch).h3_start_byte,
	1		        eei(ch).h4_start_byte,
	1		        eei(ch).data_start_byte,
	1		        eei(ch).tds_start_byte
	type 6, 'End Byte___:', eei(ch).h1_end_byte,
	1		        eei(ch).h2_end_byte,
	1		        eei(ch).h3_end_byte,
	1		        eei(ch).h4_end_byte,
	1		        eei(ch).data_end_byte,
	1		        eei(ch).tds_end_byte
	type 6, 'Byte Size__:', eei(ch).h1_byte_size,
	1		        eei(ch).h2_byte_size,
	1		        eei(ch).h3_byte_size,
	1		        eei(ch).h4_byte_size,
	1		        eei(ch).data_byte_size,
	1		        eei(ch).tds_byte_size
	type *, ' '

	wind_iar_show_eei = 1
	return
	end

!------------------------------------------------------------------------------
! iar = internal analysis routine
! eb  = event buffer
! This routine prints to standard output the final state of the event buffer
! data structure for the most recently extracted event for the given channel.
! This routine is intended primarily as an analysis aid for
! the wind_lib developer, but may also be useful for users.
!------------------------------------------------------------------------------
	integer*4	function	wind_iar_show_eb(ch)
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	character*24	ert, scet
	integer*4	i
	integer*4	ios

  1	format(1x,a,i2)
  2	format(1x,a,a,i)
  3	format(1x,a,'0x',z4.4)
  4	format(1x,a,i8,4x,a,i6)
  5	format(1x,t12,6(2x,a6))
  6	format(1x,a,t12,6(3x,i4,1x))
  7	format(1x,a,i3)
  8	format(1x,a,z1)
  9	format(1x,a,i5)
 10	format(1x,8x,24(1x,i2.2))
 11	format(1x,8x,24(1x,'--'))
 12	format(1x,a8,24(1x,z2.2))

	wind_iar_show_eb = 0

	if (ch .lt. 1 .or. ch .gt. max_channels) then
	   type *, 'WIND_IAR_SHOW_EB:  Invalid channel argument.', ch
	   return
	end if

	ert = ' '
	write(ert,'(i8,1x,i6)',iostat=ios) exi(ch).ert(1), exi(ch).ert(2)
!	call wind_sys_to_ert_time(exi(ch).ert, ert)
	scet = ' '
!	call wind_sys_to_ert_time(exi(ch).scet, scet)
	write(scet,'(i8,1x,i6)',iostat=ios) exi(ch).scet(1), exi(ch).scet(2)

	type *, 'WIND/WAVES Wind_lib Event Buffer Summary'
	type *, '----------------------------------------'
	type 1, '             Channel: ', ch
	type 2, '               Event: ', eei(ch).event_type
	type 9, '       Event Subtype: ', eei(ch).event_subtype
	type 1, '     Current Packet#: ', eb(ch).cpn
	type 1, '   #Packets in Event: ',
	1    eb(ch).num_packets_to_build_this_event
	type 9, '      Last Data Byte: ', eb(ch).last_data_byte
	type *, '     ERT Major Frame: ', exi(ch).major
	type 7, '     ERT Minor Frame: ', exi(ch).minor
	type 2, '                 ERT: ', ert, exi(ch).ert1000
	type 2, '                SCET: ', scet, exi(ch).scet1000
	type *, '  DBMS Date and Time: ', eei(ch).dbms_date_time(1),
	1	eei(ch).dbms_date_time(2)
	type *, 'ERT DPU MF of ERT mf: ', exi(ch).dpu_major_ert
	type 3, '         DPU Version: ', exi(ch).dpu_version
	type 3, '         FFT Version: ', exi(ch).fft_version
	type 3, '         TDS Version: ', exi(ch).tds_version
	type *, ' '
	type 10, (i,i=0,23)
	type 11
	type 12, '1st Hdr:', (eb(ch).ph(1).h1(i), i=1,4)  !eei(ch).h1_byte_size)
	type 12, '2nd Hdr:', (eb(ch).ph(1).h2(i), i=1,24) !eei(ch).h2_byte_size)
	type 12, '3rd Hdr:', (eb(ch).ph(1).h3(i), i=1,24) !eei(ch).h3_byte_size)
	type 12, '4th Hdr:', (eb(ch).ph(1).h4(i), i=1,24) !eei(ch).h4_byte_size)
	type 12, 'Data  0:', (eb(ch).data(i), i=1,24)
	type 12, '     24:', (eb(ch).data(i), i=25,48)
	type 12, '     48:', (eb(ch).data(i), i=49,72)
	type 12, '     72:', (eb(ch).data(i), i=73,96)
	type 12, '     96:', (eb(ch).data(i), i=97,120)

	type *, ' '

	wind_iar_show_eb = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_r8_compare(arrval, target)
	implicit	none
	real*8		arrval
	real*8		target
	integer*4	i

	if (arrval .lt. target) then
	   i = 1
	else if (arrval .gt. target) then
	   i = -1
	else
	   i = 0
	end if

	w_r8_compare = i
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_r4_compare(arrval, target)
	implicit	none
	real*4		arrval
	real*4		target
	integer*4	i

	if (arrval .lt. target) then
	   i = 1
	else if (arrval .gt. target) then
	   i = -1
	else
	   i = 0
	end if

	w_r4_compare = i
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_i4_compare(ai4   , ti4   )
	implicit	none
	integer*4	arrval
	integer*4	target
	integer*4	ai4, ti4
	integer*2	ai2, ti2
	byte		ai1, ti1
	integer*4	i
	integer*4	w_ui4_compare
	integer*4	w_i2_compare
	integer*4	w_ui2_compare
	integer*4	w_i1_compare
	integer*4	w_ui1_compare
	integer*4	w_r8_compare
	real*8		a, t
	real*8		add
$IF ABSOFT_FORTRAN
	parameter	(add=2147483648.0)
$ELSE
	parameter	(add=2.0 * dfloat('40000000'x))
!	parameter	(add=2.0 * '40000000'x)
$ENDIF

	arrval = ai4
	target = ti4
	goto 1000

	entry	w_ui4_compare(ai4, ti4)
$IF ABSOFT_FORTRAN
	i = ai4 .and. int('7fffFFFF'x)
	a = i
	if ((ai4 .and. int('80000000'x)) .ne. 0) a = a + add

	i = ti4 .and. int('7fffFFFF'x)
	t = i
	if ((ti4 .and. int('80000000'x)) .ne. 0) t = t + add
$ELSE
	i = ai4 .and. '7fffFFFF'x
	a = i
	if ((ai4 .and. '80000000'x) .ne. 0) a = a + add

	i = ti4 .and. '7fffFFFF'x
	t = i
	if ((ti4 .and. '80000000'x) .ne. 0) t = t + add
$ENDIF

	w_ui4_compare = w_r8_compare(a, t)
	return

	entry	w_i2_compare(ai2, ti2)
	arrval = ai2
	target = ti2
	goto 1000

	entry	w_ui2_compare(ai2, ti2)
	arrval = zext(ai2)
	target = zext(ti2)
	goto 1000

	entry	w_i1_compare(ai1, ti1)
	arrval = ai1
	target = ti1
	goto 1000

	entry	w_ui1_compare(ai1, ti1)
	arrval = zext(ai1)
	target = zext(ti1)
	goto 1000

 1000	continue

	if (arrval .lt. target) then
	   i = 1
	else if (arrval .gt. target) then
	   i = -1
	else
	   i = 0
	end if

	w_i4_compare = i
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_linear_interpolate(x,x1,x2,y,y1,y2)
	implicit	none
	real*8		x		! r, domain value to interpolate at
	real*8		x1		! r, left end of domain
	real*8		x2		! r, right end of domain
	real*8		y		! w, range value to find
	real*8		y1		! r, range value for x1
	real*8		y2		! r, range value for x2
	real*8		m, b, rise, run

	w_linear_interpolate = 0

	rise = y 2 - y 1
	run  = x 2 - x 1

	! catch the divide by zero, but not divide overflow
	if (run .eq. 0.0) then
	   if (y1 .eq. y2) then
	      y = y1
	      w_linear_interpolate = 1
	   end if
	   return
	end if

	m = rise / run
	b = y1 - (m * x1)
	y = (m * x) + b

	w_linear_interpolate = 1
	return
	end
