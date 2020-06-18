! wind_item_lib.for - source for WIND/WAVES Item routines, including 
! wind_tm_get_item, wind_tm_get_item_xlate (translation [xlate] module),
! and other item information routines.
!


!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_item_from_all
	1				(ch, item, a, size, ret_size)
! Temporarily sets global current event name to be "GLOBAL" and calls
! the regular wind_tm_get_item.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	integer*4	ch				! TM channel number
	character*(*)	item				! name of item
	integer*4	a(*)				! array of returned data
	integer*4	size				! size of array a
	integer*4	ret_size			! # i*4's written to a
	character*32	c32
	integer*4	ok
	integer*4	wind_tm_get_item

	c32 = eei(ch).event_type
	eei(ch).event_type = 'GLOBAL'//char(0)

	ok = wind_tm_get_item(ch, item, a, size, ret_size)

	eei(ch).event_type = c32

	wind_tm_get_item_from_all = ok

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_item_routines
	1				(ch, item, ai4, size, ret_size)
! This routine initializes the user's arguments and other arguments for a call
! to the "secret" routine w_really_get_item.  W_really_get_item is
! where the item retrieval through the item definition database is really
! performed.
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'wind_os_def.for'
	include		'item_def.for'
	integer*4	ch				! TM channel number
	character*(*)	item				! name of item
	integer*4	ai4(*)				! array of returned data
	integer*4	size				! size of array a
	integer*4	ret_size			! # i*4's written to a
	integer*4	ok				! return status value
	integer*4	w_really_get_item		! a function
	external	w_really_get_item !$pragma C( w_really_get_item )
	integer*4	w_save_this_psni
	external 	w_save_this_psni  !$pragma C (w_save_this_psni)
	integer*4	k2len				! a function
	integer*4	i,j,k
	integer*4	bit_start
	integer*4	callers_type			! caller's expected type
	integer*4	ret_type			! data type of item
	integer*4	arg_size
	record /c_string_36/ my_item
	integer*4	wind_tm_get_item		! an entry point
	integer*4	w_item_i4_old			! an entry point
	integer*4	w_item_r4_old			! an entry point
	real*4		ar4(*)
	integer*4	w_item_r8_old			! an entry point
	real*8		ar8(*)
	integer*4	w_item_char_old			! an entry point
	character*(*)	str(*)
	integer*4	ios
	structure /i4_or_r4/
	   union
	   map
	      integer*4	i4
	   end map
	   map
	      real*4	r4
	   end map
	   end union
	end structure
	record /i4_or_r4/ i_o_r
	integer*4	max_ints
	integer*4	max_doubles
	integer*4	max_floats
	integer*4	max_bytes
	parameter	(max_ints = 4096*2)
	parameter	(max_floats = max_ints)
	parameter	(max_doubles = max_ints/2)
	parameter	(max_bytes = max_ints*4)
	structure /multi_data_types/
	   union
	   map
	      integer*4	i4(max_ints)
	   end map
	   map
	      real*4	r4(max_floats)
	   end map
	   map
	      real*8	r8(max_doubles)
	   end map
	   map
	      byte	b(max_bytes)
	   end map
	   map
	      character*(max_bytes)	c
	   end map
	   end union
	end structure
	record /multi_data_types/ mdt
	integer*4	arg_buf
	integer*4	cpy_and_cnvt_i4_to_ch

	!----------------------------------------------------------------------
	entry	wind_tm_get_item(ch, item, ai4, size, ret_size)
	entry	w_item_i4_old(ch, item, ai4, size, ret_size)
	do i=1,size
	   ai4(i) = 0
	end do
	arg_size = size
$IF ABSOFT_FORTRAN
	arg_buf = loc(ai4)
$ELSE
	arg_buf = %loc(ai4)
$ENDIF
	callers_type = p_int_return
	goto 1000

	!----------------------------------------------------------------------
	entry	w_item_r4_old(ch, item, ar4, size, ret_size)
	do i=1,size
	   ar4(i) = 0.0
	end do
	arg_size = size
$IF ABSOFT_FORTRAN
	arg_buf = loc(ar4)
$ELSE
	arg_buf = %loc(ar4)
$ENDIF
	callers_type = p_float_return
	goto 1000

	!----------------------------------------------------------------------
	entry	w_item_r8_old(ch, item, ar8, size, ret_size)
	do i=1,size 
	   ar8(i) = 0.0
	end do
	arg_size = size
$IF ABSOFT_FORTRAN
	arg_buf = loc(ar8)
$ELSE
	arg_buf = %loc(ar8)
$ENDIF
	callers_type = p_double_return
	goto 1000

	!----------------------------------------------------------------------
	entry	w_item_char_old(ch, item, str, size, ret_size)
	i = len(str(1))
	do j=1,size
	   str(j) = ' '
	end do
	arg_size = max_bytes
$IF ABSOFT_FORTRAN
	arg_buf = loc(mdt.c(1:1))
$ELSE
	arg_buf = %loc(mdt.c(1:1))
$ENDIF
	do i=1,max_ints
	   mdt.i4(i) = 0
	end do
	callers_type = p_char_return
	goto 1000

 1000	continue

	wind_tm_get_item_routines = 0

	! initialize the argument list
	if (size .lt. 1) return
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10
	i = k2len(item)
	if (i .lt. 1) goto 20
	i = i + 1
	my_item.c = item
	my_item.c(i:i) = char(0)
	call to_upper(my_item.c,1,i-1)
	ret_type = callers_type
	ret_size = 0
	bit_start = 0
	if (arg_size .eq. 0) goto 50

	ok = w_really_get_item(
	1	%val(ch-1),
	1	eei(ch).event_type_b,
	1	my_item.b,
	1	%val(arg_buf),
	1	%val(arg_size),		! size in input data type units
	1	ret_size,		! return size: output data type units
	1	bit_start,
	1	ret_type,		! the return data type of the item
	1	exi(ch).psni)		! ptr to item, for auxill. info
	k = w_save_this_psni(%val(ch), %val(exi(ch).psni))
	if (ok .ne. 1) goto 30

	if (callers_type .eq. ret_type) then
	   ! copy item to user's buffer for character, otherwise ok
	   if (callers_type .eq. p_char_return) then
	      k = 1
	      j = len(str(1))
	      do i=1,min(ret_size, (j*size))
	         if (i .gt. (k*j)) k = k + 1
	         str(k)(i:i) = mdt.c(i:i)
	      end do
	   end if
	else if (callers_type .eq. p_float_return .and. 
	1        ret_type .eq. p_int_return) then
	   do i=1,ret_size ! min(size,ret_size)
	      i_o_r.r4 = ar4(i)
	      ar4(i) = real(i_o_r.i4)
	   end do
	else if (callers_type .eq. p_double_return .and. 
	1        ret_type .eq. p_int_return) then
	   call cpy_and_cnvt_i4_to_r8(ar8, ret_size, ar8, size)
	else if (callers_type .eq. p_double_return .and. 
	1        ret_type .eq. p_float_return) then
	   call cpy_and_cnvt_r4_to_r8(ar8, ret_size, ar8, size)
	else if (callers_type .eq. p_char_return .and.
	1        ret_type .eq. p_int_return) then
	      ok = cpy_and_cnvt_i4_to_ch(str, size, 
	1	mdt.i4, ret_size, exi(ch).psni)
	else
	   goto 40
	end if

	wind_tm_get_item_routines = ok

	return
  1	format(1x,'WIND_TM_GET_ITEM: ', a, a)
  2	format(1x,4x, 'Caller requested type: ', a, '.')
  3	format(1x,4x, 'Item database returned type: ', a, '.')
 10	wind_tm_get_item_routines = w_bad_channel
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'invalid channel number.'
	return
 20	wind_tm_get_item_routines = w_invalid_argument
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'length of item string is less than zero.'
	return
 30	wind_tm_get_item_routines = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'cannot get value for item: ', item
	return
 40	wind_tm_get_item_routines = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'item data type conversion error.'
	if (callers_type .eq. p_int_return) then
	   write(6,2,iostat=ios) 'INTEGER'
	   do i=1,size
	      ai4(i) = 0
	   end do
	else if (callers_type .eq. p_float_return) then
	   write(6,2,iostat=ios) 'REAL*4'
	   do i=1,size
	      ar4(i) = 0.0
	   end do
	else if (callers_type .eq. p_double_return) then
	   write(6,2,iostat=ios) 'REAL*8'
	   do i=1,size 
	      ar8(i) = 0.0
	   end do
	else if (callers_type .eq. p_char_return) then
	   write(6,2,iostat=ios) 'CHARACTER'
	   str(1) = ' '
	else
	   write(6,2,iostat=ios) 'UNKNOWN'
	end if
	ret_size = 0
	if (ret_type .eq. p_char_return) then
	   write(6,3,iostat=ios) 'CHARACTER'
	else if (ret_type .eq. p_int_return) then
	   write(6,3,iostat=ios) 'INTEGER'
	else if (ret_type .eq. p_float_return) then
	   write(6,3,iostat=ios) 'REAL*4'
	else if (ret_type .eq. p_double_return) then
	   write(6,3,iostat=ios) 'REAL*8'
	else
	   write(6,3,iostat=ios) 'UNKNOWN'
	end if
	return
 50	wind_tm_get_item_routines = 0
	write(6,1,iostat=ios) 'You have specified a zero length buffer: ', item
	return
	end
!------------------------------------------------------------------------------
! ai4 and ar8 share the same memory, that is i*4 values have been overlain
! onto the ar8 locations and this routine unravels them by allocation and type.
!------------------------------------------------------------------------------
	subroutine	cpy_and_cnvt_i4_to_r8( ai4, sz_i4, ar8, sz_r8 )
	implicit	none
	integer*4	sz_i4		! size of the integer*4	array
	integer*4	sz_r8		! size of the real*8 array
	integer*4	ai4(sz_i4)
	real*8		ar8(sz_r8)
	integer*4	i,j
	integer*4	end_i
	integer*4	end_8
	integer*4	max_vals
	character*(*)	rn
	parameter	(rn='CPY_AND_CNVT_I4_TO_R8')

	if (sz_i4 .gt. (sz_r8*2)) then
	   type *, rn, ':', ' ERROR, buffer overflow!'
	   return
	end if

	max_vals = min(sz_r8, sz_i4)
	end_i = max_vals
	end_8 = max_vals

	i = end_i
	do j=end_8,1,-1
	   ar8(j) = dble(ai4(i))
	   i = i - 1
	end do

	return
	end
!------------------------------------------------------------------------------
! ar4 and ar8 share the same memory, that is r*4 values have been overlain
! onto the ar8 locations and this routine unravels them by allocation and type.
!------------------------------------------------------------------------------
	subroutine	cpy_and_cnvt_r4_to_r8( ar4, sz_r4, ar8, sz_r8 )
	implicit	none
	integer*4	sz_r4		! size of the integer*4	array
	integer*4	sz_r8		! size of the real*8 array
	real*4		ar4(sz_r4)
	real*8		ar8(sz_r8)
	integer*4	i,j
	integer*4	end_i
	integer*4	end_8
	character*(*)	rn
	parameter	(rn='CPY_AND_CNVT_R4_TO_R8')

	if (sz_r4 .gt. (sz_r8*2)) then
	   type *, rn, ':', ' ERROR, buffer overflow!'
	   return
	end if

	end_i = sz_r8
	end_8 = sz_r8

	i = end_i
	do j=end_8,1,-1
	   ar8(j) = dble(ar4(i))
	   i = i - 1
	end do

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	cpy_and_cnvt_i4_to_ch
	1				(str, size, i4, sz_i4, psni)
	character*(*)	str(*)
	integer*4	size
	integer*4	i4(*)
	integer*4	sz_i4
	integer*4	psni
	integer*4	wid_do_xlate_cpy		! a function
	external	wid_do_xlate_cpy !$pragma C( wid_do_xlate_cpy )
	integer*4	i,j
	integer*4	return_code
	integer*4	sz_el

	cpy_and_cnvt_i4_to_ch = 0
	return_code = 1
	sz_el = len(str(1))

	do j=1,min(size, sz_i4)
	   ok = wid_do_xlate_cpy(
	1	%val(psni), 
	1	i4(j), 
$IF ABSOFT_FORTRAN
	1	%val( loc(str(j)(1:1)) ), 
$ELSE
	1	%val( %loc(str(j)(1:1)) ), 
$ENDIF
	1	%val(sz_el), 
	1	%val(0))
	   if (ok .ne. 1) then
	      return_code = 0
	   else
	      ! replace the trailing null characters with spaces
	      ! (idl chokes on this)
	      do i=1,sz_el
	         if (str(j)(i:i) .lt. ' ') str(j)(i:i) = ' '
	      end do
	   end if
	end do

	cpy_and_cnvt_i4_to_ch = return_code

	return
	end
!------------------------------------------------------------------------------
! This routine takes the event, item_name, and item_value input arguements
! and uses them to index the table of string equivalents for the integer
! item values.
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_xlate_item
	1				(ch,event,item_name,item_val,item_xlate)
	implicit	none
$IF ABSOFT_FORTRAN
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'wind_os_def.for'
	include		'item_def.for'
$ELSE
	include		'wind_tm_user_def.for/nolist'
	include		'wind_tm_event_def.for/nolist'
	include		'wind_return_code_def.for/nolist'
$ENDIF

	integer*4	ch		! caller's channel number
	character*(*)	event		! event name
	character*(*)	item_name	! item name
	integer*4	item_val	! integer item value
	character*(*)	item_xlate	! returned string equivalent

	integer*4	ok
	integer*4	max_event_name_size
	integer*4	max_item_name_size
	integer*4	max_xlate_str_size
	parameter	(max_event_name_size=36)
	parameter	(max_item_name_size =32)
	parameter	(max_xlate_str_size =32)
	character*16	c_event
	byte		b_event(max_event_name_size)
	equivalence	(c_event, b_event)
	character*32	c_item
	byte		b_item(max_item_name_size)
	equivalence	(c_item, b_item)
	character*32	c_xlate
	byte		b_xlate(max_xlate_str_size)
	equivalence	(c_xlate, b_xlate)
	integer*4	k
	integer*4	k2len
	integer*4	wid_get_item_xlate
	external	wid_get_item_xlate !$pragma C ( wid_get_item_xlate )
	integer*4	w_item_xlate		! an entry point
	integer*4	ios

	!----------------------------------------------------------------------
	entry	w_item_xlate(ch,event,item_name,item_val,item_xlate)

	wind_tm_xlate_item = 0
	item_xlate = ' '
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 8

	! initialize the arguments for the call to get_item_xlate
	c_event = event
	k = min(k2len(c_event) + 1, max_event_name_size)
	c_event(k:k) = null
	!
	c_item = item_name
	k = min(k2len(c_item) + 1, max_item_name_size)
	c_item(k:k) = null
	!
	c_xlate = ' '
	b_xlate(max_xlate_str_size) = 0

	ok = wid_get_item_xlate(%val(ch-1), b_event, b_item, 
	1	item_val, b_xlate, %val(max_xlate_str_size), %val(0))
	if (ok.ne.1) goto 20

	k = k2len(c_xlate) + 1
	if (k.le. max_xlate_str_size) c_xlate(k:) = ' '
	item_xlate = c_xlate

	wind_tm_xlate_item = 1
	return
   1	format(1x,'W_ITEM_XLATE: ', a, :, a6, a20)
  8	wind_tm_xlate_item = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'invalid channel number.'
	return
! 10	wind_tm_xlate_item = ok
!	if (user(ch).suppress_messages) return
!	write(6,1,iostat=ios) 'cannot find database item ', event, item_name
!	return
 20	wind_tm_xlate_item = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'cannot find translation for ', event, item_name
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_get_addr_of_user_struct
	1		(ch,area,addr,size,is_little_endian)
! This routine returns the address of a specified internal wind/waves
! buffer for access by w_item* routines as part of the "current" event.
!
! Note that alignment of variables with respect to machine dependant 
! boundaries is critical here.
!
	implicit	none
	include		'wind_os_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	area				! area code from .db 
	integer*4	addr				! a pointer
	integer*4	size				! byte size of area
	integer*4	is_little_endian		! int encoding flag
	integer*4	cpn
	integer*4	o
	integer*4	addr2

	w_get_addr_of_user_struct = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10
	cpn = eb(ch).cpn
	is_little_endian = 1

	if (area .eq. ichar('1')) then			! in primary header
	   if (.not. eb(ch).ph(cpn).got_1st) goto 21
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).ph(cpn).h1(1))
$ELSE
	   addr = %loc(eb(ch).ph(cpn).h1(1))
$ENDIF
	   size = eei(ch).h1_byte_size
	else if (area .eq. ichar('2')) then		! in secondary header
	   if (.not. eb(ch).ph(cpn).got_2nd) goto 22
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).ph(cpn).h2(1))
$ELSE
	   addr = %loc(eb(ch).ph(cpn).h2(1))
$ENDIF
	   size = eei(ch).h2_byte_size
	else if (area .eq. ichar('3')) then		! in tertiary header
	   if (.not. eb(ch).ph(cpn).got_3rd) goto 23
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).ph(cpn).h3(1))
$ELSE
	   addr = %loc(eb(ch).ph(cpn).h3(1))
$ENDIF
	   size = eei(ch).h3_byte_size
	else if (area .eq. ichar('4')) then		! in 4th header
	   if (.not.eb(ch).ph(cpn).got_4th) goto 24
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).ph(cpn).h4(1))
$ELSE
	   addr = %loc(eb(ch).ph(cpn).h4(1))
$ENDIF
	   size = eei(ch).h4_byte_size
	else if (area .eq. ichar('5')) then		! in data area of pkt
	   if (.not.eb(ch).ph(cpn).got_data) goto 25
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).data(1))
$ELSE
	   addr = %loc(eb(ch).data(1))
$ENDIF
	   size = eb(ch).last_data_byte
	else if (area .eq. ichar('6')) then		! in extra info section
$IF ABSOFT_FORTRAN
	   addr = loc(exi(ch).extra_info(1))
$ELSE
	   addr = %loc(exi(ch).extra_info(1))
$ENDIF
	   size = n_extra_info_lws * 4
	   if (sunos) is_little_endian = 0
	   if (macos_ppc) is_little_endian = 0
	else if (area .eq. ichar('B')) then		! in extract info sectn
$IF ABSOFT_FORTRAN
	   addr  = loc(eei(ch).event_type_b)
	   addr2 = loc(eei(ch).got_a_last_packet)
$ELSE
	   addr  = %loc(eei(ch).event_type_b)
	   addr2 = %loc(eei(ch).got_a_last_packet)
$ENDIF
	   size = addr2 - addr + 4
	   if (sunos) is_little_endian = 0
	   if (macos_ppc) is_little_endian = 0
	else if (area .eq. ichar('8')) then		! in associated HK area
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).hk(1))
$ELSE
	   addr = %loc(eb(ch).hk(1))
$ENDIF
	   size = max_len_hk
	else if (area .eq. ichar('A')) then		! in user channel struc
$IF ABSOFT_FORTRAN
	   addr = loc(user(ch).channel)
	   size = loc(user(2).channel) - loc(user(1).channel)
$ELSE
	   addr = %loc(user(ch).channel)
	   size = %loc(user(2).channel) - %loc(user(1).channel)
$ENDIF
	   if (sunos) is_little_endian = 0
	   if (macos_ppc) is_little_endian = 0
	else if (area .eq. ichar('P')) then
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).packet(0))
$ELSE
	   addr = %loc(eb(ch).packet(0))
$ENDIF
	   size = size_of_packet
	else if (area .eq. ichar('M')) then
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).major_frame(0))
$ELSE
	   addr = %loc(eb(ch).major_frame(0))
$ENDIF
	   size = sizeof_major_frame
	else if (area .eq. ichar('m')) then
$IF ABSOFT_FORTRAN
	   addr = loc(eb(ch).minor_frame(0))
$ELSE
	   addr = %loc(eb(ch).minor_frame(0))
$ENDIF
	   size = sizeof_minor_frame
	else
	   goto 60
	end if

	w_get_addr_of_user_struct = 1
	return
  1	format(1x,'W_GET_ADDR_OF_USER_STRUCT: ', a, :, i)
 10	continue
	write(6,1,iostat=o) 'invalid channel=', ch
	return
 21	continue
	! w_get_addr_of_user_struct = w_no_1st_header_for_item
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'Primary header not present.'
	return
 22	continue
	!  w_get_addr_of_user_struct = w_no_2nd_header_for_item
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'Secondary header not present.'
	return
 23	continue
	!  w_get_addr_of_user_struct = w_no_3rd_header_for_item
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'Tertiary header not present.'
	return
 24	continue
	!  w_get_addr_of_user_struct = w_no_4th_header_for_item
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'Fourth header not present.'
	return
 25	continue
	!  w_get_addr_of_user_struct = w_no_data_area_for_item
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'Packet data area not present.'
	return
 60	continue
	if (user(ch).suppress_messages) return
	write(6,1,iostat=o) 'invalid area code: ', area
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wid_get_item_source_file(event,file)
	implicit	none
	include		'c_string_def.for'
	character*(*)	event
	character*(*)	file
	integer*4	w_get_item_source_file		! a function
	external	w_get_item_source_file !$pragma C(w_get_item_source_file)
	integer*4	ok
	record/c_string_36/ ev
	record/c_string_256/ fi
	integer*4	null_terminate

	file = ' '
	ev.c = event
	fi.c = file
	ok = null_terminate(ev.c)

	ok = w_get_item_source_file(ev.b, fi.b)
	if (ok .eq. 1) file = fi.c
	wid_get_item_source_file = ok

	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wid_get_event_name_list(list,dim,size_e)
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'
	include		'wind_os_def.for'
	include		'item_def.for'
	character*(*)	list(*)				! buffer
	integer*4	dim				! number of elements
	integer*4	size_e				! size of each element
	integer*4	w_get_event_name_list		! a function
	external	w_get_event_name_list !$pragma C(w_get_event_name_list)
	integer*4	ok
	integer*4	addr
	integer*4	i,j
	record /c_string_64/ spec
	integer*4	max_db_files
	parameter	(max_db_files=32)
	record /c_string_256/ files(max_db_files)
	character*80	work
	integer*4	w_get_file_list2
	integer*4	w_trim_to_event_name
	integer*4	ch_heapsort
	integer*4	rm_dups_in_sorted_array
	integer*4	ios

	wid_get_event_name_list = 0

	do i=1,dim
	   list(i) = ' '
	end do

	if (vms) then
	   i = max_db_files
	   spec.c = 'wind_dbms:items_*.db '
	   ok = w_get_file_list2(files(1).c, i, spec.c)
	   if (i .lt. 1 .or. ok .ne. 1) then
	      write(6,1) 'cannot get file list of ', spec.c
	      return
	   end if
	   ok = w_trim_to_event_name(files(1).c, i)
	   if (ok .ne. 1) then
	      write(6,1) 'cannot get names from file specs.'
	      return
	   end if
	   ok = ch_heapsort(files(1).c,i,work)
	   if (ok .ne. 1) then
	      write(6,1) 'error sorting event names.'
	      return
	   end if
	   ok = rm_dups_in_sorted_array(files(1).c, i)
	   if (ok .ne. 1) then
	      write(6,1) 'error removing duplicate event names.'
	      return
	   end if
	   do j=1,min(i,dim)
	      list(j) = files(j).c
	   end do
	   if (i .gt. dim) write(6,2,iostat=ios) 
	1	i-dim, ' names lost, buffer too small.'
	else if (sunos) then
$IF ABSOFT_FORTRAN
	   addr = loc(list(1)(1:1))
$ELSE
	   addr = %loc(list(1)(1:1))
$ENDIF
	   ok = w_get_event_name_list(%val(addr), %val(dim), %val(size_e))
	else if (macos_ppc) then
$IF ABSOFT_FORTRAN
	   addr = loc(list(1)(1:1))
$ELSE
	   addr = %loc(list(1)(1:1))
$ENDIF
	   ok = w_get_event_name_list(%val(addr), %val(dim), %val(size_e))
	else if (macos_intel) then
$IF ABSOFT_FORTRAN
	   addr = loc(list(1)(1:1))
$ELSE
	   addr = %loc(list(1)(1:1))
$ENDIF
	   ok = w_get_event_name_list(%val(addr), %val(dim), %val(size_e))
	end if

	wid_get_event_name_list = ok

	return
 1	format('WID_GET_EVENT_NAME_LIST: ', a, :, a28)
 2	format('WID_GET_EVENT_NAME_LIST: ', i, a)
	end
!------------------------------------------------------------------------------
	integer*4	function	w_trim_to_event_name(f,nf)
	implicit	none
	character*(*)	f(*)
	integer*4	nf
	integer*4	i,j,k

	w_trim_to_event_name = 0

	do i=1,nf
	   j = index(f(i),'.DB;')
	   if (j .eq. 0) then
	      type *, 'This is not a .db item file: '
	      type *, f(i)
	      return
	   end if
	   j = j - 1
	   k = j
	   do while(j .gt. 0 .and. f(i)(j:j) .ne. ']')
	      j = j - 1
	   end do
	   if (j .eq. 0) then
	      type *, 'Where is the "]" ?'
	      type *, f(i)
	      return
	   end if
	   j = j + 7 ! the length of 'items_' is 6, then plus 1 = 7
	   if (j .gt. k) then
	      type *, 'Where is the event name ?'
	      type *, f(i)
	      return
	   end if
	   f(i) = f(i)(j:k)
	end do

	w_trim_to_event_name = 1
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wid_goto_item
	1	(event_name, item_number, same_name_item_number, item_name)
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'wind_os_def.for'
	include		'item_def.for'
	character*(*)	event_name		! read
	character*(*)	item_name		! write
	integer*4	item_number		! read
	integer*4	same_name_item_number	! read
	integer*4	w_goto_nth_sni			! a function
	external	w_goto_nth_sni !$pragma C( w_goto_nth_sni )
	integer*4	w_goto_nth_item			! a function
	external	w_goto_nth_item !$pragma C( w_goto_nth_item )
	integer*4	k2len				! a function
	integer*4	k
	integer*4	ok
	record /c_string_36/ ev, prev_ev
	record /c_string_64/ it
	integer*4	ppin
	integer*4	ios
	integer*4	nth_item /-1/
	logical*4	goto_nth

	wid_goto_item = 0

	if (user(1).channel .ne. 0) goto 10

	ev.c = event_name
	call to_upper(ev.c,0,0)
	k = k2len(ev.c) + 1
	ev.c(k:k) = char(0)

	goto_nth = .true.
	if (ev.c .eq. prev_ev.c) then
	   if (nth_item .eq. item_number) then
	      goto_nth = .false.
	   end if
	else
	   prev_ev.c = ev.c
	end if
	nth_item = item_number

	if (goto_nth) then
	   item_name = ' '
	   ok = w_goto_nth_item( ev.b, it.b, ppin, exi(1).psni, item_number )
	   if (ok .ne. 1) goto 20
	   k = k2len(it.c)
	   item_name = it.c(1:k)
	end if

	ok = w_goto_nth_sni( ppin, exi(1).psni, same_name_item_number )

	wid_goto_item = ok

	return
  1	format(1x,
	1    'WID_GOTO_ITEM: cannot use this routine with open channel')
 10	continue
	write(6,1,iostat=ios) 
	return
 20	continue
	wid_goto_item = ok
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_item_format(ch, buf)
! This routine uses the stored pointer-to-item-database-item-structure of
! the most recently retrieved item from this channel to retrieve 
! the default output format specification string associated with the
! current item.
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'wind_os_def.for'
	include		'item_def.for'
	integer*4	ch				! TM channel number
	character*(*)	buf
	integer*4	ok
	integer*4	w_get_item_info			! a function
	external	w_get_item_info !$pragma C( w_get_item_info )
	integer*4	atom
	integer*4	ios
	record /c_string_64/ x
	integer*4	ret_size
	integer*4	i

	w_item_format = 0
	buf = ' '
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10
	atom = p_format
	x.c = ' '

	i = min(len(buf),64)
	ok = w_get_item_info( %val(exi(ch).psni), atom, x.b, i, ret_size)
	if (ok .ne. 1) goto 20

	buf = x.c
	do i=1,len(buf)
	   if (buf(i:i) .lt. ' ') buf(i:i) = ' '
	end do
	w_item_format = 1

	return
  1	format(1x,'W_ITEM_FORMAT: ', a, i)
 10	continue
	w_item_format = w_bad_channel
	write(6,1,iostat=ios) 'invalid channel=', ch
	return
 20	w_item_format = ok
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_get_item_info
	1				(ch, name, buf, size, ret_size)
! This routine uses the stored pointer-to-item-database-item-structure of
! the most recently retrieved item from this channel to retrieve 
! ancilliary item information.  Possible values for argument "name" closely
! follow the definitions described in item_def.for and in the compound IF 
! statement below.
	implicit	none
	include		'c_string_def.for'
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_extra_info_def.for'
	include		'wind_return_code_def.for'
	include		'wind_os_def.for'
	include		'item_def.for'
	integer*4	ch				! TM channel number
	character*(*)	name
	integer*4	buf(*)
	integer*4	size
	integer*4	ret_size
	integer*4	ok
	integer*4	w_get_item_info			! a function
	external	w_get_item_info !$pragma C( w_get_item_info )
	integer*4	atom
	character*32	key
	integer*4	k2len				! a function
	integer*4	k
	integer*4	ios

	wind_tm_get_item_info = 0

	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10

	key = name
	call to_upper(key,0,0)
	k = k2len(key)

	if (key(1:k) .eq. 'EXTRACT') then
	   atom = p_extract
	else if (key(1:k) .eq. 'XLATE_PRESENT') then
	   atom = p_xlate_present
	else if (key(1:k) .eq. 'VALIDATION') then
	   atom = p_validation
	else if (key(1:k) .eq. 'DESCRIPTION') then
	   atom = p_description
	else if (key(1:k) .eq. 'FIXED_VALUE') then
	   atom = p_fixed_value
	else if (key(1:k) .eq. 'COMPOSITE') then
	   atom = p_composite
	else if (key(1:k) .eq. 'FILE_NAME') then
	   atom = p_file_name
	else if (key(1:k) .eq. 'FUNCTION_NUMBER') then
	   atom = p_function_number
	else if (key(1:k) .eq. 'COUNT_ITEM') then
	   atom = p_count_item
	else if (key(1:k) .eq. 'TEXT_ITEM') then
	   atom = p_text_item
	else if (key(1:k) .eq. 'FLAGS') then
	   atom = p_attribute_flags
	else if (key(1:k) .eq. 'LOOKUP_TABLE') then
	   atom = p_lookup_table
	else if (key(1:k) .eq. 'PROCEDURE') then
	   atom = p_procedure_number
	else if (key(1:k) .eq. 'CDF_ITEM') then
	   atom = p_cdf_item
	else
	   goto 30
	end if

	ok = w_get_item_info( %val(exi(ch).psni), atom, buf, size, ret_size)
	if (ok .ne. 1) goto 20

	wind_tm_get_item_info = 1

	return
  1	format(1x,'WIND_TM_GET_ITEM_INFO: ', a, i)
  2	format(1x,'WIND_TM_GET_ITEM_INFO: ', a, a)
  3	format(1x,'WIND_TM_GET_ITEM_INFO: ', a, a, a, i4)
 10	continue
	write(6,1,iostat=ios) 'invalid channel=', ch
	return
 20	wind_tm_get_item_info = ok
	if (user(ch).suppress_messages) return
! No error message here, item attributes are dynamic, any single attribute
! may or may not exist for a given item.
!	write(6,3,iostat=ios) 'error getting info type ', key(1:k), ' ok=', ok
	return
 30	continue 
	if (user(ch).suppress_messages) return
	write(6,2,iostat=ios) 'unknown item ancilliary info keyword: ', name
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_set_item_func
	1		(ch, event, item, newfun)
! Though probably never to be used, this routine calls a C routine to
! put a (FORTRAN) function address into the item database's array of
! function addresses referenced by function= items.
! A sample call looks like:
!
!	integer*4	new_test_fun
!	external	new_test_fun
!	integer*4	wind_tm_set_item_func
!	integer*4	ok
!	integer*4	ch
!	ok = wind_tm_set_item_func(ch,'TDSF', 'JK_TEST_THING',
!	1	%val(%loc(new_test_fun)))
!
	implicit	none
	include		'c_string_def.for'
	integer*4	ch
	character*(*)	event
	character*(*)	item
	integer*4	newfun
	external	newfun
	integer*4	ok
	integer*4	wid_set_item_func_addr
	external	wid_set_item_func_addr!$pragma C(wid_set_item_func_addr)
	record /c_string_36/ ev
	record /c_string_32/ it
	integer*4	null_terminate		! a function

	wind_tm_set_item_func = 0
	ok = newfun()

	ev.c = event
	ok = null_terminate(ev.c)
	it.c = item
	ok = null_terminate(it.c)

$IF ABSOFT_FORTRAN
	ok = wid_set_item_func_addr(%val(6), %val(loc(newfun)))
$ELSE
	ok = wid_set_item_func_addr(%val(6), %val(%loc(newfun)))
$ENDIF
!	ok = wid_load_function_item(%val(ch-1),ev.b,it.b,
!	1	%val(%loc(newfun)),
!	1	aux_arg_string.b,
!	1	validation_string.b)

	! dummy statement for compiler
	if (ch .eq. -998765) ok = 8888

	wind_tm_set_item_func = ok

	return
	end
!------------------------------------------------------------------------------
! Uses the current event name to call a physical units routine.
! Note that this function is intended to be called from a C function.
!
	integer*4	function	w_physical_units_r4
	1				(ch, c_item, r4, size, ret_size)
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'c_string_def.for'
	integer*4	ch				! TM channel number
	byte		c_item(*)			! null terminated str
	real*4		r4(*)				! array of returned data
	integer*4	size				! size of array a
	integer*4	ret_size			! # i*4's written to a
	record /c_string_64/	item			! name of item
	integer*4	ok
	character*8	ev(0:15)
	logical*4	first_time /.true./
	integer*4	w_item_xlate
	logical*4	lval
	character*16	packet_type
	integer*4	i,j
	integer*4	k3len
	integer*4	k3(0:15)
	integer*4	ios
	integer*4	id
	integer*4	w_master_cylinder

	if (ch .lt. 0 .or. ch .gt. max_channels) goto 10

	i = 1
	item.c = ' '
	do while(c_item(i) .ne. 0 .and. i .le. 64)
	   item.b(i) = c_item(i)
	   i = i + 1
	end do

	if (first_time) then
	   ! get the "standard" event names stored in the GLOBAL database
	   packet_type = 'PACKET_TYPE'
	   lval = user(ch).suppress_messages
	   user(ch).suppress_messages = .true.
	   j = 0
	   do i=0,15
	      ok = w_item_xlate(ch,eei(ch).event_type, packet_type,i,ev(i))
	      if (ok .eq. 1) then
	         k3(i) = k3len(ev(i))
	         if (k3(i) .gt. 0) j = j + 1
	      end if
	   end do
	   user(ch).suppress_messages = lval
	   first_time = .false.
	   if (j .eq. 0) goto 20
	end if

	! get the current event/packet type
	id = eei(ch).packet_id
	if (k3(id) .lt. 1) goto 50

	! call the appropriate physical units routine
	ok = w_master_cylinder(ev(id)(1:k3(id)), ch, item.c, r4,
	1    size, ret_size)

	w_physical_units_r4 = ok
	if (ok .ne. 1) goto 70

	return
   1	format(1x,'W_PHYSICAL_UNITS_R4: ', a, :, a, :, i4)
  10	w_physical_units_r4 = 0
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'invalid channel number.'
	return
  20	w_physical_units_r4 = 0
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'no xlates for item ', packet_type
	return
  50	w_physical_units_r4 = 0
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'blank xlate for item ', packet_type
	return
!  60	w_physical_units_r4 = 0
!	if (user(ch).suppress_messages) return
!	write(6,1,iostat=ios) 'event '//eei(ch).event_type//
!	1 ' has no physical units routine.'
!	return
  70	w_physical_units_r4 = 0
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'error calling '//ev(id)(1:k3(id))//
	1 ' physical units routine.'
	return
	end
