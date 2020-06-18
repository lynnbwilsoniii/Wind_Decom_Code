! wind_time_lib.for - generic date/time routines supporting wind_lib

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Date-Time functions for wind_lib 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
	integer*4	function	wind_tm_scet_to_mfmf
	1				(ch,major,minor,scet)
! This routine returns the major/minor frame indexes of the TM record
! closest to the UR8 scet (space craft event time) specified by
! the calling argument scet.  The caller's ur8 is changed to the actual
! position time.
!
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! channel id
	integer*4	major		! major frame number
	integer*4	minor		! minor frame number
	real*8		scet
	character*24	c_scet		! 24 character earth receive time
	integer*4	ok		! used for checking return status
	integer*4	ios
	integer*4	wind_check_parms
	integer*4	ur8_to_c24
	integer*4	wnd_to_ur8
$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='WIND_TM_SCET_TO_MFMF')
$ELSE
	parameter	rn='WIND_TM_SCET_TO_MFMF'
$ENDIF
	integer*4	w_f_iiiclli
	integer*4	w_f_iiidli
	integer*4	cdhf_flag
	integer*4	i

	wind_tm_scet_to_mfmf = 0
	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

!xxxxxx see if cdhf must still fill the wind_record value, I don't think
! so, jk, 2-14-96
	cdhf_flag = user(ch).stream_type .and. any_cdhf_stream
	if (cdhf_flag .ne. 0) then
	   ok = w_f_iiidli( %val(user(ch).f_goto_time),
	1	ch, major, minor, scet,
	1	.false., user(ch).wind_record)
	   if (ok .ne. 1) goto 10
	else
	   ok = ur8_to_c24(scet, c_scet)
	   if (ok .ne. 1) goto 9
!	   ok = wind_get_record_by_time(ch,major,minor,c_scet,2)
	   ok = w_f_iiiclli( %val(user(ch).f_goto_time),
	1	ch, major, minor, c_scet, 
	1	.false.,
	1	.true., user(ch).wind_record)
	   if (ok .ne. 1) goto 10
	   ok = wnd_to_ur8(user(ch).wind_record.scet.b, scet)
	   if (ok .ne. 1) goto 20
	end if

	wind_tm_scet_to_mfmf = 1

	return
  1	format(1x,'WIND_TM_SCET_TO_MFMF: ', a, f16.10)
  2	format(1x,'WIND_TM_SCET_TO_MFMF: ', a, 8(z2.2))
  8	wind_tm_scet_to_mfmf = ok
	return
  9	wind_tm_scet_to_mfmf = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot convert UR8 to C24 time: ', scet
	return
 10	wind_tm_scet_to_mfmf = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get a TM record at ur8 time: ', scet
	return
 20	wind_tm_scet_to_mfmf = ok
	if (user(ch).suppress_messages) return
	write(6,2,iostat=ios) 'Cannot convert wnd to UR8 time: ',
	1	(user(ch).wind_record.scet.b(i), i=1,8)
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_mfmf_to_scet
	1				(ch,major,minor,scet)
! This routine returns the Ulysses real*8 scet (space craft event time) of the
! record specified by the major/minor frame indexes.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! channel id
	integer*4	major		! major frame number
	integer*4	minor		! minor frame number
	real*8		scet		! Ulysses real*8 format time
	integer*4	ok		! used for checking return status
	integer*4	status		! holder for eventual return status
	integer*4	ios
	integer*4	wind_get_record
	integer*4	wnd_to_ur8
	integer*4	wind_check_parms
$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='WIND_TM_MFMF_TO_SCET')
$ELSE
	parameter	rn='WIND_TM_MFMF_TO_SCET'
$ENDIF

	wind_tm_mfmf_to_scet = 0

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

	ok = wind_get_record(ch,major,minor,user(ch).wind_record)
	if (ok .eq. w_bad_rec_quality_flag) then
	   ! error already reported, let user position to a bad record
	else if (ok .ne. 1) then
	   goto 10
	end if
	status = ok

	ok = wnd_to_ur8(user(ch).wind_record.scet.b, scet)
	if (ok .ne. 1) goto 20
!	type '(a,f16.10,2x,8(1x,z2.2))', '>>>>>>> ', scet, 
!	1 (user(ch).wind_record.scet.b(i), i=1,8)

	wind_tm_mfmf_to_scet = status

	return
  1	format(1x,'WIND_TM_MFMF_TO_SCET: ', a)
  8	wind_tm_mfmf_to_scet = ok
	return
 10	wind_tm_mfmf_to_scet = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get specified record.'
	return
 20	wind_tm_mfmf_to_scet = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get time from record.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_ert_to_mfmf
	1				(ch,major,minor,ert)
! This routine returns the major/minor frame indexes of the TM record
! closest to the UR8 ert (earth receive time) specified by
! the calling argument ert.
!
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! channel id
	integer*4	major		! major frame number
	integer*4	minor		! minor frame number
	real*8		ert		! Ulysses real*8 format time
	character*24	c_ert		! 24 character earth receive time
	integer*4	ok		! used for checking return status
	integer*4	wind_check_parms
$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='WIND_TM_ERT_TO_MFMF')
$ELSE
	parameter	rn='WIND_TM_ERT_TO_MFMF'
$ENDIF
	integer*4	w_f_iiiclli
	integer*4	ios
	integer*4	ur8_to_c24

	wind_tm_ert_to_mfmf = 0
	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

	ok = ur8_to_c24(ert, c_ert)
	if (ok .ne. 1) goto 9

!	ok = wind_get_record_by_time(ch,major,minor,c_ert,1)
	ok = w_f_iiiclli( %val(user(ch).f_goto_time),
	1	ch, major, minor, c_ert, 
	1	.true.,
	1	.true., user(ch).wind_record)
	if (ok .ne. 1) goto 10

	wind_tm_ert_to_mfmf = 1

	return
  1	format(1x,'WIND_TM_ERT_TO_MFMF: ', f16.10)
  8	wind_tm_ert_to_mfmf = ok
	return
  9	wind_tm_ert_to_mfmf = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot convert UR8 to C24 time: ', ert
	return
 10	wind_tm_ert_to_mfmf = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get a TM record at time: ', ert
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_tm_mfmf_to_ert
	1				(ch,major,minor,ert)
! This routine returns the UR8 ert (earth receive time) of the
! record specified by the major/minor frame indexes.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch		! channel id
	integer*4	major		! major frame number
	integer*4	minor		! minor frame number
	real*8		ert		! Ulysses real*8 time format
	integer*4	ok		! used for checking return status
	integer*4	ios
	integer*4	wind_get_record
	integer*4	wnd_to_ur8
	integer*4	wind_check_parms
$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='WIND_TM_MFMF_TO_ERT')
$ELSE
	parameter	rn='WIND_TM_MFMF_TO_ERT'
$ENDIF

	wind_tm_mfmf_to_ert = 0

	ok = wind_check_parms(rn,ch,major,minor)
	if (ok .ne. 1) goto 8

	ok = wind_get_record(ch,major,minor,user(ch).wind_record)
	if (ok .ne. 1) goto 10

	ok = wnd_to_ur8(user(ch).wind_record.gathertime.b, ert)
	if (ok .ne. 1) goto 20

	wind_tm_mfmf_to_ert = 1

	return
  1	format(1x,'WIND_TM_MFMF_TO_ERT: ', a)
  8	wind_tm_mfmf_to_ert = ok
	return
 10	wind_tm_mfmf_to_ert = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get specified record.'
	return
 20	wind_tm_mfmf_to_ert = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get time from record.'
	call w_show_wind_rec(user(ch).wind_record)
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_sys_to_ert_time(sys,ert)
! Converts 8-byte .wnd/VMS system time into ASCII format.
! ERT is returned in a "dd-mmm-yyy hh:mm:ss.cc" format.
	implicit	none
	character*(*)	ert		! earth receive time
	integer*4	sys(2)		! system time
	integer*4	sys_asctim	! a system service
$IF ABSOFT_FORTRAN
	integer*4	min_length
	parameter	(min_length=23)	! minimum length for time string, 24=ok
$ELSE
	parameter	min_length=23	! minimum length for time string, 24=ok
$ENDIF
	logical*4	wind_suppress_internal_msgs
	include		'wind_return_code_def.for'

	wind_sys_to_ert_time = 0
	if (len(ert) .lt. min_length) goto 10

	wind_sys_to_ert_time = sys_asctim(0,ert,sys)
	if (wind_sys_to_ert_time.ne.1) goto 20

	return
  1	format(1x,'WIND_SYS_TO_ERT_TIME: ',a,z8.8,a)
  10	wind_sys_to_ert_time = w_buffer_too_small
	if (wind_suppress_internal_msgs()) return
	type 1, 'ERT string must be GE 23 chars long.'
	return
 20	wind_sys_to_ert_time = w_bad_64bit_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'Bad 8-byte time value, status:',wind_sys_to_ert_time,'.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_ert_to_sys_time(ert,sys)
! Converts ASCII earth received time to 8-byte VMS system time.
! ERT is expected in a "dd-mmm-yyyy hh:mm:ss.cc" format.
	implicit	none
	character*(*)	ert		! earth receive time
	integer*4	sys(2)		! system time
	integer*4	sys_bintim	! a system service
	logical*4	wind_suppress_internal_msgs
	include		'wind_return_code_def.for'

	call to_upper(ert, 0, 0)

	wind_ert_to_sys_time = sys_bintim(ert, sys)
	if (wind_ert_to_sys_time.ne.1) goto 30

	return
 30	wind_ert_to_sys_time = w_bad_ascii_date_time
	if (wind_suppress_internal_msgs()) return
	type *, 'WIND_ERT_TO_SYS_TIME: Invalid time: ', ert
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_sys_to_real8_time(sys,r8)
! Converts a quadword .wnd/VMS delta time to real*8.
! A D-floating value will handle a delta time over 1 year to 1/100 of a
! second.
	implicit	none
	integer*4	sys(2)			! delta time in system format
	real*8		r8			! delta time in D-floating
	real*4		r4			! delta time in F-floating
	integer*4	lib_cvtf_from_internal_time ! convert sys to floating
	integer*4	status			! to hold return status
!	include		'($libdtdef)'		! def's for date/time package
	integer*4	op ! /lib$k_delta_seconds_f/! says "return value in seconds"
	logical*4	wind_suppress_internal_msgs
	include		'wind_return_code_def.for'

	wind_sys_to_real8_time = 0

	! get the f-floating form of the delta time, good to nearest second
	status = lib_cvtf_from_internal_time(op,r4,sys)
	if (status.ne.1) goto 10

	! compute the total number of 1/100 th's of seconds
	r8 = dble(r4) * 100.0

	wind_sys_to_real8_time = 1

	return
  1	format(1x,'WIND_SYS_TO_REAL8_TIME: ',a, z8.8)
 10	wind_sys_to_real8_time = w_bad_64bit_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'Cannot convert delta time to F-floating, status:', status
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_real8_to_sys_time(r8,sys)
! Converts real*8 delta time to a quadword VMS delta time.
! A D-floating value will handle a delta time over 1 year to 1/100 of a
! second.
	implicit	none
	integer*4	sys(2)			! delta time in system format
	real*8		r8			! delta time in D-floating
	real*4		r4			! delta time in F-floating
	integer*4	lib_cvtf_to_internal_time ! convert sys to floating
	integer*4	status			! to hold return status
!	include		'($libdtdef)'		! def's for date/time package
	integer*4	op ! /lib$k_delta_seconds_f/! says "return value in seconds"
	logical*4	wind_suppress_internal_msgs
	include		'wind_return_code_def.for'

	wind_real8_to_sys_time = 0

	! get the quadword internal form of the f-floating delta time 
	r4 = r8
	status = lib_cvtf_to_internal_time(op,r4,sys)
	if (status.ne.1) goto 10

	wind_real8_to_sys_time = 1

	return
  1	format(1x,'WIND_REAL8_TO_SYS_TIME: ',a, z8.8)
 10	wind_real8_to_sys_time = w_bad_64bit_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'Cannot convert F-floating to internal, status:', status
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_ert_to_dbms_time
	1				(u_ert,dbms_date,dbms_time)
! This routine takes an ERT of the form DD-MMM-YYYY HH:MM:SS.CC and converts
! it to separate date integer and time integer.  The date and time integer
! are binary, but their ASCII representations are YYYYmmDD and HHMMSS,
! respectively.
! [Months are handled in a "portable" way.]
	implicit	none
	include		'wind_return_code_def.for'
	character*(*)	u_ert
	character*24	ert
	integer*4	dbms_date
	integer*4	dbms_time
	integer*4	a,b,c
	character*3	month
	logical*4	wind_suppress_internal_msgs

	wind_ert_to_dbms_time = 0
	ert = u_ert

	read(ert(1:2),9,err=10) c			! day
	month = ert(4:6)
	if (month.eq.'JAN') then			! month
	   b = 1
	else if (month.eq.'FEB') then
	   b = 2
	else if (month.eq.'MAR') then
	   b = 3
	else if (month.eq.'APR') then
	   b = 4
	else if (month.eq.'MAY') then
	   b = 5
	else if (month.eq.'JUN') then
	   b = 6
	else if (month.eq.'JUL') then
	   b = 7
	else if (month.eq.'AUG') then
	   b = 8
	else if (month.eq.'SEP') then
	   b = 9
	else if (month.eq.'OCT') then
	   b = 10
	else if (month.eq.'NOV') then
	   b = 11
	else if (month.eq.'DEC') then
	   b = 12
	else
	   goto 11
	end if
	read(ert(8:11),9,err=12) a			! year
	dbms_date = (a*10000) + (b*100) + c

	read(ert(13:14),9,err=13) a			! hour
	read(ert(16:17),9,err=14) b			! minute
	read(ert(19:20),9,err=15) c			! second
	dbms_time = (a*10000) + (b*100) + c

	wind_ert_to_dbms_time = 1

	return
  9	format(i)
  1	format(1x,'WIND_ERT_TO_DBMS: ', a, a)
 10	wind_ert_to_dbms_time = w_bad_ascii_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot read day from ', ert
	return
 11	wind_ert_to_dbms_time = w_bad_ascii_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot read month from ', ert
	return
 12	wind_ert_to_dbms_time = w_bad_ascii_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot read year from ', ert
	return
 13	wind_ert_to_dbms_time = w_bad_ascii_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot read hour from ', ert
	return
 14	wind_ert_to_dbms_time = w_bad_ascii_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot read minute from ', ert
	return
 15	wind_ert_to_dbms_time = w_bad_ascii_date_time
	if (wind_suppress_internal_msgs()) return
	type 1, 'cannot read second from ', ert
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_delta_time(t1,t2,r8)
! Subtracts two quadword system times and converts the result to a D-floating
! number of 1/100th's of seconds.
	implicit	none
	real*8		r8		! (t1 - t2) in 100th's of seconds
	integer*4	t1(2)		! a quadword system time
	integer*4	t2(2)		! a quadword system time
	integer*4	delta_time(2)	! a quadword system delta time
	integer*4	lib_sub_times	! rtl routine
	integer*4	status		! to hold return status values
	logical*4	make_negative	! flag saying t2 > t1
!	include		'($libdtdef)'	! def's for date/time package
!	include		'($libdef)'	! def's for general utility library
	integer*4	wind_sys_to_real8_time
	logical*4	wind_suppress_internal_msgs
	include		'wind_return_code_def.for'

	wind_delta_time = 0
	make_negative = 0

	! subtract the two times
	status = lib_sub_times(t1,t2,delta_time)
	if (status .eq. lib_negtime) then
	   status = lib_sub_times(t2,t1,delta_time)
	   if (.not. status) goto 10
	   make_negative = 1
	else if (status.ne.1) then
	   goto 20
	end if

	! convert to double precision
	status = wind_sys_to_real8_time(delta_time, r8)
	if (status.ne.1) goto 30

	if (make_negative) r8 = - r8
	wind_delta_time = 1

	return
  1	format(1x,'WIND_DELTA_TIME: ', a, z8.8)
 10	wind_delta_time = w_bad_time_difference
	if (wind_suppress_internal_msgs()) return
	type 1, 'Cannot subtract times in either order, status:', status
	return
 20	wind_delta_time = w_bad_time_difference
	if (wind_suppress_internal_msgs()) return
	type 1, 'Cannot subtract times, status:', status
	return
 30	wind_delta_time = status
	if (wind_suppress_internal_msgs()) return
	type 1, 'Cannot convert to real*8.'
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	wind_subtract_sys_time(t1,t2,t3)
! Subtracts two quadword system times and returns a quadword system time
	implicit	none
	integer*4	t1(2)		! a quadword system time
	integer*4	t2(2)		! a quadword system time
	integer*4	t3(2)		! a quadword system time
	integer*4	lib_sub_times	! rtl routine
	integer*4	ok		! to hold return status values
!	include		'($libdtdef)'	! def's for date/time package
!	include		'($libdef)'	! def's for general utility library
	logical*4	wind_suppress_internal_msgs
	include		'wind_return_code_def.for'

	wind_subtract_sys_time = 0

	! subtract the two times
	ok = lib_sub_times(t1,t2,t3)
	if (ok .eq. lib_negtime) then
	   ok = lib_sub_times(t2,t1,t3)
	   if (ok .ne. 1) goto 10
	   goto 30
	else if (ok .ne. 1) then
	   goto 20
	end if

	wind_subtract_sys_time = 1

	return
  1	format(1x,'WIND_SUBTRACT_SYS_TIME: ', a, z8.8)
 10	wind_subtract_sys_time = w_bad_time_difference
	if (wind_suppress_internal_msgs()) return
	type 1, 'Cannot subtract times in either order, ok:', ok
	return
 20	wind_subtract_sys_time = w_bad_time_difference
	if (wind_suppress_internal_msgs()) return
	type 1, 'Cannot subtract times, ok:', ok
	return
 30	wind_subtract_sys_time = w_bad_time_difference
	if (wind_suppress_internal_msgs()) return
	type 1, 'Negative time difference, ok:', ok
	return
	end
!------------------------------------------------------------------------------
	integer*4	function	w_sub_from_dbms_time
	1		(ch,rd,rd1000,wt,wt1000,mrate,diff)
! Subtracts a time interval from a wind dbms format time.
	implicit	none
	include		'wind_tm_user_def.for'
	include		'wind_tm_event_def.for'
	include		'wind_return_code_def.for'
	integer*4	ch				! TM channel number
	integer*4	rd(2)		! dbms format scet, the known time
	integer*4	rd1000		! milliseconds portion of rd
	integer*4	wt(2)		! dbms format scet, to calculate
	integer*4	wt1000		! milliseconds portion of wt
	integer*4	mrate		! number of milliseconds between mf's
	integer*4	diff		! number of mf's between times rd and wt
	integer*4	mdiff

$IF ABSOFT_FORTRAN
	integer*4	msec_in_minute
	integer*4	msec_in_hour
	integer*4	msec_in_day
	parameter	(msec_in_minute=60*1000)
	parameter	(msec_in_hour=msec_in_minute*60)
	parameter	(msec_in_day=msec_in_hour*24)
$ELSE
	parameter	msec_in_minute=60*1000
	parameter	msec_in_hour=msec_in_minute*60
	parameter	msec_in_day=msec_in_hour*24
$ENDIF
	integer*4	iday_of_year
	logical*4	julian_to_mmdd
	character*8	cdate
	integer*4	doy
	integer*4	year,month,day
	integer*4	ios
	character*6	ctime
	integer*4	hour,minute,second
	integer*4	mdays,mhours,mminutes,mseconds
	integer*4	mlittle_rd
	integer*4	totalmillisec
	logical*4	lok

	! should work for both negative and positive diff's
	w_sub_from_dbms_time = 0

!	type *, 'XREF time:', rd(1), rd(2), rd1000, mrate, diff

	write(cdate,'(i8)',iostat=ios,err=30) rd(1)
	read(cdate(1:4),'(i4)',err=40,iostat=ios) year
	read(cdate(5:6),'(i2)',err=40,iostat=ios) month
	read(cdate(7:8),'(i2)',err=40,iostat=ios) day
	doy = iday_of_year(year,month,day)

	mdiff = diff * mrate
	mdays = mdiff / msec_in_day
	if (msec_in_day .ne. 0) then
	   doy = doy + mdays
	   mdiff = mdiff - (mdays * msec_in_day)
	   call adjust_doy_and_year(doy,year)
	end if

	write(ctime,'(i6)',iostat=ios,err=10) rd(2)
	read(ctime(1:2),'(i4)',err=20,iostat=ios) hour
	read(ctime(3:4),'(i2)',err=20,iostat=ios) minute
	read(ctime(5:6),'(i2)',err=20,iostat=ios) second

	! calculate the total number of elapsed milliseconds in the
	! fractional day at the reference position rd
	mlittle_rd = (hour*msec_in_hour) + 
	1             (minute*msec_in_minute) +
	1             (second*1000) +
	1             rd1000
	! apply the day's fractional difference in milliseconds to the
	! reference position
	totalmillisec = mlittle_rd + mdiff

	if (totalmillisec .gt. msec_in_day) then
	   totalmillisec = totalmillisec - msec_in_day
	   doy = doy + 1
	   call adjust_doy_and_year(doy,year)
	else if (totalmillisec .lt. 0) then
	   totalmillisec = msec_in_day + totalmillisec
	   doy = doy - 1
	   call adjust_doy_and_year(doy,year)
	end if

	mhours = totalmillisec / msec_in_hour
	if (mhours .ne. 0)
	1  totalmillisec = totalmillisec - (mhours * msec_in_hour)

	mminutes = totalmillisec / msec_in_minute
	if (mminutes .ne. 0)
	1  totalmillisec = totalmillisec - (mminutes * msec_in_minute)

	mseconds = totalmillisec / 1000
	if (mseconds .ne. 0)
	1   totalmillisec = totalmillisec - (mseconds * 1000)

	wt1000 = totalmillisec

	lok = julian_to_mmdd(doy,year,month,day)

	write(cdate,'(i4,i2.2,i2.2)',iostat=ios,err=80) year, month, day
        read(cdate,'(i8)',iostat=ios,err=82) wt(1)

	write(ctime,'(i2.2,i2.2,i2.2)',iostat=ios,err=84)
	1   mhours,mminutes,mseconds
	read(ctime,'(i6)',iostat=ios,err=86) wt(2)

	w_sub_from_dbms_time = 1
	return
 1	format(1x,'W_SUB_FROM_DBMS_TIME: ', a, :, i3)
 2	format(1x,t1,a,i)
 10	w_sub_from_dbms_time = 0
	if (user(ch).suppress_messages) return
	type 1, 'error writing reference time [hhmmss], ios=', ios
	type 2, 'the time value is ', rd(2)
	return
 20	w_sub_from_dbms_time = 0
	if (user(ch).suppress_messages) return
	type 1, 'error reading reference time [hhmmss], ios=', ios
	type 2, 'the time value is ', rd(2)
	return
 30	w_sub_from_dbms_time = 0
	if (user(ch).suppress_messages) return
	type 1, 'error writing reference date [yyyymmdd], ios=', ios
	type 2, 'the date value is ', rd(1)
	return
 40	w_sub_from_dbms_time = 0
	if (user(ch).suppress_messages) return
	type 1, 'error  reading reference date [yyyymmdd], ios=', ios
	type 2, 'the date value is ', rd(1)
	return
 80	w_sub_from_dbms_time = 0
	if (user(ch).suppress_messages) return
	type 1, 'error writing target date [yyyymmdd], ios=', ios
	type 2, 'the date value is ', wt(1)
	return
 82	w_sub_from_dbms_time = 0
	if (user(ch).suppress_messages) return
	type 1, 'error reading target date [yyyymmdd], ios=', ios
	type 2, 'the date value is ', wt(1)
	return
 84	w_sub_from_dbms_time = 0
	if (user(ch).suppress_messages) return
	type 1, 'error writing target time [hhmmss], ios=', ios
	type 2, 'the time value is ', wt(2)
	return
 86	w_sub_from_dbms_time = 0
	if (user(ch).suppress_messages) return
	type 1, 'error reading target time [hhmmss], ios=', ios
	type 2, 'the time value is ', wt(2)
	return
	end
!------------------------------------------------------------------------------
! Delete this?
	integer*4	function	wind_tm_date(channel,date_string)
! Returns via argument the date stamp of the current record in DD-MMM-YY format.
! This is the same format returned by the FORTRAN subroutine DATE.
! The SCET time stamp is used.
	implicit	none
	integer*4	channel
	character	date_string*(*)
	character	timbuf*24
	integer*4	sys_asctim
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ios

	wind_tm_date = 0
	date_string = ' '
	if (channel .lt. 1 .or. channel .gt. max_channels) goto 10

$IF ABSOFT_FORTRAN
	wind_tm_date = sys_asctim(0,timbuf,user(channel).wind_record.scet)
$ELSE
	wind_tm_date = sys_asctim(0,timbuf,user(channel).wind_record.scet,)
$ENDIF
	if (wind_tm_date.ne.1) goto 20

	date_string = timbuf(1:7)//timbuf(10:11)

	return
  1	format(1x,'WIND_TM_DATE: ', a)
 10	wind_tm_date = w_bad_channel
	if (user(channel).suppress_messages) return
	write(6,1,iostat=ios) 'invalid channel number.'
	return
 20	wind_tm_date = w_bad_date_time_in_frame
	if (user(channel).suppress_messages) return
	write(6,1,iostat=ios) 'invalid system-format date.'
	return
	end
!------------------------------------------------------------------------------
! Delete this?
	integer*4	function	wind_tm_time(channel,time_string)
! Returns via argument the time stamp of the current record in HH:MM:SS format.
! This is the same format returned by the FORTRAN subroutine TIME.
! The SCET time stamp is used.
	implicit	none
	integer*4	channel
	character	time_string*(*)
	character	timbuf*24
	integer*4	sys_asctim
	include		'wind_tm_user_def.for'
	include		'wind_return_code_def.for'
	integer*4	ios

	wind_tm_time = 0
	time_string = ' '
	if (channel .lt. 1 .or. channel .gt. max_channels) goto 10

$IF ABSOFT_FORTRAN
	wind_tm_time = sys_asctim(0,timbuf,user(channel).wind_record.scet)
$ELSE
	wind_tm_time = sys_asctim(0,timbuf,user(channel).wind_record.scet,)
$ENDIF
	if (wind_tm_time.ne.1) goto 10

	time_string = timbuf(13:20)

	return
 1	format(1x,'WIND_TM_TIME: ', a)
 10	wind_tm_time = w_bad_channel
	if (user(channel).suppress_messages) return
	write(6,1,iostat=ios) 'invalid channel number.'
	return
! 20	wind_tm_time = w_bad_date_time_in_frame
!	if (user(channel).suppress_messages) return
!	write(6,1,iostat=ios) 'cannot convert system time to ASCII.'
!	return
	end
!------------------------------------------------------------------------------
	integer*4	function	old_channel_position(ch, t)
! Positions stream to time t for non-zero t.  For t==zero the beginning of
! file position is used.  On return, time t is written with the current
! position in UR8 format.
	implicit	none
	include		'wind_tm_user_def.for'
	integer*4	ch
	real*8		t
	integer*4	mjr
	integer*4	mnr
	integer*4	ios
	integer*4	ok
	integer*4	wind_tm_mfmf_to_scet
	integer*4	wind_tm_scet_to_mfmf
	integer*4	wnd_to_ur8
	integer*4	i

	old_channel_position = 0
	if (ch .lt. 1 .or. ch .gt. max_channels) goto 10

	if (t .eq. 0.0) then
	   ! goto earliest position in file
	   mjr = user(ch).first_major
	   mnr = user(ch).first_minor.i4val
	   ok = wind_tm_mfmf_to_scet(ch,mjr,mnr,t)
	   if (ok .ne. 1) goto 20
	else
	   ok = wind_tm_scet_to_mfmf(ch,mjr,mnr,t)
	   if (ok .ne. 1) goto 30
	   ok = wnd_to_ur8(user(ch).wind_record.scet.b, t)
	   if (ok .ne. 1) goto 40
	end if

	old_channel_position = 1
	return
  1	format(1x,'OLD_CHANNEL_POSITION: ', a, :, f16.10)
  2	format(1x,'OLD_CHANNEL_POSITION: ', a, 8(z2.2))
 10	write(6,1,iostat=ios) 'invalid channel number.'
	return
 20	old_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get record at beginning of file.'
	return
 30	old_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,1,iostat=ios) 'Cannot get record at UR8 time: ', t
	return
 40	old_channel_position = ok
	if (user(ch).suppress_messages) return
	write(6,2,iostat=ios) 'Cannot convert wnd to UR8 time: ',
	1	(user(ch).wind_record.scet.b(i), i=1,8)
	return
	end
