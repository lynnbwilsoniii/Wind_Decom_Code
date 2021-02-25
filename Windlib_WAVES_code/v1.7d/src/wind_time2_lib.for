! wind_time2_lib.for - generic (wind independant) date/time routines
!
! This module is linked with megaint2.obj and wind_string_lib to form
! a shareable image.
!
! Time related acronyms:
!	UR8	- Ulysses Real*8 time format, #days since 1982
!	C24	- ASCII character date/time string: dd-mmm-yyyy hh:mm:ss.ccc
!	STRING	- "DD-MMM-YYYY, HH:MM:SS:NNN" format, 25 characters
!       CMP     - comparison, "YYYY-MM-DD HH:MM:SS.NN" format, 22 characters
!	DBMS	- Wind/Waves TM Item Database time format, 2 [or 3] integers
!		  coded as YYYYMMDD and HHMMSS [and MSEC].
!	ATC	- ATC time format used in CDHF files, 3 integers
!		  coded as YYYY, JULIAN_DAY, and MSEC_OF_DAY.
!	PB5	- PB5 time format used in CDHF files, 2 integers subdivided
!		  into bit fields.
!	WND	- 64 bit integer representing the number of 100-nano second
!		  increments since 17-NOV-1858 00:00:00.00 (Smithsonian epoch)
!	UI8	- Unsigned 8 byte array, same as WND time
!	YDOY	- Year, Day-Of-Year, and millisecond of day: same as ATC
!	YMD	- Year,Month,Day,hour,minute,second,millisecond format
!	EPOCH	- A real*8 format used by CDF

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_from_ydoy(ur8, year, doy, msec)
	implicit	none
	real*8		ur8
	integer*4	year
	integer*4	doy
	integer*4	msec
	integer*4	atc_to_ur8

	w_ur8_from_ydoy = atc_to_ur8(year, doy, msec, ur8)

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_from_ymd
	1		(ur8, year, month, day, hour, minute, second, msec)
	implicit	none
	real*8		ur8
	integer*4	year, month, day
	integer*4	hour, minute, second, msec
	integer*4	dbms_to_ur8
	integer*4	i,j

	i = (year*10000) + (month*100) + day		! YYYYMMDD
	j = (hour*10000) + (minute*100) + second	! HHMMSS

	w_ur8_from_ymd = dbms_to_ur8(i,j, msec, ur8)

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_to_string(ur8, str)
	implicit	none
	real*8		ur8
	character*(*)	str
	character*24	c24
	integer*4	ur8_to_c24
	integer*4	ok
	integer*4	w_ur8_to_string_fr		! an entry point
	character*36	fr_mon /'JANFEVMARAVRMAIJUNJULAOUSEPOCTNOVDEC'/
	integer*4	number_of_month
	logical*4	in_french
	integer*4	i,j

	in_french = .false.
	goto 1000

	!----------------------------------------------------------------------
	entry	w_ur8_to_string_fr(ur8, str)
	in_french = .true.
	goto 1000

 1000	continue
	w_ur8_to_string = 0

	ok = ur8_to_c24(ur8, c24)
	if (ok .ne. 1) return

	if (in_french) then
	   ok = number_of_month(i, c24(4:6))
	   if (ok .ne. 1) then
	      type *, 'What?, # of month cannot be invalid!'
	   else
	      j = (i*3) - 2
	      c24(4:6) = fr_mon(j:j+2)
	   end if
	end if

	w_ur8_to_string = ok
	str = c24(1:11)//', '//c24(13:24)

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_to_ydoy(ur8, year, doy, msec)
	implicit	none
	real*8		ur8
	integer*4	year
	integer*4	doy
	integer*4	msec
	integer*4	ur8_to_atc

	w_ur8_to_ydoy = ur8_to_atc(ur8, year, doy, msec)

	return
	end


!------------------------------------------------------------------------------
	integer*4	function	w_ur8_to_ymd
	1		(ur8, year, month, day, hour, minute, second, msec)
	implicit	none
	real*8		ur8
	integer*4	year, month, day
	integer*4	hour, minute, second, msec
	integer*4	ur8_to_dbms
	integer*4	i,j
	integer*4	x,y
	integer*4	ok

	ok = ur8_to_dbms(ur8, i, j, msec)
	w_ur8_to_ymd = ok
	if (ok .ne. 1) return

	year = i / 10000
	x = year * 10000
	month = (i - x) / 100
	y = month * 100
	day = i - x - y

	hour = j / 10000
	x = hour * 10000
	minute = (j - x) / 100
	y = minute * 100
	second = j - x - y

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_to_ymd_i(ur8, dt, tm)
	implicit	none
	real*8		ur8
	integer*4	dt		! YYYYMMDD
	integer*4	tm		! HHMMSS
	integer*4	msec
	integer*4	ur8_to_dbms

	w_ur8_to_ymd_i = ur8_to_dbms(ur8, dt, tm, msec)

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_from_ymd_i(ur8, dt, tm)
	implicit	none
	real*8		ur8
	integer*4	dbms_to_ur8
	integer*4	dt		! YYYYMMDD
	integer*4	tm		! HHMMSS
	integer*4	msec

	msec = 0
	w_ur8_from_ymd_i = dbms_to_ur8(dt, tm, msec, ur8)

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	accumulate_days(y1,ny,buf)
! Places into array buf the number of days accumulated such that for function
! FY(j) returning the number of days in year j, the first year y1, and
! ny the number of years to work over the elements of buf will be
!
!	buf(1) = FY(y1)
!	buf(2) = FY(y1+1) + buf(1)
!	...
!	buf(i) = FY(y1+i-1) + buf(i-1)
!	...
!	buf(y1+ny-1) = FY(y1+ny-1) + buf(y1+ny-2)
!
	implicit	none
	integer*4	y1		! initial year
	integer*4	ny		! number of years to accumulate days
	integer*4	buf(*)
	integer*4	i,j,k,n
	logical*4	is_a_leap_year	! returns 1 for leap year, 0 otherwise
	integer*4	days(0:1) /365,366/

	accumulate_days = 1
	k = y1
	n = 0
	if (is_a_leap_year(k)) n = 1
	buf(1) = days(n)
	do i=2,ny
	   k = k + 1
	   n = 0
	   if (is_a_leap_year(k)) n = 1
	   j = days(n)
	   buf(i) = buf(i-1) + j
	end do
	return
	end

$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE

	options/extend_source
$ENDIF
!------------------------------------------------------------------------------
	integer*4	function	r8_fractional_day_routines()
! Routines for converting various formats to and from a real*8 fractional day
! format.  This fractional day format is currently used in Ulysses data
! processing and analysis.  This format is the serial count of the number
! of days since Jan 1, 1982 with the fractional portion of the day calcualted
! to the nearest millisecond.  So, ur8 time 1.50000 is noon on Jan 2, 1982.
	implicit	none
	real*8		r8arg		! callers real*8 value
	integer*4	dbms1, dbms2, dbmsmillisec
	integer*4	dbms_to_ur8	! an entry point
	integer*4	ur8_to_dbms	! an entry point
	integer*4	year,month,day
	integer*4	doy
	integer*4	hour,minute,second
	integer*4	i,j,k,n
	real*8		a,b
	integer*4	iday_of_year
	logical*4	julian_to_mmdd
	logical*4	lok
	integer*4	cdy(0:3) /0, 365, 730, 1096/ ! cumulative days/year
	integer*4	days_in_4_years
	parameter	(days_in_4_years=1461)

	real*8		minutes_in_day
	real*8		seconds_in_day
	real*8		millisecs_in_day
	parameter	(minutes_in_day=24.0*60.0)
	parameter	(seconds_in_day=minutes_in_day*60.0)
	parameter	(millisecs_in_day=seconds_in_day*1000.0)

	integer*4	i4_n_msec_in_day
	parameter	(i4_n_msec_in_day=24*60*60*1000)

	!----------------------------------------------------------------------
	entry		dbms_to_ur8(dbms1,dbms2,dbmsmillisec,r8arg)
	dbms_to_ur8 = 0
	r8arg = 0.0
	if (dbms1 .eq. 0) then
	   ! silently coerce time to beginning of ur8 epoch
	   year = 1982
	   doy = 1
	else
	   ! assume a valid dbms time
           year = dbms1 / 10000
	   i = year * 10000
	   month = (dbms1 - i) / 100
	   j = month * 100
	   day = dbms1 - i - j
	   doy = iday_of_year(year,month,day)
	end if

	i = year - 1982
	j = (i/4) * days_in_4_years
	k = mod(i,4)
	n = j + cdy(k) + doy
	
	r8arg = dfloat(n-1)

	hour = dbms2 / 10000
	i = hour * 10000
	minute = (dbms2 - i) / 100
	j = minute * 100
	second = dbms2 - i - j

	i = hour * 3600 * 1000
	j = minute * 60 * 1000
	k = second * 1000
	i = i + j + k + dbmsmillisec
	r8arg = r8arg + dfloat(i)/dfloat(i4_n_msec_in_day)

	dbms_to_ur8 = 1
	return

	!----------------------------------------------------------------------
	entry		ur8_to_dbms(r8arg,dbms1,dbms2,dbmsmillisec)
	ur8_to_dbms = 0
	dbms1 = 0
	dbms2 = 0
	dbmsmillisec = 0

	n = r8arg
	n = n + 1
	j = n/days_in_4_years
	j = j * 4
	year = 1982 + j
	i = mod(n,days_in_4_years)
	! '82=1,365 '83=366,730 '84L=731,1096 '85=1097,1461
	if (i .ge. 1097) then
	   year = year + 3
	   doy  = i - 1096
	else if (i .ge. 731) then
	   year = year + 2
	   doy  = i - 730
	else if (i .ge. 366) then
	   year = year + 1
	   doy  = i - 365
	else if (i .gt. 0) then
	   doy  = i
	else
	   year = year - 1
	   doy = 365
	end if
	lok = julian_to_mmdd(doy, year, month, day)
	if (.not. lok) then
	   type *, 'UR8_TO_DBMS: (err) doy,year,i,n=', doy, year, i, n
	   return
	end if

	! hhmmss
	b = r8arg - aint(r8arg)
	b = b * millisecs_in_day   ! dfloat(i4_n_msec_in_day)
	a = dnint(b) ! anint(b)	! using "i=nint(b)" causes an ld.so problem
	i = a
	if (i .ge. i4_n_msec_in_day) i = i4_n_msec_in_day - 1
	hour = i / (3600 * 1000)
	i = i - (hour * 3600 * 1000)
	minute = i / (60 * 1000)
	i = i - (minute * 60 * 1000)
	second = i / 1000
	i = i - (second * 1000)
	dbmsmillisec = i

	dbms1 = (year * 10000) + (month*100) + day
	dbms2 = hour * 10000
	dbms2 = dbms2 + (minute * 100)
	dbms2 = dbms2 + second

	ur8_to_dbms = 1
	return
	end

$IF ABSOFT_FORTRAN
!	options/extend_source
$ELSE

	options/extend_source
$ENDIF
!------------------------------------------------------------------------------
	integer*4	function	iday_of_year(year,month,day)
! calculates Julian day of year -- minimal error checking of arguments
	implicit	none
	logical*4	julian_to_mmdd	! an entry point
	integer*4	year,month,day
	integer*4	julian
$IF ABSOFT_FORTRAN
	integer*4	a,b,c,d,e,f,g,h,i,j,k,l
	parameter	(a=31)		! January
	parameter	(b=28+a)	! February
	parameter	(c=31+b)	! March  
	parameter	(d=30+c)	! April   
	parameter	(e=31+d)	! May    
	parameter	(f=30+e)	! June   
	parameter	(g=31+f)	! July   
	parameter	(h=31+g)	! August 
	parameter	(i=30+h)	! September
	parameter	(j=31+i)	! October
	parameter	(k=30+j)	! November
	parameter	(l=31+k)	! December
$ELSE
	parameter	a=31		! January
	parameter	b=28+a		! February
	parameter	c=31+b		! March  
	parameter	d=30+c		! April   
	parameter	e=31+d		! May    
	parameter	f=30+e		! June   
	parameter	g=31+f		! July   
	parameter	h=31+g		! August 
	parameter	i=30+h		! September
	parameter	j=31+i		! October
	parameter	k=30+j		! November
	parameter	l=31+k		! December
$ENDIF
	integer*4	m(0:12) / 0,a,b,c,d,e,f,g,h,i,j,k,l /
	logical*4	is_a_leap_year	! returns 1 for leap year, 0 otherwise
	logical*4	got_a_leap_year
	integer*4	ndays_in_month	! an entry point
	integer*4	x,y,z

	iday_of_year = 0
	if (day .lt. 1 .or. day .gt. 31) return
	if (month .lt. 1 .or. month .gt. 12) return

	x = m(month-1) + day
	if (is_a_leap_year(year) .and. (month .gt. 2)) x = x + 1
	iday_of_year = x

	return

	!---------------------------------------------------------------------
	entry	ndays_in_month(year,month)
! Returns the integer number of days in a month, accounting for leap years.
	ndays_in_month = 0
	if (month .lt. 1 .or. month .gt. 12) return
	ndays_in_month = m(month) - m(month-1)
	if (is_a_leap_year(year) .and. month.eq.2)
	1 ndays_in_month = ndays_in_month + 1
	return

	!---------------------------------------------------------------------
	entry	julian_to_mmdd(julian, year, month, day)
! Returns the month and day corresponding to the Julian date passed.
! Arguments julian and year are read only, month and day are written.

	julian_to_mmdd = .false.
	month = 0
	day   = 0
	x     = julian
	if (julian .lt. 1 .or. julian .gt. 366) return
	got_a_leap_year = is_a_leap_year(year)
	if ((.not. got_a_leap_year) .and. (julian .gt. 365)) return
	if (got_a_leap_year .and. (julian .gt. b)) x = x - 1

	y = 0
	do while(month .eq. 0 .and. y .lt. 12)
	   z = y + 1
	   if (x .gt. m(y) .and. x .le. m(z)) month = z
	   y = y + 1
	end do

	day = x - m(month-1)
	if ((got_a_leap_year) .and. (julian .gt. (b+1))) then
	   if (julian .gt. (m(month)+1)) then
	      day = 1
	      month = month + 1
	   end if
	else if (got_a_leap_year .and. julian .eq. 60) then
	   day = 29
	end if

	julian_to_mmdd = .true.
	return
	end

!------------------------------------------------------------------------------
	logical*4	function	is_a_leap_year(year)
! Returns .false. if year is not a leap year or .true. if year is a leap year.
	implicit	none
	integer*4	year
	is_a_leap_year = .false.
	if (mod(year,4) .ne. 0) return		! year not divisible by 4
	if (mod(year,100) .eq. 0 .and.
	1   mod(year,400) .ne. 0) return	! century year not div by 400
	is_a_leap_year = .true.
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	adjust_doy_and_year(doy,year)
! Successively increments year and subtracts days (365 or 366 if leap) while
! doy argument is greater than the number of days in year year.  Used for
! adjusting date/time calculations.
	implicit	none
	integer*4	doy
	integer*4	year
	logical*4	leap
	logical*4	is_a_leap_year
	logical*4	done

	done = .false.

	leap = is_a_leap_year(year)
	do while(.not. done)
	   if (leap .and. (doy .gt. 366)) then
	      year = year + 1
	      doy = doy - 366
	   else if (doy .gt. 365) then
	      year = year + 1
	      doy = doy - 365
	   else if (doy .lt. 1) then
	      year = year - 1
	      leap = is_a_leap_year(year)
	      if (leap) then
	         doy = 366 + doy
	      else
	         doy = 365 + doy
	      end if
	   end if
	   leap = is_a_leap_year(year)
	   if (doy .gt. 0) then
	      if (leap) then
	         if (doy .le. 366) done = .true.
	      else
	         if (doy .le. 365) done = .true.
	      end if
	   end if
	end do

	adjust_doy_and_year = 1
	return
	end

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
	integer*4	function	lib_cvtf_from_internal_time(op,r4,sys)
! simulates the vms routine, returns fractional seconds via r4 argument
	implicit	none
	include		'wind_os_def.for'
	include		'ui8_def.for'
	include		'vms_time_constants_def.for'
	integer*4	op		! lib$k_delta_seconds_f, not used
	real*4		r4
	record /ui8/	sys		! a vms 64-bit delta time
	record /ui8/	x,r
	integer*4	i,k
	integer*4	vxtosuni4
	external	ui8_divide	!$pragma C (ui8_divide)

	if (op .eq. 0) i = 0 ! dummy assignment

	! get the number of 1/100 seconds in argument
	call ui8_divide(sys,ui8hsec,x,r, k)
	if (sunos .or .macos_ppc) then
	   i = vxtosuni4(x.b(1))
	else
	   i = x.i
	end if
	r4 = i
	r4 = r4 / 100.0

	lib_cvtf_from_internal_time = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	lib_cvtf_to_internal_time(op,r4,sys)
! simulates the vms routine
	implicit	none
	include		'wind_os_def.for'
	include		'ui8_def.for'
	include		'vms_time_constants_def.for'
	integer*4	op		! lib$k_delta_seconds_f, not used
	real*4		r4
	record /ui8/	sys		! a vms 64-bit delta time
	record /ui8/	x
	real*4		val
	integer*4	i,k
	byte		b(4)
	equivalence	(i,b)
	integer*4	suntovxi4
	external	ui8_multiply	!$pragma C (ui8_multiply)

	lib_cvtf_to_internal_time = 1

	if (op .eq. 0) i = 0 ! dummy assignment

	val = r4 * 100.0
	i = val
	if (sunos .or . macos_ppc) then
	   x.i = suntovxi4(b)
	else
	   x.i = i
	end if

	call ui8_multiply(ui8hsec,x,sys,k)

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	name_of_month(n_month, name)
! Returns the character name of month for given n_month: {1..12}
	implicit	none
	integer*4	n_month
	character*(*)	name
	character*(*)	ch_month
	character*(*)	mm
	character*36	mon /'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'/
	character*36	cim /' 01 02 03 04 05 06 07 08 09 10 11 12'/
	integer*4	number_of_month		! an entry point
	integer*4	ch_number_of_month	! an entry point
	integer*4	ch_mm_to_ch_mmm		! an entry point
	integer*4	i,j
	character*3	c, c3

	if (n_month .ge. 1 .or. n_month .le. 12) then
	   j = (n_month *3)
	   i = j - 2
	   name = mon(i:j)
	   name_of_month = 1
	else
	   name = '???'
	   name_of_month = 0
	end if

	return

	!----------------------------------------------------------------------
	entry	ch_mm_to_ch_mmm(mm, name)
! Returns the character 3-letter month for a given 2-digit-character month
	ch_mm_to_ch_mmm = 0
	name = ' '
	c3 = ' '//mm
	if (c3(2:2) .eq. ' ') c3(2:2) = '0'
	i = index(cim, c3)
	if (i .le. 0) return
	name = mon(i:i+2)
	ch_mm_to_ch_mmm = 1
	return	

	!----------------------------------------------------------------------
	entry	number_of_month(n_month, name)
! Returns the integer number of month for a given 3-character 
! uppercase month appreviation
	n_month = 0
	number_of_month = 0
	c = name
	i = index(mon,c)
	if (i .eq. 0) return
	n_month = (i/3) + 1
	number_of_month = 1
	return

	!----------------------------------------------------------------------
	entry	ch_number_of_month(ch_month, name)
! Returns the integer number as an i2.2 character of month for a 
! given 3-character uppercase month appreviation
	ch_month = 'XX'
	ch_number_of_month = 0
	c = name
	i = index(mon,c)
	if (i .eq. 0) return
	ch_month = cim(i+1:i+2)
	ch_number_of_month = 1
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	sys_asctim(dummy,buf,tim)
! buf is returned as a date-time string in "dd-mmm-yyyy hh:mm:ss.cc" format.
! VMS 64-bit time is the number of 100-nanosecond increments elapsed since
! the Smithsonian astronomical calendar base epoch at 00:00 o'clock 17-NOV-1858.
! Simulates the VMS routine.
	implicit	none
	include		'vms_time_constants_def.for'
	integer*4	dummy		! not used
	character*(*)	buf
	byte		tim(8)		! 64-bit vms style system time
	character*24	c
	integer*4	ok
	integer*4	name_of_month
	integer*2	i2(8)
	integer*4	sys_numtim
	integer*4	ios
	integer*4	i

	if (dummy .eq. 0) ok = 0 ! dummy statement

	buf = ' '
	sys_asctim = 0
	ok = sys_numtim(i2, tim)
	if (ok .ne. 1) return

  2	format(i2.2, '-XXX-', i4.4, ' ', i2.2, ':',i2.2,':',i2.2,'.',i3.3)
	write(c,2,iostat=ios) i2(3), i2(1), i2(4), i2(5), i2(6), i2(7)
	if (ios .ne. 0) then
	   write(6,1) 'internal write error, iostat=', ios, c
	end if

	i = i2(2)
	ok = name_of_month(i, c(4:6))
	if (ok .ne. 1) return

	buf = c

	sys_asctim = 1
	return
  1	format(1x,'SYS_ASCTIM: ', a, i3, 1x, a)
	end

!------------------------------------------------------------------------------
	integer*4	function	sys_numtim(i2ary, t64bit)
! returns 7 16-bit integers in the passed array as year, month[1..12], day of
! month, hour[0..23], minute, second[0..59], milliseconds[0..999]
! Simulates the VMS routine.
	implicit	none
	include		'ui8_def.for'
	include		'vms_time_constants_def.for'
	include		'wind_os_def.for'
	record /ui8/ t64bit
	integer*2	i2ary(*)
	integer*4	year, month, dom, hour, minute, sec, msec, doy
	record /ui8/	r,u,v,w
	integer*4	i
$IF ABSOFT_FORTRAN
	integer*4	ir
$ENDIF
	external	ui8_sub			!$pragma C( ui8_sub )
	external	ui8_is_eq		!$pragma C ( ui8_is_eq )
	external	ui8_cpy			!$pragma C ( ui8_cpy )
	external	ui8_divide		!$pragma C ( ui8_divide )
	integer*4	sign, sign2
	integer*4	vxtosuni4		! a function
$IF ABSOFT_FORTRAN
	integer*4	adjust_doy_and_year	! a function
$ENDIF
	logical*4	is_a_leap_year		! a function
	logical*4	julian_to_mmdd		! a function
	logical*4	lok
	integer*4	err
	integer*4	ios

	sys_numtim = 0
	do i=1,7
	   i2ary(i) = 0
	end do

	year = 1994
	call ui8_cpy(boy1994,u)			! no data recorded at this time
	call ui8_sub(u,t64bit,v,sign)		! so the sign will never be 0

	if (sign .eq. 1) then
	   do while (sign .eq. 1 .and. year .gt. 0)
	      year = year - 1
	      if (is_a_leap_year(year)) then
	         call ui8_sub(u,ui8Lyear,u,err)
	      else
	         call ui8_sub(u,ui8year,u,err)
	      end if
	      if (err .ne. 1) goto 18
	      call ui8_sub(u,t64bit,v,sign)
	   end do
!!!!!!!!!!   call ui8_divide(t64bit,ui8day,w,r,sign2) ----> this is ok, also.
	   call ui8_divide(v,ui8day,w,r,sign2)
	   if (w.j .ne. 0) goto 10
	   if (sign2 .gt. 1 .or. sign2 .lt. -1) goto 12
	   doy = w.i
	   if (sunos) doy = vxtosuni4(w.b)
	   if (macos_ppc) doy = vxtosuni4(w.b)
	   doy = doy + 1
	else if (sign .eq. -1) then
	   call ui8_divide(v,ui8day,w,r,sign2)
	   if (w.j .ne. 0) goto 10
	   if (sign2 .gt. 1 .or. sign2 .lt. -1) goto 12
	   doy = w.i
	   if (sunos) doy = vxtosuni4(w.b)
	   if (macos_ppc) doy = vxtosuni4(w.b)
	   doy = doy + 1
!xxxxx
!	type *, '...sys_numtim: doy, year=', doy,year
$IF ABSOFT_FORTRAN
	   ir = adjust_doy_and_year(doy,year)
$ELSE
	   call adjust_doy_and_year(doy,year)
$ENDIF
	else
	   ! sign should never be zero, but just in case...
	   year   = 1994
	   month  = 1
	   dom    = 1
	   hour   = 0
	   minute = 0
	   sec    = 0
	   msec   = 1
	   goto 1000
	end if

	lok = julian_to_mmdd(doy, year, month, dom)
	if (.not. lok) goto 14

	! the remainder should now contain the number of 10**-7 seconds
	! in a fractional day, so we convert to milliseconds which should
	! leave us with a 32-bit signed quantity less than (60*60*24*1000)

	call ui8_divide(r,ui8msec,w,v,sign2)
	if (w.j .ne. 0) goto 20
	i = w.i
	if (sunos) i = vxtosuni4(w.i)
	if (macos_ppc) i = vxtosuni4(w.i)

	hour = i / msec_per_hour
	i = i - (hour * msec_per_hour)
	minute = i / msec_per_minute
	i = i - (minute * msec_per_minute)
	sec = i / msec_per_sec
	i = i - (sec * msec_per_sec)
	msec = i

 1000	continue

	i2ary(1) = year
	i2ary(2) = month
	i2ary(3) = dom
	i2ary(4) = hour
	i2ary(5) = minute
	i2ary(6) = sec
	i2ary(7) = msec

	sys_numtim = 1
	return
  1	format(1x,'SYS_NUMTIM: ', a)
 10	continue
	write(6,1,iostat=ios) 'ui8 divide error: doy (a).'
	return
 12	continue
	write(6,1,iostat=ios) 'ui8 divide error: doy (b).'
	return
 14	continue
	write(6,1,iostat=ios) 'Julian to MMDD error.'
	return
! 16	continue
!	write(6,1,iostat=ios) 'ui8 add error: year.'
!	return
 18	continue
	write(6,1,iostat=ios) 'ui8 subtract error: year.'
	return
 20	continue
	write(6,1,iostat=ios) 'ui8 divide error: msec.'
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	ui8_show(s,b)
! Writes an 8 byte array to terminal screen in hex prefixed by label s.
	implicit	none
	character*(*)	s
	byte		b(8)
	integer*4	i
	integer*4	ios

	ui8_show = 1

	write(6,1,iostat=ios) s, (b(i), i=1,8)
  1	format(1x,a, 8(1x,z2.2))
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	sys_bintim(ch_time, t64bit)
! Converts ASCII date-time string to 64-bit vax/vms style time
! Simulates the VMS routine.
	implicit	none
	include		'wind_os_def.for'
	include		'ui8_def.for'
	include		'vms_time_constants_def.for'
	character*(*)	ch_time
	record /ui8/ t64bit
	integer*4	year, month, dom, hour, minute, sec, msec
	integer*4	doy
	character*24	ct
	integer*4	i,j,k,n
	record /ui8/	u,v,w
	integer*4	ios
	integer*4	ok
	integer*4	err
	external	ui8_is_eq		!$pragma C ( ui8_is_eq )
	external	ui8_cpy			!$pragma C ( ui8_cpy )
	external	ui8_sub			!$pragma C( ui8_sub )
	external	ui8_divide		!$pragma C ( ui8_divide )
	external	ui8_add			!$pragma C ( ui8_add )
	external	ui8_multiply		!$pragma C ( ui8_multiply )
	integer*4	suntovxi4		! a function
	integer*4	iday_of_year		! a function
	integer*4	number_of_month		! a function
$IF ABSOFT_FORTRAN
!	logical*4	is_a_leap_year		! a function
$ELSE
	logical*4	is_a_leap_year		! a function
$ENDIF
	logical*4	is_digit		! a statement function
	character*1	c			! statement function argument
	is_digit(c) = c .ge. '0' .and. c .le. '9'

	sys_bintim = 0
	ct = ch_time
	call ui8_cpy(ui8zero,t64bit)

  2	format(i2.2, 5x, i4.4, 1x, i2.2, 1x, i2.2,1x, i2.2,1x ,i2.2)
  3	format(i2.2, 5x, i4.4, 1x, i2.2, 1x, i2.2,1x, i2.2,1x ,i3.3)
	if (is_digit(ct(24:24))) then
	   read(ct,3,iostat=ios) dom, year, hour, minute, sec, msec
	else
	   read(ct,2,iostat=ios) dom, year, hour, minute, sec, msec
	   msec = msec * 100
	end if
	if (ios .ne. 0) goto 10

	ok = number_of_month(month, ct(4:6))
	if (ok .ne. 1) goto 20

	doy = iday_of_year(year, month, dom)
	if (doy .eq. 0) goto 30

	!type *, '...sys_bintim...(a)'

	call ui8_cpy(boy1994,u)
	j = 1994
	i = year - j
	if (i .gt. 0) then
	   ! leap year is 1996
	   k = i / 4
	   n = mod(i,4)
	   if (n .ge. 3) k = k + 1
	   v.i = i
	   v.j = 0
	   if (sunos) v.i = suntovxi4(i)
	   if (macos_ppc) v.i = suntovxi4(i)
	   call ui8_multiply(v,ui8year,w,err) ! number of years
	   call ui8_add(u,w,u,err)
	   v.i = k
	   if (sunos) v.i = suntovxi4(k)
	   if (macos_ppc) v.i = suntovxi4(k)
	   call ui8_multiply(v,ui8day,w,err) ! all leap year corrections
	   call ui8_add(u,w,u,err)
	else if (i .lt. 0) then
	   ! leap year is 1992
	   i = -i
	   k = i / 4
	   n = mod(i,4)
	   if (n .ge. 2) k = k + 1
	   v.i = i
	   v.j = 0
	   if (sunos) v.i = suntovxi4(i)
	   if (macos_ppc) v.i = suntovxi4(i)
	   call ui8_multiply(v,ui8year,w,err) ! number of years
	   call ui8_sub(u,w,u,err)
	   v.i = k
	   if (sunos) v.i = suntovxi4(k)
	   if (macos_ppc) v.i = suntovxi4(k)
	   call ui8_multiply(v,ui8day,w,err) ! all leap year corrections
	   call ui8_sub(u,w,u,err)
	else
	end if
	!type *, '...sys_bintim...(b)'

! old, pre Jan-96, breaks on 01-jan-1996, jk
!	do while(i .ne. 0)
!	   if (i .gt. 0) then
!	      ! we go year by year adding
!	      j = j + 1
!	      if (is_a_leap_year(j)) then
!	         call ui8_add(u,ui8Lyear,u,err)
!	      else
!	         call ui8_add(u,ui8year,u,err)
!	      end if
!	      if (err .ne. 1) goto 40
!	   else if (i .lt. 0) then
!	      j = j - 1
!	      if (is_a_leap_year(j)) then
!	         call ui8_sub(u,ui8Lyear,u,err)
!	      else
!	         call ui8_sub(u,ui8year,u,err)
!	      end if
!	      if (err .ne. 1) goto 42
!	   end if
!	   i = year - j
!	end do

	w.j = 0
	doy = doy - 1					! day of year
	w.i = doy
	if (sunos) w.i = suntovxi4(doy)
	if (macos_ppc) w.i = suntovxi4(doy)
	call ui8_multiply(ui8day,w,v,err)
	if (err .ne. 0) goto 50
	call ui8_add(u,v,u,err)

	! compute the millisecond of the day
	i = (hour * msec_per_hour) +
	1   (minute * msec_per_minute) +
	1   (sec * msec_per_sec) +
	1    msec
	w.i = i
	if (sunos) w.i = suntovxi4(i)
	if (macos_ppc) w.i = suntovxi4(i)
	call ui8_multiply(ui8msec,w,v,err)
	if (err .ne. 0) goto 60
	call ui8_add(u,v,u,err)

	call ui8_cpy(u,t64bit)
	!type *, '...sys_bintim...(c)'

	sys_bintim = 1
	return
  1	format(1x,'SYS_BINTIM: ', a, :, a)
 10	continue
	write(6,1,iostat=ios) 'error reading values from ', ct
	return
  20	continue
	write(6,1,iostat=ios) 'error getting number of month from ', ct
	return
 30	continue
	write(6,1,iostat=ios) 'error getting Julian DOY from ', ct
	return
! 40	continue
!	write(6,1,iostat=ios) 'ui8_add error, str: ', ct
!	return
! 42	continue
!	write(6,1,iostat=ios) 'ui8_sub error, str: ', ct
!	return
 50	continue
	write(6,1,iostat=ios) 'ui8_multiply error, DOY: ', ct
	return
 60	continue
	write(6,1,iostat=ios) 'ui8_multiply error, msec: ', ct
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	lib_sub_times(t1,t2,t3)
! Simulates the VMS routine lib$sub_times, but doesn't flag delta time
! values in the same way.
	implicit	none
	include		'parm_def.for'
	record /vms_64bit_time/ t1, t2, t3
	include		'wind_return_code_def.for'
	integer*4	err
	external	ui8_sub	!$pragma C( ui8_sub )

	call ui8_sub(t1,t2,t3,err)
	if (err .eq. 1) then
	   lib_sub_times = 1
	else if (err .eq. -1) then
	   lib_sub_times = lib_negtime
	else if (err .eq. 0) then
	   lib_sub_times = 1
	else
	   lib_sub_times = 0
	end if

	return
	end

!------------------------------------------------------------------------------
	character*24	function	pb5_to_str(pb5)
! For a given 56-bit PB5 time this routine returns the string representation
! in YYYY DDD HH:MM:SS.nnnnn
	implicit	none
	structure /pb5_time/
	   union
	   map
	   integer*4	x
	   integer*4	y
	   end map
	   map
	   byte		b(8)
	   end map
	   end union
	end structure
	record /pb5_time/ pb5
	record /pb5_time/ w

	character*24	s
!	integer*4	year
	integer*4	tjd
!	integer*4	doy
	integer*4	sec
	integer*4	millisec
	integer*4	microsec
	integer*4	tenths
	integer*4	i,j,k,m,n,o
	integer*4	ios
!	integer*4	brev
!	character*80	str

	! just for the vax
	w.b(1) = pb5.b(4)
	w.b(2) = pb5.b(3)
	w.b(3) = pb5.b(2)
	w.b(4) = pb5.b(1)
	w.b(5) = pb5.b(8)
	w.b(6) = pb5.b(7)
	w.b(7) = pb5.b(6)
	w.b(8) = pb5.b(5)
!	w = pb5

	tjd = ibits(w.x,00,14)
	sec = ibits(w.x,14,17)
	i = ibits(sec,0,2)
	j = ibits(sec,2,8)
	k = ibits(sec,10,7)
	m = ibits(sec,0,7)
	n = ibits(sec,7,8)
	o = ibits(sec,15,2)
!	write(6,4,iostat=ios) i,j,k,m,n,o
!  4	format(1x,3(1x,z2.2), ' +++++ ', 3(1x,z2.2))

	millisec = w.y .and. '0777'o !'01ff'x
	millisec = ishft(millisec,-1)
$IF ABSOFT_FORTRAN
	if (btest(w.x,31)) millisec = millisec .or. int('01'x)
$ELSE
	if (btest(w.x,31)) millisec = millisec .or. '01'x
$ENDIF

	microsec = ibits(w.y,9,10)
	tenths   = ibits(w.y,19,5)

!	type *, ' '
!	write(6,3,iostat=ios) tjd, sec, millisec, microsec, tenths
!	write(6,2,iostat=ios) tjd, sec, millisec, microsec, tenths
!  2	format(1x,'tjd:',i5,' s:',i6, ' ms:', i4, ' us:', i4, ' th:', i2)
!  3	format(1x,'tjd:',z4.4,' s:',z5.5, ' ms:', z3.3, 
!	1	' us:', z3.3, ' th:', z2.2)

!	type 5, w.b, pb5.b
!	type 5, (w.b(i), i=1,8), (pb5.b(j), j=1,8)
!  5	format(1x,('sc_clock: ', 8(1x,z2.2)), (' orig: ', 8(1x,z2.2)) )

!	write(str(:40),5,iostat=ios) 'sc_clock:', (pb5.b(i), i=1,8)
!	write(str(44:),5) 'new: ', w.b
!	write(6,*) str
! 5	format(1x, a, 8(1x,z2.2) )
!	write(6,6) 'sc_clock: ', pb5.x, pb5.y, ' new ', w.x, w.y
!  6	format(1x, a, z8.8,1x,z8.8, 2x, a, z8.8, 1x, z8.8)

!	tjd = brev(tjd,0,13)
!	sec = brev(sec,0,16)
!	millisec = brev(millisec,0,9)
!	microsec = brev(microsec,0,9)
!	tenths   = brev(tenths,0,4)
!	write(6,3,iostat=ios) tjd, sec, millisec, microsec, tenths
!	write(6,2,iostat=ios) tjd, sec, millisec, microsec, tenths


	write(s,1,iostat=ios) tjd, sec, millisec, microsec
  1	format(i5,':',i5,'.',i3.3,i3.3)

	pb5_to_str = s

	return
	end

!------------------------------------------------------------------------------
	character*24	function	atc_to_str
	1				(atcyear,atcdoy,atcmsec,atcusec)
! For a given ATC time this routine returns the string representation
! in YYYY DDD HH:MM:SS.nnnnn
	implicit	none
	integer*4	atcyear
	integer*4	atcdoy
	integer*4	atcmsec
	integer*4	atcusec

	character*24	s
	integer*4	sec
	integer*4	hour
	integer*4	minute
	integer*4	msec
	integer*4	usec
	integer*4	ios

	sec = atcmsec / 1000
	hour = sec/3600
	sec = sec - (hour*3600)
	minute = sec/60
	sec = sec - (minute*60)
	msec = mod(atcmsec,1000)
	usec = atcusec

	write(s,1,iostat=ios) atcyear, atcdoy, hour, minute, sec, msec, usec
  1	format(i4,1x,i3.3,1x,i2.2,':',i2.2,':',i2.2, '.', i3.3, i3.3)

	atc_to_str = s
	return
	end

!------------------------------------------------------------------------------
	character*24	function	atc_to_str2
	1				(atcyear,atcdoy,atcmsec)
! For a given ATC time this routine returns the string representation
! in DD-MMM-YYYY HH:MM:SS.nnn
	implicit	none
	integer*4	atcyear
	integer*4	atcdoy
	integer*4	atcmsec

	character*24	s
	integer*4	sec
	integer*4	hour
	integer*4	minute
	integer*4	msec
	integer*4	ios
	integer*4	day
	integer*4	month
	logical*4	julian_to_mmdd
	integer*4	name_of_month
	integer*4	ok
	logical*4	lok
	character*4	name

	sec = atcmsec / 1000
	hour = sec/3600
	sec = sec - (hour*3600)
	minute = sec/60
	sec = sec - (minute*60)
	msec = mod(atcmsec,1000)

	lok = julian_to_mmdd(atcdoy, atcyear, month, day)
	ok = name_of_month(month, name)

	write(s,2,iostat=ios) day, name, atcyear, hour, minute, sec, msec

	atc_to_str2 = s
	return
$IF ABSOFT_FORTRAN
  2	format(i2.2, '-',a3,'-', i4.4, ' ', i2.2, ':',i2.2,':',
	1    i2.2,'.',i3.3)
$ELSE
  2	format(i2.2, '-',a3,'-', i4.4, ' ', i2.2, ':',i2.2,':',i2.2,'.',i3.3)
$ENDIF
	end

!------------------------------------------------------------------------------
	integer*4	function	atc_to_c24
	1				(atcyear,atcdoy,atcmsec,buf)
! For a given ATC time this routine returns the string representation
! in DD-MMM-YYYY HH:MM:SS.nnn
	implicit	none
	integer*4	atcyear
	integer*4	atcdoy
	integer*4	atcmsec
	character*(*)	buf

	character*24	s
	integer*4	sec
	integer*4	hour
	integer*4	minute
	integer*4	msec
	integer*4	ios
	integer*4	day
	integer*4	month
	logical*4	julian_to_mmdd
	integer*4	name_of_month
	integer*4	ok
	logical*4	lok
	character*4	name

	atc_to_c24 = 0

	sec = atcmsec / 1000
	hour = sec/3600
	sec = sec - (hour*3600)
	minute = sec/60
	sec = sec - (minute*60)
	msec = mod(atcmsec,1000)

	lok = julian_to_mmdd(atcdoy, atcyear, month, day)
	ok = name_of_month(month, name)

	write(s,2,iostat=ios,err=10) 
	1	day, name, atcyear, hour, minute, sec, msec
	buf = s

	atc_to_c24 = 1
	return
  1	format(1x,'ATC_TO_C24: ', a, 3(1x,i))
$IF ABSOFT_FORTRAN
  2	format(i2.2, '-',a3,'-', i4.4, ' ', i2.2, ':',i2.2,
	1    ':',i2.2,'.',i3.3)
$ELSE
  2	format(i2.2, '-',a3,'-', i4.4, ' ', i2.2, ':',i2.2,':',i2.2,'.',i3.3)
$ENDIF
 10	write(6,1,iostat=ios) 'conversion err, input=', 
	1	atcyear,atcdoy,atcmsec
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	c24_to_atc
	1				(buf,atcyear,atcdoy,atcmsec)
! For a given string representation in DD-MMM-YYYY HH:MM:SS.nnn format
! this routine returns the corresponding ATC time format.
!
	implicit	none
	integer*4	atcyear
	integer*4	atcdoy
	integer*4	atcmsec
	character*(*)	buf

	character*24	s
	integer*4	sec
	integer*4	hour
	integer*4	minute
	integer*4	msec
	integer*4	ios
	integer*4	day
	integer*4	month
	logical*4	iday_of_year
	integer*4	number_of_month
	integer*4	ok
	character*4	name

	c24_to_atc = 0

	s = buf
	read(s,2,iostat=ios,err=10) 
	1	day, name, atcyear, hour, minute, sec, msec

	atcmsec = msec + (1000 * (sec + (minute*60) + (hour*3600)) )

	ok = number_of_month(month, name)
	if (ok .ne. 1) goto 10

	atcdoy = iday_of_year(atcyear, month, day)
	if (atcdoy .eq. 0) goto 10

	c24_to_atc = 1
	return
  1	format(1x,'C24_TO_ATC: ', a, 3(1x,i))
$IF ABSOFT_FORTRAN
  2	format(i2.2, '-',a3,'-', i4.4, ' ', i2.2, ':',i2.2,':',
	1    i2.2,'.',i3.3)
$ELSE
  2	format(i2.2, '-',a3,'-', i4.4, ' ', i2.2, ':',i2.2,':',i2.2,'.',i3.3)
$ENDIF
 10	write(6,1,iostat=ios) 'conversion err, input=', 
	1	atcyear,atcdoy,atcmsec
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	c24_to_wnd(c24,sys)
! Converts ASCII 24 character time in format:
!
!	dd-mmm-yyyy hh:mm:ss.cc
!
! to unsigned 64-bit WND standard time format (like 8-byte VMS system time)
	implicit	none
	character*(*)	c24		! earth receive time
	byte		sys(8)		! system time
	integer*4	sys_bintim	! a system service

	call to_upper(c24, 0, 0)

	c24_to_wnd = sys_bintim(c24, sys)
	if (c24_to_wnd.ne.1) goto 30

	return
 30	c24_to_wnd = 0
	type *, 'C24_TO_WND: Invalid time: ', c24
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wnd_to_c24(sys,buf)
! Converts unsigned 64-bit WND standard time format (like 8-byte VMS system 
! time) to ASCII 24 character time in format
!
!	dd-mmm-yyyy hh:mm:ss.cc
!
	implicit	none
	character*(*)	buf
	character*24	c24		! earth receive time
	byte		sys(8)		! system time
	integer*4	sys_asctim	! a system service
	integer*4	dummy

	wnd_to_c24 = sys_asctim(dummy,c24, sys)
	buf = c24
	if (wnd_to_c24.ne.1) goto 30

	return
 30	wnd_to_c24 = 0
	type *, 'WND_TO_C24: Invalid time: ', c24
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	c24_to_dbms
	1		(argc24,dbms_date,dbms_time,dbms_msec)
! This routine takes a string of the form DD-MMM-YYYY HH:MM:SS.CCC and converts
! it to separate date integer and time integer.  The date and time integer
! are binary, but their ASCII representations are YYYYMMDD and HHMMSS,
! respectively.  Note that fractional seconds are lost.
! [Months are handled in a "portable" way.]
	implicit	none
	character*(*)	argc24
	integer*4	dbms_date
	integer*4	dbms_time
	integer*4	dbms_msec
	integer*4	a,b,c
	character*3	month
	integer*4	o
	character*24	c24

	c24_to_dbms = 0

	if (argc24(2:2) .eq. '-') then
	   c24 = '0'//argc24
	else
	   c24 = argc24
	end if

	read(c24(1:2),9,err=10) c			! day
	month = c24(4:6)
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
	   goto 10
	end if
	read(c24(8:11),9,err=10) a			! year
	dbms_date = (a*10000) + (b*100) + c

	read(c24(13:14),9,err=10) a			! hour
	read(c24(16:17),9,err=10) b			! minute
	read(c24(19:20),9,err=10) c			! second
	dbms_time = (a*10000) + (b*100) + c

	read(c24(22:24),9,err=10) dbms_msec		! milliseconds

	c24_to_dbms = 1

	return
  9	format(i)
  1	format(1x,'C24_TO_DBMS: ', a, a)
 10	c24_to_dbms = 0
	write(6,1,iostat=o) 'cannot read date/time from ', c24
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	dbms_to_c24
	1	(dbms_date,dbms_time,dbms_msec,buf)
! This routine converts the binary date and time integer arguments
! (whose ASCII representations are YYYYMMDD and HHMMSS, respectively)
! to a string of the form DD-MMM-YYYY HH:MM:SS.CCC.
! Note that fractional seconds, CCC, are always zero.
! [Months are handled in a "portable" way.]
	implicit	none
	character*(*)	buf
	character*24	c24
	integer*4	dbms_date
	integer*4	dbms_time
	integer*4	dbms_msec
	integer*4	a,b,c
	character*3	cmonth
	integer*4	ios
	integer*4	year, month, day, hour, minute, second
	integer*4	name_of_month

	dbms_to_c24 = 0

	buf = ' '
	c24 = ' '
	year = dbms_date / 10000			! year
	a = year * 10000
	month = (dbms_date - a ) / 100			! month
	b = month * 100
	day   = (dbms_date - a - b)			! day

	c = name_of_month(month, cmonth)
	if (c .ne. 1) goto 10

	hour = dbms_time / 10000
	a = hour * 10000
	minute = (dbms_time - a) / 100
	b = minute * 100
	second = (dbms_time - a - b)

	write(c24,3,iostat=ios,err=10) day, cmonth, year, 
	1	hour, minute, second, dbms_msec

$IF ABSOFT_FORTRAN
  3	format(i2.2,'-',a3,'-', i4.4, ' ', i2.2, ':', i2.2, ':',
	1    i2.2,'.',i3.3)
$ELSE
  3	format(i2.2,'-',a3,'-', i4.4, ' ', i2.2, ':', i2.2, ':', i2.2,'.',i3.3)
$ENDIF

	buf = c24
	dbms_to_c24 = 1

	return
  9	format(i)
  1	format(1x,'DBMS_to_C24: ', a, ', date=', i8, ' time=', i6)
 10	dbms_to_c24 = 0
	buf = c24
	write(6,1,iostat=ios) 'error converting time from ', 
	1	dbms_date, dbms_time
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wnd_to_dbms(t_wnd, db1, db2, dbm)
! Converts 64-bit wnd (vms/vax system format) time to time format used by
! the wind/waves tm extraction item database.
	implicit	none
	include		'ui8_def.for'
	record /ui8/	t_wnd
	integer*4	db1		! yyyymmdd
	integer*4	db2		! hhmmss
	integer*4	dbm		! ccc : 0 <= milliseconds < 1000
	integer*2	i2(8)
	integer*4	sys_numtim
	integer*4	ok

	wnd_to_dbms = 0
	db1 = 0
	db2 = 0
	dbm = 0

	ok = sys_numtim(i2, t_wnd)
	if (ok .ne. 1) return

	db1 = (zext(i2(1)) * 10000) + (i2(2) * 100) + i2(3)
	db2 = (zext(i2(4)) * 10000) + (i2(5) * 100) + i2(6)
	dbm = i2(7)

	wnd_to_dbms = 1

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	dbms_to_wnd(db1, db2, dbm, t_wnd)
! Converts wind/waves tm extraction item database time to 64-bit wnd (vms/vax
! system format) time.
	implicit	none
	include		'ui8_def.for'
	record /ui8/	t_wnd
	integer*4	db1		! yyyymmdd
	integer*4	db2		! hhmmss
	integer*4	dbm		! ccc : 0 <= milliseconds < 1000
	character*24	c
	integer*4	dbms_to_c24
	integer*4	c24_to_wnd
	integer*4	ok

	dbms_to_wnd = 0
	t_wnd.i = 0
	t_wnd.j = 0

	ok = dbms_to_c24(db1, db2, dbm, c)
	if (ok .ne. 1) return

!	type *, '...dbms_to_wnd: ', c

	ok = c24_to_wnd(c, t_wnd)
	if (ok .ne. 1) return

	dbms_to_wnd = 1

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	c24_to_ur8(c24, ur8)
! This routine takes a string of the form DD-MMM-YYYY HH:MM:SS.CCC and converts
! it to Ulysses format real*8 days since 1982.
	implicit	none
	character*(*)	c24
	real*4		ur8
	integer*4	dbms_date
	integer*4	dbms_time
	integer*4	dbms_msec
	integer*4	c24_to_dbms
	integer*4	dbms_to_ur8
	integer*4	ok

	c24_to_ur8 = 0
	ur8 = 0.0

	ok = c24_to_dbms(c24, dbms_date, dbms_time, dbms_msec)
	if (ok .ne. 1) goto 10

	ok = dbms_to_ur8(dbms_date, dbms_time, dbms_msec, ur8)
	if (ok .ne. 1) goto 11

	c24_to_ur8 = 1
	return
  1	format(1x,'C24_TO_UR8: ', a, a)
 10	c24_to_ur8 = 0
	type 1, 'error (a) converting from ', c24
	return
 11	c24_to_ur8 = 0
	type 1, 'error (b) converting from ', c24
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	ur8_to_c24(ur8,c24)
! Converts Ulysses real*8 time format to ASCII 24-character string.
	implicit	none
	real*8		ur8
	character*(*)	c24
	character*24	buf
	integer*4	ok
	integer*4	i,j,k
	integer*4	ur8_to_dbms
	integer*4	dbms_to_c24

	ur8_to_c24 = 0
	c24 = ' '

	ok = ur8_to_dbms(ur8, i,j,k)
	if (ok .ne. 1) goto 10

	ok = dbms_to_c24(i,j,k, buf)
	if (ok .ne. 1) goto 20

	c24 = buf

	ur8_to_c24 = 1
	return
  1	format(1x,'UR8_TO_C24: ', a, g)
 10	write(6,1,iostat=ok) 'error (1) converting ur8=', ur8
	return
 20	write(6,1,iostat=ok) 'error (2) converting ur8=', ur8
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	ur8_to_wnd(ur8,wnd)
! Converts Ulysses real*8 to 64-bit WND format
	implicit	none
	real*8		ur8
	byte		wnd(8)
	integer*4	ok
	integer*4	i,j,k
	integer*4	ur8_to_dbms
	integer*4	dbms_to_wnd

	ur8_to_wnd = 0
	do i=1,8
	   wnd(i) = 0
	end do

	ok = ur8_to_dbms(ur8, i,j,k)
	if (ok .ne. 1) goto 10

	ok = dbms_to_wnd(i,j,k, wnd)
	if (ok .ne. 1) goto 10

	ur8_to_wnd = 1
	return
  1	format(1x,'UR8_TO_WND: ', a, g)
 10	write(6,1,iostat=ok) 'error converting ur8=', ur8
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	ur8_to_atc(ur8,atcy, atcd, atcm)
! Converts Ulysses real*8 time to CDHF ATC time.
	implicit	none
	real*8		ur8
	integer*4	atcy		! atc year
	integer*4	atcd		! atc day of year
	integer*4	atcm		! atc millisecond of day
	integer*4	ok
	integer*4	i,j,k
	integer*4	ur8_to_dbms
	integer*4	dbms_to_atc

	ur8_to_atc = 0
	atcy = 0
	atcd = 0
	atcm = 0

	ok = ur8_to_dbms(ur8, i,j,k)
	if (ok .ne. 1) goto 10

	ok = dbms_to_atc(i,j,k, atcy, atcd, atcm)
	if (ok .ne. 1) goto 10

	ur8_to_atc = 1
	return
  1	format(1x,'UR8_TO_ATC: ', a, g)
 10	write(6,1,iostat=ok) 'error converting ur8=', ur8
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	atc_to_dbms(a1,a2,a3, d1,d2,d3)
! Converts CDHF ATC time to integer format used by item extraction data base.
	implicit	none
	integer*4	a1		! atc year
	integer*4	a2		! atc day of year
	integer*4	a3		! atc millisecond of day
	integer*4	d1		! yyyymmdd
	integer*4	d2		! hhmmss
	integer*4	d3		! msec
	integer*4	year, day, month, hour, minute, second, msec
	integer*4	n

	integer*4	msec_per_sec
	integer*4	msec_per_minute
	integer*4	msec_per_hour
	parameter	(msec_per_sec=1000)
	parameter	(msec_per_minute=1000*60)
	parameter	(msec_per_hour=1000*60*60)

	integer*4	ok
	logical*4	lok
	logical*4	julian_to_mmdd

	atc_to_dbms = 0
	d1 = 0
	d2 = 0
	d3 = 0
	year = a1

	lok = julian_to_mmdd(a2, year, month, day)
	if (.not. lok) goto 10

	n = a3
	hour = n / msec_per_hour
	n = n - (hour * msec_per_hour)
	minute = n / msec_per_minute
	n = n - (minute * msec_per_minute)
	second = n / msec_per_sec
	msec = n - (second * msec_per_sec)

	d1 = (year*10000) + (month*100) + day
	d2 = (hour*10000) + (minute*100) + second
	d3 = msec

	atc_to_dbms = 1

	return
  1	format(1x,'ATC_TO_DBMS: ', a, 3(1x,i))
 10	write(6,1,iostat=ok) 'error using this ATC: ',a1,a2,a3
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	dbms_to_atc(d1,d2,d3, a1,a2,a3)
! Converts item extraction database format time to CDHF ATC time format.
	implicit	none
	integer*4	d1		! yyyymmdd
	integer*4	d2		! hhmmss
	integer*4	d3		! msec
	integer*4	a1		! atc year
	integer*4	a2		! atc day of year
	integer*4	a3		! atc millisecond of day
	integer*4	year, day, month, hour, minute, second
	integer*4	n

	integer*4	msec_per_sec
	integer*4	msec_per_minute
	integer*4	msec_per_hour
	parameter	(msec_per_sec=1000)
	parameter	(msec_per_minute=1000*60)
	parameter	(msec_per_hour=1000*60*60)

$IF ABSOFT_FORTRAN
!	integer*4	ok
$ELSE
	integer*4	ok
$ENDIF
	integer*4	iday_of_year

	dbms_to_atc = 0

	n = d1
	year = n / 10000
	n = n - (year * 10000)
	month = n / 100
	day = n - (month * 100)

	a1 = year
	a2 = iday_of_year(year,month,day)

	n = d2
	hour = n / 10000
	n = n - (hour * 10000)
	minute = n / 100
	second = n - (minute * 100)

	a3 = d3
	a3 = a3 + (second * msec_per_sec)
	a3 = a3 + (minute * msec_per_minute)
	a3 = a3 + (hour   * msec_per_hour)

	dbms_to_atc = 1

	return
  1	format(1x,'DBMS_TO_ATC: ', a, 3(1x,i))
! 10	write(6,1,iostat=ok) 'error using this DBMS: ',d1,d2,d3
!	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wnd_to_atc(wnd, a1,a2,a3)
! Converts 64-bit WND time to CDHF ATC format time.
	implicit	none
	byte		wnd(8)
	integer*4	a1		! atc year
	integer*4	a2		! atc day of year
	integer*4	a3		! atc millisecond of day
	integer*4	ok
	integer*4	wnd_to_dbms
	integer*4	dbms_to_atc
	integer*4	i,j,k

	wnd_to_atc = 0

	ok = wnd_to_dbms(wnd, i,j,k)
	if (ok .ne. 1) goto 10

	ok = dbms_to_atc(i,j,k, a1,a2,a3)
	if (ok .ne. 1) goto 10

	wnd_to_atc = 1
	return
  1	format(1x,'WND_TO_ATC: ', a, 8(1x,z2.2))
 10	write(6,1,iostat=ok) 'error using this wnd: ', (wnd(i), i=1,8)
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	wnd_to_ur8(wnd, ur8)
! Converts 64-bit WND format time to Ulysses real*8 format time.
	implicit	none
	byte		wnd(8)
	real*8		ur8
	integer*4	i,j,k
	integer*4	ok
	integer*4	wnd_to_dbms
	integer*4	dbms_to_ur8

	wnd_to_ur8 = 0

	ok = wnd_to_dbms(wnd, i,j,k)
	if (ok .ne. 1) goto 10

	ok = dbms_to_ur8(i,j,k, ur8)
	if (ok .ne. 1) goto 10

	wnd_to_ur8 = 1
	return
  1	format(1x,'WND_TO_UR8: ', a, 8(1x,z2.2))
 10	write(6,1,iostat=ok) 'error using this wnd: ', (wnd(i), i=1,8)
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	atc_to_ur8(a1,a2,a3, ur8)
! Converts CDHF ATC time to Ulysses real*8 format time.
	implicit	none
	integer*4	a1		! atc year
	integer*4	a2		! atc day of year
	integer*4	a3		! atc millisecond of day
	real*8		ur8		! Ulysses format time
	integer*4	i,j,k
	integer*4	ok
	integer*4	atc_to_dbms
	integer*4	dbms_to_ur8

	atc_to_ur8 = 0

	ok = atc_to_dbms(a1,a2,a3, i,j,k)
	if (ok .ne. 1) goto 10

	ok = dbms_to_ur8(i,j,k, ur8)
	if (ok .ne. 1) goto 10

	atc_to_ur8 = 1
	return
  1	format(1x,'ATC_TO_UR8: ', a, 8(1x,z2.2))
 10	write(6,1,iostat=ok) 'error using this ATC: ', a1,a2,a3
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	atc_to_wnd(a1, a2, a3, t_wnd)
! Converts CDHF ATC time format to WND/VMS 64-bit format time.
	implicit	none
	integer*4	a1		! atc year
	integer*4	a2		! atc day of year
	integer*4	a3		! atc millisecond of day
	include		'ui8_def.for'
	record /ui8/	t_wnd		! WND format time
	integer*4	atc_to_dbms
	integer*4	dbms_to_wnd
	integer*4	ok
	integer*4	dt, ti, ms

	atc_to_wnd = 0
	t_wnd.i = 0
	t_wnd.j = 0

	ok = atc_to_dbms(a1, a2, a3, dt, ti, ms)
	if (ok .ne. 1) return

	ok = dbms_to_wnd(dt, ti, ms, t_wnd)
	if (ok .ne. 1) return

	atc_to_wnd = 1

	return
	end

!------------------------------------------------------------------------------
! This is the CDF C language JulianDay routine.  Supposed to compute the
! day since 0 AD (what about those 10 days in 1582 ?)
	integer*4	function	cdf_julianday(y,m,d)
	implicit	none
	integer*4	y	! year, 0..9999
	integer*4	m	! month, 1..12
	integer*4	d	! day, 1..31
	integer*4	jd

	jd = (367*y-7*(y+(m+9)/12)/4-3*((y+(m-9)/7)/100+1)/4+275*m/9+d+1721029)

	cdf_julianday = jd
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	cdf_epochbreakdown
	1	(epoch,year,month,day,hour,minute,second,msec)
	implicit	none
	real*8		epoch
	integer*4	year
	integer*4	month
	integer*4	day
	integer*4	hour
	integer*4	minute
	integer*4	second
	integer*4	msec
	integer*4	jd,i,j,k,l,n
	real*8		msec_ad, second_ad, minute_ad, hour_ad, day_ad
	real*8		MAX_EPOCH_BINARY
	parameter	(MAX_EPOCH_BINARY=3.15569519999999e14)
	real*8		d24, d60, d1000
	parameter	(d24=24.0, d60=60.0, d1000=1000.0)

	cdf_epochbreakdown = 1

	if (epoch .lt. 0.0) epoch = -epoch
	epoch = min(MAX_EPOCH_BINARY, epoch)

	msec_AD = epoch
	second_AD = msec_AD / 1000.0
	minute_AD = second_AD / 60.0
	hour_AD = minute_AD / 60.0
	day_AD = hour_AD / 24.0

	jd = (1721060 + day_AD)
	l=jd+68569
	n=4*l/146097
	l=l-(146097*n+3)/4
	i=4000*(l+1)/1461001
	l=l-1461*i/4+31
	j=80*l/2447
	k=l-2447*j/80
	l=j/11
	j=j+2-12*l
	i=100*(n-49)+i+l

	year  = i
	month = j
	day  = k

	hour   = dmod (hour_AD, d24)
	minute = dmod (minute_AD,d60)
	second = dmod (second_AD, d60)
	msec   = dmod (msec_AD, d1000)

	return
	end

!------------------------------------------------------------------------------
	real*8		function	cdf_computeepoch
	1		(year, month, day, hour, minute, second, msec)
	implicit	none
	integer*4	year
	integer*4	month
	integer*4	day
	integer*4	hour
	integer*4	minute
	integer*4	second
	integer*4	msec
	integer*4	jd
	real*8		epoch
	integer*4	cdf_julianday

	if ((day    .lt. 1 .or. day    .gt. 31)   .or.
	1   (month  .lt. 1 .or. month  .gt. 12)   .or.
	1   (year   .lt. 0 .or. year   .gt. 9999) .or.
	1   (hour   .lt. 0 .or. hour   .gt. 23)   .or.
	1   (minute .lt. 0 .or. minute .gt. 59)   .or.
	1   (second .lt. 0 .or. second .gt. 59)   .or.
	1   (msec   .lt. 0 .or. msec   .gt. 999)) then
	   epoch = -1.0
	else
	   jd = cdf_JulianDay (year, month, day) - 1721060
	   epoch = jd
	   epoch = epoch * 24.0 + hour
	   epoch = epoch * 60.0 + minute
	   epoch = epoch * 60.0 + second
	   epoch = epoch * 1000.0 + msec
	end if

	cdf_computeepoch = epoch

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_to_epoch(ur8, epoch)
	implicit	none
	real*8		ur8
	real*8		epoch
	integer*4	year, month, day
	integer*4	hour, minute, second, msec
	integer*4	w_ur8_to_ymd 			! a function
	real*8		cdf_computeepoch		! a function
	integer*4	ok

	ok = w_ur8_to_ymd(ur8,year,month,day,hour,minute,second,msec)

	epoch = cdf_computeepoch(year,month,day,hour,minute,second,msec)

	w_ur8_to_epoch = ok

	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_from_epoch(ur8,epoch)
	implicit	none
	real*8		ur8
	real*8		epoch
	integer*4	year, month, day
	integer*4	hour, minute, second, msec
	integer*4	w_ur8_from_ymd			! a function
	real*8		cdf_epochbreakdown		! a function
	integer*4	ok

	ok = cdf_epochbreakdown(epoch,year,month,day,hour,minute,second,msec)

	ok = w_ur8_from_ymd(ur8,year,month,day,hour,minute,second,msec)

	w_ur8_from_epoch = ok

!	if (ok .ne. 1) then
!	   type *, '...error...Y M D:', year, month, day
!	   type *, '...........HMS.m:', hour, minute, second, msec
!	end if

	return
	end

!------------------------------------------------------------------------------
! returns time string in form dd-mmm-yyyy:hh:mm:ss.nnn
	integer*4	function	w_ur8_to_string_absolute(ur8,str)
	implicit	none
	real*8		ur8
	character*(*)	str
	integer*4	ur8_to_c24
	character*24	c24
	integer*4	ok

	w_ur8_to_string_absolute = 0

	ok = ur8_to_c24(ur8, c24)
	if (ok .ne. 1) goto 10

	c24(12:12) = ':'

	str = c24
	w_ur8_to_string_absolute = 1
	return
 10	continue
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_from_string_absolute(ur8,str)
	implicit	none
	real*8		ur8
	character*(*)	str
	integer*4	c24_to_ur8
	character*24	c24
	integer*4	ok

	w_ur8_from_string_absolute = 0

	c24 = str
	c24(12:12) = ' '

	ok = c24_to_ur8(c24, ur8)
	if (ok .ne. 1) goto 10

	w_ur8_from_string_absolute = 1
	return
 10	continue
	return
	end

!------------------------------------------------------------------------------
! returns sortable time string in form yyyy-mm-dd hh:mm:ss.nnn
	integer*4	function	w_ur8_to_string_comparison(ur8,str)
	implicit	none
	real*8		ur8
	character*(*)	str
	integer*4	ur8_to_c24		! a function
	integer*4	ch_number_of_month	! a function
	character*24	c24
	character*32	c32
	character*2	c2
	integer*4	ok

	w_ur8_to_string_comparison = 0

	ok = ur8_to_c24(ur8, c24)
	if (ok .ne. 1) goto 10

	ok = ch_number_of_month(c2, c24(4:6))
	if (ok .ne. 1) goto 10

	c32 = c24(08:11)//'-'//c2//'-'//c24(1:2)//' '//c24(13:24)

	str = c32
	w_ur8_to_string_comparison = 1
	return
 10	continue
	return
	end

!------------------------------------------------------------------------------
	integer*4	function	w_ur8_from_string_comparison(ur8,str)
	implicit	none
	real*8		ur8
	character*(*)	str
	integer*4	c24_to_ur8		! a function
	integer*4	ch_mm_to_ch_mmm		! a function
	character*24	c24
	character*32	c32
	character*3	c3
	integer*4	ok

	w_ur8_from_string_comparison = 0

        ! str is in form yyyy-mm-dd hh:mm:ss.nnn
	! need form      dd-mmm-yyyy hh:mm:ss.ccc
	!                123456789012345678901234
	c32 = str
	ok = ch_mm_to_ch_mmm(c32(6:7), c3)
	if (ok .ne. 1) goto 10
	c24 = c32(09:10)//'-'//c3//'-'//c32(1:4)//c32(11:24)
	ok = c24_to_ur8(c24, ur8)
	if (ok .ne. 1) goto 10

	w_ur8_from_string_comparison = 1
	return
 10	continue
	return
	end