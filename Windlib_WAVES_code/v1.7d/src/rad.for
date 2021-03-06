! rad.for - this module contains the rad1 and rad2 physical units routines
! (which are identical at this time -- different entry points are provided
! for rad1 and rad2 callers -- 02/24/1995)

	integer*4  function w_physical_rad2_r4
	1			(channel,item,output,insize,outsize)

!	Purpose:  To return an array of rad values in physical units.
!	History:  Shell written by C. Meetre, HSTX, February 1995
!		  Computation routine written by Mike Reiner, HSTX, Feb 1995


!	PROGRAM FOR CONVERTING TELEMETRY VALUES TO MICROVOLTS
!	NOTE THAT THIS PROGRAM IS DESIGNED TO INTERFACE WITH MIKE REINER'S
!	CONVERSION PROGRAM, SO THE STRUCTURE AND ARGUMENTS DEPEND ON HIS
!	DECISIONS 

!	THE STRING PASSED IN AS ITEM CONTAINS AN 'ACTIVE' PART, WHICH IS
!	THE ROOT E.G. IN S_MV_R4, THE ACTIVE PART IS S BECAUSE THAT IS A
!	TELEMETRY ITEM IN THE DATABASE.  THE REMAINDER OF THE STRING IS TO
!	ALLOW THIS PROGRAM TO DECIDE WHICH UNITS TO USE.

	IMPLICIT NONE

	REAL*4		MVOLT		!FUNCTION TO CONVERT TO PHYSICAL UNITS
	real*4 		y		!A SINGLE TELEMETRY VALUE
$IF ABSOFT_FORTRAN
!	real*4		VOLT		!A SINGLE PHYSICAL VALUE
$ELSE
	real*4		VOLT		!A SINGLE PHYSICAL VALUE
$ENDIF
	real*4		output(*)	!ARRAY PASSED IN FOR OUTPUT
	
	integer*4	ICHANNEL	!A SINGLE CHANNEL NUMBER
	integer*4	channel		!CHANNEL INTO DATAFILE
	integer*4	insize		!SIZE OF INCOMING ARRAY
	integer*4	outsize		!SIZE OF OUTGOING ARRAY
	integer*4	s_size/1/	!SIZE OF ANTENNA RETURN VALUE
	integer*4	r_size		!SIZE OF ARRAY ACTUALLY RETURNED
	integer*4	size/1024/	!SIZE OF DATA RETURN ARRAY
	integer*4	chan_buffer(1024)!CHANNEL RETURN ARRAY
	integer*4	rad_buffer(1024)!DATA RETURN ARRAY
	integer*4	index		!LOOP COUNTER
	integer*4	ok		!STATUS RETURN CODE
$IF ABSOFT_FORTRAN
!	integer*4	icount		!COUNTER OF VALUES
	integer*4	antenna		!ANTENNA FLAG 
!	integer*4	last_letter	!LAST LETTER POSITION OF 'ITEM'
!	integer*4	end_letter	!LAST LETTER POSITION OF 'ACTIVE' ITEM
$ELSE
	integer*4	icount		!COUNTER OF VALUES
	integer*4	antenna		!ANTENNA FLAG 
	integer*4	last_letter	!LAST LETTER POSITION OF 'ITEM'
	integer*4	end_letter	!LAST LETTER POSITION OF 'ACTIVE' ITEM
$ENDIF
	
	character*(*)	item		!NAME OF ITEM REQUIRED
	character*80	item_name	!COPY OF ITEM 
	character*4 	RCV		!NAME OF ANTENNA
	character*4	in_units	!CHANNEL OR FREQUENCY UNITS
	character*4	out_units	!REQUIRED OUTPUT UNITS
	character*2	rcvmode		!S, S_PRIME OR Z
	character*2	ant		!ANTENNA
	character*20	item_clip	!TEMP STORAGE FOR 'ACTIVE' ITEM

	integer*4	w_physical_rad1_r4	! an entry point
	integer*4	w_item_i4	!-xragma C (w_item_i4)
	integer*4	w_item_char	!-xragma C (w_item_char)
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
$IF ABSOFT_FORTRAN
!	integer*4	ios
	integer*4	w_cnvrt_c_fn_by_os !$pragma C (w_cnvrt_c_fn_by_os)
	integer*4	k3len
	integer*4	i,j,k
!	integer*4	o
$ELSE
	integer*4	ios
	integer*4	w_cnvrt_c_fn_by_os !$pragma C (w_cnvrt_c_fn_by_os)
	integer*4	k3len
	integer*4	i,j,k,o
$ENDIF
	integer*4	ch
	character*20	rn 

	rcv = 'RAD2'
	rn = 'W_PHYSICAL_RAD2_R4: '
	goto 10

	!----------------------------------------------------------------------
	entry	w_physical_rad1_r4(channel,item,output,insize,outsize)
	rcv = 'RAD1'
	rn = 'W_PHYSICAL_RAD1_R4: '
	goto 10

!	POSSIBLE OUTPUT UNITS: "mV", "Watt", "logW","sfu","logS","dBV" '
!	mV SELECTED IN THIS CONTEXT

10	continue
	w_physical_rad2_r4 = 0
	ch = channel

	! get the item containing the name of the table file
	if (first_time) then
	   myitem = 'PHYSICAL_LOOKUP_TABLE'
	   ok = w_item_char(channel,myitem,cstr.plt_file,1,j)
	   if (ok .ne. 1) then
	      call w_msg_put(ch,rn, 'cannot get item '//myitem)
	      return
	   end if
	   i = k3len(cstr.plt_file)
	   call null_terminate(cstr.plt_file)
	   ok = w_cnvrt_c_fn_by_os(cstr.b_plt_file, cstr2.b_plt_file, 256)
	   ! rad2=rad1 for this filename
	   call rad2_set_coeff_filename(cstr2.plt_file)
	   first_time = .false.
	end if
  1	format(1x,'W_PHYSICAL_RAD_R4: ', a, a)

!	TURN ITEM NAME INTO UPPERCASE, AND REMOVE LEADING BLANKS
!	DO THIS TO A COPY OF THE NAME
	item_name = item
	call strupcase(item_name)

	! item names are of the form: S_MICROVOLTS_R4, Z_SFU_R4, etc.
	! ...parse input item name for base data name (S, Z, or SP)
	k = k3len(item_name)
	if (k .lt. 6) then
	   call w_msg_put(ch,rn,
	1     'invalid item name for physical units '//item_name(1:k) )
	   return
	end if
	i = 3
	if (item_name(1:2) .eq. 'S_') then
	   item_clip = 'S'
	else if (item_name(1:2) .eq. 'Z_') then
	   item_clip = 'Z'
	else if (item_name(1:3) .eq. 'SP_') then
	   item_clip = 'S_PRIME'
	   i = 4
	else
	   call w_msg_put(ch,rn,
	1     'invalid base data name for physical units '//item_name(1:k) )
	   return
	end if

	! parse input item name for units specification
	if (item_name(k-2:k) .ne. '_R4') then
	   call w_msg_put(ch,rn,
	1     'invalid data type for physical units '//item_name(1:k))
	   return
	end if
	j = k - 3
	if (item_name(i:j) .eq. 'MICROVOLTS') then
	   out_units = 'MV'	        
	else if (item_name(i:j) .eq. 'WATTS') then
	   out_units = 'WATT'
	else if (item_name(i:j) .eq. 'LOGWATTS') then
	   out_units = 'LOGW'
	else if (item_name(i:j) .eq. 'SFU') then
	   out_units = 'SFU'
	else if (item_name(i:j) .eq. 'LOGSFU') then
	   out_units = 'LOGS'
	else if (item_name(i:j) .eq. 'DBVOLTS') then
	   out_units = 'DBV'
	else
	   call w_msg_put(ch,rn,
	1     'invalid units for physical units '//item_name(1:k))
	   return
	endif

17	continue

!	NAME RECEIVER MODE, USING MJR'S CONVENTION
	if (item_name(1:1) .eq. 'Z') then
	    rcvmode = 'Z'
	else 
	    if (item_name(2:3) .eq. '_P') then
	        rcvmode = 'SP'
	    else
	        rcvmode = 'S'
	    endif
	endif

!	IDENTIFY ANTENNA.  THERE IS ONLY A CHOICE WITH RAD2 'S' TYPES
	ant = 'EZ'
	if (rcvmode .ne. 'Z') then
	    if (rcv .eq. 'RAD1') then
	        ant = 'EX'
!		ok = w_item_char(channel,'Antenna_flag',antenna,s_size,r_size)
$IF ABSOFT_FORTRAN
		ok = w_item_i4(channel,trim('Antenna_flag')//char(0),
     +                  antenna,1,r_size)
$ELSE
		ok = w_item_i4(channel,'Antenna_flag',antenna,1,r_size)
$ENDIF
	        if (ok .ne. 1) then
	           call w_msg_put(ch,rn, 'cannot get item ANTENNA_FLAG')
	        end if
	        if (antenna .eq. 1) ant = 'EY'
	    else
	        ant = 'EY'
	    endif
	endif

!	EXTRACT DATA AND CHANNEL INFORMATION
	
$IF ABSOFT_FORTRAN
	ok = w_item_i4(channel,trim(item_clip)//char(0),
     +        rad_buffer,size,r_size)
$ELSE
	ok = w_item_i4(channel,item_clip,rad_buffer,size,r_size)
$ENDIF
	if (ok .ne. 1) then
	   call w_msg_put(ch,rn, 'cannot get '//item_clip)
	end if

$IF ABSOFT_FORTRAN
	ok = w_item_i4(channel,trim('Frequencies')//char(0),
     +         chan_buffer,size,r_size)
$ELSE
	ok = w_item_i4(channel,'Frequencies',chan_buffer,size,r_size)
$ENDIF
	if (ok .ne. 1) then
	   call w_msg_put(ch,rn, 'cannot get FREQUENCIES')
	end if

! 	IF VALID, RECONCILE ARRAY LENGTHS TO FIT ARRAY PROVIDED
	if (ok .eq. 1) then
	    in_units = 'CHAN'

!	    if (insize .gt. r_size) insize = r_size ! cannot assign to insize
	    outsize = min(r_size,insize)

!	    CALL THE CONVERSION ROUTINE FOR EACH VALUE IN OUTPUT ARRAY
!	    MJR'S ROUTINE REQUIRES Y BE REAL.
	    do index = 1,outsize
	        y = rad_buffer(index)
	        ichannel = chan_buffer(index)
	        output(index) = mvolt(y,rcv,rcvmode,ant,in_units,ichannel,
	1                 out_units)
	    enddo

!	    FLAG SUCCESSFUL COMPLETION
	    w_physical_rad2_r4 = 1
	else

!	    GIVE ERROR MESSAGE AND SET ERROR FLAGS
	    call w_msg_put(ch,rn, 'No values available for '//item)
	    w_physical_rad2_r4 = 0
	endif

	return
	end


c     ***************************************************************
      Real Function mVolt(Tlm,rcv,rcvmode,ant,In_units,jchannel,
     .                    Out_units)
c     ***************************************************************
c
c     Program to convert receiver output telemetry values to voltage at 
c     input of the preamps.

c     Input: Tlm (receiver output telemetry value) -- need not be an integer

c     Input/Output Parameters: 
c                 rcv : receiver -- 'rad1' or 'rad2'
c                 rcvmode: receiver mode -- 's', 'sp', or 'z'
c                 ant: antenna -- 'ex', 'ey', or 'ez'
c                 In_units: 'chan' or 'freq'
c                 jchannel: frequency channel -- 0 - 255
c                           or frequency: 20 - 1040kHz for RAD1
c                                         1075 - 13825 for RAD2
c                 Out_units: 'MV' = microvolts/sqrt(Hz) at preamp input
c                            'WATT' = Watts/m^2Hz                   
c                            'LOGW' = log(Watts/m^2Hz)
c                            'SFU'  = solar flux units(sfu)
c                            'LOGS' = log(sfu)
c                            'DBV'  = dB volts
c
      Parameter ( Mrad=2, Msz=3, Mfreq=256, Mfrec=16, Munit=3 )
      Real Tlm
      Real*8 A1s1x(Mfreq),A2s1x,A3s1x,V0s1x(Mfreq)
      Real*8 A1sp1x(Mfreq),A2sp1x,A3sp1x
      Real*8 A1s1y(Mfreq),A2s1y,A3s1y,V0s1y(Mfreq)
      Real*8 A1sp1y(Mfreq),A2sp1y,A3sp1y
      Real*8 A1z1(Mfreq),A2z1,A3z1,V0z1(Mfreq)
      Real*8 A1s2(Mfreq),A2s2,A3s2,V0s2(Mfreq)
      Real*8 A1sp2(Mfreq),A2sp2,A3sp2
      Real*8 A1z2(Mfreq),A2z2,A3z2,V0z2(Mfreq)
$IF ABSOFT_FORTRAN
!      Integer Ircv,Imode,Ichannel,Jchannel
      Integer Ichannel,Jchannel
$ELSE
      Integer Ircv,Imode,Ichannel,Jchannel
$ENDIF
      character*(*) rcv,in_units,out_units
      character*(*) rcvmode,ant
       real*4 Lx,Ly,Lz,Cax,Cay,Caz,Cbx,Cby,Cbz,Z0
	integer*4	lun		! logical unit number
	character*256	name		! name of file containing lookup table
	integer*4	ios
	integer*4	k3len
$IF ABSOFT_FORTRAN
!	integer*4	i,j,o
	integer*4	i
$ELSE
	integer*4	i,j,o
$ENDIF
	character*(*)	file
	integer*4	rad1_set_coeff_filename	! an entry point
	integer*4	rad2_set_coeff_filename	! an entry point
$IF ABSOFT_FORTRAN
	character*(*)	rn
	parameter	(rn='RAD Physical:')
$ELSE
	parameter	rn='RAD Physical:'
$ENDIF

	logical*1	first/.true./
	integer*4	big_count /0/
	
	mvolt = 0.0

	if (first) then
	   i = k3len(name)
	   if (i .le. 1) then
	      call w_msg_put_always(rn,
	1         'call rad[1,2]_set_coeff_filename first')
	      stop
	   end if

	   call lib$get_lun(lun)
$IF ABSOFT_FORTRAN
!
! (2007/07/16):  Absoft Fortran requires "status='old'":
!
	   open(unit=lun,name=name(1:i),
	1        form='formatted',
	1        status='old',
	1	 iostat=ios,
	1        readonly)
$ELSE
	   open(unit=lun,name=name(1:i),
	1        form='formatted',
	1        type='old',
	1	 iostat=ios,
	1        readonly)
$ENDIF
	   if (ios .ne. 0) then
	      call w_msg_put_always2(rn,'cannot open '//name(1:i), ios)
	      call lib$free_lun(lun)
	      lun = 0
	      stop
	   end if

c        ------------------------------------------------------------
c        Tables of the reference voltages measured for RAD1 and RAD2
c        and fitted to the following function:
c            dBV0 = a(0) + a(1)*f + a(2)*f^2 + a(3)*f^3 (Manning)
c        where the coefficients are,
c        RAD1 X:
c             a=[-64.10, -8.798e-3,  5.284e-5,-1.594e-7]
c        RAD1 Y:
c             a=[-63.93,  4.53e-3 , -5.802e-5, 1.106e-7]
c        RAD1 Z:
c             a=[-64.30,  2.624e-3, -3.470e-5, 5.662e-8]
c        RAD2 Y:
c             a=[-73.87, -9.559e-3, -6.615e-5, -3.623e-8]
c        RAD2 Z:
c             a=[-74.27, -8.958e-3, -5.786e-5, -1.126e-8]
c        ---------------------------------------------------------

	  read(lun,*)lx,ly,lz,cax,cay,caz,cbx,cby,cbz,z0
	  read(lun,*)a1s1x
	  read(lun,*)a1sp1x
	  read(lun,*)a1s1y
	  read(lun,*)a1sp1y
	  read(lun,*)a1z1
	  read(lun,*)a2s1x,a2sp1x,a2s1y,a2sp1y,a2z1,a3s1x,
	1              a3sp1x,a3s1y,a3sp1y,a3z1
	  read(lun,*)a1s2
	  read(lun,*)a1sp2
	  read(lun,*)a1z2
	  read(lun,*)a2s2,a2sp2,a2z2,a3s2,a3sp2,a3z2
	  read(lun,*)v0s1x
	  read(lun,*)v0s1y
	  read(lun,*)v0z1
	  read(lun,*)v0s2
	  read(lun,*)v0z2

	   close(lun)
	   call lib$free_lun(lun)

	  first = .false.
	endif

c          -------------------------------
c          check limits on telemetry value
c          -------------------------------
        if (tlm.lt.0.0.or.tlm.gt.255.) then
          call w_msg_put(0,rn,'telemetry units out of range')
          return
        endif
        if (tlm.gt.240.) then
	  big_count = big_count + 1
c	  if (big_count .le. 1) then
c	    call w_msg_put(0,rn,'BIG TM value - calibration may be invalid')
c	    call w_msg_put(0,rn,'             - message not repeated')
c	  endif
$IF ABSOFT_FORTRAN
          call w_msg_put(0,rn,
	1    'BIG TM value - calibration may be invalid')
$ELSE
          call w_msg_put(0,rn,'BIG TM value - calibration may be invalid')
$ENDIF
        endif

c               ----------------------------------------------
c               convert telemetry value to microvolts/sqrt(Hz)
c               ----------------------------------------------
        if (rcv .eq. 'RAD1') then
c               check input units
           if (in_units.eq.'CHAN') then
                if (jchannel.lt.0.or.jchannel.gt.255) then
                call w_msg_put(0,rn,'invalid frequency channel')
                return
                endif
                Ichannel = jchannel + 1
           else if (in_units.eq.'FREQ') then
c               check valid frequency for RAD1
                if (jchannel.lt.20.or.jchannel.gt.1040) then
                   call w_msg_put(0,rn,'invalid frequency for RAD1')
                   return
                endif
                if (mod(jchannel-20,4).ne.0) then
                   call w_msg_put(0,rn,'invalid frequency for RAD1')
                   return
                endif
                ichannel=(jchannel-20)/4 + 1
           else
                call w_msg_put(0,rn, 'incorrect input units')
           endif
c                   calculate microvolts/sqrt(Hz) from log law function
c                   and reference voltage for different mode and antenna
c                   configurations
              if (rcvmode .eq. 'S') then
                 if (ant .eq. 'EX') then
                    mVolt=10.**(2.*DLOG10(10.**((Tlm-A3s1x)/A2s1x)
     .                                  +1.)-A1s1x(Ichannel)/20.)
c                   take into account the 26 dB attenuator used in the
c                   calibration of RAD1
                    mVolt = mVolt/10**(2.6/2)
                    mVolt = mVolt*V0s1x(Ichannel)
                  	Ceff = (Cax * Lx/(Cax + Cbx))**2 * Z0
                  else if (ant .eq. 'EY') then
                    mVolt=10.**(2.*DLOG10(10.**((Tlm-A3s1y)/A2s1y)
     .                                  +1.)-A1s1y(Ichannel)/20.)
c                   take into account the 26 dB attenuator used in the
c                   calibration of RAD1
                    mVolt = mVolt/10**(2.6/2)
                    mVolt = mVolt*V0s1y(Ichannel)
        		Ceff = (Cay * Ly/(Cay + Cby))**2 * Z0
                 endif
              else if (rcvmode .eq. 'SP') then
                 if (ant .eq. 'EX') then
                    mVolt=10.**(2.*DLOG10(10.**((Tlm-A3sp1x)/A2sp1x)
     .                                  +1.)-A1sp1x(Ichannel)/20.)
c                   take into account the 26 dB attenuator used in the
c                   calibration of RAD1
                    mVolt = mVolt/10**(2.6/2)
                    mVolt = mVolt*V0s1x(Ichannel)
                    Ceff = (Cax * Lx/(Cax + Cbx))**2 * Z0
                 else if (ant .eq. 'EY') then
                    mVolt=10.**(2.*DLOG10(10.**((Tlm-A3sp1y)/A2sp1y)
     .                                  +1.)-A1sp1y(Ichannel)/20.)
c                   take into account the 26 dB attenuator used in the
c                   calibration of RAD1
                    mVolt = mVolt/10**(2.6/2)
                    mVolt = mVolt*V0s1y(Ichannel)
                    Ceff = (Cay * Ly/(Cay + Cby))**2 * Z0
                 endif
              else if (rcvmode .eq. 'Z' .and. ant .eq. 'EZ') then
                 mVolt=10.**(2.*DLOG10(10.**((Tlm-A3z1)/A2z1)
     .                  +1.)-A1z1(Ichannel)/20.)
c                   take into account the 26 dB attenuator used in the
c                   calibration of RAD1
                    mVolt = mVolt/10**(2.6/2)
                 mVolt = mVolt*V0z1(Ichannel)
                 Ceff = (Caz * Lz/(Caz + Cbz))**2 * Z0
              else
                 call w_msg_put(0,rn, 'invalid mode for RAD1')
              endif       

        else if (rcv .eq. 'RAD2') then
c               check input units
           if (in_units.eq.'CHAN') then
                if (jchannel.lt.0.or.jchannel.gt.255) then
                call w_msg_put(0,rn,'invalid frequency channel')
                return
                endif
                Ichannel = jchannel + 1
           else if (in_units.eq.'FREQ') then
c               check valid frequency for RAD2
                if (jchannel.lt.1075.or.jchannel.gt.13825) then
                   call w_msg_put(0,rn,'invalid frequency for RAD2')
                   return
                endif
                if (mod(jchannel-1075,50).ne.0) then
                   call w_msg_put(0,rn,'invalid frequency for RAD2')
                   return
                endif
                ichannel=(jchannel-1075)/50 + 1
           else
                call w_msg_put(0,rn, 'incorrect input units')
           endif
              if (rcvmode .eq. 'S' .and. ant .eq. 'EY') then
                  mVolt=10.**(2.*DLOG10(10.**((Tlm-A3s2)/A2s2)
     .                                +1.)-A1s2(Ichannel)/20.)
                  mVolt = mVolt*V0s2(Ichannel)
                  Ceff = (Cay * Ly/(Cay + Cby))**2 * Z0
              else if (rcvmode .eq. 'SP' .and. ant .eq. 'EY') then

                 mVolt=10.**(2.*DLOG10(10.**((Tlm-A3sp2)/A2sp2)
     .                               +1.)-A1sp2(Ichannel)/20.)
                 mVolt = mVolt*V0s2(Ichannel)
                 Ceff = (Cay * Ly/(Cay + Cby))**2 * Z0
              else if (rcvmode .eq. 'Z' .and. ant .eq. 'EZ') then
                 mVolt=10.**(2.*DLOG10(10.**((Tlm-A3z2)/A2z2)
     .                               +1.)-A1z2(Ichannel)/20.)
                 mVolt = mVolt*V0z2(Ichannel)
                 Ceff = (Caz * Lz/(Caz + Cbz))**2 * Z0
              else
                 call w_msg_put(0,rn, 'invalid mode for RAD2')
              endif       
        else
              call w_msg_put(0,rn, 'no such receiver')
              return
        endif

c               -----------------------------------------------------------
c               convert microvolts/sqrt(Hz) to various other physical units
c               -----------------------------------------------------------
        if (out_units.eq.'MV') then
		mVolt = mVolt
        else if (out_units.eq.'WATT') then
		mVolt = 3. * mVolt**2/Ceff * 1.e-12
        else if (out_units.eq.'LOGW') then
                mVolt = Alog10(3. * mVolt**2/Ceff * 1.e-12)
        else if (out_units.eq.'SFU') then
                mVolt = 3. * mVolt**2/(Ceff*1.e-22) * 1.e-12
        else if (out_units.eq.'LOGS') then
                mVolt = alog10(3. * mVolt**2/(Ceff*1.e-22) *1.e-12)
        else if (out_units.eq.'DBV') then
                mVolt = 20*Alog10(mVolt/1.e6)
        else
                call w_msg_put(0,rn,'invalid output units')
        endif
        return

	!----------------------------------------------------------------------
	entry	rad1_set_coeff_filename(file)
	rad1_set_coeff_filename = 0.0
	name = file
	return

	entry	rad2_set_coeff_filename(file)
	rad2_set_coeff_filename = 0.0
	name = file
	return

         END
