c
c    Subroutines for the program udf_lister
c
c     Contents:
c
c             logical function get_data_record
c
c             integer function check_time
c
c             subroutine  reset_time
c
c             subroutine get_spin_period
c
c
c
c
c*****************************************************
c
      logical function get_data_record(unit_in)
c
c*****************************************************
c
c     Reads 1 mf data packet from unformatted sdf file
c     return .true. if EOF reached, .false. otherwise
c
c     6/16/98 by J. Dwyer
c     4/8/99, collection time is 1 mf earlier, subtracted offset from start and end times
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      integer*4   unit_in        ! file unit for input sdf file
      integer*4   id    ! data type id
      integer*4   i,j
      integer*4   dmf_time
      character*120 test_line
c
      get_data_record = .false.  ! not at EOF
      spha_flag_1mf = .false.   ! spha kp not read
      mfi_flag_1mf = .false.   ! mfi kp not read
      swe_flag_1mf = .false.   ! swe kp not read
      tdp_flag_1mf  = .false.   ! 3dp kp not read
      epa_flag_1mf = .false.   ! epact kp not read


c
8888	continue
      read(unit_in,4000,err=700,end=10000) id	
4000    format(50z8)
      do while (id.ne.-1)
  
         if (id.eq.1) then   ! STEP data
c	read(unit_in,3610) test_line
c3610	format(a)
c3600	type *, j, test_line
	   
             read(unit_in,4010,err=600,end=10000) start_time_string,
     1      start_time, end_time
4010    format(a24,2z8)


c
            dmf_time = end_time-start_time
            start_time = start_time - dmf_time  ! added 4/8/99, collection time is 1 mf earlier
            end_time = end_time - dmf_time
c
c           type *, start_time_string,
c     1     start_time, end_time

             read(unit_in,4020,err=600,end=10000)
     1      STEPHV, STEPtherm, STEPt1, STEPt2, STEPt3, MFnum  ! housekeeping
4020    format(5(1pe12.4),z8)
c           type *, STEPHV, STEPtherm, STEPt1, STEPt2, STEPt3, MFnum



             read(unit_in,4030,err=500,end=10000)
     1        (packet(j), j=1,482), packet(733)
4030    format(483z2)

            read(unit_in,4040,err=500,end=10000) NPHA
4040	format(z2)
            if (NPHA.gt.0) then
               read(unit_in,4030,err=500,end=10000)
     1            ((packet(483+(i-1)*5),
     1            packet(483+(i-1)*5+1),
     1            packet(483+(i-1)*5+2),
     1            packet(483+(i-1)*5+3),
     1            packet(483+(i-1)*5+4)), i=1,NPHA)
            end if
            
   
            read(unit_in,4050,err=500,end=10000) (STEP_KP_fluxes(j), j=1,6)
4050    format(20(1pe12.4))

            goto 400   ! go to next record
         end if  ! end STEP data type
c
         if (id.eq.2) then  ! spha kp data
            spha_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000)
     1        spha_asr
c           type *,  spha_asr
            goto 400   ! go to next record
         end if
c
         if (id.eq.3) then  ! mfi kp data
            mfi_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000)
     1        mfi_bgse, mfi_rms, mfi_position
c            type *, mfi_bgse,mfi_rms,mfi_position
            goto 400   ! go to next record
         end if
c
         if (id.eq.4) then  ! swe kp data
            swe_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000)
     1       swe_vgse,swe_vth,swe_protdens
c            type *, swe_vgse,swe_vth,swe_protdens
            goto 400   ! go to next record
         end if
c
         if (id.eq.5) then  ! 3dp kp data
            tdp_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000)
     1       tdp_eflux, tdp_ionflux
            goto 400   ! go to next record
         end if
c
         if (id.eq.6) then  ! epact kp data
            epa_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000)
     1         epa_apeb2, epa_apeb3,
     1         epa_apeb4, epa_apeb5,
     1         epa_lemt1, epa_lemt2,
     1         epa_lemt3
c           type *, epa_apeb1,
c     1         epa_apeb2, epa_apeb3,
c     1         epa_apeb4, epa_apeb5,
c     1         epa_lemt1, epa_lemt2,
c     1         epa_lemt3
            goto 400   ! go to next record
         end if
c
         type *, 'Unknown data type: ', id
         read(unit_in,err=800,end=10000)     ! skip data for unknown id
c 

400		read(unit_in,4000,err=700,end=10000) id   ! read next record

      end do
c
      return
c
500    type *, 'start_time_string: ',
     1     start_time_string   ! if error come here
      type *,'Error reading packet data!'
      type *,'Stopped in function get_data_packet'
      stop
600    type *, 'start_time_string: ',
     1     start_time_string   ! if error come here
      type *,'Error reading HK line!'
      type *,'Stopped in function get_data_packet'
      stop
700    type *, 'start_time_string: ',
     1     start_time_string   ! if error come here
      type *, ' id = ', id
      type *,'Error reading packet time!'
      type *,'Stopped in function get_data_packet'
      stop
c
800    type *, 'start_time_string: ',
     1     start_time_string   ! if error come here
      type *,'Error reading kp data!'
      type *,'Stopped in function get_data_packet'
      stop
c
10000  get_data_record = .true.   ! reached end of file
      return
      end   ! end get_data_record
c
c
c
c
c************************************************
c
        function check_time()
c
c************************************************
c
c       Compares start time and finish time of the major frame in data file with
c       the requested start and stop time.
c
c       function returns
c                 0 = continue to skip forward
c                 1 = continue and analyze
c                 2 = stop reading and exit
c
c       10/15/95 by J. Dwyer, based on code in pha_lister by G. Mason
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      integer     major_frame_length     ! duration of major frame in seconds
      integer check_time
      check_time = 1
c
      if(start_time.lt.stime_start) check_time = 0
      if(start_time.ge.stime_stop)  check_time = 2
c
	 major_frame_length = end_time-start_time
	 mf_type = 1
	 if (major_frame_length.eq.92) mf_type = 2
	   if ((major_frame_length.ne.46).and.
     1     (major_frame_length.ne.92) ) then
	     type *,' Illegal major frame length at: ',
     1        start_time_string
          type *,  'major_frame_length: ',
     1      major_frame_length
	       type *,'  Packet skipped '
          check_time = 0
      end if
      return
      end  ! end check_time
c
c
c
c
c************************************************
c
        subroutine  reset_time(script_number)
c
c************************************************
c
c     clears all accumulated variables
c
c     6/16/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c  start accumulating counts at beginning of time interval
c
      integer*4   script_number
      integer*4   i,j,k,l
c
c   flags that measurement made
      reset_interval(script_number) = 0
      Nhk(script_number) = 0
      Nr_counts(script_number)  = 0
      time_interval(script_number) = 0.0
      time_interval_2x(script_number,1) = 0.0
      time_interval_2x(script_number,2) = 0.0
      time_interval_4x(script_number,1) = 0.0
      time_interval_4x(script_number,2) = 0.0
      time_interval_4x(script_number,3) = 0.0
      time_interval_4x(script_number,4) = 0.0
      start_time_avg(script_number) = start_time
c
      do i = 1, 6
        STEP_KP_fluxes_accum(script_number,i) = 0.0
        hk_accum(script_number,i) = 0.0
      end do
      Nspha(script_number) = 0.0
      Nmfi(script_number,1) = 0.0
      Nmfi(script_number,2) = 0.0
      Nswe(script_number) = 0.0
      N3dp(script_number,1) = 0.0
      N3dp(script_number,2) = 0.0
      Nepa(script_number,1) = 0.0
      Nepa(script_number,2) = 0.0
      spin_rate_accum(script_number) =  0.0
      Bx_accum(script_number) = 0.0
      By_accum(script_number) = 0.0
      Bz_accum(script_number) = 0.0
      Brms_accum(script_number) = 0.0
      Bfield_accum(script_number) = 0.0
      SCx_accum(script_number) = 0.0
      SCy_accum(script_number) = 0.0
      SCz_accum(script_number) = 0.0
      SWVx_accum(script_number) = 0.0
      SWVy_accum(script_number) = 0.0
      SWVz_accum(script_number) = 0.0
      Nproton_accum(script_number) = 0.0
      SWVth_accum(script_number) = 0.0
      do i=1,7
          eflux_accum(script_number,i)=0.0
          ionflux_accum(script_number,i)=0.0
      end do
c
      J_apeb1_accum(script_number) = 0.0
      J_apeb2_accum(script_number) = 0.0
      J_apeb3_accum(script_number) = 0.0
      J_apeb4_accum(script_number) = 0.0
      J_apeb5_accum(script_number) = 0.0
      J_lemt1_accum(script_number) = 0.0
      J_lemt2_accum(script_number) = 0.0
      J_lemt3_accum(script_number) = 0.0
      spha_flag(script_number) = .false.
      mfi_flag(script_number,1) = .false.
      mfi_flag(script_number,2) = .false.
      swe_flag(script_number) = .false.
      tdp_flag(script_number,1) = .false.
      tdp_flag(script_number,2) = .false.
      epa_flag(script_number,1) = .false.
      epa_flag(script_number,2) = .false.
c
      saturation(script_number) = 0
      vse1A(script_number) = 0.0
      vse1B(script_number) = 0.0
      vse2A(script_number) = 0.0
      vse2B(script_number) = 0.0
      vse_master_flag(script_number) = 0
      vsebar(script_number) = 0
      vsebar_flag(script_number) = 0
      start(script_number) = 0
      start_flag(script_number) = 0
      stop(script_number) = 0
      stop_flag(script_number) = 0
      d1(script_number) = 0
      d1_flag(script_number) = 0
      d2(script_number) = 0
      d2_flag(script_number) = 0
      do j=1,2
        do  k=1,8
          do i=1,42
            mdata(script_number,i,j,k) = 0.0
            mdata_flag(script_number,i,j,k) = 0
          end do
        end do
      end do
      do i=1,42
        do j=1,2
          exposure(script_number,i,j) = 0.0
        end do
      end do
c
      if ((dohistflag.eq.1).and.
     1 (script_output_type(script_number).eq.3))
     1  call reset_histogram(script_number)
      if ((dophafluxflag.eq.1).and.
     1 (script_output_type(script_number).eq.4))
     1   call reset_phaflux(script_number)
c
c
      return
      end    ! end reset_time
c
c
c
c
c************************************************
c
        subroutine get_spin_period
c
c************************************************
c
c    assigns the correct
c    spin period using the kp data, if available
c
c    11/2/95 by J. Dwyer
c          modified  2/13/97 by J. Dwyer changed KP data
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      if ((spha_flag_1mf).and.
     1   (spha_asr.ge.0.0).and.
     1   (spha_asr.le.6.28319)) then
         spin_period = 2.0*3.141592654/spha_asr   ! new spin_period
      end if
c
      return
      end   ! end  get_spin_period
c

