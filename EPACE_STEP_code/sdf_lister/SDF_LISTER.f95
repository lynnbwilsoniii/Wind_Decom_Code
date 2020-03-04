c
c    subroutines and functions called by Sdf_lister:
c
c    Calib_lib.for:
c          subroutine read_pha_cal
c          subroutine get_geofactor
c          subroutine get_track_effic
c          subroutine get_ROMboxes
c
c    Decode_lib.for
c          subroutine expand_data_packet
c          integer function decode
c          integer function reames
c
c    Sdf_Hist_Subrout.for:
c          subroutine initial_histogram
c          subroutine reset_histogram
c          logical function fill_histogram
c          logical function write_hist_output
c
c    Sdf_mask_subrout.for:
c          logical function get_expose
c          subroutine check_expose
c          logical function get_mask
c          subroutine check_mask
c          subroutine check_skipday
c
c    Sdf_phaflux_subrout.for:
c          subroutine  reset_phaflux
c          subroutine calc_effic
c          logical function fill_phaflux
c          subroutine calculate_phaflux
c          subroutine calculate_phacounts
c          real function get_sigma_flux
c          real function get_sigma_flux2
c          logical function write_phaflux_output
c
c    Sdf_pha_subrout.for:
c          subroutine evaluate_pha
c          subroutine convert_data
c          real function lininterp
c          subroutine search
c          real function Einc_MeVnuc
c          real function mass_amu
c          function tof_fraction
c          real function spread
c
c    Sdf_subrout1.for:
c          subroutine  initial_global
c          logical function get_config
c          logical function read_script
c          subroutine script_key
c
c    Sdf_subrout2.for:
c          logical function get_data_record
c          integer function check_time
c          subroutine  reset_time
c          subroutine get_spin_period
c
c    Sdf_subrout3.for:
c          logical function cut_interval_test
c          subroutine  cut_PHA_data
c          subroutine  assign_script_flags
c          subroutine  write_titles
c          subroutine strip_blanks20
c          subroutine strip_blanks41
c
c    Sdf_subrout4.for:
c          subroutine update_ROMbox
c          logical function get_ROMbox_history
c          subroutine get_mdata
c
c    Sdf_subrout5.for:
c          subroutine get_exposure
c          subroutine sum_kp
c          subroutine ave_kp
c          subroutine ave_kp_1mf
c          subroutine calculate_rates
c          subroutine calculate_counts
c          subroutine calculate_rates_1mf
c          subroutine calculate_counts_1mf
c
c    Sdf_subrout6.for:
c          logical function open_output
c          subroutine write_header
c          function write_output
c          function write_output_omni
c          subroutine write_line
c
c    Time_Lib.For:
c           subroutine convert_time
c           integer function day_of_year
c           integer function IDAYS_PER_YEAR
c           subroutine convert_to_sampextime
c           subroutine sampex_timcon
c
c    Open_Sdf.for:
c          subroutine open_sdf
c
c
c
c***************************************************

      program Sdf_lister

c***************************************************
c
c      Sdf_lister is a data analysis program for the WIND/STEP instrument.
c      It reads in unformatted SDF files,
c      decodes and processes the data,
c      and writes the output to a kalidagraph and IDL format ASCII file.
c      Include files:
c            Sdf_include.inc
c            Hex_include.inc  ! Also used by L0_to_sdfkp
c
c      version 3.99
c      Written by J. Dwyer, 7/2/98
c          9/16/98 added if (indexi.gt.1) to write in logical function write_output
c          9/21/98 fixed output problem in logical function write_output
c          10/5/98 fixed bug in disc output
c          10/8/98 changed to date_and_time in write_header
c          10/27/98 corrected factor of 2 error in Tel1&2 VSE rate in calculate_rates
c          11/4/98 fixed bug in junk output
c          11/6/98 added subroutine check_skipday, and skip day feature
c          11/6/98 added saturation = 0 at beginning and sat_1mf in get_mdata
c          11/6/98 corrected factor of 2 error in Tel1&2 VSE rate in  calculate_rates_1mf
c          11/10/98 removed data_flag, bas event mass= -99.9
c          11/11/98 fixed bug in fill_histogram
c          12/22/98 changed state from flux to rate
c          1/21/99 added script_stateab_flag cut to cut_PHA_data
c          4/8/99 collection time is 1 mf earlier, subtracted offset from start and end times, change in get_data_record
c          6/21/99 fixed datew output in header
c          6/22/99 changed dt/2 to exponential format
c          8/9/99 added index number to sigma so each title is unique
c          1/13/00 fixed bugs in check_expose and sampex_timcon, problem with call get_geofactor, changed spread to spread1
c          4/21/00 by J. Dwyer added script_stateab_flag = -1 to hist input in read_script
c          1/9/01  allowed position to be gt 220 RE and lt -220 RE in sum_kp
c		   1/24/01 by J. Dwyer increased max mask and expose lines to 9999
c          2/5/01  no longer output at least one PHA record per major frame filled with -1s
c		21-Apr-2008    install on Mac with Absoft fortran compiler /gm
c
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c                                        local variable type declarations
c                                     *****************************
      logical	get_config              ! function declaration
      logical	get_ROMbox_history       ! function declaration
      logical	read_script           ! function declaration
      logical	get_data_record   ! function declaration
      logical	fill_histogram     ! function declaration
      integer	idays_per_year       ! function declaration
      logical	open_output     ! function declaration
      logical	write_output    ! function declaration
      logical	write_output_omni    ! function declaration
      logical	fill_phaflux   ! function declaration
      logical	write_hist_output    ! function declaration
      logical	write_phaflux_output    ! function declaration
      logical	get_mask              ! function declaration
      logical	get_expose              ! function declaration
      logical	cut_interval_test      ! function declaration
      integer	check_time             ! function declaration
c
      integer*4      time_flag  ! 0 = continue to skip forward, 1 = continue and analyze, 2 = stop reading and exit
      integer*4      script_number    ! number of script that is being processed
      integer*4      spin_number     ! spin pair number in science record [1,5]
      integer*4      reset_interval_all ! flag to see if all scripts are ready to output
      integer*4      skipday  ! determined by subroutine check_skipday (1 = skip day)
      integer*4      unit_in,
     1     unit_cfg,
     1     unit_script,
     1     unit_out,
     1     unit_ROM,
     1     unit_cal   ! file units
      logical      output_flag   ! .false. = all output files have exceeded maximum number of rows, .true. otherwise
      logical      output_script_flag(Nscripts_max)   ! .false. = individual output file exceeded maximum number of  rows, .true. otherwise
      logical      cut_interval_flag  ! result of cuts (true =  write time interval, false = don't)
      logical      thishistfull(Nscripts_max)    !  .true. = max number of counts reached in a hist
      logical      thisphafluxfull(Nscripts_max)     !  .true. = max number of counts reached in a pha flux array
      logical      end_flag  ! .true. = end of file reached, .false. otherwise
      logical      iflag    !  .true. = open_udf was successfully opened input data file, .false. = otherwise
      logical      ifile                ! test for config. file inquire
      logical      open_flag  ! .true. if file opened successfully, .false. otherwise
      logical      first_record(Nscripts_max)   ! .true. first good science record; .false. otherwise



c ********************** read in the data paths



	include 'include_paths.inc'
	open(unit=10,name='/paths/sdf_lister_paths.txt',status='old',action='read')
	read(10,*)
	read(10,500)	calibration_path			! folder containing input udf files
500	format(a)
	read(10,*)
	read(10,500)	data_path		! folder containing instrument calibration data
	read(10,*)	
	read(10,500) control_path
	close(10)
	type *, adjustl(trim(data_path))	
	type * ,	adjustl(trim(calibration_path))	
	type *, adjustl(trim(control_path))
c***********************




c
c                                        set local variables
c                                   ********************
      unit_in = 1        ! file unit of input STEP data file (SDF)
      unit_cfg = 2
      unit_cal = 3
      unit_script = 4
      unit_ROM = 5
c
      skipday = 0
c                                set initial values of variables
c                                   ********************
      call initial_global  !  set initial values for global variables
c
      output_flag = .true.    ! still want to output data, keep going
      reset_interval_all = 0
      do script_number = 1, Nscripts_max
        first_record(script_number) = .true.       ! first record in a time interval
        output_script_flag(script_number) = .true.   ! write to individual script file
        thishistfull(script_number) =.false.     ! a histogram has enough data
        thisphafluxfull(script_number) =.false.  ! a pha flux array has enough data
      end do
c
c
c                                  get configuration file
c                                   ********************
      configfile = adjustl(trim(control_path)) // 'Sdf_lister.cfg'   ! default value
c
      inquire(file=configfile,exist=ifile )  ! does file exist?
      if (.not.ifile ) then                 ! if it exists, open it and read data
        type *, 'Configuration file ', configfile ,
     1               ' does not exist! '
      end if
      open_flag = get_config(unit_cfg)  ! open config. file and read data
      if (.not.open_flag) then
        type *, 'stopped at config file open'
        stop  ! error opening config file
      end if
c
c                                  get calibration data
c                                   ********************
       call read_pha_cal(unit_cal)
       call get_geofactor(unit_cal)
       call get_track_effic(unit_cal)
c
c                                  get ROM box history data
c                                   ********************
      open_flag =
     1    get_ROMbox_history(unit_cal)  ! open file
      if (.not.open_flag) then
        type *, 'stopped at ROM box history file open'
        stop  ! error opening ROM box history file
      end if
c
c                                  read mask and expose data
c                                   ***********************
      if (expose_flag.eq.1) then
        idelta_t = 0.0    ! don't average over time interval in this version
        open_flag = get_expose(unit_cal)  ! open file and read data
        if (.not.open_flag) then
          type *, 'stopped at expose file open'
          stop  ! error opening expose file
        end if
      end if
c
      if (mask_flag.eq.1) then
        open_flag = get_mask(unit_cal)  ! open file and read data
        if (.not.open_flag) then
          type *, 'stopped at mask file open'
          stop  ! error opening mask file
        end if
      end if
c
c
c                                    open and read script files
c                                        *****************
      if (Nscripts.gt.0) then
        do  i=1, Nscripts
          open_flag = read_script(unit_script, i)
          if (.not.open_flag) then
            type *, 'stopped at script open'
            stop  ! error opening data output file
          end if
        end do
      else
        type *,'No output files requested.'
        type *,'Stopping run.'
        stop
      end  if
c
c
      call script_key   ! get titles for scripts
      start_time = stime_start   ! use this time for first ROM box file call
      call update_ROMbox(unit_ROM)   ! get latest ROM box data file
      call assign_script_flags   !  set appropriate output flags
      if (dohistflag.eq.1) call initial_histogram   !  initialize histograms
      do  i=1, Nscripts
        call  reset_time(i)   !  reset accumulated variables
      end do
c
c                                    open output files
c                                    *****************
      do  i=1, Nscripts
        unit_out = i+10
        open_flag = open_output(unit_out,i)  ! open data output file
        if (.not.open_flag) then
          stop  ! error opening data output file
        end if
      end do
c
c
c
c                               Main loop through multiple UDFs
c                              ********************************
c	  Loop through all days between the start and stop days.
      iyear = istart_year
      iday = istart_day
      zero=0
      secinit=secnds(zero)  ! used to keep track of time since last open_sdf call
c
      do while ((((iday.le.istop_day).and.(iyear.eq.istop_year) ).or.
     1            (iyear.lt.istop_year) ).and.(output_flag) )  ! continue if start date is LE than stop date
c
        if (mask_flag.eq.1)
     1     call check_skipday(iday,iyear,skipday)
c
        iflag = .false.
        if (skipday.eq.1) type *,'skip file',iyear,iday
        if (skipday.eq.0)
     1     call open_Sdf(iyear,iday,unit_in,file_extent_prefix,iflag)  ! open input data file
c
        if (iflag) then  ! check if file exists
c
c	                            Main loop through data packets in UDF
c                                ********************************
c
          end_flag = .false.
          end_flag =  get_data_record(unit_in)   ! read in data
          call expand_data_packet   ! decode data in packet
          call evaluate_pha
          call update_ROMbox(unit_ROM)    ! get latest ROM box data file
          time_flag =  check_time()   ! check if packet is within specified start and stop times
c
          do while ((.not.end_flag).and.(time_flag.ne.2)
     1                    .and.(output_flag))
              if (mask_flag.eq.1) call check_mask    ! test to see if data should be cut
c
c          read data until the EOF is reached,
c          the packet time is not past stop time,
c          and the number of output lines does not exceed specified value
c
            if ((time_flag.eq.1).and. !  process data if packet time is within specifed start and stop times
     1     (nspins.gt.0).and.   ! trap (encountered 1995-01-19 07:29:33)
     1     (((select_calib_mode.eq.0).and.
     1     (sum_cal_mode.eq.0)).or.
     1     ((select_calib_mode.eq.1).and.
     1     (sum_cal_mode.gt.0)).or.
     1     (select_calib_mode.eq.-1)).and.
     1     (mask_interval_flag.eq.0)) then
c                                      process data
c                                    *****************
c
        if (expose_flag.eq.1) then
          call check_expose   ! check if time is in selected interval
        else
          expose_interval_flag = 1   ! process all times
        end if
c
c      cccccccccccccccc output proceedures ccccccccccccccccc
c
          do script_number =1, Nscripts
            unit_out =  script_number+10
            if ((script_output_type(script_number).ne.2).and.
     1          ((reset_interval(script_number).eq.1).and.
     1           ((output_sych_flag(script_number).eq.0)).or.
     1           ((reset_interval_all.eq.1).and.
     1           (output_sych_flag(script_number).eq.1)))) then    ! stop accumulating, process results and write out
              if (time_interval(script_number).gt.0.0) then
                call ave_kp(script_number)    ! browse parameters
                call calculate_rates(script_number)
                cut_interval_flag =
     1            cut_interval_test(script_number,1)
                if (cut_interval_flag) then
                  if (raw_counts_flag.eq.1)
     1              call calculate_counts(script_number)
                  if ((output_script_flag(script_number)).and.
     1               (script_output_type(script_number).eq.1)) then         ! write out rates data, type 1 (no PHA data)
                      output_script_flag(script_number)=
     1                   write_output(unit_out,script_number,
     1                   start_time_avg(script_number),
     1                   stop_time_avg(script_number))
                if (.not.output_script_flag(script_number))  type *,
     1         'maximum # of lines reached in script ' , script_number
                  end if
c
                  if ((output_script_flag(script_number)).and.
     1               (script_output_type(script_number).eq.5)) then         ! write out omni rates data, type 5 (no PHA data)
                      output_script_flag(script_number)=
     1                   write_output_omni(unit_out,script_number,
     1                   start_time_avg(script_number),
     1                   stop_time_avg(script_number))
                if (.not.output_script_flag(script_number))  type *,
     1         'maximum # of lines reached in script ' , script_number
                  end if
c
                  if ((output_script_flag(script_number)).and.
     1               (script_output_type(script_number).eq.3)) then          ! write out hist data, type 3
                     output_script_flag(script_number)=
     1                  write_hist_output(unit_out,script_number,
     1                   start_time_avg(script_number),
     1                   stop_time_avg(script_number))
                if (.not.output_script_flag(script_number))  type *,
     1         'maximum # of lines reached in script ' ,script_number
                  end if
c
                  if ((output_script_flag(script_number)).and.
     1                (script_output_type(script_number).eq.4)) then  ! write out pha flux data, type 4
                    if (raw_counts_flag.eq.0)
     1                 call calculate_phaflux(script_number,
     1                   start_time_avg(script_number),
     1                   stop_time_avg(script_number))
                    if (raw_counts_flag.eq.1)
     1                 call calculate_phacounts(script_number)
                    output_script_flag(script_number)=
     1                write_phaflux_output(unit_out,script_number,
     1                   start_time_avg(script_number),
     1                   stop_time_avg(script_number))
                if (.not.output_script_flag(script_number))  type *,
     1         'maximum # of lines reached in script ' , script_number
                  end if
                end if  ! end if cut_interval_flag
              end if      ! end if time_interval.gt.0.0
              call  reset_time(script_number)   !  reset accumulated variables
              first_record(script_number) = .true.
            end if       ! end if reset_interval.eq.1
          end do  ! end do script_number
c
c      cccccccccccccccc accumulation proceedures ccccccccccccccccc
c
          reset_interval_all = 1
          do script_number =1, Nscripts
            unit_out =  script_number+10
            if (expose_interval_flag.gt.0) then     !  accumulate data to make average
              if (first_record(script_number)) then
                start_time_avg(script_number) = start_time
                first_record(script_number) = .false.
              end if
              stop_time_avg(script_number) = end_time
              call get_spin_period  ! get correct spin_period from kp data if available
              call get_mdata(script_number)   !  accumulate rates data
              call get_exposure(script_number)  ! get accumulation time and exposure
              call ave_kp_1mf(script_number)
              call calculate_rates_1mf(script_number)   ! calculate rates for spin pair data, used for PHA outputs (type 2)
              cut_interval_flag =
     1          cut_interval_test(script_number,2)
              if (cut_interval_flag) then
                if (script_output_type(script_number).ne.1)
     1            call cut_PHA_data(script_number)
                if ((output_script_flag(script_number)).and.
     1            (script_output_type(script_number).eq.2)) then
                  if (raw_counts_flag.eq.1)
     1              call calculate_counts_1mf(script_number)
c
                  output_script_flag(script_number)=
     1              write_output(unit_out,script_number,
     1              start_time, end_time)
             if (.not.output_script_flag(script_number))  type *,
     1      'maximum # of lines reached in script ' , script_number
                end if
c
                if ((output_script_flag(script_number)).and.
     1             (script_output_type(script_number).eq.3)) then   ! accumulate histogram data
                   thishistfull(script_number) =
     1               fill_histogram(script_number)
                end if
c
                if (script_output_type(script_number).ne.2) then
                  call sum_kp(script_number)      ! accumulate browse data
                  if ((output_script_flag(script_number)).and.
     1               (script_output_type(script_number).eq.4))     ! accumulate pha flux data
     1               thisphafluxfull(script_number) =
     1                fill_phaflux(script_number,spin_number)
                end if
              end if  ! end if cut_interval_flag
            end if  ! end if expose_interval_flag.gt.0
c
cccccccccccccccccccc check accumulation cccccccccccccccccccccc
            if (expose_flag.ne.1) then
              if (script_output_type(script_number).eq.1) then
                if (((script_accum_t_flag(script_number).eq.0).or.     ! check to see if accumulation criteria has been meet
     1            ((script_accum_t_flag(script_number).eq.1).and.
     1            (time_interval(script_number).ge.
     1             script_idelta_t(script_number)))).and.
     1            ((script_accum_Nr_flag(script_number).eq.0).or.
     1            ((script_accum_Nr_flag(script_number).eq.1).and.
     1            (Nr_counts(script_number).ge.
     1             script_idelta_Nr(script_number))))) then
                     reset_interval(script_number) = 1   ! stop accumulating
                end if
              end if
              if (script_output_type(script_number).eq.3) then
                 if (((script_accum_t_flag(script_number).eq.0).or.     ! check to see if accumulation criteria has been meet
     1           ((script_accum_t_flag(script_number).eq.1).and.
     1            (time_interval(script_number).ge.
     1             script_idelta_t(script_number)))).and.
     1            ((script_accum_Nr_flag(script_number).eq.0).or.
     1            ((script_accum_Nr_flag(script_number).eq.1).and.
     1            (Nr_counts(script_number).ge.
     1             script_idelta_Nr(script_number)))).and.
     1            ((script_accum_Nh_flag(script_number).eq.0).or.
     1            ((script_accum_Nh_flag(script_number).eq.1).and.
     1            (thishistfull(script_number))))) then
                     reset_interval(script_number) = 1   ! stop accumulating
                end if
              end if
               if (script_output_type(script_number).eq.4) then
                 if (((script_accum_t_flag(script_number).eq.0).or.     ! check to see if accumulation criteria has been meet
     1            ((script_accum_t_flag(script_number).eq.1).and.
     1            (time_interval(script_number).ge.
     1             script_idelta_t(script_number)))).and.
     1            ((script_accum_Nr_flag(script_number).eq.0).or.
     1            ((script_accum_Nr_flag(script_number).eq.1).and.
     1            (Nr_counts(script_number).ge.
     1              script_idelta_Nr(script_number)))).and.
     1            ((script_accum_Nphaf_flag(script_number).eq.0).or.
     1            ((script_accum_Nphaf_flag(script_number).eq.1).and.
     1            (thisphafluxfull(script_number))))) then
                     reset_interval(script_number) = 1   ! stop accumulating
                end if
              end if
            end if
c
c           handle time interval for averaging:
            if ((output_sych_flag(script_number).eq.1).and.
     1          (output_script_flag(script_number)).and.
     1          (script_output_type(script_number).ne.2).and.
     1          (reset_interval(script_number).eq.0))  then
               reset_interval_all = 0
            end if
          end do  ! end do script_number
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                end if  ! end if time_flag
            end_flag =  get_data_record(unit_in)  ! read in next major frame packet
            call expand_data_packet   ! decode data in packet
            call evaluate_pha
            call update_ROMbox(unit_ROM)
            time_flag =  check_time()   ! check if packet is within specified start and stop times
            output_flag = .false.
            do  script_number = 1, Nscripts
              if (output_script_flag(script_number)) then  ! write data to output file
                 output_flag = .true.   ! still writing to some output files
              end if
            end do
c
          end do  ! end loop that reads in packets
c
          close (unit=unit_in,err=1000)   ! close input file
        end if  ! if not a good file
c
        iday = iday + 1  ! go to next day
c  	  check for a year boundary crossing
	      if (iday.gt.idays_per_year(iyear)) then
	         iday = 1
	         iyear = iyear + 1
        end if
c	    write out time since last call
	      secnow=secnds(zero)
c	seconds elapsed message commented out 28-Jul-2008 to keep down lenght of batch file outputs
c	      type *, ' Seconds elapsed since last open_SDF call: ',
c    1    secnow-secinit
	      secinit=secnow
c
      end do ! end loop through multiple UDF's
c
cccccccccccc write final accumulation to output ccccccccccc
      do script_number =1, Nscripts
        unit_out = script_number+10
        if ((script_output_type(script_number).ne.2).and.
     1     (time_interval(script_number).gt.0.0)) then
           call calculate_rates(script_number)
           cut_interval_flag =
     1       cut_interval_test(script_number,1)
           if (cut_interval_flag) then
             if (raw_counts_flag.eq.1)
     1         call calculate_counts(script_number)
             call ave_kp(script_number)
             if ((output_script_flag(script_number)).and.
     1     (script_output_type(script_number).eq.1)) then
              output_script_flag(script_number) =
     1           write_output(unit_out,script_number,
     1           start_time_avg(script_number),
     1           stop_time_avg(script_number))
             end if
             if ((output_script_flag(script_number)).and.
     1     (script_output_type(script_number).eq.5)) then
              output_script_flag(script_number) =
     1           write_output_omni(unit_out,script_number,
     1           start_time_avg(script_number),
     1           stop_time_avg(script_number))
             end if
             if ((output_script_flag(script_number)).and.
     1       (script_output_type(script_number).eq.3)) then
                output_script_flag(script_number)=
     1           write_hist_output(unit_out,script_number,
     1                   start_time_avg(script_number),
     1                   stop_time_avg(script_number))
             end if
             if ((output_script_flag(script_number)).and.
     1       (script_output_type(script_number).eq.4)) then
               if (raw_counts_flag.eq.0)
     1            call calculate_phaflux(script_number)
               if (raw_counts_flag.eq.1)
     1            call calculate_phacounts(script_number)
               output_script_flag(script_number)=
     1         write_phaflux_output(unit_out,script_number,
     1                   start_time_avg(script_number),
     1                   stop_time_avg(script_number))
             end if
           end if  ! end if cut_interval_flag
        end if  ! end time_interval
      end do ! end script_number loop
c
      do  i=1, Nscripts
         unit_out = i+10
         close (unit=unit_out,err=2000)   ! close output file
      end do
c
c
      type *, 'Sdf_lister is finished'
      stop
c
1000  type *,'Error closing input file!'   ! on error go to here
      stop
2000  type *,'Error closing output file!'   ! on error go to here
      stop
c
      end  ! end Sdf_lister
c
c
c
c

