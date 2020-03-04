c
c    Subroutines for the program sdf_lister
c    
c     Contents:
c
c             subroutine  initial_global
c
c             logical function get_config
c
c             subroutine write_header
c
c             subroutine write_kpdata
c
c             subroutine write_kptitles
c
c
c************************************************
c
        subroutine  initial_global
c
c************************************************
c
c     set initial values for global variables
c 
c     12/1/95 by J. Dwyer
c        modified   12/31/96 by J. Dwyer changed format to vse,E2,...,E6., write_sect_matrix
c                        now can have valoes, 0,1,2,3,4, 0=dont write output
c                        1 = H,2=He,3=CNO,4=Fe.
c                  1/20/97 added E1 to sector flag
c                  2/7/97 by J. Dwyer added E7 - E10 flags for protons
c 
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c     
      ave_spins(1) = 14.39    ! ave number of spins per 46 s mf                                 
      ave_spins(2) = 28.58    ! ave number of spins per 92 s mf
      spin_period = 3.011         ! S/C spin period in seconds (Default value)
c     
c    assign flags for matrices that are sectored. Used in anisotropy calculations
      do 100 i=1,41
          H_sectored_flag(i) = .false.    
          He_sectored_flag(i) = .false.
          CNO_sectored_flag(i) = .false.
          Fe_sectored_flag(i) = .false.
          all_sectored_flag(i) = .false.
          E1_sectored_flag(i) = .false.   
          E2_sectored_flag(i) = .false.
          E3_sectored_flag(i) = .false.
          E4_sectored_flag(i) = .false.
          E5_sectored_flag(i) = .false.
          E6_sectored_flag(i) = .false.
          E7_sectored_flag(i) = .false.
          E8_sectored_flag(i) = .false.
          E9_sectored_flag(i) = .false.
          E10_sectored_flag(i) = .false.
100    continue
c     
       if (write_sect_matrix.eq.1) then   ! protons
            E1_sectored_flag(2) = .true.  
            E2_sectored_flag(3) = .true.  
            E3_sectored_flag(4) = .true.
            E4_sectored_flag(5) = .true.
            E5_sectored_flag(6) = .true.
            E6_sectored_flag(7) = .true.
            E7_sectored_flag(8) = .true.
            E8_sectored_flag(9) = .true.
            E9_sectored_flag(10) = .true.
            E10_sectored_flag(11) = .true.
       end if
       if (write_sect_matrix.eq.2) then   ! He
            E1_sectored_flag(12) = .true.
            E2_sectored_flag(13) = .true.  
            E3_sectored_flag(14) = .true.
            E4_sectored_flag(15) = .true.
            E5_sectored_flag(16) = .true.
            E6_sectored_flag(17) = .true.
       end if
       if (write_sect_matrix.eq.3) then   ! CNO
            E1_sectored_flag(19) = .true.
            E2_sectored_flag(20) = .true.  
            E3_sectored_flag(21) = .true.
            E4_sectored_flag(22) = .true.
            E5_sectored_flag(23) = .true.
            E6_sectored_flag(24) = .true.
       end if
       if (write_sect_matrix.eq.4) then  ! Fe
            E1_sectored_flag(33) = .true. 
            E2_sectored_flag(34) = .true.  
            E3_sectored_flag(35) = .true.
            E4_sectored_flag(36) = .true.
            E5_sectored_flag(37) = .true.
            E6_sectored_flag(38) = .true.
       end if
c
      do 200 i=2,11
          H_sectored_flag(i) = .true.         ! leave out lowest energy bin
200    continue
      do 300 i=14,18
          He_sectored_flag(i) = .true.          ! leave out lowest energy bin
300    continue
      do 400 i=21,24
          CNO_sectored_flag(i) = .true.     ! leave out lowest energy bin     
400    continue
      do 500 i=34,38
          Fe_sectored_flag(i) = .true.        ! leave out lowest energy bin  
500    continue
      do 600 i=1,41         
          all_sectored_flag(i) =
     1   (H_sectored_flag(i).or.
     1   He_sectored_flag(i).or.
     1   CNO_sectored_flag(i).or.
     1   Fe_sectored_flag(i))
600    continue
c
      do 700 i=1,18
           mdata_x(i) = 1     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
700    continue
      mdata_x(39) = 1     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      mdata_x(40) = 1     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      mdata_x(41) = 1     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      do 800 i=19,25        
           mdata_x(i) = 2     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
800    continue
      do 900 i=26,38
           mdata_x(i) = 4     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
900    continue 
c
      return
      end  ! end initial_global
c		
c
c
c
c
c*******************************************************
c
       logical function get_config()
c
c*******************************************************
c
c    Open the configuration file and read instructions for running sdf_lister
c    function returns .true. if file successfully opened and read,
c    .false. otherwise
c
c     12/4/95 by J. Dwyer
c                modifications:
c                    2/6/96 by J. Dwyer added STEP_KP_fluxes time averaging
c                    4/8/96 by J. Dwyer added calib_file_track to input
c                    4/24/96 by J. Dwyer continued start and stop time read to new line
c                    2/14/97  by J. Dwyer  read append kp flags
c
      include 'sdf_include.inc'   ! include type declarations
c         
      integer     unit_cfg        ! file unit for config file                                   
      integer     iseconds        ! used in convert_to_sampextime call
      	integer values(8)	! for date_and_time call
	character da,tm,zo	! for date_and_time call
	character*120	last_stop_time_file
	integer	day_of_year
c  
      unit_cfg = 19
      get_config = .true.
c
      open(unit=unit_cfg,name=configfile, status='old',     
     1    action='read', disp='keep', err=400)  ! open the config file
c
100     format(a)
200     format(a60)
300     format(a80)
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,100,err=500)     file_extent_prefix   ! used in name of input data file
      	if((file_extent_prefix.eq.'hex').or.(file_extent_prefix.eq.'xdr')) goto 150
		type *, ' illegal file extent prefix: ', file_extent_prefix
		type *, 'sdf_lister stopping'
		stop
150      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,200,err=500) outputfile  ! read output data file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,200,err=500) outputdir  ! read output data file directory
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)    calib_file_tof  ! tof calibration file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)     calib_file_lo   ! lo calibration file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)     calib_file_hi   ! hi calibration file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)    calib_file_einctof  ! Einc vs tof calibration file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)    calib_file_geom  ! Geometrical factor calibration file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)    calib_file_rombox  ! ROM box effic calibration file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)    calib_file_track  ! ROM box track eff calibration file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)    calib_file_phaflux  ! PHA flux calibration file name
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)     expose_file  ! time interval mask file
      read(unit_cfg,*,err=500)     expose_flag  ! use time interval mask
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,300,err=500)     mask_file  ! time interval mask file
      read(unit_cfg,*,err=500)     mask_flag  ! use time interval mask 
      read(unit_cfg,*,err=500)     KPweight_flag   ! KP weighting flag (0: weight=1.0;1: weight=vse)
      read(unit_cfg,*,err=500)     Bdir_flag  ! B direction weighting flag (0=avg B vectors;1=avg unit vectors)
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,*,err=500)     min_en_channel  ! minimum energy channel
      read(unit_cfg,*,err=500)     min_tof_channel ! minimum tof channel
      read(unit_cfg,*,err=500)     min_Einc, max_Einc  ! minimum, maximum incident energy (MeV/nuc)
      read(unit_cfg,*,err=500)     min_mass, max_mass  ! minimum, maximum mass (AMU)
      read(unit_cfg,*,err=500)     min_start, max_start   ! minimum, maximum START count rate (cts/sec) 
      read(unit_cfg,*,err=500)     lambda   ! power law spectral index of differential particle flux (E^lambda)
	    read(unit_cfg,*,err=500)     tel_flag	! 1 vs. 2;  -1 = list both
	    read(unit_cfg,*,err=500)     ramp_flag	! 0 vs. 1;  -1 = list both
	    read(unit_cfg,*,err=500)     cal_flag	! 0 vs. 1 (cal);  -1 = list both
	    read(unit_cfg,*,err=500)     slant_flag	! 0 vs. 1 (slant fired);  -1 = list both
	    read(unit_cfg,*,err=500)     SSD2_flag	! 0 vs. 1 (D2 fired);  -1 = list both
	    read(unit_cfg,*,err=500)     stateab_flag	! 0 vs. 1 (state A);  -1 = list both
	    read(unit_cfg,*,err=500)     rom_box_flag	! box # to list;  -1 = list all
	    read(unit_cfg,*,err=500)     sector_flag	! sector # to list;  -1 = list all
      read(unit_cfg,*,err=500)     randomize_flag	! 1 = randomize PHA tof and energy, 0 = don't randomize
      read(unit_cfg,*,err=500)     ! read comment line
	    read(unit_cfg,*,err=500)     istart_year,istart_day,
     1                             istart_hour,istart_min
	    read(unit_cfg,*,err=500)     ! read comment line
	    read(unit_cfg,*,err=500)     istop_year,istop_day,
     1                             istop_hour,istop_min
      read(unit_cfg,*,err=500)     ! read comment line
	    read(unit_cfg,*,err=500)     idelta_t   ! time interval in seconds to average matrix rates.     
      read(unit_cfg,*,err=500)     maximum_rows  ! maximum # of data rows to write to output file
      read(unit_cfg,*,err=500)     dataformat_flag    ! Output data format (0=kaleidagraph format, 1=IDL format)
      read(unit_cfg,*,err=500)     fluxformat_flag    ! Output flux format (0=differential flux, 1=integrated flux)
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,*,err=500)     write_pha_data    ! write output flag
      read(unit_cfg,*,err=500)     write_ave_matrix  ! write output flag
      read(unit_cfg,*,err=500)     write_sect_matrix   ! write output flag
      read(unit_cfg,*,err=500)     write_omni_matrix   ! write output flag
      read(unit_cfg,*,err=500)     write_abr_matrix   ! write output flag
      read(unit_cfg,*,err=500)     write_flux_matrix   ! write output flag
      read(unit_cfg,*,err=500)     write_hsk_matrix   ! write output flag
      read(unit_cfg,*,err=500)     write_STEPKP   ! write output flag
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,*,err=500)     kp_append_spin       ! append S/C spin data
      read(unit_cfg,*,err=500)     kp_append_MFI        ! append MFI data
      read(unit_cfg,*,err=500)     kp_append_SCpos    ! append S/C GSE coordinates
      read(unit_cfg,*,err=500)     kp_append_SWE       ! append Solar wind data
      read(unit_cfg,*,err=500)     kp_append_3dpe      ! append 3DP electron data
      read(unit_cfg,*,err=500)     kp_append_3dpion   ! append 3DP ion data
      read(unit_cfg,*,err=500)     kp_append_APE       ! append EPACT APE data
      read(unit_cfg,*,err=500)     kp_append_LEMT     ! append EPACT LEMT data

      close(unit=unit_cfg, err=400) 

c
      iseconds = 0  ! specify time only to within a minute
c	get start time from last day processed file:  
	last_stop_time_file='/Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/last_day_sdf_rom_spikes.txt'
	OPEN(UNIT=88,NAME=last_stop_time_file,status='OLD')
     	read(88,*) istart_year,istart_day
350	format(2i5)     	
     	istart_hour=0
     	istart_min=0
      call convert_to_sampextime(istart_year,istart_day,
     1   istart_hour,istart_min,iseconds,stime_start) 
c	get current date for a tentative end date -- will actually stop when first data file not found
	call date_and_time(da,tm,zo,values)
	istop_year = values(1)
	istop_day = day_of_year(values(1),values(2),values(3))
     	istop_hour=0
     	istop_min=0
      call convert_to_sampextime(istop_year,istop_day,
     1   istop_hour,istop_min,iseconds,stime_stop)       
	type *, 'sdf_rom_spikes processing between: ', istart_year,istart_day, ' and ', istop_year,istop_day

c

      return        !  successful
c
400      get_config = .false.   ! on error go to here
      print *, 'Error opening or closing configuration file:', 
     1     configfile
      return
500      get_config = .false.   ! on error go to here
      print *, 'Error reading configuration file:', configfile
      close(unit=unit_cfg, err=400) 
      return
      end  ! end get_config
c
c
c
c*******************************************************
c
       subroutine write_header(unit_out,file_prefix,headerlines)
c
c*******************************************************
c 
c     Writes header to output file
c
c     12/4/95 by J. Dwyer
c          modified 2/28/96 by J. Dwyer added headerline to input variables,
c                                            also fixed new line commands in header output.
c                       4/8/96 by J. Dwyer added calib_file_track to output
c
      include 'sdf_include.inc'   ! include type declarations
      byte  datew(9)       ! run date
      byte  timew(8)       ! run time
      integer   unit_out   ! file unit of output file
      integer  headerlines  ! number of lines in header before titles
      character*4       file_prefix   ! prefix to be attached to output file name
      character*80     line    ! used to write line in header  
c
      line = 
     1  ' *** '//file_prefix//' data file generated by sdf_lister ***'
      call date(datew)	! get run time and date info
      call time(timew)
c
c	Write the contents of the config file to the output data file header
      write(unit_out,100) headerlines,
     1    line,
     1    datew,timew,
     1    file_extent_prefix ,
     1    calib_file_tof,
     1    calib_file_lo,
     1    calib_file_hi,
     1    calib_file_einctof,
     1    calib_file_geom,
     1    calib_file_rombox,
     1    calib_file_track,
     1    calib_file_phaflux,
     1    expose_file, expose_flag,
     1    mask_file, mask_flag, 
     1    KPweight_flag, Bdir_flag,
     1    min_en_channel,min_tof_channel,
     1    min_Einc, max_Einc, 
     1    min_mass, max_mass,  
     1    min_start, max_start,  
     1    lambda,
     1    tel_flag,ramp_flag,cal_flag,slant_flag,ssd2_flag,
     1    stateab_flag,rom_box_flag,sector_flag,
     1    randomize_flag,
     1    istart_year,istart_day,istart_hour,istart_min,
     1    istop_year,istop_day,istop_hour,istop_min,
     1    idelta_t, maximum_rows, dataformat_flag,
     1    fluxformat_flag
c
100   format(' Header Lines =',i4,/,
     1    a80,/,
     1  ' run time: '9a1,2x,8a1,/,
     1  ' prefix on input file = 'a/,
     1  ' tof calibration file = 'a/,
     1  ' lo calibration file = 'a/,
     1  ' hi calibration file = 'a/,
     1  ' Einc vs tof calibration file = 'a/,
     1  ' geom factor calibration file = 'a/,
     1  ' ROM box calibration file = 'a/,
     1  ' ROM box track calibration file = 'a/,
     1  ' PHA flux calibration file = 'a/,
     1  ' time interval window file = 'a/,
     1  ' time interval window flag = 'i4,/,
     1  ' time interval mask file = 'a/,
     1  ' time interval mask flag = 'i4,/,
     1  ' KP weighting flag (0: wt=1.0;1: wt=vse)= 'i4,/,
     1  ' B dir weighting flag (0=B vectors,1=unit vectors) = 'i4,/,
     1  ' minimum Energy channel = 'i4,/,
     1  ' minimum Time-of-flight channel = 'i4,/,
     1  ' minimum, maximum incident energy (MeV/nuc) = ',
     1   F9.4', ',F9.4,/,
     1  ' minimum, maximum mass (AMU) =  'F9.4', ',F9.4,/,
     1  ' minimum, maximum START count rate (cts/sec) = ',
     1    E12.5', ',E12.5,/,
     1  ' spectral index = 'F7.3,/, 
     1  ' tel_flag     ='i3'  1 vs. 2;  -1 = list both',/,
     1  ' ramp_flag    ='i3'  0 vs. 1;  -1 = list both',/,
     1  ' cal_flag     ='i3'  0 vs. 1 (cal);  -1 = list both',/,
     1  ' slant_flag   ='i3'  0 vs. 1 (slant fired);  -1 = list both',/,
     1  ' SSD2_flag    ='i3'  0 vs. 1 (D2 fired);  -1 = list both',/,
     1  ' stateab_flag ='i3'  0 vs. 1 (state A);  -1 = list both',/,
     1  ' rom_box_flag ='i3'  box # to list;  -1 = list all',/,
     1  ' sector_flag  ='i3'  sector # to list;  -1 = list all',/,
     1  ' randomize_flag  ='i3' (1 = randomize PHA; 0 = do not)',/,
     1  ' start time (year,day,hour,min) = 'i4,2x,i3,2x,i2,2x,i2,/,
     1  ' stop time (year,day,hour,min) = 'i4,2x,i3,2x,i2,2x,i2,/,
     1  ' time period to  to average = 'i8,/,
     1  ' maximum number of rows = 'i8,/,
     1  ' data format ='i3' (0=kaleidagraph; 1=IDL)',/,
     1  ' flux format ='i3' (0=differential; 1=integrated flux)',/)
c
       return
       end  ! end write_header
c
c
c
c
c*******************************************************
c
       subroutine write_kptitles(kpstring,stringlength)
c
c*******************************************************
c 
c     Writes kp names to string for titles
c
c     12/4/95 by J. Dwyer
c           modified  3/19/97 by J. Dwyer added Vsw,dist
c
c
      include 'sdf_include.inc'   ! include type declarations
c
      integer    c1, c2, dc
      integer    stringlength
      character*400   kpstring
c
       c1 = 0
       dc = 0
       if (kp_append_spin.eq.1) then 
         c1 = c1+dc+1
         dc = 11
         kpstring(c1:(c1+dc)) =  ' spin rate,'
       end if
       if (kp_append_MFI.eq.1) then 
         c1 = c1+dc+1
         dc = 3
         kpstring(c1:(c1+dc)) = ' B,'
         c1 = c1+dc+1
         dc = 9
         kpstring(c1:(c1+dc)) = ' B theta,' 
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) = ' B phi,' 
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) = ' B rms,' 
       end if
       if (kp_append_SCpos.eq.1) then
         c1 = c1+dc+1
         dc = 6
         kpstring(c1:(c1+dc)) = ' dist,' 
         c1 = c1+dc+1
         dc = 6
         kpstring(c1:(c1+dc)) = ' Xgse,' 
         c1 = c1+dc+1
         dc = 6
         kpstring(c1:(c1+dc)) = ' Ygse,' 
         c1 = c1+dc+1
         dc = 6
         kpstring(c1:(c1+dc)) = ' Zgse,'
       end if
       if (kp_append_SWE.eq.1) then
         c1 = c1+dc+1
         dc = 5
         kpstring(c1:(c1+dc)) = ' Vsw,' 
         c1 = c1+dc+1
         dc = 4
         kpstring(c1:(c1+dc)) = ' Vx,'
         c1 = c1+dc+1
         dc = 4
         kpstring(c1:(c1+dc)) = ' Vy,' 
         c1 = c1+dc+1
         dc = 4
         kpstring(c1:(c1+dc)) = ' Vz,'
         c1 = c1+dc+1
         dc = 4
         kpstring(c1:(c1+dc)) = ' Np,' 
         c1 = c1+dc+1
         dc = 5
         kpstring(c1:(c1+dc)) = ' Vth,'
       end if
       if (kp_append_3dpe.eq.1) then
         c1 = c1+dc+1
         dc = 8
         kpstring(c1:(c1+dc)) =  ' 3dp e1,'
         c1 = c1+dc+1
         dc = 8
         kpstring(c1:(c1+dc)) =  ' 3dp e2,'
         c1 = c1+dc+1
         dc = 8
         kpstring(c1:(c1+dc)) = ' 3dp e3,'
         c1 = c1+dc+1
         dc = 8
         kpstring(c1:(c1+dc)) = ' 3dp e4,'
         c1 = c1+dc+1
         dc = 8
         kpstring(c1:(c1+dc)) = ' 3dp e5,'
         c1 = c1+dc+1
         dc = 8
         kpstring(c1:(c1+dc)) = ' 3dp e6,'
         c1 = c1+dc+1
         dc = 8
         kpstring(c1:(c1+dc)) =  ' 3dp e7,'
       end if
       if (kp_append_3dpion.eq.1) then
         c1 = c1+dc+1
         dc = 10
         kpstring(c1:(c1+dc)) = ' 3dp ion1,'
         c1 = c1+dc+1
         dc = 10
         kpstring(c1:(c1+dc)) = ' 3dp ion2,'
         c1 = c1+dc+1
         dc = 10
         kpstring(c1:(c1+dc)) = ' 3dp ion3,'
         c1 = c1+dc+1
         dc = 10
         kpstring(c1:(c1+dc)) = ' 3dp ion4,'
         c1 = c1+dc+1
         dc = 10
         kpstring(c1:(c1+dc)) =  ' 3dp ion5,'
         c1 = c1+dc+1
         dc = 10
         kpstring(c1:(c1+dc)) = ' 3dp ion6,'
         c1 = c1+dc+1
         dc = 10
         kpstring(c1:(c1+dc)) = ' 3dp ion7,'
       end if
       if (kp_append_APE.eq.1) then
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) =  ' APEB2,'
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) = ' APEB3,'
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) =  ' APEB4,'
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) =  ' APEB5,'
       end if
       if (kp_append_LEMT.eq.1) then
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) = ' LEMT1,'
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) = ' LEMT2,'
         c1 = c1+dc+1
         dc = 7
         kpstring(c1:(c1+dc)) = ' LEMT3,'
       end if
c
       stringlength = c1+dc-1
       if (stringlength.le.0) stringlength = 1
c
       return
       end  ! end write_kptitles
c
c
c
c*******************************************************
c
       subroutine write_kpdata(index, outputstring)
c
c*******************************************************
c 
c     Writes kp data to output string
c
c     12/4/95 by J. Dwyer
c           modified  3/19/97 by J. Dwyer added Vsw,dist
c
      include 'sdf_include.inc'   ! include type declarations
c
      integer    index     ! last index of outputstring written to
      character*4096   outputstring
      real    dist, Vsw
c
       if (kp_append_spin.eq.1) then
          call write_line(index, outputstring, 
     1        spin_rate, spha_flag, .false.,
     1                dataformat_flag)        
      end if
      if (kp_append_MFI.eq.1) then
         call write_line(index, outputstring, 
     1        Bfield, mfi_flag(1), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1        Btheta, mfi_flag(1), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1        Bphi, mfi_flag(1), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1        Brms, mfi_flag(1), .false.,
     1                dataformat_flag)
      end if
      if (kp_append_SCpos.eq.1) then   
         dist = sqrt(SCx*SCx+
     1             SCy*SCy+SCz*SCz)   
         call write_line(index, outputstring, 
     1        dist, swe_flag, .false.,
     1                dataformat_flag) 
         call write_line(index, outputstring, 
     1        SCx, mfi_flag(2), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1        SCy, mfi_flag(2), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1        SCz, mfi_flag(2), .false.,
     1                dataformat_flag)        
      end if
      if (kp_append_SWE.eq.1) then 
         Vsw = sqrt(SWVx*SWVx+
     1             SWVy*SWVy+SWVz*SWVz)   
         call write_line(index, outputstring, 
     1        Vsw, swe_flag, .false.,
     1                dataformat_flag)  
         call write_line(index, outputstring, 
     1        SWVx, swe_flag, .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1        SWVy, swe_flag, .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1        SWVz, swe_flag, .false.,
     1                dataformat_flag)  
         call write_line(index, outputstring, 
     1       Nproton, swe_flag, .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1       SWVth, swe_flag, .false.,
     1                dataformat_flag)
      end if
      if (kp_append_3dpe.eq.1) then
        do 100 i=1,7
           call write_line(index, outputstring, 
     1       eflux(i), tdp_flag(1), .false.,
     1                dataformat_flag)
100      continue
      end if
      if (kp_append_3dpion.eq.1) then
        do 200 i=1,7
          call write_line(index, outputstring, 
     1       ionflux(i), tdp_flag(2), .false.,
     1                dataformat_flag)
200      continue    
      end if
      if (kp_append_APE.eq.1) then
         call write_line(index, outputstring, 
     1       J_apeb2, epa_flag(1), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1       J_apeb3, epa_flag(1), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1       J_apeb4, epa_flag(1), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1       J_apeb5, epa_flag(1), .false.,
     1                dataformat_flag)
      end if
      if (kp_append_LEMT.eq.1) then
         call write_line(index, outputstring, 
     1       J_lemt1, epa_flag(2), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1       J_lemt2, epa_flag(2), .false.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1       J_lemt3, epa_flag(2), .false.,
     1                dataformat_flag)
      end if
c                 
c
       return
       end  ! end write_kpdata
c
c
