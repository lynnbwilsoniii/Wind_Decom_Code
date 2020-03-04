c
c
c    include file for sdf_lister
c    contains type declarations
c    12/6/95  by J. Dwyer
c             modifications:
c                    2/6/96 by J. Dwyer added STEP_KP_fluxes time averaging
c                    3/21/96 by J. Dwyer added atomic_charge
c                    3/25/96 by J. Dwyer added effic_pha
c                    3/26/96 by J. Dwyer added effic_pha_ave
c                    4/4/96 by J. Dwyer  added PHA_E_mid and atomic_mass_mid
c                    4/4/96 by J. Dwyer increase possible number of PHA flux boxes to 1000
c                    4/4/96 by J. Dwyer added mdata_1mf and mdata_flag_1mf
c                    4/8/96 by J. Dwyer added calib_file_track to calib filenames
c                    4/25/96 by J. Dwyer added PHA_tel_flag
c                    4/26/96 by J. Dwyer changed PHA_stateab_flag to PHA_slant_flag
c                    9/4/96 by J. Dwyer added mask_last_index
c                    2/7/97 by J. Dwyer added E8 - E10 flags
c                    2/13/97 by J. Dwyer added tdp_flag_1mf, tdp_flag, N3dp, Brms,eflux,ionflux
c                                        added epact kp data. removed sdv_spin_rate, SCr
c                    2/14/97 by J. Dwyer added append kp flags
c
c
c	                    variables to be read from configuration file 
c                    ************************************
      real    min_Einc, max_Einc  ! minimum, maximum incident energy (MeV/nuc)
      real    min_mass, max_mass  ! minimum, maximum mass (AMU)
      real    min_start, max_start   ! minimum, maximum START count rate (cts/sec) 
      real    lambda   ! power law spectral index of differential particle flux (E^lambda)
      integer   expose_flag    ! use time interval mask 
      integer   mask_flag    ! use time interval mask 
      integer   KPweight_flag   ! KP weighting flag (0: weight=1.0;1: weight=vse)
      integer   Bdir_flag  ! B direction weighting flag (0=avg B vectors;1=avg unit vectors)
      integer   min_en_channel   ! minimum energy channel
      integer   min_tof_channel ! minimum tof channel
      integer	 tel_flag	! 1 vs. 2;  -1 = list both
      integer	 ramp_flag	! 0 vs. 1;  -1 = list both
      integer	 cal_flag	! 0 vs. 1 (cal);  -1 = list both
      integer	 slant_flag	! 0 vs. 1 (slant fired);  -1 = list both
      integer	 SSD2_flag	! 0 vs. 1 (D2 fired);  -1 = list both
      integer	 stateab_flag	! 0 vs. 1 (state A);  -1 = list both
      integer	 rom_box_flag	! box # to list;  -1 = list all
      integer	 sector_flag	! sector # to list;  -1 = list all
      integer   randomize_flag ! t = randomize PHA tof and energy, f = don't randomize
      Integer	 istart_year,istart_day,istart_hour,istart_min   
      Integer	 istop_year,istop_day,istop_hour,istop_min
      integer    stime_start, stime_stop   ! sampex time of start and stop time and date, in sec
      integer   idelta_t   ! time interval in seconds to average matrix rates.
      integer   maximum_rows   ! maximum number of data rows to write to output file
      integer   dataformat_flag    ! Output data format (0=kaleidagraph format, 1=IDL format)
      integer   fluxformat_flag    ! Output flux format (0=differential flux, 1=integrated flux)
      integer   write_pha_data    ! 1=write results to PHA output file, 0=don't
      integer   write_ave_matrix    ! 1=write results to matrix rate output file, 0=don't
      integer   write_sect_matrix     ! 1=write results to sectored output file, 0=don't
      integer   write_omni_matrix     ! 1=write results to step_omnirates style output file
      integer   write_abr_matrix    ! 1=write abridges version of AVE
      integer   write_flux_matrix     ! 1=write results to PHA flux output file
      integer   write_hsk_matrix    ! 1=write results to housekeeping output file
      integer   write_STEPKP          ! 1=write results to STEP KP output file
      integer   kp_append_spin       ! 1=append S/C spin data
      integer   kp_append_MFI        ! 1=append MFI data
      integer   kp_append_SCpos    ! 1=append S/C GSE coordinates
      integer   kp_append_SWE       ! 1=append Solar wind data
      integer   kp_append_3dpe      ! 1=append 3DP electron data
      integer   kp_append_3dpion   ! 1=append 3DP ion data
      integer   kp_append_APE       ! 1=append EPACT APE data
      integer   kp_append_LEMT     ! 1=append EPACT LEMT data
c
      common /config/ min_Einc, max_Einc,
     1      min_mass, max_mass,
     1      min_start, max_start, lambda,
     1      expose_flag, mask_flag, 
     1      KPweight_flag, Bdir_flag,
     1      min_en_channel, min_tof_channel, 
     1      fluxformat_flag, dataformat_flag, 
     1      randomize_flag,
     1      write_pha_data, write_ave_matrix,
     1      write_sect_matrix, write_omni_matrix,
     1      write_abr_matrix, write_flux_matrix,
     1      write_hsk_matrix, write_STEPKP,
     1      kp_append_spin, kp_append_MFI,
     1      kp_append_SCpos, kp_append_SWE,
     1      kp_append_3dpe, kp_append_3dpion,
     1      kp_append_APE, kp_append_LEMT,
     1      tel_flag, ramp_flag, 
     1      cal_flag, slant_flag,
     1      SSD2_flag, stateab_flag,
     1      rom_box_flag, sector_flag,
     1      istart_year,istart_day,
     1      istart_hour,istart_min,   
     1      istop_year,istop_day,
     1      istop_hour,istop_min, idelta_t,
     1      stime_start, stime_stop,
     1      maximum_rows
    
c
c  file names:
      character*120     calib_file_tof  ! tof calibration file name
      character*120     calib_file_lo   ! lo calibration file name
      character*120     calib_file_hi   ! hi calibration file name
      character*120     calib_file_einctof  ! Einc vs tof calibration file name
      character*120     calib_file_geom  ! Geometrical factor calibration file name
      character*120     calib_file_rombox  ! ROM box efficiency calibration file name
      character*120     calib_file_track  ! ROM box track efficiency calibration file name
      character*120     calib_file_phaflux   ! PHA flux calibration file name
      character*120     expose_file  ! time interval mask file
      character*120     mask_file  ! time interval mask file
      character*120     configfile       ! configuration file name. To be entered from keyboard
      character*120      outputfile     ! partial name of output data file. A prefix is added to 
c                                                       output files which identifies type. e.g. PHA.
      character*120      outputdir      ! output file directory
      character*3   file_extent_prefix   ! file extension prefix for sdf input file
c
      common /files/ calib_file_tof, calib_file_lo,
     1    calib_file_hi, calib_file_einctof,
     1    calib_file_geom, calib_file_rombox,
     1    calib_file_track, calib_file_phaflux,
     1    expose_file, mask_file,
     1    configfile, outputfile, outputdir,
     1    file_extent_prefix
c
c                                             Calibration data
c                                         ****************
c  PHA calibration data:
      integer*4  cal,ntof,nlo,nhi
      Parameter (cal = 100, ntof = 66, nlo = 34, nhi = 93)
      real	 einc_cal(100),tof_cal(100)  ! used to covert tof to energy
      real    tof(3,cal), hi(3,cal), lo(3,cal)
c  Matrix calibartion data:
      real		elo(41,2)       ! lower energy bound for flux bin
      real		ehi(41,2)	! upper energy bound for flux bin
      real		aom(2)		! geometry factors tel 1,2
      real		effic(41,2)	! efficiencies for each flux bin
      real    atomic_charge(1000)   ! atomic charges for each flux box
      real    atomic_mass_lo(1000), 
     1          atomic_mass_hi(1000)   !  mass range for PHA fluxes
      real   PHA_E_lo(1000), PHA_E_hi(1000)  !  energy range for PHA fluxes
      real   atomic_mass_mid(1000) ! mass of each bin used to calculate efficiency
      real   PHA_E_mid(1000) ! energy of each bin used to calculate efficiency
      real   effic_pha_ave(1000)   ! average efficiency for PHA flux boxes
      integer   nfluxes  ! number of PHA fluxes to be calculated
      integer   PHA_slant_flag(1000)  ! state A/B flag (0= B only,1= A only, -1=both)
      integer   PHA_tel_flag(1000)  ! telescope flag (1= tel1,2= tel2, -1=both)
      logical effic_flag(41,2)  ! .true.= ROM box O.K. to use,.false.=don't use this box
      integer    N_cal  ! N elements read into einc_cal and tof_cal
      character*10      rate_name(41)  ! names of elements and energy bins
      character*20   pha_flux_names(1000)  ! names of PHA fluxes 
c
      common /cal/ tof,hi,lo,elo,ehi,aom,effic,
     1    atomic_mass_lo, atomic_mass_hi,
     1    atomic_mass_mid, PHA_E_mid,
     1     atomic_charge, PHA_E_lo, PHA_E_hi,
     1     effic_pha_ave,
     1     einc_cal, tof_cal, N_cal, nfluxes,
     1     PHA_slant_flag, PHA_tel_flag,
     1     effic_flag
      common /calstr/  pha_flux_names, rate_name
c
c                                               Processed Data
c                                         ******************
c  mask variables:
      real    starttime_mask(5000)   ! start time of selected interval to cut
      real    stoptime_mask(5000)   ! stop time of selected interval to cut
      real    starttime_expose(5000)   ! start time of selected interval to keep
      real    stoptime_expose(5000)   ! stop time of selected interval to keep
      integer    startyear_mask(5000)   ! start year of selected interval to cut
      integer    stopyear_mask(5000)   ! stop year of selected interval to cut
      integer   mask_interval_flag   ! (>0: process time interval with that #; 0:do not process)
      integer   Nmask   ! number of mask time intervals
      integer   mask_last_index   ! last mask index searched 
      integer    startyear_expose(5000)   ! start year of selected interval to keep
      integer    stopyear_expose(5000)   ! stop year of selected interval to keep
      integer   expose_interval_flag   ! (>0: process time interval with that #; 0:do not process)
      integer   Nexpose   ! number of expose time intervals
      
c
      common /mask/  starttime_mask,
     1     stoptime_mask, starttime_expose,
     1     stoptime_expose, startyear_mask,
     1     stopyear_mask, mask_interval_flag,
     1     Nmask, startyear_expose,
     1     stopyear_expose, expose_interval_flag,
     1     Nexpose, mask_last_index
c
c  PHA variables: 
      logical    data_flag(50)    ! .true.= good data, .false. = bad data. used for cuts
      integer	  epha(50)      ! uncalibrated energy channel number
      integer    tpha(50)      ! uncalibrated tof channel number
      integer    ramp(50), tel(50), cn(50), slant(50), ssd2(50), ab(50),
     1       rom(50), spin(50), sect(50)   ! pha info
      real 		    nsec(50)     ! tof in nsec
      real         mass(50)    ! mass in AMU
      real         MeV(50)      !  Solid State detector Energy Measurement in MeV
      real         Einc(50)     !  Energy from tof measurement in MeV/nuc 
      real         effic_pha(50)     !  calculated efficiency of STEP for each PHA event
      real         PHA_counts(1000,2,2,2)  ! accumulated PHA counts, PHA_counts (bin, tel, stateab, slant)
      real         PHA_counts_sum(2,2,2)  ! accumulated PHA counts, PHA_counts_sum(tel, stateab, slant)
      real          PHA_exposure(1000,2) ! average accumulated PHA exposure based on ROM boxes
      real         PHA_Ncounts(1000)    ! total number of PHA events accumulated
c
c  Accumulated matrix counts, disc counts and exposure:
      real         STEP_KP_fluxes_ave(6)  ! time averaged STEP Key parameter fluxes
      real         mdata(41,2,8)  ! array containing matrix counts, where the indices are the element, 
c                                             the telescope, and the sector, respectively
      real         mdata_1mf(41,2,8)  ! total matrix for this mf
      real        exposure(41,2)   ! denominator in matrix flux calculations     
      real    vse(2,8)  ! sectored discriminator counts   
      real    vsebar, start, stop, d2, d1    ! nonsectored discriminator rates  
      real   vse1A, vse1B, 
     1        vse2A, vse2B   ! summed vse counts for tel 1,2 and state A,B
      integer     NSTEP_KP_fluxes(6)  ! number of events accumulated
      integer     vse_master_flag  ! saturation flag for vse1A,vse1B,vse2A,vse2B
      integer     mdata_x(41) ! flag listing 1x (=1), 2x (=2) and 4x (=4) matrices
      integer       mdata_flag(41,2,8)  ! flag indicating if good data is in mdata array                     
c                                                     1 = good data measured, 0 = no measurement, -1 = saturated measurement
      integer  mdata_flag_1mf(41,2,8)  ! matrix flag for this mf
      integer    vsebar_flag, start_flag, 
     1       stop_flag, d2_flag, d1_flag    ! nonsectored discriminator rate flags
      integer        vse_flag(2,8)  ! flag indicating if good data is in vse
c
c  variables for accumulating data over time:
      real        time_interval  ! time elapsed (sec) for 1x rates, used to calculate time averaged matrix rates
      real        time_interval_2x(2)  ! time elapsed (sec) for 2x rates, phases 0 or 2, and phases 1or 3     
      real        time_interval_4x(4)  ! time elapsed (sec) for 4x rates, phase 1, phase 2, phase 3, and phase 4      
      integer  start_time_avg   ! start time of time interval over which rates are averaged  
      integer  stop_time_avg   ! stop time of time interval over which rates are averaged     
      integer    reset_interval   ! 1=start new time interval to average matrix rates, 0 otherwise
c
c  S/C info
      real        ave_spins(2)  ! ave number of spins per mf
c                                        1 = 46 s mf; 2 = 92  s mf
c                                        46 sec mf: 14 spins 61% of the time, 15 spins otherwise
c                                        92 sec mf: 29 spins 58% of the time, 28 spins otherwise
      real     spin_period     !  S/C spin period in sec
      integer      mf_type          ! type of major frame
c
c  matrix  info
      logical    H_sectored_flag(41)     ! .true.= H and sectored, .false.=otherwise
      logical    He_sectored_flag(41)    ! .true.= He and sectored, .false.=otherwise
      logical    CNO_sectored_flag(41)  ! .true.= CNO and sectored, .false.=otherwise
      logical    Fe_sectored_flag(41)    ! .true.= Fe and sectored, .false.=otherwise
      logical    all_sectored_flag(41)   ! .true.= sectored (not state), .false.=otherwise
      logical    E1_sectored_flag(41)   ! .true. = energy bin 1 included in sectored flux
      logical    E2_sectored_flag(41)   ! .true. = energy bin 2 included in sectored flux
      logical    E3_sectored_flag(41)   ! .true. = energy bin 3 included in sectored flux
      logical    E4_sectored_flag(41)   ! .true. = energy bin 4 included in sectored flux
      logical    E5_sectored_flag(41)   ! .true. = energy bin 5 included in sectored flux
      logical    E6_sectored_flag(41)   ! .true. = energy bin 6 included in sectored flux
      logical    E7_sectored_flag(41)   ! .true. = energy bin 7 included in sectored flux
      logical    E8_sectored_flag(41)   ! .true. = energy bin 5 included in sectored flux
      logical    E9_sectored_flag(41)   ! .true. = energy bin 6 included in sectored flux
      logical    E10_sectored_flag(41)   ! .true. = energy bin 7 included in sectored flux
c
      common /sect/ H_sectored_flag,
     1   He_sectored_flag, CNO_sectored_flag,
     1   Fe_sectored_flag, all_sectored_flag,
     1   E1_sectored_flag, E2_sectored_flag,    
     1   E3_sectored_flag, E4_sectored_flag,
     1   E5_sectored_flag, E6_sectored_flag,
     1   E7_sectored_flag, E8_sectored_flag, 
     1   E9_sectored_flag, E10_sectored_flag
c     
c  decoded raw data for 1 mf:   
      real    vsebar_1mf, start_1mf, 
     1        stop_1mf, d2_1mf, d1_1mf    ! nonsectored discriminator rates for 1mf only
      real    sdrate(32)   ! sectored discriminator rates for 1 mf
      integer   phase   ! value determines which bins are used in matrix rates (see below)
      integer	 nspins          	 ! number of spins in mf
      integer	 first_spin	   ! number of first spin in mf
      integer	 step_status	! step status
      real   m1rate(302)   ! 1x matrix rates, bins 1-290, 651-662 (every mf)
      real    m2rate(84)     !  2x matrix rates
c                                                 phase 0,2: bins 291-374
c                                                 phase 1,3: bins 375-458
      real   m4rate(48)     ! 4x matrix rates
c                                                 phase 0: bins 459-506
c                                                 phase 1: bins 507-554
c                                                 phase 2: bins 555-602
c                                                 phase 3: bins 603-650             
c
      common /proc/ data_flag, epha, tpha,
     1      ramp, tel, cn, slant, ssd2, ab, rom, spin, sect, 
     1      nsec, mass, MeV, Einc,effic_pha,PHA_counts,
     1      PHA_exposure, PHA_Ncounts,
     1      PHA_counts_sum,
     1      ave_spins, spin_period,
     1      start_time_avg, stop_time_avg,
     1      vsebar, start, stop, d2, d1,
     1      vsebar_1mf, start_1mf, 
     1      stop_1mf, d2_1mf, d1_1mf,
     1      vse1A, vse1B, 
     1      vse2A, vse2B,
     1      sdrate, STEP_KP_fluxes_ave,
     1      phase, nspins, first_spin, step_status,	
     1      m1rate, m2rate, m4rate, mf_type,     
     1      vsebar_flag, start_flag, 
     1      stop_flag, d2_flag, d1_flag,
     1      vse_master_flag, NSTEP_KP_fluxes  
c
      common /proc2/ mdata, exposure, vse,
     1       time_interval, time_interval_2x, 
     1       time_interval_4x, mdata_1mf, 
     1       mdata_flag, vse_flag, reset_interval,
     1       mdata_x, mdata_flag_1mf
     
c
c                                 kp  data
c                             **********
          real        Nspha   ! weighted number of spha kp events averaged in time interval
          real        Nmfi(2)    ! weighted number of mfi kp events averaged in time interval
          real        Nswe   ! weighted number of swe kp events averaged in time interval
          real        N3dp(2)    ! weighted number of 3dp kp events averaged in time interval
          real        Nepa(2)    ! weighted number of 3dp kp events averaged in time interval
          real        spin_rate    ! S/C spin rate in rad/sec
          real        Bx    ! x-component of magnetc field (GSE)
          real        By    ! y-component of magnetc field (GSE)
          real        Bz    ! z-component of magnetc field (GSE)
          real        Brms    ! rms of B field
          real        Bfield   ! magnitude of magnetic field 
          real        Btheta    ! theta of magnetic field (deg)
          real        Bphi    ! phi magnetic field (deg)       
          real        SCx  ! x GSE position of S/C (Re)
          real        SCy  ! y GSE position of S/C (Re)
          real        SCz   ! z GSE position of S/C (Re)     
          real        SWVx   ! x-component solar wind velocity (km/sec)
          real        SWVy   ! y-component solar wind velocity (km/sec)
          real        SWVz   ! z-component solar wind velocity (km/sec)
          real        Nproton  ! proton density - #/cc
          real        SWVth     ! Solar Wind thermal speed
          real        eflux(7)   !  3dp electron flux
          real        ionflux(7)   !  3dp ion flux
          real        J_apeb1, J_apeb2,   
     1                 J_apeb3, J_apeb4, J_apeb5    ! EPACT fluxes
          real        J_lemt1, J_lemt2, J_lemt3   ! EPACT fluxes
          logical   spha_flag  ! swe kp data read at all
          logical   mfi_flag(2)  ! swe kp data read at all
          logical   swe_flag  ! swe kp data read at all
          logical   tdp_flag(2)  ! 3dp kp data read at all
          logical   epa_flag(2)  ! epact kp data read at all
          logical   spha_flag_1mf  ! swe kp data read in current packet
          logical   mfi_flag_1mf  ! swe kp data read in current packet
          logical   swe_flag_1mf  ! swe kp data read in current packet
          logical   tdp_flag_1mf  ! 3dp kp data read in current packet
          logical   epa_flag_1mf  ! epact kp data read in current packet
c
      common /kp/    Nspha, Nmfi, Nswe, N3dp, Nepa,
     1      spin_rate, Brms, Bx,
     1      By, Bz,SCx,SCy,SCz, SWVx, 
     1      SWVy,SWVz,Nproton,SWVth,
     1      J_apeb1,J_apeb2,J_apeb3,
     1      J_apeb4,J_apeb5,J_lemt1,
     1      J_lemt2, J_lemt3,
     1      eflux,ionflux,
     1      spha_flag,mfi_flag,swe_flag,tdp_flag,
     1      epa_flag,  epa_flag_1mf,
     1      spha_flag_1mf,mfi_flag_1mf,swe_flag_1mf,
     1      tdp_flag_1mf, Bfield, Btheta, Bphi
c
