c
c
c    include file for Sdf_lister
c    contains type declarations
c    7/2/98  by J. Dwyer
c       11/10/98 removed data_flag
c
c
c
c
c
cccccccccccccccccccccccccccccc parameters cccccccccccccccccccccccccccccc
      real        dt_science_record    ! duration of 1 ULEIS science data record in seconds
      real        ave_spins46,ave_spins92  ! ave number of spins per 46 and 92 s mf 
      real        spin_period_default     ! S/C spin period in seconds (Default value)
      integer*4   Nscripts_max    ! maximum number of script files allowed
      integer*4   Ncolumns_max   ! maximum number columns allowed in script file
      integer*4   Nhistbins_max    ! maximum number of bins allowed for all histogram data for all scripts
      integer*4   Ninput_max   ! maximum number mask and expose and history intervals allowed 
      Parameter (dt_science_record = 120.0, ! duration of 1 ULEIS science data record in seconds
     1            ave_spins46 = 14.39, 
     1            ave_spins92 = 28.58,
     1            spin_period_default = 3.011, 
     1            Nscripts_max = 20,    ! maximum number of script files allowed
     1            Ncolumns_max = 1000,   ! maximum number columns allowed in script file
     1            Nhistbins_max = 100000,   ! maximum number of bins allowed for all histogram data for all scripts
     1            Ninput_max = 10000)   ! maximum number mask and expose and history intervals allowed 
c
c
c
cccccccccccccccccccccccccccccc variables to be read from configuration file cccccccccccccccccccccccccccccc   
      integer*4   expose_flag    ! use time interval mask 
      integer*4   mask_flag    ! use time interval mask 
      integer*4   KPweight_flag   ! KP weighting flag (0: weight=1.0;1: weight=vse)
      integer*4   Bdir_flag  ! B direction weighting flag (0=avg B vectors;1=avg unit vectors)   
      integer*4   randomize_flag ! t = randomize PHA tof and energy, f = don't randomize 
      integer*4   select_calib_mode     ! (1=calibration mode, 0 = non cal mode, -1 = no cut)
      Integer*4	  istart_year,istart_day,istart_hour,istart_min   
      Integer*4	  istop_year,istop_day,istop_hour,istop_min
      integer*4   stime_start, stime_stop   ! sampex time of start and stop time and date, in sec          
      integer*4   maximum_rows   ! maximum number of data rows to write to output file
      integer*4   dataformat_flag    ! Output data format (0=kaleidagraph format, 1=IDL format)       
      integer*4   fluence_flag    ! flux or fluence format (0=flux, 1=fluence) 
      integer*4   Nscripts    ! number of scripts to process
      integer*4   raw_counts_flag !  1= raw # counts only, 0 = calculate rates and intensities 
c
      common /configcommon/ 
     1      expose_flag, mask_flag, 
     1      KPweight_flag, Bdir_flag,
     1      randomize_flag,select_calib_mode,  
     1      istart_year,istart_day,
     1      istart_hour,istart_min,   
     1      istop_year,istop_day,
     1      istop_hour,istop_min, 
     1      stime_start, stime_stop,
     1      maximum_rows,
     1      dataformat_flag, 
     1      fluence_flag, Nscripts,
     1      raw_counts_flag
c     
c file names: 
      character*120     calib_file_tof  ! tof calibration file name
      character*120     calib_file_lo   ! lo calibration file name
      character*120     calib_file_hi   ! hi calibration file name
      character*120     calib_file_einctof  ! Einc vs tof calibration file name
      character*120     calib_file_geom  ! Geometrical factor calibration file name    
      character*120     ROMboxhistoryfile   ! file containing ROM box history  
      character*120     calib_file_track  ! ROM box track efficiency calibration file name
      character*120     expose_file  ! time interval mask file
      character*120     mask_file  ! time interval mask file
      character*120     configfile       ! configuration file name. To be entered from keyboard
      character*120     outputfile     ! partial name of output data file. A prefix is added to 
c                                          output files which identifies type. e.g. PHA.
      character*120     outputdir      ! output file directory
      character*120     scriptfile(Nscripts_max)   ! output script file names      
      character*3      file_extent_prefix   ! file extension prefix for sdf input file
c
      common /filescommon/ calib_file_tof, calib_file_lo,
     1    calib_file_hi, calib_file_einctof,
     1    calib_file_geom, ROMboxhistoryfile,
     1    calib_file_track, 
     1    expose_file, mask_file,
     1    configfile, outputfile, outputdir,
     1    scriptfile, file_extent_prefix
c   
c                
cccccccccccccccccccccccccccccc  calibration data cccccccccccccccccccccccccccccc                                      
c  PHA calibration data: 
      integer*4  cal,ntof,nlo,nhi
      Parameter (cal = 100, ntof = 66, nlo = 34, nhi = 93)
      real	     einc_cal(100),tof_cal(100)  ! used to covert tof to energy
      real     tof(3,cal), hi(3,cal), lo(3,cal)
      real	     elo(41,2)       ! lower energy bound for flux bin
      real	     ehi(41,2)	     ! upper energy bound for flux bin 
      real	     effic_low(41,2)	! efficiencies for lower range of each flux bin
      real	     effic_high(41,2)  ! efficiencies for upper range of each flux bin
      real	     effic(41,2)	! efficiencies for each flux bin   
c  Matrix calibration data:
      real     geofactor(2)    ! total geometry factor       
      real     track_effic(41,2)  ! fraction of mass track in ROM box                  
      integer*4   N_cal  ! N elements read into einc_cal and tof_cal
      logical effic_flag(41,2)  ! .true.= ROM box O.K. to use,.false.=don't use this box
c
      common /calcommon/ einc_cal,tof_cal,
     1    tof,hi,lo,elo,ehi,
     1     effic_low,effic_high, effic,
     1     geofactor,track_effic,
     1     N_cal, effic_flag
c
c
cccccccccccccccccccccccccccccc ROM box history cccccccccccccccccccccccccccccc  
c          
      integer*4   ROMbox_time(Ninput_max)        ! beginning time of each ROM box file interval
      integer*4   current_ROMbox_time     ! beginning time of current ROMbox time interval
      character*120   ROMboxfile(Ninput_max)     ! name of ROM box files as a function of time  
      character*120   current_ROMboxfile    ! current name of ROM box file
c
      common /ROM1common/  ROMbox_time,
     1      current_ROMbox_time
      common /ROM2common/  ROMboxfile,
     1              current_ROMboxfile
c  
c               
cccccccccccccccccccccccccccccc script data cccccccccccccccccccccccccccccc
      real    script_minvalue(Nscripts_max,Ncolumns_max)  ! min allowed value of variable if cut is requested
      real    script_maxvalue(Nscripts_max,Ncolumns_max)  ! max allowed value of variable if cut is requested
      integer*4   script_idelta_t(Nscripts_max)   ! accumulate until this time interval reached.
      integer*4   script_idelta_Nr(Nscripts_max)  ! accumulate until this many counts in rate reached
      integer*4   script_idelta_Nh(Nscripts_max)   ! accumulate until this many counts in histogram reached
      integer*4   script_idelta_Nphaf(Nscripts_max)  ! accumulate until this many counts in pha flux array reached
      integer*4   script_accum_t_flag(Nscripts_max)  ! flag to accumulate until time interval reached
      integer*4   script_accum_Nr_flag(Nscripts_max)  ! flag to accumulate until counts in rate reached
      integer*4   script_accum_Nh_flag(Nscripts_max)     ! flag to accumulate until counts in histogram reached
      integer*4   script_accum_Nphaf_flag(Nscripts_max)     ! flag to accumulate until counts in histogram reached
      integer*4   script_Ncolumns(Nscripts_max)      ! number of rows read in from script file            
      integer*4   script_writeflag(Nscripts_max,Ncolumns_max)  ! flag instructing whether to write data
      integer*4   script_cutflag(Nscripts_max,Ncolumns_max)  ! flag instructing whether to cut data       
      integer*4   script_output_type(Nscripts_max)   ! output type  (1 = standard output, 2 = PHA data, 3 = histogram, 4 = PHA fluxes) 
      integer*4   output_sych_flag(Nscripts_max)  ! 0 = output asychronous outputs,1 = only output together when all scripts are ready           
      integer*4   script_stateab_flag(Nscripts_max)   ! accumulates either state A , state B or both
      logical     titlefound_flag(Nscripts_max,Ncolumns_max)   ! flag stating whether title was identified     
c
      common /script1common/ script_minvalue,
     1     script_maxvalue,
     1     script_idelta_t,script_idelta_Nr,
     1     script_idelta_Nh,script_idelta_Nphaf,
     1     script_accum_t_flag,script_accum_Nr_flag,
     1     script_accum_Nh_flag,script_accum_Nphaf_flag,
     1     script_Ncolumns, 
     1     script_writeflag, script_cutflag,
     1     script_output_type,
     1     output_sych_flag,script_stateab_flag,
     1     titlefound_flag         
c strings:
      character*100  scriptheader(Nscripts_max,10)   ! script comment lines
      character*41   script_titles_raw(Nscripts_max,Ncolumns_max)   ! script titles as read
      character*41   script_titles_ratio(Nscripts_max,100) ! script_titles_raw corresponding to ratio indices
      character*20   script_titles(Nscripts_max,Ncolumns_max)   ! script titles
      character*20   script_histtitles(Nscripts_max,Ncolumns_max)   ! script titles
      character*20   script_phafluxtitles(Nscripts_max,Ncolumns_max)   ! script titles
      character*20   script_ratio_titles(Nscripts_max,Ncolumns_max,2)   ! script titles
      character*60    script_file_prefix(Nscripts_max)     ! output file prefix     
      character*4    script_sector_flag(Nscripts_max,Ncolumns_max) ! sector flag ('SECT'= write sectored data,'AVER' = don't) 
      character*20   script_accum_rate_title(Nscripts_max)  ! rate to test accumulated counts
      character*6    script_tel_flag(Nscripts_max,Ncolumns_max)   ! accumulates either tel 1 , tel 2 or both 
      character*20   mdata_key(42)     ! names of ROM boxes
      character*20   kp_key(37)  ! key parameter names
      character*20   disc_key(3)  ! names of discriminator data
      character*20   D_key  ! name of discriminator data
      character*20   STEPkp_key(6)  ! names of STEP key parameters
      character*20   HK_key(6)  ! names of STEP housekeeping data
      character*20   PHA_key(16)  ! names of PHA data
c
      common /script2common/ scriptheader,
     1    script_titles_raw, script_titles_ratio,
     1    script_titles,script_histtitles, 
     1    script_phafluxtitles, script_ratio_titles,
     1    script_file_prefix, 
     1    script_sector_flag, script_accum_rate_title,
     1    script_tel_flag, mdata_key, kp_key,
     1    disc_key, D_key, STEPkp_key, HK_key, 
     1    PHA_key 
c
c
cccccccccccccccccccccccccccccc histogram data cccccccccccccccccccccccccccccc                                        
      real    script_histmin(Nscripts_max,Ncolumns_max)  ! minimum range in histogram
      real    script_histmax(Nscripts_max,Ncolumns_max)  ! max range in histogram
      integer*4   script_Nhist(Nscripts_max,Ncolumns_max)      ! number of bins in histogram
      integer*4   script_logflag(Nscripts_max,Ncolumns_max)    !  histogram log of variable
      integer*4   hist_pha_index(Nscripts_max,Ncolumns_max)   ! used to assign histogram variables
      integer*4   hist_starting_index(Nscripts_max)  ! starting index in hist for each script
      integer*4   script_Nhistcolumns(Nscripts_max)      ! number of hist rows read in from script file
      integer*4   hist_size  ! total number of elements in all histograms
      integer*4   dohistflag    ! 1 if requesting at least 1 histogram, 0 otherwise
      integer*4   submatrixsize(Nscripts_max,Ncolumns_max)  ! used to index histograms into 1-dim array
      integer*4   hist(Nhistbins_max)   ! 1-d histogram
      integer*4   sumhist(Nscripts_max)   ! sum of counts in histogram      
c      
      common /histcommon/ script_histmin,
     1     script_histmax, script_Nhist,
     1     script_logflag, hist_pha_index,
     1     hist_starting_index,script_Nhistcolumns, 
     1     hist_size,dohistflag,  
     1     submatrixsize,     
     1     hist, sumhist
c
c
cccccccccccccccccccccccccccccc  PHA flux data cccccccccccccccccccccccccccccc  
      real  PHA_counts(Nscripts_max,Ncolumns_max,2,2,2)  ! accumulated PHA counts, PHA_counts (bin, tel, stateab, slant,sector)
      real  PHA_counts_sum(Nscripts_max,2,2,2)  ! accumulated PHA counts, PHA_counts_sum(tel, stateab, slant,sector)     
      real  PHA_Ncounts(Nscripts_max,Ncolumns_max)    ! total number of PHA events accumulated  
      real  pha_fluxes(Ncolumns_max)  ! unsectored PHA fluxes
      real  pha_fluxes_err(Ncolumns_max)  ! unsectored PHA flux errors       
      real  script_phaflux_Ebins(Nscripts_max,2,Ncolumns_max)  ! Energy bins read from script for PHA fluxes
      real  script_phaflux_Mbins(Nscripts_max,2,Ncolumns_max)  ! mass bins read from script for PHA fluxes     
      real  phaflux_m(Nscripts_max,Ncolumns_max)  ! mass of PHA fluxes in script
      real  phaflux_z(Nscripts_max,Ncolumns_max)   ! charge of PHA fluxes in script      
      integer*4   PHA_slant_flag(Nscripts_max,Ncolumns_max)  
      integer*4   script_Nphafluxcolumns(Nscripts_max)      ! number of pha flux rows read in from script file        
      integer*4   dophafluxflag   ! doing at least 1 PHA flux script
      integer*4   sumphaflux(Nscripts_max)  ! sum of all counts in PHA flux array
c
      common /phafluxcommon/ 
     1     pha_fluxes, pha_fluxes_err,
     1     script_Nphafluxcolumns, 
     1     script_phaflux_Ebins, script_phaflux_Mbins, 
     1     phaflux_m,phaflux_z,PHA_slant_flag,
     1     dophafluxflag,sumphaflux
c
c
cccccccccccccccccccccccccccccc output and cuts bookkeeping flags cccccccccccccccccccccccccccccc                             
      real        mdata_minvalue(Nscripts_max,42,3) ! min value if cut is requested
      real        mdata_maxvalue(Nscripts_max,42,3) ! max value if cut is requested     
      real        disc_minvalue(Nscripts_max,3) ! min value if cut is requested
      real        disc_maxvalue(Nscripts_max,3) ! max value if cut is requested
      real        D_minvalue(Nscripts_max,3) ! min value if cut is requested
      real        D_maxvalue(Nscripts_max,3) ! max value if cut is requested     
      real        sat_minvalue(Nscripts_max) ! min value if cut is requested
      real        sat_maxvalue(Nscripts_max) ! max value if cut is requested               
      real        kp_minvalue(Nscripts_max,37) ! min value if cut is requested
      real        kp_maxvalue(Nscripts_max,37) ! max value if cut is requested
      real        stepkp_minvalue(Nscripts_max,6) ! min value if cut is requested
      real        stepkp_maxvalue(Nscripts_max,6) ! max value if cut is requested 
      real        hk_minvalue(Nscripts_max,6) ! min value if cut is requested
      real        hk_maxvalue(Nscripts_max,6) ! max value if cut is requested
      real        ratio_minvalue(Nscripts_max,100) ! min value if cut is requested
      real        ratio_maxvalue(Nscripts_max,100) ! max value if cut is requested
      real        pha_minvalue(Nscripts_max,16) ! min value if cut is requested
      real        pha_maxvalue(Nscripts_max,16) ! max value if cut is requested                                
      integer*4   mdata_cutflag(Nscripts_max,42,3)           
      integer*4   disc_cutflag(Nscripts_max,3)
      integer*4   D_cutflag(Nscripts_max,3)         
      integer*4   sat_cutflag(Nscripts_max)        
      integer*4   kp_cutflag(Nscripts_max,37)      
      integer*4   stepkp_cutflag(Nscripts_max,6)       
      integer*4   hk_cutflag(Nscripts_max,6)    
      integer*4   ratio_cutflag(Nscripts_max,100)  
      integer*4   pha_cutflag(Nscripts_max,16)                                     
      integer*4   mdata_ave_writeflag(Nscripts_max,42,3)
      integer*4   mdata_sect_writeflag(Nscripts_max,42,3)             
      integer*4   disc_writeflag(Nscripts_max,3) 
      integer*4   D_writeflag(Nscripts_max,3)         
      integer*4   sat_writeflag(Nscripts_max)         
      integer*4   kp_writeflag(Nscripts_max,37)      
      integer*4   stepkp_writeflag(Nscripts_max,6)       
      integer*4   hk_writeflag(Nscripts_max,6)               
      integer*4   ratio_writeflag(Nscripts_max,100)  
      integer*4   pha_writeflag(Nscripts_max,16)    
      integer*4   ratio_index(Nscripts_max,Ncolumns_max,2)  ! which ROM box to use is calculating ratio
      integer*4   ratio_which(Nscripts_max,Ncolumns_max,2)  ! used with script_ratio_index (1 = ss,2 = sp, 3 = disc)
      integer*4   Nratios(Nscripts_max)   ! number of ratios requested in each script 
      integer*4   ratio_good(100)   ! flag that a good measurement has been made           
c
      common /outputcommon/ 
     1       mdata_minvalue,mdata_maxvalue,
     1       disc_minvalue,disc_maxvalue,
     1       D_minvalue, D_maxvalue,
     1       sat_minvalue, sat_maxvalue,
     1       kp_minvalue, kp_maxvalue,
     1       stepkp_minvalue, stepkp_maxvalue,
     1       hk_minvalue, hk_maxvalue,
     1       ratio_minvalue,ratio_maxvalue,
     1       pha_minvalue, pha_maxvalue,
     1       mdata_cutflag, disc_cutflag,
     1       D_cutflag, sat_cutflag,        
     1       kp_cutflag, stepkp_cutflag,      
     1       hk_cutflag, ratio_cutflag, 
     1       pha_cutflag, mdata_ave_writeflag,
     1       mdata_sect_writeflag, disc_writeflag, 
     1       D_writeflag, sat_writeflag,         
     1       kp_writeflag, stepkp_writeflag,      
     1       hk_writeflag, ratio_writeflag,  
     1       pha_writeflag, ratio_index, 
     1       ratio_which, Nratios, ratio_good     
c
c
cccccccccccccccccccccccccccccc  PHA Data cccccccccccccccccccccccccccccc     
      real 	   nsec(50)     ! tof in nsec
      real         mass(50)    ! mass in AMU
      real         MeV(50)      !  Solid State detector Energy Measurement in MeV
      real         Einc(50)     !  Energy from tof measurement in MeV/nuc 
      real         effic_pha(50)     !  calculated efficiency of STEP for each PHA event     
      integer*4	   epha(50)      ! uncalibrated energy channel number
      integer*4    tpha(50)      ! uncalibrated tof channel number
      integer*4    ramp(50), tel(50), cn(50), 
     1             slant(50), ssd2(50), ab(50),
     1             rom(50), spin(50), sect(50)   ! pha info
      integer*4    PHA_data_flag(50,Nscripts_max)  ! 1= keep PHA event, 0 = don't
      integer*4    PHA_index_good(50,Nscripts_max)  ! index of good PHA events
      integer*4    Npha_good(Nscripts_max)   ! number of good PHA events           
      integer*4    pha_good   ! flag that a good measurement has been made     
      integer*4    sum_cal_mode   ! sum of cal_mode
c
          common /phacommon/  nsec,mass,MeV,Einc,
     1     effic_pha,epha,tpha,
     1     ramp, tel, cn, slant, ssd2, ab, rom, 
     1     spin, sect, PHA_data_flag,
     1     PHA_index_good, Npha_good,
     1     pha_good,    
     1     sum_cal_mode
c      
c
cccccccccccccccccccccccccccccc  rates Data cccccccccccccccccccccccccccccc
      real      mdata_sect(42,3,8)  ! array containing matrix counts, where the indices are the element, 
c                                             the telescope, and the sector, respectively 
      real      mdata_sect_err(42,3,8)
      real      mdata_ave(42,3)
      real      mdata_ave_err(42,3)
      real      disc_ave(3)
      real      disc_ave_err(3)
      real      D_ave(3)
      real      D_ave_err(3)
      real      kp_ave(37)
      real      STEPKP_ave(6) 
      real      hk_ave(6)       
      real      rates_ratio(100)   ! ratio of specified rates
      real      rates_ratio_err(100)  ! error in ratio of specified rates      
      real      mdata_1mf(Nscripts_max,42,2,8)  ! total matrix for this mf
      real      mdata(Nscripts_max,42,2,8)  ! total accumulated matrix
      real      vse1A(Nscripts_max)  ! total accumulated VSE 1 A
      real      vse1B(Nscripts_max)  ! total accumulated VSE 1 B
      real      vse2A(Nscripts_max)  ! total accumulated VSE 2 A
      real      vse2B(Nscripts_max)  ! total accumulated VSE 2 B
      real      exposure(Nscripts_max,42,2)   ! denominator in matrix flux calculations  
      real      exposure_1mf(Nscripts_max,42,2)   ! denominator in matrix flux calculations      
      real      vsebar(Nscripts_max)  ! nonsectored discriminator rates 
      real      start(Nscripts_max)   ! nonsectored discriminator rates 
      real      stop(Nscripts_max)    ! nonsectored discriminator rates 
      real      d2(Nscripts_max), d1(Nscripts_max) ! nonsectored discriminator rates          
      real      m1rate(302)   ! 1x matrix rates, bins 1-290, 651-662 (every mf)
      real      m2rate(84)     !  2x matrix rates  
      real      m4rate(48)     ! 4x matrix rates  
      real      vsebar_1mf, 
     1          start_1mf, 
     1          stop_1mf,
     1          d2_1mf, 
     1          d1_1mf    ! nonsectored discriminator rates for 1mf only
      real      sdrate(32)   ! sectored discriminator rates for 1 mf        
      real      STEP_KP_fluxes_accum(Nscripts_max,6) 
      real      hk_accum(Nscripts_max,6) 
      integer*4   saturation(Nscripts_max)  ! number of events accumulated 
      integer*4   mdata_sect_flag(42,3,8)        
      integer*4   mdata_ave_flag(42,3)
      integer*4   disc_ave_flag(3)
      integer*4   D_ave_flag(3)  
      integer*4   kp_ave_flag(37) 
      integer*4   STEPkp_ave_flag(6)   
      integer*4   Nhk(Nscripts_max)  ! number of HK and STEPkp data acumulated
      integer*4   hk_ave_flag(6)   
      integer*4   vse_master_flag(Nscripts_max)  ! saturation flag for vse1A,vse1B,vse2A,vse2B
      integer*4   mdata_x(42) ! flag listing 1x (=1), 2x (=2) and 4x (=4) matrices
      integer*4   mdata_flag(Nscripts_max,42,2,8)  ! flag indicating if good data is in mdata array                     
c                                                     1 = good data measured, 0 = no measurement, -1 = saturated measurement
      integer*4  mdata_flag_1mf(Nscripts_max,42,2,8)  ! matrix flag for this mf
      integer*4    vsebar_flag(Nscripts_max), 
     1       start_flag(Nscripts_max), 
     1       stop_flag(Nscripts_max),
     1       d2_flag(Nscripts_max), d1_flag(Nscripts_max)    ! nonsectored discriminator rate flags 
c
          common /ratescommon/  mdata_sect,
     1       mdata_sect_err,mdata_ave,
     1       mdata_ave_err,disc_ave,
     1       disc_ave_err, D_ave,
     1       D_ave_err, kp_ave,
     1       STEPKP_ave,hk_ave,      
     1       rates_ratio,rates_ratio_err,  
     1       STEP_KP_fluxes_accum, hk_accum, 
     1       mdata_1mf, mdata, 
     1       vse1A, vse1B, 
     1       vse2A, vse2B,
     1       exposure,
     1       exposure_1mf,      
     1       vsebar, start, 
     1       stop,d2, d1,     
     1       m1rate, m2rate,  
     1       m4rate,  
     1       vsebar_1mf, start_1mf, 
     1       stop_1mf, d2_1mf, d1_1mf,  
     1       sdrate, saturation,
     1       mdata_sect_flag,        
     1       mdata_ave_flag,
     1       disc_ave_flag,
     1       D_ave_flag,kp_ave_flag,
     1       STEPkp_ave_flag, Nhk,
     1       hk_ave_flag,
     1       vse_master_flag,mdata_x,
     1       mdata_flag,                     
     1       mdata_flag_1mf,vsebar_flag,
     1       start_flag, stop_flag,
     1       d2_flag, d1_flag    
c
c
cccccccccccccccccccccc accumulation variables cccccccccccccccccccccc      
      real        ave_spins(2)  ! ave number of spins per mf
c                                        1 = 46 s mf; 2 = 92  s mf
c                                        46 sec mf: 14 spins 61% of the time, 15 spins otherwise
c                                        92 sec mf: 29 spins 58% of the time, 28 spins otherwise 
      real        spin_period     ! S/C spin period in seconds 
                 
      real        time_interval(Nscripts_max)  ! time elapsed (sec) for 1x rates, used to calculate time averaged matrix rates
      real        time_interval_2x(Nscripts_max,2)  ! time elapsed (sec) for 2x rates, phases 0 or 2, and phases 1or 3     
      real        time_interval_4x(Nscripts_max,4)  ! time elapsed (sec) for 4x rates, phase 1, phase 2, phase 3, and phase 4      
      integer*4   start_time_avg(Nscripts_max)   ! start time of time interval over which rates are averaged  
      integer*4   stop_time_avg(Nscripts_max)   ! stop time of time interval over which rates are averaged 
      integer*4   reset_interval(Nscripts_max)   ! 1=start new time interval to average matrix rates, 0 otherwise
      integer*4   Nr_counts(Nscripts_max)  ! number of counts accumulated in specified rate
      integer*4   accum_rate_index(Nscripts_max) ! index that accum_rate_title corresponds to
      integer*4   accum_rate_which(Nscripts_max)  ! which rate is accum_rate_index an index of
      integer*4   Noutput_lines(Nscripts_max)    ! number of output lines written for each script
      integer*4   mf_type          ! type of major frame
      integer*4   phase   ! value determines which bins are used in matrix rates (see below)
      integer*4   nspins          	 ! number of spins in mf
      integer*4	  first_spin	   ! number of first spin in mf
      integer*4	  step_status	! step status         
c      
c                                                 phase 0,2: bins 291-374
c                                                 phase 1,3: bins 375-458 
c                                                 phase 0: bins 459-506
c                                                 phase 1: bins 507-554
c                                                 phase 2: bins 555-602
c                                                 phase 3: bins 603-650 
c
      common /accumccommon/ ave_spins,
     1      spin_period,time_interval,
     1      time_interval_2x,time_interval_4x,     
     1      start_time_avg, stop_time_avg,
     1      reset_interval,Nr_counts,
     1      accum_rate_index, accum_rate_which,
     1      Noutput_lines,mf_type,phase,
     1      nspins,first_spin,step_status
c
c
cccccccccccccccccccccc mask Data cccccccccccccccccccccc                       
      real    starttime_mask(Ninput_max)   ! start time of selected interval to cut
      real    stoptime_mask(Ninput_max)   ! stop time of selected interval to cut
      real    starttime_expose(Ninput_max)   ! start time of selected interval to keep
      real    stoptime_expose(Ninput_max)   ! stop time of selected interval to keep
      integer*4   startyear_mask(Ninput_max)   ! start year of selected interval to cut
      integer*4   stopyear_mask(Ninput_max)   ! stop year of selected interval to cut
      integer*4   mask_interval_flag   ! (>0: process time interval with that #; 0:do not process)
      integer*4   Nmask   ! number of mask time intervals
      integer*4   mask_last_index   ! last mask index searched 
      integer*4   startyear_expose(Ninput_max)   ! start year of selected interval to keep
      integer*4   stopyear_expose(Ninput_max)   ! stop year of selected interval to keep
      integer*4   expose_interval_flag   ! (>0: process time interval with that #; 0:do not process)
      integer*4   Nexpose   ! number of expose time intervals     
c
      common /maskcommon/  starttime_mask,
     1     stoptime_mask, starttime_expose,
     1     stoptime_expose, startyear_mask,
     1     stopyear_mask, mask_interval_flag,
     1     Nmask,  mask_last_index,startyear_expose,
     1     stopyear_expose, expose_interval_flag,
     1     Nexpose
c   
c  
cccccccccccccccccccccc  kp  data cccccccccccccccccccccccccc
      real        Nspha(Nscripts_max)   ! weighted number of spha kp events averaged in time interval
      real        Nmfi(Nscripts_max,2)    ! weighted number of mfi kp events averaged in time interval
      real        Nswe(Nscripts_max)   ! weighted number of swe kp events averaged in time interval
      real        N3dp(Nscripts_max,2)    ! weighted number of 3dp kp events averaged in time interval
      real        Nepa(Nscripts_max,2)    ! weighted number of 3dp kp events averaged in time interval
      real        spin_rate_accum(Nscripts_max)    ! S/C spin rate in rad/sec
      real        Bx_accum(Nscripts_max)    ! x-component of magnetc field (GSE)
      real        By_accum(Nscripts_max)    ! y-component of magnetc field (GSE)
      real        Bz_accum(Nscripts_max)    ! z-component of magnetc field (GSE)
      real        Brms_accum(Nscripts_max)    ! rms of B field
      real        Bfield_accum(Nscripts_max)   ! magnitude of magnetic field                 
      real        SCx_accum(Nscripts_max)  ! x GSE position of S/C (Re)
      real        SCy_accum(Nscripts_max)  ! y GSE position of S/C (Re)
      real        SCz_accum(Nscripts_max)   ! z GSE position of S/C (Re)     
      real        SWVx_accum(Nscripts_max)   ! x-component solar wind velocity (km/sec)
      real        SWVy_accum(Nscripts_max)   ! y-component solar wind velocity (km/sec)
      real        SWVz_accum(Nscripts_max)   ! z-component solar wind velocity (km/sec)
      real        Nproton_accum(Nscripts_max)  ! proton density - #/cc
      real        SWVth_accum(Nscripts_max)     ! Solar Wind thermal speed
      real        eflux_accum(Nscripts_max,7)   !  3dp electron flux
      real        ionflux_accum(Nscripts_max,7)   !  3dp ion flux
      real        J_apeb1_accum(Nscripts_max),
     1            J_apeb2_accum(Nscripts_max),   
     1            J_apeb3_accum(Nscripts_max), 
     1            J_apeb4_accum(Nscripts_max), 
     1            J_apeb5_accum(Nscripts_max)    ! EPACT fluxes
      real        J_lemt1_accum(Nscripts_max), 
     1            J_lemt2_accum(Nscripts_max), 
     1            J_lemt3_accum(Nscripts_max)   ! EPACT fluxes
      logical   spha_flag(Nscripts_max)  ! swe kp data read at all
      logical   mfi_flag(Nscripts_max,2)  ! swe kp data read at all
      logical   swe_flag(Nscripts_max)  ! swe kp data read at all
      logical   tdp_flag(Nscripts_max,2)  ! 3dp kp data read at all
      logical   epa_flag(Nscripts_max,2)  ! epact kp data read at all
      logical   spha_flag_1mf  ! swe kp data read in current packet
      logical   mfi_flag_1mf  ! swe kp data read in current packet
      logical   swe_flag_1mf  ! swe kp data read in current packet
      logical   tdp_flag_1mf  ! 3dp kp data read in current packet
      logical   epa_flag_1mf  ! epact kp data read in current packet
c
      common /kpcommon/  Nspha, Nmfi, 
     1      Nswe, N3dp, Nepa,
     1      spin_rate_accum, Bx_accum,
     1      By_accum, Bz_accum, Brms_accum,
     1      Bfield_accum,SCx_accum, 
     1      SCy_accum,SCz_accum, SWVx_accum, 
     1      SWVy_accum,SWVz_accum,Nproton_accum,
     1      SWVth_accum,
     1      eflux_accum,ionflux_accum,
     1      J_apeb1_accum,J_apeb2_accum,J_apeb3_accum,
     1      J_apeb4_accum,J_apeb5_accum,J_lemt1_accum,
     1      J_lemt2_accum, J_lemt3_accum,
     1      spha_flag,mfi_flag,swe_flag,tdp_flag,
     1      epa_flag, 
     1      spha_flag_1mf,mfi_flag_1mf,swe_flag_1mf,
     1      tdp_flag_1mf,epa_flag_1mf
c








