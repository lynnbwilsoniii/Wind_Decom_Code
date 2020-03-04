c
c    include file for L0_to_sdfkp and sdf_lister
c    contains type declarations
c    2/12/97 by J. Dwyer
c
c                  ***********************************************
      
      real     STEPtherm    ! housekeeping data      
      real     STEPt1          ! housekeeping data      
      real     STEPt2          ! housekeeping data          
      real     STEPt3          ! housekeeping data      
      real     STEPHV         ! housekeeping data       
      integer*4   MFnum          ! major frame number (ELITE)	     
      real STEP_KP_fluxes(6)           ! Fluxes for He3, He5, CNO3, CNO5, Fe3, Fe6
c                                                      CDHF Key Parameters (Kristin's calculation) 
      byte      packet(733)  ! raw data packet data
      byte      NPHA    ! number of nonzero PHA events in a packet (50 maximum)     
c       
      character*24	start_time_string	!  string containing time and date of start of mf
      integer*4	      start_time	 ! start time of mf (sampex time format)
      integer*4	      end_time	    ! end time of mf  (sampex time format)    
c
      common /raw/  start_time, end_time, 
     1      STEPtherm,STEPt1,STEPt2,STEPt3,STEPHV,
     1      MFnum,STEP_KP_fluxes, packet, NPHA
c
      common /rawstr/ start_time_string
c
      real      spha_asr,
     1            mfi_bgse(3),
     1            mfi_rms,
     1            mfi_position(3),
     1            swe_vgse(3),
     1            swe_vth,
     1            swe_protdens,
     1            tdp_eflux(7),
     1            tdp_ionflux(7),
     1            epa_apeb1,
     1            epa_apeb2,
     1            epa_apeb3,
     1            epa_apeb4,
     1            epa_apeb5,
     1            epa_lemt1,
     1            epa_lemt2,
     1            epa_lemt3
c
      common /kpdata/  spha_asr, 
     1            mfi_bgse, 
     1            mfi_rms,
     1            mfi_position,
     1            swe_vgse,
     1            swe_vth, 
     1            swe_protdens,
     1            tdp_eflux,
     1            tdp_ionflux,
     1            epa_apeb1,
     1            epa_apeb2,
     1            epa_apeb3,
     1            epa_apeb4,
     1            epa_apeb5,
     1            epa_lemt1,
     1            epa_lemt2,
     1            epa_lemt3


