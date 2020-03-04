c
c      l0_include.inc
c      include file for L0_to_sdfkp programs
c      3/3/97
c          
c
c**************************************************
c 
      integer	      start_time_last	 ! last start time of mf (sampex time format)
      integer	      end_time_last	    ! last end time of mf  (sampex time format)  
      integer    PHA_index(50)    ! index number of PHA event 
      integer*4  spha_time, ! epoch time given in the cdf record that
     1        mfi_time,  ! the data was taken from -
     1        swe_time,   ! converted to sampextime
     1        tdp_time,
     1        epa_time
      integer*4  spha_id, mfi_id, swe_id, 
     1               tdp_id, epa_id
      integer*4  spha_rec, mfi_rec, swe_rec, 
     1               tdp_rec, epa_rec
      integer*4  spha_max_rec, mfi_max_rec, 
     1          swe_max_rec, tdp_max_rec, epa_max_rec    
      logical  spha_open, mfi_open, swe_open,
     1           tdp_open, epa_open
      logical  spha_read, mfi_read, swe_read, 
     1          tdp_read, epa_read
      integer*4  vn_spha_epoch,
     1            vn_spha_asr
      integer*4   vn_mfi_epoch,
     1            vn_mfi_bgse,
     1            vn_mfi_rms,
     1            vn_mfi_position
      integer*4   vn_swe_epoch,
     1            vn_swe_vgse,
     1            vn_swe_vth,
     1            vn_swe_protdens
      integer*4  vn_tdp_epoch,
     1            vn_tdp_eflux,
     1            vn_tdp_ionflux
      integer*4  vn_epa_epoch,
     1            vn_epa_apeb1,
     1            vn_epa_apeb2,
     1            vn_epa_apeb3,
     1            vn_epa_apeb4,
     1            vn_epa_apeb5,
     1            vn_epa_lemt1,
     1            vn_epa_lemt2,
     1            vn_epa_lemt3
c
      common /cdfdat1/  start_time_last,
     1            end_time_last, PHA_index, 
     1            spha_time,
     1            mfi_time, swe_time, tdp_time,
     1            epa_time,
     1            spha_id, mfi_id, swe_id, tdp_id,
     1            epa_id,
     1            spha_max_rec, mfi_max_rec, 
     1            swe_max_rec, tdp_max_rec,
     1            epa_max_rec,
     1            spha_rec, mfi_rec, swe_rec, tdp_rec, 
     1            epa_rec,
     1            vn_spha_epoch,
     1            vn_spha_asr,
     1            vn_mfi_epoch,
     1            vn_mfi_bgse,
     1            vn_mfi_rms,
     1            vn_mfi_position,
     1            vn_swe_epoch,
     1            vn_swe_vgse,
     1            vn_swe_vth,
     1            vn_swe_protdens,
     1            vn_tdp_epoch,
     1            vn_tdp_eflux,
     1            vn_tdp_ionflux,
     1            vn_epa_epoch,
     1            vn_epa_apeb1,
     1            vn_epa_apeb2,
     1            vn_epa_apeb3,
     1            vn_epa_apeb4,
     1            vn_epa_apeb5,
     1            vn_epa_lemt1,
     1            vn_epa_lemt2,
     1            vn_epa_lemt3,
     1            spha_open, 
     1            mfi_open, swe_open, tdp_open,
     1            epa_open, spha_read, 
     1            mfi_read, swe_read, tdp_read,
     1            epa_read
c
      real*4       spha_asr_1mf,     
     1            mfi_bgse_1mf(3),
     1            mfi_rms_1mf,
     1            mfi_position_1mf(3),
     1            swe_vgse_1mf(3),
     1            swe_vth_1mf,
     1            swe_protdens_1mf,
     1            tdp_eflux_1mf(7),
     1            tdp_ionflux_1mf(7),
     1            epa_apeb1_1mf,
     1            epa_apeb2_1mf,
     1            epa_apeb3_1mf,
     1            epa_apeb4_1mf,
     1            epa_apeb5_1mf,
     1            epa_lemt1_1mf,
     1            epa_lemt2_1mf,
     1            epa_lemt3_1mf
c
      common /kpdata1mf/  spha_asr_1mf, 
     1            mfi_bgse_1mf, 
     1            mfi_rms_1mf,
     1            mfi_position_1mf,
     1            swe_vgse_1mf,
     1            swe_vth_1mf, 
     1            swe_protdens_1mf,
     1            tdp_eflux_1mf,
     1            tdp_ionflux_1mf,
     1            epa_apeb1_1mf,
     1            epa_apeb2_1mf,
     1            epa_apeb3_1mf,
     1            epa_apeb4_1mf,
     1            epa_apeb5_1mf,
     1            epa_lemt1_1mf,
     1            epa_lemt2_1mf,
     1            epa_lemt3_1mf


