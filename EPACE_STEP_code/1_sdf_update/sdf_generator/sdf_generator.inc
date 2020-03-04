;	sdf_generator include file

	common times, mjd_start, mjd_stop, mjd_last_rec, cdf_time
	mjd_start = 0.d0
	mjd_stop = 0.d0
	mjd_last_rec = 0.d0
	cdf_time = dblarr(5)
	
	common luns, lun_hex, lun_k0, lun_sdf, cdf_recnum, cdf_maxrecs, step_hexrecs, lun_exist
		lun_k0 = ulonarr(5)
		lun_hex = 0L
		lun_sdf = 0L
		cdf_recnum = ulonarr(5)
		cdf_maxrecs  = ulonarr(5)
		step_hexrecs= 0L
		lun_exist = intarr(5)
		
	common spha, spha_asr
		spha_asr = 0.
		
	common mfi,  mfi_bgse, mfi_rms, mfi_position
		mfi_bgse = fltarr(3)
		mfi_rms = 0.
		mfi_position = fltarr(3)
		
	common swe,  swe_vgse, swe_vth, swe_prodens
		swe_vgse = fltarr(3)
		swe_vth = 0.
		swe_prodens = 0.
		
	common tdp, tdp_eflux, tdp_ionflux
		tdp_eflux = fltarr(7)
		tdp_ionflux = fltarr(7)
	
	common epa, epa_apeb2, epa_apeb3, epa_apeb4, epa_apeb5, epa_lemt1, epa_lemt2, epa_lemt3
		epa_apeb2=0.
		epa_apeb3=0.
		epa_apeb4=0.
		epa_apeb5=0.
		epa_lemt1=0.
		epa_lemt2=0.
		epa_lemt3=0.
		
