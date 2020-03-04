	pro read_swe, index
;	reads the next swe record
	common luns
	common times
	common swe
		cdf_varget, lun_k0(index), 2, time, rec_start=cdf_recnum(index)    ; note pb5_time is var #2 in swe files
		pb5_year = time(0)
		pb5_day = time(1)
		pb5_msec = time(2)	
		cdf_time(index) = date2mjdfract(pb5_year, pb5_day) + floor(pb5_msec/1000.d0)/86400.d0
			cdf_varget,lun_k0(index), 18, var18,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 25, var25,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 26, var26,  rec_start=cdf_recnum(index)
			swe_vgse = var18
			swe_vth = var25(0)
			swe_prodens = var26(0)
		cdf_recnum(index) ++
	end
