	pro read_spha, index
;	reads the next s_pha record
	common luns
	common times
	common spha
		cdf_varget, lun_k0(index), 1, time, rec_start=cdf_recnum(index)
		pb5_year = time(0)
		pb5_day = time(1)
		pb5_msec = time(2)	
		cdf_time(index) = date2mjdfract(pb5_year, pb5_day) + floor(pb5_msec/1000.d0)/86400.d0
		cdf_varget, lun_k0(index), 6, var6, rec_start=cdf_recnum(index)
		spha_asr = var6(0)
		cdf_recnum(index) ++
	end
