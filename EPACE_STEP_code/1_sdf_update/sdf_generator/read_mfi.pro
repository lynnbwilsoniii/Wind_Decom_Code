	pro read_mfi, index
;	reads the next mfi record
	common luns
	common times
	common mfi
		cdf_varget, lun_k0(index), 1, time, rec_start=cdf_recnum(index), /zvariable
		pb5_year = time(0)
		pb5_day = time(1)
		pb5_msec = time(2)	
;	drop fractions of second to preserve compatibility with Vax version of program
		cdf_time(index) = date2mjdfract(pb5_year, pb5_day) + floor(pb5_msec/1000.d0)/86400.d0
		cdf_varget, lun_k0(index), 8, mfi_bgse, rec_start=cdf_recnum(index), /zvariable
		cdf_varget, lun_k0(index), 5, mfi_rms, rec_start=cdf_recnum(index), /zvariable
		cdf_varget, lun_k0(index),12, mfi_position, rec_start=cdf_recnum(index), /zvariable
		cdf_recnum(index) ++
	end
