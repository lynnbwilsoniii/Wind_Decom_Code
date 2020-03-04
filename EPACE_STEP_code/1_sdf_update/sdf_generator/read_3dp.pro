	pro read_3dp, index
;	reads the next 3dp record
	common luns
	common times
	common tdp
		cdf_varget, lun_k0(index), 1, time, rec_start=cdf_recnum(index)
			pb5_year = time(0,0,0)
			pb5_day = time(0,0,1)
			pb5_msec = time(0,0,2)
		cdf_time(index) = date2mjdfract(pb5_year, pb5_day) + floor(pb5_msec/1000.d0)/86400.d0
			cdf_varget,lun_k0(index), 12, var12,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 22, var22,  rec_start=cdf_recnum(index)
			for i=0,6 do tdp_eflux(i)= var12(1,i)
			for i=0,6 do tdp_ionflux(i) = var22(i,1)
		cdf_recnum(index) ++
	end
	
