	pro read_epa, index
;	reads the next epa record
	common luns
	common times
	common epa
		cdf_varget, lun_k0(index), 1, time, rec_start=cdf_recnum(index)
			pb5_year = time(0)
			pb5_day = time(1)
			pb5_msec = time(2)
		cdf_time(index) = date2mjdfract(pb5_year, pb5_day) + floor(pb5_msec/1000.d0)/86400.d0
			cdf_varget,lun_k0(index), 6, var6,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 7, var7,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 8, var8,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 9, var9,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 2, var2,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 3, var3,  rec_start=cdf_recnum(index)
			cdf_varget,lun_k0(index), 4, var4,  rec_start=cdf_recnum(index)
			epa_apeb2=var6(0)
			epa_apeb3=var7(0)
			epa_apeb4=var8(0)
			epa_apeb5=var9(0)
			epa_lemt1=var2(0)
			epa_lemt2=var3(0)
			epa_lemt3=var4(0)
		cdf_recnum(index) ++
	end
