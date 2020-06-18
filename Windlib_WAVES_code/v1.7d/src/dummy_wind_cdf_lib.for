! dummy_wind_cdf_lib.for - entry points to resolve CDF references on platforms
! without CDF.
!
! On VMS systems, also used to 
!
	subroutine cdflib()
	return
	end

	subroutine cdf_open()
	type *, 'ERROR!  CDF_OPEN dummy entry point.'
	return
	end

	subroutine cdf_close()
	type *, 'ERROR!  CDF_CLOSE dummy entry point.'
	return
	end

	subroutine cdf_var_get()
	type *, 'ERROR!  CDF_VAR_GET dummy entry point.'
	return
	end

	subroutine cdf_inquire()
	type *, 'ERROR!  CDF_INQUIRE dummy entry point.'
	return
	end

	subroutine cdf_var_inquire()
	type *, 'ERROR!  CDF_VAR_INQUIRE dummy entry point.'
	return
	end

	subroutine cdf_error()
	type *, 'ERROR!  CDF_ERROR dummy entry point.'
	return
	end

	integer*4	function	cdf_var_num()
	type *, 'ERROR!  CDF_VAR_GET dummy entry point.'
	cdf_var_num = 0
	return
	end

	integer*4	function	dummy_cdf_fun()
!	entry		CDFlib
	entry		CDFattrInquire
	entry		CDFattrEntryInquire
	entry		CDFattrGet
	entry		CDFattrPut
	entry		CDFvarNum
	entry		CDFattrNum
	entry		CDF_lib
	entry		CDF_create
!	entry		CDF_open
!	entry		CDF_close
	entry		CDF_delete
!	entry		CDF_inquire
!	entry		CDF_error
	entry		CDF_doc
	entry		CDF_var_create
!	entry		CDF_var_inquire
!	entry		CDF_var_get
	entry		CDF_var_put
	entry		CDF_var_hyper_get
	entry		CDF_var_hyper_put
	entry		CDF_var_close
	entry		CDF_var_rename
!	entry		CDF_var_num
	entry		CDF_attr_create
	entry		CDF_attr_inquire
	entry		CDF_attr_entry_inquire
	entry		CDF_attr_get
	entry		CDF_attr_put
	entry		CDF_attr_rename
	entry		CDF_attr_num
	entry		EPOCHbreakdown
	entry		computeEPOCH
	entry		parseEPOCH
	entry		encodeEPOCH
	entry		encodeEPOCH1
	entry		encodeEPOCH2
	entry		encodeEPOCH3
	entry		EPOCH_breakdown
	entry		compute_EPOCH
	entry		parse_EPOCH
	entry		encode_EPOCH
	entry		encode_EPOCH1
	entry		encode_EPOCH2
	entry		encode_EPOCH3
	entry		CDF_lib_4
	entry		CDF_lib_5
	entry		CDF_lib_6
	entry		CDF_lib_7
	entry		CDF_lib_8
	entry		CDF_lib_9
	entry		CDF_lib_10
	entry		CDF_lib_11
	entry		CDF_lib_12
	entry		CDF_lib_13
	entry		CDF_lib_14
	entry		CDF_lib_15
	entry		CDF_lib_16
	entry		CDF_lib_17
	entry		CDF_lib_18
	entry		CDF_lib_19
	entry		CDF_lib_20
	entry		CDF_lib_21
	entry		CDF_lib_22
	entry		CDF_lib_23
	entry		CDF_lib_24
	entry		CDF_lib_25
	dummy_cdf_fun = 0
	return
	end
