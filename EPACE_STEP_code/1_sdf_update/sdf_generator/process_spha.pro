	pro process_spha, index
;	reads spha data off kp_filename(index) and write out sdf file name

	common luns
	common times
	common spha
	
;	if first call, just read first record and get time and variables
	if( cdf_recnum(index) eq 0 ) then read_spha, index

;	if we're read all the records in this file then return
	if(cdf_recnum(index) ge cdf_maxrecs(index) ) then return

;	if current cdf time is earlier than current step record, read another cdf record
	while (( cdf_time(index) lt mjd_start) and (cdf_recnum(index) le cdf_maxrecs(index)) ) do read_spha, index

;	if current cdf time is later than current step record, just return
	if( cdf_time(index) gt mjd_stop) then return

;	if current cdf file data falls in between start and stop times of step record, then write it out	
	if( (cdf_time(index) ge mjd_start) and (cdf_time(index) le mjd_stop) ) then begin
		printf, lun_sdf, index + 2, format="(i8)"
		printf, lun_sdf, spha_asr, format="(e12.4)"
	endif
;		if(cdf_recnum(index) eq 938 ) then print, cdf_recnum(index), cdf_time(index), mjd_start, mjd_stop, mjd_start-cdf_time(index), index

	return
	end

