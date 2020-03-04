	pro	open_files, year, doy

;
;	modification history
;		25-Jun-2008		modify paths for new directory structure	
;		19-Nov-2008		move L0 data file folder to hd1 -- modify paths /gm
;		7-Jan-2009		allow missing cdf files -- produce message /gm

	common luns

;	opens input files and output sdf file (new)

;	forms for file addresses:  -- note, sometimes have v02 files
;	/Users/masongm1/Data_L0_flight/STEP/step_hex/step08_01_02.hex 
;	/Users/masongm1/Data_L0_flight/STEP/step_kp_files/spha/wi_k0_spha_20080501_v01.cdf 
;	/Users/masongm1/Data_L0_flight/STEP/step_kp_files/mfi/wi_k0_mfi_20080501_v01.cdf 
;	/Users/masongm1/Data_L0_flight/STEP/step_kp_files/swe/wi_k0_swe_20080501_v01.cdf 
;	/Users/masongm1/Data_L0_flight/STEP/step_kp_files/3dp/wi_k0_3dp_20080501_v01.cdf 
;	/Users/masongm1/Data_L0_flight/STEP/step_kp_files/epa/wi_k0_epa_20080501_v01.cdf

;	find the day of year
	timeCDS = { MJD: 0L , TIME: 0L }
	mjdfract = date2mjdfract(year, doy)
	timecds.mjd = ulong(mjdfract)
	mjd2date, mjdfract, yearmjd, month, day


	
	hex_filename = '/Users/mdesai/Desktop/IDL/wind/step_kp_files/step/step' + string(year mod 100, month, day,   $
		format='(i2.2,"_",i2.2,"_",i2.2,".hex")') 
	openr, lun_hex, hex_filename, /get_lun
	 
;	get the path to the STEP output directory   -- 4-Aug-2008 /gm
	openr, lun_paths, '/paths/sdf_lister_paths.txt', /get_lun
	sdf_path = ''
	for i = 1, 4 do readf, lun_paths, sdf_path    ; sdf_path is on the 4th line of this file
	free_lun, lun_paths
	 
	sdf_filename =  trim(sdf_path) + 'sdf' + string(year, doy, format="(i4,'_',i3.3,'.hex;1')")
	print, ' opening new sdf file: ', sdf_filename
	openw, lun_sdf, sdf_filename, /get_lun

	for i=0,4 do begin
		lun_exist(i) = 1   ; default case is that cdf file exists
	case i of
0:		type ='spha'
1:		type ='mfi'
2:		type ='swe'
3:		type ='3dp'
4:		type ='epa'
	endcase
		for j = 3, 1, -1 do begin
		kp_filename = '/Users/mdesai/Desktop/IDL/wind/step_kp_files/' + type + $
			'/wi_k0_' + type + string(year, month, day, j, format="('_',i4,2i2.2,'_v0',i1,'.cdf')")
;		print, kp_filename
;		see if this file exists by opening in idl:
		openr, lun, kp_filename, error = err, /get_lun
;		print, err
			if( err eq 0 ) then break
		endfor   ; end of j loop for version number
		
			free_lun, lun
				if(j gt 0) then begin
					lun_k0(i) = cdf_open(kp_filename)
				endif else begin
					print, 'did not find: ', kp_filename
					openw, lun_missing, '/Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/cdf_missing_days.txt', /get_lun, /append
					printf, lun_missing, 'did not find: ', kp_filename
					lun_exist(i) = 0
					free_lun, lun_missing
				endelse
	case i of
0:		var_num = 6
1:		var_num = 8
2:		var_num = 18
3:		var_num = 18
4:		var_num = 6
	endcase

	if( lun_exist(i) ) then begin
;		get the maximum number of records in this file
		if(i eq 1) then cdf_control, lun_k0(i), get_var_info=info, variable=var_num, /zvariable $, 
			else cdf_control, lun_k0(i), get_var_info=info, variable=var_num
			cdf_maxrecs(i) = info.maxrecs
;			print, i, cdf_maxrecs(i)
	endif
	
	endfor  ; end of i loop for k0 file opening
	end