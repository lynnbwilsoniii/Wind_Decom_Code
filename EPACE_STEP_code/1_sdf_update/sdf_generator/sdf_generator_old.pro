	pro	sdf_generator, batch_flag
;
;	routine to read STEP hex files from GSFC/Kristin and merge with KP file data
;	
;	Modification history:
;		13-Jun-2008		original version /gm
;		26-Jun-2008		modify for batch jobs;  batch_flag =1 for batch, otherwise leave out
;		7-Jan-2009 		modify to handle missing cdf files /gm
;
; ************** note from Joe Dwyer's code L0_tosddfkp.for ********************
;	4/13/99 note: at some point the KP times should be corrected to reflect the fact that the
;     STEP start and end times are 1 mf after the collection times. SDF_lister corrects the
;     STEP data to give the correct time, but this makes the KP times wrong by 1 mf.
;	see SDF_LISTER code:   sdf_subrout2.f95, function get_data_record    /gm   6/17/2008
; *******************************************************************************
;
;
	if(n_elements(batch_flag) eq 0 ) then batch_flag = 0 else batch_flag = 1

	@sdf_generator.inc
;
;
		timeCDS = { MJD: 0L , TIME: 0L }
		end_flag =  4294967295
		instring=''
		run_stop_mjd = 0L

;		now find the last day processed
		openr, lun_out, '/Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/last_day_sdf_generator.txt', /get_lun
		readf, lun_out, year_proc, doy_proc
		free_lun, lun_out


;		find the year and day range to process 
		if(batch_flag eq 0) then begin
			read, lun_cfg, year_start, doy_start, year_stop, doy_stop, prompt='Enter year_start, doy_start, year_stop, doy_stop :'
			run_start_mjd = date2mjdfract(year_start, doy_start)
			run_stop_mjd = date2mjdfract(year_stop, doy_stop)
		endif else begin
;			if batch job, start date = last day processed + 1;  end date = last hex file date
				run_start_mjd = date2mjdfract(year_proc, doy_proc)	+ 1
			openr, lun_files, '/Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/step_hex_files.txt', /get_lun
			while( eof(lun_files) ne 1 ) do begin
					readf, lun_files, instring
					loc = strpos(strlowcase(instring),'.hex')
				if( loc gt 0 ) then begin
					reads, strmid(instring,loc-8,2), year
						if(year gt 90 ) then year += 1900 else year += 2000 
					reads, strmid(instring,loc-5,2), month
					reads, strmid(instring,loc-2,2), day
					mjd= date2mjd(year, month, day)
						if( mjd gt run_stop_mjd ) then run_stop_mjd = mjd
				endif
			endwhile
		endelse
		
		if( run_stop_mjd le run_start_mjd) then begin
			print, ' sdf_generator:  stop date does not exceed start date; quitting early '
			exit, status = 2
		endif

;	open output file for list of dates processed  -- commented out 7/28/08 /gm
;		openw, lun_txt, '/Users/masongm1/Data/Production/STEP/1_sdf_update/sdf_generator/sdf_generator.txt', /get_lun


		
		for mjd = 	run_start_mjd, run_stop_mjd do begin
			timecds.mjd = mjd
			year = mjd2yr(timecds)
			doy = mjd2doy(timecds)
		
;		for doy =  day_start, day_end do begin
;		for doy = 124, 124 do begin

;		open the input hex file and KP files;  open the output sdf file;  max_recs_hex is number of records in the hex file
		open_files, year, doy


;			process a STEP hex record
nextrec:		process_step, eof_flag,eor_flag
				if( eof_flag ) then goto, close_files		

;			now merge in the KP data for each id type -- if there is a cdf file for it
			if( lun_exist(0) ) then process_spha, 0
			if( lun_exist(1) ) then process_mfi, 1
			if( lun_exist(2) ) then process_swe, 2
			if( lun_exist(3) ) then process_3dp, 3
			if( lun_exist(4) ) then process_epa, 4
;		end of record flag
			if( eor_flag ) then printf, lun_sdf, end_flag, format="(z8)"
		goto, nextrec
		
;		close all files
close_files:		free_lun, lun_hex
		free_lun, lun_sdf
		for i = 0,4 do if( lun_exist(i)) then cdf_close, lun_k0(i)
;		reset cdf flags	
		cdf_recnum = ulonarr(5)
		cdf_time = dblarr(5)		
		cdf_maxrecs = ulonarr(5)
		mjd_last_rec = 0.d0
		print, year, doy, format="('sdf generator finished ',i4,' day ',i3)"
;		printf, lun_txt, year, doy, format="('sdf generator finished ',i4,' day ',i3)"
		endfor   ; end of loop for days

;	now write out the last day processed
		openw, lun_out, '/Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/last_day_sdf_generator.txt', /get_lun
		printf, lun_out, year, doy, format="(2i5, '   last day processed by sdf_generator')"
		free_lun, lun_out
;		free_lun, lun_txt
;

		end
		

		
		