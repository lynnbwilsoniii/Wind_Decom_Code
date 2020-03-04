	PRO	merge_mask_saturation
;
;	routine to merge the STEP mask_saturation.000 file with the latest update
;	file (mask_saturation.tmp)
;
;	Modification history:
;		7-May-2008	initial version /gm
;		25-Jun-2008	update paths for new directory structure /gm
;
;
;
;
	path_saturation = '/Users/mdesai/Desktop/IDL/wind/fortran_vax/sdf_lister_control/'
	path_update = '/Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/'
	
;	search for and open the mask_saturation.000 file

	for ivers = 300,1,-1 do begin
		file_name=path_saturation + 'MASK_SATURATION.000' + string(ivers, format='(";",i0)')
;		print, file_name
		openr, lun_saturation, file_name,  error=err, /get_lun
;		print, ' error variable: ', err
		if(err eq 0) then break
	endfor
;	print, ' break with ivers = ', ivers
	close, lun_saturation
	openw, lun_saturation, file_name,  /append, error=err, /get_lun
	
;	search for and open the mask_saturation.tmp file	
	for ivers_tmp = 300,1,-1 do begin
		file_name_tmp=path_update + 'MASK_SATURATION.tmp' + string(ivers_tmp, format='(";",i0)')
;		print, file_name
		openr, lun_saturation_tmp, file_name_tmp,  error=err, /get_lun
;		print, ' error variable: ', err
		if(err eq 0) then break
	endfor
;	print, ' break with ivers_tmp = ', ivers_tmp
		close, lun_saturation_tmp
		
	openr, lun_saturation_tmp, file_name_tmp,  error=err, /get_lun
;	space over the 10 header lines

	a=''
	for iline = 1,10 do begin
;		print, iline
		readf, lun_saturation_tmp, a
	endfor

	input_line = ''
	n_lines=0L
	while( not eof(lun_saturation_tmp) ) do begin
		readf,  lun_saturation_tmp, input_line
;		print, input_line
		printf, lun_saturation, input_line
		n_lines ++
	endwhile
	print, ' merge mask saturation is finished'
	openw, lun_out, '/Users/mdesai/Desktop/IDL/wind/fortran_vax/3_merge_mask_sat/merge_mask_saturation.txt', /get_lun
	printf, lun_out,  ' merge mask saturation is finished ', n_lines, ' new lines written out'
	printf, lun_out,  ' new file written is: ', file_name
	free_lun, lun_out
	free_lun, lun_saturation
	free_lun, lun_saturation_tmp
	close, /all
end