	pro process_step, eof_flag, eor_flag

;	routine to process a single STEP packet ---
;		reads the input hex file and writes out SDF data from it
;		writes are put after all the reads in order to handle files with input i/o error due
;			to missing packets  (e.g., see 2008/05/03 09:10:16 -- packet data missing)
;		format line 33 removed 12/1/2011 since case of mf=0 gave end of line error on Nov 8, 2011 file   /gm
	common luns
	common times

	eof_flag = 0
	eor_flag = 1   ; set to true
	char = ''
	if( eof(lun_hex) ) then goto, end_file

nextrec: 	readf, lun_hex, char    ;  skip the first line
		step_hexrecs ++
	if( eof(lun_hex) ) then goto, end_file

;	second line contains times
	readf, lun_hex, start_y, start_m, start_d, start_h, start_min, start_s, stop_y, stop_m, stop_d, stop_h, stop_min, stop_s, $
		format="(i3, x, i2, x, i2, i3, x, i2, x,  i2, i4, x, i2, x, i2, i3, x, i2, x,  i2)" 
	if(start_y gt 90) then start_y += 1900 else start_y += 2000
	if(stop_y gt 90) then stop_y += 1900 else stop_y += 2000
	mjd_start = date2mjdfract(start_y, start_m, start_d, start_h, start_min) + start_s/86400.d0
	start_sampextime = round(86400.d0*(mjd_start-48622.d0))
	mjd_stop = date2mjdfract(stop_y, stop_m, stop_d, stop_h, stop_min) + stop_s/86400.d0
	stop_sampextime = round(86400.d0*(mjd_stop-48622.d0))	
;	print, start_sampextime, stop_sampextime, format="(2e25.15)"
;	print, start_sampextime, stop_sampextime, format="(2z12)"
;	print,  start_y, start_m, start_d, start_h, start_min, start_s	
;	line 2 analogs and MF number:  old line:readf, lun_hex, STEPHV, STEPtherm, STEPt1, STEPt2, STEPt3, MFnum, format="(2f12.6,3f10.6,i6)"
	readf, lun_hex, STEPHV, STEPtherm, STEPt1, STEPt2, STEPt3, MFnum     ; format removed Dec 1, 2011 /gm
	
;	read in the matrix counts and phas:
	packet = uintarr(733)
	packetline = uintarr(16)
	index=0
	for i = 0, 44 do begin

			on_ioerror, nextrec    ; trap for missing packets
			readf, lun_hex, packetline, format="(16z3)"
			for j = 0, 15 do packet(j + index) = packetline(j)	
			index += 16

	endfor  ; i loop
; 	get the last line
		packetline = uintarr(13)
		readf, lun_hex, packetline, format="(13z3)"
		for k = 0, 12 do packet(k + index) = packetline(k)	


;	now do the phas;  first count the number of non-zero pha events and write it out
	npha = 0U
	for index = 482, 727, 5 do if total(packet(index:index+4) gt 0 ) then npha ++
	
;	now read out the STEP kp data and write it out
	stepkp = fltarr(6)
	readf, lun_hex, stepkp


;	********* write out the data **********
;	if start time is repeated (happened at end of 2008/122) don't write out record
	if(mjd_start eq 	mjd_last_rec ) then begin
		eor_flag = 0    ; set to false
		return
	endif
	mjd_last_rec = mjd_start

;	write out sdf id= 1, and 2nd line containing times
	if( step_hexrecs gt 1 ) then printf, lun_sdf, 1, format="(i8)" else printf, lun_sdf, 1, format="(i8, ' file created by sdf_generator')"
	printf, lun_sdf, start_y, start_m, start_d, start_h, start_min, start_s, start_sampextime, stop_sampextime, $
	     format="(i4,'-',i2.2,'-',i2.2,'T',i2.2,':',i2.2,':',i2.2,5x,2z8)"

;	now the analog values and MF number
	printf, lun_sdf, STEPHV, STEPtherm, STEPt1, STEPt2, STEPt3, MFnum, format="(5e12.4,z8)"
	
;	write out the matrix rates, and last packet
	printf, lun_sdf, packet(0:481), packet(732), format="(483z2)"
	
;	write out the number of phas
	printf, lun_sdf, npha, format="(z2)"
	
;	now write out only the non-zero pha events
	for index = 482, 727, 5 do if total(packet(index:index+4) gt 0 ) then printf, lun_sdf, packet(index:index+4), format="($,5z2)"
;	print out a space to force a line feed (unless NPHA = 0)
	if(npha gt 0 ) then printf, lun_sdf, ' ', format="(a1)"	
	
;	write out the kp data
	printf, lun_sdf, stepkp, format="(6e12.4)"
	
	return
	
end_file:	print, ' eof encountered!'
		eof_flag = 1
		return

	end
	
