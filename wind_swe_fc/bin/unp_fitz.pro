; @(#)unp_fitz.pro  VERSION 1.3    11/21/94   14:36:23
;     unp_fitz.pro:  reads IDL swelz output file fc_data.prt and outputs 
;                    data in the same format as record2.f output files
;
;            USAGE:  unpickle_fitz[, inputfilename]
;
;           OUTPUT:  stdout:    inputfilename only
;                    caldata:   calibration mode data for each cup and collector
;                    moddata:   modulator levels for each cup & collector
;
;     frank v marcoline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO openfiles      

  common share, fcff, calfile, calunit, modfile, modunit
  openw,calunit,calfile,/GET_LUN,error=err	       ;open calfile for writing
  if (err ne 0) then print,'Could not open output file: ',calfile
  openw,modunit,modfile,/GET_LUN,error=err	       ;open modfile for writing
  if (err ne 0) then print,'Could not open output file: ',modfile
end  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end openfiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO stampfilename,filename 

  common share, fcff, calfile, calunit, modfile, modunit
  print,filename
  printf,calunit,filename
  printf,modunit,filename
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;; end stampfilename ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION up_1,num			;increment a number (like C's ++num)

  num = num + 1
  return, num
end 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end up_1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO do_fcff, data

  common share, fcff, calfile, calunit, modfile, modunit
  i = 0 
  p = 2
  mod_hi_a = (mod_lo_a = (mod_hi_b = (mod_lo_b = 0)))
  ms = (min = (hour = (spin_cnt = (ml = (fc_max = (fc_stat = 0))))))
  isec = (utc = (tjd = 0L)) 					  ;long integers

  fcff.flag = fcff.flag + 1

  utc = long(data(p))
  utc = utc + ishft(long(data(up_1(p))),8)
  utc = utc + ishft(long(data(up_1(p))),16)
  utc = utc + ishft(long(data(up_1(p))),24)
  
  ms = long(data(up_1(p)))
  ms = ms + ishft(long(data(up_1(p))),8)
  ms = 2L^16 - 1 - ms
  if ms gt 999 then ms = 0
  
  isec = utc and '1ffff'XL
  hour = isec / 3600
  min  = (isec - long(hour*3600))/60
  isec = isec - hour*3600 - min*60
  tjd  = ishft(utc,-17)
  ;print,long(utc),long(tjd)
  fcff.time = string(tjd,hour,min,isec,$
		     format='("day",i5,i3.2,":",i2.2,":",i2.2)')

  spin_cnt = data(up_1(p))
  
  if (fcff.init) then begin
    printf,modunit,fcff.time
    fcff.init=0;
  endif

  fcff.step = data(up_1(p));
  fc_max = data(up_1(p));
  fc_stat = data(up_1(p));
         ;printf("#spins/spectrum = %02u\n",fc_max)
	 ;printf("cal step = %02u (%02Xh)\nfc_stat = %02u\n",fcff.step, 
	 ;fcff.step,fc_stat);
  
  p = up_1(p) 				;skip spare byte, data(12) = # cal. step

  for i=0,10 do begin
    ml = data(up_1(p))
    ;converting 3 bytes to 2 numbers
    mod_hi_a = data(up_1(p))
    mod_hi_a = mod_hi_a + (data(p) and '0F'X) * 2^8
    
    mod_lo_a = ( data(up_1(p)) and 'F0'X ) / 2^4
    mod_lo_a = mod_lo_a + data(up_1(p)) * 2^4
    
    mod_hi_b = data(up_1(p))
    mod_hi_b = mod_hi_b + (data(p) and '0F'X) * 2^8
    
    mod_lo_b = ( data(up_1(p)) and 'F0'X ) / 2^4
    mod_lo_b = mod_lo_b + data(up_1(p)) * 2^4

    printf,modunit,ml,mod_hi_a,mod_lo_a,mod_hi_b,mod_lo_b, $
      format='(i2.2, i8.5, i8.5, i8.5, i8.5)'
  endfor
  ;print,p,format='("p = ",i3)'
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end do_fcff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO closefiles    

  common share, fcff, calfile, calunit, modfile, modunit
  free_lun,calunit,modunit
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end closefiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO unpickle, data

;;;;;declarations & initializations
  common share, fcff, calfile, calunit, modfile, modunit
  aword = (bword = (cup1 = (cup2 = (i = 0))))
  fcblock = intarr(4,20)
  cup = [1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,1,1,1,1,$
	 2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2]

  if (fcff.flag) then begin
    printf,calunit,fcff.time,format='(a)'
    printf,calunit,fcff.step,format='(i3)'
    fcff.flag = 0
  endif else printf,calunit,format='(/)'	    ;output two carriage returns
  ;print,'format unpickle printf 1'		;dumb reminder message to stdout
  ;print,'make sure for loops do not for once extra'
  printf,calunit,fix(data(0)) and '1f'XB, data(0), data(1), $	  ;spin counters
			format = '(i2,i4,i4)'
		; bits in lb1..lb3 are packed in the order
		;    lb1 -> A7 A6 A5 A4 A3 A2 A1 A0
		;    lb2 -> B3 B2 B1 B0 P1 P0 A9 A8
		;    lb3 -> Q1 Q0 B9 B8 B7 B6 B5 B4
		; where A,B represent data bits and P,Q represent range bits.

  for i=2,120,3 do begin
    aword = ((data(i+1) and '0F'X) * 256) or data(i)
    bword = ((data(i+1) and 'F0'X) /  16) or (data(i+2) * 16)
    aword = aword xor '3FF'X
    bword = bword xor '3FF'X

    if (cup((i-2)/3) eq 1) then begin
      fcblock(0,cup1) = aword
      fcblock(1,cup1) = bword
      cup1 = cup1 + 1
    endif else begin	
      fcblock(2,cup2) = aword
      fcblock(3,cup2) = bword
      cup2 = cup2 + 1
    endelse
    ;print,cup1,cup2,format='("Cup: ",i2, i2)'
  endfor

  printf,calunit,fcblock,format='(i5,3i6)'

  ;for i=0,19 do $
    ;printf,calunit, fcblock(0,i), fcblock(1,i), fcblock(2,i), fcblock(3,i))

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end unpickle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO unp_fitz,inputfile
on_error,2

;;;;;declarations & initializations
  common share, fcff, calfile, calunit, modfile, modunit
  byte1 = (byte2 = '0'XB)			 	;initialize as type Byte
  data = bytarr(125)	       ;could use same statement later to 0 out elements
  fcff = {init: 1, flag: 0, time: strarr(1), step: 0}; time is 1 string array
			;fcff.init:  is the current FCFF the first?
			;fcff.flag:  was there an FCFF before this data block?
			;fcff.time:  decoded utc time stamp (fcff bytes 3-8)
			;fcff.step:  calibration level (fcff byte 10)
  calfile = 'caldata' & modfile = 'moddata'		      ;output file names
  junk=''

;;;;;opening data file for input

  if n_params() ne 1 then $  ;if default inputfile not specified in command line
	inputfile = 'fc_data.prt' 			      ;then try this one
  openr,inunit,inputfile,/GET_LUN,error=err          ;open inputfile for reading
  if (err ne 0) then $
	print,'Could not open data file ',inputfile,' for reading'
						            ;query-new-inputfile
;;;;;prepair data files for output

  openfiles                                               ;open files for output
  stampfilename,inputfile	   ;puts source file name at top of output files

;;;;;read in the data, and process it

  for i=0,3 do begin line = '' & readf,inunit,line & endfor ;discard 1st 4 lines

  while not eof(inunit) do begin
    line = '' & readf,inunit,line ;& print,line
    data = bytarr(122)
    readf,inunit,data,format='(6(20z3.2,/),2z3.2)'   ;read full fc block of data
    ;print,data,format='(6(20z3.2,/),2z3.2)'
    byte1 = data(0)				       ;read the first two bytes
    byte2 = data(1)					    ;to check block type
    if ((byte1 eq 'fc'XB) and (byte2 eq 'ff'XB)) then $
      do_fcff, data else $				     ;process fcff block 
    if (not eof(inunit)) and $
       (((byte1 ne 'ff'XB))and $;"or(byte1 ne '00'XB))" B4 )& 2 ingore mpt block 
	(byte2 ne '00'XB)) then $	       		   ;if not an fill block
      unpickle, data		               ;process normal calibration block
   endwhile
;;;;;close the data files
  free_lun,inunit
  closefiles

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end unp_fitz ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	





