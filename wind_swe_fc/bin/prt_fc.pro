; @(#)prt_fc.pro  VERSION 1.2    7/28/94   16:13:29
pro prt_fc,lun,fcbl,scidat,infile,APPEND=APPEND,QUIET=QUIET

if not keyword_set(APPEND) then APPEND = 0	
if not keyword_set(QUIET) then QUIET = 0
if lun eq 0 then begin
  openw,lun2,'fc_data.prt',/get_lun,APPEND=APPEND
  if n_params() eq 3 then infile = ' '

  if not APPEND then begin
    printf,lun2,infile
    printf,lun2,'faraday cup data'
    printf,lun2,'offsets'
    printf,lun2,'data values'
  endif

  if not QUIET then begin 
    print,' '
    print,'faraday cup data'
    print,'offsets:'
    help,fcbl,/str
    print,'data values'
  endif
  s=size(fcbl)
  for j=0,s(1)-1 do begin
    if not QUIET then begin 
      print,'faraday cup block # ',j
      print,scidat(fcbl(j).offs+indgen(fcbl(j).ln)),format='(20z3.2)'
    endif
    printf,lun2,'faraday cup  block # ',j
    printf,lun2,scidat(fcbl(j).offs+indgen(fcbl(j).ln)),format='(20z3.2)'
  endfor
  free_lun,lun2

endif else begin
endelse
end

;I want to process lots of records in a row, 
;so printing to the screen would be a terrible mess
;Also, i don't want to change the program so that it is no longer compatible
;with the original program
;I want to be able to append new data to the old file...
;Thus prt_fc,lun,fcbl,scidat,/APPEND will append to the file
;Typically i would want to call prt_fc with infile the first time,
;with or without QUIET
;Then to append lots of other records, i would call prt_fc with QUIET and APPEND
;but without infile
