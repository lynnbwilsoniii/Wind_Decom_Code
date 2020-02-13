pro prt_sci,lun,scidat,recn

if lun eq 0 then begin
  print,'scidat'
  print,scidat,format='(25z3)'  
endif else begin
  openw,lun,'prt_sci',/get_lun
  printf,lun,'scidat'
  printf,lun,scidat,format='(25z3)'
endelse

end
