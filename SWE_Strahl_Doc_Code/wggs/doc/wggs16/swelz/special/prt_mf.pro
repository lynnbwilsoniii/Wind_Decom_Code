pro prt_mf,lun,mf,recn

if lun eq 0 then begin
  print,'major frame # ',recn
  for i=0,249 do begin
    print,'mnf # ',i
    print,mf(i*45+indgen(45)),format='(25z3)'
  endfor
  print,'fill ',mf(11250:11251),format='(a5,2z3)'
  endif else begin
endelse
  openw,lun,'prt_mf',/get_lun
  printf,lun,'major frame # ',recn
  for i=0,249 do begin
     printf,lun,'mnf # ',i
     printf,lun,mf(i*45+indgen(45)),format='(25z3)'
  endfor
  printf,lun,'fill ',mf(11250:11251),format='(a5,2z3)'
end
