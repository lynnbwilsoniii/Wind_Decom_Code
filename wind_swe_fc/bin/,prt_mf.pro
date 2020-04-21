pro prt_mf,lun,mf,recn

if lun eq 0 then begin
openw,lun2,'fc_data.prt',/get_lun
print,'major frame # ',recn
printf,lun2,'major frame # ',recn
for i=0,249 do begin
  print,'mnf # ',i
  printf,lun2,'mnf # ',i
  print,mf(i*45+indgen(45)),format='(25z3)'
  printf,lun2,mf(i*45+indgen(45)),format='(25z3)'
endfor
print,'fill ',mf(11250:11251),format='(a5,2z3)'
printf,lun2,'fill ',mf(11250:11251),format='(a5,2z3)'
free_lun,lun2
endif else begin
endelse
end
