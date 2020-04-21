pro prt_fc,lun,fcbl,scidat

if lun eq 0 then begin
  openw,lun2,'fc_data.prt',/get_lun
  print,' '
  printf,lun2,' '
  print,'faraday cup data'
  printf,lun2,'faraday cup data'
  print,'offsets:'
  printf,lun2,'offsets'
  help,fcbl,/str
  print,'data values'
  printf,lun2,'data values'
  s=size(fcbl)
  for j=0,s(1)-1 do begin
    print,'faraday cup block # ',j
  printf,lun2,'faraday cup  block # ',j
    print,scidat(fcbl(j).offs+indgen(fcbl(j).ln)),format='(20z3)'
    printf,lun2,scidat(fcbl(j).offs+indgen(fcbl(j).ln)),format='(20z3.2)'
  endfor
  free_lun,lun2

endif else begin
endelse
end
