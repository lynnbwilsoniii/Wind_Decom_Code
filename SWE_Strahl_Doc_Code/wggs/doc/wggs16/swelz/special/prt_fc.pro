pro prt_fc,lun

;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc

mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc

if lun eq 0 then begin
  print,' '
  print,'faraday cup data'
  print,'offsets:'
  help,fcblm1,/str
  print,'data values'
  s=size(fcblm1)
  for j=0,s(1)-1 do begin
    print,'faraday cup block # ',j
    print,lz.mf(fcblm1(j).offs+indgen(fcblm1(j).ln)),format='(20z3)'
  endfor

endif else begin
endelse
end
