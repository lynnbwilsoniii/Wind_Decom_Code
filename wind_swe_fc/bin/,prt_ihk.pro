pro prt_ihk,lun,ihk,lz

if lun eq 0 then begin
   openw,lun2,'fc_data.prt',/get_lun
  print,' ' & print,'instrument housekeeping data'
  printf,lun2,' ' & print,'instrument housekeeping data'
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  print,'offset, byte value, bit var ',ihk(2).offs,lz.mf(ihk(2).offs),$
    ihk(2).bv(0).bnm,scimode_ihk,format='(a30,i6,z3,a16,i4)'
  printf,lun2,'offset, byte value, bit var ',ihk(2).offs,lz.mf(ihk(2).offs),$
    ihk(2).bv(0).bnm,scimode_ihk,format='(a30,i6,z3,a16,i4)'


  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  print,'offset, byte value, bit var ',ihk(2).offs,lz.mf(ihk(2).offs),$
    ihk(2).bv(1).bnm,tmmode_ihk,format='(a30,i6,z3,a16,i4)'
  printf,lun2,'offset, byte value, bit var ',ihk(2).offs,lz.mf(ihk(2).offs),$
    ihk(2).bv(1).bnm,tmmode_ihk,format='(a30,i6,z3,a16,i4)'

  tmrate_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
  print,'offset, byte value, bit var ',ihk(2).offs,lz.mf(ihk(2).offs),$
    ihk(2).bv(2).bnm,tmrate_ihk,format='(a30,i6,z3,a16,i4)'
  printf,lun2,'offset, byte value, bit var ',ihk(2).offs,lz.mf(ihk(2).offs),$
    ihk(2).bv(2).bnm,tmrate_ihk,format='(a30,i6,z3,a16,i4)'
  
  for i=0,44 do begin
    if ihk(i).nbv eq 0 then begin
      print,' '
      printf,lun2,' '
      print, ihk(i).descr,lz.mf(ihk(i).offs),format='(a16,z3)'
      printf,lun2, ihk(i).descr,lz.mf(ihk(i).offs),format='(a16,z3)'
    endif else begin
      print,' '
      printf,lun2,' '
      print, ihk(i).descr,lz.mf(ihk(i).offs),format='(a16,z3)'
      printf,lun2, ihk(i).descr,lz.mf(ihk(i).offs),format='(a16,z3)'
      for j=0,ihk(i).nbv-1 do print, ihk(i).bv(j).bnm,$
        get_bits(lz.mf(ihk(i).offs),ihk(i).bv(j).p,ihk(i).bv(j).n),$
        format='(a16,i3)'
    endelse
      
  endfor

	free_lun,lun2
endif else begin
endelse
end

