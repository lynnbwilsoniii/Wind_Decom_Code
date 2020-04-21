; @(#)prt_hk.pro  VERSION 1.2    7/28/94   16:13:31
pro prt_hk,lun,hk,scidat
if lun eq 0 then begin
  print,' ' & print,'general housekeeping'
  scimode_hk=get_bits(scidat(hk(0).offs),hk(0).bv(0).p,hk(0).bv(0).n)
  print,'offset, byte value, bit var ',hk(0).offs,scidat(hk(0).offs),$
    hk(0).bv(0).bnm,scimode_hk,format='(a30,i6,z3.2,a16,i4)'

  tmmode_hk=get_bits(scidat(hk(0).offs),hk(0).bv(1).p,hk(0).bv(1).n)
  print,'offset, byte value, bit var ',hk(0).offs,scidat(hk(0).offs),$
    hk(0).bv(1).bnm,tmmode_hk,format='(a30,i6,z3.2,a16,i4)'

  tmrate_hk=get_bits(scidat(hk(0).offs),hk(0).bv(2).p,hk(0).bv(2).n)
  print,'offset, byte value, bit var ',hk(0).offs,scidat(hk(0).offs),$
    hk(0).bv(2).bnm,tmrate_hk,format='(a30,i6,z3.2,a16,i4)'

   s=size(hk)
   for i=0,s(1)-1 do begin
     if hk(i).nbv eq 0 then begin
       print,' '
       print, hk(i).descr,format='(a16)'
       print,scidat(hk(i).offs+indgen(hk(i).ln)),format='(16z3.2)'
     endif else begin
       print,' '
       print, hk(i).descr,scidat(hk(i).offs),format='(a16,z3.2)'
       for j=0,hk(i).nbv-1 do print, hk(i).bv(j).bnm,$
         get_bits(scidat(hk(i).offs),hk(i).bv(j).p,hk(i).bv(j).n),$
         format='(a16,i3)'
     endelse
   endfor
      

endif else begin

endelse
end

