pro prt_hk,lun

;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
;common lzstuff,$
;infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf

if lun eq 0 then begin
  print,' ' & print,'general housekeeping'
  scimode_hk=get_bits((hkm1(0).offs),hkm1(0).bv(0).p,hkm1(0).bv(0).n)
  print,'offset, byte value, bit var ',hkm1(0).offs,(hkm1(0).offs),$
    hkm1(0).bv(0).bnm,scimode_hk,format='(a30,i6,z3,a16,i4)'

  tmmode_hk=get_bits((hkm1(0).offs),hkm1(0).bv(1).p,hkm1(0).bv(1).n)
  print,'offset, byte value, bit var ',hkm1(0).offs,(hkm1(0).offs),$
    hkm1(0).bv(1).bnm,tmmode_hk,format='(a30,i6,z3,a16,i4)'

  tmrate_hk=get_bits((hkm1(0).offs),hkm1(0).bv(2).p,hkm1(0).bv(2).n)
  print,'offset, byte value, bit var ',hkm1(0).offs,(hkm1(0).offs),$
    hkm1(0).bv(2).bnm,tmrate_hk,format='(a30,i6,z3,a16,i4)'

   s=size(hkm1)
   for i=0,s(1)-1 do begin
     if hkm1(i).nbv eq 0 then begin
       print,' '
       print, hkm1(i).descr,format='(a16)'
       print,lz.mf(hkm1(i).offs+indgen(hkm1(i).ln)),format='(16z3.2)'
     endif else begin
       print,' '
       print, hkm1(i).descr,lz.mf(hkm1(i).offs),format='(a16,z3)'
       for j=0,hkm1(i).nbv-1 do print, hkm1(i).bv(j).bnm,$
         get_bits(lz.mf(hkm1(i).offs),hkm1(i).bv(j).p,hkm1(i).bv(j).n),$
         format='(a16,i3)'
     endelse
   endfor
      

endif else begin

endelse
end
