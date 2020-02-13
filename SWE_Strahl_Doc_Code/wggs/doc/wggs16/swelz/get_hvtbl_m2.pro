pro get_hvtbl_m2

;if mode2 then read 4 mjf's to get full 256 bytes of hv tbl
;each record is 64 bytes, ordered in the table with mjf_cnt mod 4= 3 first


common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp

print,'get_hvtbl_m2: NEW VEIS HV TABLE'

current_recn=recn

tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

tbl=[0,0,0,0]
veis_hvtbl=bytarr(256)
while tmmode_ihk eq 2 and (scimode_ihk eq 2 or scimode_ihk eq 11) and $
   total(tbl) lt 4 do begin
   read_rec,date_time   
   indx=( (lz.mf(ihk(1).offs) mod 4) + 1) mod 4      
   veis_hvtbl(indx*64+indgen(64))=$
     lz.mf(hkind(ghk(27).offs+indgen(ghk(27).ln)))
   tbl(indx)=1
   print,'recn, recn,lz.mf(ihk(1).offs) mod 4, mode, tbl ',$
            recn,recn,lz.mf(ihk(1).offs) mod 4,scimode_ihk,tbl
   print,'veis_hvtbl ',veis_hvtbl
   tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
   scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
   recn=recn+1
endwhile

recn=current_recn
read_rec,date_time
end
