pro lzhdrs

;reads all mjf's and  searches for bad quality flags etc

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
recn_current=recn
print,' ' & print,'reading.....'
for recn=1,fh.nmf-1 do begin
  lz=read_lzrcd(lundat,recn,fh.rln,fh.spcid) ;lz=data structure incl header
  wbad=where(lz.qlty gt 0,nwbad)
  if nwbad gt 0 then print,'recn, # bad mf''s',recn,nwbad
endfor
print,' ' & print,'end reading file'
recn=recn_current
end
