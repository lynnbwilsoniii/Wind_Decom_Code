pro notscimode

;reads all mjf header's and  searches for non science mode records

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
md=strarr(257)
  md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
  md(8)='con46' & md(128)='trans' & md(256)='u'

recn_current=recn

print,' ' & print,'search for non science mode records' & print,'reading.....'
nscimd=lonarr(fh.nmf)
for recn=1,fh.nmf-1 do begin
  lz=read_lzrcd(lundat,recn,fh.rln,fh.spcid) ;lz=data structure incl header
  if lz.telmod ne 1 and lz.telmod ne 5 then begin
    print,'data record header tm mode indicator ',$
      recn,lz.telmod, ' ',md(lz.telmod)
    nscimd(recn)=lz.telmod
  endif
endfor
w=where(nscimd ne 0,nw)
if w(0) ne -1 then nscimd=nscimd(w) else nw=0
print,' ' & print,'end reading file'
print,nw,' of ',fh.nmf,' records are not in science mode' & print,' '
recn=recn_current
end
