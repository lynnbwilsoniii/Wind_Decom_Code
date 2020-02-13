pro scan_mode,recnstop=recnstop

;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc

if keyword_set(recnstop) eq 0 then recnstop=2000

md=strarr(257)
md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
md(8)='con46' & md(128)='trans' & md(256)='u'

elecion=['elecs','ions']

tmm=[' u ','man','sci','eng']

recn_current=recn
print,' ' & print,'reading.....'
print,lzfile
for recn=1,fh.nmf-1 do begin
  lz=read_lzrcd(lundat,recn,fh.rln,fh.spcid) ;lz=data structure incl header

  ms_hms,lz.ms,h,m,s  ;get hhmmss.ms from msec of day
  date_time=string(lz.yr,format='(i4)') + ' ' + string(lz.dy,format='(i3)') +$
    '   ' + string(h,format='(i2)') + ':' + string(m,format='(i2)') +$
    ':' + string(s,format='(f6.3)')

  ;determine tm mode, tm rate, science mode, and mjf count from instr hk
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  if tmmode_ihk eq 2 and (scimode_ihk eq 0 or scimode_ihk eq 1) then begin
    elec_ion=get_bits(lz.mf(ihk(6).offs),ihk(6).bv(0).p,ihk(6).bv(0).n)  
    print,recn,date_time,'   hdr:',md(lz.telmod),$
    '   ihk:',tmm(tmmode_ihk),scimode_ihk,elecion(elec_ion),$
    '  ebias',lz.mf(ihk(18).offs),lz.mf(ihk(28).offs),$
    format='(i4,2x,a22,a7,a5,a7,a3,z2,3x,a5,a7,2i3)'
    ;'  q',fix(total(lz.qlty)),$
  endif else begin
    print,recn,date_time,'   hdr:',md(lz.telmod),$
    '  ihk:',tmm(tmmode_ihk),scimode_ihk,$
    '  ebias',lz.mf(ihk(18).offs),lz.mf(ihk(28).offs),$
    format='(i4,2x,a22,a7,a5,a7,a3,z2,3x,5x,a7,2i3)'
    ;' q',fix(total(lz.qlty)),
  endelse
  if recnstop*fix(recn/recnstop) eq recn then begin
    answ='' & print,'hit return to continue' 
    read,answ & if answ ne '' then stop
  endif
endfor
print,' ' & print,'scan_mode finished'
end
