;================================= read_rec ==================================

pro read_rec,date_time

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
;read selected lz mjf record num recn (Jim Byrnes' procedure)
  if recn gt fh.nmf then begin
    print,' ' & print,'end of file: recn= ',recn,'  fh.nmf=',fh.nmf
    return
  endif

;print,recn,fh.rln,fh.spcid

  lz=read_lzrcd(lundat,recn,fh.rln,fh.spcid) ;lz=data structure incl header

  ms_hms,lz.ms,h,m,s  ;get hhmmss.ms from msec of day
  date_time=string(lz.yr,format='(i4)') + ' ' + string(lz.dy,format='(i3)') +$
    '   ' + string(h,format='(i2)') + ':' + string(m,format='(i2)') +$
    ':' + string(s,format='(f6.3)')

  
  end

