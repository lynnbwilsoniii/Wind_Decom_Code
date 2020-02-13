pro get_spinp,start=start,lpr=lpr

;determines a spin period using the first two consecutive records encountered
;beginning with current record n if first record in file or n-1 if not the first
;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc

ctmmode=['u','m','s','e']

if keyword_set(lpr) eq 0 then lpr=0
if keyword_set(start) ne 0 then recn=start 
current_recn=recn 

if recn gt 1 then recn=recn-1

point1:
read_rec,date_time
 
;determine telemetry mode from record header
  md=strarr(257)
  md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
  md(8)='con46' & md(128)='trans' & md(256)='u'
  if lz.telmod ne 1 and lz.telmod ne 5 then begin
    print,'recn,data record header tm mode indicator ',$
    recn,lz.telmod,md(lz.telmod)
    recn=recn+1
    goto,point1
  endif

;determine tm mode from instr hk and test whether in science mode
tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
if tmmode_ihk ne 2 then begin
   print,' '&print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
   recn=recn+1
   goto,point1
endif

;the first in a consecutive pair of science records has been read

;get time tagged spincnt
     time_utc,lz.mf(hkm1(1).offs:hkm1(1).offs+6),$
     tjd,sec,hour,min,isec,ms,hms,spincnt
     sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
     sp.mfrecn=lz.recn & sp.mfyr=lz.yr & sp.mfdy=lz.dy & sp.mfms=lz.ms
  
     ;get spin period
     sp.mjfcnt=lz.mf(ihk(1).offs)
     spinperiod,sp 
     ;print,' ' & print,'get spin period:' 
     if lpr then print,' ' 
     if lpr then print,'current_recn, recn',current_recn,recn
     if lpr then print,$
      'get_spinp: time tag:  tjd      secdy  hh:mm:ss.ms  spincnt  spinperiod'
     if lpr then print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',$
      ms,sp.spincnt,sp.spinp,$
      format='(9x,i5,2x,f9.3,2x,i2,a1,i2,a1,i2,a1,i3,2x,i3,f12.3)'
     ;print,tmmode_ihk,tmrate_ihk,scimode_ihk,sp.mjfcnt,format='(3i5,z3)'

lsttmmode_ihk=tmmode_ihk
recn=recn+1
read_rec,date_time

;determine telemetry mode from record header
  if lz.telmod ne 1 and lz.telmod ne 5 then begin
    print,'recn,data record header tm mode indicator ',$
     recn,lz.telmod,md(lz.telmod)
    recn=recn+1
    goto,point1
  endif

if tmmode_ihk ne 2 then begin
   print,' '&print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
   recn=recn+1
   goto,point1
endif

;the second in a consecutive pair of science records has been read

;get time tagged spincnt
     time_utc,lz.mf(hkm1(1).offs:hkm1(1).offs+6),$
     tjd,sec,hour,min,isec,ms,hms,spincnt
     sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
     sp.mfrecn=lz.recn & sp.mfyr=lz.yr & sp.mfdy=lz.dy & sp.mfms=lz.ms
  
     ;get spin period
     sp.mjfcnt=lz.mf(ihk(1).offs)
     spinperiod,sp  
            
     if lpr then print,' ' 
     if lpr then print,'current_recn, recn',current_recn,recn
     if lpr then print,$
       'get_spinp: time tag:  tjd      secdy  hh:mm:ss.ms  spincnt  spinperiod'
     if lpr then print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',$
      ms,sp.spincnt,sp.spinp,$
      format='(9x,i5,2x,f9.3,2x,i2,a1,i2,a1,i2,a1,i3,2x,i3,f12.3)'
     ;print,tmmode_ihk,tmrate_ihk,scimode_ihk,sp.mjfcnt,format='(3i5,z3)'

;a valid spinperiod should have been determined

start=recn-1   ;start is redefined as  the first science record of file

if recn ne current_recn then begin ;reset to current record and read
  recn=current_recn  
  read_rec,date_time
  
;determine telemetry mode from record header
  if lz.telmod ne 1 and lz.telmod ne 5 then $
    print,'data record header tm mode indicator ',lz.telmod, ' ',md(lz.telmod)
    
endif

end
