;================================= proc_rec ==============================

pro proc_rec,tmmode_ihk=tmmode_ihk,lpr=lpr,elec_ion_m1=elec_ion_m1,$
  ifsci=ifsci,date_time,scimode_ihk=scimode_ihk,noglnt=noglnt,err=err,$
  norelgains=norelgains

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6
common strahlstuff,strahl_hvtbl
common backg,subtrbkg
common swestuff,swest

;print,'proc_rec....'

err=''

lpr2=0
lpr1=0

ctmmode=['u','m','s','e']
ctmrate=['s','f']
elecion=['electrons','ions']
md=strarr(257)
md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
md(8)='con46' & md(128)='trans' & md(256)='u'

if keyword_set(lpr) eq 0 then lpr=0
if keyword_set(norelgains) eq 0 then norelgains=0

;testing
;lpr=1 & lpr1=1 & lpr2=1

;input_rec is the recn coming into this proc
;recn is the current record number in common lzstuff

;Read input_rec and input_rec + 1 to determine spin period and 
;test whether mode changes.

;If mode changes or if hv table (mode 2) changes in input_rec + 1, 
;then input_rec is suspect and will be skipped and 
;current recn updated to input_rec + 1.

;If hv table (mode 2) changes then get new hv table by reading ahead 4 recn's 
;beginning with newly updated input_recn, i.e., 
;the recn in which the hv table changed. After the 4 recn's have been read, 
;then reset recn back to newly updated input_rec

input_recn_orig=recn

ifsci=0
point1:
hvtbl64=bytarr(64,4)
hvtbl16=bytarr(16,2)
hvtbl16_strl=bytarr(16,2)
input_recn=recn

;--------------- read current or FIRST record -------------------------------

if recn lt fh.nmf then read_rec,date_time else return ;stop,'end of input data file'

;determine telemetry mode from record header
  if lz.telmod ne 1 and lz.telmod ne 5 then begin
    print,'not a SWE science mode: recn,data record header tm mode indicator ',$
    recn,lz.telmod,md(lz.telmod)
    ;if recn lt fh.nmf then recn=recn+1 else stop,'end of input data file'
    if recn lt fh.nmf then recn=recn+1 else begin
      err='end of input data file'
      return
    endelse
    ;goto, point1
    ifsci=0
    return
  endif

;determine tm mode from instr hk and test whether in science mode
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  if tmmode_ihk ne 2 then begin
    print,' '
    print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
    ;if recn lt fh.nmf then recn=recn+1 else stop,'end of input data file'
    if recn lt fh.nmf then recn=recn+1 else begin
      err='end of input data file'
      return
    endelse
    ;goto, point1
    ifsci=0
    return
  endif

;current_recn is in science mode 
  ifsci=1

;determine which science mode 
  scimode_ihk_0=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

;if mode2 then test for full hv table 
;(if first time in mode2 then it will contain zeros)

  if scimode_ihk_0 eq 2 or scimode_ihk_0 eq 11 then begin
    if keyword_set(veis_hvtbl) eq 0 then begin
      get_hvtbl_m2   ;also resets recn and rereads record
      subtrbkg='No'
    endif else begin
      ;test for a full mode2 hv table
      w0=where(veis_hvtbl eq 0)
      if w0(0) ne -1 then begin 
        get_hvtbl_m2   ;also resets recn and rereads record
        subtrbkg='No'
      endif
    endelse
  endif

;get mode dependent portion of hv table this mjf
if scimode_ihk_0 eq 2 or scimode_ihk_0 eq 11 then begin
    hvtbl64(*,0)=lz.mf(hkind(ghk(27).offs+indgen(ghk(27).ln)))  
    hvtbl16_strl(*,0)=lz.mf(hkind(ghk(28).offs+indgen(ghk(28).ln)))
endif else $
if scimode_ihk_0 eq 0 or scimode_ihk_0 eq 1 or scimode_ihk_0 eq 4 then $
    hvtbl16(*,0)=lz.mf(hkm1(4).loc(indgen(16)))        

;get mjf count
  mjfcnt=bytarr(4)
  mjfcnt(0)=lz.mf(ihk(1).offs)
  
;get time tagged spincnt
  time_utc,lz.mf(hkind(ghk(1).offs):hkind(ghk(1).offs)+6),$
    tjd,sec,hour,min,isec,ms,hms,spincnt
  sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
  sp.mfrecn=lz.recn & sp.mfyr=lz.yr & sp.mfdy=lz.dy & sp.mfms=lz.ms
 

if lpr1 then $
print,'the FIRST in a consecutive pair of science records has been read:',recn

;if this record not the input_recn_orig 
;then set input_recn=recn
  if recn ne input_recn_orig then begin
    input_recn=recn
    print,'incrementing input_recn: input_recn_orig, input_recn ',$
    input_recn_orig, input_recn
  endif



;----------- read SECOND record in order to get spin period -------------------

;if recn lt fh.nmf then recn=recn+1 else stop,'end of input data file'
if recn lt fh.nmf then recn=recn+1 else begin
  err='end of input data file'
  return
endelse
read_rec,date_time

;determine telemetry mode from record header
  if lz.telmod ne 1 and lz.telmod ne 5 then begin
    print,'not a SWE science mode: recn,data record header tm mode indicator ',$
    recn,lz.telmod,md(lz.telmod)
    print,'increment recn to find first of consecutive science records'
    ;if recn lt fh.nmf then recn=recn+1 else stop,'end of input data file'
    if recn lt fh.nmf then recn=recn+1 else begin
      err='end of input data file'
      return
    endelse
    goto, point1   ;go back to beginning
  endif

;determine tm mode from instr hk and test whether in science mode
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  if tmmode_ihk ne 2 then begin
    print,' '
    print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
    ;if recn lt fh.nmf then recn=recn+1 else stop,'end of input data file'
    if recn lt fh.nmf then recn=recn+1 else begin
      err='end of input data file'
      return
    endelse
    print,'increment recn to find first of consecutive science records'
    goto, point1     ;go back to beginning
  endif

;get time tagged spincnt
  time_utc,lz.mf(hkind(ghk(1).offs):hkind(ghk(1).offs)+6),$
    tjd,sec,hour,min,isec,ms,hms,spincnt
  sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
  sp.mfrecn=lz.recn & sp.mfyr=lz.yr & sp.mfdy=lz.dy & sp.mfms=lz.ms

;get spin period
  sp.mjfcnt=lz.mf(ihk(1).offs)
  spinperiod,sp 
  if sp.spinp eq 0 and sp.lst_spinp ne 0 then sp.spinp=sp.lst_spinp $
  else if sp.lst_spinp eq 0 then begin
    err='spin period not determined' & return
  endif

        
  if lpr then print,' ' & if lpr then print,'recn',recn
  if lpr then print,$
    'a: proc_rec: time tag:  tjd      secdy  hh:mm:ss.ms  spincnt  spinperiod'
  if lpr then print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',$
    ms,sp.spincnt,sp.spinp,$
    format='(9x,i5,2x,f12.3,2x,i2,a1,i2,a1,i2,a1,i3,2x,i3,f12.3)'

;determine science mode for SECOND recn 
  scimode_ihk_1=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

;get mode dependent portion of hv table this mjf
if scimode_ihk_1 eq 2 or scimode_ihk_1 eq 11 then begin
    hvtbl64(*,1)=lz.mf(hkind(ghk(27).offs+indgen(ghk(27).ln))) 
    hvtbl16_strl(*,1)=lz.mf(hkind(ghk(28).offs+indgen(ghk(28).ln)))
endif else if scimode_ihk_1 eq 0 or scimode_ihk_1 eq 1 $
  or scimode_ihk_1 eq 4 then $
    hvtbl16(*,1)=lz.mf(hkm1(4).loc(indgen(16)))

;construct strahl voltage table from the two mjf's just read (modes 1 and 2)
if scimode_ihk_1 ne 4 then begin
  strahl_hvtbl=bytarr(32)
  strahl_hvtbl(*)=hvtbl16_strl(sort(hvtbl16_strl))
endif
  
;get mjf count
  mjfcnt(1)=lz.mf(ihk(1).offs)

;test for mode change
  if scimode_ihk_1 ne scimode_ihk_0 then begin
    print,' '
    print,'change in science mode: ',scimode_ihk_0,scimode_ihk_1
    ;if recn lt fh.nmf then recn=recn+1 else stop,'end of input data file'
    if recn lt fh.nmf then recn=recn+1 else begin
      err='end of input data file'
      return
    endelse
    print,$
     'increment recn to find first of consecutive same mode science records'
    goto, point1
  endif

 
if lpr1 then $
print,'SECOND in pair of consecutive same mode science records read:',recn
if lpr1 then print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',$
    ms,sp.spincnt,sp.spinp,$
    format='(9x,i5,2x,f12.3,2x,i2,a1,i2,a1,i2,a1,i3,2x,i3,f12.3)'


;SECOND in pair of consecutive same science mode recns 
;has been read at this point and spinperiod has been calculated
;current recn = input_recn+1


;------------- do mode2 stuff -------------------------------------------------

;test FIRST recn for science mode: 
;   if mode1 then skip this section and continue to process,
;   if mode2 read next 2 records and combine with first 2 recns to get hv tbl

if scimode_ihk_0 eq 2 or scimode_ihk_0 eq 11 then begin  ;3'rd and 4'th record
          
  ;store the first half-spin block of current or SECOND mjf data for future use
     mode2_nxt_blk0,err=err2
     if err2 ne '' then begin
       ;if recn lt fh.nmf then recn=recn+2 else stop,'end of input data file'
       if recn lt fh.nmf then recn=recn+1 else begin
         err='end of input data file'
         return
       endelse
       goto, point1   ;go back to beginning
     endif

  ;read THIRD record
  if recn lt fh.nmf then recn=recn+1 else goto,point2
  read_rec,date_time
  
  ;determine telemetry mode from record header
  if lz.telmod ne 1 and lz.telmod ne 5 then begin
    print,'not a SWE science mode: recn,data record header tm mode indicator ',$
    recn,lz.telmod,md(lz.telmod)
    goto, point2
  endif

  ;determine tm mode from instr hk and test whether in science mode
    tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
    if tmmode_ihk ne 2 then begin
      print,' '
      print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
      goto, point2
    endif

  ;determine science mode for THIRD recn 
    scimode_ihk_2=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

  ;get mjf count
    mjfcnt(2)=lz.mf(ihk(1).offs)
  
  ;test mode
    if (scimode_ihk_2 eq 2 or  scimode_ihk_2 eq 11) then begin  
    
      ;get this (THIRD) mjf portion of hv tbl
        hvtbl64(*,2)=lz.mf(hkind(ghk(27).offs+indgen(ghk(27).ln)))
        if lpr1 then $
          print,'THIRD consecutive same mode science records read:',recn

      
      ;read FOURTH record
        if recn lt fh.nmf then recn=recn+1 else goto,point2
        read_rec,date_time
  
      ;determine telemetry mode from record header
         if lz.telmod ne 1 and lz.telmod ne 5 then begin
           print,'not a SWE science mode: ',$
                 'recn,data record header tm mode indicator ',$
                 recn,lz.telmod,md(lz.telmod)
           goto, point2
         endif

      ;determine tm mode from instr hk and test whether in science mode
          tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
          if tmmode_ihk ne 2 then begin
            print,' '
            print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
            goto, point2
          endif

      ;determine science mode for FOURTH recn 
          scimode_ihk_3=$
            get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

      if (scimode_ihk_3 eq 2 or  scimode_ihk_3 eq 11) then begin 
           ;get this mjf portion of hv tbl
              hvtbl64(*,3)=lz.mf(hkind(ghk(27).offs+indgen(ghk(27).ln)))

           ;get mjf count
              mjfcnt(3)=lz.mf(ihk(1).offs)
     
           if lpr1 then $
             print,'FOURTH consecutive same mode science records read:',recn

           ;construct hv table from the 4 mode2 mjf's just read
              tbl=[0,0,0,0]
              veis_hvtbl=bytarr(256)
              for i=0,3 do begin
                indx=( (mjfcnt(i) mod 4) + 1) mod 4
                veis_hvtbl(indx*64+indgen(64))=hvtbl64(*,i)
                tbl(indx)=1
                if lpr2 then $
                  print,'i, mjfcnt(i),( (mjfcnt(i) mod 4) + 1) mod 4 ',$
                  i, mjfcnt(i),( (mjfcnt(i) mod 4) + 1) mod 4 
                if lpr2 then print,'veis_hvtbl ',veis_hvtbl
              endfor
       endif  
     endif   
endif     ;end 3'rd and 4'th recn


point2:     

if scimode_ihk_0 ne 6 and scimode_ihk_1 ne 6 then begin
;check the stored mode1 and mode2 hv table 
  if (scimode_ihk_0 eq 2 or  scimode_ihk_0 eq 11) then $ 
    eleion_sweep=sweepmode(veis_hvtbl,mode=2) else $
  if (scimode_ihk_0 eq 0 or  scimode_ihk_0 eq 1 or  scimode_ihk_0 eq 4) then $
    eleion_sweep=sweepmode(hvtbl16(*,0),mode=1) else eleion_sweep=-1
    ;print,'eleion_sweep ',eleion_sweep
    if eleion_sweep eq -1 then begin 
      print,'undetermined sweep mode...incrementing one recns and continuing'
      ;print,veis_hvtbl
      ;if recn lt fh.nmf then recn=recn+1 else stop,'end of input data file'
      if recn lt fh.nmf then recn=recn+1 else begin
        err='end of input data file'
        return
      endelse
      goto,point1  
    endif
endif

;reset recn to input_recn and read record
  recn=input_recn
  read_rec,date_time
 

;================= proceed with normal processing =========================

if lpr1 then print,'processing recn ',recn

;instr hk tm and sci mode  
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

;time tagged spincnt
  time_utc,lz.mf(hkind(ghk(1).offs):hkind(ghk(1).offs)+6),$
    tjd,sec,hour,min,isec,ms,hms,spincnt
  sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
  sp.mfrecn=lz.recn & sp.mfyr=lz.yr & sp.mfdy=lz.dy & sp.mfms=lz.ms

;determine elec_ion_m1 select (meaningful only in mode1)
  elec_ion_m1=get_bits(lz.mf(ihk(6).offs),ihk(6).bv(0).p,ihk(6).bv(0).n)
  if lpr then print,'elec/ion select= ',elecion(elec_ion_m1)
         
  if lpr then print,' ' & if lpr then print,'recn',recn
  if lpr then print,$
    'b: proc_rec: time tag:  tjd      secdy  hh:mm:ss.ms  spincnt  spinperiod'
  if lpr then print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',$
    ms,sp.spincnt,sp.spinp,$
    format='(9x,i5,2x,f12.3,2x,i2,a1,i2,a1,i2,a1,i3,2x,i3,f12.3)'

;do mode dependent unpacking
  if keyword_set(noglnt) eq 0 then noglnt=0 else noglnt=1
  if (scimode_ihk eq 0 or  scimode_ihk eq 1 or  scimode_ihk eq 4) and $
    elec_ion_m1 eq 0 then begin
    ;test for mode change
      if swest.mode_this_file eq 2 then begin
        err='change from mode'+string(swest.mode_this_file,format='(i2)')+$
         ' to mode1; mode1 background and glint required'
        return
      endif 
    mode1,lpr=lpr,noglnt=noglnt,err=err,norelgains=norelgains
    
    if err ne '' then begin
      print,err & return
    endif
  endif else if scimode_ihk eq 2 or scimode_ihk eq 11 then begin
    ;test for mode change
      if swest.mode_this_file eq 1 or swest.mode_this_file eq 6 then begin
        err='change from mode'+string(swest.mode_this_file,format='(i2)')+$
         ' to mode2; mode2 background and glint required'         
        return
      endif 
    mode2,eleion_sweep,lpr=lpr,noglnt=noglnt,err=err
  endif else if (scimode_ihk eq 0 or  scimode_ihk eq 1 $
    or  scimode_ihk eq 4) and $
  elec_ion_m1 eq 1 then begin
    print,'proc_rec: mode 1 ion mode'
    return
  endif else if scimode_ihk eq 6 then begin
    ;test for mode change 
      if swest.mode_this_file eq 2 then begin
        err='change from mode'+string(swest.mode_this_file,format='(i2)')+$
         ' to mode6'
        ;return
      endif
    mode6,lpr=lpr,noglnt=noglnt,err=err,norelgains=norelgains  
  endif else begin
  
    print,'proc_rec: not in a known mode'
    return
  endelse
ifsci=1

end
