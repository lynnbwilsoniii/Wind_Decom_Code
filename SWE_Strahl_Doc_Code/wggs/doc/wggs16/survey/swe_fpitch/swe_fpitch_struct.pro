pro swe_fpitch_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

idatype=where(d.datype eq 'swe_fpitch')

d.pnl(k).pltype='z(x,y)'

if swe_moments eq 0 then $ 
  d.pnl(k).subtitle=strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatype))),$
  strlen(d.flnm(idatyp))-strlen(getenv(d.pathenv(idatype))))

if varbl eq 'Spin avg f(en,t)' then begin
  d.pnl(k).labl='log eV'
  d.pnl(k).range=[2,15] 
endif else begin
  ;    MPH (10/10/2003)--This updates fpitch labelling to reflect correct mode.
  if ((ymd_pb5(long(wst.indate)))[0] ge 2002l) then begin ;   2002-and-after...
     ; Note: Due to VEIS instrument failure in mid-2001, only new-mode (mode7)
     ;        electron pitch-angle data exists for 2002-and-after.
     print,"Using NEW MODE (Strahl) energy list." ;                 Diagnostic.
     en = $ ;                                            Use STRAHL energies...
     volt_en_strl(d.swe_pdat[d.ndx[0,idatyp]].vsteps[d.pnlist.ev_val[k]],  /En)
  endif else begin ;                     For OLD-MODE data use VEIS energies...
     ; Note: Electron pitch-angle data from 2001-and-before must be old-mode.
     print,"Using OLD MODE (VEIS) energy list." ;                   Diagnostic.
     en = volt_en(d.swe_pdat[d.ndx[0,idatyp]].vsteps[d.pnlist.ev_val[k]-1],/En)
  endelse
  d.pnl(k).labl=string(en,format='(I4)')+' eV'         
  d.pnl(k).range=[0,180] ;d.pnl(k).lzrange(*,wst.cf)
  d.pnl(k).ztitle='SWE electrons'
endelse

end

