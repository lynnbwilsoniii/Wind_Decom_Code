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
  en=volt_en(d.swe_pdat(d.ndx(0,idatyp)).vsteps(d.pnlist.ev_val(k)-1),/en)
  d.pnl(k).labl=string(en,format='(i4)')+' ev'         
  d.pnl(k).range=[0,180] ;d.pnl(k).lzrange(*,wst.cf)
  d.pnl(k).ztitle='SWE electrons'
endelse

end

