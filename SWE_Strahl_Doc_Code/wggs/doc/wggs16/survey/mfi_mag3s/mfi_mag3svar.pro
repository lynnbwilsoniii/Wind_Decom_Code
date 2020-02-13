
;============== function mag3svar =============================================

;returns data variable corresponding to the variable name given in panelist.pro
; and mstruct.pro

function mfi_mag3svar,varname

common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common shared,d

idatype=where(d.datype eq 'mfi_mag3s')
idatype_swe_ionkp=where(d.datype eq 'swe_ionkp')

id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)

case varname of


'mag_b3s': return,d.mfi_mag3sdat(id1:id2).b

'th_b3s': return,d.mfi_mag3sdat(id1:id2).th

'ph_b3s': return,d.mfi_mag3sdat(id1:id2).ph


'R fshck': begin
   if d.flnm(idatype_swe_ionkp) eq '' then return,-1
   jd1=d.ndx(0,idatype_swe_ionkp) & jd2=d.ndx(1,idatype_swe_ionkp)
   dens=interpol(d.swe_ionkpdat(jd1:jd2).n,d.swe_ionkpdat(jd1:jd2).ta,$
     d.mfi_mag3sdat(id1:id2).ta)
   umag=interpol(d.swe_ionkpdat(jd1:jd2).v,d.swe_ionkpdat(jd1:jd2).ta,$
     d.mfi_mag3sdat(id1:id2).ta)
   bx=d.mfi_mag3sdat(id1:id2).b*$
     cos(d.mfi_mag3sdat(id1:id2).th*!dtor)*cos(d.mfi_mag3sdat(id1:id2).ph*!dtor)
   by=d.mfi_mag3sdat(id1:id2).b*$
     cos(d.mfi_mag3sdat(id1:id2).th*!dtor)*sin(d.mfi_mag3sdat(jd1:id2).ph*!dtor)
   bz=d.mfi_mag3sdat(id1:id2).b*sin(d.mfi_mag3sdat(id1:id2).th*!dtor)
   mindx=fix(float(d.mfi_mag3sdat(id1:id2).tpb5(2))/600000.)
   re=6373.
   gsex=gse_pos(mindx,0)/re
   gsey=gse_pos(mindx,1)/re
   gsez=gse_pos(mindx,2)/re
   
   rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x,/new
   return,r
           endcase

'X fshck': begin
   if d.flnm(4) eq '' then return,-1
   jd1=d.ndx(0,4) & jd2=d.ndx(1,4)
   
   dens=interpol(d.swe_ionkpdat(jd1:jd2).n,d.swe_ionkpdat(jd1:jd2).ta,$
     d.mfi_mag3sdat(id1:id2).ta)
   umag=interpol(d.swe_ionkpdat(jd1:jd2).v,d.swe_ionkpdat(jd1:jd2).ta,$
     d.mfi_mag3sdat(id1:id2).ta)
   bx=d.mfi_mag3sdat(id1:id2).b*$
     cos(d.mfi_mag3sdat(id1:id2).th*!dtor)*cos(d.mfi_mag3sdat(id1:id2).ph*!dtor)
   by=d.mfi_mag3sdat(id1:id2).b*$
     cos(d.mfi_mag3sdat(id1:id2).th*!dtor)*sin(d.mfi_mag3sdat(id1:id2).ph*!dtor)
   bz=d.mfi_mag3sdat(id1:id2).b*sin(d.mfi_mag3sdat(id1:id2).th*!dtor)
   mindx=fix(float(d.mfi_mag3sdat(id1:id2).tpb5(2))/600000.)
   re=6373.
   gsex=gse_pos(mindx,0)/re
   gsey=gse_pos(mindx,1)/re
   gsez=gse_pos(mindx,2)/re
   
   rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x,/new
   shckerr=0.05  ;0.10  ;0.15  ;x100 %
   magerr=5.  ;10.  ;15.   ;degrees
   if shckerr ne 0 or magerr ne 0 then begin 
     varerr=fltarr(2,n_elements(x)) 
     rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,xx,/new,$
       shckerr=shckerr,magerr=magerr
     varerr(0,*)=xx
     rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,xx,/new,$
       shckerr=-shckerr,magerr=magerr
     varerr(1,*)=xx
     err=1
   endif  
   return,x
           endcase

 
endcase
end
