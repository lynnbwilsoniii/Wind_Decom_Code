
;============== function magkpvar =============================================

;returns data variable corresponding to the variable name given in panelist.pro
; and mstruct.pro

function mfi_magkpvar,varname

common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common shared,d

idatype=where(d.datype eq 'mfi_magkp')
idatype_swe_ionkp=where(d.datype eq 'swe_ionkp')

id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)
fill=-1.e31

case varname of


'mag_bkp': return,d.mfi_magkpdat(id1:id2).b

'th_bkp': return,d.mfi_magkpdat(id1:id2).th

'ph_bkp': return,d.mfi_magkpdat(id1:id2).ph

'bx_kp' : return,d.mfi_magkpdat(id1:id2).bf(0)

'by_kp' : return,d.mfi_magkpdat(id1:id2).bf(1)

'bz_kp' : return,d.mfi_magkpdat(id1:id2).bf(2)

'magkp pressure' : return,d.mfi_magkpdat(id1:id2).magp*1e10

'R fshck': begin
   if d.flnm(idatype_swe_ionkp) eq '' then return,-1
   jd1=d.ndx(0,idatype_swe_ionkp) & jd2=d.ndx(1,idatype_swe_ionkp)
   ;press=1.67e-27 * d.swe_ionkpdat(jd1:jd2).n * $
     ;1e6 *(d.swe_ionkpdat(jd1:jd2).v*1e3)^2 *1e9
   dens=interpol(d.swe_ionkpdat(jd1:jd2).n,d.swe_ionkpdat(jd1:jd2).ta,$
     d.mfi_magkpdat(id1:id2).ta)
   umag=interpol(d.swe_ionkpdat(jd1:jd2).v,d.swe_ionkpdat(jd1:jd2).ta,$
     d.mfi_magkpdat(id1:id2).ta)
   bx=d.mfi_magkpdat(id1:id2).b*$
     cos(d.mfi_magkpdat(id1:id2).th*!dtor)*cos(d.mfi_magkpdat(id1:id2).ph*!dtor)
   by=d.mfi_magkpdat(id1:id2).b*$
     cos(d.mfi_magkpdat(id1:id2).th*!dtor)*sin(d.mfi_magkpdat(jd1:id2).ph*!dtor)
   bz=d.mfi_magkpdat(id1:id2).b*sin(d.mfi_magkpdat(id1:id2).th*!dtor)
   mindx=fix(float(d.mfi_magkpdat(id1:id2).tpb5(2))/600000.)
   re=6373.
   gsex=gse_pos(mindx,0)/re
   gsey=gse_pos(mindx,0)/re
   gsez=gse_pos(mindx,0)/re
   
   rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x
   return,r
           endcase

'X fshck': begin
   if d.flnm(idatype_swe_ionkp) eq '' then return,-1
   jd1=d.ndx(0,idatype_swe_ionkp) & jd2=d.ndx(1,idatype_swe_ionkp)
   ;press=1.67e-27 * mdat(id1:id2).fnout * 1e6 * (mdat(jd1:jd2).umag*1e3)^2 *1e9
   dens=interpol(d.swe_ionkpdat(jd1:jd2).n,d.swe_ionkpdat(jd1:jd2).ta,$
     d.mfi_magkpdat(id1:id2).ta)
   umag=interpol(d.swe_ionkpdat(jd1:jd2).v,d.swe_ionkpdat(jd1:jd2).ta,$
     d.mfi_magkpdat(id1:id2).ta)
   bx=d.mfi_magkpdat(id1:id2).b*$
     cos(d.mfi_magkpdat(id1:id2).th*!dtor)*cos(d.mfi_magkpdat(id1:id2).ph*!dtor)
   by=d.mfi_magkpdat(id1:id2).b*$
     cos(d.mfi_magkpdat(id1:id2).th*!dtor)*sin(d.mfi_magkpdat(id1:id2).ph*!dtor)
   bz=d.mfi_magkpdat(id1:id2).b*sin(d.mfi_magkpdat(id1:id2).th*!dtor)
   mindx=fix(float(d.mfi_magkpdat(jd1:id2).tpb5(2))/600000.)
   re=6373.
   gsex=gse_pos(mindx,0)/re
   gsey=gse_pos(mindx,0)/re
   gsez=gse_pos(mindx,0)/re
   
   rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x
   return,x
           endcase

 
endcase
end
