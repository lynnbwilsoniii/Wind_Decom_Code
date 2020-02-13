
;============== function mvar =============================================

;returns data variable corresponding to the variable name given in panelist.pro
; and mstruct.pro

function swe_momentsvar,varname,err=err,varerr=varerr
common shared,d
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common wstuff,wst

idatype=where(d.datype eq 'swe_moments')
idatype_swe_ionkp=where(d.datype eq 'swe_ionkp')

err=0

id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)

momcdf=strmid(d.flnm(idatype),strlen(d.flnm(idatype))-3,3) eq 'cdf'

if momcdf eq 0 then begin     ;NOT a cdf file

  ver=strmid(d.flnm(idatype),strlen(d.flnm(idatype))-6,2)
                  
  case varname of

  'N density': return,d.swe_mdat(id1:id2).fnout

  'U flow speed': return,d.swe_mdat(id1:id2).umag

  'th_u': return,d.swe_mdat(id1:id2).theu

  'ph_u': return,d.swe_mdat(id1:id2).phiu

  'Ux (gse)': return,d.swe_mdat(id1:id2).uout(0)

  'Uy (gse)': return,d.swe_mdat(id1:id2).uout(1)

  'Uz (gse)': return,d.swe_mdat(id1:id2).uout(2)

  'T temperature': return,d.swe_mdat(id1:id2).trout

  'A anisotropy': return,1.0/d.swe_mdat(id1:id2).ettrt   ;ettrt=tperp/tpara

  'T parallel' : $
    return,3*d.swe_mdat(id1:id2).trout/(1.+2*d.swe_mdat(id1:id2).ettrt)
                 
  'T perpendicular' : $
    return,3*d.swe_mdat(id1:id2).trout*d.swe_mdat(id1:id2).ettrt/$
      (1.+2*d.swe_mdat(id1:id2).ettrt)
  
  'T eV': begin
     boltzk=8.617e-5     ;ev/degK
     tev=boltzk * d.swe_mdat(id1:id2).trout
     return,tev
          endcase
  
  'T para eV' : begin
    boltzk=8.617e-5     ;ev/degK
    tparev=boltzk * 3*d.swe_mdat(id1:id2).trout/(1.+2*d.swe_mdat(id1:id2).ettrt)
    return,tparev
                endcase
  
  'T perp eV' : begin
    boltzk=8.617e-5     ;ev/degK
    tperev=boltzk * 3*d.swe_mdat(id1:id2).trout*d.swe_mdat(id1:id2).ettrt/$
       (1.+2*d.swe_mdat(id1:id2).ettrt)
    return,tperev                        
                endcase
                                                   
  'Q heat flux': return,d.swe_mdat(id1:id2).hmag

  'th_q': return,d.swe_mdat(id1:id2).theh

  'ph_q': return,d.swe_mdat(id1:id2).phih

  'Bx': return,d.swe_mdat(id1:id2).b(0)

  'By': return,d.swe_mdat(id1:id2).b(1)

  'Bz': return,d.swe_mdat(id1:id2).b(2)

  'B magnetic field': return,d.swe_mdat(id1:id2).bmag

  'th_b': return,d.swe_mdat(id1:id2).theb

  'ph_b': return,d.swe_mdat(id1:id2).phib

  'gyrtrpy': return,d.swe_mdat(id1:id2).gyrtrpy

  'W thermal speed': $
    return,sqrt(d.swe_mdat(id1:id2).eavg*1.6022e-12/9.1095e-28)*1e-5

  'cos(Pa,B)': begin
      paxismag=sqrt(d.swe_mdat(id1:id2).paxis(0)^2+$
        d.swe_mdat(id1:id2).paxis(1)^2+d.swe_mdat(id1:id2).paxis(2)^2)
      return,(d.swe_mdat(id1:id2).paxis(0)*d.swe_mdat(id1:id2).b(0)+ $
            d.swe_mdat(id1:id2).paxis(1)*d.swe_mdat(id1:id2).b(1)+ $
            d.swe_mdat(id1:id2).paxis(2)*d.swe_mdat(id1:id2).b(2))/$
            (d.swe_mdat(id1:id2).bmag * paxismag)
    endcase

  'cos(Q,B)': return,d.swe_mdat(id1:id2).qdotb

  'Qx (gse)': return,d.swe_mdat(id1:id2).hout(0)

  'Qy (gse)': return,d.swe_mdat(id1:id2).hout(1)

  'Qz (gse)': return,d.swe_mdat(id1:id2).hout(2)

  'magnetic pressure': return,1e10 * d.swe_mdat(id1:id2).magp 

  'elec plas pressure': $
      return,1e10*d.swe_mdat(id1:id2).fnout*1.38e-16*d.swe_mdat(id1:id2).trout

  'enavg': return, d.swe_mdat(id1:id2).eavg

  'R fshck': begin
     dens=d.swe_mdat(id1:id2).fnout
     umag=d.swe_mdat(id1:id2).umag
     bx=d.swe_mdat(id1:id2).b(0)
     by=d.swe_mdat(id1:id2).b(1)
     bz=d.swe_mdat(id1:id2).b(2)
     mindx=fix(float(d.swe_mdat(id1:id2).tpb5(2))/600000.)
     re=6373.
     gsex=gse_pos(mindx,0)/re
     gsey=gse_pos(mindx,0)/re
     gsez=gse_pos(mindx,0)/re
   
     rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x,/new
     return,r
           endcase

  'X fshck': begin
     dens=d.swe_mdat(id1:id2).fnout
     umag=d.swe_mdat(id1:id2).umag
     bx=d.swe_mdat(id1:id2).b(0)
     by=d.swe_mdat(id1:id2).b(1)
     bz=d.swe_mdat(id1:id2).b(2)
     mindx=fix(float(d.swe_mdat(id1:id2).tpb5(2))/600000.)
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


  'A fshck': begin
     dens=d.swe_mdat(id1:id2).fnout
     umag=d.swe_mdat(id1:id2).umag
     bx=d.swe_mdat(id1:id2).b(0)
     by=d.swe_mdat(id1:id2).b(1)
     bz=d.swe_mdat(id1:id2).b(2)
     mindx=fix(float(d.swe_mdat(id1:id2).tpb5(2))/600000.)
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
     return,atan(x/abs(r))/!dtor
           endcase
                      
   'Xtanpt' :  begin
     dens=d.swe_mdat(id1:id2).fnout
     umag=d.swe_mdat(id1:id2).umag
     bx=d.swe_mdat(id1:id2).b(0)
     by=d.swe_mdat(id1:id2).b(1)
     bz=d.swe_mdat(id1:id2).b(2)
     mindx=fix(float(d.swe_mdat(id1:id2).tpb5(2))/600000.)
     re=6373.
     gsex=gse_pos(mindx,0)/re
     gsey=gse_pos(mindx,1)/re
     gsez=gse_pos(mindx,2)/re
   
     rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x,x0r=x_tanpt
     return,x_tanpt
           endcase        

           
  'ratio e/i density': begin
     if d.flnm(4) ne '' then begin
       if d.swe_ionkpdat(n_elements(d.swe_ionkpdat)-1).hrday gt 0.1 then $
       nionkp=n_elements(d.swe_ionkpdat) else nionkp=n_elements(d.swe_ionkpdat)-1
       return,d.swe_mdat(id1:id2).misc(5) 
     endif
                          endcase

  'scpot': return,d.swe_mdat(id1:id2).spcpot

  'N_error' : return,d.swe_mdat(id1:id2).misc(8)

  'T_error' : return,d.swe_mdat(id1:id2).misc(9)

  'v_Alfvenic' : begin
     v=fltarr(id2-id1+1)
     dv=21.8*(shift(d.swe_mdat(id1:id2).b(0),-1)-d.swe_mdat(id1:id2).b(0))/$
       (1.4*(d.swe_mdat(id1:id2).fnout+shift(d.swe_mdat(id1:id2).fnout,-1))/2) 
     dv(0)=0.
     v(0)=d.swe_mdat(id1).uout(0)                 
     for i=1,id2-id1 do v(i)=v(i-1)+dv(i)
     return,v  
               endcase
               
  'Qnml_qtherm' : begin
     emass=9.11e-28
     tepar=d.swe_mdat(id1:id2).trout/(1.+2*d.swe_mdat(id1:id2).ettrt)
     veth=sqrt((2*1.38e-16*tepar)/emass)
     return,d.swe_mdat(id1:id2).hmag/$
         (d.swe_mdat(id1:id2).fnout*emass*veth*veth*veth)
                endcase 

  'X (gse)' : return, d.swe_mdat(id1:id2).rgse(0)

  'Y (gse)' : return, d.swe_mdat(id1:id2).rgse(1)

  'Z (gse)' : return, d.swe_mdat(id1:id2).rgse(2)

  'N core'  : begin
              if ver ge '13' then return, d.swe_mdat(id1:id2).misc(13) $
              else begin
                ncore=fltarr(id2-id1+1)
                for i=id1,id2 do begin
                  bnfit=d.swe_mdat(i).bnfit
                  fcore,bnfit,dne_core,te_core,u_core
                  ncore(i-id1)=dne_core
                endfor
              return, ncore  
              endelse
            endcase  

  'T core'  : begin
              if ver ge '13' then return, d.swe_mdat(id1:id2).misc(14)  $
              else begin
                tcore=fltarr(id2-id1+1)
                for i=id1,id2 do begin
                  bnfit=d.swe_mdat(i).bnfit
                  fcore,bnfit,dne_core,te_core,u_core
                  tcore(i-id1)=te_core
                endfor
              return, tcore  
              endelse
            endcase  

  'U core flow speed': begin  
    ucore=fltarr(id2-id1+1)
    for i=id1,id2 do begin
      bnfit=d.swe_mdat(i).bnfit
      fcore,bnfit,dne_core,te_core,u_core
      ucore(i-id1)=sqrt(u_core(0)^2+u_core(1)^2+u_core(2)^2)
    endfor
    return, ucore*1e-5
  endcase  

  'th_u core': begin  
    thucore=fltarr(id2-id1+1)
    for i=id1,id2 do begin
      bnfit=d.swe_mdat(i).bnfit
      fcore,bnfit,dne_core,te_core,u_core
      ucore=sqrt(u_core(0)^2+u_core(1)^2+u_core(2)^2)
      thucore(i-id1)=asin(u_core(2)/ucore)/!dtor
    endfor
    return, thucore
  endcase

  'ph_u core': begin
    phucore=fltarr(id2-id1+1)
    for i=id1,id2 do begin
      bnfit=d.swe_mdat(i).bnfit
      fcore,bnfit,dne_core,te_core,u_core
      phucore(i-id1)=atan(u_core(1),u_core(0))/!dtor
    endfor
    wphlt0=where(phucore lt 0,nwphlt0)
    if nwphlt0 gt 0 then phucore(wphlt0)=phucore(wphlt0)+360.
    return, phucore
  endcase

  'Ni (interpolated)' : begin
     ni_interpol = interpol($
            d.swe_ionkpdat(d.ndx(0,idatype_swe_ionkp):$
                 d.ndx(1,idatype_swe_ionkp)).n,$
           (d.swe_ionkpdat(d.ndx(0,idatype_swe_ionkp):$
                 d.ndx(1,idatype_swe_ionkp)).ta-d.refsec)/3600.d,$
           (d.swe_mdat(id1:id2).ta-d.refsec)/3600.d)
    return,ni_interpol
  endcase
  
  'Ui (interpolated)' : begin
     ui_interpol = interpol($
            d.swe_ionkpdat(d.ndx(0,idatype_swe_ionkp):$
                 d.ndx(1,idatype_swe_ionkp)).v,$
           (d.swe_ionkpdat(d.ndx(0,idatype_swe_ionkp):$
                 d.ndx(1,idatype_swe_ionkp)).ta-d.refsec)/3600.d,$
           (d.swe_mdat(id1:id2).ta-d.refsec)/3600.d)
    return,ui_interpol
  endcase

   'pxx' : return, d.swe_mdat(id1:id2).pout(0,0) *1.e10
  
  'pxy' : return, d.swe_mdat(id1:id2).pout(0,1)  *1.e10
  
  'pxz' : return, d.swe_mdat(id1:id2).pout(0,2)  *1.e10
  
  'pyy' : return, d.swe_mdat(id1:id2).pout(1,1)  *1.e10
  
  'pyz' : return, d.swe_mdat(id1:id2).pout(1,2)  *1.e10
  
  'pzz' : return, d.swe_mdat(id1:id2).pout(2,2)  *1.e10
  
  'pyx' : return, d.swe_mdat(id1:id2).pout(1,0)  *1.e10
  
  'pzx' : return, d.swe_mdat(id1:id2).pout(2,2)  *1.e10-$
     d.swe_mdat(id1:id2).pout(0,0)  *1.e10
  
  'pzy' : return, d.swe_mdat(id1:id2).pout(2,2)  *1.e10-$
     d.swe_mdat(id1:id2).pout(1,1)  *1.e10
  
  'pzz/pxx' : begin
     zz=d.swe_mdat(id1:id2).pout(2,2) *1.e10
     xx=d.swe_mdat(id1:id2).pout(0,0) *1.e10
     r=fltarr(id2-id1+1)
     w=where(xx ne 0)
     r(w)=zz(w)/xx(w)
     return,r
  endcase
  
  'nodata' : return,replicate(0,id2-id1+1)

  endcase
  
endif $



else begin  ;YES moments cdf file

  case varname of

  'N density': return,d.swe_mdat(id1:id2).fnout

  'U flow speed': return,d.swe_mdat(id1:id2).umag

  'th_u': return,d.swe_mdat(id1:id2).theu

  'ph_u': return,d.swe_mdat(id1:id2).phiu

  'T temperature': return,d.swe_mdat(id1:id2).trout

  'A anisotropy': return,d.swe_mdat(id1:id2).anis  ;anis=tpara/tperp

  'Q heat flux': return,d.swe_mdat(id1:id2).hmag

  'th_q': return,d.swe_mdat(id1:id2).theh

  'ph_q': return,d.swe_mdat(id1:id2).phih

  'cos(Pa,B)': return,d.swe_mdat(id1:id2).padotb

  'cos(Q,B)': return,d.swe_mdat(id1:id2).qdotb

  'scpot': return,d.swe_mdat(id1:id2).spcpot

  'X (gse)' : return, d.swe_mdat(id1:id2).rgse(0)

  'Y (gse)' : return, d.swe_mdat(id1:id2).rgse(1)

  'Z (gse)' : return, d.swe_mdat(id1:id2).rgse(2)

  'enavg': return, d.swe_mdat(id1:id2).eavg
 
 
  endcase
   
endelse  
end
