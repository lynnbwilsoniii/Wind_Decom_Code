
;============== function mvar =============================================

;returns data variable corresponding to the variable name given in panelist.pro
; and mstruct.pro

function isee_momentsvar,varname
common shared,d

idatype=where(d.datype eq 'isee_moments')
id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)

re=6373.

case varname of

'N density': return,d.iseemdat(id1:id2).fnout

'U flow speed': return,d.iseemdat(id1:id2).umag

'th_u': return,d.iseemdat(id1:id2).theu

'ph_u': return,d.iseemdat(id1:id2).phiu

'Ux (gse)': return,d.iseemdat(id1:id2).uout(0)

'Uy (gse)': return,d.iseemdat(id1:id2).uout(1)

'Uz (gse)': return,d.iseemdat(id1:id2).uout(2)

'T temperature': return,d.iseemdat(id1:id2).trout

'A anisotropy': return,d.iseemdat(id1:id2).ettrt      

'Q heat flux': return,d.iseemdat(id1:id2).hmag

'th_q': return,d.iseemdat(id1:id2).theh

'ph_q': return,d.iseemdat(id1:id2).phih

'Bx': return,d.iseemdat(id1:id2).b(0)

'By': return,d.iseemdat(id1:id2).b(1)

'Bz': return,d.iseemdat(id1:id2).b(2)

'B magnetic field': return,d.iseemdat(id1:id2).bmag

'th_b': return,d.iseemdat(id1:id2).theb

'ph_b': return,d.iseemdat(id1:id2).phib

'cos(Pa,B)': return,d.iseemdat(id1:id2).cosb
  
'cos(Q,B)': return,d.iseemdat(id1:id2).qdotb

'Qx (gse)': return,d.iseemdat(id1:id2).fhout(0)

'Qy (gse)': return,d.iseemdat(id1:id2).fhout(1)

'Qz (gse)': return,d.iseemdat(id1:id2).fhout(2)



'R fshck': begin
   press=1.67e-27 * d.iseemdat(id1:id2).fnout * 1e6 * $
         (d.iseemdat(id1:id2).umag*1e3)^2 *1e9
   dens=d.iseemdat(id1:id2).fnout
   umag=d.iseemdat(id1:id2).umag
   bx=d.iseemdat(id1:id2).b(0)
   by=d.iseemdat(id1:id2).b(1)
   bz=d.iseemdat(id1:id2).b(2)
   mindx=fix(float(d.iseemdat(id1:id2).tpb5(2))/600000.)
   gsex=d.iseemdat.gsex/re
   gsey=d.iseemdat.gsey/re
   gsez=d.iseemdat.gsez/re
   
   rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x
   return,r
           endcase

'X fshck': begin
   press=1.67e-27 * d.iseemdat(id1:id2).fnout * 1e6 * $
         (d.iseemdat(id1:id2).umag*1e3)^2 *1e9
   dens=d.iseemdat(id1:id2).fnout
   umag=d.iseemdat(id1:id2).umag
   bx=d.iseemdat(id1:id2).b(0)
   by=d.iseemdat(id1:id2).b(1)
   bz=d.iseemdat(id1:id2).b(2)
   mindx=fix(float(d.iseemdat(id1:id2).tpb5(2))/600000.)
   gsex=d.iseemdat.gsex/re
   gsey=d.iseemdat.gsey/re
   gsez=d.iseemdat.gsez/re
   
   rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x
   return,x
           endcase

's/c potential'       : return,d.iseemdat(id1:id2).potnew

'active exp (mozer)'  : return,d.iseemdat(id1:id2).mozer

'active exp (harvey)' : return,d.iseemdat(id1:id2).harvey

'mode'                : return,d.iseemdat(id1:id2).mode
 
'format'              : return,d.iseemdat(id1:id2).format
 
'X (gse)'             : return,d.iseemdat(id1:id2).gsex/re
 
'Y (gse)'             : return,d.iseemdat(id1:id2).gsey/re
 
'Z (gse)'             : return,d.iseemdat(id1:id2).gsez/re
 
'n (sector1)'         : return,d.iseemdat(id1:id2).fn(0)
 
'n (sector2)'         : return,d.iseemdat(id1:id2).fn(1)
 
'n (sector3)'         : return,d.iseemdat(id1:id2).fn(2)
 
'n (sector4)'         : return,d.iseemdat(id1:id2).fn(3)
 
'n (sector5)'         : return,d.iseemdat(id1:id2).fn(4)
 
'n (sector6)'         : return,d.iseemdat(id1:id2).fn(5)
 
'deig'                : return,d.iseemdat(id1:id2).deig
 
'eigval0'              : return,d.iseemdat(id1:id2).eigval(0)

'eigval1'              : return,d.iseemdat(id1:id2).eigval(1)
'eigval2'              : return,d.iseemdat(id1:id2).eigval(2)
    
endcase
end
