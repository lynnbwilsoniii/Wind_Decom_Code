
;============== function exp_dtypvar =============================================

;returns data variable, e.g.,d.exp_dtypdat(id1:id2).varbl1  
;corresponding to the variable name defined in panelist.pro, e.g., "var1".

function exp_dtypvar,varname

common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common shared,d

idatype=where(d.datype eq 'exp_dtyp')

id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)
fill=-1.e31

case varname of

'var1': return,d.exp_dtypdat(id1:id2).varbl1

'var2': return,d.exp_dtypdat(id1:id2).varbl2

'var3': return,d.exp_dtypdat(id1:id2).varbl3

'var4' : return,d.exp_dtypdat(id1:id2).varbl4

endcase
end
