
;============== function windorbit_var =============================================

;returns data variable corresponding to the variable name given in panelist.pro
; and mstruct.pro

function wind_orbitvar,varname,err=err,varerr=varerr
common shared,d
common wstuff,wst

idatype=where(d.datype eq 'wind_orbit')

err=0

id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)

case varname of

'X (gse)' : return, d.wind_orbitdat(id1:id2).gse_pos(0)

'Y (gse)' : return, d.wind_orbitdat(id1:id2).gse_pos(1)

'Z (gse)' : return, d.wind_orbitdat(id1:id2).gse_pos(2)

endcase
end
