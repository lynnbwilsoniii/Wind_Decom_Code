
;============== function wav_nehrvar =========================================


;returns data variable corresponding to the variable name given in panelist.pro
; and mstruct.pro

function wav_nehrvar,varname

common shared,d

idatype=where(d.datype eq 'wav_nehr')

id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)
print,'wav_nehrvar : varname ',varname

case varname of

  'wav_nehr': return,d.wav_nehrdat(id1:id2).n

  'wav_qnehr': return,d.wav_nehrdat(id1:id2).q

endcase

end
