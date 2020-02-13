
;============== function wav_nekpvar =========================================


;returns data variable corresponding to the variable name given in panelist.pro
; and mstruct.pro

function wav_nekpvar,varname

common shared,d

idatype=where(d.datype eq 'wav_nekp')

id1=d.ndx(0,idatype) & id2=d.ndx(1,idatype)

print,'wav_nekpvar : varname ',varname

case varname of

  'wav_nekp': return,d.wav_nekpdat(id1:id2).n  

  'wav_qnekp': return,d.wav_nekpdat(id1:id2).q

endcase

end
