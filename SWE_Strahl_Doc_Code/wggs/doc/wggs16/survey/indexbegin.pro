;============================= indexbegin =================================

pro indexbegin,x

common shared,d

;indx_begin finds time at point x


for idatype=0,d.ndvar-1 do begin
  if d.flnm(idatype) ne '' then begin
    datim=call_function(d.datype(idatype)+'_datatime')
    d.ndx_buff(0,idatype)=indx_begin(datim/3600.d,x)
  endif  
endfor    
hour_hms,x,hms
print,hms

end
