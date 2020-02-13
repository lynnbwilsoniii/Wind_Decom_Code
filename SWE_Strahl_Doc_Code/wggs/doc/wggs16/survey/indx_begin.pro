
function indx_begin,t,tsel

w=where( t - tsel ge 0,nw)
if nw gt 0 then id1=w(0) else id1=0
;print,'indx_begin: ',id1,t(id1)
return, id1

end

