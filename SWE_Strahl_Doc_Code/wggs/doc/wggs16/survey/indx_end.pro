
function indx_end,t,tsel

w=where(tsel - t gt 0,nw)
if nw gt 0 then id2=w(nw-1)
;print,'indx_end: ',id2,t(id2)
return, id2

end

