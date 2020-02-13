pro dupli_times,data
 
;test for for non-advancing ta in the last 3 mjf's or 21 data records
;( this is a bug in _v13.mom and _sv13.mom files)
  
  nrec=n_elements(data)
    
  nn=21
  ta=data(nrec-1-nn+1+indgen(nn)).ta 
  dta=lonarr(nn-1)
  for i=1,nn-1 do begin
    dta(i-1)=ta(i)-ta(i-1)
    ;print,i,data(nrec-1-nn+1+i).ta,ta(i),nrec-1-nn+1+i,dta(i-1)
  endfor  
  w=where(dta le 0)
  
  ;if non-advancing ta is found, then truncate the array
  if w(0) ne -1 then data=data(0:nrec-1-nn+1+w(0))
  
end  