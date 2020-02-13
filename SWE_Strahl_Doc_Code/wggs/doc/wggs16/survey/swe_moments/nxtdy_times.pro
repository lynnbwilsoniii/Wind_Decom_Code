pro nxtdy_times,data,fill

;test for next day records within the last  mfj or 7 records

nrec=n_elements(data)
nn=7
ta=data(nrec-1-nn+1+indgen(nn)).ta
tjd=long(ta/86400.d0)
;for i=0,nn-1 do $
;  print,i,nrec-1-nn+1+i,data(nrec-1-nn+1+i).ta,ta(i),tjd(i)
this_tjd=long(data(nrec-1-nn).ta/86400.d0)  
w=where(tjd ne this_tjd)
if w(0) ne -1 then begin
  inxtdy=nrec-1-nn+1 + w
  data(nrec-1-nn+1 + w).b = fill  
endif

end