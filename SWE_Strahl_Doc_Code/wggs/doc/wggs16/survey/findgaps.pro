pro findgaps,elapspn,gapx=gapx,spns=spns,edges

if keyword_set(gapx) eq 0 then gapx=100
if keyword_set(spns) eq 0 then spns=7

s=elapspn-shift(elapspn,1)
wgap=where(s gt gapx,ngaps)
edges=-1
if ngaps gt 0 then begin
  edges=intarr(2*spns*ngaps)
  for i=0,ngaps-1 do begin
    edges(2*spns*i+indgen(spns))=wgap(i)-1-indgen(spns)
    edges(2*spns*i+spns+indgen(spns))=wgap(i)+indgen(spns)
  endfor     
endif


end