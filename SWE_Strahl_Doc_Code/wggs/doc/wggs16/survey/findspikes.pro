pro findspikes,f,spikind,width=width,medf=medf,test=test

if keyword_set(width) eq 0 then width=5
if keyword_set(test) eq 0 then test=0


medf=median(float(f),width)
diff=float(f)-medf
avg=total(medf)/n_elements(medf)
spikind=where(diff gt 2*sqrt(avg))           ;spikes indices

  
if test then begin 
  g=float(f) 
  window,0
  plot,g,title='input data'
  oplot,medf,color=55
  
  window,1
  plot,diff,title='f - median(f,width= '+string(width,format='(i2)')+' )'
  
  window,2
  if spikind(0) ne -1 then g(spikind)=medf(spikind)
  plot,g,title='spikes removed width= '+string(width,format='(i2)')
  oplot,medf,color=55
endif  



end