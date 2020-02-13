;pro fpitchavg_read,tjd0_thisfile=tjd0


print,'fpitchavg_read :'


   
  openr,lunp,'/data1/swe/mpnew/19980826_v05.pitavg',/get_lun

  

  ;read header
  nrec=0l
     hedr={nrec:0l,thisdate:string('',format='(i8)'),$
            date:string('',format='(a8)'),$
            scimode:0l,oknoatt:0l,$
            ndets:0l,nvsteps:0l,nsectors:0l,$
            glnt:lonarr(3,64),ensteptbl:fltarr(64),$
            max_enstep:0l,$
            thisrecn:0l,scimodechange:0l,dummy:0l}
      
    readu,lunp,hedr
    nrec=hedr.nrec
 
   
  indat={  $
      mfrec:0l,  $
      mfspinbl:0l,  $
      ta:0.d,  $
      pb5tim:lonarr(3),  $
      velocity:fltarr(16),  $
      fspa:fltarr(16),   $
      pbin:fltarr(5), $
      f:fltarr(4,16),   $
      b:fltarr(3),$
      eleion:0l,  $
      misc:fltarr(9),$
      misc2:bytarr(8),$
      gains:fltarr(6),$
      rgse:fltarr(3)  }
      
  newpfrmt=1

  data=replicate(indat,nrec)

  readu,lunp,data
  free_lun,lunp

for i=0,n_elements(data)-1 do print,i,pb5_ymdhms(data(i).pb5tim),$
  double(data(i).pb5tim(2))/double(3600000l),format='(i5,a20,f10.4)'
id1=0;50  ;66
id2=n_elements(data)-1;100 ;89
ndat=id2-id1+1

;put input data into structure pavgdat


pavgdat=replicate($
   {tpb5:lonarr(3),$
   time:'',$
   velocity:fltarr(16),$
   energy:fltarr(16),$
   pbin:fltarr(5), $
   f:fltarr(4,16),$
   b:fltarr(3)}, $
   ndat)

;input data
pavgdat.pbin=data(id1:id2).pbin
pavgdat.f=data(id1:id2).f
pavgdat.velocity=data(id1:id2).velocity
pavgdat.energy=2.85e-16 * data(id1:id2).velocity^2
pavgdat.b=data(id1:id2).b
pavgdat.tpb5=data(id1:id2).pb5tim
for i=0,ndat-1 do pavgdat(i).time=pb5_ymdhms(data(i-id1).pb5tim)
data=0

save,filename=getenv('IDLSAV')+'pitchavg.dat',pavgdat
pavgdat=0
restore,getenv('IDLSAV')+'pitchavg.dat'
help,pavgdat
help,pavgdat,/str
print,'index,   date,   hhmmss,  hour of day'
for i=0,n_elements(pavgdat)-1 do print,i,pavgdat(i).time,$
  double(pavgdat(i).tpb5(2))/double(3600000l),format='(i5,a25,f12.5)'

;select time indices to plot
i1=50  ;0
i2=100  ;n_elements(pavgdat)-1

print,' '
print,'index    pitchangle bin'
for i=0,3 do print,i,pavgdat(0).pbin(i),pavgdat(0).pbin(i+1)
;select pitch angle bin to plot
j=3 ;2 ;1 ;0

;stop
window,0

hrday=double(pavgdat(i1:i2).tpb5(2))/double(3600000l)
f=fltarr(4,16,n_elements(pavgdat(i1:i2)))
f=pavgdat(i1:i2).f

yrange=[1e-31,1e-25] 
xrange=[hrday(0),hrday(n_elements(hrday)-1)]
vel=pavgdat(i1).velocity
en=pavgdat(i1).energy

pbin=[pavgdat(id1).pbin(j),pavgdat(id1).pbin(j+1)]
print,pbin

plot,xrange,yrange,/nodata,/ylog,ystyle=1,xstyle=1,$
  xtitle='hour of day 19980826',ytitle='phase density',$
  title='pitch angle bin: '+string(pbin,format='(2i4)') 

;select energy step indices
k1=5  ;0
k2=15
for k=k1,k2 do begin
  oplot,hrday,f(j,k,*)
  oplot,hrday,f(j,k,*),psym=4,symsize=0.6
  xyouts,hrday(0),f(j,k,0),string(en(k),format='(i3)'),alignment=-1.0,/data
  print,en(k)
endfor  

end
