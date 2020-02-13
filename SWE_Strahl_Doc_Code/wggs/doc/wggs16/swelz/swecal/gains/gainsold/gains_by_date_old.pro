
;---------------- MAIN: gains_by_date ----------------------------------------

;plots a time history of relative gains for the SWE/VEIS electron spectrometer

common shared,d
common wstuff,wst
common swestuff,swest

clr_blue=(20./168.)*!d.n_colors
clr_green=(80./168.)*!d.n_colors
clr_yellow=(120./168.)*!d.n_colors
clr_red=(160./168.)*!d.n_colors
clr_white=!d.n_colors

dodata=0;1
if dodata eq 0 then goto,plotdata

define_widgets
;setpaths

;---initialize structures (d,wst,swest)
panelist
structuresw

date_begin=19950101l
date_end=20011231l
pb5_range=[[1995l,1l,0l],[2002l,1l,0l]]
xticks=7

pb5ref=ymd_pb5(date_begin)
elapsec_begin=pb5_elapsec(ymd_pb5(date_begin),pb5ref)
elapsec_end=pb5_elapsec(ymd_pb5(date_end),pb5ref)

;---find current date
y=strmid(systime(0),20,4)
mos=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
m=string(where(mos eq strmid(systime(0),4,3))+1,format='(i2)')
d=strmid(systime(0),8,2)
thisdate=y*10000l+m*100l+d*1l
print,'thisdate ',thisdate

sec=dblarr(3000)
rg=fltarr(6,3000)
pb5t=lonarr(5,3000)

k=-1
elapsec=elapsec_begin
while elapsec le elapsec_end do begin
  k=k+1
  elapsec=elapsec_begin+k*double(86400l)
  pb5=elapsec_pb5(elapsec,pb5ref)
  wst.lzdate=string(pb5_ymd(pb5),format='(i8)')
  relgain=fltarr(6)
  rgflnm=getrelgains_tbl_flnm()
  if rgflnm ne '' then begin
    restore,getenv('WGGSBASE')+rgflnm              
    ;help,datetbl,rgtbl
    wdate=where(long(wst.lzdate) eq datetbl,nwdate)
    if nwdate ne 1 then stop,'bad gains table'
    relgain=rgtbl(*,wdate(0))
  endif  
  print,wst.lzdate,relgain,elapsec,format='(a8,2x,6f6.2,2x,i10)' 
  sec(k)=elapsec
  rg(*,k)=relgain
  wlosens=where(relgain eq 0)
  if wlosens(0) ne -1 and total(relgain) ne 0 then $
    rg(wlosens,k)=-1.0
  
  ;if k eq 200 then stop
  
endwhile

sec=sec(0:k)
rg=rg(*,0:k)
pb5t=pb5t(*,0:k)

plotdata:
npl=6
pos,npl,posn,xoff=0.15,xtop=0.84,yoff=0.1
stop
ztitle=strarr(npl) & ztitle(0)='Time History of SWE/VEIS Relative Gains'
subtitle=strarr(npl)
x=sec/86400.d0
xrange=[pb5_elapsec(pb5_range(*,0),pb5ref),pb5_elapsec(pb5_range(*,1),pb5ref)]$
  /86400.d0
pb5_tickv=lonarr(3,xticks+1)
pb5_tickv(0,*)=pb5_range(0,0)+lindgen(xticks+1)
pb5_tickv(1,*)=1l
xtickv=dblarr(xticks+1)
xtickn=strarr(xticks+1)
for i=0,xticks do begin
  xtickv(i)=pb5_elapsec(pb5_tickv(*,i),pb5ref)/86400.d0
  yr=string(pb5_tickv(0,i),format='(i4)')
  xtickn(i)=strmid(yr,2,2)+'.1'
endfor



xtitle=strarr(npl) & xtitle(npl-1)='days from '+string(date_begin,format='(i8)')
yrange=[0.,8.]
yticks=4 & yminor=2
psym=4 & symsize=0.1
noerase=1+lonarr(npl) & noerase(0)=0
charsize=1.25
xcharsize=0.001+fltarr(npl) & xcharsize(npl-1)=1.0

window,0,xsize=600,ysize=800
for i=0,npl-1 do begin
  ytitle='Det '+string(i,format='(i1)')
  plot,/nodata,$
      x,rg(i,*),$
      title=ztitle(i),subtitle=subtitle(i),$
      xrange=xrange,  xticks=xticks,  xstyle=1,$
      
      xtitle=xtitle(i),$      
      yrange=yrange,yticks=yticks,ystyle=1,ytitle=ytitle,$
      yminor=yminor,$
      psym=psym,symsize=symsize,noerase=noerase(i),/normal,$
      position=posn(*,i),xticklen=0.05,$
      charsize=charsize,xcharsize=xcharsize(i)
  ;xtickv=xtickv(i),xtickname=xtickn(i),$
  wok=where(rg(i,*) ge 0)
  if wok(0) ne -1 then $
    oplot,x(wok),rg(i,wok),psym=psym,symsize=symsize,color=clr_white 
     
  wlo=where(rg(i,*) lt 0)  
  if wlo(0) ne -1 then $        
    oplot,x(wlo),abs(rg(i,wlo)),psym=psym,symsize=symsize,color=clr_red     
endfor      
end