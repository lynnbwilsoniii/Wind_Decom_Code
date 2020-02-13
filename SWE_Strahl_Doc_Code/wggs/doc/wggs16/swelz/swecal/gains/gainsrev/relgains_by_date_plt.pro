
;---------------- MAIN: relgains_by_date -------------------------------------

;plots relative gains as a function of date
common wstuff,wst
          
readrg=0;1
if readrg then begin

  define_widgets
  panelist
  structuresw

  date_begin=19950101l
  date_end=20011231l

  pb5ref=ymd_pb5(19941130l)
  elapsec_begin=pb5_elapsec(ymd_pb5(date_begin),pb5ref)
  elapsec_end=pb5_elapsec(ymd_pb5(date_end),pb5ref)

  ndet=6
  rg=fltarr(ndet,3000)
  esec=dblarr(3000)
  pb5=lonarr(3,3000)
  k=-1
  elapsec=elapsec_begin
  while elapsec le elapsec_end do begin
    k=k+1
    elapsec=elapsec_begin+k*double(86400l)
    esec(k)=elapsec
    pb5(*,k)=elapsec_pb5(elapsec,pb5ref)  
    date=string(pb5_ymd(pb5(*,k)),format='(i8)')
    wst.lzdate=date
    rgflnm=getrelgains_tbl_flnm() 
    ;print,'gains table file :',rgflnm
    ;print,date,rgflnm
    if rgflnm ne '' then begin
      restore,getenv('WGGSBASE')+rgflnm              
      ;help,datetbl,rgtbl  
      wdate=where(long(wst.lzdate) eq datetbl,nwdate)
      if nwdate ne 1 then stop,'bad gains table'
      rg(*,k)=rgtbl(*,wdate(0))
    endif else rg(*,k)=-1.+fltarr(6)                
  
  endwhile
  rg=rg(*,0:k)

  ;channeltron bias voltage changes
  ymd_bias=long([19950510,19951121,19960513,19960813,19980514,19980728,$
                 19980811,19981124,19990326,19990405,20001023,20011029])
  esec_bias=dblarr(n_elements(ymd_bias))               
  for j=0,n_elements(ymd_bias)-1 do begin
    esec_bias(j)=pb5_elapsec(ymd_pb5(ymd_bias(j)),pb5ref)
  endfor
     
endif

hardcopy=0
start:
if hardcopy eq 0 then window,0,xsize=800,ysize=800

posn=fltarr(4,ndet)
pos,ndet,posn,xoff=0.15,xtop=0.84,yoff=0.085,ytop=0.98
xrange=[esec(0),esec(k)]
w=where(pb5(1,*) eq 1,nw)
xtickv=esec(w)
xtickname=strarr(nw)
for j=0,nw-1 do xtickname(j)=strmid(string(pb5(0,w(j)),format='(i4)'),2,2)
noerase=1+intarr(ndet) & noerase(0)=0
xcharsize=0.0001+fltarr(ndet) & xcharsize(ndet-1)=1.0
title=strarr(ndet) & title(0)='SWE/VEIS Relative Gains (29 eV energy level)'
xtitle=strarr(ndet) & xtitle(ndet-1)='Year'
ymx=0.6*[10.,10.,10.,10.,10.,10.]
yminor=6
if hardcopy then clr_red=255. else clr_red=143.*float(!d.n_colors)/147.
for i=0,ndet-1 do begin
  yrange=[0.,ymx(i)]
  yticks=1
  plot,xrange,yrange,xstyle=4,ystyle=1,yticks=yticks,yminor=yminor,$
    yrange=yrange,charsize=1.35,noerase=noerase(i),/nodata,$
    pos=posn(*,i),ytitle='det '+string(i,format='(i1)'),title=title(i)
  axis,xaxis=0,xticks=nw-1,xtickv=xtickv,xrange=xrange,charsize=1.35,$
    xcharsize=xcharsize(i),xtickname=xtickname,xtitle=xtitle(i),/save
  axis,xaxis=1,xticks=nw-1,xtickv=xtickv,xrange=xrange,$
    xtickname=replicate(' ',nw),/save 
    
  wne0=where(rg(i,*) gt 0)
  if wne0(0) ne -1 then oplot,esec(wne0),rg(i,wne0),psym=3 
  we0=where(rg(i,*) eq 0)
  if we0(0) ne -1 then $
    oplot,esec(we0),replicate(1.,n_elements(we0)),psym=3,color=clr_red
  
  for j=0,n_elements(esec_bias)-1 do $
    oplot,[esec_bias(j),esec_bias(j)],yrange,linestyle=1  
endfor

if hardcopy then begin
  device,/close
  set_plot,'x'
  hardcopy=0
  print,'hardcopy file: ',pltfil
  stop 
endif

set_plot,'ps',/interpolate
pltfil=getenv('IDLSAV')+'relgains_by_date.ps' 
device,/inches,xoffset=0.75,yoffset=1.,xsize=7.,ysize=9.5,filename=pltfil
     
hardcopy=1
goto,start

end