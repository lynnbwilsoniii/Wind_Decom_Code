;============== pltrg ========================================================
pro pltrg,sec,rg,insec,inrg

window,1,xsize=700,ysize=800

ndets=6
posn=fltarr(4,ndets)
pos,ndets,posn


yticks=4
yminor=2
xticks=6
xminor=5
xticklen=0.09 
xrange=[sec(0)-86400.d0,sec(n_elements(sec)-1)+86400.d0]
xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
pb5tickv=lonarr(3)
xtickname=strarr(n_elements(xtickv))
for i=0,n_elements(xtickv)-1 do begin
  pb5tickv(*)=sec_pb5(xtickv(i))
  xtickname(i)=strmid(string(pb5_ymd(pb5tickv(*)),format='(i8)'),2,6)
endfor
noerase=1+intarr(ndets) & noerase(0)=0
title=strarr(ndets) & title(0)='relative gains'
xtitle=strarr(ndets) & xtitle(ndets-1)='date (yymmdd)'
xcharsize=0.001+fltarr(6) & xcharsize(5)=1.0
charsize=1.25
symsize=0.4


for i=0,ndets-1 do begin     ;detector loop for nrmlzd relgains plot 
  
  case i of
  0: yrange=[0,2]
  1: yrange=[0,2]
  2: yrange=[0,6]
  3: yrange=[0,6]
  4: yrange=[0,2]
  5: yrange=[0,6]
  endcase

  plot,xrange,yrange,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    title=title(i),ytitle='det '+string(i,format='(i1)'),xcharsize=xcharsize(i),$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=xtitle(i),position=posn(*,i),noerase=noerase(i),charsize=charsize,$
    xminor=xminor,xticklen=xticklen,/nodata
  
  oplot,sec(*),rg(i,*),color=100
  oplot,insec(*),inrg(i,*),psym=4,symsize=symsize  
endfor


end

;======== parobolic fitting function =========================================

pro funct3,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end


;================== MAIN =====================================================
pro add_to_tbl,begindate,enddate,pb5ref,datetbl,rgfit

filenm=getenv('WGGSBASE')+'swelz/swecal/gains/gainsrev/add_to_tbl.ascii'
result=read_ascii(filenm)
help,result
help,result,/str
sz=size(result.field1)
ndets=6
if sz(1) ne ndets+1 then stop
nindates=sz(2)
 
indate=lonarr(nindates)

indate(*)=result.field1(0,*)

inrg=result.field1(1:ndets,*)
print,' '
print,'input dates and rel gains'
for i=0,nindates-1 do print,indate(i),inrg(*,i),format='(i8,6f7.3)'

;find input dates within desired date interval
winput=where(indate ge long(begindate) and indate le long(enddate))

print,' '
print,'desired dates over which to fit rel gains'
for i=0,n_elements(winput)-1 do print,indate(i),inrg(*,i),format='(i8,6f7.3)'

;do fit and get coeff's
if winput(0) ne -1 then begin
  xdat=dblarr(n_elements(winput))
  ydat=dblarr(ndets,n_elements(winput))
  coeff=fltarr(3,ndets)
  for i=0,n_elements(winput)-1 do begin
    xdat(i)=pb5_sec(ymd_pb5(long(indate(winput(i)))))
    ydat(*,i)=inrg(*,i)
  endfor  
  for idet=0,ndets-1 do begin
    wm1=where(ydat(idet,*) eq -1.)
    if wm1(0) ne -1 then goto,endfit
    wt= 1.+fltarr(n_elements(winput))
    c=fltarr(3)
    c(0)=total(reform(ydat(idet,*)))/nindates
    ycrvfit=curvefit(xdat,reform(ydat(idet,*)),wt,c,sigc,$
      function_name='funct3',chi2=chi2)
      
    coeff(*,idet)=c(*) 
    endfit: 
  endfor
endif


;desired date interval over which to compute fitted values of rel gains
pb5begin=ymd_pb5(long(begindate))
elapsecbegin=pb5_elapsec(pb5begin,pb5ref)

pb5end=ymd_pb5(long(enddate))
elapsecend=pb5_elapsec(pb5end,pb5ref)

ndays=long((elapsecend-elapsecbegin)/double(86400.) + 1)
datetbl=lonarr(ndays)
xfit=dblarr(ndays)
elapsecfit=lonarr(ndays)
for iday=0,ndays-1 do begin
  elapsecfit(iday)=elapsecbegin+iday*86400l
  pb5tm=elapsec_pb5(elapsecfit(iday),pb5ref) 
  sectm=pb5_sec(pb5tm)
  xfit(iday)=sectm
  datetbl(iday)=pb5_ymd(pb5tm)
  ;print,datetbl(iday)
endfor

rgfit=fltarr(ndets,ndays)
for idet=0,ndets-1 do $
  rgfit(idet,*)=coeff(0,idet)+coeff(1,idet)*xfit(*)+coeff(2,idet)*xfit(*)^2




print,' '
print,'fitted rel gains'
for i=0,ndays-1 do print,datetbl(i),rgfit(*,i),format='(i8,6f7.3)'

pltrg,xfit,rgfit,xdat,ydat


end