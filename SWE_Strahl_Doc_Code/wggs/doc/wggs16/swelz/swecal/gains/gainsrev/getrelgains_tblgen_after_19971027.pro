;============== pltrg ========================================================
pro pltrg,sec,rg,insec,inrg

window,1,xsize=700,ysize=800

ndets=6
posn=fltarr(4,ndets)
pos,ndets,posn


yticks=5
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
symsize=0.6

rmx=fix(max(rg)) + 1
yrange=[0,rmx]
for i=0,ndets-1 do begin     ;detector loop for nrmlzd relgains plot 
  
  ;case i of
  ;0: yrange=[0,10]
  ;1: yrange=[0,10]
  ;2: yrange=[0,10]
  ;3: yrange=[0,10]
  ;4: yrange=[0,10]
  ;5: yrange=[0,10]
  ;endcase

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

;======== linear fitting function =========================================

pro funct2,x,c,f,pder
f=c(0)+c(1)*x
;if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
return
end

;-------------------- add_to_tbl ---------------------------------------------
pro add_to_tbl,begindate,enddate,pb5ref,datetbl,rgfit

filenm=getenv('WGGSBASE')+'swelz/swecal/gains/gainsrev/add_to_tbl.ascii'
data={indate:0l,inrg:fltarr(6),ebias1:0l,ebias2:0l,enstep:0l}
datarr=replicate(data,1000)
openr,lun,filenm,/get_lun
i=-1
while not eof(lun) do begin
  i=i+1
  readf,lun,data
  datarr(i)=data
endwhile
free_lun,lun
datarr=datarr(0:i)

ndets=6
sz=size(datarr.inrg)
if sz(1) ne ndets then stop
nindates=sz(2)
 
indate=datarr.indate
inrg=datarr.inrg
print,' '
print,'input dates and rel gains'
for i=0,nindates-1 do print,indate(i),inrg(*,i),format='(i8,6f7.3)'

;find input dates within desired date interval

winput=where(indate ge long(begindate) and indate le long(enddate))
print,' '
print,'desired dates over which to fit rel gains',long(begindate),long(enddate)
for i=0,n_elements(winput)-1 do print,indate(winput(i)),inrg(*,winput(i)),$
  format='(i8,6f7.3)'
  
if begindate eq enddate  then begin ;avg for each det
  datetbl=long(begindate)+lonarr(1)
  rgfit=fltarr(ndets,1)
  for idet=0,ndets-1 do begin
    wm1=where(inrg(idet,winput) eq -1.)
    if wm1(0) eq -1 then $
      rgfit(idet,0)=total(inrg(idet,winput))/n_elements(winput)
  endfor  
  print,'avg' 
  print,datetbl(0),rgfit(*,0),format='(i8,6f7.3)'
  stop 
  return
endif



;do fit and get coeff's
ncoeff=-1
if n_elements(winput) gt 3 then ncoeff=3  ;do parabolic fit
if n_elements(winput) eq 3 then ncoeff=2  ;do linear fit
if n_elements(winput) lt 3 then ncoeff=1 ;average each det

  xdat=dblarr(n_elements(winput))
  ydat=dblarr(ndets,n_elements(winput))
  coeff=fltarr(ncoeff,ndets)
  for i=0,n_elements(winput)-1 do begin
    xdat(i)=pb5_sec(ymd_pb5(long(indate(winput(i)))))
    ydat(*,i)=inrg(*,winput(i))
  endfor
    
  for idet=0,ndets-1 do begin
    wm1=where(ydat(idet,*) eq -1.)
    if wm1(0) ne -1 then goto,endfit
    wt= 1.+fltarr(n_elements(winput))
    c=fltarr(ncoeff)
    c(0)=total(reform(ydat(idet,*)))/n_elements(winput)
    if ncoeff eq 3 then ycrvfit=curvefit(xdat,reform(ydat(idet,*)),wt,c,sigc,$
      function_name='funct3',chi2=chi2) else $
    if ncoeff eq 2 then ycrvfit=curvefit(xdat,reform(ydat(idet,*)),wt,c,sigc,$
      function_name='funct2',chi2=chi2)   
    coeff(*,idet)=c(*) 
    endfit: 
  endfor



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

if ncoeff eq 3 then for idet=0,ndets-1 do $
  rgfit(idet,*)=coeff(0,idet)+coeff(1,idet)*xfit(*)+coeff(2,idet)*xfit(*)^2 $
else if ncoeff eq 2 then for idet=0,ndets-1 do $
  rgfit(idet,*)=coeff(0,idet)+coeff(1,idet)*xfit(*) $  
else if ncoeff eq 1 then for idet=0,ndets-1 do $
  rgfit(idet,*)=coeff(0,idet)
  
print,' '
print,'fitted rel gains'
for i=0,ndays-1 do print,datetbl(i),rgfit(*,i),format='(i8,6f7.3)'

pltrg,xfit,rgfit,xdat,ydat


end

;================== MAIN =====================================================


;pro getrelgains_tblgen_after_19971027

;This procedure uses 
;  add_to_tbl.pro to add revised or test gains in specified time intervals
;  to be inserted into existing gains table.
;Both an ascii file 
;  (getenv('WGGSBASE')+'swelz/swecal/gains/getrelgains_tbl.ascii')
;and an idlsave file 
; (getenv('WGGSBASE')+'swelz/swecal/gains/getrelgains_tbl.dat'
;of the gains table is made

pb5ref=ymd_pb5(19971028l)

;desired begin and end dates
   date_range=[$
               ['19971028', '19980514'],$ 
               ['19980515', '19980610'],$ 
               ['19980611', '19980728'],$
               ['19980729', '19980811'],$
               ['19980812', '19980829'],$
               ['19980830', '19981025'],$
               ['19981026', '19981124'],$
               ['19981125', '19981216'],$
               ['19990108', '19990201'],$
               ['19990202', '19990208'],$
               ['19990209', '19990211'],$
               ['19990212', '19990325'],$
               ['19990326', '19990404'],$
               ['19990405', '19990505'],$
               ['19990506', '19990604'],$
               ['19990605', '19990704'],$
               ['19990705', '19990725'],$
               ['19990726', '19990902'],$
               ['19990903', '19991004'],$
               ['19991005', '19991102'],$
               ['19991103', '19991117'],$
               ['19991203', '20000101'],$
               ['20000128', '20000226'],$
               ['20000227', '20000328'],$
               ['20000329', '20000427'],$
               ['20000428', '20000525'],$
               ['20000526', '20000611'],$
               ['20000612', '20000625'],$
               ['20000626', '20000709'],$
               ['20000710', '20000724'],$
               ['20000725', '20000806'],$
               ['20000807', '20000822'],$
               ['20000823', '20000919'],$
               ['20000920', '20001022'],$
               ['20001023', '20001110'],$
               ['20001111', '20001125'],$
               ['20001126', '20001214'],$
               ['20001215', '20010115'],$
               ['20010116', '20010213'],$
               ['20010214', '20010314'],$
               ['20010315', '20010412'],$
               ['20010413', '20010513'],$
               ['20010514', '20010610']]
               
   ;['20000715', '20000715'],$
   sz=size(date_range)
   numdates=sz(2)
   date_start=reform(date_range(0,*))
   date_stop=reform(date_range(1,*))
               
   ;date_start=['19971028', '19980515', '19980611', '19980729', '19980812',$
   ;            '19980830', '19981026', '19981125', '19981217', '19990212',$
   ;            '20000715']
               
   ;date_stop= ['19980514', '19980610', '19980728', '19980811', '19980829',$
   ;            '19981025', '19981124', '19981216', '19990211', '19990325',$
   ;            '20000715']
   print,'begin and end dates'
   for i=0,numdates-1 $
     do print,i,date_start(i),date_stop(i),format='(i3,3x,2a10)'
   print,'select date interval index:'
   idate=-1
   read,idate
   print,'date interval selected:',date_start(idate),'  ',date_stop(idate)
   stop,'.c to continue'
   
   begindate=date_start(idate)
   enddate=date_stop(idate)
  
add_to_tbl,begindate,enddate,pb5ref,datetbl,rgtbl

ascii_file=getenv('WGGSBASE')+$
  'swelz/swecal/gains/gainsrev/getrelgains_tbl_'+begindate+'_'+enddate+'.ascii'
result=findfile(ascii_file)

if result(0) ne '' then begin
  print,'ascii file exists: ',result
  print,'Do you want to overwrte it?'
  stop,'If yes, then .c to continue.'
endif  
openw,1,ascii_file
for i=0,n_elements(datetbl)-1 do $
  printf,1,datetbl(i),rgtbl(*,i),format='(i8,5x,6f7.3)'
close,1
print,'ascii file created ',ascii_file
print,' '
sav_file=getenv('WGGSBASE')+$
  'swelz/swecal/gains/gainsrev/getrelgains_tbl_'+begindate+'_'+enddate+'.dat'
  result=findfile(ascii_file)
if result(0) ne '' then begin
  print,'idlsav file exists: ',result
  print,'Do you want to overwrte it?'
  stop,'If yes, then .c to continue.'
endif  
save,file=sav_file,datetbl,rgtbl
print,'idlsav file created ',sav_file  
print,' '  
end