pro parabfunct,x,a,f,pder
  f=a(0)+a(1)*x*x
    if n_params() ge 4 then $
    pder=[[replicate(1.,n_elements(x))],[x*x]]
end

pro parabfit,p,g,pfitrange,wdthhalfmax,i

;parabolic fit of g vs p in order to find full width at half maximum of g

  wdthhalfmax=-1e31
  
  wpfitrange=where(p le pfitrange and g gt 0,npts)  
  if npts lt 7 then goto,endfit
  xinp=p(wpfitrange)
  yinp=g(wpfitrange)
  sortx=sort(xinp)
  xinp=xinp(sortx)
  yinp=yinp(sortx)
  logyinp=alog10(yinp)
  
  ;do parabolic in log fit
  a=[max(logyinp),0.] 
  wt=yinp                 
  yfit=$
     curvefit(xinp,logyinp,wt,a,siga,function_name='parabfunct',$
     iter=iter0,chi2=chisq)
  ;xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
  ;parabfunct,xcurve,a,logycurve
  ;ycurve=10.^(logycurve)
  ;ymax=ycurve(0)
  
  ;find width at half-maximum                
  nc=fix(pfitrange)+1
  xc=findgen(nc)
  parabfunct,xc,a,logyc
  yc=10.^(logyc)
  mn=min(abs(yc-0.5*yc(0)),mindx)
  yctsmax=yc(0)
  wdthhalfmax=xc(mindx)
   
  
  endfit: 
  ;if i eq 5988 then stop      
end 

 
pro swe_strahl_read,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst
common swestuff,swest
common log_delog,comp_tbl,dcomp_tbl

fiodial_append_req,' '
fiodial_append_req,'swe_strahl_read: ['+$
                   string(wst.number_days,Format='(I2.2)')+' date(s)...]'

idatype=where(d.datype eq 'swe_strahl')
flnm=d.flnm(idatype)

openr,/Swap_if_Little_Endian,lunp,flnm,/get_lun
;print,' ' &
fiodial_append_req,'reading SWE electron strahl file '+flnm+'...'

nrec=0l 
nstrdets=0l
nstrphis=0l
readu,lunp,nrec,nstrdets,nstrphis
;print,nrec,nstrdets,nstrphis
indat={  $
        lzrec:0l,  $
        spinbl:0l,  $
        ta:0.d,  $
        pb5tim:lonarr(3),  $
        enstep:0l,$
        f:bytarr(nstrdets,nstrphis),   $
        pa:bytarr(nstrdets,nstrphis), $
        b:fltarr(3),$
        misc:fltarr(5)  }
data=replicate(indat,nrec)
readu,lunp,data
free_lun,lunp
;print,'end reading file ',flnm

if data(n_elements(data)-1).pb5tim(0) le 1995l and $
   data(n_elements(data)-1).pb5tim(1) lt 107l then $
   data(*).misc(0)=49.                      ;strahl voltage bias level
   
;if multiple days 
if wst.number_days gt 1 then begin
  thisflnm=flnm
  thisdate=wst.indate
  for inxt=1,wst.number_days-1 do begin
    thispb5=ymd_pb5(long(thisdate))
    thissec=pb5_sec(thispb5)
    pb5_next=sec_pb5(thissec+long(86400))
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
    get_flnm('strahl',getenv(d.pathenv(idatype)),nullstr,'.strahl',$
      date_next,err=err)
    if err ne '' then goto,getoutloop  
    openr,/Swap_if_Little_Endian,lun_next,file_next,/get_lun
    fiodial_append_req,'reading SWE electron strahl file '+file_next+'...'

    nrec_next=0l 
    nstrdets_next=0l
    nstrphis_next=0l
    readu,lunp,nrec_next,nstrdets_next,nstrphis_next
    ;print,nrec_next,nstrdets_next,nstrphis_next
    data_next=replicate(indat,nrec_next)
    readu,lun_next,data_next
    free_lun,lun_next
        
    if data_next(n_elements(data_next)-1).pb5tim(0) le 1995l and $
    data_next(n_elements(data_next)-1).pb5tim(1) lt 107l then $
    data_next(*).misc(0)=49.                      ;strahl voltage bias level
    
    ;print,'end reading next file ',file_next
    data=[temporary(data),data_next]
    data_next=0
    thisflnm=file_next
    thisdate=date_next
  endfor
endif
getoutloop:


wok=where(data.enstep le 64)
data=data(wok)


d.ndx(0,idatype)=0
d.ndx(1,idatype)=n_elements(data)-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)

;strahldat=data

;get start time for this file
  
 tjd0=long(fix(data(20).ta/86400.d)) 
    

;----------------------- NOTE! --------------------------------------------
;modify the strahl data structure, saving only the max strahl
fiodial_append_req,'find max each spin...'

sdat=data
data=0

sz=size(sdat.f)
ndets=sz(1)
nstrphis=sz(2)
nsdat=sz(3)

fill=-1.e31
data=replicate({ta:0.d,pb5:lonarr(3),elapsec:0l,enstep:0l,en:0.,scimod:0,$
  strl:0.,pstrl:fill,wstrl:0.,strlmaxdet:0.,$
  astrl:0.,pastrl:fill,wastrl:0.},nsdat)

phistrl=[  $
       204.15781,       207.67344,       211.18906,       214.70469,  $
       218.22031,       221.73594,       225.25156,       228.76719,  $
       232.28281,       235.79844,       239.31406,       242.82969,  $
       246.34531,       249.86094,       23.454687,       26.970312,  $
       30.485937,       34.001562,       37.517187,       41.032812,  $
       44.548437,       48.064062,       51.579687,       55.095312,  $
       58.610937,       62.126562,       65.642187,       69.157812   ]

thesdet=[61.30, 65.65, 70.46,   75.35,  80.35,  84.39, $
           94.89, 99.79, 104.67, 109.52, 114.35, 118.75]

cosph=cos(phistrl*!dtor)
sinph=sin(phistrl*!dtor)
costh=cos(thesdet*!dtor)
sinth=sin(thesdet*!dtor)

;get unitvector for each measurement
vunit=fltarr(n_elements(thesdet),n_elements(phistrl),3)
vunit(*,*,0)=sinth # cosph
vunit(*,*,1)=sinth # sinph
vunit(*,*,2)=costh # replicate(1.,n_elements(phistrl))


;time
data.ta=sdat.ta
data.pb5=sdat.pb5tim

;get mode
  data.scimod=sdat.misc(1)
  
;energy step
data.enstep=sdat.enstep
                                  
for i=0l,nsdat-1 do begin 
  
;set f ge fmx = 0 (strahl flux values sdat(i).f are 12-to-8-compressed bytes)
  fmx=comp_tbl(wst.newstrlmax) ;plots f lt fmx 
  wfmx=where(sdat(i).f ge fmx)
  if wfmx(0) ne -1 then sdat(i).f(wfmx)=0

;energy this strahl spectra (this spin)
  data(i).en=long(volt_en_strl(sdat(i).enstep,/en))
  
;find the detector-spinphase sample in this spin with the maximum counts
;get the maximum counts and the corresponding pitch angle, also
;get the maximum counts in the opposite strahl sector, i.e., anti-strahl
;also get beam width for strahl and anti-strahl


;maximum counts is assumed to be the strahl and phmx, thmx are directions
;from which the strahl arrives 

;---------------------- find the strahl ----------------------------- 
;find phi of maximum counts 
  max=max(sdat(i).f,mxind,min=min)
  data(i).strl=float(dcomp_tbl(max)) ;DECOMPRESS the strahl max from 8 to 12-bit
  data(i).pb5=sdat(i).pb5tim      
  phasemxind=mxind/ndets
  detmxind=mxind-phasemxind*ndets
  data(i).strlmaxdet=detmxind
  phmx=double(phistrl(phasemxind))
  thmx=double(thesdet(detmxind))
    
;get strahl particle pitch angle
  data(i).pstrl=sdat(i).pa(mxind)
   
;get the strahl and anti-strahl phi indices
  if phasemxind lt nstrphis/2 then begin
    anti_strphis=nstrphis/2 + indgen(nstrphis/2)
    strphis=indgen(nstrphis/2)
  endif else begin
    anti_strphis=indgen(nstrphis/2)
    strphis=nstrphis/2 + indgen(nstrphis/2)
  endelse   
      
;estimate strahl beam size at half maximum
method=0;1
case method of
0: begin
  g=dcomp_tbl(sdat(i).f(0:ndets-1,strphis))     
  ph=phistrl(0:ndets-1,strphis)
  th=thesdet#replicate(1.,nstrphis/2)
  cl=0.5*float(dcomp_tbl(max)) 
  wcl=where(g ge cl)
  if wcl(0) ne -1 then begin
    xmn=min(ph(wcl),max=xmx)
    ymn=min(th(wcl),max=ymx)
    data(i).wstrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
  endif
endcase

1: begin
  ;unit vector of max strahl
  vunitmx=dblarr(3)
  vunitmx(0)=sin(thmx*!dtor)*cos(phmx*!dtor)
  vunitmx(1)=sin(thmx*!dtor)*sin(phmx*!dtor)
  vunitmx(2)=cos(thmx*!dtor)
  
  ;for the detector, detmxind, measuring maximum strahl, find angle, 
  ;  p(nstrphis/2),  between maximum and all azimuth at detmxind
  p=fltarr(nstrphis/2)
  g=fltarr(nstrphis/2)
  for j=0,nstrphis/2-1 do begin
    p(j)=acos(total(vunit(detmxind,strphis(j),*)*vunitmx(*)))/!dtor
    g(j)=dcomp_tbl(sdat(i).f(detmxind,strphis(j)))
  endfor
  pfitrange=30.
  
  parabfit,p,g,pfitrange,phwdthhalfmax,i
  
   ;for the aximuthal angle,phasemxind,detector, measuring maximum strahl, 
   ;find angle, p(ndets),  between maximum and all elevations at phasemxind
  p=fltarr(ndets)
  g=fltarr(ndets)
  for j=0,ndets-1 do begin
    p(j)=acos(total(vunit(j,phasemxind,*)*vunitmx(*)))/!dtor
    g(j)=dcomp_tbl(sdat(i).f(j,phasemxind))
  endfor
  pfitrange=30.
  
  parabfit,p,g,pfitrange,thwdthhalfmax,i
  data(i).wstrl=(phwdthhalfmax+thwdthhalfmax)/2
  
endcase
endcase    

;------- now find the anti-strahl ---------------------------------------  
     
  ganti=sdat(i).f(0:ndets-1,anti_strphis)
  phanti=phistrl(0:ndets-1,anti_strphis)
  max_anti=max(ganti,mxind_anti)      
  data(i).astrl=float(dcomp_tbl(max_anti))
      
  phasemxind=mxind_anti/ndets
  detmxind=mxind_anti-phasemxind*ndets
  phmx_anti=double(phanti(phasemxind))
  thmx_anti=double(thesdet(detmxind))
      
;get anti-strahl particle pitch angle
    data(i).pastrl=sdat(i).pa(max_anti) 
      
;estimate anti-strahl beam size at half maximum
method=0
case method of
0: begin
  g=dcomp_tbl(sdat(i).f(0:ndets-1,anti_strphis))     
  ph=phistrl(0:ndets-1,anti_strphis)
  th=thesdet#replicate(1.,nstrphis/2)
  cl=0.5*float(dcomp_tbl(max_anti)) 
  wcl=where(g ge cl)
  if wcl(0) ne -1 then begin
    xmn=min(ph(wcl),max=xmx)
    ymn=min(th(wcl),max=ymx)
    data(i).wastrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
  endif      
endcase
1:
endcase
                 
endfor

strahldat=data
data=0

;print,'leaving strahl_read.... '

strahldat=data
data=0

return, {lzrec:sdat.lzrec, spinbl:sdat.spinbl, ta:sdat.ta, pb5tim:sdat.pb5tim, $
         enstep:sdat.enstep, f:sdat.f, phistrl_gse:sdat.phistrl_gse, b:sdat.b, misc:sdat.misc, $
         elapsec:strahldat.elapsec, en:strahldat.en, scimod:strahldat.scimod, $
         strl: strahldat.strl, pstrl:strahldat.pstrl, wstrl:strahldat.wstrl, $
         strlmaxdet:strahldat.strlmaxdet, astrl:strahldat.astrl, pastrl:strahldat.pastrl, wastrl:strahldat.wastrl}

end
