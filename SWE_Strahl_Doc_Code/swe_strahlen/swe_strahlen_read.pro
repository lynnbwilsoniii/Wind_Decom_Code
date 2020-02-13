pro swe_strahlen_read,tjd0_thisfile=tjd0

common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common strahlenstuff,strahlenflnm,strahlendat
common shared,d
common wstuff,wst
common swestuff,swest
common log_delog,comp_tbl,dcomp_tbl

print,'strahlen_read :'

idatype=where(d.datype eq 'swe_strahlen')
flnm=d.flnm(idatype)

thisdate=wst.indate
ifilenr=0
start:

openr,/Swap_if_Little_Endian,lunp,flnm,/get_lun
print,' ' & print,'reading strahl file ',flnm,' ......'

nrec=0l 
nstrdets=0l
nstrphis=0l
readu,lunp,nrec,nstrdets,nstrphis
print,nrec,nstrdets,nstrphis

if nstrdets eq 12 and nstrphis eq 28 then scimod=1 else $
if nstrdets eq 4 and nstrphis eq 32 then scimod=2

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

print,'end reading file ',flnm

if data(n_elements(data)-1).pb5tim(0) le 1995l and $
   data(n_elements(data)-1).pb5tim(1) lt 107l then $
   data(*).misc(0)=49.                      ;strahl voltage bias level

;from the available strahl energy steps, find steps closest to selected en steps
  energies=volt_en_strl(data.enstep,/en)
  mn=min(abs(energies-wst.strlen0),indx)
  wst.strlen0=energies(indx)
  enstep0=data(indx).enstep
  wen0=where(data.enstep eq enstep0)
 
  mn=min(abs(energies-wst.strlen1),indx)
  wst.strlen1=energies(indx)
  enstep1=data(indx).enstep
  wen1=where(data.enstep eq enstep1)
  
data=[data(wen0),data(wen1)]   
   
wok=where(data.enstep le 64)
data=data(wok)


;d.ndx(0,idatype)=0
;d.ndx(1,idatype)=n_elements(data)-1  ;begin and end time indices
;d.ndx_orig(*,idatype)=d.ndx(*,idatype)


;get start time for this file  
 ;tjd0=long(fix(data(20).ta/86400.d)) 
    

;----------------------- NOTE! --------------------------------------------
;modify the strahl data structure, saving only the max strahl
print,'find max each spin at energy step ',swest.strlstep_slct

;sdat=data
;data=0

sz=size(data.f)
ndets=sz(1)
nstrphis=sz(2)
nsdat=sz(3)

fill=-1.e31
outdat={ta:0.d,pb5:lonarr(3),elapsec:0l,enstep:0l,en:0.,$
         strl:0.,wstrl:0.,pstrl:0.,astrl:0.,wastrl:0.,pastrl:0.}

;if ifilenr eq 0 then strldat=replicate(outdat,nsdat) else $
;  strldat=[strldat,replicate(outdat,nsdat)]
;ns=n_elements(strldat)-nsdat

case scimod of

1: begin 
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
endcase

2: begin
       phistrl=[  $
       203.80625,       206.61875,       209.43125,       212.24375, $
       215.05625,       217.86875,       220.68125,       223.49375, $
       226.30625,       229.11875,       231.93125,       234.74375, $
       237.55625,       240.36875,       243.18125,       245.99375, $
       23.806250,       26.618750,       29.431250,       32.243750, $
       35.056250,       37.868750,       40.681250,       43.493750, $
       46.306250,       49.118750,       51.931250,       54.743750, $
       57.556250,       60.368750,       63.181250,       65.993750] 

       thesdet=[ 65.650000,       80.350000,       99.790000,       114.35000]
endcase
endcase
       
;time
;strldat(ns:ns+nsdat-1).ta=data.ta
ta=data.ta
pb5=data.pb5tim
;strldat(ns:ns+nsdat-1).pb5=data.pb5tim

;energy step
;strldat(ns:ns+nsdat-1).enstep=data.enstep
enstep=data.enstep

en=fltarr(nsdat) 
strl=fltarr(nsdat)
pstrl=fltarr(nsdat)
wstrl=fltarr(nsdat)
astrl=fltarr(nsdat)
pastrl=fltarr(nsdat)
wastrl=fltarr(nsdat)                                 
for i=0,nsdat-1 do begin 

;set f ge fmx = 0 (strahl flux values data(i).f are 12-to-8-compressed bytes)
  fmx=comp_tbl(wst.newstrlmax) ;plots f lt fmx 
  wfmx=where(data(i).f ge fmx)
  if wfmx(0) ne -1 then data(i).f(wfmx)=0

;energy this strahl spectra (this spin)
  ;strldat(ns+i).en=long(volt_en_strl(data(i).enstep,/en))
  en(i)=long(volt_en_strl(data(i).enstep,/en))
  
;find the detector-spinphase sample in this spin with the maximum counts
;get the maximum counts and the corresponding pitch angle, also
;get the maximum counts in the opposite strahl sector, i.e., anti-strahl
;also get beam width for strahl and anti-strahl


;maximum counts is assumed to be the strahl and phmx, thmx are directions
;from which the strahl arrives 

;---------------------- find the strahl ----------------------------- 
;find phi of maximum counts 
  max=max(data(i).f,mxind,min=min)
  ;strldat(ns+i).strl=float(dcomp_tbl(max)) ;DECOMPRESS the strahl max from 8 to 12-bit
  strl(i)=float(dcomp_tbl(max))
  ;strldat(ns+i).pb5=data(i).pb5tim 
  phasemxind=mxind/ndets
  detmxind=mxind-phasemxind*ndets
  ;if phasemxind ge 28 then stop
  ;stop
  phmx=double(phistrl(phasemxind))
  thmx=double(thesdet(detmxind))
    
;get strahl particle pitch angle
  ;strldat(ns+i).pstrl=data(i).pa(mxind)
  pstrl(i)=data(i).pa(mxind)
     
;get the strahl and anti-strahl phi indices
  if phasemxind lt nstrphis/2 then begin
    anti_strphis=nstrphis/2 + indgen(nstrphis/2)
    strphis=indgen(nstrphis/2)
  endif else begin
    anti_strphis=indgen(nstrphis/2)
    strphis=nstrphis/2 + indgen(nstrphis/2)
  endelse   
      
;estimate strahl beam size at half maximum
method=1
case method of
0: begin
  g=dcomp_tbl(data(i).f(0:ndets-1,strphis))     
  ph=phistrl(0:ndets-1,strphis)
  th=thesdet#replicate(1.,nstrphis/2)
  cl=0.5*float(dcomp_tbl(max)) 
  wcl=where(g ge cl)
  if wcl(0) ne -1 then begin
    xmn=min(ph(wcl),max=xmx)
    ymn=min(th(wcl),max=ymx)
    ;strldat(ns+i).wstrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
    wstrl(i)=(abs(xmx-xmn)+abs(ymx-ymn) )/2
  endif
endcase

1: begin
  g=dcomp_tbl(data(i).f(0:ndets-1,strphis)) 
  dim=size(g)
  dimfctr=5 
  g=rebin(g,dimfctr*dim(1),dimfctr*dim(2))   
  ph=rebin(phistrl(0:ndets-1,strphis),dimfctr*dim(1),dimfctr*dim(2))
  th=rebin(thesdet#replicate(1.,nstrphis/2),dimfctr*dim(1),dimfctr*dim(2))
  cl=0.5*float(dcomp_tbl(max)) 
  wcl=where(g ge cl)
  if wcl(0) ne -1 then begin
    xmn=min(ph(wcl),max=xmx)
    ymn=min(th(wcl),max=ymx)
    ;strldat(ns+i).wstrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
    wstrl(i)=(abs(xmx-xmn)+abs(ymx-ymn) )/2
  endif
endcase
endcase    

;------- now find the anti-strahl ---------------------------------------  
     
  ganti=data(i).f(0:ndets-1,anti_strphis)
  phanti=phistrl(0:ndets-1,anti_strphis)
  max_anti=max(ganti,mxind_anti)      
  ;strldat(ns+i).astrl=float(dcomp_tbl(max_anti))
  astrl(i)=float(dcomp_tbl(max_anti))
      
  phasemxind=mxind_anti/ndets
  detmxind=mxind_anti-phasemxind*ndets
  phmx_anti=double(phanti(phasemxind))
  thmx_anti=double(thesdet(detmxind))

;get anti-strahl particle pitch angle
   ;strldat(ns+i).pastrl=data(i).pa(max_anti) 
   
   pastrl(i)=data(i).pa(mxind_anti)
          
;estimate anti-strahl beam size at half maximum
method=0
case method of
0: begin
  g=dcomp_tbl(data(i).f(0:ndets-1,anti_strphis))     
  ph=phistrl(0:ndets-1,anti_strphis)
  cl=0.5*float(dcomp_tbl(max_anti)) 
  wcl=where(g ge cl)
  if wcl(0) ne -1 then begin
    xmn=min(ph(wcl),max=xmx)
    ymn=min(th(wcl),max=ymx)
    ;strldat(ns+i).wastrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
    wastrl(i)=(abs(xmx-xmn)+abs(ymx-ymn) )/2
  endif      
endcase

1:
endcase
                 
endfor

if ifilenr eq 0 then begin
  tam=ta
  pb5m=pb5
  enstepm=enstep
  enm=en
  strlm=strl
  wstrlm=wstrl
  pstrlm=pstrl
  astrlm=astrl
  wastrlm=wastrl
  pastrlm=pastrl
endif else begin
  tam=[tam,ta]
  pb5m=[[pb5m],[pb5]]
  enstepm=[enstepm,enstep]
  enm=[enm,en]
  strlm=[strlm,strl]
  wstrlm=[wstrlm,wstrl]
  pstrlm=[pstrlm,pstrl]
  astrlm=[astrlm,astrl]
  wastrlm=[wastrlm,wastrl]
  pastrlm=[pastrlm,pastrl]
endelse
help,ta,pb5,elapsec,enstep,en,strl,wstrl,pstrl,astrl,wastrl,pastrl
help,tam,pb5m,elapsecm,enstepm,enm,strlm,wstrlm,pstrlm,astrlm,wastrlm,pastrlm
;stop
;if ifilenr eq 0 then strldat_last=[strldat] else $
;  strldat_last=[strldat_last,strldat]
;if ifilenr gt 0  sdat2_last=[sdat2_last,sdat2]
;strldat=0

;if multiple days
donxt: 
if ifilenr lt wst.number_days-1 then begin
    thispb5=ymd_pb5(long(thisdate))
    thissec=pb5_sec(thispb5)
    pb5_next=sec_pb5(thissec+long(86400))
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
      get_flnm('strahl',getenv(d.pathenv(idatype)),nullstr,'.strahl',$
      date_next,err=err)
    ifilenr=ifilenr+1
    flnm=file_next
    thisdate=date_next
    if err eq '' then goto,start else goto,donxt     
endif

strahlendat=replicate({ta:0.d,pb5:lonarr(3),elapsec:0l,enstep:0l,en:0.,$
        strl:0.,wstrl:0.,pstrl:0.,astrl:0.,wastrl:0.,pastrl:0.},n_elements(tam))
strahlendat.ta=tam
strahlendat.pb5=pb5m
strahlendat.enstep=enstepm
strahlendat.en=enm
strahlendat.strl=strlm
strahlendat.wstrl=wstrlm
strahlendat.pstrl=pstrlm
strahlendat.astrl=astrlm
strahlendat.wastrl=wastrlm
strahlendat.pastrl=pastrlm

d.ndx(0,idatype)=0
d.ndx(1,idatype)=n_elements(strahlendat)-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)

;get start time for this file  
  tjd0=long(fix(strahlendat(20).ta/86400.d)) 
         
;stop
;strahlendat=strldat
 
d=create_struct(d,'swe_strahlendat',strahlendat)



print,'leaving strahl_read.... '

end
