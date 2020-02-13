pro swe_strahlen_read_v7,tjd0_thisfile=tjd0

common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common strahlenstuff,strahlenflnm,strahlendat
common shared,d
common wstuff,wst
common swestuff,swest
common log_delog,comp_tbl,dcomp_tbl

print,'strahlen_v7_read :'

idatype=where(d.datype eq 'swe_strahlen')
flnm=d.flnm(idatype)

openr,lunp,flnm,/get_lun
print,' ' & print,'reading strahl file ',flnm,' ......'

nrec=0l 
nstrdets=0l
nstrphis=0l
readu,lunp,nrec,nstrdets,nstrphis
print,nrec,nstrdets,nstrphis
indat={  $
        lzrec:0l,  $
        spinbl:0l,  $
        ta:0.d,  $
        pb5tim:lonarr(3),  $
        enstep:0l,$
        f:bytarr(nstrdets,nstrphis),   $
        phistrl_gse:fltarr(nstrphis), $
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
   
;if multiple days 
if wst.number_days gt 1 then begin
  thisflnm=d.flnm(6)
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
    if err ne '' then goto,endloop  
    openr,lun_next,file_next,/get_lun
    nrec_next=0l 
    nstrdets_next=0l
    nstrphis_next=0l
    readu,lunp,nrec_next,nstrdets_next,nstrphis_next
    print,nrec_next,nstrdets_next,nstrphis_next
    data_next=replicate(indat,nrec_next)
    readu,lun_next,data_next
    free_lun,lun_next
        
    if data_next(n_elements(data_next)-1).pb5tim(0) le 1995l and $
    data_next(n_elements(data_next)-1).pb5tim(1) lt 107l then $
    data_next(*).misc(0)=49.                      ;strahl voltage bias level
    
    wen0=where(data_next.enstep eq enstep0)
    wen1=where(data_next.enstep eq enstep1)
    data_next=[data_next(wen0),data_next(wen1)]   
    
    print,'end reading next file ',file_next
    data=[temporary(data),data_next]
    data_next=0
    
    endloop:
    thisflnm=file_next
    thisdate=date_next
  endfor
endif


wok=where(data.enstep le 64)
data=data(wok)


d.ndx(0,idatype)=0
d.ndx(1,idatype)=n_elements(data)-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)


;get start time for this file
  
 tjd0=long(fix(data(20).ta/86400.d)) 
    

;----------------------- NOTE! --------------------------------------------
;modify the strahl data structure, saving only the max strahl; filter out sun
print,'find max each spin at energy step ',swest.strlstep_slct, $
   '  and filter out the sun.......'

sdat=data
data=0

sz=size(sdat.f)
ndets=sz(1)
nstrphis=sz(2)
nsdat=sz(3)

fill=-1.e31
data=replicate({ta:0.d,pb5:lonarr(3),elapsec:0l,enstep:0l,en:0.,$
  strl:0.,wstrl:0.,pstrl:0.,$
  astrl:0.,wastrl:0.,pastrl:0.},nsdat)

thesdet=[61.30, 65.65, 70.46,   75.35,  80.35,  84.39, $
           94.89, 99.79, 104.67, 109.52, 114.35, 118.75]

;time
data.ta=sdat.ta
data.pb5=sdat.pb5tim

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

     
;apply sunmask     
;sunmask location in phi_gse
  phsunindx=where(sdat(i).phistrl_gse gt swest.strlphsun(0) and $
                  sdat(i).phistrl_gse lt swest.strlphsun(1),nphsunindx)
  if nphsunindx gt 1 then $
    sdat(i).f(*,phsunindx(0):phsunindx(nphsunindx-1))=0                   


;maximum counts is assumed to be the strahl and phmx, thmx are directions
;from which the strahl arrives 

;---------------------- find the strahl ----------------------------- 
;find phi of maximum counts 
  max=max(sdat(i).f,mxind,min=min)
  data(i).strl=float(dcomp_tbl(max)) ;DECOMPRESS the strahl max from 8 to 12-bit
  data(i).pb5=sdat(i).pb5tim      
  phasemxind=mxind/ndets
  detmxind=mxind-phasemxind*ndets
  phmx=double(sdat(i).phistrl_gse(phasemxind))
  thmx=double(thesdet(detmxind))
    
;get unitvector of strahl particle velocity
  vunit_gse=dblarr(3)
  vunit_gse(0)=sin(thmx*!dtor)*cos(phmx*!dtor)
  vunit_gse(1)=sin(thmx*!dtor)*sin(phmx*!dtor)
  vunit_gse(2)=cos(thmx*!dtor)
            
;get strahl particle pitch angle
  if sdat(i).b(0) ne fill then begin
    bhat=sdat(i).b/sqrt(total(sdat(i).b^2))    
    vsparahat=$
        vunit_gse(0)*bhat(0)+vunit_gse(1)*bhat(1)+vunit_gse(2)*bhat(2)
    data(i).pstrl=acos(vsparahat)/!dtor
  endif else data(i).pstrl=fill  
   
      
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
  g=dcomp_tbl(sdat(i).f(0:ndets-1,strphis))     
  ph=sdat(i).phistrl_gse(0:ndets-1,strphis)
  th=thesdet#replicate(1.,nstrphis/2)
  cl=0.5*float(dcomp_tbl(max)) 
  wcl=where(g ge cl)
  if wcl(0) ne -1 then begin
    xmn=min(ph(wcl),max=xmx)
    ymn=min(th(wcl),max=ymx)
    data(i).wstrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
  endif
endcase

1:begin
  g=dcomp_tbl(sdat(i).f(0:ndets-1,strphis))
  dim=size(g)
  dimfctr=5
  g=rebin(g,dimfctr*dim(1),dimfctr*dim(2))  
  ph=rebin(sdat(i).phistrl_gse(0:ndets-1,strphis),dimfctr*dim(1),dimfctr*dim(2))
  th=rebin(thesdet#replicate(1.,nstrphis/2),dimfctr*dim(1),dimfctr*dim(2))
  cl=0.5*float(dcomp_tbl(max)) 
  wcl=where(g ge cl)
  if wcl(0) ne -1 then begin
    xmn=min(ph(wcl),max=xmx)
    ymn=min(th(wcl),max=ymx)
    data(i).wstrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
  endif
endcase
endcase    

;------- now find the anti-strahl ---------------------------------------  
     
  ganti=sdat(i).f(0:ndets-1,anti_strphis)
  phanti=sdat(i).phistrl_gse(0:ndets-1,anti_strphis)
  max_anti=max(ganti,mxind_anti)      
  data(i).astrl=float(dcomp_tbl(max_anti))
      
  phasemxind=mxind_anti/ndets
  detmxind=mxind_anti-phasemxind*ndets
  phmx_anti=double(phanti(phasemxind))
  thmx_anti=double(thesdet(detmxind))

;get unitvector of anti-strahl particle velocity
  vunit_gse=dblarr(3)
  vunit_gse(0)=sin(thmx_anti*!dtor)*cos(phmx_anti*!dtor)
  vunit_gse(1)=sin(thmx_anti*!dtor)*sin(phmx_anti*!dtor)
  vunit_gse(2)=cos(thmx_anti*!dtor) 
      
;get anti-strahl particle pitch angle
  if sdat(i).b(0) ne fill then begin
    vsparahat=vunit_gse(0)*bhat(0)+vunit_gse(1)*bhat(1)+vunit_gse(2)*bhat(2)
    data(i).pastrl=acos(vsparahat)/!dtor 
  endif else data(i).pastrl=fill 
            
;estimate anti-strahl beam size at half maximum
method=0
case method of
0: begin
  g=dcomp_tbl(sdat(i).f(0:ndets-1,anti_strphis))     
  ph=sdat(i).phistrl_gse(0:ndets-1,anti_strphis)
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

strahlendat=data
data=0

d=create_struct(d,'swe_strahlendat',strahlendat)

print,'leaving strahl_read.... '
end
