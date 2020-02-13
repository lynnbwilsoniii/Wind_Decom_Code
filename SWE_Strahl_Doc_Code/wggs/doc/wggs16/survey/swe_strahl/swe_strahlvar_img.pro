;================== pro strahlvar_img ===================================

pro swe_strahlvar_img,data

common log_delog,comp_tbl,dcomp_tbl
common swestuff,swest
common wstuff,wst

idatype=where(d.datype eq 'swe_strahl')

sdat=d.swe_strahldat

sz=size(sdat.f)
ndets=sz(1)
nstrphis=sz(2)
nsdat=sz(3)

fill=-1.e31
data=replicate({pb5:lonarr(3),elapsec:0l,en:0.,strl:0.,pstrl:fill,wstrl:0.,$
                       astrl:0.,pastrl:0.,wastrl:0.},nsdat)

thesdet=[61.30, 65.65, 70.46,   75.35,  80.35,  84.39, $
           94.89, 99.79, 104.67, 109.52, 114.35, 118.75]
                                  
for i=0,nsdat-1 do begin 

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
  g=dcomp_tbl(sdat(i).f(0:ndets-1,anti_strphis))     
  ph=sdat(i).phistrl_gse(0:ndets-1,anti_strphis)
  cl=0.5*float(dcomp_tbl(max_anti)) 
  wcl=where(g ge cl)
  if wcl(0) ne -1 then begin
    xmn=min(ph(wcl),max=xmx)
    ymn=min(th(wcl),max=ymx)
    data(i).wastrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
  endif      
                 
endfor

end

