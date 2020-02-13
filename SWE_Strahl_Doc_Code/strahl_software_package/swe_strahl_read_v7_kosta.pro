; date in time_string formate ('2001-12-31')

;pro swe_strahl_read_v7,tjd0_thisfile=tjd0   ; commented 06/04/2016

; READS VERSIONS 6 AND 7 *.strahl files

FUNCTION swe_strahl_read_v7_kosta, date

common shared,d
common wstuff,wst
common swestuff,swest
common log_delog,comp_tbl,dcomp_tbl

CATCH, error_status
IF error_status NE 0 THEN BEGIN
RETURN, {lzrec:!values.f_nan, spinbl:!values.f_nan, ta:!values.f_nan, pb5tim:!values.f_nan, $
         enstep:!values.f_nan, f:!values.f_nan, phi:!values.f_nan, b:!values.f_nan, misc:!values.f_nan, $
         elapsec:!values.f_nan, en:!values.f_nan, scimod:!values.f_nan, $
         theta:!values.f_nan, nrec:!values.f_nan, time:!values.f_nan, status:0}
ENDIF

;fiodial_append_req,' '                     ; commented 06/04/2016
;fiodial_append_req,'swe_strahl_read_v7: ['+$            ; commented 06/04/2016
;                   string(wst.number_days,Format='(I2.2)')+' date(s)...]' ; commented 06/04/2016

;idatype=where(d.datype eq 'swe_strahl')
;flnm=d.flnm(idatype)
flnm = getenv('STRAHL_DATA') + strmid(date, 0, 4) + strmid(date, 5, 2) + strmid(date, 8, 2) + '_v7.strahl'

openr,/Swap_if_Little_Endian,lunp,flnm,/get_lun
;print,' ' &
;fiodial_append_req,'reading SWE electron strahl file '+flnm+'...'        ; commented 06/04/2016

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
        phistrl_gse:fltarr(nstrphis), $
        b:fltarr(3),$
        misc:fltarr(5)  }
data=replicate(indat,nrec)
readu,lunp,data
free_lun,lunp
;print,'end reading file ',flnm

if data(n_elements(data)-1).pb5tim(0) le 1995l and $
   data(n_elements(data)-1).pb5tim(1) lt 107l then $
   data(*).misc(0)=49.                      ;strahl voltage bias level

;v commented 06/04/2016 v   
;;if multiple days 
;if wst.number_days gt 1 then begin
;  thisflnm=flnm
;  thisdate=wst.indate
;  for inxt=1,wst.number_days-1 do begin
;    thispb5=ymd_pb5(long(thisdate))
;    thissec=pb5_sec(thispb5)
;    pb5_next=sec_pb5(thissec+long(86400))
;    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
;    nullstr=''
;;    file_next=$                                               ; commented 06/04/2016
;;    get_flnm('strahl',getenv(d.pathenv(idatype)),nullstr,'.strahl',$
;;      date_next,err=err)
;    if err ne '' then goto,getoutloop  
;    openr,/Swap_if_Little_Endian,lun_next,file_next,/get_lun
;;    fiodial_append_req,'reading SWE electron strahl file '+file_next+'...'       ; commented 06/04/2016
;
;    nrec_next=0l 
;    nstrdets_next=0l
;    nstrphis_next=0l
;    readu,lunp,nrec_next,nstrdets_next,nstrphis_next
;    ;print,nrec_next,nstrdets_next,nstrphis_next
;    data_next=replicate(indat,nrec_next)
;    readu,lun_next,data_next
;    free_lun,lun_next
;        
;    if data_next(n_elements(data_next)-1).pb5tim(0) le 1995l and $
;    data_next(n_elements(data_next)-1).pb5tim(1) lt 107l then $
;    data_next(*).misc(0)=49.                      ;strahl voltage bias level
;    
;    ;print,'end reading next file ',file_next
;    data=[temporary(data),data_next]
;    data_next=0
;    thisflnm=file_next
;    thisdate=date_next
;  endfor
;endif
;
;getoutloop:
;
;^ commented 06/04/2016 ^

wok=where(data.enstep le 64)
data=data(wok)

; v commented 06/04/2016 v
;d.ndx(0,idatype)=0
;d.ndx(1,idatype)=n_elements(data)-1  ;begin and end time indices
;d.ndx_orig(*,idatype)=d.ndx(*,idatype)
; ^ commented 06/04/2016 ^

;strahldat=data

;get start time for this file
  
 tjd0=long(fix(data(20).ta/86400.d)) 
    

;----------------------- NOTE! --------------------------------------------
;modify the strahl data structure, saving only the max strahl; filter out sun
;fiodial_append_req,'find max each spin and filter out the sun...'      ; commented 06/04/2016

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

thesdet=[61.30, 65.65, 70.46,   75.35,  80.35,  84.39, $
           94.89, 99.79, 104.67, 109.52, 114.35, 118.75]   ; thetas (12 measurements centered on ecliptic)

;phistrl=[  $
;       204.15781,       207.67344,       211.18906,       214.70469,  $
;       218.22031,       221.73594,       225.25156,       228.76719,  $
;       232.28281,       235.79844,       239.31406,       242.82969,  $
;       246.34531,       249.86094,       23.454687,       26.970312,  $
;       30.485937,       34.001562,       37.517187,       41.032812,  $
;       44.548437,       48.064062,       51.579687,       55.095312,  $
;       58.610937,       62.126562,       65.642187,       69.157812   ]


;time
data.ta=sdat.ta
data.pb5=sdat.pb5tim

;energy step
data.enstep=sdat.enstep

;get mode
  data.scimod=sdat.misc(1)
                                    
for i=0l,nsdat-1 do begin 
  
;set f ge fmx = 0 (strahl flux values sdat(i).f are 12-to-8-compressed bytes)
; v commented 06/04/2016 v
;  fmx=comp_tbl(wst.newstrlmax) ;plots f lt fmx 
;  wfmx=where(sdat(i).f ge fmx)
;  if wfmx(0) ne -1 then sdat(i).f(wfmx)=0
; ^ commented 06/04/2016 ^

;energy this strahl spectra (this spin)
;  data(i).en=long(volt_en_strl(sdat(i).enstep,/en))  ; commented 06/04/2016
  data(i).en=volt_en_strl(sdat(i).enstep,/en)
;  data(i).en=volt_en_strl_kosta(sdat(i).enstep,/en)  ; added 06/04/2016
  
;find the detector-spinphase sample in this spin with the maximum counts
;get the maximum counts and the corresponding pitch angle, also
;get the maximum counts in the opposite strahl sector, i.e., anti-strahl
;also get beam width for strahl and anti-strahl

     
;apply sunmask     
;sunmask location in phi_gse
;  v commented 06/04/2016 v
;  phsunindx=where(sdat(i).phistrl_gse gt swest.strlphsun(0) and $
;                  sdat(i).phistrl_gse lt swest.strlphsun(1),nphsunindx)
;  if nphsunindx gt 1 then $
;    sdat(i).f(*,phsunindx(0):phsunindx(nphsunindx-1))=0                   
;  ^ commented 06/04/2016 ^

;maximum counts is assumed to be the strahl and phmx, thmx are directions
;from which the strahl arrives 

;;---------------------- find the strahl ----------------------------- 
;;find phi of maximum counts 
;  max=max(sdat(i).f,mxind,min=min)
;;  data(i).strl=float(dcomp_tbl(max)) ;DECOMPRESS the strahl max from 8 to 12-bit  ; commented 06/04/2016
;  data(i).strl=float(max) ; added 06/04/2016
;  data(i).pb5=sdat(i).pb5tim      
;  phasemxind=mxind/ndets
;  detmxind=mxind-phasemxind*ndets
;  data(i).strlmaxdet=detmxind
;  phmx=double(sdat(i).phistrl_gse(phasemxind))
;  thmx=double(thesdet(detmxind))
;    
;;get unitvector of strahl particle velocity
;  vunit_gse=dblarr(3)
;  vunit_gse(0)=sin(thmx*!dtor)*cos(phmx*!dtor)
;  vunit_gse(1)=sin(thmx*!dtor)*sin(phmx*!dtor)
;  vunit_gse(2)=cos(thmx*!dtor)
;            
;;get strahl particle pitch angle
;  if sdat(i).b(0) ne fill then begin
;    bhat=sdat(i).b/sqrt(total(sdat(i).b^2))    
;    vsparahat=$
;        vunit_gse(0)*bhat(0)+vunit_gse(1)*bhat(1)+vunit_gse(2)*bhat(2)
;    data(i).pstrl=acos(vsparahat)/!dtor
;  endif else data(i).pstrl=fill  
;   
;;get the strahl and anti-strahl phi indices
;  if phasemxind lt nstrphis/2 then begin
;    anti_strphis=nstrphis/2 + indgen(nstrphis/2)
;    strphis=indgen(nstrphis/2)
;  endif else begin
;    anti_strphis=indgen(nstrphis/2)
;    strphis=nstrphis/2 + indgen(nstrphis/2)
;  endelse   
;      
;;estimate strahl beam size at half maximum
;method=0
;case method of
;0: begin
;  g=sdat(i).f(0:ndets-1,strphis)
;;  g=dcomp_tbl(sdat(i).f(0:ndets-1,strphis))   ; commented 06/04/2016
;  ph=sdat(i).phistrl_gse(0:ndets-1,strphis)
;  th=thesdet#replicate(1.,nstrphis/2)
;;  cl=0.5*float(dcomp_tbl(max))   ; commented 06/04/2016 
;  cl=0.5*float(max)
;  wcl=where(g ge cl)
;  if wcl(0) ne -1 then begin
;    xmn=min(ph(wcl),max=xmx)
;    ymn=min(th(wcl),max=ymx)
;    data(i).wstrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
;  endif
;endcase
;
;1:
;endcase        
;
;;------- now find the anti-strahl ---------------------------------------  
;     
;  ganti=sdat(i).f(0:ndets-1,anti_strphis)
;  phanti=sdat(i).phistrl_gse(0:ndets-1,anti_strphis)
;  max_anti=max(ganti,mxind_anti)      
;;  data(i).astrl=float(dcomp_tbl(max_anti))    ; commented 06/04/2016
;  data(i).astrl=float(max_anti)
;      
;  phasemxind=mxind_anti/ndets
;  detmxind=mxind_anti-phasemxind*ndets
;  phmx_anti=double(phanti(phasemxind))
;  thmx_anti=double(thesdet(detmxind))
;    
;;get unitvector of anti-strahl particle velocity
;  vunit_gse=dblarr(3)
;  vunit_gse(0)=sin(thmx_anti*!dtor)*cos(phmx_anti*!dtor)
;  vunit_gse(1)=sin(thmx_anti*!dtor)*sin(phmx_anti*!dtor)
;  vunit_gse(2)=cos(thmx_anti*!dtor) 
;      
;;get anti-strahl particle pitch angle
;  if sdat(i).b(0) ne fill then begin
;    vsparahat=vunit_gse(0)*bhat(0)+vunit_gse(1)*bhat(1)+vunit_gse(2)*bhat(2)
;    data(i).pastrl=acos(vsparahat)/!dtor 
;  endif else data(i).pastrl=fill 
;      
;;estimate anti-strahl beam size at half maximum
;method=0
;case method of
;0: begin
;;  g=dcomp_tbl(sdat(i).f(0:ndets-1,anti_strphis))     ; commented 06/04/2016
;  g=sdat(i).f(0:ndets-1,anti_strphis) 
;  ph=sdat(i).phistrl_gse(0:ndets-1,anti_strphis)
;;  cl=0.5*float(dcomp_tbl(max_anti))  ; commented 06/04/2016
;  cl=0.5*float(max_anti)
;  wcl=where(g ge cl)
;  if wcl(0) ne -1 then begin
;    xmn=min(ph(wcl),max=xmx)
;    ymn=min(th(wcl),max=ymx)
;    data(i).wastrl=(abs(xmx-xmn)+abs(ymx-ymn) )/2
;  endif      
;endcase

;1:
;endcase 
                 
endfor

strahldat=data
data=0

;;; new lines

nrec = n_elements(sdat.ta)
phistrl_gse = dblarr(14, nrec)
phiastrl_gse = dblarr(14, nrec)
FOR i = 0L, n_elements(sdat.ta)-1L DO BEGIN
phistrl_gse[*,i] = reverse(sdat[i].phistrl_gse[14:*])
phiastrl_gse[*,i] = sdat[i].phistrl_gse[0:13]  ; descending order, matches how data is arranged in f_anti
ENDFOR
;FOR i = 0L, n_elements(sdat.ta)-1L DO phistrl_gse[*,i] = sdat[i].phistrl_gse[14:*]

;;f_out = sdat.f[*,14:*,*]
f_out = dblarr(14,12, nrec)
f_anti_out = dblarr(14,12, nrec)
phi_out = phistrl_gse*0.d
phi_anti_out = phiastrl_gse*0.d
;FOR i=0L, nrec-1L DO BEGIN
;FOR j=0L, 13 DO BEGIN
;;   f_out[j, *, i] = sdat[i].f[*, 14+j]
;   f_out[j, *, i] = reverse(sdat[i].f[*, 14+j])
;ENDFOR
;ENDFOR

;CATCH, error_status
;IF error_status NE 0 THEN BEGIN
;RETURN, {lzrec:!values.f_nan, spinbl:!values.f_nan, ta:!values.f_nan, pb5tim:!values.f_nan, $
;         enstep:!values.f_nan, f:!values.f_nan, phi:!values.f_nan, b:!values.f_nan, misc:!values.f_nan, $
;         elapsec:!values.f_nan, en:!values.f_nan, scimod:!values.f_nan, $
;         theta:!values.f_nan, nrec:!values.f_nan, time:!values.f_nan, status:0}
;
;ENDIF

; apply sunmask and mask out spurious 255 counts

FOR i=0L, nrec-1L DO BEGIN
;   f_out[*, *, i] = sdat[i].f[*, 14:*]
   f_out_temp = mask_255(sunmask(rotate(transpose(sdat[i].f[*, 14:*]), 2), phistrl_gse[*,i]))
   f_anti_out_temp = mask_255(sunmask(rotate(transpose(sdat[i].f[*, 0:13]), 2), phiastrl_gse[*,i]))
   IF max(f_out_temp) GT max(f_anti_out_temp) THEN BEGIN
      f_out[*, *, i] = f_out_temp
      f_anti_out[*, *, i] = f_anti_out_temp
      phi_out[*,i] = phistrl_gse[*,i]
      phi_anti_out[*,i] = phiastrl_gse[*,i]
   ENDIF ELSE BEGIN
      f_out[*, *, i] = f_anti_out_temp
      f_anti_out[*, *, i] = f_out_temp
      phi_out[*,i] = (phiastrl_gse[*,i] + 720.d) mod 360.d
      phi_anti_out[*,i] = (phistrl_gse[*,i] + 720) mod 360.d
   ENDELSE
ENDFOR

;decompress counts from 8-bit to 12-bit (counts were compressed, due to bandwidth limitations perhaps?)
f_out = dcomp_tbl_kosta(f_out)
f_anti_out = dcomp_tbl_kosta(f_anti_out)

time = reform(time_double(strc(sdat.pb5tim[0,*]) + '-01-01/00:00:00') + (sdat.pb5tim[1,*]-1L) * 86400.d + sdat.pb5tim[2,*]/1000.d)

;time = sdat.ta + (9413.d * 86400.d)
;;IF time_double(date) LE time_double('1995-10-09/23:59:59') THEN time = time - 86400. * 10000.d
;time_d = time_double(strmid(date, 0, 10))
;diff_days = fix((time - time_d) / 86400.d)*86400.d
;time = time - diff_days * (abs(diff_days) GT 2)
;; fix offsets of integer number of days, caused by error in tjd_pb5.pro?

theta_gse = -reverse(thesdet) + 90.d
;theta_gse = -thesdet + 90.d

return, {lzrec:sdat.lzrec, spinbl:sdat.spinbl, ta:sdat.ta, pb5tim:sdat.pb5tim, $
         enstep:sdat.enstep, f:f_out, f_anti:f_anti_out, phi:phi_out, phi_anti:phi_anti_out,b:sdat.b, misc:sdat.misc, $
         elapsec:strahldat.elapsec, en:strahldat.en, scimod:strahldat.scimod, $
         strl: strahldat.strl, pstrl:strahldat.pstrl, wstrl:strahldat.wstrl, $
         strlmaxdet:strahldat.strlmaxdet, astrl:strahldat.astrl, pastrl:strahldat.pastrl, wastrl:strahldat.wastrl, $
         theta:theta_gse, nrec:nrec, time:time, status:1}
;return, {lzrec:sdat.lzrec, spinbl:sdat.spinbl, ta:sdat.ta, pb5tim:sdat.pb5tim, $
;         enstep:sdat.enstep, f:f_out, f_anti:f_anti_out, phi:phistrl_gse, phi_anti:phiastrl_gse,b:sdat.b, misc:sdat.misc, $
;         elapsec:strahldat.elapsec, en:strahldat.en, scimod:strahldat.scimod, $
;         strl: strahldat.strl, pstrl:strahldat.pstrl, wstrl:strahldat.wstrl, $
;         strlmaxdet:strahldat.strlmaxdet, astrl:strahldat.astrl, pastrl:strahldat.pastrl, wastrl:strahldat.wastrl, $
;         theta:theta_gse, nrec:nrec, time:sdat.ta + (9413.d * 86400.d), status:1}

end
