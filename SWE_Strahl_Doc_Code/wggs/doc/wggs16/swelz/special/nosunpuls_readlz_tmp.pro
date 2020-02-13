


;======================= MAIN ======================================


;reads a SWE lz file (RJF Jul2000)

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6
common log_delog,comp_tbl,dcomp_tbl

;1) Determine time interval of no sunpuls. Set nosun1=1 which prints the 
;   spin period determination data (time tagged spin each mafor frame)
;   and the computed spin period 
nosun1=0

;2) Given 




;<<<<<<<<<<<<<<<<<<<<<<<<<<< input parameters >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;dates=['20000714','20000715','20000716']
;recns=[[477,2000],[1,2000],[1,704]]
dates=['20000714']
recns=[450,500]

lpr1=1
lpr2=1 

skiprec=100
n_spins=7
spincnt_last=0


iclicks_sunpulse=4096 & iclicks_bci=40
phistrl_bci0=202.4d
phisc=46.1

;spinp_real=double(2.9922)  ;measured
spinp_real=2.9921820d0  ;2.9921822d0 ;2.9921815d0  ;2.9921799d0  ;2.9921772d0




;spnphx=replicate({ $
;          mjf:0l,$                  ;current time-tagged mjf
;          tjd_tag:0l,$              ;tjd time tag
;          sec_tag:0.d,$             ;second of day time tag
;          spincnt_tag:0l,$          ;time tagged apparent sun pulse count
;          spinp_apparent:0.d,$      ;period of apparent sun pulse
;          phi_spinp:0.d,$           ;rotation angle in each spin betw tim tags
;          phi_bci:0.d,$             ;rot angle each bci    
;          phxsc_tag:0.d,$           ;angle of tim tagged sc-x rel to reference 
;          ispin_diff:lonarr(7),$    ;sun pulse count relative to time tag
;          sunsec_vsbl:dblarr(7),$   ;sec of day each spin using spinp_apparent
;          suntim_vsbl:dblarr(7),$   ;sec of tjd epoch each spin        
;          phxsc:dblarr(7),$         ;phase angle of sc x-axis, 
;                                     cumulative from reference
;          phsun_xsc:dblarr(7),$     ;predict angle of sun relative to phxsc
;          spinp_real:0.d,$          ;actual spin period 
;          mjf_strlsun_ref:0l,$          ;mjf when sun sensed by strahl detector
;          tjd_strlsun_ref:0l,$          ;tjd when sun sensed by strahl detector
;          ispin_strlsun_ref:0l,$       ;spin when sun sensed by strahl detector
;          bci_strlsun_ref:0l,$          ;bci of sun sensed by strahl detector 
;          phi_bci_strlsun_ref:0.d,$     ;rot angle each bci when sun sensed
;          phxsc_tag_strlsun_ref:0.d0,$  ;phxsc_tag of time tagged spin in mjf
;                                     when sun sensed
;},3000)    
          
spnphx=replicate({ $
          mjf:0l,$
          tjd_tag:0l,$
          sec_tag:0.d,$
          spincnt_tag:0l,$  
          spinp_apparent:0.d,$
          phi_spinp:0.d,$
          phi_bci:0.d,$ 
          phxsc_tag:0.d,$
          ispin_diff:lonarr(7),$
          sunsec_vsbl:dblarr(7),$           
          suntim_vsbl:dblarr(7),$          
          phxsc:dblarr(7),$
          phsun_xsc:dblarr(7),$
          spinp_real:0.d,$
          tjd_strlsun_ref:0l,$
          mjf_strlsun_ref:0l,$          
          ispin_strlsun_ref:0l,$       
          bci_strlsun_ref:0l,$           
          phi_bci_strlsun_ref:0.d,$
          phxsc_tag_strlsun_ref:0.d0$              
 },3000)    


tjd_strlsun_ref=1739l
mjf_strlsun_ref=479l
ispin_strlsun_ref=2l
bci_strlsun_ref=49.d0
            
;<<<<<<<<<<<<<<<<<<<<<<<<<<< end input parameters >>>>>>>>>>>>>>>>>>>>>>>>>>>>>





;----------------------- begin ---------------------------------------------- 

i=-1
tjdsec_last=0.d0
phxsc_tag=0.d0

for idate=0,n_elements(dates)-1 do begin

indate=dates(idate)
recs=recns(*,idate)

date=indate

ctmmode=['u','m','s','e']
ctmrate=['s','f']
elecion=['electrons','ions']

rmd=strarr(257)
rmd(1)='sci92' & rmd(3)='man92' & rmd(4)='con92' & rmd(5)='sci46'  
rmd(7)='man46' & rmd(8)='con46' & rmd(128)='trans' & rmd(256)='u'
    
lpr=0
rfill=-1.0e31

;read compress/decompress tables
  decompress_tbl

;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1, mode1 tm map of science and genl hk data offsets into lz.mf
   mode1map
   mode6map
   mode2map

;get mode1 and mode2 sun phase angles of detectors, unit vectors
   phasem1
   phasem2


;open LZ data file and read header, 


;arg=getenv('SWE_LZPATH')+'wi_lz_swe_'+indate+'*'+'.dat'
arg=getenv('SWE_LZPATH')+strmid(indate,2,6)+'*'+'.dat' 
result=findfile(arg,count=count)
flnm=' '
if count gt 0 then begin
     flnm=result(count-1)   ;most recent version
     print,flnm
endif else stop,' file not found'
lzfile=flnm
lundat=1
close,1
close,lundat
openr,lundat,lzfile     ;,/get_lun
fh=read_lzhdr(lundat) ;fh=file header structure 
 
recn_range=[recs(0),recs(1)<fh.nmf]  ;[1,fh.nmf] ;read entire file

recn1=recn_range(0)
recn2=recn_range(1)

print,'date, recn1,recn2 ',date, recn1,recn2



for recn=recn1,recn2 do begin
  
  if recn lt fh.nmf then read_rec,date_time else goto,endlzfile
  i=i+1
  
  ;determine telemetry mode from record header
  if lz.telmod ne 1 and lz.telmod ne 5 then begin
    print,'not a SWE science mode: recn,data record header tm mode indicator ',$
    recn,lz.telmod,rmd(lz.telmod)
  endif
    
  ;determine tm mode from instr hk and test whether in science mode 
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  if tmmode_ihk ne 2 then begin
     print,'not in science mode'
     goto,endrecordloop
  endif
  
  ;determine which science mode 
  scimode_ihk_0=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  ;print,'lz.telmod,tmmode_ihk,scimode_ihk_0 ',$
    ;lz.telmod,tmmode_ihk,scimode_ihk_0
  ;get mjf count
  mjfcnt=bytarr(4)
  mjfcnt(0)=lz.mf(ihk(1).offs)
  
  ;get time tagged spincnt
  time_utc,lz.mf(hkind(ghk(1).offs):hkind(ghk(1).offs)+6),$
    tjd,sec,hour,min,isec,ms,hms,spincnt  
  ms=long(sec*1000l)
  tjdsec=tjdms_sec(tjd,ms)


 ;get period, rotation angle per spincnt and phase angle of sc x-axis 
  ;based on time tagged spins for the current and last mjf's      
  spinp_diff=(tjdsec-tjdsec_last)/(spincnt-spincnt_last)  
  phi_spinp=(spinp_diff/spinp_real)*360.d0
  phi_bci=iclicks_bci*360.d0*(spinp_diff/spinp_real)/iclicks_sunpulse
  
  if nosun1 then begin
    print,tjd,recn,spinp_diff,tjdsec-tjdsec_last,spincnt-spincnt_last
    goto,endrecordloop
  endif  
    
  spincnt_vsbl=intarr(n_spins)
  spincnt_vsbl(0:n_spins-1)=fix(lz.mf((vsm6(0:n_spins-1).offs(0))))
  ispin_diff=intarr(n_spins)
  ispin_diff=spincnt_vsbl-spincnt
  
  w=where(ispin_diff lt -33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)+256
  w=where(ispin_diff gt  33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)-256
  sunsec_vsbl=dblarr(n_spins) 
  sunsec_vsbl=sec+ispin_diff*spinp_diff
  suntim_vsbl=dblarr(n_spins)
  suntim_vsbl=tjd*86400.d0+sunsec_vsbl

  if (tjd eq tjd_strlsun_ref and recn lt mjf_strlsun_ref) then $
  goto,endrecordloop
   
  ;as reference, get phase angle of sc-x for mjf, spin when strahl sees sun
  if tjd eq tjd_strlsun_ref and recn eq mjf_strlsun_ref then begin
    k=ispin_strlsun_ref
    phbci_strlsun=$
      (bci_strlsun_ref*phi_bci+(phistrl_bci0-phisc)) mod 360.d0
    phxsc_strlsun=360.d0- phbci_strlsun
    ;if phxsc_strlsun lt 0.d then phxsc_strlsun=360.d0+phxsc_strlsun
    phxsc_tag=phxsc_strlsun-ispin_diff(k)*phi_spinp
    phxsc_tag=phxsc_tag mod 360.d0
    phxsc=(phxsc_tag+ispin_diff*phi_spinp) mod 360.d0
    phsun_xsc=(360.d0-phxsc) mod 360.d0
    ;if phxsc_tag lt 0.d then phxsc_tag=360.d0+phxsc_tag 
    print,phxsc_strlsun,ispin_diff(k),ispin_diff(k)*phi_spinp,$
      phxsc_tag,format='(f8.1,i6,2f8.1)' 
    i=0 
    stop   
  endif else begin
    
    phxsc_tag=(phxsc_tag_last+(spincnt-spincnt_last)*phi_spinp) mod 360.d0
  
    ;phase angle of sc x-axis each spin relative to time tagged spin this mjf
    phxsc=(phxsc_tag+ispin_diff*phi_spinp) mod 360.d0

    ;predicted phase of sun relative to sc x-axis
    phsun_xsc=(360.d0-phxsc) mod 360.d0
  endelse
    
  ;make structure assignments  
  spnphx(i).mjf=recn
  spnphx(i).tjd_tag=tjd
  spnphx(i).sec_tag=sec
  spnphx(i).spincnt_tag=spincnt
  spnphx(i).spinp_apparent=spinp_diff
  spnphx(i).phi_spinp=phi_spinp
  spnphx(i).phi_bci=phi_bci
  spnphx(i).phxsc_tag=phxsc_tag
  spnphx(i).ispin_diff=ispin_diff
  spnphx(i).sunsec_vsbl=sunsec_vsbl
  spnphx(i).suntim_vsbl=suntim_vsbl
  spnphx(i).phxsc=phxsc
  spnphx(i).phsun_xsc=phsun_xsc
  spnphx(i).spinp_real=spinp_real
  spnphx(i).tjd_strlsun_ref=tjd_strlsun_ref
  spnphx(i).mjf_strlsun_ref=mjf_strlsun_ref
  spnphx(i).ispin_strlsun_ref=ispin_strlsun_ref
  spnphx(i).bci_strlsun_ref=bci_strlsun_ref
  
  
  point1:
    
  if lpr1 then print,recn,tjd,sec, $
    spincnt,spinp_diff,phi_spinp,$
    format='(i8,i6,f11.3,i6,f12.7,f8.1)'
    
  if lpr2 then begin
    for k=0,n_spins-1 do begin
      print,recn,tjd,sec,k,$
      ispin_diff(k),sunsec_vsbl(k),phxsc_tag,phxsc_tag_last,phxsc(k),$
      phsun_xsc(k),$
      format='(i8,i6,f11.3,i3,i6,f11.3,4f8.1)'
    endfor
  endif

 
  endrecordloop: 
  tjdsec_last=tjdsec
  spincnt_last=spincnt 
  phxsc_tag_last=phxsc_tag
    
  

endfor     ;end record loop
endlzfile:
close,lundat
endfor       ;end date loop

spnphx=spnphx(0:i)

;save,filename=getenv('IDLSAV')+'spnphxdat',spnphx




stop
end



