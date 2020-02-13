;======================= MAIN: nosunpuls_readlz.pro ===========================


;reads a SWE lz file (RJF Jul2000)

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6
common log_delog,comp_tbl,dcomp_tbl


;------------ Determine spin period and spin phase of x-axis ----------------

;-----Method: Strahl measurements of the Sun are used to determine the true
;             spin period. The spin phase of the spacecraft X-axis is determined
;             at a reference point in the no-sun pulse interval and subsequent
;             values of calculated spin phase of the Sun are predicted using
;             an estimate of the the true spin period. Refined values are 
;             obtained by iteration, i.e., adjusting the spin period and 
;             repeating the procedure. The final spin period and predicted
;             spin phase of X-axis are then used in SWE levelzero processing.

;-----Detailed description: 
;1) Determine time interval of no sunpuls. Set nosun1=1 which prints the 
;   apparent spin period determination data, i.e., time tagged spin each 
;   major frame (mjf).
    nosun1=0

;2) Strahl measurements of the Sun are used at two separated times within the
;   the no-sunpulse interval to determine the true spin period.
;   Using nominal values for the true spin period and the bci location of the
;   strahl measurement of the Sun in the first reference mjf, bci_ref for j=0, 
;   compute the spin phase of the mjf time-tagged s/c x-axis, phxsc_tag. Then
;   compute the sc x-axis spin phase for each SWE data spin in the mjf, phxsc. 
;   Then compute phxsc_tag for the next mjf time-tagged s/c x-axis and phxsc 
;   for each data spin and so on for successive mjf's. 
   
;   The first strahl measurement of the Sun used should be as early as possible
;   in in the no-sunpulse interval, j=0. The purpose is to determine the initial
;   phxsc_tag on which successive incrementing of elapsed phase angle between
;   time-tagged mjf's is based. 
;   At selected subsequent time intervals spaced throughout the nosun interval, 
;   j=1,2,.., when the strahl sensor sees the Sun, spin phase of the Sun, phsun, 
;   is computed using 
;       (i) the computed phxsc_tag, 
;       (ii) the phase angle of the Sun as measured by the strahl sensor, 
;       (iii) and the estimated true spin period, spinp_real.
;   phsun should be 0 +/- 4 degrees if the estimate for spinp_real is correct.
;   The +/- 4 is the precision of the strahl sensor Sun measurement. 
;   Iteratively refine spinp_real as elapsed time between the first (j=0)
;   and subsequent strahl Sun measurements (j=1,2,3,...) increases.  

;   In practise, this program reads every record, 
;   computing the time-tagged phxsc_tag and the phxsc for each data spin. The
;   program stops at the mjf specified by j (the strahl sensor Sun measurement)
;   and prints phsun, which should be 0 +/- 4 degrees. If not, then refine 
;   spinp_real, and repeat for the given j. Then increment j and repeat. 
;   Very few strahl sensor Sun measurements are needed but they should span the
;   entire no-sunpulse interval in order to gain precision.
 
;3) After the spin period has been determined, run the program throughout the 
;   interval. For each mjf, time-tagged phxsc_tag and the phxsc each data spin
;   are computed and saved in the structure spnphx. Then save spnphx in an IDL 
;   save file, which will be used by procedure nosunpuls_get_spnphx.pro, 
;   which is called by procedure mode6.pro for all times in the
;   no-sunpulse interval. The time interval must be added to procedure 
;   mode6.pro following the previous "no sun pulse " entries in mode6.pro.


;---define widgets and initialize control structures
  define_widgets;,WDGT
  setpaths
  
;--------------------------input parameters-----------------------------------  
;---specify which nosunpuls event
nosunpuls_event='20011106'  ;'20001109'  ;'20000714'   
case nosunpuls_event of

'20000714' : begin

  ;---Reference measurements when the strahl sensor sees the Sun:
  nj=9
  ;date=            20000714           20000715         20000716
  tjd_ref=  long([1739, 1739, 1739, 1740, 1740, 1740, 1741, 1741, 1741])
  mjf_ref=  long([ 479,  687,  913,  239,  504,  725,  174,  393,  653])
  ispin_ref=long([   2,    0,    4,    5,    6,    2,    1,    4,    6])
  bci_ref=       [49.0, 43.0, 46.0, 62.0, 78.0, 16.0, 65.0, 39.0, 52.0]
   
  ;---The first reference strahl measurement of the Sun: 
  j=0 
  tjd_strlsun_ref=tjd_ref(j)      
  mjf_strlsun_ref=mjf_ref(j)
  ispin_strlsun_ref=ispin_ref(j)
  bci_strlsun_ref=bci_ref(j)
    
  ;---The second reference strahl measurement of the Sun:
  j=8  ;7  ;6  ;5  ;4  ;3  ;2  ;1
  tjd_strlsun_ref1=tjd_ref(j)      
  mjf_strlsun_ref1=mjf_ref(j)
  ispin_strlsun_ref1=ispin_ref(j)
  bci_strlsun_ref1=bci_ref(j)

  ;---History of the iterations used to determine the true spin period:
  ;spinp_real=2.9921935d0 - 0.000015d0 ;0.00001d0       ;j=1
  ;spinp_real=2.9921935d0 - 0.000016d0  ;0.000015d0       ;j=2 
  ;spinp_real=2.9921935d0 - 0.000013d0  ;0.000015d0  ;.000016d0       ;j=3
  ;spinp_real=2.9921935d0 - 0.000011d0    ;0.000013d0      ;j=4
  ;spinp_real=2.9921935d0 - 0.0000115d0   ;0.000011d0          ;j=5
  ;spinp_real=2.9921935d0 - 0.0000111d0 ;0.0000113d0   ;0.0000115d0    ;j=6
  ;spinp_real=2.9921935d0 - 0.0000113d0  ;0.0000111d0   ;j=7
  spinp_real=2.9921935d0 -  0.0000115d0;0.0000117d0;0.0000116d0;0.0000113d0;j=8
  ;final result: spinp_real=2.9921935d0 -  0.0000115d0 = 2.9921820d0

  dates=['20000714','20000715','20000716']
  recns=[[450,2000],[1,2000],[1,704]]
  ;dates=['20000714']
  ;recns=[450,500]

endcase ;end input data for nosunpuls_event = '20000714'

'20001109' : begin
  dates=['20001109']   ;['20001108','20001109']
  recns=[1,2000]       ;[[900,2000],[1,2000]]

  ;---Reference measurements when the strahl sensor sees the Sun:
  nj=24
  ;date=            '20001109'
  tjd_ref=  1857l+lonarr(nj)
  mjf_ref=  long([  72,104,145,159,163,177,218,236,250,268,328,341,387,419,447,479,506,524,597,616,629,643,689,735])
  ispin_ref=long([   1,  1,  2,  0,  5,  2,  3,  5,  3,  5,  1,  6,  5,  5,  0,  1,  3,  5,  6,  1,  6,  4,  2,  0])
  bci_ref=       [  23.,18.,26.,53.,21.,20.,43.,26.,37.,20.,26.,38.,28.,23.,52.,30.,42.,25.,44.,28.,40.,51.,30.,25.] 

  ;---The first reference strahl measurement of the Sun: 
  j=0 
  tjd_strlsun_ref=tjd_ref(j)      
  mjf_strlsun_ref=mjf_ref(j)
  ispin_strlsun_ref=ispin_ref(j)
  bci_strlsun_ref=bci_ref(j)
    
  ;---The second reference strahl measurement of the Sun:
  j=18   ;23;22;21;20;19;17;15;13;11;9;7;5;3;1
  tjd_strlsun_ref1=tjd_ref(j)      
  mjf_strlsun_ref1=mjf_ref(j)
  ispin_strlsun_ref1=ispin_ref(j)
  bci_strlsun_ref1=bci_ref(j)

  ;---History of the iterations used to determine the true spin period:    
  ;spinp_real=3.07067d0 ;j=1
  ;spinp_real=3.07067d0 - 0.000005d0 ;j=3
  ;spinp_real=3.07067d0 - 0.000002d0 ;j=5
  ;spinp_real=3.07067d0 - 0.000002d0 ;j=7
  ;spinp_real=3.07067d0 - 0.000006d0 ;j=9
  ;spinp_real=3.07067d0 - 0.000006d0 ;j=11
  ;spinp_real=3.07067d0 - 0.000005d0 ;j=13
  ;spinp_real=3.07067d0 - 0.000004d0 ;j=15
  ;spinp_real=3.07067d0 - 0.000003d0 ;j=17
  ;spinp_real=3.07067d0 - 0.0000005d0 ;j=19
  ;spinp_real=3.07067d0 - 0.0000005d0 ;j=20
  ;spinp_real=3.07067d0 - 0.0000011d0 ;j=21
  ;spinp_real=3.07067d0 - 0.0000007d0 ;j=22
   spinp_real=3.07067d0 - 0.0000002d0  ;0.0000005d0 ;j=23
  ;final result: spinp_real=3.07067d0 - 0.0000002d0 = 3.0706698d0
  
endcase ;end input data for nosunpuls_event = '20001109'

'20011106' : begin
  dates=['20011106']  
  recns=[68,851]  ;[73,848] ;[10,900]       
  
  ;---Reference measurements when the strahl sensor sees the Sun:
  nj=3
  tjd_ref=  long([2219,2219,2219])
  mjf_ref=  long([235,445,628])
  ispin_ref=long([2,3,2])
  bci_ref=       [60.0,34.0,50.0]
   
  ;---The first reference strahl measurement of the Sun: 
  j=0 
  tjd_strlsun_ref=tjd_ref(j)      
  mjf_strlsun_ref=mjf_ref(j)
  ispin_strlsun_ref=ispin_ref(j)
  bci_strlsun_ref=bci_ref(j)
    
  ;---The second reference strahl measurement of the Sun:
  j=2
  tjd_strlsun_ref1=tjd_ref(j)      
  mjf_strlsun_ref1=mjf_ref(j)
  ispin_strlsun_ref1=ispin_ref(j)
  bci_strlsun_ref1=bci_ref(j)

  ;--History of the iterations used to determine the true spin period:
  ;spinp_real=2.9921935d0 - 0.000015d0 ;0.00001d0       ;j=1
  spinp_real=3.11055d0 - 0.000014d0 - 0.000004d0  ;=3.1105320
     
endcase
endcase

lpr1=1
lpr2=0 
skiprec=100
n_spins=7
spincnt_last=0

;---SWE samples each spin 
iclicks_sunpulse=4096 & iclicks_bci=40
;---strahl sensor coordinates in sc
phistrl_bci0=202.4d
phisc=46.1


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
          spinp_real:0.d,$
          tjd_strlsun_ref:0l,$
          mjf_strlsun_ref:0l,$          
          ispin_strlsun_ref:0l,$       
          bci_strlsun_ref:0l,$           
          phi_bci_strlsun_ref:0.d,$
          phxsc_tag_strlsun_ref:0.d0$              
 },3000)    



            
;--------------------------end input parameters--------------------------------


;----------------------- begin ---------------------------------------------- 

;---definitions
ctmmode=['u','m','s','e']
ctmrate=['s','f']
elecion=['electrons','ions']
lpr=0
rfill=-1.0e31

;---initialization
i=-1
tjdsec_last=0.d0
phxsc_tag=0.d0
  
for idate=0,n_elements(dates)-1 do begin

  indate=dates(idate)
  recs=recns(*,idate) 
  date=indate

  rmd=strarr(257)
  rmd(1)='sci92' & rmd(3)='man92' & rmd(4)='con92' & rmd(5)='sci46'  
  rmd(7)='man46' & rmd(8)='con46' & rmd(128)='trans' & rmd(256)='u'
    
  ;---read compress/decompress tables
  decompress_tbl

  ;---get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

  ;---get mode1, mode1 tm map of science and genl hk data offsets into lz.mf
   mode1map
   mode6map
   mode2map

  ;---get mode1 and mode2 sun phase angles of detectors, unit vectors
   phasem1
   phasem2


  ;---open LZ data file and read header 
  ;arg=getenv('SWE_LZPATH')+'wi_lz_swe_'+indate+'*'+'.dat'
  arg=getenv('SWE_LZPATH')+'*'+strmid(indate,2,6)+'*'+'.dat' 
  result=findfile(arg,count=count)
  flnm=' '
  if count gt 0 then begin
     flnm=result(count-1)   ;most recent version
     print,flnm
  endif else stop,' file not found'
  lzfile=flnm
  openr,lundat,lzfile,/get_lun
  fh=read_lzhdr(lundat) ;fh=file header structure  
  recn_range=[recs(0),recs(1)<fh.nmf]  ;[1,fh.nmf] ;read entire file
  recn1=recn_range(0)
  recn2=recn_range(1)
  print,'date, recn1,recn2 ',date, recn1,recn2

  for recn=recn1,recn2 do begin  
    if recn lt fh.nmf then read_rec,date_time else goto,endlzfile
    i=i+1
    ;---determine telemetry mode from record header
    if lz.telmod ne 1 and lz.telmod ne 5 then begin
      print,'not a SWE science mode: recn,data record header tm mode indicator ',$
      recn,lz.telmod,rmd(lz.telmod)
    endif
    
    ;---determine tm mode from instr hk and test whether in science mode 
    tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
    if tmmode_ihk ne 2 then begin
      print,'not in science mode'
      goto,endrecordloop
    endif
  
    ;---determine which science mode 
    scimode_ihk_0=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
    ;print,'lz.telmod,tmmode_ihk,scimode_ihk_0 ',$
    ;lz.telmod,tmmode_ihk,scimode_ihk_0
    
    ;---get mjf count
    mjfcnt=bytarr(4)
    mjfcnt(0)=lz.mf(ihk(1).offs)
    
    ;---get time tagged spin counter, spincnt 
    ;---tjd,sec are truncated Julian day number and seconds of day
    time_utc,lz.mf(hkind(ghk(1).offs):hkind(ghk(1).offs)+6),$
      tjd,sec,hour,min,isec,ms,hms,spincnt  
    ms=long(sec*1000l)
    tjdsec=tjdms_sec(tjd,ms)

   ;---get period, rotation angle per spincnt and phase angle of sc x-axis 
   ;   based on time tagged spins for the current and last mjf's      
   spinp_diff=(tjdsec-tjdsec_last)/(spincnt-spincnt_last)  
   phi_spinp=(spinp_diff/spinp_real)*360.d0
   phi_bci=iclicks_bci*360.d0*(spinp_diff/spinp_real)/iclicks_sunpulse

   ;---print time between current and last tagged mjf's in order to estimate
   ;   spin period 
   if nosun1 then begin  
     datetime=pb5_ymdhms(tjd_pb5(tjd,long(sec*1000)))
     print,tjd,sec,tjdsec,datetime,recn,spinp_diff,tjdsec-tjdsec_last,$
      spincnt-spincnt_last,$
      format='(i4,f10.3,f15.3,a20,i5,f11.7,f10.6,i4)
     goto,endrecordloop
   endif  

   ;---spin counter for each of data spins in mjf and change since last    
   spincnt_vsbl=intarr(n_spins)
   spincnt_vsbl(0:n_spins-1)=fix(lz.mf((vsm6(0:n_spins-1).offs(0))))
   ispin_diff=intarr(n_spins)
   ispin_diff=spincnt_vsbl-spincnt
  
   w=where(ispin_diff lt -33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)+256
   w=where(ispin_diff gt  33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)-256

   ;---seconds of this day and seconds from tjd=0 for each data spin 
   sunsec_vsbl=dblarr(n_spins) 
   sunsec_vsbl=sec+ispin_diff*spinp_diff
   suntim_vsbl=dblarr(n_spins)
   suntim_vsbl=tjd*86400.d0+sunsec_vsbl
  
  if (tjd eq tjd_strlsun_ref and recn lt mjf_strlsun_ref) then $
    goto,endrecordloop
    
  ;---as reference, get initial phase angle of sc-x for mjf, spin when 
  ;   strahl sensor sees Sun
  if tjd eq tjd_strlsun_ref and recn eq mjf_strlsun_ref then begin
    k=ispin_strlsun_ref
    phbci_strlsun=$
      (bci_strlsun_ref*phi_bci+(phistrl_bci0-phisc)) mod 360.d0 
    phxsc_strlsun=360.d0- phbci_strlsun
    phxsc_tag=(phxsc_strlsun-ispin_diff(k)*phi_spinp) mod 360.d0  
    ;---where
    ;   phxsc_tag=spin phase of time-tagged mjf and used as reference       
    ;   spinp_diff=the apparent spin period (2.4s)
    ;   spinp_real=real spin period estimate
    ;   phi_bci=apparent spin phase per sample
    ;          =iclicks_bci*360.d0*(spinp_diff/spinp_real)/iclicks_sunpulse  
    ;   bci_strlsun_ref=measured number of strahl sensor spin phase samples
    ;   phistrl_bci0, phisc are sc coordinate system constants 
    ;   phxsc_strlsun=phase angle of sc x-axis rel to Sun this spin  
    ;   phbci_strlsun=strahl measurement of Sun rel to sc x-axis
    ;   ispin_diff(k)=number spins between time-tag and sun measured spin
    ;   phi_spinp=degrees of rotation between pseudo sun pulses
    ;            =(spinp_diff/spinp_real)*360.d0
    
    ;---phase angle of sc x-axis rel to Sun for all spins this mjf 
    phxsc=(phxsc_tag+ispin_diff*phi_spinp) mod 360.d0
    
    ;---phase angle of sun (should be exactly 0 for this reference)
    phsun=(phxsc(k) + phbci_strlsun) mod 360.d0 
    
    i=0 
    if j eq 0 then stop   
  endif else if tjd eq tjd_strlsun_ref1 and recn eq mjf_strlsun_ref1 then begin
     
    ;---phxsc_tag_last=spin phase of time tagged X-axis previous mjf 
    ;---spincnt-spincnt_last=number of apparent spins since last time tag
    ;---phxsc_tag=phase angle of sc x-axis rel to Sun for time tagged spin 
    ;   based on incrementing successive apparent sun pulses
    phxsc_tag=(phxsc_tag_last+(spincnt-spincnt_last)*phi_spinp) mod 360.d0

    ;---phase angle of sc x-axis each spin relative to time tagged spin this mjf
    phxsc=(phxsc_tag+ispin_diff*phi_spinp) mod 360.d0

    ;---spin of strahl sensor Sun measurement    
    k=ispin_strlsun_ref1
    
    ;---strahl sensor measurement of Sun rel to sc x-axis
    phbci_strlsun=(bci_strlsun_ref1*phi_bci+(phistrl_bci0-phisc)) mod 360.d0 
              
    ;---phase angle of sun (should be 0 +/- 4 degrees)
    phsun=(phxsc(k) + phbci_strlsun) mod 360.d0
    
    print,'j ',j
    print,tjd,sec,recn,k,bci_strlsun_ref1,phbci_strlsun,phxsc_tag
    print,'spinp_real,phsun ',spinp_real,phsun
         
    stop,'check value of phsun; it should be 0 +/- 4 degrees
  endif else begin 
    ;---increment phxsc_tag_last (from previous record)
    ;---phi_spinp=(spinp_diff/spinp_real)*360.d0 
    phxsc_tag=(phxsc_tag_last+(spincnt-spincnt_last)*phi_spinp) mod 360.d0
  
    ;---phase angle of sc x-axis each spin relative to time tagged spin this mjf
    phxsc=(phxsc_tag+ispin_diff*phi_spinp) mod 360.d0
  endelse
  
    
  ;---make structure assignments  
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
  spnphx(i).spinp_real=spinp_real
  spnphx(i).tjd_strlsun_ref=tjd_strlsun_ref
  spnphx(i).mjf_strlsun_ref=mjf_strlsun_ref
  spnphx(i).ispin_strlsun_ref=ispin_strlsun_ref
  spnphx(i).bci_strlsun_ref=bci_strlsun_ref
  
  
  if lpr1 then print,recn,tjd,sec, $
    spincnt,spinp_diff,phi_spinp,$
    format='(i8,i6,f11.3,i6,f12.7,f8.1)'
    
  if lpr2 then begin
    for k=0,n_spins-1 do begin
      print,recn,tjd,sec,k,$
      ispin_diff(k),sunsec_vsbl(k),phxsc_tag,phxsc_tag_last,phxsc(k),$
      format='(i8,i6,f11.3,i3,i6,f11.3,3f8.1)'
    endfor
  endif

 
  endrecordloop:
  
  ;---save current values to be incremented next record 
  tjdsec_last=tjdsec
  spincnt_last=spincnt 
  phxsc_tag_last=phxsc_tag
   
endfor     ;end record loop
endlzfile:
close,lundat
endfor       ;end date loop

spnphx=spnphx(0:i)

stop,'manually execute save if desired'
;save,filename=getenv('SWEDATLIB')+nosunpuls_event+'_spnphxdat',spnphx





end



