;============================= lzinput =================================

;procedure to read level zero input data
 
pro lzinput,err=err,oknoatt=oknoatt,sheath=sheath,noorb=noorb,oknomag=oknomag,$
  lzm=lzm

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common reldetgains,relgain,relgain_backg
common magstuff,magfile,tpb5,bgse
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common m6stuff,hkm6,vsm6,vdatc6,sdatc6,bxyzdat6
common m7stuff,hkm7,sdatc7
common backgrnd,avgcts_m1,avgcts_m2,bdate
common sunglint,xveis_m1,xveis_m2,gdate
common sunglint_other,xveis_m1_other,gdate_other,xveis_m2_other
common special1,lunar
common shared,d
common wstuff,wst
common swestuff,swest


;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1, mode1 tm map of science and genl hk data offsets into lz.mf
   mode1map
   mode6map
   mode2map
   mode7map

;get mode1 and mode2 sun phase angles of detectors, unit vectors
   phasem1
   phasem2
   
if keyword_set(oknoatt) eq 0 then oknoatt=0
if keyword_set(oknomag) eq 0 then oknomag=0
if keyword_set(noorb) eq 0 then noorb=0
if keyword_set(displaytool) eq 0 then displaytool=0

err=''
nullstr=''

;select lz data file(s) 
  if wst.date_file eq 'Date' and keyword_set(wst.indate) eq 0 then $
     wst.date_file='File'
  fdir=getenv('SWE_LZPATH') 
  
  case wst.date_file of
  'File' : lzfile=pickfile(/read,get_path=fdir,path=fdir(0),filter=lzfltr,$
               title='Open SWE LZ Data Files',/must_exist)
  'Date' : begin              
      lzfile=get_flnm('swelz',fdir,nullstr,'*.dat',wst.lzindate,err=err)
      if lzfile eq '' then err='lz file not found'
      print,err
           endcase
  endcase
  print,'lzfile ',lzfile
    

;open LZ data file and read header
  if keyword_set(lundat) ne 0 then close,lundat    ;free_lun,lundat
  ;print,'lz data file selected ',lzfile

;set up LZ parameters structure
  sp={spinparams,mfrecn:0l,mfyr:0l,mfdy:0l,mfms:0l,$
    spincnt:0b,tjd:0l,sec:0d,mjfcnt:0b,spinp:0d,$
    old_spincnt:0b,old_tjd:0l,old_sec:0d,old_mjfcnt:0b,$
    lst_spinp:0d,lst_tjd:0l,lst_sec:0d,newmjf:1,datayes:0,lst_scimod:-1}

  wst.lzdate=''
  char0=strmid(lzfile,strlen(fdir),1)
  case char0 of 
  'W' : wst.lzdate=strmid(lzfile,strlen(lzfile)-16,8)
  'w' : wst.lzdate=strmid(lzfile,strlen(lzfile)-16,8)
  '9' : wst.lzdate='19'+strmid(lzfile,strlen(lzfile)-12,6)
  '0' : wst.lzdate='20'+strmid(lzfile,strlen(lzfile)-12,6)
  else:
  endcase




;-------nosun pulse spinp
  appsunpuls=wst.lzdate eq '20000715' or wst.lzdate eq '20000716'
  if appsunpuls then sp.lst_spinp=2.9921772d0 
;-------nosun pulse spinp
  
;--------------------- define rel gains and swest.patch_include --------------  
;get relgains for this date
   print,'realtive gains:'
   

rgflnm=getrelgains_tbl_flnm()    

if rgflnm ne '' then begin    ;gains table exists
  print,'gains table file :',rgflnm
  restore,getenv('WGGSBASE')+rgflnm              
  help,datetbl,rgtbl
  wdate=where(long(wst.lzdate) eq datetbl,nwdate)
  if nwdate ne 1 then stop,'bad gains table'
  relgain=rgtbl(*,wdate(0))
  ;SPECIAL
  if wst.lzdate eq '20001110' then $
    relgain=[0.826,  1.676, 0.000,  2.508,  1.000,  0.660]
  
  print,' ' & print,'relgain ',relgain
endif else begin             ;gains table does not exist
  print,'gains table file : none'
  relgain=[1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
  print,' ' & print,'relgain ',relgain
endelse
    
;In cases where the detector response was extremely low (say a factor 5 or more 
;  compared with the average) the rel gain table entry has been set to zero
;  for that paricular detector(s), for the purpose of identification.
;  In this case, the actual relgain used will be set to unity but 
;  swest.patch_include will be set to zero so that the patchfit will not
;  use these data, but instead patched values will be used for the
;  lowest lchan energy steps in the core, i.e., first lchan steps above scpot.

;initialize
swest.patch_include(*,*,*)=1

swest.lchan=9l ;8l;6l 
swest.lchan_old=swest.lchan
   
wgeq0=where(relgain eq 0)
if wgeq0(0) ne -1 then begin
   relgain(wgeq0)=1.0
   swest.patch_include(wgeq0,*,*)=0   ;exclude from patch
endif
      
;lowest two en steps are also excluded from patch for all detectors 
   swest.patch_include(*,0:1,*)=0 

;------------Four of six VEIS detectors cease to function
;Beginning Nov 20, 2001 (20011120), det's 3,4,5 have zero counts 
;  due to apparent side 2 bias supply failure
;After Nov 20, 2001, det's 0 and 1 appeared to be stable, whereas det 2 response
;  rapidly diminished and was effectively zero by Dec 1, 2001, 
;  even when density was greater than 10

;setting swest.patch_include to 0 for detectors with zero response
if long(wst.lzdate) ge 20011119l and long(wst.lzdate) le 20011130l then begin
  swest.patch_include(3:5,*,*)=0
  swest.patch_include(3,*,*)=0      
  ;---TEST: det's 5,4 will be mirror of det's 0,1
  ;swest.patch_include(5,*,*)=swest.patch_include(0,*,*)
  ;swest.patch_include(4,*,*)=swest.patch_include(1,*,*)
endif else if long(wst.lzdate) gt 20011130l then begin
  swest.patch_include(3:5,*,*)=0
  swest.patch_include(3,*,*)=0  
  ;---TEST:det's 5,4 will be mirror of det's 0,1
  ;swest.patch_include(5,*,*)=swest.patch_include(0,*,*)
  ;swest.patch_include(4,*,*)=swest.patch_include(1,*,*)
  ;swest.patch_include(2,*,*)=0
endif  
         
swest.relgain=relgain
print,relgain

;--------------------- rel gains and swest.patch_include have been defined ----


  lundat=long(getenv('LOGICAL_UNITLZ'))
  openr,lundat,lzfile     ;,/get_lun
  fh=read_lzhdr(lundat) ;fh=file header structure 

;for nrt set
  if fh.rln eq 0 then fh.rln=11552l
   prt_flhdr,0,lzfile,fh  ;print file header 
   print,'lz file ',lzfile
   print,'lzdate ',wst.lzdate


;lunar eclipse sun glint   
  if wst.lzdate eq '19941227' then begin   
    ;restore,getenv('WGGSBASE')+'special/27dec94/data/lunar_27dec.idlsav'
     restore,$
        getenv('WGGSBASE')+'misc/special/27dec94/data/newlunar_27dec94.idlsav'
     help,lunar & help,lunar,/str
  endif else if wst.lzdate eq '19961113' then begin
    restore,$
        getenv('WGGSBASE')+'misc/special/13nov96/data/newlunar_13nov96.idlsav'
    help,lunar & help,lunar,/str
  endif

;initialize mode_this_file
  swest.mode_this_file=0   

;----------------------- get magnetic field data -----------------------------
;swest.mag_3s_kp=0, 1, or 2 : 0=no mag data, 1=3smag used,  2=magkp used

;initialize 
   swest.mag_3s_kp=0

   ;first preference: use 3sec mag data if available
   magpath=getenv('MFI_MAGPATH')
   magfile=get_flnm('mag3s',magpath,nullstr,'.cdf',wst.lzdate,err=err) 
   if keyword_set(lzm) ne 0 and err ne '' then return
     
  if magfile ne '' then begin     ;mag file exists  
    wst.magdate= strmid(magfile,strlen(magfile)-16,8)
    if wst.magdate ne wst.lzdate then stop       ;checking dates
    print,'mag 3SEC file ',magfile
    print,'magdate ',wst.magdate 
    ;open mag file 
       lunmfi=1      
       openr,lunmfi,magfile;,/get_lun   
    ;get mfi 3sec mag field
       loadcdf,magfile,'Time_PB5',tpb5     ;tpb5=lonarr(1440,3)
       loadcdf,magfile,'B3GSE',bgse        ;bgse=fltarr(28800,3)
       bgse=transpose(reform(bgse,20,1440,3),[1,2,0])  ;bgse=fltarr(1440,3,20) 
       close,lunmfi   
       swest.mag_3s_kp=1
       
  endif else if magfile eq '' then begin;no 3smag, use magkp
    print,'3sec mag data unavailable; using magkp data instead'
    idatype=where(d.datype eq 'mfi_magkp')    
    if d.flnm(idatype) ne '' then swest.mag_3s_kp=2 else begin
      print,'magkp data not read in or unavailable'
      print,'anti-solar direction will be used for the parallel direction'
      swest.mag_3s_kp=0 
    endelse               
  endif 
  
  if swest.mag_3s_kp eq 0 then begin  ;no 3smag of magkp data readin
    print,'no magdata available' 
    print,'magdate ' & print,wst.magdate
    print,'lz and mag file dates do not match, or mag file does not exist'
    if getenv('LZNOMAG') eq '' then begin
      print,'Hit return if you want to continue.'
      print,'(If you continue, -Xgse to be used instead of the mag vector.)'
      answ='' & read,answ & if answ ne '' then stop 
    endif else  $
        print,'(-Xgse to be used as principle axis in place of mag vector.)'
    swest.mag_3s_kp=0
  endif

;----------------- end get mag data ------------------------------------------  

;------------------ get attitude data ----------------------------------------
    oapath=getenv('WIND_OAPATH')
    arg=oapath+'*'+'at_def_'+string(wst.lzdate,format='(i8)')+'*'+'.cdf'
    result=findfile(arg,count=count)
    atfile='' & wst.atdate=''
    if count eq 0 then begin
      arg=oapath+'*'+'at_pre_'+string(wst.lzdate,format='(i8)')+'*'+'.cdf'
      result=findfile(arg,count=count)
    endif
    if count ne 0 then begin
      atfile=result(count-1) ;assumes highest count is latest version 
      wst.atdate= strmid(atfile,strlen(atfile)-16,8)
      print,'at file ',atfile
      print,'wst.atdate ',wst.atdate 
      ;open at file 
        lunat=1      
        openr,lunat,atfile;,/get_lun 
      ;get attitude 10 min data
        loadcdf,atfile,'Time_PB5',tpb5_at   
        loadcdf,atfile,'GSE_R_ASCENSION',gse_ra
        loadcdf,atfile,'GSE_DECLINATION',gse_dec
        close,lunat
      ;verify that attitude array has dimension = 144 (24hrs * 60min / 10min) 
         ;if n_elements(gse_ra) ne 144 then $
         ;stop,'lzinput: attitude array has wrong dimension'
    endif else if $
      (count eq 0 or wst.atdate ne wst.lzdate) and oknoatt eq 0 then begin
      print,'wst.atdate ' & print,wst.atdate
      print,'lz and attitude file dates do not match, ',$
             'or attitude file does not exist'
      print,'Do you want to continue? (y/n)'
      print,'(If you continue, the payload to gse coord transformation ',$
             'will be accomplished by a 180 deg rotation about the ',$
             'Xpayload axis instead.)' 
      answ='' & read,answ & if answ eq 'n' then begin & return & endif
    endif 

;------------------ end get attitude data ------------------------------------
if noorb then goto,skiporbit

;---------------------- get orbit data --------------------------------------- 
   oapath=getenv('WIND_OAPATH')  
   arg=oapath+'*'+'or_def_'+string(wst.lzdate,format='(i8)')+'*'+'.cdf'
   result=findfile(arg,count=count)
   orbfile='' & wst.orbdate=''
   if count eq 0 then begin
     arg=oapath+'*'+'or_pre_'+string(wst.lzdate,format='(i8)')+'*'+'.cdf'
     result=findfile(arg,count=count)
   endif
   if count ne 0 then begin
     orbfile=result(count-1) ;assumes highest count is latest version 
     wst.orbdate= strmid(orbfile,strlen(orbfile)-16,8)
     print,'orb file  ',orbfile
     print,'wst.orbdate ',wst.orbdate 
     ;open orb file 
     lunat=1      
     openr,lunat,orbfile;,/get_lun 
     ;get orbit 10 min data
     loadcdf,orbfile,'Time_PB5',tpb5_orb   
     loadcdf,orbfile,'GSE_POS',gse_pos
     ;loadcdf,orbfile,'GSE_VEL',gse_vel
     close,lunat
     gse_vel=0
     ;verify that orbit array has dimension = 144 (24hrs * 60min / 10min) x 3 
     if n_elements(gse_pos) ne 144*3 then $
     print,'lzinput: orbit array time dimension not = 144'
     ;stop,'lzinput: orbit array has wrong dimension'
   endif
   
;---------------------- end get orbit data -----------------------------------
skiporbit:

;get ionkp data and use ion bulk velocity to put electron distributions in
;  solar wind frame IF useionkp=1
get_ionkp_lz,err=err

;get the spinperiod for the start of the lz file
   start=1
   get_spinp,start=start,lpr=1

;process first lz science record in file
   if start gt 1 then recn=start-1 else recn=0 
;!
if (total(wst.lzdate eq ['20020721','20020801',$ ;           Mixed-mode days.
                         '20020802','20020815']) ge 1) then begin
   ifsci = 0 & in_mode7 = 0 ;        Search for first mode7 science record...
   while ((in_mode7 ne 7) or (ifsci eq 0)) do begin
      recn=recn+1
      proc_rec,lpr=1,ifsci=ifsci,noglnt=1,err=err,in_mode7=in_mode7
      if err ne '' then ifsci=0
   endwhile
endif else begin
   ifsci = 0 & in_mode7 = 0 ;              Search for first science record...
   while (ifsci eq 0) do begin
      recn=recn+1
      proc_rec,lpr=1,ifsci=ifsci,noglnt=1,err=err,in_mode7=in_mode7
      if err ne '' then ifsci=0
   endwhile
endelse
;!

;determine which science mode
   scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  ;if mode2 then read 4 mjf's to get full 256 bytes of hv tbl
  ;each record is 64 bytes, ordered in the table with mjf_cnt mod 4= 3 first
     if scimode_ihk eq 2 or scimode_ihk eq 11 then begin
       get_hvtbl_m2
       subtrbkg='No'
     endif

;mode1 and mode4 and mode6
if scimode_ihk eq 0 or scimode_ihk eq 1 or scimode_ihk eq 4 or $
scimode_ihk eq 6 then begin
  if scimode_ihk eq 0 or scimode_ihk eq 1 or scimode_ihk eq 4 then $
    swest.mode_this_file=1
  if scimode_ihk eq 6 then swest.mode_this_file=6

  ;------------------------- read mode1 background counts ---------------------
  
  ;backgpath=getenv('BACKGPATH') 
  ;if wst.lzdate lt '19981026' then backgpath=getenv('BACKGPATH') $
  if wst.lzdate lt '19971026' then backgpath=getenv('BACKGPATH') $
  else backgpath=''
  bdate='00000000'
  if backgpath ne '' then begin     
     bdatesfn=backgpath+'bdates_m1'
     ;find the appropriate background file name
     lunb=1
     openr,lunb,bdatesfn;,/get_lun
     strng='' & bfn=''
     while not eof(lunb) do begin
       readf,lunb,strng
       backgdate=strmid(strng,0,8)
       if wst.lzdate eq backgdate then $
       bfn=strmid(strng,10,strlen(strng)-10)+'_m1_backg.dat'
     endwhile
     close,lunb

     ;mode1 :  
     n_vdets=6 & n_vesteps=16 & n_sectors=6
     avgcts_m1=fltarr(n_vdets,n_vesteps,n_sectors)

     if bfn ne '' then begin  ;read the background file
       bdate=strmid(bfn,0,8)
       lunb=1
       openr,lunb,backgpath+bfn;,/get_lun
       readu,lunb,avgcts_m1
       close,lunb
       print,'lzdate, bdate, backgroundfile ',wst.lzdate,' ',bdate,$
           ' ',backgpath+bfn
         
       ;get relgains for this background file date
         wbdate=where(long(bdate) eq datetbl,nwdate)
         if nwdate ne 1 then stop,'bad gains table'
         relgain_backg=rgtbl(*,wbdate(0))
      
     endif else begin           ;no background file exists
       swest.subtrbkg='No'
       print,$
          'No background file has been read. Background has been set to 0.'
     endelse
   endif else begin
     n_vdets=6 & n_vesteps=16 & n_sectors=6
     avgcts_m1=fltarr(n_vdets,n_vesteps,n_sectors)
     swest.subtrbkg='No'
     print,' '
     print,'No background file has been read. Background has been set to 0.'
     ;print,'Do you want to continue? (y/n)'
     ;  answ='' & read,answ & if answ eq 'n' then return
   endelse   
   
   ;------------------- end mode1 read background counts ----------------------
   
   ;------ identify selected  mode1 (sun glint) samples with xveis_m1=-1 -----
   
   ;get glint map date
   glintpath=getenv('GLINTPATH')
   gdate=''
   if glintpath ne '' then begin
     gdatesfn=glintpath+'glintdates_m1'
     lung=1
     openr,lung,gdatesfn;,/get_lun
     strng='' & gfn=''
     while not eof(lung) do begin
        readf,lung,strng
        if wst.lzdate eq strmid(strng,0,8) then begin
          gfn=strmid(strng,strlen(strng)-8,8)
          gdate=gfn    ;strmid(strng,0,8)
        endif  
     endwhile
     close,lung
     
     ;mode1  glintmap
     xveis_m1=1+intarr(n_vdets,n_vesteps,n_sectors)
     arg=getenv('WGGSBASE')+'swelz/swecal/glint/dglint_m1_'+gfn+'.pro'
     result=findfile(arg)
     
     if result(0) ne '' and keyword_set(sheath) eq 0 $
     then call_procedure,'dglint_m1_'+gfn,xveis_m1 else $ 
     if result(0) ne '' and keyword_set(sheath) ne 0 $
     then call_procedure,'dglint_m1_'+gfn,xveis_m1,/sheath 
   endif
   ;universal mode1 glint mask 
   call_procedure,'dglint_m1_universal',xveis_m1_other
   gdate_other='universal'
   ;swest.univgmask is used in mode1 to switch glintmasks 
   
   ;--------------- end mode1 glint map input --------------------------------

endif else if scimode_ihk eq 2 or scimode_ihk eq 11 then begin ;mode 2  

   swest.mode_this_file=2

   ;----------------------- read mode2 background counts ---------------------
   backgpath=getenv('BACKGPATH')
   if backgpath ne '' then begin     
     bdatesfn=backgpath+'bdates_m2'
     ;find the appropriate background file name
     lunb=1
     openr,lunb,bdatesfn;,/get_lun
     strng='' & bfn=''
     while not eof(lunb) do begin
        readf,lunb,strng
        backgdate=strmid(strng,0,8)
        if wst.lzdate eq backgdate then $
         bfn=strmid(strng,strlen(strng)-8,8)+'_m2_backg.dat'
     endwhile
     close,lunb

     n_vdets=6 & n_vesteps=16 & n_sectors=8
     avgcts_m2=fltarr(n_vdets,n_vesteps,n_sectors)

     if bfn ne '' then begin  ;read the background file
       bdate=strmid(bfn,0,8)
       lunb=1
       openr,lunb,backgpath+bfn;,/get_lun
       readu,lunb,avgcts_m2
       close,lunb
       print,'lzdate, bdate, backgroundfile ',wst.lzdate,' ',bdate,$
           ' ',backgpath+bfn 
              
     endif else begin           ;no background file exists
       swest.subtrbkg='No'
       print,$
            'No background file has been read. Background has been set to 0.'
     endelse
   endif else begin
     n_vdets=6 & n_vesteps=16 & n_sectors=8
     avgcts_m2=fltarr(n_vdets,n_vesteps,n_sectors)
     swest.subtrbkg='No'
     print,' '
     print,'No background file has been read. Background has been set to 0.'
     print,'Do you want to continue? (y/n)'
     answ='' & read,answ & if answ eq 'n' then return
   endelse    

   ;-------------------- end mode2 background input ---------------------------

   ;-------identify selected  mode2 (sun glint) samples with xveis_m2=-1 -----
   
   do_m2glintbydate=0
   if do_m2glintbydate then begin
     ;get glint map date
     glintpath=getenv('GLINTPATH')
     if glintpath ne '' then begin
       gdatesfn=glintpath+'glintdates_m2'
       lung=1
       openr,lung,gdatesfn;,/get_lun
       strng='' & gfn=''
       while not eof(lung) do begin
         readf,lung,strng
         if wst.lzdate eq strmid(strng,0,8) then begin
           gfn=strmid(strng,strlen(strng)-8,8)
           gdate=gfn   ;strmid(strng,0,8)
         endif  
       endwhile
       close,lung
       ;mode2  glintmap
       n_vdets=6 & n_vesteps=16 & n_sectors=8
       xveis_m2=1+intarr(n_vdets,n_vesteps,n_sectors)
       arg=getenv('WGGSBASE')+'swelz/swecal/glint/dglint_m2_'+gfn+'.pro'
       result=findfile(arg)
       if result(0) ne '' then call_procedure,'dglint_m2_'+gfn,xveis_m2
     
       ;universal mode2 glint mask 
       call_procedure,'dglint_m2_universal',xveis_m2_other
       gdate_other='universal'   
     endif
   endif else begin
     gdate_other='universal'
     gdate='universal'   
     dglint_m2,xveis_m2;mode2 glint map obtained from mode6 glint phase angles
     xveis_m2_other=xveis_m2
  endelse 
   
   ;-------------------- end mode2 glint map input -------------------------   
endif  
;---------------- end mode1 & mode2 background and glint -------------------    
 
  
print,' '
print,'LZ, orbit-attitude, magnetic field and background data have been read.'
print,'Ready to proceed.'

;---new strahl mode flag: 
;------if swest.veis_on=0 then no veis data, but new strahl mode 
;------if swest.veis_on=1, then yes veis data, and old strahl mode 
if (in_mode7 eq 7) then swest.veis_on=0 else swest.veis_on=1
print,'swest.veis_on = ',swest.veis_on 
         
end
