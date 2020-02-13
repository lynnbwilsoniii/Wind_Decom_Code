;============================= lzinput =================================

;procedure to read level zero input data
 
pro lzinput_special,err=err

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common magstuff,magfile,tpb5,bgse
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common backgrnd,avgcts_m1,avgcts_m2,bdate
common sunglint,xveis_m1,xveis_m2,gdate
common special1,lunar
common shared,d
common wstuff,wst
common swestuff,swest

;print,'pro lzinput'
;help,wa,/str

err=''

;select lz data file(s) 
  if wst.date_file eq 'Date' and keyword_set(wst.indate) eq 0 then $
     wst.date_file='File'
  lzfltr='*.dat'
  fdir=getenv('LZPATH') 
  case wst.date_file of
    'File' : lzfile=pickfile(/read,get_path=fdir,path=fdir(0),filter=lzfltr,$
               title='Open SWE LZ Data Files',/must_exist)

    'Date' : begin
               arg=fdir+'*'+string(wst.indate,format='(i6)')+'*'+lzfltr+'*'
               result=findfile(arg,count=count)
               if count gt 1 then begin
                 for i=0,count-1 do print,i,result(i)
                 print,'multiple lz files: modify lz filter'
                 return
               endif else if result(0) eq '' then begin
                 err='lz file not found'
                 print,err,arg
                 return
               endif
               lzfile=result(0)
             endcase
    endcase
    print,lzfile


;open LZ data file and read header
    if lzfile ne '' then begin
      if keyword_set(lundat) ne 0 then free_lun,lundat
      ;print,'lz data file selected ',lzfile

    ;set up LZ parameters structure
      sp={spinparams,mfrecn:0l,mfyr:0l,mfdy:0l,mfms:0l,$
      spincnt:0b,tjd:0l,sec:0d,mjfcnt:0b,spinp:0d,$
      old_spincnt:0b,old_tjd:0l,old_sec:0d,old_mjfcnt:0b,$
      lst_spinp:0d,lst_tjd:0l,lst_sec:0d,newmjf:1,datayes:0,lst_scimod:-1}

     wst.lzdate=''
     case strlen(lzfile)-strlen(fdir) of
       12: wst.lzdate='19'+strmid(lzfile,strlen(lzfile)-12,6)
       26: wst.lzdate=strmid(lzfile,strlen(lzfile)-16,8)
       else:   ;'lzdate undetermined'
     endcase

     openr,lundat,lzfile,/get_lun
     fh=read_lzhdr(lundat) ;fh=file header structure 

      ;for nrt set
      if fh.rln eq 0 then fh.rln=11552l

      prt_flhdr,0,lzfile,fh  ;print file header 
      
      print,'lz file ',lzfile
      print,'lzdate ',wst.lzdate

   
      ;if wst.lzdate eq '19941227' then begin   ;lunar eclipse sun glint
      ;  restore,getenv('WGGSBASE')+'swe/27dec94/data/lunar_27dec.idlsav'
      ;  help,lunar & help,lunar,/str
      ;endif
      
      ;initialize mode_this_file
      swest.mode_this_file=0   

;get magnetic field data 
     ;first preference: use 3sec mag data if available
     magpath=getenv('MAGPATH')
     arg=magpath+'*'+'mfi_'+string(wst.lzdate,format='(i8)')+'*'+'.cdf'
     result=findfile(arg,count=count)
     magfile='' & wst.magdate=''
     if count ge 1 then begin  ;mag file(s) exists
       magfile=result(count-1) ;assumes highest count is latest version 
       wst.magdate= strmid(magfile,strlen(magfile)-16,8)
       print,'mag 3SEC file ',magfile
       print,'magdate ',wst.magdate 
       ;open mag file       
         openr,lunmfi,magfile,/get_lun   
       ;get mfi 3sec mag field
         loadcdf,magfile,'Time_PB5',tpb5
         loadcdf,magfile,'B3GSE',bgse
         free_lun,lunmfi

     endif else if count eq 0 or wst.magdate ne wst.lzdate then begin
     ;second preference: use mag kp data if available

        magkpdir=getenv('MAGKPPATH')
        magkpfltr='*v*.cdf'
        arg=magkpdir+'*'+string(wst.indate,format='(i6)')+'*'+magkpfltr+'*'
        result=findfile(arg,count=count)
        if count ge 1 and magkpdir ne '' then begin  ;magkp file(s) exists
           ;pick latest version
           version=intarr(count)
           for i=0,count-1 do $             
              version(i)=fix(strmid(result(i),strlen(result(i))-6,2))
           max=max(version,imax)
           result=result(imax)
           magfile=result(0)
           wst.magdate=strmid(magfile,strlen(magkpdir)+12,6)
           print,'mag KP file ',magfile
           print,'magdate ',wst.magdate
           ;open mag file       
             openr,lunmfi,magfile,/get_lun
           ;get magkp data
             loadcdf,magfile,'Time_PB5',t
             loadcdf,magfile,'BGSEc',x
             fill=-1.e31
             wok=where(x(*,0) ne fill,nwok)
             t=t(wok,*)
             x=x(wok,*)
           ;now interpolate kp data into 3 sec data
             tinterp=dblarr(1440,20)
             tinterp(*)=double(1500)+dindgen(1440*20)*3000
             bgse=fltarr(3,1440,20)
             bgse(0,*,*)=interpol(double(x(*,0)),double(t(*,2)),tinterp(*))
             bgse(1,*,*)=interpol(double(x(*,1)),double(t(*,2)),tinterp(*))
             bgse(2,*,*)=interpol(double(x(*,2)),double(t(*,2)),tinterp(*))
           ;transpose to be same dimension as 3sec data : dim = 1440,3,20
             bgse=transpose(bgse,[1,0,2])
           ;form time array centerd on minutes
             tpb5=lonarr(1440,3)
             tpb5(*,0)=replicate(t(0,0),1440)
             tpb5(*,1)=replicate(t(0,1),1440)
             tpb5(*,2)=30l+lindgen(1440)*60

           ;plot for verification
           window,0,xsize = 600,ysize = 600
           !p.multi=[0,0,4,0,0]
           plot,t(*,2),sqrt(x(*,0)^2+x(*,1)^2+x(*,2)^2),ytitle='bmag',title=$
             string(tpb5(0,0),format='(i4)')+string(tpb5(0,1),format='(i4)')
           plot,t(*,2),x(*,0),ytitle='bx'
           oplot,tinterp(*,*),bgse(*,0,*),psym=3,color=225
           plot,t(*,2),x(*,1),ytitle='by'
           oplot,tinterp(*,*),bgse(*,1,*),psym=3,color=225
           plot,t(*,2),x(*,2),ytitle='bz'
           oplot,tinterp(*,*),bgse(*,2,*),psym=3,color=225
           !p.multi=0
          endif
             
     endif else begin
     ;no magdata available
       print,'magdate ' & print,wst.magdate
       print,'lz and mag file dates do not match, or mag file does not exist'
       print,'Do you want to continue? (y/n)'
       print,'(If you continue, -Xgse will be used instead of the mag vector.)'
       answ='' & read,answ & if answ eq 'n' then begin
         return & endif
     endelse 

;get attitude data 
     oapath=getenv('OAPATH')
     arg=oapath+'*'+'at_def_'+string(wst.lzdate,format='(i8)')+'_v*.cdf'
     result=findfile(arg,count=count)
     atfile='' & wst.atdate=''
     if count ge 1 then begin  ;attitude file(s) exists
       atfile=result(count-1) ;assumes highest count is latest version 
       wst.atdate= strmid(atfile,strlen(atfile)-16,8)
       print,'at file ',atfile
       print,'wst.atdate ',wst.atdate 
       ;open at file       
         openr,lunat,atfile,/get_lun 
       ;get attitude 10 min data
         loadcdf,atfile,'Time_PB5',tpb5_at   
         loadcdf,atfile,'GSE_R_ASCENSION',gse_ra
         loadcdf,atfile,'GSE_DECLINATION',gse_dec
       ;verify that attitude array has dimension = 144 (24hrs * 60min / 10min) 
         ;if n_elements(gse_ra) ne 144 then $
         ;stop,'lzinput: attitude array has wrong dimension'
     endif else if count eq 0 or wst.atdate ne wst.lzdate then begin
       print,'wst.atdate ' & print,wst.atdate
       print,'lz and attitude file dates do not match, ',$
             'or attitude file does not exist'
       print,'Do you want to continue? (y/n)'
       print,'(If you continue, the payload to gse coord transformation ',$
             'will be accomplished by a 180 deg rotation about the ',$
             'Xpayload axis instead.)' 
       answ='' & read,answ & if answ eq 'n' then begin
         return & endif
     endif 

;get orbit data     
     oapath=''  ;getenv('OAPATH')
     arg=oapath+'*'+'or_def_'+string(wst.lzdate,format='(i8)')+'_v*.cdf'
     result=findfile(arg,count=count)
     orbfile='' & wst.orbdate=''
     if count ge 1 then begin  ;orbit file(s) exists
       orbfile=result(count-1) ;assumes highest count is latest version 
       wst.orbdate= strmid(orbfile,strlen(orbfile)-16,8)
       print,'orb file ',orbfile
       print,'wst.orbdate ',wst.orbdate 
       ;open orb file       
         openr,lunat,orbfile,/get_lun 
       ;get orbit 10 min data
         loadcdf,orbfile,'Time_PB5',tpb5_orb   
         loadcdf,orbfile,'GSE_POS',gse_pos
         ;loadcdf,orbfile,'GSE_VEL',gse_vel
         gse_vel=0
       ;verify that orbit array has dimension = 144 (24hrs * 60min / 10min) x 3 
         if n_elements(gse_pos) ne 144*3 then $
         stop,'lzinput: orbit array has wrong dimension'
     endif

   
  ;get the spinperiod for the start of the lz file
     start=1
     get_spinp,start=start,lpr=1

  ;process first lz science record in file
        if start gt 1 then recn=start-1 else recn=0 
        ifsci=0
        while ifsci eq 0  do begin
          recn=recn+1
          proc_rec_special,lpr=1,ifsci=ifsci,noglnt=1,err=err
          if err ne '' then ifsci=0
        endwhile
  
  ;determine which science mode
    scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

  ;if mode2 then read 4 mjf's to get full 256 bytes of hv tbl
  ;each record is 64 bytes, ordered in the table with mjf_cnt mod 4= 3 first
     if scimode_ihk eq 2 or scimode_ihk eq 11 then begin
       get_hvtbl_m2
       subtrbkg='No'
     endif

  if scimode_ihk eq 0 or scimode_ihk eq 1 then begin      ;mode 1

  swest.mode_this_file=1

  ;read background counts 
     backgpath=getenv('BACKGPATH')
     if backgpath ne '' then begin     
       bdatesfn=backgpath+'bdates_m1'
       ;find the appropriate background file name
       openr,lunb,bdatesfn,/get_lun
       strng='' & bfn=''
       while not eof(lunb) do begin
         readf,lunb,strng
         backgdate='19'+strmid(strng,0,6)
         if wst.lzdate eq backgdate then $
           bfn=strcompress(strmid(strng,6,strlen(strng)-6),/remove_all)+$
               '_m1_backg.dat'
       endwhile
       free_lun,lunb

       ;mode1 :  
         n_vdets=6 & n_vesteps=16 & n_sectors=6
         avgcts_m1=fltarr(n_vdets,n_vesteps,n_sectors)

       if bfn ne '' then begin  ;read the background file
         bdate='19'+strmid(bfn,0,6)
         openr,lunb,backgpath+bfn
         readu,lunb,avgcts_m1
         free_lun,lunb
         print,'lzdate, bdate, backgroundfile ',wst.lzdate,' ',bdate,$
           ' ',backgpath+bfn
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
       print,'Do you want to continue? (y/n)'
       answ='' & read,answ & if answ eq 'n' then return
     endelse    ;end background input

     ;identify selected  (sun glint) samples with xveis_m1=-1
     combined_gmask=0    ;if eq 1 then use combined glint mask
     ;get glint map date
       glintpath=getenv('GLINTPATH')
       if glintpath ne '' then begin
         gdatesfn=glintpath+'glintdates_m1'
         openr,lung,gdatesfn,/get_lun
         strng='' & gfn=''
         while not eof(lung) do begin
           readf,lung,strng
           gdate='19'+strmid(strng,0,6)
           if wst.lzdate eq gdate then $
           gfn=strcompress(strmid(strng,6,strlen(strng)-6),/remove_all)
         endwhile
         free_lun,lung
         ;mode1  glintmap
         xveis_m1=1+intarr(n_vdets,n_vesteps,n_sectors)
         arg=getenv('WGGSBASE')+'swe/glint/dglint_m1_'+gfn+'.pro'
         result=findfile(arg)
         if result(0) ne '' and combined_gmask eq 0 $
         then call_procedure,'dglint_m1_'+gfn,xveis_m1 else if combined_gmask $
         then call_procedure,'dglint_m1_combined18',xveis_m1
       endif   ;end mode1 glint map input

   endif else if scimode_ihk eq 2 or scimode_ihk eq 11 then begin    ;mode 2  

     swest.mode_this_file=2

     ;read background counts 
     backgpath=getenv('BACKGPATH')
     if backgpath ne '' then begin     
       bdatesfn=backgpath+'bdates_m2'
       ;find the appropriate background file name
       openr,lunb,bdatesfn,/get_lun
       strng='' & bfn=''
       while not eof(lunb) do begin
         readf,lunb,strng
         backgdate='19'+strmid(strng,0,6)
         if wst.lzdate eq backgdate then $
           bfn=strcompress(strmid(strng,6,strlen(strng)-6),/remove_all)+$
               '_m2_backg.dat'
       endwhile
       free_lun,lunb

       ;mode2 :  
         n_vdets=6 & n_vesteps=16 & n_sectors=8
         avgcts_m2=fltarr(n_vdets,n_vesteps,n_sectors)

       if bfn ne '' then begin  ;read the background file
         bdate='19'+strmid(bfn,0,6)
         openr,lunb,backgpath+bfn
         readu,lunb,avgcts_m2
         free_lun,lunb
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
     endelse    ;end background input

     ;identify selected  (sun glint) samples with xveis_m2=-1
     ;get glint map date
       glintpath=getenv('GLINTPATH')
       if glintpath ne '' then begin
         gdatesfn=glintpath+'glintdates_m2'
         openr,lung,gdatesfn,/get_lun
         strng='' & gfn=''
         while not eof(lung) do begin
           readf,lung,strng
           gdate='19'+strmid(strng,0,6)
           if wst.lzdate eq gdate then $
           gfn=strcompress(strmid(strng,6,strlen(strng)-6),/remove_all)
         endwhile

         free_lun,lung         
         ;mode2  glintmap
         n_vdets=6 & n_vesteps=16 & n_sectors=8
         xveis_m2=1+intarr(n_vdets,n_vesteps,n_sectors)
         arg=getenv('WGGSBASE')+'swe/glint/dglint_m2_'+gfn+'.pro'
         result=findfile(arg)
         if result(0) ne '' then call_procedure,'dglint_m2_'+gfn,xveis_m2
      endif   ;end mode2 glint map input

    endif  ;end mode1 & mode2 background and glint
     
 
endif  


if lzfile eq '' then begin
  current_appl=xregistered('wanal')
endif  

print,' '
print,'LZ, orbit-attitude, magnetic field and background data have been read.'
print,'Ready to proceed.'

end
