
;=========================== proc_fpitch =====================================

pro proc_fpitch,wid=wid,pltype=pltype,nopltf=nopltf,err=err

common sharewidglz,wlz
common sharewidg,wa
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common magstuff,magfile,tpb5,bgse
common log_delog,comp_tbl,dcomp_tbl
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common ftemp,fblok,vel
common shared,d
common wstuff,wst
common swestuff,swest
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common fsvshare,nvs,np,strlndx,enrgstp
common sharelevelzero,pltwin_frmt,oplot_sec

err=0

if keyword_set(infile) eq 0 then return
if infile eq '' then return
if keyword_set(wid) eq 0 then wid=1
if keyword_set(swest.subtrbkg) eq 0 then swest.subtrbkg='Yes'
if keyword_set(idlsavf) eq 0 then idlsavf=0

swpmd=['all elecs','all ions ','alt sects','alt spins','bckg test']

md=strarr(257)
md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
md(8)='con46' & md(128)='trans' & md(256)='u'

pbinselect_orig=swest.pbinselect
swest.pbinselect=3.
dp=float(swest.pbinselect)
np=fix(180./dp)

get_recnspin            ;get recn and spin number
                 
if keyword_set(recn) eq 0 then begin
     recn=1 & swest.ispinbl=0
endif


wst.lz_is_read=0

if wst.lz_is_read then goto,lzisread

point1:

wst.lz_is_read=0
 
;process selected lz record
   print,'processinf recn ',recn
   proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,elec_ion_m1=elec_ion_m1,$
    err=err
   print,'err: ',err
   widget_control, wlz.field(14), set_value = swest.subtrbkg

;test tm mode
   if keyword_set(tmmode_ihk) eq 0 then return
   if tmmode_ihk ne 2 then begin
     print,' ' & print,'not in tm science mode'
     return
   endif

;test whether in background test mode
  if vsmjf.background_test then begin
    print,' ' & print, 'BACKGROUND TEST mode'
    return
  endif

wst.lz_is_read=1

lzisread:    ;an lz record has been read

;test current spinbl number against max number spins in current mjf 
   if swest.ispinbl gt vsmjf.n_spins-1 then begin
     swest.ispinbl=0   
     recn=recn+1
     goto,point1     ;read the next record
   endif


widget_control,wlz.field(10),set_value=recn
spinbl=swest.ispinbl & widget_control,wlz.field(11),set_value=spinbl
if keyword_set(swest.nvmax) eq 0 then swest.nvmax=vsmjf.n_vesteps
swest.nvmax = swest.nvmax < vsmjf.n_vesteps
nvmax=swest.nvmax & widget_control,wlz.field(12),set_value=nvmax
if (vsmjf.scimode eq 0 or vsmjf.scimode eq 1) then begin
   if vsmjf.eleion eq 1 then print,'MODE 1 ION MODE!!'
endif
widget_control, wlz.field(17), set_value = vsmjf.scimode
widget_control, wlz.field(18), set_value = swpmd(vsmjf.eleion_sweep)
widget_control, wlz.field(19),set_value=swest.pbinselect

;get tjd and pb5 times
   tjd=long(fix(vsmjf.suntim_vsbl(swest.ispinbl)/86400.d))
   sec=vsmjf.suntim_vsbl(swest.ispinbl) - tjd*86400.d
   ;timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
   timpb5=vsmjf.pb5tim_vsbl(*,swest.ispinbl)
   wst.xydata(0)=sec/3600.d
   hour_hms,wst.xydata(0),hms
   wst.hms=hms(0)
   wst.pb5=timpb5
  
  print,' ' & print,'proc_fpitch: timsel',wst.timsel
  print,'time selected from lz file: '
  print,'lz.recn, swest.ispinbl, vsmjf.suntim_vsbl(swest.ispinbl) (seconds) ',$
      lz.recn,swest.ispinbl,vsmjf.suntim_vsbl(swest.ispinbl)
  print,'tjd, sec of day ',tjd,sec
  print,'pb5 time ',timpb5
  print,ymd(timpb5),'  ',hms

  ;test for data quality
  if vsmjf.vqlty(swest.ispinbl) ne 0 then begin
     print,' '
     print,'quality flag ne 0 for this spinblock',swest.ispinbl,$
        '; data set to 0'
     return
  endif

;get mfi 3sec mag field
    ; timpb5=given pb5 time (year, day of year, millisec of day)  lonarr(3)
    ; tpb5 = pb5 time at center of minute for given record recn   lonarr(1440,3)
    ; bgse = mag vector at 3 sec intervals in minute tpb5     fltarr(1440,3,20)

    ;test whether day of the given time is the same as "lzdate" 
    ;or is just before midnight on previous day
    ;if given time is the same as lzdate, then the correct magfile has already
    ;been read;  
    ;if given time is NOT the same as lzdate, then the appropriate
    ;magfile should be read to get the magnetic vector and then read again the
    ;magfile that agrees with lzdate..
    ;TEMPORARILY, we will not read the appropriate (previous day's) magfile
    ;but will use the first mag vector on the day corresponding to lzdate..
    ;the probable error will be to use mag data corresponding in time to 
    ;one or two spins later than the given time.

    if ymd(timpb5) eq wst.lzdate then begin 
      mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
      sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
    endif else begin
      wmag=where(wst.magdate eq wst.lzdate)
      if wmag(0) ne -1 then begin
        mindx=0 & sindx=0
        print,'given time and magdate do not agree'
        print,'ymd(given time), lzdate, magdate ',$
          ymd(timpb5), wst.lzdate, wst.magdate(wmag)
        print,$
         'first 3sec interval on lzdate will be used: mindx,sindx',mindx,sindx
      endif
    endelse

    if magfile ne '' then begin      
      magfld=bgse(mindx,0:2,sindx)
      magtpb5=[tpb5(mindx,0),tpb5(mindx,1),$
        tpb5(mindx,2) - (30000l - 1500l - long(sindx)*3000)]
      print,' '
      print,'given time ',timpb5
      print,'mindx, sindx ',mindx, sindx
      print,'  mag time ',magtpb5
      print,'mag fld (gse) ',magfld
      if magfld(0) eq -1.e31 then begin
        magfld=[-1.,0.,0.]
        print,'fill mag data, substitute ',magfld
      endif
    endif else begin
      magfld=[-1.,0.,0.]
      print,'no mag data; using direction of Sun instead'
    endelse
   
;get time labels
    mjfdt='mjf '+$
       string(lz.yr,format='(i4)')+'_'+string(lz.dy,format='(i3)')+'_'+$
       string(lz.ms/1000,format='(f9.3)')
    hour_hms,sec/3600.d,hms,lhms=lhms
    spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
    swest.spndt=spndt(0)
    title='SWE ele  '
    widget_control,wlz.field(13),set_value=lhms
   
;get patch fit coeff's if there is a moments file
   npatch=0 &  bnfit=0 & vpot=0. & spcpot=0.
   u=fltarr(3)  ;electron moment bulk flow

   if keyword_set(d) ne 0 then begin
   if d.wdatype(0,0) ne -1 then begin    ;moments data has also been selected   
     if wst.timsel eq 'survey' then mspn=swest.spn
     if wst.timsel eq 'lz' or wst.timsel eq 'lztm' then  $
       mspn=long(indx_begin(mdat.ta,vsmjf.suntim_vsbl(swest.ispinbl) ))
     u=mdat(mspn).uout*1e5
     print,' '
     print,'lchan, n_vesteps_trunc, truncstep, skip_step0, skip_step1, nglint :'
     print,mdat(mspn).iflgs(7),mdat(mspn).misc(0),mdat(mspn).misc(5),$
           mdat(mspn).misc(1),mdat(mspn).misc(2),mdat(mspn).misc(6)
     print,' '
     print,'v : ',mdat(mspn).v
     print,' '
     if total(mdat(mspn).bnfit) ne 0 then begin
       bnfit=mdat(mspn).bnfit
       npatch=4
     endif
     ;w=$
     ;where(volt_en(vsmjf.veistep(0:vsmjf.n_vesteps-1),/vel) gt mdat(mspn).vpot)
     vpot=mdat(mspn).vpot
     ;if vpot ne 0. then nvmin=w(0)


 
     ;============================= NOTICE ==================================

     ;this modification to fparr_patch decouples the potential and the number of
     ;low vel steps that use patch-fitted values, i.e.,
     ;we can fit the lowest step or the lowest two steps 
     ;and still arbitrarily set potential to zero

     ;the modification to fparr_patch is the following:
     ;if vpot=0. then nvmin=0, 1, or 2
     
     nvmin=0
     if vpot eq 0. then begin
       if mdat(mspn).misc(1) eq 1 and mdat(mspn).misc(2) eq 1 then nvmin=2
       if mdat(mspn).misc(1) eq 1 and mdat(mspn).misc(2) eq 0 then nvmin=1
       if mdat(mspn).misc(1) eq 0 and mdat(mspn).misc(2) eq 0 then nvmin=0
       if mdat(mspn).misc(1) eq 0 and mdat(mspn).misc(2) eq 1 then $
         stop,'not allowed to skip second and not first vel step'
     endif

     print,'proc_fpitch: npatch, nvmin, nvmax ',npatch, nvmin, swest.nvmax
     print,'proc_fpitch: vpot, spcpot ',mdat(mspn).vpot,mdat(mspn).spcpot
   endif
   endif 


if swest.subtrbkg eq 'Yes' then fblok=vsmjf.fveis_b(*,*,*,swest.ispinbl)  $
else fblok=vsmjf.fveis(*,*,*,swest.ispinbl)

;-------  treatment of glint points --------------------------
  if swest.delete eq 0 then fblok=abs(fblok)
  widget_control, wlz.field(15), set_value = swest.delete * swest.ndel


;form plotting arrays
;  (for now, we assume that all sectors of a given spin are the same specie
;   and have the same steps......see comment in mode2.pro  )
   if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then $
     wsteps=vsmjf.veistep(*,0,swest.ispinbl)
   if vsmjf.scimode eq 0 or vsmjf.scimode eq 1 then wsteps=vsmjf.veistep
   vel=volt_en(wsteps,/vel,ion=swest.specie_selct)
   en=volt_en(wsteps,/en,ion=swest.specie_selct)
   ;get strahl energy (and velocity)
     ivstep=vsmjf.strlstep(swest.ispinbl) 
     velstrl=volt_en_strl(ivstep,/vel)
     enstrl=volt_en_strl(ivstep,/en)
     fblokstrl=vsmjf.fstrl(*,*,swest.ispinbl)  
   fsv_patch,fblokstrl,velstrl,enstrl,$
      fblok,vel,en,wsteps,vpot,spcpot,magfld,u,timpb5,$
      swest.specie_selct,$
      wid=wid,pltype=pltype,$
      npatch=npatch,bnfit=bnfit,nvmin=nvmin,nvmax=swest.nvmax,err=err

   if err then return         

;grid the data for contouring
  fsv_grid,pltype=pltype

 if keyword_set(nopltf) eq 0 then nopltf=0 else nopltf=1
 if nopltf eq 0 then begin

 if swest.delete eq 0 then $
  labl1='SWE '+swest.specie(swest.specie_selct)+' VEIS+STRAHL '+$
  string(enstrl,format='(i4)')+'ev (Mask Off)' else $
  labl1='SWE '+swest.specie(swest.specie_selct)+' VEIS+STRAHL '+$
  string(enstrl,format='(i4)')+'ev  

   wset,wlz.win(wid) 
   fplot,wlz.win_xsize(wid),wlz.win_ysize(wid),pltype=pltype,$
       ion=swest.specie_selct,$
       labl1=labl1,$
       labl2=spndt
 endif




swest.pbinselect=pbinselect_orig

swest.win_pltype(0:n_elements(pltype)-1,0)=pltype
swest.win_npltypes(0)=n_elements(pltype)
swest.win_delete(0)=swest.delete
swest.win_nvmax(0)=swest.nvmax
swest.win_pbinselect(0)=swest.pbinselect
swest.win_hidepoints(0)=swest.hidepoints


end




;----------------------- strlf_veisf --------------------------------------

pro strlf_veisf,wid=wid

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest
common drawf,pltype

if swest.specie(swest.specie_selct) eq 'ions' or $
  vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then return

proc_fpitch,wid=wid,pltype=pltype, err=err

end
