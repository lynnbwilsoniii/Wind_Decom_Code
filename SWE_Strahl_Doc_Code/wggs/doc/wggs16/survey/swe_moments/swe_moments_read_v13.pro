pro swe_moments_read_v13,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst

start:

print,'moments_read_v13 :'

indat={$
    mfrec:0l,$
    mfspinbl:0l,$
    ta:0.d,$
    iflgs:lonarr(8),$
    gains:fltarr(6),$
    misc:fltarr(20),$
    v:fltarr(16),$
    vpot:0.,$
    fnout:0.,$
    uout:fltarr(3),$
    hout:fltarr(3),$
    trout:0.,$
    ettrt:0.,$
    gyrtrpy:0.,$
    eavg:0.,$
    paxis:fltarr(3),$
    pout:fltarr(3,3),$
    spcpot:0.,$
    b:fltarr(3),$
    bnfit:fltarr(10),$
    timpb5:lonarr(3),$
    misc2:bytarr(28),$
    rgse:fltarr(3)}

    
idatype=where(d.datype eq 'swe_moments')
flnm=d.flnm(idatype)
ver=strmid(flnm,strlen(flnm)-6,2)
savfil_id=strmid(flnm,strlen(flnm)-8,1)

if savfil_id eq 's' then begin ;restore save file

  print,'restoring idl_sav file ',flnm
  restore,flnm
  
endif else begin   ;not a save file

  openr,lunm,flnm,/get_lun
  print,' ' & print,'reading mdat file ',flnm,' ......'
    
  ;read header
  nrec=0l
  prod_date=0l
  if ver ge '09' then begin
    hedr={nrec:0l,thisdate:string('',format='(i8)'),$
            date:string('',format='(a8)'),$
            scimode:0l,oknomag:0l,oknoatt:0l,eclipse:0l,norelgains:0l,$
            ldflag:lonarr(6), n_vdets:0l,n_vesteps:0l,n_sectors:0l,$
            glnt:lonarr(3,64),ensteptbl:fltarr(64),$
            trnc:string('',format='(a8)'),trunc_enstep:0l,max_enstep:0l,$
            sheath:0l,do_scpot:0l,wchglntmask:string('',format='(a8)'),$
            thisrecn:0l,scimodechange:0l,dummy:0l,use_patch:lonarr(6,10,6)}
    readu,lunm,hedr
    nrec=hedr.nrec
  endif else begin
    print,'wrong moments version ',ver
    stop
  endelse
  
  data=replicate(indat,nrec)  
  readu,lunm,data
  free_lun,lunm 
  print,'end reading file ',flnm
  
  ;create save file
  if wst.creatmsvfl and getenv('CREATSVFL') ne '' then begin
    idlsvflnm=getenv('CREATSVFL')+$
      strmid(flnm,strlen(getenv(d.pathenv(idatype))),8)+'_sv'+ver+'.mom'
    save,filename=idlsvflnm,hedr,data
    print,'moments idlsav file created ',idlsvflnm 
    
    ;go back and read the save file
    d.flnm(idatype)=idlsvflnm
    goto,start      
   ;stop
  endif

endelse

nrec=n_elements(data)

;test for for non-advancing msday in the last 3 mjf's or 21 data records
;if non-advancing msday is found, then truncate the array
;( this is a bug in _v13.mom and _sv13.mom files)
dupli_times,data

;test for next day times at end of file
;if found then set mag field to fill (this is abug in _v14.mom files)
fill=-1.e31
nxtdy_times,data,fill

if wst.number_days gt d.maxnumberdays then $
    swe_moments_structavg,ver,indat,data,nrec,wst.number_days
       
prod_date=hedr.thisdate  
print,'prod_date ',prod_date
  


;if multiple days 
if wst.number_days gt 1 then begin
  thisflnm=flnm
  thisdate=wst.indate 
  data1=data
  for inxt=1,wst.number_days-1 do begin
    thispb5=ymd_pb5(long(thisdate))
    thissec=pb5_sec(thispb5)
    pb5_next=sec_pb5(thissec+long(86400))
    date_next=string(long(pb5_ymd(pb5_next)),format='(i8)')
    nullstr=''
    file_next=$
      get_flnm(d.datype(idatype),d.dir(idatype),nullstr,d.fltr(idatype),$
      date_next,err=err)
    if err ne '' then goto,endloop
    
    savfil_id=strmid(file_next,strlen(file_next)-8,1)

    if savfil_id eq 's' then begin ;restore save file
    
      print,'restoring idl_sav file ',file_next
      restore,file_next
      nrec_next=hedr.nrec 

    endif else begin   ;not a save file

      openr,lun_next,file_next,/get_lun
      ;read header
      hedr={nrec:0l,thisdate:string('',format='(i8)'),$
            date:string('',format='(a8)'),$
            scimode:0l,oknomag:0l,oknoatt:0l,eclipse:0l,norelgains:0l,$
            ldflag:lonarr(6), n_vdets:0l,n_vesteps:0l,n_sectors:0l,$
            glnt:lonarr(3,64),ensteptbl:fltarr(64),$
            trnc:string('',format='(a8)'),trunc_enstep:0l,max_enstep:0l,$
            sheath:0l,do_scpot:0l,wchglntmask:string('',format='(a8)'),$
            thisrecn:0l,scimodechange:0l,dummy:0l,use_patch:lonarr(6,10,6)}
      readu,lun_next,hedr
      nrec_next=hedr.nrec
      prod_date=hedr.thisdate
    
      print,'nrec_next ',nrec_next
      data=replicate(indat,nrec_next)
      readu,lun_next,data
      free_lun,lun_next
      print,'end reading next file ',file_next
      
    endelse 
      
      dupli_times,data
      nxtdy_times,data,fill
      
      if wst.number_days gt d.maxnumberdays then $
      swe_moments_structavg,ver,indat,data,nrec_next,wst.number_days
     
      nrec1=n_elements(data1)
      data2=replicate(indat,nrec1+n_elements(data))
      
      data2.mfrec=[data1.mfrec,data.mfrec]
      data2.mfspinbl=[data1.mfspinbl,data.mfspinbl]
      data2.ta=[data1.ta,data.ta]
      data2.iflgs=[[data1.iflgs],[data.iflgs]]
      data2.gains=[[data1.gains],[data.gains]]
      data2.misc=[[data1.misc],[data.misc]]
      data2.v=[[data1.v],[data.v]]
      data2.vpot=[data1.vpot,data.vpot]
      data2.fnout=[data1.fnout,data.fnout]
      data2.uout=[[data1.uout],[data.uout]]
      data2.hout=[[data1.hout],[data.hout]]
      data2.trout=[data1.trout,data.trout]
      data2.ettrt=[data1.ettrt,data.ettrt]
      data2.gyrtrpy=[data1.gyrtrpy,data.gyrtrpy]
      data2.eavg=[data1.eavg,data.eavg]
      data2.paxis=[[data1.paxis],[data.paxis]]
      data2.pout=[[[data1.pout]],[[data.pout]]]
      data2.spcpot=[data1.spcpot,data.spcpot]
      data2.b=[[data1.b],[data.b]]
      data2.bnfit=[[data1.bnfit],[data.bnfit]]
      data2.timpb5=[[data1.timpb5],[data.timpb5]]
      data2.misc2=[[data1.misc2],[data.misc2]]
      data2.rgse=[[data1.rgse],[data.rgse]]
      
      data1=data2
      data2=0
      data=0 
    endloop:
    thisflnm=file_next
    thisdate=date_next
  endfor
  data=data1
endif


d.ndx(0,idatype)=0 
d.ndx(1,idatype)=n_elements(data)-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)


;put input data into structure mdat
mdat=replicate($
  {mfrec:0l,mfspinbl:0l,ta:0.d,tpb5:lonarr(3),$
  fnout:0.,uout:fltarr(3),hout:fltarr(3),trout:0.,ettrt:0.,$
  gyrtrpy:0.,eavg:0.,paxis:fltarr(3),b:fltarr(3),qdotb:fill,$
  umag:0.,theu:0.,phiu:0.,hmag:0.,theh:0.,phih:0.,$
  bmag:fill,theb:fill,phib:fill,magp:0.,bnfit:fltarr(10),$
  spcpot:0.,vpot:0.,$
  iflgs:lonarr(8),gains:fltarr(6),misc:fltarr(20),v:fltarr(16),$
  misc2:bytarr(28),rgse:fltarr(3),pout:fltarr(3,3),$
  prod_date:prod_date},$
  n_elements(data))

;input data
  mdat.mfrec=data.mfrec
  mdat.mfspinbl=data.mfspinbl
  mdat.ta=data.ta
  mdat.fnout=data.fnout
  mdat.uout=data.uout*1e-5
  mdat.hout=data.hout
  mdat.trout=data.trout
  mdat.ettrt=data.ettrt
  mdat.gyrtrpy=data.gyrtrpy
  mdat.eavg=data.eavg
  mdat.paxis=data.paxis
  mdat.b=data.b
  mdat.bnfit=data.bnfit
  mdat.spcpot=data.spcpot
  mdat.vpot=data.vpot
  mdat.iflgs=data.iflgs
  mdat.gains=data.gains
  mdat.misc=data.misc(0:19)
  mdat.v=data.v
  mdat.misc2=data.misc2
  mdat.rgse=data.rgse
  mdat.pout=data.pout
  data=0
    
;computed from input data
  for i=0l,n_elements(mdat)-1 do mdat(i).tpb5=sec_pb5(mdat(i).ta)
  mdat.umag=sqrt(mdat.uout(0)^2+mdat.uout(1)^2+mdat.uout(2)^2)
  mdat.theu=asin(mdat.uout(2)/mdat.umag)/!dtor
  mdat.phiu=atan(mdat.uout(1),mdat.uout(0))/!dtor
     wphlt0=where(mdat.phiu lt 0,nwphlt0)
     if nwphlt0 gt 0 then mdat(wphlt0).phiu=mdat(wphlt0).phiu+360.

  mdat.hmag=sqrt(mdat.hout(0)^2+mdat.hout(1)^2+mdat.hout(2)^2)
  mdat.theh=asin(mdat.hout(2)/mdat.hmag)/!dtor
  mdat.phih=atan(mdat.hout(1),mdat.hout(0))/!dtor
     wphlt0=where(mdat.phih lt 0,nwphlt0)
     if nwphlt0 gt 0 then mdat(wphlt0).phih=mdat(wphlt0).phih+360.

  wbok=where(mdat.b(0) gt fill,nwbok)  
  if wbok(0) ne -1 then begin
    mdat(wbok).bmag=sqrt(mdat(wbok).b(0)^2+mdat(wbok).b(1)^2+mdat(wbok).b(2)^2)
    mdat(wbok).theb=asin(mdat(wbok).b(2)/mdat(wbok).bmag)/!dtor
    mdat(wbok).phib=atan(mdat(wbok).b(1),mdat(wbok).b(0))/!dtor
    wbylt0=where(mdat.b(0) gt fill and mdat.b(1) lt 0,nwbylt0)
    if nwbylt0 gt 0 then mdat(wbylt0).phib=mdat(wbylt0).phib+360.
    mdat(wbok).magp=(mdat(wbok).bmag *1e-5)^2 / (8 * !pi)
  
    mdat(wbok).qdotb= (mdat(wbok).hout(0)*mdat(wbok).b(0)+ $
            mdat(wbok).hout(1)*mdat(wbok).b(1)+ $
            mdat(wbok).hout(2)*mdat(wbok).b(2))/$
            (mdat(wbok).bmag * mdat(wbok).hmag)
  endif
  
  ;get start time for this file 
      nstrt=21
      if n_elements(mdat) lt nstrt then nstrt=n_elements(mdat)
      w=where(long(fix(mdat(0:nstrt-1).ta/86400.d)) - $
              long(fix(mdat(0).ta/86400.d)) ne 0)
      ;reset the reference time and time index to skip up to first nstrt spins 
      ; that may have times (should be less than 7)before midnight on previous day
      
      if w(0) ne -1 then begin 
        k0=w(n_elements(w)-1)
        ws=where(long(fix(mdat(0:nstrt-1).ta/86400.d)) $
                 eq long(fix(mdat(k0).ta/86400.d)))
        k0=ws(0)
        d.ndx(0,idatype)=k0
        d.ndx_orig(0,idatype)=d.ndx(0,idatype)        
      endif else k0=0
       
      tjd0=long(fix(mdat(k0).ta/86400.d)) 
     
 print,' '
 print,'glint mask used (moments) : ',mdat(0).misc(6)
 print,hedr.glnt(where(hedr.glnt ne -1))
 print,' '
 
 d=create_struct(d,'swe_mdat',mdat)
 
 
end
