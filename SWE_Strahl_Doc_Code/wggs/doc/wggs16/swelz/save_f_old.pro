;------------------ fintt --------------------------------------------------
function fintt,vx,vy,vz,bn

arg=bn(0) + bn(1)*vx   + bn(2)*vy   + bn(3)*vz   +$
            bn(4)*vx^2 + bn(5)*vy^2 + bn(6)*vz^2 +$
            bn(7)*vx*vy + bn(8)*vx*vz + bn(9)*vy*vz
return, exp(50.*arg)
end


;-------------------- init_fdstr --------------------------------------------- 
pro init_fdstr

common sharesavef,mbase,button1,button2,button3,field1,field2,field3,field4,$
  field5,field6,draw,win,fdstr,ksector,vel,kdstr,ndstr
  
      ndstr=100
      widget_control,field6,set_value=ndstr
      kdstr=-1
      
      fdstr=replicate($
      {timpb5:lonarr(3),datetime:string('',format='(i15)'),$ 
      fe_patch:fltarr(6,7,6),ve_patch:dblarr(6,7,6,3),vem_patch:dblarr(6,7,6),$
      fe:fltarr(6,16,6),ve:dblarr(6,16,6,3),vem:dblarr(6,16,6),$
      fe_1ct:fltarr(6,16),mode:0,$
      vpot:0.,enpot:0.,dense:0.,tempe:0.,ue:fltarr(3),bf:fltarr(3),$
      bnfit:dblarr(10)},ndstr)

print,'init_fdstr'
      
      save_f_plt,storef=0
      help,fdstr,/str 
      
end


;-------------------------- sav_fdstr ----------------------------------------
pro sav_fdstr

common sharesavef,mbase,button1,button2,button3,field1,field2,field3,field4,$
  field5,field6,draw,win,fdstr,ksector,vel,kdstr,ndstr
  
fdstr=fdstr(0:kdstr)      
      flnm=$
      strcompress(/remove_all,fdstr(0).datetime+'_'+string(n_elements(fdstr)))+$
      '.fdistrib'
      save,filename=getenv('IDLSAV')+flnm,fdstr 
      print,getenv('IDLSAV')+flnm
      help,fdstr 
      
print,'sav_fdstr: fdstr structure saved.''
print,'Hit return to continue' & answ='' & read,answ & if answ ne '' then stop
end

      
                
;------------- increment_spin ---------------------------------------------

pro increment_spin

common drawf,pltype
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common sharewidg,WDGT
;common sharewidglz,wlz
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common sharesavef,mbase,button1,button2,button3,field1,field2,field3,field4,$
  field5,field6,draw,win,fdstr,ksector,vel,kdstr,ndstr
   
                    swest.ispinbl=swest.ispinbl+1
                    if swest.ispinbl gt vsmjf.n_spins-1 then begin
                       swest.ispinbl=0 
                       if recn lt fh.nmf-1 then recn=recn+1
                       wst.lz_is_read=0
                    endif
                    ;widget_control,WDGT.swelz_spinfld,set_value=swest.ispinbl
                    ;widget_control,WDGT.swelz_recfld,set_value=recn
                    widget_control,field1,set_value=swest.ispinbl
                    widget_control,field2,set_value=recn
                    wst.timsel='lz'
                    print,swest.ispinbl,wst.timsel
                                        
                    pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                    proc_fw,wid=pltwin,pltype=pltype
                    
                    pb5=vsmjf.pb5tim_vsbl(*,swest.ispinbl)                    
                    hour_hms,double(pb5(2))/3600000.d,hms,lhms=lhms
                    widget_control,field3,set_value=lhms(0)
                    
                               
end



;--------------------- save_f_plt ---------------------------------------------
pro save_f_plt,pltsector=pltsector,storef=storef


common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common swestuff,swest
common wstuff,wst
common shared,d
;common mpstuff,mflnm,pflnm,swe_mdat,pdat,refsec
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common sharesavef,mbase,button1,button2,button3,field1,field2,field3,field4,$
  field5,field6,draw,win,fdstr,ksector,vel,kdstr,ndstr

if keyword_set(pltsector) eq 0 then pltsector=0 else pltsector=pltsector
ksector=pltsector
if keyword_set(storef) eq 0 then storef=0



if d.wdatype(0,0) eq -1 then begin
  print,'no patch data; moments file has not been read'
  return
endif


hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & ispin=swest.ispinbl
if nsectors ne 6 then stop,'nsectors ne 6'

theveis=vsmjf.theveis
phiveis=vsmjf.phiveis

specie=['elecs','ions']


;get patch fit coeff's and sc potential if there is a moments file
   npatch=0 &  bnfit=0 & vpot=0. & spcpot=0.

     if wst.timsel eq 'survey' then mspn=swest.spn
     if wst.timsel eq 'lz' or wst.timsel eq 'lztm' then  $
       mspn=long(indx_begin(d.swe_mdat.ta,vsmjf.suntim_vsbl(swest.ispinbl) ))
     
     print,' '
     print,'lchan, n_vesteps_trunc, truncstep, skip_step0, skip_step1, nglint :'
     print,d.swe_mdat(mspn).iflgs(7),d.swe_mdat(mspn).misc(0),d.swe_mdat(mspn).misc(5),$
           d.swe_mdat(mspn).misc(1),d.swe_mdat(mspn).misc(2),d.swe_mdat(mspn).misc(6)
     print,' '
     print,'v : ',d.swe_mdat(mspn).v
     print,' '
     if total(d.swe_mdat(mspn).bnfit) ne 0 then begin
       bnfit=d.swe_mdat(mspn).bnfit
       npatch=2
     endif
     vpot=d.swe_mdat(mspn).vpot
     enpot=2.85e-16*vpot*vpot
     ue=d.swe_mdat(mspn).uout
     bf=d.swe_mdat(mspn).b
     dense=d.swe_mdat(mspn).fnout
     tempe=d.swe_mdat(mspn).trout
     
;number of steps included in fit (skipped steps are not excluded here
  nvmin=0     
  lchan=d.swe_mdat(mspn).iflgs(7) - nvmin 
  ;if lchan gt 6 then stop,'lchan gt 6'
  ;if npatch+lchan ne 7 then stop
  
;initialize arrays
  fe_patch=fltarr(vsmjf.n_vdets,npatch+lchan,nsectors)
  ve_patch=dblarr(vsmjf.n_vdets,npatch+lchan,nsectors,3)
  vem_patch=dblarr(vsmjf.n_vdets,npatch+lchan,nsectors)
  fe=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps,nsectors)
  ve=dblarr(vsmjf.n_vdets,vsmjf.n_vesteps,nsectors,3)
  vem=dblarr(vsmjf.n_vdets,vsmjf.n_vesteps,nsectors)
  fe_1ct=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps)
    
for isector=0,nsectors-1 do begin

  ;determine specie, elecs or ions, and voltage steps for given sector = isector
  if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then begin
    specie_selct=vsmjf.eleion(isector,swest.ispinbl) 
    wsteps=vsmjf.veistep(*,isector,swest.ispinbl)
  endif else if $
    vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 then begin 
      specie_selct=vsmjf.eleion
      wsteps=vsmjf.veistep 
  endif else if vsmjf.scimode eq 6 then begin
    specie_selct=vsmjf.eleion(swest.ispinbl)
    wsteps=vsmjf.veistep(*,swest.ispinbl) 
  endif
  
  

  ;sort voltage steps in order of increasing step; form velocity sorted arrays
  sortv=sort(wsteps)
  vel=volt_en(wsteps(sortv),/vel,ion=specie_selct)

  ;phase density of counts minus background (speed sorted)
   phased_b=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps)
   phased_b(*,*)=vsmjf.fveis_b(*,sortv,isector,swest.ispinbl)
   
  ;one-count phase density
   fe_1ct(*,*)=vsmjf.cts_factor(*,sortv)

  ;measured electron velocities corrected for s/c potntial
  ;(NOTE: v_vpot is in order of ascending energy step) 
  v_vpot=sqrt(vel^2 - vpot^2)
  wgepot=where(v_vpot ge 0)
  nvstart=wgepot(0)
     
  ;patch speeds
  vpatch=(findgen(npatch)/npatch)*v_vpot(nvstart)
     
  print,'veis_steps: vpot, spcpot ',d.swe_mdat(mspn).vpot,d.swe_mdat(mspn).spcpot
     
  ngaussfit=npatch + lchan - nvstart
  fegaussfit=fltarr(vsmjf.n_vdets,ngaussfit)
  vgaussfit=fltarr(vsmjf.n_vdets,ngaussfit)
     
  y0=8.25787 & a0=0.238804
  for i=0,ndets-1 do begin
    for j=0,npatch-1 do begin
    
       ;if vpatch(j) eq 0 then begin
       if j eq 0 then begin
         fegaussfit(i,j)=fintt(0.,0.,0.,bnfit)
         fe_patch(i,j,isector)=fegaussfit(i,j)
       endif else begin
         phipatch=phiveis(i,nvmin,isector)+(alog10(vpatch(j))-y0)/a0
         wx=-sin(theveis(i)*!dtor)*cos(phipatch*!dtor)*vpatch(j)
         wy=-sin(theveis(i)*!dtor)*sin(phipatch*!dtor)*vpatch(j)
         wz=-cos(theveis(i)*!dtor)*vpatch(j)
         ;transform from payload to gse
         wc_gse=dblarr(3)
         atindx=fix((vsmjf.pb5tim_vsbl(2,swest.ispinbl))/600000) 
         if atfile ne '' then payload_to_gse,[wx,wy,wz],$
             [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  $
         else begin
           ;(approx transform from payload to gse: SWE spin axis along -zgse)
           wc_gse(0)=wx
           wc_gse(1)=-wy
           wc_gse(2)=-wz 
         endelse
         ;put wx, wy, wz into GSE coords in SPACECRAFT FRAME  
         wx=wc_gse(0)
         wy=wc_gse(1)
         wz=wc_gse(2)
         ;get fitted surface
         fegaussfit(i,j)=fintt(wx*1e-8,wy*1e-8,wz*1e-8,bnfit)
         vgaussfit(i,j)=sqrt(wx^2+wy^2+wz^2)
         fe_patch(i,j,isector)=fegaussfit(i,j)
         ve_patch(i,j,isector,0)=wx
         ve_patch(i,j,isector,1)=wy
         ve_patch(i,j,isector,2)=wz 
         vem_patch(i,j,isector)=vgaussfit(i,j)  
       endelse 
    endfor 
    
    for j=nvstart,lchan-1 do begin
        wx=-sin(theveis(i)*!dtor)*cos(phiveis(i,j,isector)*!dtor)*v_vpot(j)
        wy=-sin(theveis(i)*!dtor)*sin(phiveis(i,j,isector)*!dtor)*v_vpot(j)
        wz=-cos(theveis(i)*!dtor)*v_vpot(j)
        ;transform from payload to gse
        wc_gse=dblarr(3)
        if atfile ne '' then payload_to_gse,[wx,wy,wz],$
          [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  $
        else begin
          ;(approx transform from payload to gse: SWE spin axis along -zgse)
          wc_gse(0)=wx
          wc_gse(1)=-wy
          wc_gse(2)=-wz 
        endelse
        ;put wx, wy, wz into GSE coords in SPACECRAFT FRAME  
        wx=wc_gse(0) 
        wy=wc_gse(1)
        wz=wc_gse(2) 
        ;get fitted surface
        fegaussfit(i,npatch+j-nvstart)=fintt(wx*1e-8,wy*1e-8,wz*1e-8,bnfit)
        vgaussfit(i,npatch+j-nvstart)=sqrt(wx^2+wy^2+wz^2)
        fe_patch(i,npatch+j-nvstart,isector)=fegaussfit(i,npatch+j-nvstart)
        ve_patch(i,npatch+j-nvstart,isector,0)=wx
        ve_patch(i,npatch+j-nvstart,isector,1)=wy
        ve_patch(i,npatch+j-nvstart,isector,2)=wz
        vem_patch(i,npatch+j-nvstart,isector)=vgaussfit(i,npatch+j-nvstart)   
     endfor         
   endfor 


k1=nvstart
k2=n_elements(wsteps)-1 
  
for i=0,ndets-1 do begin
  for j=k1,k2 do begin
     wx=-sin(theveis(i)*!dtor)*cos(phiveis(i,j,isector)*!dtor)*v_vpot(j)
     wy=-sin(theveis(i)*!dtor)*sin(phiveis(i,j,isector)*!dtor)*v_vpot(j)
     wz=-cos(theveis(i)*!dtor)*v_vpot(j)
     ;transform from payload to gse
     wc_gse=dblarr(3)
     if atfile ne '' then payload_to_gse,[wx,wy,wz],$
       [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  $
     else begin
       ;(approx transform from payload to gse: SWE spin axis along -zgse)
       wc_gse(0)=wx
       wc_gse(1)=-wy
       wc_gse(2)=-wz 
     endelse
     ;put wx, wy, wz into GSE coords in SPACECRAFT FRAME  
     wx=wc_gse(0) 
     wy=wc_gse(1)
     wz=wc_gse(2) 
        
     fe(i,j,isector)=phased_b(i,j)
     ve(i,j,isector,0)=wx
     ve(i,j,isector,1)=wy
     ve(i,j,isector,2)=wz 
     vem(i,j,isector)=sqrt(total(ve(i,j,isector,*)^2))      
  endfor
endfor
endfor   ;end sector loop

timpb5=vsmjf.pb5tim_vsbl(*,swest.ispinbl)
  sec=double(timpb5(2)/1000)
  hour_hms,sec/3600.d,hms,lhms=lhms
  datetime=string(pb5_ymd(timpb5),format='(i8)')+'_'+lhms
  
if storef eq 0 then begin  ;plot current distr
  wset,win
  oppdet=[5,4,3,2,1,0]

  stitle=[' ',' ',datetime]
  pos,3,posn,ysep=0.1,xoff=0.1,xtop=0.9
  noerase=[0,1,1]
  charsize=1.15 
   
  for idet=0,2 do begin 
  
  x=[[-reverse(reform(vem(idet,*,pltsector)))],$
        [reform(vem(oppdet(idet),*,pltsector))]] ;speed
 
  f=[[reverse(reform(fe(idet,*,pltsector)))],$
      [reform(fe(oppdet(idet),*,pltsector))]];phase den corrctd 
  xfit=[[-reverse(reform(vem_patch(idet,*,pltsector)))],$
        [reform(vem_patch(oppdet(idet),*,pltsector))]]
  ffit=[[reverse(reform(fe_patch(idet,*,pltsector)))],$
      [reform(fe_patch(oppdet(idet),*,pltsector))]] 
  yrange=[.1,1000]
   
  
  frange=[1.e-31,1.e-24]
  fticks=7
  plot_io,x,f,/nodata,yrange=frange,yticks=fticks,ystyle=1,$
      xrange=[-max(1.05*vel),max(1.05*vel)],xticks=6,xstyle=1,xminor=6,$
      ytitle='phase density',xtitle='speed',$
      title=$
            'det '+string(idet,format='(i1)')+'         '+$
            'sector '+string(pltsector,format='(i1)')+'         '+$
            'det '+string(oppdet(idet),format='(i1)')+'      ', $            
      subtitle=stitle(idet),charsize=charsize,$
      position=posn(*,idet),noerase=noerase(idet)
  oplot,[0.,0.],frange,linestyle=1 
  oplot,x,f,psym=1,symsize=0.75,color=wst.clr_orange
    
  ;plot patch fit obtained from moments analysis
  oplot,xfit,ffit  

  oplot,-reverse(vel),reverse(reform(fe_1ct(idet,*))),linestyle=1,$
      color=wst.clr_orange
  oplot,vel,reform(fe_1ct(oppdet(idet),*)),linestyle=1,$
      color=wst.clr_orange
         
  endfor

  widget_control,field1,set_value=swest.ispinbl
  widget_control,field2,set_value=recn
endif     ;end plot of current distr


if storef then begin  ;store and plot current distr

  ;store current distribution into structure
  
  kdstr=kdstr+1
  widget_control,field5,set_value=kdstr
  if kdstr eq ndstr then stop,'max number of stored distributions exceeded'
  stop
  fdstr(kdstr).timpb5=timpb5
  fdstr(kdstr).datetime=datetime
  fdstr(kdstr).fe_patch=fe_patch
  fdstr(kdstr).ve_patch=ve_patch
  fdstr(kdstr).vem_patch=vem_patch
  fdstr(kdstr).fe=fe
  fdstr(kdstr).ve=ve
  fdstr(kdstr).vem=vem
  fdstr(kdstr).fe_1ct=fe_1ct
  fdstr(kdstr).mode=vsmjf.scimode
  fdstr(kdstr).vpot=vpot
  fdstr(kdstr).enpot=enpot
  fdstr(kdstr).dense=dense
  fdstr(kdstr).tempe=tempe
  fdstr(kdstr).ue=ue
  fdstr(kdstr).bf=bf
  fdstr(kdstr).bnfit=bnfit
  
  print,'stored distr:  ',kdstr,'   ',datetime
  
  ;end storing of distribution
  ;begin plot of stored distribution    

wset,win
oppdet=[5,4,3,2,1,0]

timpb5=vsmjf.pb5tim_vsbl(*,swest.ispinbl)
  sec=double(timpb5(2)/1000)
  hour_hms,sec/3600.d,hms
  datetime=string(pb5_ymd(timpb5),format='(i8)')+'_'+hms
stitle=[' ',' ',datetime]
pos,3,posn,ysep=0.1,xoff=0.1,xtop=0.9
noerase=[0,1,1]
charsize=1.15 
   
for idet=0,2 do begin 
  x=[[-reverse(reform(fdstr(kdstr).vem(idet,*,pltsector)))],$
        [reform(fdstr(kdstr).vem(oppdet(idet),*,pltsector))]] ;speed
 
  f=[[reverse(reform(fdstr(kdstr).fe(idet,*,pltsector)))],$
      [reform(fdstr(kdstr).fe(oppdet(idet),*,pltsector))]];phase den corrctd 
  xfit=[[-reverse(reform(fdstr(kdstr).vem_patch(idet,*,pltsector)))],$
        [reform(fdstr(kdstr).vem_patch(oppdet(idet),*,pltsector))]]
  ffit=[[reverse(reform(fdstr(kdstr).fe_patch(idet,*,pltsector)))],$
      [reform(fdstr(kdstr).fe_patch(oppdet(idet),*,pltsector))]] 
  yrange=[.1,1000]
   
  
  frange=[1.e-31,1.e-24]
  fticks=7
  plot_io,x,f,/nodata,yrange=frange,yticks=fticks,ystyle=1,$
      xrange=[-max(1.05*vel),max(1.05*vel)],xticks=6,xstyle=1,xminor=6,$
      ytitle='phase density',xtitle='speed',$
      title=$
            'det '+string(idet,format='(i1)')+'         '+$
            'sector '+string(pltsector,format='(i1)')+'         '+$
            'det '+string(oppdet(idet),format='(i1)')+'      ', $            
      subtitle=stitle(idet),charsize=charsize,$
      position=posn(*,idet),noerase=noerase(idet)
  oplot,[0.,0.],frange,linestyle=1 
  oplot,x,f,psym=1,symsize=0.75,color=wst.clr_orange
    
  ;plot patch fit obtained from moments analysis
  oplot,xfit,ffit 

  oplot,-reverse(vel),reverse(reform(fdstr(kdstr).fe_1ct(idet,*))),linestyle=1,$
      color=wst.clr_orange
  oplot,vel,reform(fdstr(kdstr).fe_1ct(oppdet(idet),*)),linestyle=1,$
      color=wst.clr_orange
         
endfor

widget_control,field1,set_value=swest.ispinbl
widget_control,field2,set_value=recn

if kdstr+1 eq ndstr then begin   
  sav_fdstr    ;save   
  init_fdstr   ;re-initialize
endif          
endif ;end plot of stored distr 

end


;----------------------- save_f_event ----------------------------------------
pro save_f_event,event
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest
common sharesavef,mbase,button1,button2,button3,field1,field2,field3,field4,$
  field5,field6,draw,win,fdstr,ksector,vel,kdstr,ndstr
common sharewidg,WDGT
;common sharewidglz,wlz

;common mpstuff,mflnm,pflnm,swe_mdat,pdat,refsec
common ionkpstuff,ionkpflnm,ionkpdat

CASE event.id OF

button1: begin
  print,event.value
  case event.value of  
    
    'Initialize' : init_fdstr
          
    'Store and plot current distribution' : save_f_plt,storef=1
    
    'Increment spin and display' : begin
       increment_spin
       save_f_plt,storef=0
    endcase    
  endcase
endcase
         
button2: begin
  print,event.value
  case event.value of           
    'Increment sector in display' : begin
         ksector=ksector + 1 
         if ksector gt 5 then ksector=0
         save_f_plt,pltsector=ksector,storef=0
    endcase
    
    'Create IDLsave file' : sav_fdstr
         
    'Parent' : widget_control,WDGT.swelz_base_main,iconify=0
    
    'Quit' : widget_control,event.top,/destroy
            
  endcase  
endcase

button3: begin
  for i=0,ndstr-1 do begin
    increment_spin
    pb5=vsmjf.pb5tim_vsbl(*,swest.ispinbl)
    widget_control,field4,get_value=hhmmss
    ms=hms_msecday(hhmmss)
    if pb5(2) gt ms then begin
      sav_fdstr
      init_fdstr
      return
    endif
    save_f_plt,storef=1
  endfor  
endcase
        
field4: begin
  ms=hms_msecday(event.value)
  hour_hms,double(ms)/3600000.d,hms,lhms=lhms
  widget_control,field4,set_value=lhms(0)
endcase
        
endcase

end



;----------------------- save_f ----------------------------------------------
pro save_f
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest
common sharesavef,mbase,button1,button2,button3,field1,field2,field3,field4,$
  field5,field6,draw,win,fdstr,ksector,vel,kdstr,ndstr

if xregistered('save_f') then begin
  WIDGET_CONTROL, mbase,iconify=0
  return
                                 endif

if total([swest.ndets,swest.nvsteps,swest.nsectors]) eq 0 then plotcounts   
  
mbase = WIDGET_BASE(TITLE = 'Save SWE VEIS f', /COLUMN)  ;main base

cbase=widget_base(mbase,/column)

rbase=widget_base(cbase,/row)  
label=widget_label(rbase,value='Save f in GSE coordinates:')

rbase=widget_base(cbase,/row)
button1=cw_bgroup(rbase,['Initialize',$
  'Store and plot current distribution',$
  'Increment spin and display'],$
   row=1,/return_name)

field1=cw_field(rbase,title=' ',/long,xsize=3,ysize=1,/row)
field2=cw_field(rbase,title=' ',/long,xsize=4,ysize=1,/row)

rbase=widget_base(cbase,/row)
button3=cw_bgroup(rbase,'Auto increment spin and store',row=1,/return_name)
field3=cw_field(rbase,title='Current',/long,xsize=6,ysize=1,/row,/noedit)  
field4=cw_field(rbase,title='End',/long,xsize=6,ysize=1,/row,/return_events)
field5=cw_field(rbase,title='Curr nmber',/long,xsize=3,ysize=1,/row,/noedit)
field6=cw_field(rbase,title='Max nmber',/long,xsize=3,ysize=1,/row,/noedit)
    
button2=cw_bgroup(cbase,[$
  'Increment sector in display','Create IDLsave file','Parent','Quit'],$
  row=1,/return_name)
    
x_size = 600
y_size = 650
draw = WIDGET_DRAW(cbase,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size, YSIZE = y_size)

WIDGET_CONTROL, mbase, /REALIZE

pb5=vsmjf.pb5tim_vsbl(*,swest.ispinbl)
hour_hms,double(pb5(2))/3600000.d,hms,lhms=lhms
widget_control,field3,set_value=lhms(0)

widget_control,field1,set_value=swest.ispinbl
widget_control,field2,set_value=recn
                    
WIDGET_CONTROL, draw, GET_VALUE=windw
win=windw
wset,win

XMANAGER, "save_f", mbase, GROUP_LEADER = GROUP  ;hand off to manager
  
end