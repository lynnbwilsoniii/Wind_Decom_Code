;------------------ fintt --------------------------------------------------
function fintt,vx,vy,vz,bn

arg=bn(0) + bn(1)*vx   + bn(2)*vy   + bn(3)*vz   +$
            bn(4)*vx^2 + bn(5)*vy^2 + bn(6)*vz^2 +$
            bn(7)*vx*vy + bn(8)*vx*vz + bn(9)*vy*vz
return, exp(50.*arg)
end



pro veis_steps_patch

;plots veis counts/f data

;common sharewidg,wa
common sharewidgc,wc
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common special1,lunar
common swestuff,swest
common wstuff,wst
common shared,d
;common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common oastuff,atfile,tpb5_at,gse_ra,gse_dec

if d.wdatype(0,0) eq -1 then begin
  print,'no patch data; moments file has not been read'
  return
endif

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete & ndel=swest.ndel & minmax=swest.minmax 

theveis=vsmjf.theveis
phiveis=vsmjf.phiveis

wst.hardcopy=0

print,'veis_steps_m2: isector ',isector

yn=['no','yes']
clr=[225,wst.clr_green]
oppdet=[5,4,3,2,1,0]
specie=['elecs','ions']

start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm
endif

;help,vsmjf.cveis
;!p.multi=[0,0,3,0,0]
pos,3,posn,ysep=0.1,xoff=0.1,xtop=0.9
noerase=[0,1,1]
charsize=1.15  ;2.25
   

timpb5=vsmjf.pb5tim_vsbl(*,ispin)
sec=double(timpb5(2)/1000)
hour_hms,sec/3600.d,hms
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms

;determine specie, elecs or ions, and voltage steps for given sector = isector
if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then begin
  specie_selct=vsmjf.eleion(isector,ispin) 
  wsteps=vsmjf.veistep(*,isector,ispin)
endif else if $
  vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 then begin 
  specie_selct=vsmjf.eleion
  wsteps=vsmjf.veistep 
endif else if vsmjf.scimode eq 6 then begin
  specie_selct=vsmjf.eleion(ispin)
  wsteps=vsmjf.veistep(*,ispin) 
endif
  

stitle=[' ',' ','rel gains  '+string(vsmjf.relgain,format='(6f5.1)')]
xtitle=[' ',' ',$
'mode'+string(vsmjf.scimode,format='(i2)')$
+'  '+specie(specie_selct)+'  '+spndt+'  '+' ']

ebias1=lz.mf(ihk(18).offs)
ebias2=lz.mf(ihk(28).offs)

;sort voltage steps in order of increasing step and form velocity sorted arrays
  sortv=sort(wsteps)
  vel=volt_en(wsteps(sortv),/vel,ion=specie_selct)
  counts=float(abs(vsmjf.veis(*,sortv,isector,ispin))) > 1
  counts_b=abs(vsmjf.veis_b(*,sortv,isector,ispin)) > 1  ;counts - background
  ;counts_b=abs(vsmjf.cveis_b(*,sortv,isector,ispin)) > 1  ;counts - background

;phase density of counts minus background (speed sorted)
;convert to f's 
   phased_b=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps)
   phased_b(*,*)=vsmjf.fveis_b(*,sortv,isector,ispin)
   
;one-count phase density
   phased_1ct=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps)
   phased_1ct(*,*)=vsmjf.cts_factor(*,sortv)

;get patch fit coeff's if there is a moments file
   npatch=0 &  bnfit=0 & vpot=0. & spcpot=0.

   idatype=where(d.datype eq 'swe_moments')
   if d.wdatype(0,idatype) ne -1 then begin  ;moments data has been selected   
     if wst.timsel eq 'survey' then mspn=swest.spn
     if wst.timsel eq 'lz' or wst.timsel eq 'lztm' then  $
       mspn=long(indx_begin(d.swe_mdat.ta,vsmjf.suntim_vsbl(swest.ispinbl) ))
     
     print,' '
     print,'lchan, n_vesteps_trunc, truncstep, skip_step0, skip_step1, nglint :'
     print,d.swe_mdat(mspn).iflgs(7),d.swe_mdat(mspn).misc(0),$
           d.swe_mdat(mspn).misc(5),d.swe_mdat(mspn).misc(1),$
           d.swe_mdat(mspn).misc(2),d.swe_mdat(mspn).misc(6)
     print,' '
     print,'v : ',d.swe_mdat(mspn).v
     print,' '
     if total(d.swe_mdat(mspn).bnfit) ne 0 then begin
       bnfit=d.swe_mdat(mspn).bnfit
       npatch=2
     endif
     vpot=d.swe_mdat(mspn).vpot
     enpot=2.85e-16*vpot*vpot
     

     ;measured electron velocities corrected for s/c potntial
     ;(NOTE: v_vpot is in order of ascending energy step) 
     v_vpot=sqrt(vel^2 - vpot^2)
     wgepot=where(v_vpot ge 0)
     nvstart=wgepot(0)
     
     ;patch speeds
     vpatch=(findgen(npatch)/npatch)*v_vpot(nvstart)

     ;number of steps included in fit (skipped steps are not excluded here
     nvmin=0     
     lchan=d.swe_mdat(mspn).iflgs(7) - nvmin 
     
     print,'veis_steps: vpot, spcpot ',d.swe_mdat(mspn).vpot,$
       d.swe_mdat(mspn).spcpot
     
     ngaussfit=npatch + lchan - nvstart
     fegaussfit=fltarr(vsmjf.n_vdets,ngaussfit)
     vgaussfit=fltarr(vsmjf.n_vdets,ngaussfit)
     
     y0=8.25787 & a0=0.238804
     for i=0,ndets-1 do begin
       for j=0,npatch-1 do begin
         if vpatch(j) eq 0 then fegaussfit(i,j)=fintt(0.,0.,0.,bnfit) $
         else begin
           phipatch=phiveis(i,nvmin,isector)+(alog10(vpatch(j))-y0)/a0
           wx=-sin(theveis(i)*!dtor)*cos(phipatch*!dtor)*vpatch(j)
           wy=-sin(theveis(i)*!dtor)*sin(phipatch*!dtor)*vpatch(j)
           wz=-cos(theveis(i)*!dtor)*vpatch(j)
           ;transform from payload to gse
           wc_gse=dblarr(3)
           atindx=fix((vsmjf.pb5tim_vsbl(2,ispin))/600000) 
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
       endfor         
     endfor 
   endif 

k1=nvstart
k2=n_elements(wsteps)-1 
  
for idet=0,2 do begin
  x=[-reverse(v_vpot(k1:k2)),v_vpot(k1:k2)]                       ;speed
  y=float([[reverse(reform(counts(idet,k1:k2)))],$
           [reform(counts(oppdet(idet),k1:k2))]]) ;measured counts
  z=[[reverse(reform(counts_b(idet,k1:k2)))],$
     [reform(counts_b(oppdet(idet),k1:k2))]] ;measured counts * relgain - bckg
  f=[[reverse(reform(phased_b(idet,k1:k2)))],$
      [reform(phased_b(oppdet(idet),k1:k2))]] ;phase densty of corrected counts
      
  xfit=[[-reverse(reform(vgaussfit(idet,*)))],$
        [reform(vgaussfit(oppdet(idet),*))]]
  ffit=[[reverse(reform(fegaussfit(idet,*)))],$
        [reform(fegaussfit(oppdet(idet),*))]]
              
  yrange=[.1,1000]
   
    if specie_selct then begin
      frange=[1.e-26,1.e-18] 
      fticks=8
    endif else begin
      frange=[1.e-31,1.e-24]
      fticks=7
    endelse
    
    if idet eq 0 then title='ebias1 '+string(ebias1,format='(i2)')+'     '+$
            'det '+string(idet,format='(i1)')+'         '+$
            'sector '+string(isector,format='(i1)')+'         '+$
            'det '+string(oppdet(idet),format='(i1)')+'      '+$
            'ebias2 '+string(ebias2,format='(i2)') $
    else  title=$
            'det '+string(idet,format='(i1)')+'                              '+$
            'det '+string(oppdet(idet),format='(i1)')    
            
    plot_io,x,f,/nodata,yrange=frange,yticks=fticks,ystyle=1,$
      xrange=[-max(1.05*vel),max(1.05*vel)],xticks=6,xstyle=1,xminor=6,$
      ytitle='phase density',xtitle=xtitle(idet),$
      title=title,$
      subtitle=stitle(idet),charsize=charsize,$
      position=posn(*,idet),noerase=noerase(idet)
    oplot,[0.,0.],frange,linestyle=1 
    if hardcopy then oplot,x,f,psym=1,symsize=0.75 else $
    oplot,x,f,psym=1,symsize=0.75,color=wst.clr_orange
    
    ;plot patch fit obtained from moments analysis
    oplot,xfit,ffit
    
    xyouts,posn(0,idet)+0.65*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+0.9*(posn(3,idet)-posn(1,idet)),$
    /normal,'sc pot'
    
    xyouts,posn(0,idet)+0.65*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+0.8*(posn(3,idet)-posn(1,idet)),$
    /normal,string(vpot*1e-8,format='(f3.1)')+'e8 cm/s'
    
    xyouts,posn(0,idet)+0.65*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+0.7*(posn(3,idet)-posn(1,idet)),$
    /normal,'(moments)'
    
    fcore,bnfit,dne_core,te_core,u_core
    ucore=sqrt(u_core(0)^2+u_core(1)^2+u_core(2)^2)
    thucore=asin(u_core(2)/ucore)/!dtor
    phucore=atan(u_core(1),u_core(0))/!dtor
    if phucore lt 0 then phucore=phucore+360.
    
    xyy=0.9 & yspce=0.075 
     
    xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+xyy*(posn(3,idet)-posn(1,idet)),$
    /normal,string(dne_core,format='(f4.1)')+' cm^-3'
    
    xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(te_core,format='(e8.2)')+' degK'
    
    ev_core=8.614e-5*te_core
    xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-2*yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(ev_core,format='(f5.2)')+' ev'
    
    xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-3*yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(ucore*1e-5,format='(i4)')+' km/s'
    
   xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-4*yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(thucore,format='(i3)')+' deg el'
    
   xyouts,posn(0,idet)+0.8*(posn(2,idet)-posn(0,idet)),$
    posn(1,idet)+(xyy-5*yspce)*(posn(3,idet)-posn(1,idet)),$
    /normal,string(phucore,format='(i3)')+' deg az'
    
if hardcopy then begin    
    oplot,-reverse(vel),reverse(reform(phased_1ct(idet,*))),linestyle=1
    oplot,vel,reform(phased_1ct(oppdet(idet),*)),linestyle=1
endif else begin
  oplot,-reverse(vel),reverse(reform(phased_1ct(idet,*))),linestyle=1,$
      color=wst.clr_orange
    oplot,vel,reform(phased_1ct(oppdet(idet),*)),linestyle=1,$
      color=wst.clr_orange   
endelse    
  ;print,' ' & print,'sortv ',sortv 
endfor

if hardcopy then begin   
  device,/close
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  hardcopy=0
  clrtbl_indx
  goto,start
endif

!p.multi=0


end
