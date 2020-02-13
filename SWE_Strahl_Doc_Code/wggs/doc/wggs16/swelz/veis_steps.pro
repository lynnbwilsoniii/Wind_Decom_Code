pro veis_steps,ff=ff,crrctd=crrctd,backg=backg

;plots veis counts/f data

;common sharewidg,wa
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common special1,lunar
common swestuff,swest
common wstuff,wst

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete & ndel=swest.ndel & minmax=swest.minmax 

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
charsize=1.15 

   

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
  
stitle=[' ',' ','adjstd rel gains  '+string(swest.relgain,format='(6f5.1)')]
xtitle=[' ',' ',$
'mode'+string(vsmjf.scimode,format='(i2)')$
+'  '+specie(specie_selct)+'  '+spndt+'  '+' ']

ebias1=lz.mf(ihk(18).offs)
ebias2=lz.mf(ihk(28).offs)

;sort voltage steps in order of increasing step and form velocity sorted arrays
  sortv=sort(wsteps)
  vel=volt_en(wsteps(sortv),/vel,ion=specie_selct)
  counts=float(abs(vsmjf.veis(*,sortv,isector,ispin))) > 1
  ;counts_b=abs(vsmjf.veis_b(*,sortv,isector,ispin)) > 1  ;counts - background
  counts_b=abs(vsmjf.cveis_b(*,sortv,isector,ispin)) > 1  ;counts - background

;phase density of counts minus background (speed sorted)
;convert to f's 
   phased_b=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps)
   phased_b(*,*)=vsmjf.fveis_b(*,sortv,isector,ispin)

;Provide the capability to interactively modify the relative gains for testing.
;  (It is assumed that there has been no prior background subtraction. 
;   This will be true for dates after 19971026.)
for i=0,vsmjf.n_vdets-1 do begin
    counts_b(i,*)=counts_b(i,*)*(swest.relgain(i)/vsmjf.relgain(i))
    phased_b(i,*)=phased_b(i,*)*(swest.relgain(i)/vsmjf.relgain(i))
endfor
   
;one-count phase density
   phased_1ct=fltarr(vsmjf.n_vdets,vsmjf.n_vesteps)
   phased_1ct(*,*)=vsmjf.cts_factor(*,sortv)
   
for idet=0,2 do begin
  x=[-reverse(vel),vel]                       ;speed
  y=float([[reverse(reform(counts(idet,*)))],$
           [reform(counts(oppdet(idet),*))]]) ;measured counts
  z=[[reverse(reform(counts_b(idet,*)))],$
     [reform(counts_b(oppdet(idet),*))]]      ;measured counts * relgain - bckg
  f=[[reverse(reform(phased_b(idet,*)))],$
      [reform(phased_b(oppdet(idet),*))]]     ;phase densty of corrected counts

   
  yrange=[.1,1000]

  ;highlight locations of glint points, w=where
    xv=[[reverse(reform(vsmjf.xveis(idet,sortv,isector,ispin)))],$
       [reform(vsmjf.xveis(oppdet(idet),sortv,isector,ispin))]]
    wglnt=where(xv eq -1)

    ;wsngl=where(reform(vsmjf.xveis(idet,*,isector,ispin)) eq -1)
    ;print,'det ',idet,'  sector ',isector,'  phase steps ',wsngl
    ;wsngl=where(reform(vsmjf.xveis(oppdet(idet),*,isector,ispin)) eq -1)
    ;print,'det ',idet,'  sector ',isector,'  phase steps ',wsngl
    
    ;wsngl=where(reform(vsmjf.xveis(idet,sortv,isector,ispin)) eq -1)
    ;print,'det ',idet,'  sector ',isector,'  vel steps ',wsngl
    ;wsngl=where(reform(vsmjf.xveis(oppdet(idet),sortv,isector,ispin)) eq -1)
    ;print,'det ',idet,'  sector ',isector,'  vel steps ',wsngl
    
  if idet eq 0 then title='ebias1 '+string(ebias1,format='(i2)')+'     '+$
            'det '+string(idet,format='(i1)')+'         '+$
            'sector '+string(isector,format='(i1)')+'         '+$
            'det '+string(oppdet(idet),format='(i1)')+'      '+$
            'ebias2 '+string(ebias2,format='(i2)') $
    else  title=$
            'det '+string(idet,format='(i1)')+'                              '+$
            'det '+string(oppdet(idet),format='(i1)')    
             
  if keyword_set(ff) ne 0 then begin
    if specie_selct then begin
      frange=[1.e-26,1.e-18] 
      fticks=8
    endif else begin
      frange=[1.e-31,1.e-24]
      fticks=7
    endelse
    
    plot_io,x,f,/nodata,yrange=frange,yticks=fticks,ystyle=1,$
      xrange=[-max(1.05*vel),max(1.05*vel)],xticks=6,xstyle=1,xminor=6,$
      ytitle='phase density',xtitle=xtitle(idet),$
      title=title,$
      subtitle=stitle(idet),charsize=charsize,$
      position=posn(*,idet),noerase=noerase(idet)
    oplot,[0.,0.],frange,linestyle=1   
    oplot,x,f,psym=1,symsize=0.75,color=wst.clr_orange
    if wglnt(0) ne -1 then $
       oplot,x(wglnt),f(wglnt),psym=7,symsize=1.5,color=wst.clr_green
    oplot,-reverse(vel),reverse(reform(phased_1ct(idet,*))),linestyle=1,$
      color=wst.clr_orange
    oplot,vel,reform(phased_1ct(oppdet(idet),*)),linestyle=1,$
      color=wst.clr_orange   
    
    
  endif else begin
    if keyword_set(crrctd) ne 0 then begin
    ;if keyword_set(backg) ne 0 then begin
      plot_io,x,z,/nodata,yrange=yrange,yticks=4,ystyle=1,$
        xrange=[-max(1.05*vel),max(1.05*vel)],xticks=6,xstyle=1,xminor=6,$
        ytitle='corrected counts',xtitle=xtitle(idet),$
        title=title,$
        subtitle=stitle(idet),charsize=charsize,$
      position=posn(*,idet),noerase=noerase(idet)
      oplot,x,z,psym=1,symsize=0.75,color=wst.clr_orange   ;corrected counts
      if wglnt(0) ne -1 then $
       oplot,x(wglnt),z(wglnt),psym=7,symsize=1.5,color=wst.clr_green   ;glint 
    endif else begin
      plot_io,x,y,/nodata,yrange=yrange,yticks=4,ystyle=1,$
        xrange=[-max(1.05*vel),max(1.05*vel)],xticks=6,xstyle=1,xminor=6,$
        ytitle='measured counts',xtitle=xtitle(idet),$
        title=title,$
        charsize=charsize,$
      position=posn(*,idet),noerase=noerase(idet)
      oplot,x,y,psym=1,symsize=0.75;,color=wst.clr_orange    ;measured counts
      if wglnt(0) ne -1 then $
        oplot,x(wglnt),y(wglnt),psym=7,symsize=1.5,color=wst.clr_green   ;glint
    endelse
    oplot,[0.,0.],yrange,linestyle=1
    ;stop
  endelse
  print,' ' & print,'sortv ',sortv 
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

;!p.multi=0


end
