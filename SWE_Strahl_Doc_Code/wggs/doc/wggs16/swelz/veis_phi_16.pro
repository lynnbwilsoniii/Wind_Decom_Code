pro veis_phi_16

;plots veis counts data (all steps) vs  phi
;first used when in special mode with each vstep at the same voltage level

common sharewidg,wa
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest
common wstuff,wst

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete & ndel=swest.ndel & minmax=swest.minmax 

wst.hardcopy=0

yn=['no','yes']
clr=[225,wst.clr_green]

start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin 
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm
endif
 
!p.multi=[0,2,3,0,0]
charsize=2.25
 
xtitle=[' ',' ',' ',' ','phi','phi']
ytitle=['counts',' ','counts',' ','counts',' ']

timpb5=vsmjf.pb5tim_vsbl(*,ispin)
sec=double(timpb5(2)/1000)
hour_hms,sec/3600.d,hms
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
stitle=[' ',' ',' ',' ',spndt,' ']


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


for jdet=0,ndets-1 do begin
  x=reform(vsmjf.phiveis(jdet,*,*))
  y=reform(abs( vsmjf.veis(jdet,*,*,ispin) ))
  vmn=-1 & vmx=-1
  if vsmjf.scimode eq 2 then begin
    if vsmjf.eleion_sweep eq 0 then vmn=min(vsmjf.veistep(*,*,ispin),max=vmx) 
  endif else if vsmjf.scimode eq 1 or vsmjf.scimode eq 4 then $
    vmn=min(vsmjf.veistep(*),max=vmx) else $
  if  vsmjf.scimode eq 6 then  vmn=min(vsmjf.veistep(*,ispin),max=vmx)

  xrange=[0,360]
  xticks=4
  xtickformat='(i3)'
  ;yrange=[0,4096]
  if minmax eq 1 then $
    yrange=[0,1.1*max(abs( vsmjf.veis(*,*,*,ispin) ))] $
  else $
   yrange=[0,1.1*max(abs( vsmjf.veis(jdet,*,*,ispin) ))] 
  yticks=6
  ytickformat='(i4)'
  plot,x,y,/nodata,$
    title='det '+string(jdet,format='(i1)')+'  '+$
          '  stps '+string(0,format='(i1)')+':'+$
          string(vsmjf.n_vesteps-1,format='(i2)')+$
          '  vlvls '+string(vmn,format='(i3)')+':'+$
          string(vmx,format='(i3)')+$
          '  spn '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtickformat=xtickformat,$
    xtitle=xtitle(jdet),charsize=charsize,$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(jdet),subtitle=stitle(jdet)
  oplot,x,y,psym=4,symsize=0.50,color=clr(hardcopy)
  for isect=0,nsectors-1 do xyouts,x(0,isect),0.75*yrange(1),/data,$
    string(isect,format='(i1)'),color=wst.clr_green

  wglnt=where(vsmjf.xveis(jdet,*,*,ispin) eq -1)
  if wglnt(0) ne -1 then begin
   if hardcopy eq 0 then oplot,x(wglnt),y(wglnt),psym=4,symsize=0.50,color=125 $
   else oplot,x(wglnt),y(wglnt),psym=4,symsize=0.50,color=clr(hardcopy)
  endif  
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
