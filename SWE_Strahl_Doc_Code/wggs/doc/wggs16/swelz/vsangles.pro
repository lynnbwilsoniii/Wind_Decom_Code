pro vsangles

;plots veis strahl angles

common sharewidg,wa
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common swestuff,swest
common wstuff,wst

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete & ndel=swest.ndel & minmax=swest.minmax 

wst.hardcopy=0

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
enstep=volt_en(wsteps(0:vsmjf.n_vesteps-1),/en,ion=specie_selct)


start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin 
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm,$
    /color
endif

!p.multi=[0,2,2,0,0]
charsize=1.25
 
xtitle=['sec','phi','sec','sec']
ytitle=['theta','theta','phi','log enstep']

 if hardcopy then $
 clrs=[wst.clr_orange,wst.clr_orange,wst.clr_orange,$
       wst.clr_green,wst.clr_green,wst.clr_green] $
 else clrs=[wst.clr_orange,wst.clr_orange,wst.clr_orange,$
       wst.clr_green,wst.clr_green,wst.clr_green] 

  xrange=[min(vsmjf.secveis(*,*,ispin)),max(vsmjf.secveis(*,*,ispin))] $
    - min(vsmjf.secveis(*,*,*))

  pb5_start_rec=sec_pb5(min(vsmjf.secveis(*,*,*)))
  pb5_start_spin=sec_pb5(min(vsmjf.secveis(*,*,ispin)))
  pb5_end_spin=sec_pb5(max(vsmjf.secveis(*,*,ispin)))

  stitle=string(pb5_start_rec(2)/1000,format='(i5)')+'  '+$
         string(pb5_start_spin(2)/1000,format='(i5)')+'  '+$
         string(pb5_end_spin(2)/1000,format='(i5)')+' sec'

;plot  theta vs time 
  plot,/nodata,xrange=xrange,xticks=6,xstyle=1,xtitle=xtitle(0),$
    yrange=[0,180],yticks=3,ystyle=1,ytitle=ytitle(0),xrange,[0,180],$
    title='spin '+string(ispin,format='(i1)'),charsize=charsize
  for jsector=0,vsmjf.n_sectors-1 do for idet=0, vsmjf.n_vdets-1 do  begin
    oplot,vsmjf.secveis(*,jsector,ispin)-min(vsmjf.secveis(*,*,*)),$
    replicate(vsmjf.theveis(idet),vsmjf.n_vesteps),psym=3,color=clrs(idet)
    if jsector eq 1 then $
    xyouts,vsmjf.secveis(7,jsector,ispin)-min(vsmjf.secveis(*,*,*)),$
    vsmjf.theveis(idet),$
    string(idet,format='(i1)'),alignment=1.0,charsize=1.25,color=125
  endfor


;plot theta vs phi
  plot,/nodata,xrange=[0,360],xticks=6,xstyle=1,xtitle=xtitle(1),$
    yrange=[0,180],yticks=3,ystyle=1,ytitle=ytitle(1),[0,360],[0,180],$
    title='spin '+string(ispin,format='(i1)'),charsize=charsize
 for jsector=0,vsmjf.n_sectors-1 do for idet=0,vsmjf.n_vdets-1 do  begin
   oplot,vsmjf.phiveis(idet,*,jsector),$
   replicate(vsmjf.theveis(idet),vsmjf.n_vesteps),psym=3,color=clrs(idet)
   if jsector eq 1 then $
    xyouts,vsmjf.phiveis(idet,7,jsector),vsmjf.theveis(idet),$
    string(idet,format='(i1)'),alignment=1.0,charsize=1.25,color=125
  endfor


;plot phi vs time 
  plot,/nodata,xrange=xrange,xticks=6,xstyle=1,xtitle=xtitle(2),$
    yrange=[0,360],yticks=6,ystyle=1,ytitle=ytitle(2),xrange,[0,360],$
    title='spin '+string(ispin,format='(i1)'),charsize=charsize
  for jsector=0,vsmjf.n_sectors-1 do for idet=0, vsmjf.n_vdets-1 do  begin
    oplot,vsmjf.secveis(*,jsector,ispin)-min(vsmjf.secveis(*,*,*)),$
    vsmjf.phiveis(idet,*,jsector),psym=3,color=clrs(idet)
    if jsector eq 1 then $
    xyouts,vsmjf.secveis(7,jsector,ispin)-min(vsmjf.secveis(*,*,*)),$
    vsmjf.phiveis(idet,7,jsector),$
    string(idet,format='(i1)'),alignment=1.0,charsize=1.25,color=125
  endfor


;plot enstep vs time 
  plot_io,/nodata,xrange=xrange,xticks=6,xstyle=1,xtitle=xtitle(3),$
    yrange=[min(enstep),max(enstep)],yticks=4,ystyle=1,ytitle=ytitle(3),$
    xrange,[min(enstep),max(enstep)],charsize=charsize,$
    title='spin '+string(ispin,format='(i1)'),subtitle=stitle
  for jsector=0,vsmjf.n_sectors-1 do  begin
    oplot,vsmjf.secveis(*,jsector,ispin)-min(vsmjf.secveis(*,*,*)),$
    enstep,psym=4,symsize=0.3
    oplot,vsmjf.secveis(*,jsector,ispin)-min(vsmjf.secveis(*,*,*)),$
    enstep,linestyle=1
  endfor


if hardcopy then begin   
  device,/close 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lpr '+pflnm
  hardcopy=0
  clrtbl_indx
  goto,start
endif

!p.multi=0

end
