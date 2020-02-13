pro veis_sectors

;plots veis counts data

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

;help,vsmjf.veis
!p.multi=[0,2,3,0,0]
charsize=2.25
 
xtitle=[' ',' ',' ',' ','sector','sector']
ytitle=['counts',' ','counts',' ','counts',' ']


timpb5=vsmjf.pb5tim_vsbl(*,ispin)
sec=double(timpb5(2)/1000)
hour_hms,sec/3600.d,hms
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
stitle=[' ',' ',' ',' ',spndt,' ']

for jdet=0,ndets-1 do begin
  x=indgen(nsectors)
  y=abs(vsmjf.veis(jdet,ivstep,*,ispin))
  vlevel=-1
  if vsmjf.scimode eq 2 then begin
    if vsmjf.eleion_sweep eq 0 then vlevel=vsmjf.veistep(ivstep,0,0) 
  endif else $
 if vsmjf.scimode eq 1 or vsmjf.scimode eq 4 then vlevel=vsmjf.veistep(ivstep) $
 else if vsmjf.scimode eq 6 then vlevel=vsmjf.veistep(ivstep,ispin)
 
  xrange=[min(x)-1,max(x)+1]
  xticks=n_elements(x)+1
  xtickname=[' ','0','1','2','3','4','5',' ']
  ;yrange=[0,4096]
  if minmax eq 1 then $
     yrange=[0,1.1*max(abs(vsmjf.veis(*,ivstep,*,ispin)) )] $ 
  else $
     yrange=[0,1.1*max(abs(vsmjf.veis(jdet,ivstep,*,ispin) ))]
  yticks=6
  ytickformat='(i4)'
    plot,x,y,/nodata,$
    title='det '+string(jdet,format='(i1)')+'    '+$
          '  step '+string(ivstep,format='(i2)')+$
          '  vlevel '+string(vlevel,format='(i3)')+$
          '  spin '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtitle=xtitle(jdet),$
    xtickname=xtickname,charsize=charsize,$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(jdet),subtitle=stitle(jdet)
  oplot,x,y,psym=4,symsize=0.85     ;,color=clr(hardcopy)
  wglnt=where(vsmjf.xveis(jdet,ivstep,*,ispin) eq -1)
  if wglnt(0) ne -1 then begin
     if hardcopy eq 0 then oplot,[wglnt],$
        [abs(vsmjf.veis(jdet,ivstep,wglnt,ispin))],$
        psym=7,symsize=0.85,color=wst.clr_green  $
     else oplot,[wglnt],$
        [abs(vsmjf.veis(jdet,ivstep,wglnt,ispin))],$
        psym=7,symsize=0.85,color=clr(hardcopy)
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
