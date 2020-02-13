pro vstep_sunangle

;plots veis counts data

;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
;common lzstuff,$
;infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf
common log_delog,comp_tbl,dcomp_tbl
common shareplot,hardcopy,plotsel,$
   ndets,nvsteps,nsectors,nspins,idet,ivstep,isector,ispin

start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin 
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm
endif
 
!p.multi=[0,2,3,0,0]
!p.charsize=2.25
 
xtitle=[' ',' ',' ',' ','sunangle','sunangle']
ytitle=['counts',' ','counts',' ','counts',' ']

tjd=long(fix(vsmjf.suntim_vsbl(ispin)/86400.d))
           sec=vsmjf.suntim_vsbl(ispin) - tjd*86400.d
           hour_hms,sec/3600.d,hms
           spndt='spn '+string(tjd,format='(i5)')+' '+hms
stitle=[' ',' ',' ',' ',spndt,spndt]

steps=[15,14,13,12,11,10]
for jstep=0,n_elements(steps)-1 do begin
  x=fltarr(ndets*nsectors)
  y=fltarr(ndets*nsectors)
  k=-1
  for jdet=0,ndets-1 do for jsector=0,nsectors-1 do  begin
    k=k+1
    x(k)=acos(sin(vsmjf.theveis(jdet)*!dtor)*$
              cos(vsmjf.phiveis(jdet,steps(jstep),jsector)*!dtor))/!dtor
    y(k)=dcomp_tbl( vsmjf.veis(jdet,steps(jstep),jsector,ispin) )
  endfor
  xrange=[0,180]
  xticks=4
  xtickformat='(i3)'
  ;yrange=[0,4096]
 
  yrange=[0,1.1*max(dcomp_tbl( vsmjf.veis(*,steps(jstep),*,ispin) ))] 
  yticks=6
  ytickformat='(i4)'
  plot,x,y,/nodata,$
    title=$
          '  step '+string(steps(jstep),format='(i2)')+$
          '  spin '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtickformat=xtickformat,$
    xtitle=xtitle(jstep),$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(jstep),subtitle=stitle(jstep)
  oplot,x,y,psym=4,color=125
endfor


if hardcopy then begin   
  device,/close
 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lpr '+pflnm
  ;spawn,'cp '+pflnm+'last_'+pflnm
  !p.color=125
  hardcopy=0
  goto,start
endif

!p.multi=0
!p.charsize=1.

end
