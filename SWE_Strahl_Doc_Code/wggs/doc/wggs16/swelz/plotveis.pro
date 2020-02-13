pro plotveis

;plots veis counts data

;common lzstuff,infile,lundat,recn,fh,lz,ihk,hkm1,vsm1,vdatc,sdatc,sp,vsmjf
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common log_delog,comp_tbl,dcomp_tbl
common shareplot,hardcopy,ndets,nvsteps,nsectors,nspins,idet,ivstep,isector,ispin

start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin 
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=8.,ysize=8.,filename=plflnm
endif

;help,vsmjf.veis
;s=size(vsmjf.veis)
;ndets=s(1) & nvsteps=s(2) & nsectors=s(3) & nspins=s(4)
!p.multi=[0,2,3,0,0]
!p.charsize=2.

;window,0
;ispin=0  & ivstep=8
 
xtitle=[' ',' ',' ',' ','sector','sector']
ytitle=['counts',' ','counts',' ','counts',' ']
for idet=0,ndets-1 do begin
  x=indgen(nsectors)
  y=dcomp_tbl( vsmjf.veis(idet,ivstep,*,ispin) )
  xrange=[min(x)-1,max(x)+1]
  xticks=n_elements(x)
  xtickv=x
  ;yrange=[0,4096]
  yrange=[min(dcomp_tbl( vsmjf.veis(*,ivstep,*,ispin) )),$
        max(dcomp_tbl( vsmjf.veis(*,ivstep,*,ispin) ))  ]
  yticks=6
  ytickformat='(i4)'
  plot,x,y,/nodata,$
    title='det '+string(idet+1,format='(i2)')+'    '+$
          '  vstep '+string(ivstep,format='(i2)')+$
          '  spin '+string(ispin,format='(i2)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtickv=xtickv,xtitle=xtitle(idet),$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(idet)
  oplot,x,y,psym=1,color=225
endfor


if hardcopy then begin   
  device,/close
 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',plflnm
  spawn, 'lp '+plflnm
  spawn,'cp '+pflnm+'last_'+pflnm
  !p.color=125
  hardcopy=0
  goto,start
endif

end
