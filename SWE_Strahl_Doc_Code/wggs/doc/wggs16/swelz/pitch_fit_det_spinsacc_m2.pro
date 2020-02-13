pro funct3,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end



pro pitch_fit_det_spinsacc_m2,savegains=savegains

;plots veis counts data

common sharewidg,wa
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common magstuff,magfile,tpb5,bgse
common phasemod2,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl
common sums,sumcts,vlvls,instrhk,numspins
common swestuff,swest
common wstuff,wst

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete


if keyword_set(savegains) eq 0 then savegains=0

yn=['no','yes']
clr=[wst.clr_green,wst.clr_green]

start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin 
  !p.color=wst.clr_green 
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm
endif
 
!p.multi=[0,2,3,0,0]
!p.charsize=2.25


 
xtitle=[' ',' ',' ',' ','pitch angle','pitch angle']
ytitle=['counts',' ','counts',' ','counts',' ']

tjd=long(fix(vsmjf.suntim_vsbl(ispin)/86400.d))
sec=vsmjf.suntim_vsbl(ispin) - tjd*86400.d
hour_hms,sec/3600.d,hms
;spndt='spn '+string(tjd,format='(i5)')+' '+hms
timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
stitle=[' ',' ',' ',' ',spndt,yn(delete)+' delete']


;get unit vectors corresponding to 180 degress from look directions
phasem2  ;get detector angles (in common phase)

mthod=0
case mthod of

0: begin  ;zero flow velocity
vhatm=1.+fltarr(nvsteps)    ;unit magnitude for velocity unit vectors
snhat=sin(theveis*!dtor)#vhatm
cnhat=cos(theveis*!dtor)#vhatm
vhat=fltarr(ndets,nvsteps,nsectors,3)  ;vel unit vector
vhat(*,*,*,0)=-(snhat(*)#replicate(1,nsectors))*cos(phiveis(*,*,*)*!dtor)
vhat(*,*,*,1)=-(snhat(*)#replicate(1,nsectors))*sin(phiveis(*,*,*)*!dtor)
vhat(*,*,*,2)=-cnhat(*)#replicate(1,nsectors)
vhat(*,*,*,1:2)=-vhat(*,*,*,1:2)  ;approx transformation from payload to gse
endcase

1: begin  ;non-zero flow
vhat=fltarr(ndets,nvsteps,nsectors,3)  ;vel unit vector
velm=volt_en(vsmjf.veistep(ivstep),/vel)
;payload coords
for j=0,nsectors-1 do for i=0,ndets-1 do begin
  vhat(i,ivstep,j,0)=-sin(theveis(i)*!dtor)*cos(phiveis(i,ivstep,j)*!dtor)*velm
  vhat(i,ivstep,j,1)=-sin(theveis(i)*!dtor)*sin(phiveis(i,ivstep,j)*!dtor)*velm
  vhat(i,ivstep,j,2)=-cos(theveis(i)*!dtor)*velm
endfor 
vhat(*,ivstep,*,1:2)=-vhat(*,ivstep,*,1:2);approx transf from payload to gse
;gse coord frame moving with flow velocity u
u=fltarr(3)
u(0)=-325.e5
for i=0,2 do vhat(*,ivstep,*,i)=vhat(*,ivstep,*,i)-u(i)
;get unit vel vector
for j=0,nsectors-1 do for i=0,ndets-1 do begin
  vhatmag=sqrt(total(vhat(i,ivstep,j,*)^2))
  vhat(i,ivstep,j,*)=vhat(i,ivstep,j,*)/vhatmag
endfor
endcase
endcase

 
pa=fltarr(ndets,nsectors,numspins)  ;pitch angle
cts=intarr(ndets,nsectors,numspins)  ;decompressed counts
for j=0,nsectors-1 do for i=0,ndets-1 do for k=0,numspins-1 do begin
  pa(i,j,k)=acos(vhat(i,ivstep,j,0)*instrhk(k).bhat(0)+$
                 vhat(i,ivstep,j,1)*instrhk(k).bhat(1)+$
                 vhat(i,ivstep,j,2)*instrhk(k).bhat(2))/!dtor
  cts(i,j,k)=sumcts(i,ivstep,j,k)
endfor


;make initial estimate of fit for individual detectors by 
;  fitting all detectors and sectors
wn0=where(cts ne 0,nwn0)

wt=1.0/cts(wn0)

a=fltarr(3)
a(0)=total(cts(wn0))/nwn0
fit=curvefit(pa(wn0)-90.,cts(wn0),wt,a,siga,function_name='funct3',chi2=chi2)
mean=moment(cts(wn0),sdev=sdev)

relg=fltarr(ndets)
chi2new=fltarr(ndets)
for idet=0,ndets-1 do begin
  
  x=pa(idet,*,*)
  y=cts(idet,*,*)
  xrange=[0,180]
  xticks=4
  xtickformat='(i3)'
  ;yrange=[0,4096]
  yrange=[0,1.1*max(cts)] 
  yticks=6
  ytickformat='(i4)'
  plot,x,y,/nodata,$
    title='det '+string(idet,format='(i1)')+'    '+$
          '  step '+string(ivstep,format='(i2)')+$
          string(volt_en(vsmjf.veistep(ivstep,0,ispin),/en),format='(i4)')+'ev',$
    xrange=xrange,xticks=xticks,xstyle=1,xtickformat=xtickformat,$
    xtitle=xtitle(idet),$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(idet),subtitle=stitle(idet)
  oplot,x,y,psym=4,symsize=0.85,color=clr(hardcopy)

  ;new fitting param a(0), same a(1),a(2), i.e., same shape parabola
  wyn0=where(y ne 0,nwyn0)
  a0_new=total(y(wyn0)-a(1)*(x(wyn0)-90.)-a(2)*(x(wyn0)-90.)^2)/nwyn0
  yfit=a(0)+a(1)*(x(wyn0)-90.)+a(2)*(x(wyn0)-90.)^2
  ynewfit=a0_new+a(1)*(x(wyn0)-90.)+a(2)*(x(wyn0)-90.)^2
  ;to get chi2 for each det
    anew=fltarr(3)
    anew(0)=a0_new & anew(1:2)=a(1:2)
    wtnew=1./y(wyn0)
    fitnew=curvefit(x(wyn0)-90.,y(wyn0),wtnew,anew,siga,function_name='funct3',$
      chi2=ch)
  chi2new(idet)=ch
  relg(idet)=a(0)/a0_new
  p=x(wyn0)
  c=yfit
  cnew=ynewfit

  oplot,p(sort(p)),c(sort(p)),psym=0,symsize=0.85,color=clr(hardcopy)
  oplot,p(sort(p)),cnew(sort(p)),psym=0,symsize=0.85,color=175
endfor
print,' '
print,'mean(0),sdev,rtchi2 ',mean(0),sdev,sqrt(chi2)
print,'rtchi2new',sqrt(chi2new),format='(a11,6f7.3)'
print,'relgains ',relg,format='(a11,6f7.3)'
print,'2rtchisq ',((sqrt(chi2new)+sqrt(chi2))/a(0))*relg,format='(a11,6f7.3)'
print,'normal   ',relg/relg(0),format='(a11,6f6.3)'
;print,'normal   ',relg/min(relg),format='(a11,6f6.3)'

if savegains then begin
  flnm=getenv('IDLSAV')+'savegains_tmp.dat'
  openw,lun,flnm,/append,/get_lun
  printf,lun,spndt
  printf,lun,string(volt_en(vsmjf.veistep(ivstep,0,ispin),/en),format='(i4)')+'ev'
  printf,lun,sec_pb5(instrhk(0).sectjd),numspins,$
    instrhk(0).ebias1,instrhk(0).ebias2,instrhk(0).bhat,$
    format='(i4,i4,i9,i5,2i3,3e14.5)'
  printf,lun,'mean(0),sdev,rtchi2 ',mean(0),sdev,sqrt(chi2)
  printf,lun,'relgains   ',relg,format='(a11,6f7.3)'
  printf,lun,$
   '2*rtchisq  ',((sqrt(chi2new)+sqrt(chi2))/a(0))*relg,format='(a11,6f7.3)'
  ;printf,lun,'relgns_nml ',relg/min(relg),format='(a11,6f7.3)' 
  printf,lun,'relgns_nml ',relg/relg(0),format='(a11,6f7.3) 
  printf,lun,' '
  free_lun,lun
  print,flnm,' saved'
  savegains=0
endif


if hardcopy then begin   
  device,/close
 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lpr '+pflnm
  ;spawn,'cp '+pflnm+'last_'+pflnm
  !p.color=0
  hardcopy=0
  goto,start
endif

!p.multi=0
!p.charsize=1.

end
