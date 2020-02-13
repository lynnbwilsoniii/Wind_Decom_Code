pro funct3,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end



pro pitch_fit_det_spinsacc_m1,savegains=savegains,detpair=detpair

;plots veis counts data

common sharewidg,wa
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common magstuff,magfile,tpb5,bgse
common phasemod1,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl
common sums,sumcts,vlvls,instrhk,numspins
common swestuff,swest
common wstuff,wst
common detpair_relgain,dprg

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete
nrmldet=swest.relgain_nmldet

if keyword_set(savegains) eq 0 then savegains=0
if keyword_set(detpair) eq 0 then $
  relgain_detpairs=1.+fltarr(6) $ 
else $
  relgain_detpairs=dprg

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
charsize=2.25
 
xtitle=[' ',' ',' ',' ','pitch angle','pitch angle']
ytitle=['counts',' ','counts',' ','counts',' ']

tjd=long(fix(vsmjf.suntim_vsbl(ispin)/86400.d))
sec=vsmjf.suntim_vsbl(ispin) - tjd*86400.d
hour_hms,sec/3600.d,hms
timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
stitle=[' ',' ',' ',' ',spndt,yn(delete)+' delete']

;get unit vectors corresponding to 180 degress from look directions
phasem1  ;get detector angles (in common phase)

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

;get counts vs pitch angle 
pa=fltarr(ndets,nsectors,numspins)  ;pitch angle
cts=intarr(ndets,nsectors,numspins)  ;decompressed counts
for j=0,nsectors-1 do for i=0,ndets-1 do for k=0,numspins-1 do begin
  pa(i,j,k)=acos(vhat(i,ivstep,j,0)*instrhk(k).bhat(0)+$
                 vhat(i,ivstep,j,1)*instrhk(k).bhat(1)+$
                 vhat(i,ivstep,j,2)*instrhk(k).bhat(2))/!dtor
  cts(i,j,k)=sumcts(i,ivstep,j,k) * relgain_detpairs(i)
endfor

;select detectors upon which spin fit is to be made
detslct=intarr(6)
for i=0,5 do $
  if strmid(swest.detslct_list(swest.detslct_list_indx),i,1) eq 'x' then $
  detslct(i)=-1 else $
  detslct(i)=strmid(swest.detslct_list(swest.detslct_list_indx),i,1)
detslct=detslct(where(detslct ne -1))
print,'detslct ',detslct

;pa_slct and cts_slct are dimensioned only for the selected dets, i.e., 4 or 5
pa_slct=fltarr(n_elements(detslct),nsectors,numspins)  ;pitch angle
cts_slct=intarr(n_elements(detslct),nsectors,numspins)  ;decompressed counts
for j=0,nsectors-1 do for i=0,n_elements(detslct)-1 do for k=0,numspins-1 $
do begin
  pa_slct(i,j,k)=acos(vhat(detslct(i),ivstep,j,0)*instrhk(k).bhat(0)+$
                 vhat(detslct(i),ivstep,j,1)*instrhk(k).bhat(1)+$
                 vhat(detslct(i),ivstep,j,2)*instrhk(k).bhat(2))/!dtor
  cts_slct(i,j,k)=sumcts(detslct(i),ivstep,j,k) * relgain_detpairs(detslct(i))
endfor


;make initial estimate of fit for individual detectors by 
;  fitting all detectors and sectors
wn0=where(cts_slct ne 0,nwn0)

wt=1.0/cts_slct(wn0)

a=fltarr(3)
a(0)=total(cts_slct(wn0))/nwn0
fit=curvefit(pa_slct(wn0)-90.,cts_slct(wn0),wt,a,siga,$
  function_name='funct3',chi2=chi2)
mean=moment(cts_slct(wn0),sdev=sdev)

gainoffset=fltarr(ndets)
chi2new=fltarr(ndets)
;order plots in opposing detector pairs
  detector=   [0,5,1,4,2,3]
  
for idetector=0,ndets-1 do begin
  idet=detector(idetector)
  
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
          string(volt_en(vsmjf.veistep(ivstep),/en),format='(i4)')+'ev',$
    xrange=xrange,xticks=xticks,xstyle=1,xtickformat=xtickformat,$
    xtitle=xtitle(idet),$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(idet),subtitle=stitle(idet),charsize=charsize
  oplot,x,y,psym=4,symsize=0.85,color=clr(hardcopy)

  ;new fitting param a(0), same a(1),a(2), 
  ;i.e., same shape parabola for the indivudual detector as for the whole set,
  ;but displaced from whole set by the difference in the a(0) fit coeff
  ;between the individual detector and the set
  wyn0=where(y ne 0,nwyn0)
  a0p=y(wyn0)-a(1)*(x(wyn0)-90.)-a(2)*(x(wyn0)-90.)^2
  a0_new=total(a0p)/nwyn0  ;average over all data this detector
  
  ;fit of this det using coeff's from set of det's
    yfit=a(0)+a(1)*(x(wyn0)-90.)+a(2)*(x(wyn0)-90.)^2
  ;fit using const coeff this det and shape coeff's from set of det's  
    ynewfit=a0_new+a(1)*(x(wyn0)-90.)+a(2)*(x(wyn0)-90.)^2
  ;to get chi2 for each det
    anew=fltarr(3)
    anew(0)=a0_new & anew(1:2)=a(1:2)
    wtnew=1./y(wyn0)
    fitnew=curvefit(x(wyn0)-90.,y(wyn0),wtnew,anew,siga,function_name='funct3',$
      chi2=ch)
    chi2new(idet)=ch
  
  ;gainoffset is factor by which each det counts must be multiplied in order for
  ;the det fit to eual the fit for the set of det's 
    gainoffset(idet)=(a(0)/a0_new) * relgain_detpairs(idet)
  p=x(wyn0)
  c=yfit
  cnew=ynewfit

  oplot,p(sort(p)),c(sort(p)),psym=0,symsize=0.85,color=175
  
  oplot,p(sort(p)),cnew(sort(p)),psym=0,symsize=0.85,color=clr(hardcopy)
  
endfor
print,' '
print,spndt
print,string(volt_en(vsmjf.veistep(ivstep),/en),format='(i4)')+'ev'
print,sec_pb5(instrhk(0).sectjd),numspins,$
    instrhk(0).ebias1,instrhk(0).ebias2,instrhk(0).bhat,detslct,$
    format='(i4,i4,i9,i5,2i3,3e14.5,2x,6i1)'
print,'mean(0),sdev,rtchi2 ',mean(0),sdev,sqrt(chi2)
print,'rtchi2new',sqrt(chi2new),format='(a11,6f7.3)'
print,'2rtchisq ',((sqrt(chi2new)+sqrt(chi2))/a(0))*gainoffset,$
  format='(a11,6f7.3)'


;relative gains are the gainoffsets normalized to the one detector which is
;believed to be changing the least over time
relativegain=fltarr(6)
for i=0,5 do begin
  wdet=where(detslct eq i)
  if wdet(0) ne -1 then relativegain(i)=gainoffset(i)/gainoffset(nrmldet) $
  else relativegain(i)=-1.0
endfor
print,' '
print,'gainoffsets ',gainoffset,format='(a11,6f7.3)'
print,'normal   ',relativegain,format='(a11,6f7.3)'
print,wst.lzdate,relativegain,instrhk(0).ebias1,instrhk(0).ebias2,$
    swest.ensteps_ev(swest.ivstep),format='(i8,3x,6f7.3,2i3,i5)'

ebias1=lz.mf(ihk(18).offs)
ebias2=lz.mf(ihk(28).offs)

if savegains then begin
  flnm=getenv('WGGSBASE')+'swelz/swecal/gains/gainsrev/add_to_tbl.ascii'
  openw,lun,flnm,/append,/get_lun
  printf,lun,wst.lzdate,relativegain,instrhk(0).ebias1,instrhk(0).ebias2,$
    swest.ensteps_ev(swest.ivstep),format='(i8,3x,6f7.3,2i3,i5)'
  free_lun,lun
  print,flnm,' appended and saved'
  savegains=0
endif

;if savegains then begin
;  flnm=getenv('IDLSAV')+'savegains_tmp.dat'
;  openw,lun,flnm,/append,/get_lun
;  printf,lun,spndt
;  printf,lun,string(volt_en(vsmjf.veistep(ivstep),/en),format='(i4)')+'ev'
;  printf,lun,sec_pb5(instrhk(0).sectjd),numspins,$
;    instrhk(0).ebias1,instrhk(0).ebias2,instrhk(0).bhat,detslct,$
;    format='(i4,i4,i9,i5,2i3,3e14.5,2x,6i1)'
;  printf,lun,'mean(0),sdev,rtchi2 ',mean(0),sdev,sqrt(chi2)
;  printf,lun,'relgains   ',gainoffset,format='(a11,6f7.3)'
;  printf,lun,$
;   '2*rtchisq  ',((sqrt(chi2new)+sqrt(chi2))/a(0))*gainoffset,format='(a11,6f7.3)'
;  printf,lun,'relgns_nml ',relativegain,format='(a11,6f7.3) 
;  printf,lun,' '
;  free_lun,lun
;  print,flnm,' saved'
;  savegains=0
;endif


if hardcopy then begin   
  device,/close
 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  ;spawn,'cp '+pflnm+'last_'+pflnm
  !p.color=0
  hardcopy=0
  goto,start
endif

!p.multi=0


end
