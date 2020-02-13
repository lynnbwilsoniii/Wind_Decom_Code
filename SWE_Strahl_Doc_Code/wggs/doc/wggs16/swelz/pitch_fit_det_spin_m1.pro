pro funct3,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end



pro pitch_fit_det_spin_m1

;plots veis counts data

common sharewidg,wa
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common magstuff,magfile,tpb5,bgse
common phasemod1,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl
common swestuff,swest
common wstuff,wst

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete


yn=['no','yes']
clr=[wst.clr_green,wst.clr_green]

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


 
xtitle=[' ',' ',' ',' ','pitch angle','pitch angle']
ytitle=['counts',' ','counts',' ','counts',' ']

tjd=long(fix(vsmjf.suntim_vsbl(ispin)/86400.d))
sec=vsmjf.suntim_vsbl(ispin) - tjd*86400.d
hour_hms,sec/3600.d,hms
timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
stitle=[' ',' ',' ',' ',spndt,yn(delete)+' delete']


;get mfi 3sec mag field
    ; timpb5=given pb5 time (year, day of year, millisec of day)  lonarr(3)
    ; tpb5 = pb5 time at center of minute for given record recn   lonarr(1440,3)
    ; bgse = mag vector at 3 sec intervals in minute tpb5     fltarr(1440,3,20)
    mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
    sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
    magfld=bgse(mindx,0:2,sindx)
    magtpb5=[tpb5(0),tpb5(1),tpb5(2) - (30000l - 1500l - long(sindx)*3000)]
    print,' '
    print,'given time ',timpb5
    print,'  mag time ',magtpb5
    print,'mag fld (gse) ',magfld
    if magfld(0) eq -1.e31 then begin
      print,'mag fld eq fill' & return
    endif
    bhat=magfld/sqrt(total(magfld^2))
 
;get phi, theta for mag fld
  phimag=atan(bhat(1),bhat(0))/!dtor
  if phimag lt 0 then phimag=phimag + 360.
  themag=acos(bhat(2))/!dtor

;get unit vectors corresponding to 180 degress from look directions
phasem1  ;get detector angles (in common phase)
vhatm=1.+fltarr(nvsteps)    ;unit magnitude for velocity unit vectors
snhat=sin(theveis*!dtor)#vhatm
cnhat=cos(theveis*!dtor)#vhatm
vhat=fltarr(ndets,nvsteps,nsectors,3)  ;vel unit vector
vhat(*,*,*,0)=-(snhat(*)#replicate(1,nsectors))*cos(phiveis(*,*,*)*!dtor)
vhat(*,*,*,1)=-(snhat(*)#replicate(1,nsectors))*sin(phiveis(*,*,*)*!dtor)
vhat(*,*,*,2)=-cnhat(*)#replicate(1,nsectors)
vhat(*,*,*,1:2)=-vhat(*,*,*,1:2)  ;approx transformation from payload to gse

 
pa=fltarr(ndets,nsectors)  ;pitch angle
cts=intarr(ndets,nsectors)  ;decompressed counts
for j=0,nsectors-1 do for i=0,ndets-1 do begin
  pa(i,j)=acos(vhat(i,ivstep,j,0)*bhat(0)+vhat(i,ivstep,j,1)*bhat(1)+$
             vhat(i,ivstep,j,2)*bhat(2))/!dtor
  cts(i,j)= vsmjf.veis(i,ivstep,j,ispin)
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
pa_slct=fltarr(n_elements(detslct),nsectors)  ;pitch angle
cts_slct=intarr(n_elements(detslct),nsectors)  ;decompressed counts
for j=0,nsectors-1 do for i=0,n_elements(detslct)-1 do begin
  pa_slct(i,j)=acos(vhat(detslct(i),ivstep,j,0)*bhat(0)+$
             vhat(detslct(i),ivstep,j,1)*bhat(1)+$
             vhat(detslct(i),ivstep,j,2)*bhat(2))/!dtor
  cts_slct(i,j)=vsmjf.veis(detslct(i),ivstep,j,ispin)  
endfor
 
;make initial estimate of fit for individual detectors by 
;  fitting all (selected) detectors and sectors

wn0=where(cts_slct ne 0,nwn0)
wt=1.0/cts_slct(wn0)

a=fltarr(3)
a(0)=total(cts_slct(wn0))/nwn0
fit=curvefit(pa_slct(wn0)-90.,cts_slct(wn0),wt,a,siga,function_name='funct3')
;the fit coefficients, a, then are based on the selected detectors

relg=fltarr(ndets)
for jdet=0,ndets-1 do begin
  
  x=pa(jdet,*)
  y=cts(jdet,*)
  xrange=[0,180]
  xticks=4
  xtickformat='(i3)'
  ;yrange=[0,4096]
  yrange=[0,1.1*max(cts)] 
  yticks=6
  ytickformat='(i4)'
  plot,x,y,/nodata,$
    title='det '+string(jdet,format='(i1)')+'    '+$
          '  step '+string(ivstep,format='(i2)')+$
          string(volt_en(vsmjf.veistep(ivstep),/en),format='(i4)')+'ev'+$
          '  spin '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtickformat=xtickformat,$
    xtitle=xtitle(jdet),$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle(jdet),subtitle=stitle(jdet),charsize=charsize
  oplot,x,y,psym=4,symsize=0.85,color=clr(hardcopy)


  ;new fitting param a(0), same a(1),a(2), i.e., same shape parabola
  ;a0_new=new a(0) is estimated to be the average of the sum of the 
  ;   differences, measured cts  minus that portion of fit for det jdet
  wyn0=where(y ne 0,nwyn0)
  a0_new=total(y(wyn0)-a(1)*(x(wyn0)-90.)-a(2)*(x(wyn0)-90.)^2)/nwyn0
  ;yfit=the spin fit using only the selected detectors 
  yfit=a(0)+a(1)*(x(wyn0)-90.)+a(2)*(x(wyn0)-90.)^2
  ynewfit=a0_new+a(1)*(x(wyn0)-90.)+a(2)*(x(wyn0)-90.)^2

  print,string(ivstep,format='(i2)'),volt_en(vsmjf.veistep(ivstep),/en),$
   '  jdet, a0_new/a0 ',jdet,a0_new/a(0)
  relg(jdet)=a(0)/a0_new
  ;the relg for the detector(s) NOT included in the spin fit might not
  ;be meaningful and may have to be determined by trial and error
  p=x(wyn0)
  c=yfit
  cnew=ynewfit
    

  oplot,p(sort(p)),c(sort(p)),psym=0,symsize=0.85,color=clr(hardcopy)
;stop
  oplot,p(sort(p)),cnew(sort(p)),psym=0,symsize=0.85,color=wst.clr_orange
  
endfor
print,' '
print,'relgains ',relg,format='(a10,6f7.3)'
;print,'normal   ',relg/min(relg),format='(a10,6f7.3)'
print,'normal   ',relg/relg(0),format='(a10,6f7.3)'

if hardcopy then begin   
  device,/close
 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  swest.hardcopy=0
  clrtbl_indx
  goto,start
endif

!p.multi=0

end
