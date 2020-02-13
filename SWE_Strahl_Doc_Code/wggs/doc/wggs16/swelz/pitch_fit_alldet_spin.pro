pro funct3,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end


pro pitch_fit_alldet_spin

;plots veis counts data

common sharewidg,wa
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common magstuff,magfile,tpb5,bgse
common phasemod1,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl
common swestuff,swest
common wstuff,wst

hardcopy=wst.hardcopy & ndets=swest.ndets & nvsteps=swest.nvsteps 
nsectors=swest.nsectors & nspins=swest.nspins & idet=swest.idet 
ivstep=swest.ivstep & isector=swest.isector & ispin=swest.ispinbl
delete=swest.delete & ndel=swest.ndel & minmax=swest.minmax 

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
 
!p.charsize=2.25


 
;xtitle=[' ',' ',' ',' ','pitch angle','pitch angle']
;ytitle=['counts',' ','counts',' ','counts',' ']
xtitle='pitch angle'
ytitle='counts'

tjd=long(fix(vsmjf.suntim_vsbl(ispin)/86400.d))
sec=vsmjf.suntim_vsbl(ispin) - tjd*86400.d
hour_hms,sec/3600.d,hms
;spndt='spn '+string(tjd,format='(i5)')+' '+hms
timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
stitle=spndt+'  '+yn(delete)+' delete'


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
  pa(i,j)=acos(vhat(i,ivstep,j,0)*bhat(0)+$
             vhat(i,ivstep,j,1)*bhat(1)+$
             vhat(i,ivstep,j,2)*bhat(2))/!dtor
  cts(i,j)=vsmjf.veis(i,ivstep,j,ispin)  
endfor


;select detectors upon which spin fit is to be made
detslct=intarr(6)
for i=0,5 do $
  if strmid(swest.detslct_list(swest.detslct_list_indx),i,1) eq 'x' then $
  detslct(i)=-1 else $
  detslct(i)=strmid(swest.detslct_list(swest.detslct_list_indx),i,1)
detslct=detslct(where(detslct ne -1))
print,'detslct ',detslct
 
pa_slct=fltarr(n_elements(detslct),nsectors)  ;pitch angle
cts_slct=intarr(n_elements(detslct),nsectors)  ;decompressed counts
for j=0,nsectors-1 do for i=0,n_elements(detslct)-1 do begin
  pa_slct(i,j)=acos(vhat(detslct(i),ivstep,j,0)*bhat(0)+$
             vhat(detslct(i),ivstep,j,1)*bhat(1)+$
             vhat(detslct(i),ivstep,j,2)*bhat(2))/!dtor
  cts_slct(i,j)=vsmjf.veis(detslct(i),ivstep,j,ispin)  
endfor


;fitting all (selected) detectors and sectors
wn0=where(cts_slct ne 0,nwn0)
if wn0(0) eq -1 then stop,'all zeroes'
c_slct=cts_slct(wn0)
p_slct=pa_slct(wn0)
srtp=sort(p_slct)
p_slct=p_slct(srtp)
c_slct=c_slct(srtp)
cav=total(c_slct)/n_elements(c_slct)
;wt=1.+fltarr(n_elements(c_slct))   ;equal weights
wt=1.0/c_slct


     a=fltarr(3)
     a(0)=cav
     fit=curvefit(p_slct-90.,c_slct,wt,a,siga,function_name='funct3')
     
     wn0=where(cts ne 0,nwn0)
     if wn0(0) eq -1 then stop,'all zeroes'
     c=cts(wn0)
     p=pa(wn0)
     srtp=sort(p)
     p=p(srtp)
     c=c(srtp)
     ;fit to all dets using coef's from fit with selecterd detectors
     funct3,p-90.,a,fit_funct3

  x=p_slct
  y=c_slct
  xrange=[0,180]
  xticks=4
  xtickformat='(i3)'
  ;yrange=[0,4096]
  yrange=[0,1.1*max(cts)] 
  yticks=6
  ytickformat='(i4)'
  plot,p,c,/nodata,$
    title='dets '+swest.detslct_list(swest.detslct_list_indx)+$
          '  step '+string(ivstep,format='(i2)')+$
          string(volt_en(vsmjf.veistep(ivstep),/en),format='(i4)')+$
          'ev'+'  spin '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtickformat=xtickformat,$
    xtitle=xtitle,$
    yrange=yrange,yticks=yticks,ystyle=1,ytickformat=ytickformat,$
    ytitle=ytitle,subtitle=stitle(0)
  oplot,p,c,psym=1,symsize=0.85,color=clr(0)
  oplot,p_slct,c_slct,psym=4,symsize=0.85,color=clr(1)

  oplot,p_slct,fit,psym=0,symsize=0.85,color=clr(1)    ;fit to all dets (selected)
  
  ;fit to all dets using coef's from fit with selected detectors
  oplot,p,fit_funct3,psym=1,symsize=0.85,color=clr(0)

svfl=0
if svfl then begin
  savflnm='pitchfitspin_idlsave.dat'
  save,filename=savflnm,p,c,a
  print,'save file '+savflnm+' saved'
  help,p,c,a
endif

;endfor


if hardcopy then begin   
  device,/close 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  wst.hardcopy=0
  clrtbl_indx
  goto,start
endif

!p.multi=0
!p.charsize=1.

end
