pro funct3,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end


pro strahl_veis

;compares strahl and veis data

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common detset,detseta,detsetb,detset
common magstuff,magfile,tpb5,bgse
common swestuff,swest
common wstuff,wst

hardcopy=wst.hardcopy & nspins=swest.nspins & ispin=swest.ispinbl
nstrldets=swest.nstrldets & nstrlsteps=swest.nstrlsteps
istrldet=swest.istrldet & istrlstep=swest.istrlstep

wst.hardcopy=0

if keyword_set(strldet0) eq 0 then strldet0=0

yn=['no','yes']
clr=[125,125]

start:

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm

endif

!p.multi=[0,2,4,0,0]



timpb5=vsmjf.pb5tim_vsbl(*,ispin)
sec=double(timpb5(2)/1000)
hour_hms,sec/3600.d,hms

;get mfi mag field
if swest.mag_3s_kp eq 1 then begin               ;use mfi 3s mag field
  get_3smag,timpb5,magtpb5,b,phi_bpos,the_bpos
endif else if swest.mag_3s_kp eq 2 then begin    ;use mfi kp mag field
  get_kpmag,timpb5,b,phi_bpos,the_bpos 
endif else begin                                 ;no mag data
  phi_bpos=0 & the_bpos=0
endelse 
magfld=b    
bhat=magfld/sqrt(total(magfld^2))
phimag=phi_bpos
themag=the_bpos

;get strahl detector particle unitvectors

nds=vsmjf.n_strdets * vsmjf.n_strphis
vshat=fltarr(nds,3)
if vsmjf.scimode eq 6 then begin
  vshat(*,0)=vsmjf.vunitstrl(*,*,0,ispin)
  vshat(*,1)=vsmjf.vunitstrl(*,*,1,ispin)
  vshat(*,2)=vsmjf.vunitstrl(*,*,2,ispin)
endif else begin
  vshat(*,0)=vsmjf.vunitstrl(*,*,0)
  vshat(*,1)=vsmjf.vunitstrl(*,*,1)
  vshat(*,2)=vsmjf.vunitstrl(*,*,2)
endelse  
vshat(*,1:2)=-vshat(*,1:2)  ;approx transformation from payload to gse

;get particle pitch angles
vsparahat=vshat(*,0)*bhat(0)+vshat(*,1)*bhat(1)+vshat(*,2)*bhat(2)
strpa=acos(vsparahat)/!dtor
strpa=reform(strpa,vsmjf.n_strdets,vsmjf.n_strphis)

lprtest=0
if lprtest then begin
print,' '
for i=0, vsmjf.n_strdets-1 do begin
  print,' '
  print,'mag fld: phi= ',phimag,'   theta= ',themag
  print,'strahl det ',i,' theta= ',vsmjf.thestrl(i)
  print, '      step           phi   pitch angle    counts'
  for j=0,vsmjf.n_strphis-1 do print,j,vsmjf.phistrl(j),strpa(i,j),$
     dcomp_tbl( vsmjf.strl(i,j,ispin) ),format='(i10,f14.3,f10.3,i10)'

endfor
print,' '
endif

xrange=[0,180]
xticks=6
xminor=3
xtickformat='(i3)'

;get strahl hv step
ivstep=vsmjf.strlstep(ispin)

  ;print,energy and velocity steps
    print,' '
    print,'Strahl step, velocity, energy :'
    print,ivstep,$
             volt_en_strl(ivstep,/vel),$
             volt_en_strl(ivstep,/en)

ytitle=replicate('counts',nstrldets)

print,' '
print,string(volt_en_strl(ivstep,/en),format='(i5)')+'ev'
print,'det, chisq, cntsmax(meas), ptch, ctsmax(fit), fmax, wdth, area'

for i=0,n_elements(detset)-1 do begin
  jstrldet=detset(i)
  x=reform(strpa(jstrldet,*))
    
    yrange=fltarr(2)
    z=vsmjf.fstrl(*,*,ispin)
    yrange(0)=10.^fix(alog10(min( z(where(z ne 0))))-1.)
    yrange(1)=10.^fix(alog10(max(z(where(z ne 0)))))
    yticks=alog10(yrange(1))-alog10(yrange(0))
    y=reform(vsmjf.fstrl(jstrldet,*,ispin))
    ytitle=replicate('f_strl',nstrldets)
    ylog=1
  
  plot,x,y,/nodata,ylog=ylog,$
    title='STRAHL det '+string(jstrldet,format='(i2)')+$
          '  levl '+string(ivstep,format='(i2)')+$
          string(volt_en_strl(ivstep,/en),$
          format='(i5)')+'ev'+$
          '  spn '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,$
    xtickformat=xtickformat,xminor=xminor,charsize=2.25,$
    yrange=yrange,yticks=yticks,ystyle=1,$
    ytitle=ytitle(jstrldet)
  oplot,x,y,psym=4,symsize=0.50,color=clr(hardcopy)


endfor

;find veis steps straddling the strahl step
  if vsmjf.scimode eq 6 then iveistep=vsmjf.veistep(*,ispin) else $
    iveistep=vsmjf.veistep
  wlo=where(volt_en(iveistep,/en) lt volt_en_strl(ivstep,/en))
  whi=where(volt_en(iveistep,/en) gt volt_en_strl(ivstep,/en))
  step0=wlo(n_elements(wlo)-1)
  step1=whi(0)

;get veis pitch angle 
;get veis unit vectors corresponding to 180 degrees from look directions
  vhat=vsmjf.vunit

;approx transformation from payload to gse
  vhat(*,*,*,1:2)=-vhat(*,*,*,1:2)  

;now plot veis pitch angle distributions for the two energies nearest strahl en
xtitle=strarr(2)
xtitle(0)=yrmoda(timpb5)+'  '+string(timpb5(1),format='(i3)') +' '+hms
xtitle(1)='electron pitch angle'
vstps=[step0,step1]

for j=0,1 do begin

  x=fltarr(vsmjf.n_vdets,vsmjf.n_sectors)
  c=fltarr(vsmjf.n_vdets,vsmjf.n_sectors)
  z=fltarr(vsmjf.n_vdets,vsmjf.n_sectors)
  stp=vstps(j)
  for jdet=0,vsmjf.n_vdets-1 do begin
    vparahat=vhat(jdet,stp,*,0)*bhat(0)+$
           vhat(jdet,stp,*,1)*bhat(1)+$
           vhat(jdet,stp,*,2)*bhat(2)
    vparahat=reform(vparahat)
    x(jdet,*)=acos(vparahat)/!dtor  ;pitch angle
    c(jdet,*)=vsmjf.veis(jdet,stp,*,ispin)
    z(jdet,*)=vsmjf.fveis(jdet,stp,*,ispin) 
  endfor

  ;exclude sun glint points
  wnotglnt=where(z gt 0.)
  z=z(wnotglnt)
  c=c(wnotglnt)
  x=x(wnotglnt)
 
 sortx=sort(x)
 x=x(sortx)
 c=c(sortx)
 z=z(sortx)

  ytitle=replicate('f_veis',2)
  ylog=1
  
  plot,x,z,/nodata,ylog=ylog,$
      title='VEIS '+string(volt_en(vsmjf.veistep(stp),/en),$
          format='(i5)')+'ev'+$
          '  spn '+string(ispin,format='(i1)'),$          
      xrange=xrange,xticks=xticks,xstyle=1,xtitle=xtitle(j),$
      xtickformat=xtickformat,xminor=xminor,charsize=2.25,$
      yrange=yrange,yticks=yticks,ystyle=1,$
      ytitle=ytitle(j)
  oplot,x,z,psym=4,symsize=0.50,color=175

  ;fit parabola through logf
  logz=alog10(z)
  p=x-90.
  wt=c  ;replicate(1.,n_elements(z))  
  a=fltarr(3)
  a(0)=total(z)/n_elements(z)
  logzfit=curvefit(p,logz,wt,a,siga,function_name='funct3')
  zfit=10.^logzfit
  oplot,x,zfit
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
