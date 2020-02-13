pro linfunct,x,a,f,pder
  f=a(0)+a(1)*x
    if n_params() ge 4 then $
    pder=[[replicate(1.,n_elements(x))],[x]]
end


pro gaussfunct,x,a,f,pder
  arg1=(x/a(1))^2
  arg2=(x/a(3))^2
  f1=2*a(0)/(a(1)*sqrt(2*!pi))*exp(-arg1/2)
  f2=2*a(2)/(a(3)*sqrt(2*!pi))*exp(-arg2/2)
  f=f1+f2
    if n_params() ge 4 then $
    pder=[[f1/a(0)],[-f1/a(1)*(1.+arg1)],[f2/a(2)],[-f2/a(3)*(1.+arg2)]]
end


pro parabfunct,x,a,f,pder
  f=a(0)+a(1)*x*x
    if n_params() ge 4 then $
    pder=[[replicate(1.,n_elements(x))],[x*x]]
end


pro strl_pit,ff=ff

;plots strahl counts data

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
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

!p.multi=[0,2,3,0,0]


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
bhat=b/sqrt(total(b^2))
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
;stop,'p1'
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

xtitle=strarr(nstrldets)
xtitle(nstrldets-4)=yrmoda(timpb5)+'  '+string(timpb5(1),format='(i3)') +' '+hms
xtitle(nstrldets-3)=yrmoda(timpb5)+'  '+string(timpb5(1),format='(i3)') +' '+hms
xtitle(nstrldets-2)='electron pitch angle'
xtitle(nstrldets-1)='electron pitch angle'
ytitle=replicate('counts',nstrldets)

print,' '
print,string(volt_en_strl(ivstep,/en),format='(i5)')+'ev'
print,'det, chisq, cntsmax(meas), ptch, ctsmax(fit), fmax, wdth, area'

for i=0,n_elements(detset)-1 do begin
  jstrldet=detset(i)
  x=reform(strpa(jstrldet,*))
    
  if keyword_set(ff) ne 0 then begin
    yrange=fltarr(2)
    z=vsmjf.fstrl(*,*,ispin)
    yrange(0)=10.^fix(alog10(min( z(where(z ne 0))))-1.)
    yrange(1)=10.^fix(alog10(max(z(where(z ne 0)))))
    yticks=alog10(yrange(1))-alog10(yrange(0))
    y=reform(vsmjf.fstrl(jstrldet,*,ispin))
    ytitle=replicate('f',nstrldets)
    ylog=1
  endif else begin
    ;yrange=[0,4096]
    yrange=[0,1.1*max(dcomp_tbl( vsmjf.strl(*,*,ispin) ))]
    y=reform(dcomp_tbl( vsmjf.strl(jstrldet,*,ispin))) 
    yticks=2
    ytitle=replicate('counts',nstrldets)
    ylog=0
  endelse

  plot,x,y,/nodata,ylog=ylog,$
    title='STRAHL det '+string(jstrldet,format='(i2)')+$
          '  levl '+string(ivstep,format='(i2)')+$
          string(volt_en_strl(ivstep,/en),$
          format='(i5)')+'ev'+$
          '  spn '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtitle=xtitle(jstrldet),$
    xtickformat=xtickformat,xminor=xminor,charsize=2.25,$
    yrange=yrange,yticks=yticks,ystyle=1,$
    ytitle=ytitle(jstrldet)
  oplot,x,y,psym=4,symsize=0.50,color=clr(hardcopy)


;do fitting
  fitype=2
  ycounts=reform(dcomp_tbl( vsmjf.strl(jstrldet,*,ispin)))
  xpitch=reform(strpa(jstrldet,*))

  pitchfitrange=30.
  pitchmin=15.
  wp0=where(xpitch le pitchmin)
  wp180=where(xpitch ge 180.-pitchmin and xpitch le 180.)

  if wp0(0) ne -1 then begin    ;0 degree pitch strahl sector
    if wp0(0) lt vsmjf.n_strphis/2 then $
      indx=indgen(vsmjf.n_strphis/2) else $
      indx=vsmjf.n_strphis/2+indgen(vsmjf.n_strphis/2)
    ptch=float(xpitch(indx))
    cnts=float(ycounts(indx))
    w0=where(cnts eq 0)
    if w0(0) ne -1 then cnts(w0)=0.5
    wpitchfitrange=where(ptch le pitchfitrange)
    xinp=ptch(wpitchfitrange)
    yinp=cnts(wpitchfitrange)
    sortx=sort(xinp)
    xinp=xinp(sortx)
    yinp=yinp(sortx)

    
    case fitype of

    0: begin          ;parabolic fit
      a=[max(yinp),0.]
      wt=1./yinp   ;replicate(1.,n_elements(yinp))          
      yfit=$
        curvefit(xinp,yinp,wt,a,siga,function_name='parabfunct',$
        iter=iter0,chi2=chisq)
      print,'0pitch sector: det ',jstrldet,'  chisq ',chisq
      xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
      parabfunct,xcurve,a,ycurve
    endcase

    1: begin    ;sum of gaussian fit
      a=[10*max(yinp),10.,max(yinp),50.]
      wt=1./yinp   ;replicate(1.,n_elements(yinp))          
      yfit=$
        curvefit(xinp,yinp,wt,a,siga,function_name='gaussfunct',$
        iter=iter0,chi2=chisq)
      print,'0pitch sector: det ',jstrldet,'  chisq ',chisq
      xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
      gaussfunct,xcurve,a,ycurve
    endcase

    2: begin          ;parabolic in log fit
      logyinp=alog10(yinp)
      a=[max(logyinp),0.] 
      wt=yinp   ;replicate(1.,n_elements(yinp))                
      yfit=$
        curvefit(xinp,logyinp,wt,a,siga,function_name='parabfunct',$
        iter=iter0,chi2=chisq)
      xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
      parabfunct,xcurve,a,logycurve
      ycurve=10.^(logycurve)
    endcase

    3: begin         ;linear in log
      logyinp=alog10(yinp)
      mx=max(logyinp,mxind)
      mn=min(logyinp,mnind)
      a=[mx,(mx-mn)/(xinp(mxind)-xinp(mnind))]
      wt=yinp   ;replicate(1.,n_elements(yinp))                
      yfit=$
        curvefit(xinp,logyinp,wt,a,siga,function_name='linfunct',$
        iter=iter0,chi2=chisq)
      ;print,'0pitch sector: det ',jstrldet,'  chisq ',chisq
      xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
      linfunct,xcurve,a,logycurve
      ycurve=10.^(logycurve)
    endcase
    endcase

    ;find width at half-maximum
      ymax=ycurve(0)
      nc=2*fix(pitchfitrange)+1
      xc=findgen(nc)
      parabfunct,xc,a,logyc
      yc=10.^(logyc)
      mn=min(abs(yc-0.5*yc(0)),mindx)
      yctsmax=yc(0)
      yfmax=vsmjf.strl_cts_factor(jstrldet,ispin)*yctsmax
      wdthhalfmax=xc(mindx)
      area=yfmax*wdthhalfmax

    if keyword_set(ff) ne 0 then $
      ycurve=vsmjf.strl_cts_factor(jstrldet,ispin)*ycurve    
    oplot,xcurve,ycurve
   
    cntsmax=max(cnts,mxindx)
    
    print,'0:', jstrldet,chisq, cntsmax,ptch(mxindx),yctsmax,yfmax,$
      wdthhalfmax,area,format='(a5,i3,f12.5,3f8.1,e12.4,f8.1,e12.4)'

  endif


  if wp180(0) ne -1 then begin    ;180 degree pitch strahl sector
    if wp180(0) lt vsmjf.n_strphis/2 then $
      indx=indgen(vsmjf.n_strphis/2) else $
      indx=vsmjf.n_strphis/2+indgen(vsmjf.n_strphis/2)
    ptch=180.-float(xpitch(indx))
    cnts=float(ycounts(indx))
    w0=where(cnts eq 0)
    if w0(0) ne -1 then cnts(w0)=0.5
    wpitchfitrange=where(ptch le pitchfitrange)
    xinp=ptch(wpitchfitrange)
    yinp=cnts(wpitchfitrange)

    sortx=sort(xinp)
    xinp=xinp(sortx)
    yinp=yinp(sortx)

    case fitype of

    0: begin          ;parabolic fit
      a=[max(yinp),0.]
      wt=1./yinp   ;replicate(1.,n_elements(yinp))          
      yfit=$
        curvefit(xinp,yinp,wt,a,siga,function_name='parabfunct',$
        iter=iter0,chi2=chisq)
      print,'180pitch sector: det ',jstrldet,'  chisq ',chisq

      xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
      parabfunct,xcurve,a,ycurve
    endcase

    1: begin    ;sum of gaussian fit
      a=[10*max(yinp),10.,max(yinp),50.]
      wt=1./yinp   ;replicate(1.,n_elements(yinp))          
      yfit=$
        curvefit(xinp,yinp,wt,a,siga,function_name='gaussfunct',$
        iter=iter0,chi2=chisq)
      print,'180pitch sector: det ',jstrldet,'  chisq ',chisq
      xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
      gaussfunct,xcurve,a,ycurve
    endcase

    2: begin          ;parabolic in log fit
      logyinp=alog10(yinp)
      a=[max(logyinp),0.] 
      wt=yinp  ;replicate(1.,n_elements(yinp))                
      yfit=$
        curvefit(xinp,logyinp,wt,a,siga,function_name='parabfunct',$
        iter=iter0,chi2=chisq)
      xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
      parabfunct,xcurve,a,logycurve
      ycurve=10.^(logycurve)
    endcase

    3: begin         ;linear in log
      logyinp=alog10(yinp)
      mx=max(logyinp,mxind)
      mn=min(logyinp,mnind)
      a=[mx,(mx-mn)/(xinp(mxind)-xinp(mnind))]
      wt=yinp   ;replicate(1.,n_elements(yinp))                
      yfit=$
        curvefit(xinp,logyinp,wt,a,siga,function_name='linfunct',$
        iter=iter0,chi2=chisq)
      print,'180pitch sector: det ',jstrldet,'  chisq ',chisq
      xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
      linfunct,xcurve,a,logycurve
      ycurve=10.^(logycurve)
    endcase

    endcase

    ;find width at half-maximum
      ymax=ycurve(0)
      nc=2*fix(pitchfitrange)+1
      xc=findgen(nc)
      parabfunct,xc,a,logyc
      yc=10.^(logyc)
      mn=min(abs(yc-0.5*yc(0)),mindx)
      yctsmax=yc(0)
      yfmax=vsmjf.strl_cts_factor(jstrldet,ispin)*yctsmax
      wdthhalfmax=xc(mindx)
      area=yfmax*wdthhalfmax
      
    xcurve=180.-xcurve
    if keyword_set(ff) ne 0 then $
      ycurve=vsmjf.strl_cts_factor(jstrldet,ispin)*ycurve    
    oplot,xcurve,ycurve

    cntsmax=max(cnts,mxindx)
    
    print,'180:', jstrldet,chisq, cntsmax,ptch(mxindx),yctsmax,yfmax,$
      wdthhalfmax,area,format='(a5,i3,f12.5,3f8.1,e12.4,f8.1,e12.4)'

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
