pro strl_phi,ff=ff

;plots strahl counts data

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common detset,detseta,detsetb,detset
common magstuff,magfile,tpb5,bgse
common swestuff,swest
common wstuff,wst

hcopy=wst.hardcopy & nspins=swest.nspins & ispin=swest.ispinbl
nstrldets=swest.nstrldets & nstrlsteps=swest.nstrlsteps
istrldet=swest.istrldet & istrlstep=swest.istrlstep

wst.hardcopy=0

if keyword_set(strldet0) eq 0 then strldet0=0

yn=['no','yes']
clr=[125,125]

start:

if keyword_set(hcopy) eq 0 then hcopy=0
if hcopy then begin 
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.,filename=pflnm
endif


!p.multi=[0,2,3,0,0]


;x=payloadtogse(vsmjf.phistrl,/phi)
if vsmjf.scimode eq 6 then x=360.- vsmjf.phistrl(*,ispin) else $
   x=360.- vsmjf.phistrl ;approx gse look direction 

xrange=[0,360]
xticks=4
xtickformat='(i3)'

;get strahl hv step
  ivstep=vsmjf.strlstep(ispin)

;print,energy and velocity steps
   print,' '
   print,'Strahl step, velocity, energy :'
   print,ivstep,$
             volt_en_strl(ivstep,/vel),$
             volt_en_strl(ivstep,/en)


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

phimag=phi_bpos

xtitle=strarr(nstrldets)
xtitle(nstrldets-4)=yrmoda(timpb5)+'  '+string(timpb5(1),format='(i3)') +' '+hms
xtitle(nstrldets-3)=yrmoda(timpb5)+'  '+string(timpb5(1),format='(i3)') +' '+hms
xtitle(nstrldets-2)='look phi, mag phi (gse)'
xtitle(nstrldets-1)='look phi, mag phi (gse)'


;for jstrldet=strldet0,nstrldets-1,2 do begin 
for i=0,n_elements(detset)-1 do begin
  jstrldet=detset(i)
  
  if keyword_set(ff) ne 0 then begin
    yrange=fltarr(2)
    z=vsmjf.fstrl(*,*,ispin)
    yrange(0)=10.^fix(alog10(min( z(where(z ne 0))))-1.)
    yrange(1)=10.^fix(alog10(max(z(where(z ne 0)))))
    yticks=alog10(yrange(1))-alog10(yrange(0))
    y=vsmjf.fstrl(jstrldet,*,ispin)
    ytitle=replicate('f',nstrldets)
    ylog=1
  endif else begin
    ;yrange=[0,4096]
    yrange=[0,1.1*max(dcomp_tbl( vsmjf.strl(*,*,ispin) ))]
    y=dcomp_tbl( vsmjf.strl(jstrldet,*,ispin) ) 
    yticks=2
    ytitle=replicate('counts',nstrldets)
    ylog=0
  endelse
  plot,x,y,/nodata,ylog=ylog,$
    title='det '+string(jstrldet,format='(i2)')+'    '+$
          '  levl '+string(ivstep,format='(i2)')+$
          string(volt_en_strl(ivstep,/en),$
          format='(i5)')+'ev'+$
          '  spn '+string(ispin,format='(i1)'),$          
    xrange=xrange,xticks=xticks,xstyle=1,xtitle=xtitle(jstrldet),$
    xtickformat=xtickformat,charsize=2.25,$
    yrange=yrange,yticks=yticks,ystyle=1,$
    ytitle=ytitle(jstrldet)
  oplot,x,y,psym=4,symsize=0.50,color=clr(hcopy)
  phimaglook=phimag+180. & if phimaglook gt 360 then phimaglook=phimaglook-360.
  oplot,[phimaglook,phimaglook],yrange,color=clr(hcopy),linestyle=2
  oplot,[phimag,phimag],yrange,color=clr(hcopy)
endfor

if hcopy then begin   
  device,/close 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  !p.color=125
  hcopy=0
  goto,start
endif

!p.multi=0

end
