;=================== img_fp_strl =========================================

pro img_fp_strl, f, u, t, rlbl=rlbl, zmn=mn,zmx=mx,pos=posit,  $
  ytitle=labl, ytickv=utickv, ytickn=ytickn,  xtickv=xtickv,$
  xtickn=xtickn, xticks=xticks, xtitle=xlabl, xminor=xminor,title=title,$
  subtitle=subtitle,charsize=charsize,$
  charthick=charthick,xcharsize=xcharsize,n_colors=n_colors,$
  ximsize=ximsize,yimsize=yimsize,xp=xp,yp=yp,sym=sym,hardcopy=hardcopy,$
  wdth=wdth,fmn=fmn,fmx=fmx,onecount=onecount

common wstuff,wst
common swestuff,swest
common sharewidg,wa
common sharewidgd,wd

 
if keyword_set(hardcopy) eq 0 then hardcopy=0 else hardcopy=1
clr=[!p.color,!p.color]

if keyword_set(n_colors) eq 0 then n_colors=!d.table_size-1


;---------------------- some preliminaries ---------------------------------

sizf=size(f)
  
;set up color scale ie array
  yc=findgen(n_colors) & xc=findgen(2)
  zc=byte((intarr(100)+1)#yc)

start:
;--------------------- plot image -------------------------------------------

if hardcopy eq 0 then begin   ;no hardcopy

;---------- determine plot axes position for color scale ---------------------

;get position of plot window in device pixels
  px=[posit(2)+0.06,posit(2)+0.08]*ximsize  
  py=[posit(1),posit(3)]*yimsize

;find range of data in position coordinates
  clraxis=[float(fix(mn))-1.,float(fix(mx))]     

  dpy=fltarr(2)  
  dpy(0)=py(0)+(py(1)-py(0))*(mn-clraxis(0))/(clraxis(1)-clraxis(0))
  dpy(1)=py(0)+(py(1)-py(0))*(mx-clraxis(0))/(clraxis(1)-clraxis(0))


;get desired size of image in pixels   
  sx=px(1)-px(0)+1   
  sy=dpy(1)-dpy(0)+1

;get size of original image, sz(1)= # cols, sz(2)= # rows
  sz=size(zc) 

;get new, resized color scale image
  z_new=congrid(zc,sx,sy)

;size of new, resized color scale image
  sz_new=size(z_new) 



;------------ determine plot axes position for data ---------------------------

;get position of plot window in device pixels
  pt=[posit(0),posit(2)] *ximsize
  pp=[posit(1),posit(3)] *yimsize

;get desired size of image in pixels
  st=pt(1)-pt(0)+1   
  sp=pp(1)-pp(0)+1

;get size of original image, sz(1)= # cols, sz(2)= # rows
  szbyt=size(f) 

;get new, resized data image
  f_new=congrid(f,st,sp)

;size of new, resized data image
  sf_new=size(f_new) 


;--------------- plot color scale --------------------------------------------
;erase

;color scale axes
  pos_dev_clrsclaxis=[px(0),py(0),px(1),py(1)]
  pos_dev_clrscl=[px(0),dpy(0),px(0)+sz_new(1)-1,dpy(0)+sz_new(2)-1]

;display new, resized color scale image
  tv,z_new,px(0),dpy(0)

  ycrange=10.^clraxis 
  plot,xc,ycrange,/nodata,/ylog,$
    position=pos_dev_clrsclaxis,$
   /device,ystyle=4,xstyle=4,/noerase,$
   charthick=charthick,charsize=charsize

  if keyword_set(rlbl) ne 0 then yctitle=rlbl else yctitle='' 
  axis,yaxis=1,yrange=ycrange,ytitle=yctitle,$
     charthick=charthick,charsize=charsize,yticklen=-0.75


;mnct=10.^(mn-onecount)
;mxct=10.^(mx-onecount)
;hlfct=mxct/2
;hlf=alog10(hlfct)+onecount

;xyouts,pos_dev_clrsclaxis(0),$
;  pos_dev_clrsclaxis(1)+(mn-clraxis(0))*$
;  (pos_dev_clrsclaxis(3)-pos_dev_clrsclaxis(1))/$
;  (clraxis(1)-clraxis(0)),$
;  string(mnct,format='(i4)')+'ct',/device,alignment=1.0

;xyouts,pos_dev_clrsclaxis(0),$
;  pos_dev_clrsclaxis(1)+(hlf-clraxis(0))*$
;  (pos_dev_clrsclaxis(3)-pos_dev_clrsclaxis(1))/$
;  (clraxis(1)-clraxis(0)),$
;  string(hlfct,format='(i4)'),/device,alignment=1.0
;  string(hlfct,format='(i4)')+'ct',/device,alignment=1.0
  
;xyouts,pos_dev_clrsclaxis(0),$
;  pos_dev_clrsclaxis(1)+(mx-clraxis(0))*$
;  (pos_dev_clrsclaxis(3)-pos_dev_clrsclaxis(1))/$
;  (clraxis(1)-clraxis(0)),$
;  string(mxct,format='(i4)'),/device,alignment=1.0
;  string(mxct,format='(i4)')+'ct',/device,alignment=1.0


;----------------------- plot data -------------------------------------------

;get data axes posiions scaled to size of new image
  pos_dev_data=[posit(0)*!d.x_vsize,pp(0),posit(2)*!d.x_vsize,pp(0)+sf_new(2)-1]
  yticks=n_elements(utickv)-1

;display new, resized data image
  tv,f_new,pt(0),pp(0) 

;find contour at half-maximum
    cl=bytscl([alog10(0.5)+fmx],min=mn,max=mx,top=n_colors-1) 
    clabl=['0.5']
    x=t#replicate(1.,n_elements(u))
    y=replicate(1.,n_elements(t))#u 
    wcl=where(f ge cl(0))
    if wcl(0) ne -1 then begin
      xmn=min(x(wcl),max=xmx)
      ymn=min(y(wcl),max=ymx)
      ;wdth=sqrt((xmx-xmn)^2+(ymx-ymn)^2)
      wdth=(abs(xmx-xmn)+abs(ymx-ymn) )/2
      title=title+'  w='+string(wdth,format='(f4.1)')+'deg'
    endif
 

;data axes
  plot,t,u,/nodata,position=pos_dev_data,$
    xrange=[t(0),t(n_elements(u)-1)],xticks=xticks,xtickv=xtickv,xtitle=xlabl,$
    subtitle=subtitle,$
    xtickname=xtickn,xticklen=-.01,xminor=2,yticklen=-.02,$
    yrange=[u(0),u(n_elements(u)-1)],$
    yticks=yticks,yminor=1,ytickv=utickv,ytitle=labl,xstyle=1,$
    ystyle=1,/device,/noerase,title=title,ytickname=ytickn,$
    charthick=charthick,charsize=charsize,xcharsize=xcharsize

if keyword_set(wdth) ne 0 then  $
  contour,f,x,y,levels=cl,c_annotation=clabl,/overplot    

  if keyword_set(xp) ne 0 then begin 
    if sym eq '+B' then oplot,[xp,xp],[yp,yp],psym=1,symsize=3,thick=3
    if sym eq '-B' then begin
      usersym,[-4,+4],[0,0],thick=3
      oplot,[xp,xp],[yp,yp],psym=8
    endif
  endif

endif else begin    ;make hardcopy 



;---------------------------- color scale ----------------------------------

;get position of color scale plot window in normal coords
  px=[posit(2)+0.10,posit(2)+0.12]    
  py=[posit(1),posit(3)]

;find range of data in position coordinates
  clraxis=[float(fix(mn))-1.,float(fix(mx))]     

  dpy=fltarr(2)  
  dpy(0)=py(0)+(py(1)-py(0))*(mn-clraxis(0))/(clraxis(1)-clraxis(0))
  dpy(1)=py(0)+(py(1)-py(0))*(mx-clraxis(0))/(clraxis(1)-clraxis(0))

;plot color scale image
  tv,zc,px(0),dpy(0),xsize=px(1)-px(0),ysize=dpy(1)-dpy(0),/norm

;plot color scale axes
  ycrange=10.^clraxis
  plot,xc,ycrange,/nodata,position=[px(0),py(0),px(1),py(1)],ystyle=4,xstyle=4,$
    /noerase,charthick=charthick,charsize=charsize,/ylog
  if keyword_set(rlbl) ne 0 then yctitle=rlbl else yctitle=''
  axis,yaxis=1,yrange=ycrange,ytitle=yctitle,$
    charthick=charthick,charsize=charsize,yticklen=-0.75



;mnct=10.^(mn-onecount)
;mxct=10.^(mx-onecount)
;hlfct=mxct/2
;hlf=alog10(hlfct)+onecount

;xyouts,px(0),$
;  py(0)+(mn-clraxis(0))*$
;  (py(1)-py(0))/$
;  (clraxis(1)-clraxis(0)),$
;  string(mnct,format='(i3)')+'ct',/norm,alignment=1.0

;xyouts,px(0),$
;  py(0)+(hlf-clraxis(0))*$
;  (py(1)-py(0))/$
;  (clraxis(1)-clraxis(0)),$
;  string(hlfct,format='(i3)')+'ct',/norm,alignment=1.0

;xyouts,px(0),$
;  py(0)+(mx-clraxis(0))*$
;  (py(1)-py(0))/$
;  (clraxis(1)-clraxis(0)),$
;  string(mxct,format='(i3)')+'ct',/norm,alignment=1.0


;--------------------------- data -------------------------------------------

;get position of data plot window in normal coords
   pt=[posit(0),posit(2)] 
   pp=[posit(1),posit(3)]
   
;plot data image
   tv,f,pt(0),pp(0),xsize=pt(1)-pt(0),ysize=pp(1)-pp(0),/norm


;find contour at half-maximum
    cl=bytscl([alog10(0.5)+mx],min=mn,max=mx,top=n_colors-1) 
    clabl=['0.5']
    x=t#replicate(1.,n_elements(u))
    y=replicate(1.,n_elements(t))#u 
    wcl=where(f ge cl(0))
    if wcl(0) ne -1 then begin
      xmn=min(x(wcl),max=xmx)
      ymn=min(y(wcl),max=ymx)
      ;wdth=sqrt((xmx-xmn)^2+(ymx-ymn)^2)
      wdth=(abs(xmx-xmn)+abs(ymx-ymn) )/2
      title=title+'  w='+string(wdth,format='(f4.1)')+'deg'
    endif

;plot data axes
   yticks=n_elements(utickv)-1
   plot,t,u,/nodata,position=posit,$
     xrange=[t(0),t(n_elements(u)-1)],xticks=xticks,xtickv=xtickv,xtitle=xlabl,$
     subtitle=subtitle,$
     xtickname=xtickn,xticklen=-.02,yticklen=-.02,$
     yrange=[u(0),u(n_elements(u)-1)],$
     yticks=yticks,yminor=1,ytickv=utickv,$
     ytitle=labl,xstyle=1,$
     ystyle=1,/noerase,ytickname=ytickn,title=title,$
     charthick=charthick,charsize=charsize,xcharsize=xcharsize


if keyword_set(wdth) ne 0 then  $
  contour,f,x,y,levels=cl,c_annotation=clabl,/overplot 

  if keyword_set(xp) ne 0 then begin 
    if sym eq '+B' then oplot,[xp,xp],[yp,yp],psym=1,symsize=3,thick=3
    if sym eq '-B' then begin
      usersym,[-4,+4],[0,0],thick=3
      oplot,[xp,xp],[yp,yp],psym=8
    endif
  endif


 endelse ;end making hardcopy this panel



end



pro strl_moment,g,y,x,w

xp=x#replicate(1.,n_elements(y))
yp=replicate(1.,n_elements(x))#y

xav=total(g*xp)/total(g)
yav=total(g*yp)/total(g)

wsq=total(g*((xp-xav)^2+(yp-yav)^2))/total(g)
w=sqrt(wsq)

end


;==================== parabfunct =======================================
pro parabfunct,x,a,f,pder
  f=a(0)+a(1)*x*x
    if n_params() ge 4 then $
    pder=[[replicate(1.,n_elements(x))],[x*x]]
end


;================== strl_phth ============================================

pro strl_phth,phth_image=phth_image,pitch=pitch

;plots strahl counts data

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common magstuff,magfile,tpb5,bgse
common swestuff,swest
common wstuff,wst
common sharewidgd,wd

nspins=swest.nspins & ispin=swest.ispinbl
nstrldets=swest.nstrldets & nstrlsteps=swest.nstrlsteps
istrldet=swest.istrldet & istrlstep=swest.istrlstep

if keyword_set(wst.hardcopy) eq 0 then hardcopy=0 else hardcopy=wst.hardcopy

;get strahl hv step
  ivstep=vsmjf.strlstep(ispin)

;print,energy and velocity steps
   print,' '
   print,'Strahl step, velocity, energy :'
   print,ivstep,$
             volt_en_strl(ivstep,/vel),$
             volt_en_strl(ivstep,/en)

;log oncount phase density
  onecount=alog10(min(vsmjf.strl_cts_factor(*,ispin)))

timpb5=vsmjf.pb5tim_vsbl(*,ispin)
sec=double(timpb5(2)/1000)
hour_hms,sec/3600.d,hms

;get mfi mag field
if swest.mag_3s_kp eq 1 then begin               ;use mfi 3s mag field
  get_3smag,timpb5,magtpb5,b,phi_bpos,the_bpos
endif else if swest.mag_3s_kp eq 2 then begin    ;use mfi kp mag field
  get_kpmag,timpb5,b,phi_bpos,the_bpos 
endif else begin                                 ;no mag data
  phi_bpos=0 & the_bpos=0 & b=[-1.,0.,0.]
endelse  
phi_bneg=(phi_bpos+180.) mod 360.
the_bneg=-the_bpos

widget_control,wd.field(5),set_value=$
  string([phi_bpos,the_bpos],format='(i3,2x,i3)')
  
x_im_sz=wd.win_xsize(0)
y_im_sz=wd.win_ysize(0)
pos,2,posn,xoff=0.15,ysep=0.1
erase

n_strdets=vsmjf.n_strdets
n_strphis=vsmjf.n_strphis

;transform unit vectors to gse
atindx=fix((vsmjf.sec)/600)  ;atfile record number from 10 minute index
  wc_gse=dblarr(3)
  vunit_gse=dblarr(n_strdets,n_strphis,3)
  if vsmjf.scimode eq 6 then $
        vunitstrl=reform(vsmjf.vunitstrl(*,*,*,ispin))  $
  else vunitstrl=vsmjf.vunitstrl 
  if atfile ne '' then begin   
    for i=0,n_strdets-1 do for j=0,n_strphis-1 do begin
      payload_to_gse,$
      [vunitstrl(i,j,0),vunitstrl(i,j,1),vunitstrl(i,j,2)],$
      [gse_ra(atindx),gse_dec(atindx)],pay_gse_mtx,wc_gse  
      vunit_gse(i,j,*)=wc_gse
    endfor  
  endif else begin 
      print,'no attitude data; using 180 deg rotation about x-axis instead'
      vunit_gse(*,*,0)=vunitstrl(*,*,0)
      vunit_gse(*,*,1)=-vunitstrl(*,*,1)
      vunit_gse(*,*,2)=-vunitstrl(*,*,2)
  endelse
  
   
    
    

;get particle pitch angles
   bhat=b/sqrt(total(b^2))    
   vsparahat=vunit_gse(*,*,0)*bhat(0)+vunit_gse(*,*,1)*bhat(1)+$
        vunit_gse(*,*,2)*bhat(2)
   pa=acos(vsparahat)/!dtor


;get x,y grid axes for gse electron velocity angles, azimuth and co-latitude
  phistrl=atan(vunit_gse(*,*,1),vunit_gse(*,*,0))/!dtor   
  wl0=where(phistrl le 0)
  if wl0(0) ne -1 then phistrl(wl0)=360.+phistrl(wl0)
  thestrl=90.-acos(vunit_gse(*,*,2))/!dtor

  cstrl=vsmjf.strl(*,*,ispin)
  fstrl=vsmjf.fstrl(*,*,ispin)

;set saturated values to 0 
  for i=0,n_elements(cstrl)-1 do begin
    wcmx=where(cstrl ge comp_tbl(wst.newstrlmax))
    if wcmx(0) ne -1 then fstrl(wcmx)=0
  endfor
  
  
  if swest.strlsunmask then begin  ;mask out sun
    phsunindx=where(phistrl(n_strdets/2,*) gt swest.strlphsun(0) and $
                  phistrl(n_strdets/2,*) lt swest.strlphsun(1),nphsunindx)
                 
    thsunindx=indgen(n_strdets) 
    nthsunindx=n_elements(thsunindx)
    if nphsunindx gt 1 then $
      fstrl(thsunindx(0):thsunindx(nthsunindx-1),$
      phsunindx(0):phsunindx(nphsunindx-1))=0 
  endif                  
  
  subtitle= yrmoda(timpb5)+'  '+string(timpb5(1),format='(i3)') +' '+hms+$
         '  spn '+string(ispin,format='(i1)') 
         
  title=string(volt_en_strl(ivstep,/en),format='(i5)')+'ev'+ $
    '  STRAHL electrons'
  xtck=7
  ytck=13 

  w=where(fstrl ne 0)
  if w(0) eq -1 then return
  mn=min(alog10(fstrl(w)),max=mx)

start:


if hardcopy then begin
  print,' ' & print,'making hardcopy..... '
  clrtbl_indx,/hardcopy 
  set_plot,'ps',/interpolate
  pltfil=getenv('IDLSAV')+wst.print_flnm 
  print,'pltfil',pltfil
  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5,/color,$
     filename=pltfil;,bits_per_pixel=8
  ;device,/inches,/landscape,filename=pltfil
endif


;there are two strahl sampling sectors, one measuring electrons moving
;  AWAY from the Sun and the other, electrons moving TOWARD the Sun
  strl_sector_0 = indgen(n_strphis/2) 
  strl_sector_1 = n_strphis/2+indgen(n_strphis/2)

;determine if strl_sector_0 or strl_sector_1 measures electrons AWAY from Sun
;(choose one strahl detector (det5) in middle of strl_sector_1 to test)

  ;if phistrl(5, strl_sector_1(n_elements(strl_sector_1)/2)) ge 90. and $
  ;   phistrl(5, strl_sector_1(n_elements(strl_sector_1)/2)) le 270. then begin
if phistrl(n_strdets/2, strl_sector_1(n_elements(strl_sector_1)/2)) ge 90. and $
   phistrl(n_strdets/2, strl_sector_1(n_elements(strl_sector_1)/2)) le 270. then begin
       el_awayfromsun=strl_sector_1
       el_towrdthesun=strl_sector_0
  endif else begin
    el_awayfromsun=strl_sector_0
    el_towrdthesun=strl_sector_1
  endelse 

 
;
;<<<<<<<<<<<<<<<< electron velocity directions AWAY from the Sun >>>>>>>>>>>>>

;NOTE: phistrl = phi of electron VELOCITY, NOT look angle

;---------- get x,y grid axes for electron velocity directions AWAY from the Sun
  ;x=float(reverse(reform(phistrl(n_strdets/2,el_awayfromsun))))
  x=float(reform(phistrl(n_strdets/2,el_awayfromsun)))
  y=float(reverse(reform(thestrl(*,3*n_strphis/4))))
  g=fltarr(n_elements(x),n_elements(y))
  g(*,*)=transpose(fstrl(*,el_awayfromsun))
  ;g=reverse(g)
  g=reverse(g,2)

  p=fltarr(n_elements(x),n_elements(y))
  p(*,*)=transpose(pa(*,el_awayfromsun))
  p=reverse(p)
  p=reverse(p,2)
  f=g

;scale non-zero elements of (log) image array to byte scale
  w=where(g ne 0)
  g(w)=alog10(g(w))
  fmx=max(g(w),min=fmn)    ;find AWAY logf max
  n_colors=!d.table_size-1
  g(w)=bytscl(temporary(g(w)),min=mn,max=mx,top=n_colors-1);scale to clrs

;get mean det separation; gap in theta coverage at spin plane approx thsep wide
  phsep=3.51543
  thsep=(y(n_elements(y)-1)-y(0))/n_elements(y)

;get new theta scale and interpolate to cover gap at spin plane
  nynew=n_elements(y)+1
  ynew=y(0)+indgen(nynew)*thsep
  gnew=fltarr(n_elements(x),n_elements(ynew))
  for i=0,n_elements(x)-1 do gnew(i,*)=interpol(g(i,*),y,ynew)
    
;set plot parameters  
  ymn=min(ynew,max=ymx)
  ;extend plot range by one-half sample separation so that image pixel centers 
  ; correspond to coordinate axes 
    ymn=ymn-thsep/2
    ymx=ymx+thsep/2
  ytickv=ymn+indgen(ytck+1)*(ymx-ymn)/ytck
  ytickn=string(ytickv,format='(f5.1)')
  yrange=[ymn,ymx]

  ;xmn=min(x,max=xmx)
  xmn=x(0)
  xmx=x(n_elements(x)-1)
  ;extend plot range by one-half sample separation so that image pixel centers 
  ; correspond to coordinate axes  
    xmn=xmn-phsep/2
    xmx=xmx+phsep/2   
  xtickv=xmn+indgen(xtck+1)*(xmx-xmn)/xtck
  xtickn=string(xtickv,format='(f5.1)')
  xrange=[xmn,xmx]  

;find direction of field relative to electron moving AWAY from Sun
  if  phi_bpos ge min(x) and phi_bpos le max(x) then begin
    xp=phi_bpos
    yp=the_bpos
    symp='+B'
    xq=phi_bneg
    yq=the_bneg
    symq='-B'
  endif else if phi_bneg ge min(x) and phi_bneg le max(x) then begin
    xp=phi_bneg
    yp=the_bneg
    symp='-B'
    xq=phi_bpos
    yq=the_bpos
    symq='+B'
  endif else begin
    xp=0
    yp=0
    symp=0
    xq=0
    yq=0
    symq=0
  endelse

dogaussfit=0
if dogaussfit then begin
  result=gauss2dfit_mod(g,a)
  if a(4) lt n_elements(x) and a(5) lt n_elements(y) then begin
    ph_cntr=x(fix(a(4)))+(a(4)-float(fix(a(4))))*(x(fix(a(4))+1) - x(fix(a(4))))
    ph_sep=x(1)-x(0)
    ph_wdth=a(2)*ph_sep
    th_cntr=y(fix(a(5)))+(a(5)-float(fix(a(5))))*(y(fix(a(5))+1) - y(fix(a(5))))
    th_sep=total(y(indgen(n_elements(y)/2-1)+1)-y(indgen(n_elements(y)/2-1)))/$
      (n_elements(y)/2-1)
    th_wdth=a(3)*th_sep
    wdth=sqrt(ph_wdth^2+th_wdth^2)
    print,'Gaussian fit to image:'
    print,a
    print,'center: phi, theta ',ph_cntr,th_cntr
    print,' width: phi, theta, 2D ',ph_wdth,th_wdth,wdth
  endif
endif

if keyword_set(phth_image) ne 0 then begin

  wdth_away=-1
  img_fp_strl, gnew, reform(ynew), reform(x), zmn=mn, zmx=mx,  pos=posn(*,0),  $
     ytickv=ytickv,ytitle='theta', $
     ytickn=ytickn,$
     xtickv=xtickv, $
     xtickn=xtickn,$
     subtitle=' ',$
     xticks=xtck, xtitle='phi (velocity AWAY from Sun)' ,xminor=1, $
     title=title(0),$
     charsize=1.25,charthick=1.0,xcharsize=1.0,$
     ximsize=x_im_sz,yimsize=y_im_sz,n_colors=n_colors,xp=xp,yp=yp,sym=symp,$
     hardcopy=hardcopy,wdth=wdth_away,fmn=fmn,fmx=fmx,onecount=onecount

  print,' ' & print,'AWAY width at half-max in degrees',wdth_away 
 ;stop  
endif else if keyword_set(pitch) ne 0 then begin

  plot,p,f,/ylog,pos=posn(*,0),title=title(0),charsize=1.25,$
    xrange=[0,180],xstyle=1,xticks=6,xtitle='pitch angle',$
    yrange=[10.^fix(mn-1),10.^fix(mx)],ystyle=1,ytitle='f',$
    psym=4,symsize=0.2,/nodata
  oplot,p,f,psym=4,symsize=0.2,  color=125

  goto,nofit
  ;do pitch angle fit
    pitchfitrange=15.  ;30.
    f=f(w)                  ;w=non-zero f elements
    p=p(w)
    wpitchfitrange=where(p le pitchfitrange,npts)
    if npts lt 7 then goto,nofit
    xinp=p(wpitchfitrange)
    yinp=f(wpitchfitrange)
    sortx=sort(xinp)
    xinp=xinp(sortx)
    yinp=yinp(sortx)
    logyinp=alog10(yinp)
    ;do parabolic in log fit
    a=[max(logyinp),0.] 
    wt=replicate(1.,n_elements(yinp))                
    yfit=$
      curvefit(xinp,logyinp,wt,a,siga,function_name='parabfunct',$
      iter=iter0,chi2=chisq)
    xcurve=[0,0.25*xinp(0),0.50*xinp(0),0.75*xinp(0),xinp]
    parabfunct,xcurve,a,logycurve
    ycurve=10.^(logycurve)
    oplot,xinp,ycurve,color=wst.clr_orange

  nofit:
endif
    
;strl_moment,g,y,x,w
;print,' ' & print,'w  :',w





;<<<<<<<<<<<<<< electron velocity directions TOWARD the Sun >>>>>>>>>>>>>>>>>>>

;----------get x,y grid axes for electron velocity directions TOWARD the Sun
  ;x=reverse(phistrl(n_strdets/2,el_towrdthesun))
  x=float(reform(phistrl(n_strdets/2,el_towrdthesun)))
  ;iclicks_sunpulse=4096 & iclicks_bci=40 
  ;phi_bci=iclicks_bci*360.d0/iclicks_sunpulse   
  ;x=x(0)-indgen(n_strphis/2)*phi_bci
  if total(abs(x)-x) ne 0 then xq=xp-180.
  y=float(reverse(reform(thestrl(*,n_strphis/4))))
  g=fltarr(n_elements(x),n_elements(y))
  g(*,*)=transpose(fstrl(*,el_towrdthesun))
  ;g=reverse(g)
  g=reverse(g,2)

  p=fltarr(n_elements(x),n_elements(y))
  p(*,*)=transpose(pa(*,el_towrdthesun))
  p=reverse(p)
  p=reverse(p,2)
  f=g

;scale non-zero elements of (log) image array to byte scale
  w=where(g ne 0)
  g(w)=alog10(g(w))
  fmx=max(g(w),min=fmn)    ;find TOWARD logf max
  n_colors=!d.table_size-1
  g(w)=bytscl(temporary(g(w)),min=mn,max=mx,top=n_colors-1);scale to clrs


;get mean det separation; gap in theta coverage at spin plane approx thsep wide
  phsep=3.51543
  thsep=(y(n_elements(y)-1)-y(0))/n_elements(y)

;get new theta scale and interpolate to cover gap at spin plane
  nynew=n_elements(y)+1
  ynew=y(0)+indgen(nynew)*thsep
  gnew=fltarr(n_elements(x),n_elements(ynew))
  for i=0,n_elements(x)-1 do gnew(i,*)=interpol(g(i,*),y,ynew)
        
;set plot parameters  
  ymn=min(ynew,max=ymx)
  ;extend plot range by one-half sample separation so that image pixel centers 
  ; correspond to coordinate axes 
    ymn=ymn-thsep/2
    ymx=ymx+thsep/2
  ytickv=ymn+indgen(ytck+1)*(ymx-ymn)/ytck
  ytickn=string(ytickv,format='(f5.1)')
  ;xmn=min(x,max=xmx)
  xmn=x(0)
  xmx=x(n_elements(x)-1)
  ;extend plot range by one-half sample separation so that image pixel centers 
  ; correspond to coordinate axes 
    xmn=xmn-phsep/2
    xmx=xmx+phsep/2

  xtickv=xmn+indgen(xtck+1)*(xmx-xmn)/xtck
  xtickn=string(xtickv,format='(f5.1)')
  xrange=[xmn,xmx]  
  
wdth_toward=-1
if keyword_set(phth_image) ne 0 then begin 
             
  img_fp_strl, gnew, reform(ynew), reform(x), zmn=mn, zmx=mx,  pos=posn(*,1),  $
     ytickv=ytickv,ytitle='theta', $
     ytickn=ytickn,$
     xtickv=xmn+indgen(xtck+1)*(xmx-xmn)/xtck, $
     xtickn=xtickn,$
     subtitle=subtitle(0),$
     xticks=xtck, xtitle='phi (velocity TOWARD the Sun)' ,xminor=1, $
     title=' ',$
     charsize=1.25,charthick=1.0,xcharsize=1.0,$
     ximsize=x_im_sz,yimsize=y_im_sz,n_colors=n_colors,xp=xq,yp=yq,sym=symq,$
     hardcopy=hardcopy,wdth=wdth_toward,fmn=fmn,fmx=fmx,onecount=onecount

  print,'one-count phase density ',onecount
  print,'TOWARD width at half-max in degrees',wdth_toward 


endif else if keyword_set(pitch) ne 0 then $

  plot,p,f,/ylog,pos=posn(*,1),subtitle=subtitle(0),charsize=1.25,/noerase,$
    xrange=[0,180],xstyle=1,xticks=6,xtitle='pitch angle',$
    yrange=[10.^fix(mn-1),10.^fix(mx)],ystyle=1,ytitle='f',$
    psym=4,symsize=0.2,/nodata
 oplot,p,f,psym=4,symsize=0.2,color=125   

if hardcopy then begin
  device,/close
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pltfil
  spawn,wst.print_cmd
  hardcopy=0
  clrtbl_indx
  goto,start
endif

if wst.hardcopy eq 1 then wst.hardcopy=0 

!p.charsize=1.

if vsmjf.scimode eq 6 then begin
  print,'vsmjf.ibci_strl, vsmjf.phistrl for ispin, phistrl(gse) ',ispin
  for i=0,n_elements(reform(vsmjf.phistrl(*,ispin)))-1 do $
   print,vsmjf.ibci_strl(i,ispin),vsmjf.phistrl(i,ispin),phistrl(n_strdets/2,i),$
   max(vsmjf.fstrl(5,i,ispin)+vsmjf.fstrl(6,i,ispin))
 print,vsmjf.suntim_vsbl(ispin)
 print,vsmjf.pb5tim_vsbl(*,ispin) 
 print,recn
endif

end
