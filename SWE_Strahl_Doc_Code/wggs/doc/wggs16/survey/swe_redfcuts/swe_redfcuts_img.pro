pro swe_redfcuts_img,pos=posn,ytitle=ytitle,$
      rlbl=rlbl,$
      xrange=xrange, xtickv=xtickv,xtickn=xtickname,subtitle=subtitle,$
      xticks=xticks, xtitle=xtitle ,xminor=xminor, title=title,$
      charsize=charsize,charthick=charthick,xcharsize=xcharsize

common shared,d
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common swestuff,swest
common wstuff,wst
common sharelevelzero,pltwin_frmt,oplot_sec

idatype=where(d.datype eq 'swe_redfcuts')
  
n_colors=!d.table_size-1
clr_yellow=(90./131.)*n_colors

if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0

;select time interval
redf=d.swe_redfcutsdat(d.ndx(0,idatype):d.ndx(1,idatype))
nx=redf(0).nx
ny=redf(0).ny

gridsize=2*redf(0).vmax*1e-8/(ny-1)
wh=where(redf.vmax*1e-8 ge redf(0).vmax*1e-8 -2*gridsize and $
         redf.vmax*1e-8 le redf(0).vmax*1e-8 +2*gridsize)
if n_elements(wh) ne n_elements(redf) then begin
  print,'vmax changes: ',n_elements(redf)-n_elements(wh)
  redf=redf(wh)
endif

vpara_orig=-redf(0).vmax*1e-8+indgen(nx)*gridsize
vperp_orig=-redf(0).vmax*1e-8+indgen(ny)*gridsize   ;indgen(ny)*gridsize

vmin=swest.vmin_redf  ;0  ;6.0   
vmax=swest.vmax_redf   ;15.0  ;18.0  ;12.0   ;redf(0).vmax*1e-8 
    
wvpara=where(vpara_orig ge vmin and vpara_orig le vmax)
wvperp=where(vperp_orig ge vmin and vperp_orig le vmax)
vpara=vpara_orig(wvpara)
vperp=vperp_orig(wvperp)


smooth=swest.nsmooth_pts_redf

case ytitle of
'redf': begin
     g=transpose(redf.F(wvpara))
     g(where(g ne 0))=alog10(g(where(g ne 0)))
     ytitle=ytitle;+'vpara'  ;': v!m!d#!n'
     y=vpara
     ;--find min,max of redf
     if smooth ne 0 then z=median(g,smooth) else z=g
     fmax=max(z(where(z ne 0.)))
     fmin=min(z(where(z ne 0.)))
   endcase
'fpara': begin
     g=transpose(redf.fpara(wvpara))
     g(where(g ne 0))=alog10(g(where(g ne 0)))
     ytitle=ytitle;+': v!m!d#!n' 
     y=vpara
     ;--find min,max of fpara and fperp
     f=transpose(redf.fperp(wvperp))
     f(where(f ne 0))=alog10(f(where(f ne 0)))
     if smooth ne 0 then z=median(g,smooth) else z=g
     if smooth ne 0 then f=median(f,smooth)
     fmax=max([max(z(where(z ne 0.))),max(f(where(f ne 0.)))])
     fmin=min([min(z(where(z ne 0.))),min(f(where(f ne 0.)))])
   endcase
'fperp': begin
     g=transpose(redf.fperp(wvperp))
     g(where(g ne 0))=alog10(g(where(g ne 0)))
     ytitle=ytitle;+'vperp'  ;': v!m!d#!n'
     y=vperp
     ;--find min,max of fpara and fperp
     f=transpose(redf.fpara(wvpara))
     f(where(f ne 0))=alog10(f(where(f ne 0)))
     if smooth ne 0 then z=median(g,smooth) else z=g
     if smooth ne 0 then f=median(f,smooth)
     fmax=max([max(z(where(z ne 0.))),max(f(where(f ne 0.)))])
     fmin=min([min(z(where(z ne 0.))),min(f(where(f ne 0.)))])
   endcase   
endcase      

x=(redf.ta-d.refsec)/double(3600) 

swest.cntrs_redf=1
c_decade= 0.33  ;0.167 ;0.25;0.50 ;swest.c_decade_redf ;0.125 ;0.75 ;1.0
;prepare contour plot 
if swest.cntrs_redf then begin      
  nc=fix(float(fix(fmax+0.5)-$
           fix(fmin-0.5))/c_decade+1)
  if nc gt 50 then $
    cl=float(fix(fmin-0.5)) + 2*findgen(nc/2) $
  else $ 
    cl=float(fix(fmin-0.5)) + indgen(nc)*c_decade
  c_labels=replicate(0,nc)
endif


;yrange=[y(0),y(n_elements(y)-1)]
yrange=[-vmax,vmax]

yticks=6
noerase=0 
yminor=1

;prepare color image
;scale non-zero elements of image array to byte scale
      f=z 
      mn=min(f(where(f ne 0)),max=mx) 
      w0=where(f eq 0)
      f=bytscl(temporary(f),min=mn,max=mx,top=n_colors-1);scale to  colors
      if w0(0) ne -1 then f(w0)=0
      
;<<<<<<<<<<<<<<<<<<<<<<<<<< NOTE! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>           
;f=image array, dim(xindx,yindx), where
;xindx=time indices corresponding to actual data, no provision for gaps in time
;yindx=en steps for spectrum plot, or pitch angle bins for pitch angle plot
  sizf=size(f)
       
;the set of indices, elaps_spns, corresponds to elapsed time, therefore
;the array, fimg, contains the actual data and null elements in time gaps
    elaps_spns=redf.elapspn 
    fimg=bytarr(max(elaps_spns-elaps_spns(0))+1,sizf(2))
    fimg(elaps_spns-elaps_spns(0),*)=f
    help,f,fimg
    f=fimg
    fimg=0
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

;set up color scale image array
  yc=findgen(n_colors-1) & xc=findgen(2)
  zc=byte((intarr(100)+1)#yc)
  
if wst.hardcopy eq 0 then begin

;---------- determine plot axes position for color scale ---------------------

;get position of plot window in device pixels
  posit=posn
  px=[posit(2)+0.04,posit(2)+0.06]*!d.x_vsize    
  py=[posit(1),posit(3)]*!d.y_vsize

;get desired size of image in pixels   
  sx=px(1)-px(0)+1   
  sy=py(1)-py(0)+1

;get size of original image, sz(1)= # cols, sz(2)= # rows
  sz=size(zc) 

;get new, resized color scale image
  z_new=congrid(zc,sx,sy)

;size of new, resized color scale image
  sz_new=size(z_new) 


;--------------- plot color scale --------------------------------------------

;color scale axes
  ycticks= 3
  ycrange=[mn,mx]
  yctickv=ycrange(0)+indgen(ycticks+1)*(ycrange(1)-ycrange(0))/ycticks
  pos_dev_clrscl=[px(0),py(0),px(0)+sz_new(1)-1,py(0)+sz_new(2)-1]
  plot,xc,[mn,mx],/nodata,$
    position=pos_dev_clrscl,$
   /device,ystyle=4,xstyle=4,/noerase,$
   charthick=charthick,charsize=charsize
  yctitle='f' 

  ytickname=strcompress(string(10.^yctickv,format='(e7.1)')) 
  axis,yaxis=1,yticks=ycticks,yrange=ycrange,ytickv=yctickv,ytitle=yctitle,$
     charthick=charthick,charsize=charsize,ytickname=ytickname

;display new, resized color scale image
  tv,z_new,px(0),py(0) 


;---------- determine plot axes position for data plot ---------------------
        
;get position of plot window in device pixels
  trange=xrange
  posit=posn
  pt=posit(0)*!d.x_vsize+[(x(0)-trange(0)),(x(n_elements(x)-1)-trange(0))]*$
    (posit(2)-posit(0))/(trange(1)-trange(0)) *!d.x_vsize
  pp=[posit(1),posit(3)]*!d.y_vsize 

;get desired size of image in pixels
  st=pt(1)-pt(0)+1   
  sp=pp(1)-pp(0)+1

;get size of original image, sz(1)= # cols, sz(2)= # rows
  szbyt=size(f) 

;get new, resized data image
  f_new=congrid(f,st,sp)

;size of new, resized data image
  sf_new=size(f_new) 


;----------------------- plot data -------------------------------------------

;get data axes posiions scaled to size of new image
  pos_dev_data=$
    [posit(0)*!d.x_vsize,pp(0),posit(2)*!d.x_vsize,pp(0)+sf_new(2)-1]

;display new, resized data image
  tv,f_new,pt(0),pp(0) 

;data axes
if swest.cntrs_redf then begin
contour,z,x,y,levels=cl,max_value=0.,xrange=xrange,yrange=yrange,$
  xstyle=1,ystyle=1,xticks=xticks,$
  xtickv=xtickv,$
  xtickname=xtickname,yticks=yticks,$
  ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
  position=pos_dev_data,/device,title=title,xtitle=xtitle,$
  ytitle=ytitle,/follow,/noerase,$
  subtitle=subtitle,$
  xcharsize=xcharsize,c_labels=c_labels,yminor=yminor,xminor=xminor,$
  xticklen=-.02,yticklen=-.02,$
  charsize=charsize,charthick=charthick,c_colors=[0]
  
  
endif  
  
if swest.cntrs_redf eq 0 then begin
plot,x,y,xrange=xrange,yrange=yrange,$
  xstyle=1,ystyle=1,xticks=xticks,$
  xtickv=xtickv,$
  xtickname=xtickname,yticks=yticks,$
  ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
  position=pos_dev_data,/device,title=title,xtitle=xtitle,$
  ytitle=ytitle,/noerase,$
  subtitle=subtitle,$
  xcharsize=xcharsize,yminor=yminor,xminor=xminor,$
  xticklen=-.02,yticklen=-.02,$
  charsize=charsize,charthick=charthick,/nodata
endif  

  if keyword_set(oplot_sec) ne 0 then begin
    hrday_oplt=(oplot_sec-d.refsec)/3600.d
    oplot,[hrday_oplt,hrday_oplt],[y(0),y(n_elements(y)-1)],$
      color=0;clr_yellow;wst.clr_orange
  endif


endif else begin    ;do hardcopy

;get position of data plot window in normal coords
   trange=xrange
   posit=posn
   pt=posit(0)+[(x(0)-trange(0)),(x(n_elements(x)-1)-trange(0))]*$
    (posit(2)-posit(0))/(trange(1)-trange(0)) 
   pp=[posit(1),posit(3)] 
   
;display new, resized data image
    ;if necessary, rebin time dimension to the pixel resolution
      ntlim=wst.rebin_size_img
      szfin=size(f)
      if szfin(1) gt ntlim then begin  
        sclfctr=fix(szfin(1)/ntlim)+1
        ntmx=sclfctr*(szfin(1)/sclfctr)
        case wst.rebin of
        0: f=congrid(f,ntmx/sclfctr,szfin(2))
        else:
        endcase
      endif
      help,f
  tv,f,pt(0),pp(0),xsize=pt(1)-pt(0),ysize=pp(1)-pp(0),/norm

;data axes
contour,z,x,y,levels=cl,max_value=0.,xrange=xrange,yrange=yrange,$
  xstyle=1,ystyle=1,xticks=xticks,$
  xtickv=xtickv,$
  xtickname=xtickname,yticks=yticks,$
  ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
  position=posit,title=title,xtitle=xtitle,$
  ytitle=ytitle,/follow,/noerase,$
  subtitle=subtitle,$
  xcharsize=xcharsize,c_labels=c_labels,yminor=yminor,xminor=xminor,$
  xticklen=-.02,yticklen=-.02,$
  charsize=charsize,charthick=charthick 

if keyword_set(oplot_sec) ne 0 then begin
    hrday_oplt=(oplot_sec-d.refsec)/3600.d
    oplot,[hrday_oplt,hrday_oplt],[y(0),y(n_elements(y)-1)],$
      color=0;clr_yellow;wst.clr_orange
  endif
;---------------------------- color scale ----------------------------------

;get position of color scale plot window in normal coords
  px=[posit(2)+0.04,posit(2)+0.06]    
  py=[posit(1),posit(3)]

;plot color scale axes
  ycticks=3 
  ycrange=[mn,mx]
  yctickv=ycrange(0)+indgen(ycticks+1)*(ycrange(1)-ycrange(0))/ycticks 
  ytickname=strcompress(string(10.^yctickv,format='(e7.1)'))
  plot,xc,[mn,mx],/nodata,position=[px(0),py(0),px(1),py(1)],ystyle=4,xstyle=4,$
    /noerase,charthick=charthick,charsize=charsize
  yctitle='f'
  axis,yaxis=1,yticks=ycticks,yrange=ycrange,ytickv=yctickv,ytitle=yctitle,$
   charthick=charthick,charsize=charsize,ytickname=ytickname
;plot color scale image
  tv,zc,!x.window(0),!y.window(0),xsize=!x.window(1)-!x.window(0),$
    ysize=!y.window(1)-!y.window(0),/norm

   
endelse 
 
end