;============== strahl_spectrum_img ===========================================

pro swe_strahl_spectrum_img, f, u, t, rlbl=rlbl, zmn=mn,zmx=mx,pos=posit,$
  logimg=logimg,  $
  ytitle=labl, ytickv=utickv, ytickn=ytickn, xrange=xrange, xtickv=xtickv,$
  xtickn=xtickn, xticks=xticks, xtitle=xlabl, xminor=xminor,title=title,$
  subtitle=subtitle,charsize=charsize,$
  charthick=charthick,xcharsize=xcharsize,n_colors=n_colors,$
  ximsize=ximsize,yimsize=yimsize,yminor=yminor

common wstuff,wst
common swestuff,swest
 
if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0
if keyword_set(logimg) eq 0 then logimg=0
if keyword_set(n_colors) eq 0 then n_colors=!d.table_size-1
if keyword_set(yminor) eq 0 then yminor=1
if keyword_set(xminor) eq 0 then xminor=1


;print,'img_ep: labl,mn, mx ',labl,mn,mx

;set selected color table and line color
  ;clrtbl_indx

;---------------------- some preliminaries ---------------------------------

;set up color scale image array
  yc=findgen(n_colors) & xc=findgen(2)
  zc=byte((intarr(100)+1)#yc)

;--------------------- plot image -------------------------------------------

if wst.hardcopy eq 0 then begin   ;no hardcopy

;---------- determine plot axes position for color scale ---------------------

;get position of plot window in device pixels
  px=[posit(2)+0.05,posit(2)+0.07]*ximsize  
  py=[posit(1),posit(3)]*yimsize

;get desired size of image in pixels   
  sx=px(1)-px(0)+1   
  sy=py(1)-py(0)+1

;get size of original image, sz(1)= # cols, sz(2)= # rows
  sz=size(zc) 

;get new, resized color scale image
  z_new=congrid(zc,sx,sy)

;size of new, resized color scale image
  sz_new=size(z_new) 



;------------ determine plot axes position for data ---------------------------

;get position of plot window in device pixels
  trange=xrange
  pt=posit(0)*ximsize+[(t(0)-trange(0)),(t(n_elements(t)-1)-trange(0))]*$
    (posit(2)-posit(0))/(trange(1)-trange(0)) *ximsize
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


;color scale axes
  pos_dev_clrscl=[px(0),py(0),px(0)+sz_new(1)-1,py(0)+sz_new(2)-1]
  ycticks= 3
  ycrange=[mn,mx]
  yctickv=ycrange(0)+indgen(ycticks+1)*(ycrange(1)-ycrange(0))/ycticks
  if logimg then $
    yctickn=strcompress(string(10.^yctickv,format='(i4)')) else $
    yctickn=strcompress(string(yctickv,format='(i4)'))
 
  plot,xc,[mn,mx],/nodata,$
    position=pos_dev_clrscl,$
   /device,ystyle=4,xstyle=4,/noerase,$
   charthick=charthick,charsize=charsize
  if keyword_set(rlbl) ne 0 then yctitle=rlbl else yctitle='' 
  axis,yaxis=1,yticks=ycticks,yrange=ycrange,ytickv=yctickv,ytitle=yctitle,$
     charthick=charthick,charsize=charsize,ytickname=yctickn

;display new, resized color scale image
  tv,z_new,px(0),py(0) 



;----------------------- plot data -------------------------------------------

;get data axes posiions scaled to size of new image
  pos_dev_data=[posit(0)*!d.x_vsize,pp(0),posit(2)*!d.x_vsize,pp(0)+sf_new(2)-1]
  yticks=n_elements(utickv)-1

;display new, resized data image
  tv,f_new,pt(0),pp(0) 


;data axes
  plot,t,u,/nodata,position=pos_dev_data,$
    xrange=xrange,xticks=xticks,xtickv=xtickv,xtitle=xlabl,subtitle=subtitle,$
    xtickname=xtickn,xticklen=-.01,yticklen=-.02,$
    yrange=[u(0),u(n_elements(u)-1)],yminor=yminor,xminor=xminor,$
    yticks=yticks,ytickv=utickv,ytitle=labl,xstyle=1,$
    ystyle=1,/device,/noerase,title=title,ytickname=ytickn,$
    charthick=charthick,charsize=charsize,xcharsize=xcharsize



endif else begin    ;make hardcopy 
print,' ' & print,'making hardcopy... ' 



;---------------------------- color scale ----------------------------------

;get position of color scale plot window in normal coords
  px=[posit(2)+0.04,posit(2)+0.06]    
  py=[posit(1),posit(3)]

;plot color scale axes
  ycticks=3 
  ycrange=[mn,mx]
  yctickv=ycrange(0)+indgen(ycticks+1)*(ycrange(1)-ycrange(0))/ycticks
  if logimg then $
    ytickname=strcompress(string(10.^yctickv,format='(i4)')) else $
    ytickname=strcompress(string(yctickv,format='(i4)'))    
  plot,xc,[mn,mx],/nodata,position=[px(0),py(0),px(1),py(1)],ystyle=4,xstyle=4,$
    /noerase,charthick=charthick,charsize=charsize
  if keyword_set(rlbl) ne 0 then yctitle=rlbl else yctitle=''
  axis,yaxis=1,yticks=ycticks,yrange=ycrange,ytickv=yctickv,ytitle=yctitle,$
   charthick=charthick,charsize=charsize,ytickname=ytickname

;plot color scale image
  tv,zc,!x.window(0),!y.window(0),xsize=!x.window(1)-!x.window(0),$
    ysize=!y.window(1)-!y.window(0),/norm


;--------------------------- data -------------------------------------------

;get position of data plot window in normal coords
   trange=xrange
   pt=posit(0)+ [(t(0)-trange(0)),(t(n_elements(t)-1)-trange(0))]*$
     (posit(2)-posit(0))/(trange(1)-trange(0)) 
   pp=[posit(1),posit(3)]
   
;plot data image
   tv,f,pt(0),pp(0),xsize=pt(1)-pt(0),ysize=pp(1)-pp(0),/norm

;plot data axes
   yticks=n_elements(utickv)-1
   plot,t,u,/nodata,position=posit,$
     xrange=xrange,xticks=xticks,xtickv=xtickv,xtitle=xlabl,subtitle=subtitle,$
     xtickname=xtickn,xticklen=-.02,yticklen=-.02,$
     yrange=[u(0),u(n_elements(u)-1)],yminor=yminor,xminor=xminor,$
     yticks=yticks,ytickv=utickv,$
     ytitle=labl,xstyle=1,$
     ystyle=1,/noerase,ytickname=ytickn,title=title,$
     charthick=charthick,charsize=charsize,xcharsize=xcharsize

 endelse ;end making hardcopy this panel



end

