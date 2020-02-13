pro wav_hrtnr_img,z,x,dateymd,pos=posn,$
    xrange=xrange,xtickv=xtickv,xtickn=xtickn,subtitle=stitle,$
    xticks=xticks, xtitle=xlabl , title=ztitle,$
    charsize=charsize,charthick=charthick,xcharsize=xcharsize

common wstuff,wst
common shared,d
common sharelevelzero,pltwin_frmt,oplot_sec

n_colors=!d.table_size-1

rcvr='tnr'

flow=d.wav_hrtnr_spctrm.fhzmin  ;4.
fhigh=d.wav_hrtnr_spctrm.fhzmax  ;245.146
fspace=.188144e-1
funit='kHz (hrTNR)'
linlog=1

chan1=fix((alog10(wst.fhz1)-alog10(flow))/fspace)
chan2=fix((alog10(wst.fhz2)-alog10(flow))/fspace)
fhz1=alog10(flow)+chan1*fspace
fhz2=alog10(flow)+chan2*fspace
ydim=(chan2-chan1)+1
xdim=n_elements(x)

arrayb=z
array=arrayb(*,chan1:chan2)
lt1=where(array lt 1.)
if lt1(0) ne -1 then array(lt1)=1.
array=10.*alog10(array)
 


plot_data:
scale1=wst.scale1
scale2=wst.scale2 
nticks=10

freqlo=findgen(ydim)*fspace+fhz1
freqlo=10.^freqlo

;set up color scale image array
  yc=findgen(n_colors-1) & xc=findgen(2)
  zc=byte((intarr(100)+1)#yc)
  sz=size(zc)    ;size of original image, sz(1)= # cols, sz(2)= # rows

;data array color-scaled
  arrayb=bytscl(array,min=scale1,max=scale2,top=n_colors-1)
  
if wst.hardcopy eq 0 then begin   ;no hardcopy

;get position of color scale image plot window in device pixels
  px=[posn(2)+0.04,posn(2)+0.06]*!d.x_vsize    
  py=[posn(1),posn(3)]*!d.y_vsize

;get desired size of color scale image in pixels
  sx=px(1)-px(0)+1
  sy=py(1)-py(0)+1

;new, resized color scale image 
  z_new=congrid(zc,sx,sy)
  sz_new=size(z_new)  ;size of new, resized color scale image
   
;color scale axes
  ycticks= nticks/2
  ycrange=[scale1,scale2]
  yctickv=ycrange(0)+indgen(ycticks+1)*(ycrange(1)-ycrange(0))/ycticks
  pos_dev_clrscl=[px(0),py(0),px(0)+sz_new(1)-1,py(0)+sz_new(2)-1]
  plot,xc,[scale1,scale2],/nodata,$
    position=pos_dev_clrscl,$
   /device,ystyle=4,xstyle=4,/noerase,$
   charthick=charthick,charsize=charsize
  yctitle='rel int (dB)'
  axis,yaxis=1,yticks=ycticks,yrange=ycrange,ytickv=yctickv,ytitle=yctitle,$
     charthick=charthick,charsize=charsize
     
;display new, resized color scale image
  tv,z_new,px(0),py(0)
  
;get position of data image plot window in device pixels
  px=posn(0)*!d.x_vsize+[(x(0)-xrange(0)),(x(n_elements(x)-1)-xrange(0))]*$
    (posn(2)-posn(0))/(xrange(1)-xrange(0)) *!d.x_vsize
  py=[posn(1),posn(3)]*!d.y_vsize 
  
;get desired size of data image in pixels
  sx=px(1)-px(0)+1
  sy=py(1)-py(0)+1
  
;display new, resized data image
  tv,poly_2d(arrayb,$
    [[0,0],[xdim/sx,0]],[[0,ydim/sy],$
    [0,0]],0,sx,sy),px(0),py(0)
    
;data image axes
  contour,array,x,freqlo,position=posn,$
    /noerase,/data,xstyle=1,ystyle=1,ytype=linlog,$
    xtitle=xlabl,xticks=xticks,xtickv=xtickv,xtickname=xtickn,$
    title=ztitle,ytitle=funit,ticklen=-.01,subtitle=stitle,/nodata

 if keyword_set(oplot_sec) ne 0 then begin
    hrday_oplt=(oplot_sec-d.refsec)/3600.d
    oplot,[hrday_oplt,hrday_oplt],[freqlo(0),freqlo(n_elements(freqlo)-1)],$
      color=0
  endif
  
endif else begin

;get position of color scale plot window in normal coords
  px=[posn(2)+0.04,posn(2)+0.06]    
  py=[posn(1),posn(3)]
   
;plot color scale axes
  ycticks= nticks/2
  ycrange=[scale1,scale2]
  yctickv=ycrange(0)+indgen(ycticks+1)*(ycrange(1)-ycrange(0))/ycticks
  plot,xc,[scale1,scale2],/nodata,$
    position=[px(0),py(0),px(1),py(1)],$
    ystyle=4,xstyle=4,/noerase,$
    charthick=charthick,charsize=charsize
  yctitle='rel int (dB)'
  axis,yaxis=1,yticks=ycticks,yrange=ycrange,ytickv=yctickv,ytitle=yctitle,$
     charthick=charthick,charsize=charsize
     
;display color scale image
   tv,zc,px(0),py(0),xsize=px(1)-px(0),ysize=py(1)-py(0),/norm

;get position of data image plot window
  px=posn(0)+[(x(0)-xrange(0)),(x(n_elements(x)-1)-xrange(0))]*$
    (posn(2)-posn(0))/(xrange(1)-xrange(0)) 
  py=[posn(1),posn(3)]

;plot data image
  tv,arrayb,px(0),py(0),xsize=px(1)-px(0),ysize=py(1)-py(0),/norm
    
;plot data image axes
  contour,array,x,freqlo,position=posn,$
    /noerase,xstyle=1,ystyle=1,ytype=linlog,$
    xtitle=xlabl,xticks=xticks,xtickv=xtickv,xtickname=xtickn,$
    title=ztitle,ytitle=funit,ticklen=-.01,subtitle=stitle,/nodata

endelse  

end


