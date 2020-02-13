pro colorbar_rjf,ncolors=ncolors


print,' ' & print,'A test program to display colorbar and to demonstrate overlaying image and plot axes :' & print,' '
print,' I) device = Sun (fixed pixels),'
print,'      case 1 (k=1) : scaling image  size to plot axes;' 
print,'      case 2 (k=2) : scaling plot axes to image size'
print,' '
print,'II) device = Postscript (scaleable pixels),'
print,'      case 3 (k=3) : scaling plot axes to image size'
print,' ' & print,'(Adapted from IDL manual page 11.10. ). RJF, December 1990.'
print,' ' & print,'Enter k=1, 2, or 3' & read,k

if keyword_set(ncolors) eq 0 then ncolors=!d.n_colors
  x=indgen(ncolors)  & y=indgen(100)        ;establish plot axes
  z=byte(x#(intarr(100)+1))   ;create image array

if k lt 1 or k gt 4 then stop
case k of
3: begin  ;device = Postscript
     set_plot,'ps'
     device,/color,/inches,xoffset=1.0,yoffset=4.,xsize=6.0,ysize=3.0
     plot,x,y,/nodata,position=[0.1,0.1,0.9,0.5],ystyle=4,font=0,$
     xtitle='color index',title='CURRENT COLOR TABLE',xstyle=1;plot axes 
     ;display image with lower left corner at origin of plot and 
     ;image scaled to fit plot window :
     tv,z,!x.window(0),!y.window(0),xsize=!x.window(1)-!x.window(0),$
     ysize=!y.window(1)-!y.window(0),/norm  
     device,/close
     spawn,'lpr -Pcq idl.ps'
     set_plot,'x'
     ;!p.background=255 & ;!p.color=0
   endcase

2: begin   ;device = Sun, scale plot axes to image size
     plot,x,y,/nodata,position=[0.1,0.1,0.9,0.5],xstyle=1,ystyle=4;plot axes
     px=!x.window*!d.x_vsize ;  get size and position of plot window 
     py=!y.window*!d.y_vsize ;    in device pixels
     sz=size(z) ;get size of original image, sz(1)= # cols, sz(2)= # rows
     erase  ;erase the plot axes
     ;replot wth origin at lwr lft corner of image, scaled to size of image:
     plot,x,y,/nodata,position=[px(0),py(0),px(0)+sz(1)-1,py(0)+sz(2)-1],$
     /device,ystyle=4,xtitle='color index',$
     title='CURRENT COLOR TABLE',xstyle=1
     ;display image at lower left corner of resized plot window :     
     tv,z,px(0),py(0) 
   endcase

1: begin  ;device = Sun, scale image size to plot axes
     plot,x,y,/nodata,position=[0.1,0.1,0.9,0.5],xstyle=1,ystyle=4
     ;plot axes with no data
     px=!x.window*!d.x_vsize   ;  get size and position of 
     py=!y.window*!d.y_vsize   ;    plot window in device pixels
     sx=px(1)-px(0)+1   ;desired size of image in pixels
     sy=py(1)-py(0)+1
     sz=size(z) ;get size of original image, sz(1)= # cols, sz(2)= # rows
     erase
     z_new=poly_2d(z,[[0,0],[sz(1)/sx,0]],[[0,sz(2)/sy],[0,0]],0,sx,sy)
     sz=size(z_new) ; size of new, resized image
     ;replot at lwr lft corner of original image, scaled to size of new image
     
     plot,x,y,/nodata,position=[px(0),py(0),px(0)+sz(1)-1,py(0)+sz(2)-1],$
     /device,ystyle=4,xtitle='color index',$
     title='CURRENT COLOR TABLE',xstyle=1
     tv,z_new,px(0),py(0)  ;display new, resized image
   endcase

endcase

end
