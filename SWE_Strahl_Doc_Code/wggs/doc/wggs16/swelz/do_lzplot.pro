pro do_lzplot,pltype=pltype,F_integ=F_integ,wid=wid,lsscn=lsscn,vparc=vparc

common wstuff,wst
common swestuff,swest
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common sharelevelzero,pltwin_frmt,oplot_sec


if swest.delete eq 0 then $
  labl1='SWE '+swest.specie(swest.specie_selct)+' (Mask Off)' else $
  labl1='SWE '+swest.specie(swest.specie_selct)


if wst.hardcopy ne 1 then begin
    oplot_sec=pb5_sec(wst.pb5)
    wset,swest.win(0) & erase
    plt,/lzwin
endif
    
if max(pltype) lt 10 then begin;   display in vpara, vperp coords
  
  if keyword_set(lsscn) eq 0 then lsscn=0
  if keyword_set(vparc) eq 0 then vparc=0
   
  if keyword_set(pltype) eq 0 then pltype=[0,2]
   
  if F_integ then redf_integ,xsize=swest.win_xsize(wid),$
      ysize=swest.win_ysize(wid),$
      labl1=labl1,labl2=swest.spndt   $ 
      
  else begin
    if swest.plotf then begin
       fmulti_plot,600,600,pltype=pltype,ion=swest.specie_selct,$
         labl1=labl1,labl2=swest.spndt 
    endif else begin
       wset,swest.win(wid) 
       fplot,swest.win_xsize(wid),swest.win_ysize(wid),pltype=pltype,$
         ion=swest.specie_selct,$
         labl1=labl1,labl2=swest.spndt,lsscn=lsscn,vparc=vparc
    endelse  
  endelse   

endif else if max(pltype) eq 10 then begin
     y=reform(em(*,0))
     ymin=1.
     wle=where(y le ymin)
     if wle(0) ne -1 then y(wle(n_elements(wle)-1))=ymin
     wge=where(y ge ymin)
     y(wge)=alog10(y(wge))
     x=reverse(reform(pm(0,*)))
     g=fltarr(n_elements(x),n_elements(y(wge))) 
     for i=0,n_elements(x)-1 do g(i,*)=fm(wge,n_elements(x)-1-i) 
   
  ;scale non-zero elements of (log) image array to byte scale
     w=where(g ne 0)
     g(w)=alog10(g(w))
     mn=min(g(w),max=mx)
     w0=where(g eq 0)
     n_colors=!d.table_size-1
     g(w)=bytscl(temporary(g(w)),min=mn,max=mx,top=n_colors-1);scale to clrs
     if w0(0) ne -1 then g(w0)=0
    
  ;set plot parameters  
     ymn=min(y,max=ymx) & ytck=5 
     ytickv=ymn+indgen(ytck+1)*(ymx-ymn)/ytck
     ytickn=string(10.^ytickv,format='(i5)')
     xmn=180. & xmx=0. & xrange=[xmn,xmx] & xtck=6  
     
     wset,swest.win(wid)
     x_im_sz=swest.win_xsize(wid)
     y_im_sz=swest.win_ysize(wid)
     pos,1,posn,xoff=0.21
     img_ep, g, y, x, zmn=mn, zmx=mx,  pos=posn,  $
       ytickv=ytickv,ytitle='log eV', $
       ytickn=ytickn,$
       xrange=xrange, $
       xtickv=xmn+indgen(xtck+1)*(xmx-xmn)/xtck, $
       xtickn=string(xmn+indgen(xtck+1)*(xmx-xmn)/xtck,format='(i5)'),$
       subtitle=swest.spndt(0),$
       xticks=xtck, xtitle='Pitch Angle (deg)' ,xminor=1, $
       title=labl1,$
       charsize=1.25,charthick=1.0,xcharsize=1.0,$
       ximsize=x_im_sz,yimsize=y_im_sz,n_colors=n_colors 

endif else if max(pltype) eq 11 then begin

  wset,swest.win(wid) 
  ctr_ep,swest.win_xsize(wid),swest.win_ysize(wid),pltype=pltype,$
      ion=swest.specie_selct,$
      labl1=labl1,labl2=swest.spndt
  
endif else if max(pltype) eq 12 then begin

     xrange=[-vmax,vmax]*1e-8
     x=xrange(0)+indgen(nx)*(xrange(1)-xrange(0))/(nx-1)
     yrange=[-vmax,vmax]*1e-8
     y=yrange(0)+indgen(ny)*(yrange(1)-yrange(0))/(ny-1)
     
     ;reflect zgrd about vperp=0
     zreverse=fltarr(nx,ny)
     for i=0,nx-1 do zreverse(i,*)=rotate(zgrd(i,*),2)
     z=fltarr(nx,2*ny-1)
     z(0,0)=zreverse
     z(0,ny-1)=zgrd
     
   
  ;scale non-zero elements of (log) image array to byte scale
     w=where(z ne 0)
     mn=min(z(w),max=mx)
     w0=where(z eq 0)
     n_colors=!d.table_size-1
     z(w)=bytscl(temporary(z(w)),min=mn,max=mx,top=n_colors-1);scale to clrs
     if w0(0) ne -1 then z(w0)=0
    
  ;set plot parameters  
     ytck=4 
     ytickv=yrange(0)+indgen(ytck+1)*(yrange(1)-yrange(0))/ytck
     ytickn=string(ytickv,format='(f5.1)')
     xmn=xrange(0) & xmx=xrange(1)
     xtck=4  
     
     wset,swest.win(wid)
     x_im_sz=swest.win_xsize(wid)
     y_im_sz=x_im_sz  ;wlz.win_ysize(wid)
     pos,1,posn,xoff=0.21,yoff=0.6
     posn(3)=posn(1)+(posn(2)-posn(0))
     
     img_ep, z, y, x, zmn=mn, zmx=mx,  pos=posn,  $
       ytickv=ytickv,ytitle='v!m!dx!n  1000 km/s', $
       ytickn=ytickn,$
       xrange=xrange, $
       xtickv=xmn+indgen(xtck+1)*(xmx-xmn)/xtck, $
       xtickn=string(xmn+indgen(xtck+1)*(xmx-xmn)/xtck,format='(f5.1)'),$
       subtitle=swest.spndt(0),$
       xticks=xtck, xtitle='v!m!d#!n  1000 km/s' ,xminor=1, $
       title=labl1,$
       charsize=1.25,charthick=1.0,xcharsize=1.0,$
       ximsize=x_im_sz,yimsize=y_im_sz,n_colors=n_colors 


endif 

end