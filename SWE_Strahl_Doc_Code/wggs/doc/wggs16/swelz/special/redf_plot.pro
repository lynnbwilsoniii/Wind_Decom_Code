pro redf_plot,indx=indx,win=wid

common sharebuffer,redfbuff
common sharewidglz,wlz

;plots redf from stored array

rf=redfbuff(indx)

F=rf.F 
vscal=1e8
xrange=[-rf.vmax,rf.vmax]/vscal
nx=n_elements(rf.F)
x=xrange(0)+indgen(nx)*(xrange(1)-xrange(0))/(nx-1)
xticks=4
vlabl='1000 km/s'
xtitle='v!m!d#!n '+vlabl
xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
xtickname=string(xtickv,format='(i4)') 
title='SWE Electrons'
noerase=0
subtitle=string(ymd(rf.pb5tim),format='(i8)')+'  '+rf.hms
xtickformat='(f5.1)'
xcharsize=1.0

nstack=1
posn=fltarr(4,nstack)
x_im_sz=wlz.win_xsize(wid)
y_im_sz=wlz.win_ysize(wid)
imsize=min([0.75*x_im_sz,(1.-0.15-(nstack-1)*0.02)*y_im_sz/nstack])
x_off=0.5*x_im_sz-0.385*imsize
y_off=y_im_sz/2-nstack*imsize/2 +  0.075*imsize            
y_sep=0.02*y_im_sz
posn(0,*)=x_off
posn(2,*)=x_off+imsize
for i=0,nstack-1 do begin
  posn(1,i)=y_off+i*(imsize+y_sep)
  posn(3,i)=y_off+imsize+i*(y_sep+imsize)
endfor
dev=1
position=posn(*,0)

wset,wlz.win(wid)

;make plot
  wF=where(F gt 0)
  Frange=[1.e-12,1.e-7]
  plot_io,x(wF),F(wF),xrange=xrange,xstyle=1,xticks=xticks,$
    xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
    xtitle=xtitle,xtickname=xtickname,ytitle='reduced log!d10!n F',$
    title=title,position=position,device=dev,yrange=Frange,$
    ystyle=1,noerase=noerase,subtitle=subtitle,$
    xtickformat=xtickformat,xcharsize=xcharsize  
  
end
