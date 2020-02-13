pro rx_test,sina,cosa,a1,a2,gsex,gsey,gsez,xgser,ygser,zgser,$
 bx,by,bz,bxr,byr,bzr,th,costh,sinth,z0r,y0r,x0r,r,x

common sharewidg,wa

;xr=a1-a2*(yr^2+z0r^2)    shock equation in plane of vsw, b rotated about
;xgse such that bgsez=0

xrange=[-50,50]
yrange=xrange
xticks=10
yticks=xticks
pt=80

npl=4
posn=fltarr(4,npl)
pos,npl,posn,yoff=0.05,ytop=0.95,xoff=0.1,xtop=0.95,ysep=0.075
window,0,xsize=600,ysize=900

;show the transformation from gse to gse_rot

plot,xrange,yrange,/nodata,xticks=xticks,yticks=yticks,charsize=1.25,$
  xtitle='Ygse',ytitle='Zgse',position=posn(*,0),$
  title=string(asin(sina)/!dtor,format='(i5)')+$
  'deg rotation about Xgse!CB and Vsw in rotated x-y plane'
oplot,xrange,[0,0]
oplot,[0,0],yrange

axis=indgen(pt)*(xrange(1)-0.)/(pt-1)
ygse_rot= (sina/cosa) * axis 
zgse_rot= -(cosa/sina) * (-axis)       
oplot,axis,ygse_rot,linestyle=1 
oplot,-axis,zgse_rot,linestyle=1

xyouts,gsey,gsez,'sc',/data

rbsr=findgen(pt)
xb=xrange(0)+indgen(pt)*(xrange(1)-xrange(0))/(pt-1)

plot,xrange,yrange,xticks=xticks,yticks=yticks,charsize=1.25,$
  position=posn(*,1),/normal, $
  xtitle='Xgse_rot',ytitle='Ygse_rot',/nodata,/noerase;xcharsize=0.001,
oplot,xrange,[0,0],linestyle=1
oplot,[0,0],yrange,linestyle=1
;z=0 slice
  ybsr=rbsr
  xbsr=a1-a2*ybsr^2
oplot,xbsr,ybsr  & oplot,xbsr,-ybsr
yb=ygser+(byr/bxr)*(xb - xgser)
oplot,xb,yb
xyouts,xgser,ygser,'sc',/data



plot,xrange,yrange,xticks=xticks,yticks=yticks,charsize=1.25,/noerase,$
  ytitle='Zgse_rot',/nodata,position=posn(*,2),/normal,xtitle='Xgse_rot'
  
oplot,xrange,[0,0],linestyle=1
oplot,[0,0],yrange,linestyle=1
;y=0 slice
  zbsr=rbsr
oplot,xbsr,zbsr  & oplot,xbsr,-zbsr
oplot,xrange,[zgser,zgser],linestyle=1,color=150
xyouts,xgser,zgser,'sc',/data



;z=zgser slice
root=rbsr^2-zgser^2
wpos=where(root ge 0)
if wpos(0) ne -1 then begin
  ybsr=sqrt(root(wpos))
  plot,xrange,yrange,xticks=xticks,yticks=yticks,charsize=1.25,/noerase,$
    xtitle='Xgse_rot',ytitle='Ygse_rot',/nodata,position=posn(*,3),/normal
  xyouts,/normal,0.65,posn(1,3)+0.9*(posn(3,3)-posn(1,3)),$
  'slice : Zgse_rot='+string(zgser,format='(f6.1)')    
  oplot,xrange,[0,0],linestyle=1
  oplot,[0,0],yrange,linestyle=1
  oplot,xbsr(wpos),ybsr  & oplot,xbsr(wpos),-ybsr
  xb=xrange(0)+indgen(pt)*(xrange(1)-xrange(0))/(pt-1)
  yb=ygser+(byr/bxr)*(xb - xgser)   
  oplot,xb,yb  ;mag fld line through sc
  xyouts,xgser,ygser,'sc',/data
  ytan=y0r+(byr/bxr)*(xb-x0r)
  oplot,xb,ytan,linestyle=2   ;mag fld line tangent to shock
  ;xyouts,x0r,y0r,'X',/data
endif else print,'no intersection'  


;!p.multi=0
wset,wa.win(0)

;stop
end
