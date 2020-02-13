pro foreshock_geometry_event,event
common sharew2,sbase
common wstuff,wst




  case event.value of
                
  'xHardcopy' : begin
                 wst.hardcopy=1
                 wst.printer=wst.printer_bw
                 wst.print_flnm=wst.print_flnm_bw
                 wst.print_cmd=wst.print_cmd_bw 
                 clrtbl_indx,/hardcopy 
                 foreshock_geometry
               endcase
  
  'Quit': WIDGET_CONTROL, event.top, /DESTROY
  else:
  endcase

  


end


;======================= foreshock_geometry =================================

pro foreshock_geometry

common sharew2,sbase
common wstuff,wst
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common ionkpstuff,ionkpflnm,ionkpdat
common shared,d



print,'foreshock_geometry  ',wst.xydata(0),d.ndx_buff(0,0),d.ndx_buff(0,4)
print,'foreshock_geometry  ',wst.pb5

if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0

if xregistered('foreshock_geometry') then WIDGET_CONTROL, sbase,/destroy


if wst.hardcopy(0) then begin 
  print,' ' & print,'making hardcopy... '
  ;clrtbl_indx,/hardcopy 
  set_plot,'ps',/interpolate
  pltfil=getenv('IDLSAV')+wst.print_flnm 
  print,'pltfil',pltfil
  device,/inches,xoffset=1.,yoffset=1.,xsize=5.0,ysize=5.0,/color,$
     filename=pltfil
  orange=!p.color
  green=!p.color
endif

;---------------- set up draw widget ---------------------------------------

sbase = WIDGET_BASE(TITLE = 'Foreshock Geometry',/COLUMN)

cbase=widget_base(sbase,/column)
button=cw_bgroup(cbase,['Hardcopy','Quit'],row=1,/return_name)
   

rbase=widget_base(cbase,/row)
xsiz=300 & ysiz=800
draw1 = WIDGET_DRAW(rbase,/FRAME, $
  RETAIN = 2, XSIZE = xsiz, YSIZE = ysiz)
  
draw2 = WIDGET_DRAW(rbase,/FRAME, $
  RETAIN = 2, XSIZE = xsiz, YSIZE = ysiz)
  
WIDGET_CONTROL, sbase, /REALIZE
XMANAGER, "foreshock_geometry", sbase, GROUP_LEADER = GROUP

orange=wst.clr_orange
green=wst.clr_green


;<<<<<<<<<<<<<<<<<<<<< get local, current ram pressure >>>>>>>>>>>>>>>>>>>>>>

id1=d.ndx_buff(0,0)
dens=mdat(id1).fnout
umag=mdat(id1).umag
bx=mdat(id1).b(0)
by=mdat(id1).b(1)
bz=mdat(id1).b(2)
mindx=fix(float(mdat(id1).tpb5(2))/600000.)
re=6373.
gsex=gse_pos(mindx,0)/re
gsey=gse_pos(mindx,1)/re
gsez=gse_pos(mindx,2)/re


;<<<<<<<<<<<<<<<<<<< compute foreshock geometry >>>>>>>>>>>>>>>>>>>>>>>>>>>>
rx,dens,umag,bx,by,bz,gsex,gsey,gsez,r,x,/new,$
  th=th,x0r=x0r,y0r=y0r,z0r=z0r,$
  bxr=bxr,byr=byr,bzr=bzr,xgser=xgser,ygser=ygser,zgser=zgser,$
  sina=sina,cosa=cosa,a1=a1,a2=a2

;<<<<<<<<<<<<<<<<<<<< find plot axis limits >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

xrange=[-40.,40.]
xticks=4
xminor=2
yrange=xrange
yticks=xticks
yminor=xminor


;<<<<<<<<<<<<< plot in gse coords>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

WIDGET_CONTROL, draw1, GET_VALUE=windw1
wset,windw1

pt=80

rbs=findgen(pt)
xb=xrange(0)+indgen(pt)*(xrange(1)-xrange(0))/(pt-1)

npl=3
posn=fltarr(4,npl)
pos,npl,posn,yoff=0.05,ytop=0.95,xoff=0.2,xtop=0.95,ysep=0.075
posn(2,*)=posn(0,*)+(posn(3,*)-posn(1,*))*ysiz/xsiz

  
plot,xrange,yrange,/nodata,xticks=xticks,yticks=yticks,charsize=1.25,$
  xtitle='Xgse',ytitle='Ygse',position=posn(*,0),$
  xstyle=1,ystyle=1,xminor=xminor,yminor=yminor,$
  title=string(pb5_ymd(wst.pb5),format='(i8)')+'  '+wst.hms
oplot,xrange,[0,0]
oplot,[0,0],yrange
ybs=rbs
xbs=a1-a2*ybs^2
oplot,xbs,ybs & oplot,xbs,-ybs     ;bow shock
xyouts,gsex,gsey,'sc',/data        ;sc 
yb=gsey+(by/bx)*(xb - gsex)     ;mag fld projection
oplot,xb,yb,color=orange

  
plot,xrange,yrange,/nodata,xticks=xticks,yticks=yticks,charsize=1.25,$
  xtitle='Xgse',ytitle='Zgse',position=posn(*,1),/noerase,$
  xstyle=1,ystyle=1,xminor=xminor,yminor=yminor
oplot,xrange,[0,0]
oplot,[0,0],yrange
zbs=rbs
xbs=a1-a2*zbs^2
oplot,xbs,zbs & oplot,xbs,-zbs     ;bow shock
xyouts,gsex,gsez,'sc',/data        ;sc 
zb=gsex+(bz/bx)*(xb - gsex)     ;mag fld projection
oplot,xb,zb,color=orange


;show the transformation from gse to gse_rot
plot,xrange,yrange,/nodata,xticks=xticks,yticks=yticks,charsize=1.25,$
  xtitle='Ygse',ytitle='Zgse',position=posn(*,2),/noerase,$
  xstyle=1,ystyle=1,xminor=xminor,yminor=yminor
oplot,xrange,[0,0]
oplot,[0,0],yrange
xyouts,xrange(1)/2,yrange(1)/2,'Rotated axes',color=green  
axis=indgen(pt)*(xrange(1)-0.)/(pt-1)
ygse_rot= (sina/cosa) * axis 
zgse_rot= -(cosa/sina) * (-axis)       
oplot,axis,ygse_rot,linestyle=1,color=green     ;rotated axes
oplot,-axis,zgse_rot,linestyle=1,color=green    ;rotated axes
xyouts,gsey,gsez,'sc',/data         ;sc
yb=xb
zb=gsez+(bz/by)*(yb - gsey)     ;mag fld projection
oplot,yb,zb,color=orange


;<<<<<<<<<<<<< plot in ROTATED gse coords>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

WIDGET_CONTROL, draw2, GET_VALUE=windw2
wset,windw2

npl=3
posn=fltarr(4,npl)
pos,npl,posn,yoff=0.05,ytop=0.95,xoff=0.2,xtop=0.95,ysep=0.075
posn(2,*)=posn(0,*)+(posn(3,*)-posn(1,*))*ysiz/xsiz

plot,xrange,yrange,xticks=xticks,yticks=yticks,charsize=1.25,$
  position=posn(*,0),/normal,xstyle=1,ystyle=1,xminor=xminor,yminor=yminor, $
  xtitle='Xgse_rot',ytitle='Ygse_rot',/nodata,$
  title=string(asin(sina)/!dtor,format='(i5)')+$
  'deg rotation about Xgse!CB and Vsw in rotated x-y plane'
oplot,xrange,[0,0],linestyle=1
oplot,[0,0],yrange,linestyle=1
ybsr=rbs
xbsr=a1-a2*ybsr^2
oplot,xbsr,ybsr  & oplot,xbsr,-ybsr
yb=ygser+(byr/bxr)*(xb - xgser)
oplot,xb,yb,color=orange
xyouts,xgser,ygser,'sc',/data



plot,xrange,yrange,xticks=xticks,yticks=yticks,charsize=1.25,/noerase,$
  ytitle='Zgse_rot',xtitle='Xgse_rot',/nodata,position=posn(*,1),/normal,$
  xstyle=1,ystyle=1,xminor=xminor,yminor=yminor  
oplot,xrange,[0,0],linestyle=1
oplot,[0,0],yrange,linestyle=1
zbsr=rbs
oplot,xbsr,zbsr  & oplot,xbsr,-zbsr
oplot,xrange,[zgser,zgser],linestyle=1,color=orange  ;sc, B, V plane, edge-on
xyouts,xgser,zgser,'sc',/data
xyouts,xgser,zgser-5.,'Plane of sc, B, V, edge on',/data,color=orange


;z=zgser slice
root=rbs^2-zgser^2
wpos=where(root ge 0)
if wpos(0) ne -1 then begin
  ybsr=sqrt(root(wpos))
  plot,xrange,yrange,xticks=xticks,yticks=yticks,charsize=1.25,/noerase,$
    xtitle='Xgse_rot',ytitle='Ygse_rot',/nodata,position=posn(*,2),/normal,$
    xstyle=1,ystyle=1,xminor=xminor,yminor=yminor,$
    title='Plane of sc, B, V'    
  oplot,xrange,[0,0],linestyle=1
  oplot,[0,0],yrange,linestyle=1
  oplot,xbsr(wpos),ybsr  & oplot,xbsr(wpos),-ybsr
  yb=ygser+(byr/bxr)*(xb - xgser)   
  oplot,xb,yb,color=orange  ;mag fld line through sc
  xyouts,xgser,ygser,'sc',/data
  ytan=y0r+(byr/bxr)*(xb-x0r)
  oplot,xb,ytan,linestyle=2   ;mag fld line tangent to shock
endif else print,'no intersection'  



if wst.hardcopy(0) then begin
  device,/close
  print,' ' & print,'printing hardcopy: ',wst.print_cmd
  spawn,wst.print_cmd
  set_plot,'x'
  wst.hardcopy=0 
endif



end
