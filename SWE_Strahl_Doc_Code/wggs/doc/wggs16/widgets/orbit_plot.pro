pro orbit_plot_event,event
common wstuff,wst

widget_control,event.top,get_uvalue=info,/no_copy

case event.value of
  
  'Parent' : WIDGET_CONTROL, info.parent, /show
  
  'Hardcopy' : begin
     wst.hardcopy=1
     wst.printer=wst.printer_bw
     wst.print_flnm=wst.print_flnm_bw
     wst.print_cmd=wst.print_cmd_bw 
     clrtbl_indx,/hardcopy 
     orbit_plot,event
  endcase
  
  'Quit': begin
    widget_control,event.top,set_uvalue=info,/no_copy
    WIDGET_CONTROL, event.top, /DESTROY
    goto,endd
  endcase   
endcase

widget_control,event.top,set_uvalue=info,/no_copy
endd:

end



pro orbit_plot,event

common shared,d
common wstuff,wst
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel

info={parent:event.top}

if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0

if xregistered('orbit_plot') and wst.hardcopy(0) eq 0 then return  

if wst.hardcopy(0) then begin 
  print,' ' & print,'making hardcopy... '
  ;clrtbl_indx,/hardcopy 
  set_plot,'ps',/interpolate
  pltfil=getenv('IDLSAV')+wst.print_flnm 
  print,'pltfil',pltfil
  device,/inches,xoffset=1.,yoffset=1.,xsize=5.0,ysize=5.0,/color,$
     filename=pltfil
  orange=0  ;!p.color
  green=0  ;!p.color
endif


;---------------- set up draw widget ---------------------------------------

orb_base = WIDGET_BASE(TITLE = ' ',/COLUMN)

cbase=widget_base(orb_base,/column)
orb_quit=cw_bgroup(cbase,row=1,/return_name,$
 ['Parent','Hardcopy','Quit']) 

rbase=widget_base(cbase,/row)
xsiz=500 & ysiz=500
drawxy = WIDGET_DRAW(rbase,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = xsiz, YSIZE = ysiz)

drawxz = WIDGET_DRAW(rbase,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = xsiz, YSIZE = ysiz)
  
WIDGET_CONTROL, orb_base, /REALIZE

orange=wst.clr_orange
green=wst.clr_green


;------------------ do plot -----------------------------------------------
skip:

re=6373.

;determine if windorbit has been selected
worb=where(d.datype(where(d.datype_input ne -1)) eq 'wind_orbit')
if worb(0) ne -1 then begin
  idatype=where(d.datype eq 'wind_orbit')
  orb_gse=transpose(d.wind_orbitdat(d.ndx(0,idatype):d.ndx(1,idatype)).gse_pos)
  timepb5=transpose(d.wind_orbitdat(d.ndx(0,idatype):d.ndx(1,idatype)).tpb5)
endif else begin
  orb_gse=gse_pos/re
  timepb5=tpb5_orb
endelse

sz=size(orb_gse)

xrmx=max(orb_gse(*,0),min=xrmn)
yrmx=max(orb_gse(*,1),min=yrmn)
if xrmn/abs(xrmn) eq xrmx/abs(xrmx) and xrmn gt 0 then xrmn=-10.
if xrmn/abs(xrmn) eq xrmx/abs(xrmx) and xrmn lt 0 then xrmx=10.
if yrmn/abs(yrmn) eq yrmx/abs(yrmx) and yrmn gt 0 then yrmn=-10.
if yrmn/abs(yrmn) eq yrmx/abs(yrmx) and yrmn lt 0 then yrmx=10.

delr=10.
xminor=1
xrmx=delr*(fix(xrmx/delr)+1)
xrmn=delr*(fix(xrmn/delr)-1)
xticks=fix((xrmx-xrmn)/delr)
yrmx=delr*(fix(yrmx/delr)+1)
yrmn=delr*(fix(yrmn/delr)-1)
yticks=fix((yrmx-yrmn)/delr)

if xticks gt yticks then begin
  yticks=xticks
  yrmx=yrmn+yticks*delr
endif else if yticks gt xticks then begin
  xticks=yticks
  xrmx=xrmn+xticks*delr
endif 

if xticks gt 6 then begin
  if xticks-2*fix(xticks/2) gt 0 then xticks=xticks+1
  yticks=xticks
  yrmx=yrmn+yticks*delr
  xrmx=xrmn+xticks*delr
  start: 
  if xticks gt 6 then begin
    xticks=xticks/2
    yticks=xticks
    xminor=2
    yminor=xminor
    goto,start
  endif 
endif 

xrange=[xrmn,xrmx]
yrange=[yrmn,yrmx]

;plot x-y
WIDGET_CONTROL, drawxy, GET_VALUE=windw
if wst.hardcopy(0) eq 0 then wset,windw

plot,xrange,yrange,/nodata,$
  xstyle=1,xticks=xticks,xrange=xrange,xminor=xminor,xtitle='X_gse',$
  ystyle=1,yticks=yticks,yrange=yrange,yminor=yminor,ytitle='Y_gse',$
  title='WIND orbit  x-y  '+string(pb5_ymdhms(timepb5(0,*)),format='(i8)') +$
  ' to '+string(pb5_ymdhms(timepb5(sz(1)-1,*)),format='(i8)')
  
oplot,xrange,[0,0],linestyle=1
oplot,[0,0],yrange,linestyle=1

rmoon=60.
xmoon=-rmoon+indgen(100)*2*rmoon/99
ymoon=sqrt(rmoon^2-xmoon^2)
oplot,xmoon,ymoon,linestyle=1 & oplot,xmoon,-ymoon,linestyle=1

oplot,orb_gse(*,0),orb_gse(*,1)
xyouts,orb_gse(0,0),orb_gse(0,1),/data,'0',$
  color=orange,charsize=1.5
xyouts,orb_gse(sz(1)-1,0),orb_gse(sz(1)-1,1),/data,'1',$
  color=orange,charsize=1.5
  
dens=total(d.swe_ionkpdat.n)/n_elements(d.swe_ionkpdat)
umag=total(d.swe_ionkpdat.v)/n_elements(d.swe_ionkpdat)  
press=1.6726e-24*dens*(umag*1.e5)^2*1e8
sibeck2,rhomp,xmp,rhobs,xbs,press=press,bz=0,noplt=1

;sibeck2,rhomp,xmp,rhobs,xbs,press=2.7,bz=0,noplt=1
oplot,xbs,rhobs,color=green & oplot,xbs,-rhobs,color=green
oplot,xmp,rhomp,color=green & oplot,xmp,-rhomp,color=green


;plot x-z
WIDGET_CONTROL, drawxz, GET_VALUE=windw
if wst.hardcopy(0) eq 0 then wset,windw
plot,xrange,yrange,/nodata,$
  xstyle=1,xticks=xticks,xrange=xrange,xminor=xminor,xtitle='X_gse',$
  ystyle=1,yticks=yticks,yrange=yrange,yminor=yminor,ytitle='Z_gse',$
  title='WIND orbit  x-z  '+string(pb5_ymdhms(timepb5(0,*)),format='(i8)') +$
  ' to '+string(pb5_ymdhms(timepb5(sz(1)-1,*)),format='(i8)')

oplot,xrange,[0,0],linestyle=1
oplot,[0,0],yrange,linestyle=1

oplot,orb_gse(*,0),orb_gse(*,2)
xyouts,orb_gse(0,0),orb_gse(0,2),/data,'0',$
  color=orange,charsize=1.5
xyouts,orb_gse(sz(1)-1,0),orb_gse(sz(1)-1,2),/data,'1',$
  color=orange,charsize=1.5
  
dens=total(d.swe_ionkpdat.n)/n_elements(d.swe_ionkpdat)
umag=total(d.swe_ionkpdat.v)/n_elements(d.swe_ionkpdat)  
press=1.6726e-24*dens*(umag*1.e5)^2*1e8
sibeck2,rhomp,xmp,rhobs,xbs,press=press,bz=0,noplt=1

;sibeck2,rhomp,xmp,rhobs,xbs,press=2.7,bz=0,noplt=1
oplot,xbs,rhobs,color=green & oplot,xbs,-rhobs,color=green
oplot,xmp,rhomp,color=green & oplot,xmp,-rhomp,color=green


if wst.hardcopy(0) then begin
  device,/close
  print,' ' & print,'printing hardcopy: ',wst.print_cmd
  spawn,wst.print_cmd
  set_plot,'x'
  wst.hardcopy=0 
endif

widget_control,orb_base,set_uvalue=info,/no_copy
if wst.hardcopy(0) eq 0 then $
  XMANAGER, "orbit_plot", orb_base, GROUP_LEADER = GROUP

end
