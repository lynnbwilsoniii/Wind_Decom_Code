pro ctr_ep,x_im_sz,y_im_sz,$
  pltype=fpl_sel_ind,$
  labl1=labl1,labl2=labl2,ion=ion

;procedure to produce contour plots of f(energy,pitch angle)

common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common sharetriangl,xd,yd,zd,tr
common wstuff,wst
common swestuff,swest
common share_ctr_ep,elog,pmin,pmax,emin,emax,e0cut,e180cut

!p.charsize=1.25

if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0
if keyword_set(ion) eq 0 then ions=0 else ions=ion

erase
start:

if keyword_set(labl1) eq 0 then labl1=' '
if keyword_set(labl2) eq 0 then labl2=' '

colordots=fix((110./142.)*!d.table_size)
color=[colordots,0]
linestyle=[0,1]
symsize=[0.15,0.25]

plot_set=fpl_sel_ind
nstack=n_elements(fpl_sel_ind)
posn=fltarr(4,nstack)
imsize=min([0.70*x_im_sz,0.90*y_im_sz])
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

x_title='pitch angle'
xticks=4
xrange=[pmin,pmax]
xminor=3

xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
xtickn=replicate(string(32b),xticks+1)
xtickn(*)=string(xtickv,format='(i4)')
z_title=labl1 
stitle=labl2+'  pbin:'+string(swest.pbinselect,format='(i2)')+'deg' 

z=zgrd

;contour z in x(pitch), y(energy) coordinates
c_decade=swest.c_decade  ;0.75  ;0.5  ;contour spacing in units log10 
if keyword_set(nlevels) eq 0 then begin
  nc=fix(float(fix(max(z(where(z ne 0.)))+0.5)-$
               fix(min(z(where(z ne 0.)))-0.5))/c_decade+1)
  if nc gt 30 then $
    cl=float(fix(min(z(where(z ne 0.)))-0.5)) + 2*findgen(nc/2) $
  else $ 
    cl=float(fix(min(z(where(z ne 0.)))-0.5)) + indgen(nc)*c_decade
endif else begin
  nc=nlevels
  cl=float(fix(min(z(where(z ne 0.)))-0.5))+indgen(nc)*$
    (float(fix(max(z(where(z ne 0.)))+0.5))-$
     float(fix(min(z(where(z ne 0.)))-0.5)))/(nc-1)
endelse


if swest.c_labels then c_labels=replicate(1,nc) else c_labels=replicate(0,nc)
 
if keyword_set(maxzval) eq 0 then maxzval=max(z(where(z ne 0.)))

x=xrange(0)+indgen(nx)*(xrange(1)-xrange(0))/(nx-1)
vx=x
 
yrange=[emin,emax];range of vy
y=yrange(0)+indgen(ny)*(yrange(1)-yrange(0))/(ny-1)
vy=y
yticks=2
ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks
ytickn=replicate(string(32b),yticks+1)
ytickn(*)=string(ytickv,format='(i4)')
if elog then ytickn=10.^ytickn

iplot=fpl_sel_ind(0)
position=reform(posn(*))
xtitle=x_title

xtickformat='(i3)'
title=z_title
subtitle=stitle

contour,z,x,y,levels=cl,max_value=maxzval,xrange=xrange,yrange=yrange,$
         xstyle=1,ystyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtickname=xtickn,yticks=yticks,ytickname=ytickn,ytickv=ytickv,$
         position=position,device=dev,title=title,xtitle=xtitle,$
         ytitle='energy ev',/follow,$
         subtitle=subtitle,xtickformat=xtickformat,ytickformat='(i4)',$
         xcharsize=xcharsize,c_labels=c_labels,yminor=xminor,xminor=xminor
  
oplot,[90.,90.],yrange,linestyle=1
oplot,p0cut,e0cut,linestyle=1,color=colordots
oplot,p180cut,e180cut,linestyle=1,color=colordots


end