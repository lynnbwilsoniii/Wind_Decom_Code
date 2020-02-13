pro fplot,x_im_sz,y_im_sz,$
  pltype=fpl_sel_ind,$
  labl1=labl1,labl2=labl2,nlevels=nlevels,maxzval=maxzval,yrange=y_range,$
  ion=ion,xoplt=xoplt,oplotvx=oplotvx,lsscn=lsscn,vparc=vparc

;routine to produce contour plots of f(vpara,vperp),
;  plots of computed reduced F(vpara) and para and perp cuts of f.

common sharewidg,wa
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common onecount,f1ct,v1ct
common sharetriangl,xd,yd,zd,tr
common display,plotspinphase,plotsurface
common wstuff,wst
common swestuff,swest
common shareredfintg,xwinrc,xrangec,ywinrc,yrangec,lbl1,lbl2,xsize,ysize,$
                     xwinrF,xrangeF,ywinrF,yrangeF,xwin,ywin,vxl,vxr,ivxl,ivxr
common shareredf,sbase,button1,button2,draw1,draw2,field1,field2,$
  ndistribs,xc1,xc2,sgnslop,x1,F1,x2,F2
common sharefevents,fevents
common sharefplt,fpltc,fpltF
common mouse_ev,xrange_redf,xsize_redf

!p.charsize=1.25

if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0
if keyword_set(ion) eq 0 then ions=0 else ions=ion
if keyword_set(xoplt) eq 0 then xoplt=fltarr(2)
if keyword_set(oplotvx) eq 0 then oplotvx=0
if keyword_set(lsscn) eq 0 then lsscn=0

if ions eq 1 then begin
  vscal=1e7
  vfmt='(f5.1)'
  frange=[1.e-26,1.e-18]
  vlabl='100 km/s'
  fminor=2
  nopts=0. ;200e5
endif else begin
  vscal=1e8
  vfmt='(f5.1)'
  if vmax lt 2.e9 then frange=[1.e-31,1.e-24] else frange=[1.e-33,1.e-25]
  ;frange=[1.e-27,1.e-25]
  vlabl='1000 km/s'
  fminor=2
  nopts=0.  ;4e8

endelse


erase

start:


if keyword_set(labl1) eq 0 then labl1=' '
if keyword_set(labl2) eq 0 then labl2=' '

colordots=fix((110./142.)*!d.table_size)
color=[colordots,0]
;color=[175,225]
;color=[wst.clr_green,0]  ;225]

linestyle=[0,1]
symsize=[0.15,0.25]

;if keyword_set(fpl_sel_ind) eq 0 then fpl_sel_ind=[0,2]
;print,'entering fplot;  fpl_sel_ind= ',fpl_sel_ind
plot_set=fpl_sel_ind

nstack=n_elements(fpl_sel_ind)
posn=fltarr(4,nstack)
imsize=min([0.70*x_im_sz,(1.-0.20-(nstack-1)*0.02)*y_im_sz/nstack])
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

x_title=strarr(nstack)
x_title(0)='v!m!d#!n '+vlabl
;xticks=fix((vmax/vscal)/5)+1  ;4
xticks=4
;xrange=[-(fix((vmax/vscal)/5)+1)*5.,(fix((vmax/vscal)/5)+1)*5.];range of vx
xrange=[-vmax/vscal,vmax/vscal]
xminor=2;3

xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
xtickn=strarr(xticks+1,nstack)
for i=0,nstack-1 do begin
  xtickn(*,i)=replicate(string(32b),xticks+1)
endfor 
xtickn(*,0)=string(xtickv,format='(i4)')
no_erase=1+intarr(nstack)
no_erase(nstack-1)=0

z_title=strarr(nstack)
if swest.mag_3s_kp eq 0 then $
  z_title(nstack-1)=labl1+' ('+swest.wchmagdat(swest.mag_3s_kp)+')'

xtickf=strarr(nstack)
xtickf(0)=vfmt

stitle=strarr(nstack)
stitle(0)=labl2+'  pbin:'+string(swest.pbinselect,format='(i2)')+'deg'

;reflect zgrd about vperp=0
zreverse=fltarr(nx,ny)
for i=0,nx-1 do zreverse(i,*)=rotate(zgrd(i,*),2)
z=fltarr(nx,2*ny-1)
z(0,0)=zreverse
z(0,ny-1)=zgrd

;contour z in x(vpara), y(vperp) coordinates
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


;if swest.c_labels then c_labels=replicate(1,nc) else c_labels=replicate(0,nc)
c_labels=replicate(1,nc)
 
if keyword_set(maxzval) eq 0 then maxzval=max(z(where(z ne 0.)))
if keyword_set(y_range) eq 0 then y_range=frange 
;print,'keyword_set(y_range)',keyword_set(y_range),y_range

x=xrange(0)+indgen(nx)*(xrange(1)-xrange(0))/(nx-1)
vx=x*vscal
 
;yrange=[-(fix((vmax/vscal)/5)+1)*5.,(fix((vmax/vscal)/5)+1)*5.];range of vy
yrange=xrange

y=yrange(0)+indgen(2*ny-1)*(yrange(1)-yrange(0))/(2*ny-1-1)
;y=yrange(0)+indgen(my)*(yrange(1)-yrange(0))/(my-1)

vy=y*vscal
;yticks=fix((vmax/vscal)/5)+1;4
yticks=4

;time=string(format='(i2,":",i2,":",i2," UT")',hr,min,fix(sec))
time=''
;print,'contouring f(vpara,vperp)....'
dtm=strmid(wst.lzdate,0,8)+'_'+$
    strmid(wst.hms,0,2)+strmid(wst.hms,3,2)+strmid(wst.hms,6,2)
if wst.hardcopy then begin 
  pltfil=getenv('IDLSAV')+dtm+'.ps'  ;wst.print_flnm
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,$
    xoffset=1.0,yoffset=1.0,xsize=8.,ysize=8.,filename=pltfil,/color 
 
  posn(0,*)=posn(0,*)/x_im_sz
  posn(2,*)=posn(2,*)/x_im_sz
  posn(1,*)=posn(1,*)/y_im_sz
  posn(3,*)=posn(3,*)/y_im_sz

  posn(0,*)=0.2
  posn(2,*)=posn(0,*)+posn(3,*)-posn(1,*)
  
  dev=0
endif

for istack=0,nstack-1 do begin

  iplot=fpl_sel_ind(istack)
  position=posn(*,nstack-1-istack)
  xtitle=x_title(nstack-1-istack)
  xtickname=xtickn(*,nstack-1-istack)
  xtickformat=xtickf(nstack-1-istack)
  title=z_title(nstack-1-istack)
  subtitle=stitle(nstack-1-istack)
  noerase=no_erase(nstack-1-istack)
  if istack eq nstack-1 then xcharsize=1.0 else xcharsize=0.001

  case iplot of
  0: begin
       contour,z,x,y,levels=cl,max_value=maxzval,xrange=xrange,yrange=yrange,$
         xstyle=1,ystyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtickname=xtickname,yticks=yticks,$
         ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
         position=position,device=dev,title=title,xtitle=xtitle,$
         ytitle='v!m!dx!n '+vlabl,/follow,noerase=noerase,$
         subtitle=subtitle,xtickformat=xtickformat,ytickformat=vfmt,$
         xcharsize=xcharsize,c_labels=c_labels,yminor=xminor,xminor=xminor
       
       if swest.savez then begin
         ;sflnm=getenv('IDLSAV')+'z_x_y_F'+dtm+'.sav'
         ;save,filename=sflnm,z,x,y,F
         
         sflnm=getenv('IDLSAV')+dtm+'_fcontour.sav'
         ;sflnm=getenv('LOCALUSR')+'agu02/'+dtm+'_fcontour.sav'
         save,filename=sflnm,$
           z,x,y,cl,maxzval,xrange,yrange,$
           xticks,$
           xtickname,yticks,$
           position,dev,title,xtitle,$
           vlabl,$
           subtitle,xtickformat,vfmt,$
           xcharsize,c_labels,xminor,xminor
         print,'save file created: ',sflnm         
      endif
         
       ;overplot measured velocity space points
       if swest.hidepoints eq 0 then begin
          wpts=where(fm gt 0 and (vxm^2 + vym^2) gt (nopts)^2)
          if wpts(0) ne -1 then begin
            oplot,vxm(wpts)/vscal,vym(wpts)/vscal,psym=4,$
              symsize=symsize(wst.hardcopy),color=color(wst.hardcopy)
            oplot,vxm(wpts)/vscal,-vym(wpts)/vscal,psym=4,$
              symsize=symsize(wst.hardcopy),color=color(wst.hardcopy)
          endif
       endif
       oplot,[0,0],[0,0],psym=1
       xrangec=xrange
       yrangec=yrange
       xwinrc=[position(0),position(2)]/x_im_sz
       ywinrc=[position(1),position(3)]/y_im_sz
       lbl2=labl2
       
       if xoplt(0) ne 0 then begin
         for iopl=0,n_elements(xoplt)-1 do $
           oplot,[xoplt(iopl),xoplt(iopl)],yrange,linestyle=1
       endif 

       if lsscn then begin
         oplot,[vparc,vparc],yrange,linestyle=0,color=wst.clr_green
         b21=25./7. & vphisq=80. / 2.85  ;19980826
         ;b21=16./8. & vphisq=20. / 2.85  ;19990218
         
         vparalc=fltarr(n_elements(x))
         vperplc=fltarr(n_elements(x))
         j=-1
         for i=0,n_elements(x)-1 do begin
           if x(i) le vparc then begin
             j=j+1
             vparalc(j)=x(i)
             vperplc(j)=sqrt( ((vparalc(j) - vparc)^2 + vphisq)/(b21-1.) )
           endif
         endfor
         vparalc=vparalc(0:j)
         vperplc=vperplc(0:j)
         oplot,vparalc,vperplc,linestyle=0,color=wst.clr_green
         
       endif
               
;fpltc={z:z,x:x,y:y,cl:cl,maxzval:maxzval,xrange:xrange,yrange:yrange,$
;  xstyle:xstyle,ystyle:ystyle,xticks:xticks,$
;  xtickv:xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
;  xtickname:xtickname,yticks:yticks,$
;  ytickv:yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
;  device:dev,title:title,xtitle:xtitle,ytitle:'v!m!dx!n '+vlabl,$
;  subtitle:subtitle,xtickformat:xtickformat,ytickformat:vfmt,$
;  xcharsize:xcharsize,c_labels:c_labels,yminor:xminor,xminor:xminor,$
;  vxmeas:vxm(wpts)/vscal,vymeas:vym(wpts)/vscal,$
;  symsize:symsize(wst.hardcopy),color:color(wst.hardcopy)       
       
     endcase


  3: begin
       wne0=where(z ne 0)
       z(wne0)=z(wne0)-min(z(wne0))
       z(wne0)=median(z(wne0),3)
       surface,z,x,y,xrange=xrange,yrange=yrange,$
         xstyle=1,ystyle=1,xticks=xticks,xminor=xminor,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtickname=xtickname,yticks=yticks,$
         ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
         position=position,device=dev,title=title,xtitle=xtitle,$
         ytitle='v!m!dx!n '+vlabl,noerase=noerase,$
         subtitle=subtitle,xtickformat=xtickformat,ytickformat=vfmt,$
         xcharsize=xcharsize,charsize=2.0,ztitle='relative ln(f)'

     endcase

  1: begin
 
       wF=where(F gt 0)
       ;Frange=[10.^fix(alog10(min(F(wF)))),10.^fix(alog10(max(F(wF))))]
       Frange=[1.e-13,1.e-7]
       yticksF=Frange(1)-Frange(0)
       plot_io,x(wF),F(wF),xrange=xrange,xstyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtitle=xtitle,xtickname=xtickname,ytitle='reduced log!d10!n F',$
         title=title,position=position,device=dev,yrange=Frange,$
         ystyle=1,noerase=noerase,subtitle=subtitle,xminor=xminor,$
         xtickformat=xtickformat,xcharsize=xcharsize  ;,yticks=yticksF
         
        if lsscn then $
          oplot,[vparc,vparc],Frange,linestyle=0,color=wst.clr_green 
       
        xrange_redf =[!x.crange(0),!x.crange(1)]
        xsize_redf=[!d.x_size*!x.window(0),!d.x_size*!x.window(1)]
             
fpltF={x:x(wF),y:F(wF),xrange:xrange,xstyle:1,xticks:xticks,$
         xtickv:xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtitle:xtitle,xtickname:xtickname,ytitle:'reduced log!d10!n F',$
         title:title,device:dev,yrange:Frange,$
         ystyle:1,subtitle:subtitle,xminor:xminor,$
         xtickformat:xtickformat,xcharsize:xcharsize}
         
       xrangeF=xrange
       yrangeF=yrange
       xwinrF=[position(0),position(2)]/x_im_sz
       ywinrF=[position(1),position(3)]/y_im_sz
   
       if swest.F_integ eq 1 then begin
         if xoplt(0) ne 0 and xoplt(1) ne 0 and oplotvx eq 0 then begin
           ;xc1=xoplt(0)
           ;xc2=xoplt(1)
           redf_analyze,x,F,xc1
           oplot,[x1,x1],Frange,linestyle=1
           oplot,[x2,x2],Frange,linestyle=1
           for iopl=0,n_elements(xoplt)-1 do $
           oplot,[xoplt(iopl),xoplt(iopl)],Frange,linestyle=1,color=75
           ;stop
         endif else if xoplt(0) eq 0 and ndistribs ne 0 then begin
           redf_analyze,x,F,xc1
           oplot,[x1,x1],Frange,linestyle=1
           oplot,[x2,x2],Frange,linestyle=1
           ;stop
         endif else if oplotvx then begin
           for iopl=0,n_elements(xoplt)-1 do $
           oplot,[xoplt(iopl),xoplt(iopl)],Frange,linestyle=1,color=75
         endif
       endif
     endcase

  2:  begin
        ;if nstack eq 1 then x_style=1 else x_style=8
        ;if nstack eq 1 then title_p=' ' else title_p=title
        print,'position ',position
        plot_io,x(where(fpara ne 0)),fpara(where(fpara ne 0)),$
          xrange=xrange,xstyle=1,xticks=xticks,$
          xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
          xtitle=xtitle,xtickname=xtickname,yrange=y_range,$
          ystyle=1,ytitle='log!d10!nf',position=position,device=dev,$
          psym=0,noerase=noerase,title=title,subtitle=subtitle,xminor=xminor,$
          xtickformat=xtickformat,xcharsize=xcharsize,yminor=fminor

  
        oplot,y(where(fperp ne 0)),fperp(where(fperp ne 0)),$
          color=color(wst.hardcopy),linestyle=5

        
        oplot,v0cut(5:n_elements(v0cut)-1)/vscal,$
              f0cut(5:n_elements(v0cut)-1),psym=5,symsize=0.75
        oplot,v180cut(5:n_elements(v180cut)-1)/vscal,$
              f180cut(5:n_elements(v180cut)-1),psym=5,symsize=0.75
        
        oplot,v1ct(where(v1ct le vmax))/vscal,f1ct(where(v1ct le vmax)),$
          linestyle=1
        oplot,-v1ct(where(v1ct le vmax))/vscal,f1ct(where(v1ct le vmax)),$
          linestyle=1  
        
        if swest.savez then begin
         
         sflnm=getenv('IDLSAV')+dtm+'_fcuts.sav'
         ;sflnm=getenv('LOCALUSR')+'agu02/'+dtm+'_fcuts.sav'
         save,filename=sflnm,$
          fpara,x,$
          xrange,xticks,$
          xtitle,xtickname,y_range,$
          position,dev,$
         noerase,title,subtitle,xminor,$
         xtickformat,xcharsize,fminor,$
         fperp,y,$
         v0cut,vscal,f0cut,v180cut,f180cut,vmax,v1ct,f1ct
          
         print,'save file created: ',sflnm         
      endif
        
      endcase
  
  4: begin

       contour,z,x,y,levels=cl,max_value=maxzval,xrange=xrange,yrange=yrange,$
         xstyle=1,ystyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtickname=xtickname,yticks=yticks,$
         ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
         position=position,device=dev,title=title,xtitle=xtitle,$
         ytitle='v!m!dx!n '+vlabl,/follow,noerase=noerase,$
         subtitle=subtitle,xtickformat=xtickformat,ytickformat=vfmt,$
         xcharsize=xcharsize,c_labels=c_labels,yminor=xminor,xminor=xminor,$
         /nodata

       hidepoints_last=swest.hidepoints
       swest.hidepoints=1
       ;overplot measured velocity space points
       if swest.hidepoints eq 0 then begin
          wpts=where(fm gt 0 and (vxm^2 + vym^2) gt (nopts)^2)
          if wpts(0) ne -1 then begin
            oplot,vxm(wpts)/vscal,vym(wpts)/vscal,psym=4,$
              symsize=symsize(wst.hardcopy),color=color(wst.hardcopy)
            oplot,vxm(wpts)/vscal,-vym(wpts)/vscal,psym=4,$
              symsize=symsize(wst.hardcopy),color=color(wst.hardcopy)
          endif
       endif
       swest.hidepoints=hidepoints_last
       
       ;plot triangles
       sztr=size(tr)
       for i=0,sztr(2)-1 do oplot,xd(tr(*,i))*1e-8,yd(tr(*,i))*1e-8
       ;oplot,xd*1e-8,yd*1e-8,psym=4,$
       ;       symsize=symsize(wst.hardcopy),color=color(wst.hardcopy)
              
       oplot,v0cut/vscal,abs(v0cut/vscal)*sin(p0cut*!dtor),$
         psym=5,symsize=0.75,color=color(wst.hardcopy)
       oplot,v180cut/vscal,abs(v180cut/vscal)*sin(p180cut*!dtor),$
         psym=5,symsize=0.75,color=color(wst.hardcopy)
       print,p0cut,p180cut
       
       oplot,[0,0],[0,0],psym=1
       xrangec=xrange
       yrangec=yrange
       xwinrc=[position(0),position(2)]/x_im_sz
       ywinrc=[position(1),position(3)]/y_im_sz
       lbl=labl2
     endcase

endcase

  
               
endfor
swest.savez=0

if wst.hardcopy then begin   
  device,/close 
  set_plot,'x'
  print,' ' & print,'printing hardcopy: ',pltfil  ;wst.print_cmd
  spawn, 'lp '+pltfil  ;wst.print_cmd  ;
  wst.hardcopy=0
  clrtbl_indx

  goto,start
endif  

!p.charsize=1.00

;if plotsurface then surface_mod,zgrd

 end
