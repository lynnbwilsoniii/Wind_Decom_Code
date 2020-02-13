pro fplot_isee,x_im_sz,y_im_sz,pltype=fpl_sel_ind,hardcopy=hardcopy

;routine to produce contour plots of f(vpara,vperp),
;  plots of computed reduced F(vpara) and para and perp cuts of f.

;common iseestuff,iseemdat,iseefdat,iseerefsec
common iseestuff,iseefdat
common sharefredf,zgrd,F,nx,ny,vmax,fm,vxm,vym,fi,vxi,vyi,pm,vm,bf ;fredf data
common wstuff,wst

print,'entering fplot;  fpl_sel_ind= ',fpl_sel_ind

erase

start:

!p.charsize=1.25

color=[wst.clr_orange,0]
linestyle=[0,1]

;print,'fid,fta(fid)',fid,fta(fid)
if iseefdat.mode eq 0 then mode=2 else mode=iseefdat.mode
if iseefdat.format eq 0 then format=2 else format=iseefdat.format
inset=iseefdat.inset & ff=iseefdat.f  & spinp=iseefdat.spin  
uout=iseefdat.uout*1e-5 & cc=iseefdat.cc  & counts=iseefdat.unc
b=bf

if keyword_set(fpl_sel_ind) eq 0 then fpl_sel_ind
plot_set=fpl_sel_ind
 
nstack=n_elements(fpl_sel_ind)
posn=fltarr(4,nstack)
imsize=min([0.70*x_im_sz,(1.-0.20-(nstack-1)*0.02)*y_im_sz/nstack])
x_off=0.25*x_im_sz   ;0.2*x_im_sz
y_off=y_im_sz/2-nstack*imsize/2
y_sep=0.02*y_im_sz
posn(0,*)=x_off
posn(2,*)=x_off+imsize
for i=0,nstack-1 do begin
  posn(1,i)=y_off+i*(imsize+y_sep)
  posn(3,i)=y_off+imsize+i*(y_sep+imsize)
endfor
dev=1

x_title=strarr(nstack)
x_title(0)='v!m!d#!n 1000 km/s';='vpara 1000 km/s'
xticks=6
xrange=[-vmax,vmax]*1e-8;range of vx
xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
xtickn=strarr(xticks+1,nstack)
for i=0,nstack-1 do begin
  xtickn(*,i)=replicate(string(32b),xticks+1)
endfor 
xtickn(*,0)=string(xtickv,format='(i4)')
no_erase=1+intarr(nstack)
no_erase(nstack-1)=0

z_title=strarr(nstack)
z_title(nstack-1)='ISEE1 elecs'
hour_hms,double(iseefdat.tpb5(2))/3600000.d,hms
datetime= yrmoda(iseefdat.tpb5) + ' ' +$
  string(iseefdat.tpb5(1),format='(i3)') + ' ' + hms
stitle=strarr(nstack)
stitle(0)=datetime

;reflect zgrd about vperp=0
zreverse=fltarr(nx,ny)
for i=0,nx-1 do zreverse(i,*)=rotate(zgrd(i,*),2)
z=fltarr(nx,nx)
z(0,0)=zreverse
z(0,ny-1)=zgrd
my=nx

maxzval=max(z(where(z ne 0.)))

;contour z in x(vpara), y(vperp) coordinates
c_decade=0.75  ;0.5  ;1.0
nc=$
  (fix(max(z(where(z ne 0.)))+0.5)-fix(min(z(where(z ne 0.)))-0.5))/c_decade+1
cl=float(fix(min(z(where(z ne 0.)))-0.5)) + findgen(nc)*c_decade   ;set contour levels

x=xrange(0)+indgen(nx)*(xrange(1)-xrange(0))/(nx-1)
vx=x*1e8
 
yrange=[-vmax,vmax]*1e-8;range of vy
y=yrange(0)+indgen(my)*(yrange(1)-yrange(0))/(my-1)
vy=y*1e8
yticks=6

print,'contouring f(vpara,vperp)....'

if wst.hardcopy then begin
  pflnm=getenv('IDLSAV')+'idl.ps' 
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=8.,ysize=8.,filename=pflnm,/color

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
  title=z_title(nstack-1-istack)
  noerase=no_erase(nstack-1-istack)
  subtitle=stitle(nstack-1-istack)

  case iplot of
  0: begin

       contour,z,x,y,levels=cl,max_value=maxzval,xrange=xrange,yrange=yrange,$
         xstyle=1,ystyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtickname=xtickname,yticks=yticks,$
         ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
         position=position,device=dev,title=title,xtitle=xtitle,$
         ytitle='v!m!dx!n 1000 km/s',/follow,noerase=noerase,$
         subtitle=subtitle
       ;overplot z ne 0 points
         ;xx=fltarr(nx,my) & yy=xx
         ;for i=0,nx-1 do yy(i,*)=y & for j=0,my-1 do xx(*,j)=x
         ;wlt0=where(z lt 0) & oplot,xx(wlt0),yy(wlt0),psym=3

       ;overplot measured velocity space points
          oplot,vxm(where(fm gt 0))*1e-8,vym(where(fm gt 0))*1e-8,psym=3,$
            symsize=2
          oplot,vxm(where(fm gt 0))*1e-8,-vym(where(fm gt 0))*1e-8,psym=3,$
            symsize=2

       ;overplot interpolated velocity space points
         ;oplot,vxi(where(fi gt 0))*1e-8,vyi(where(fi gt 0))*1e-8,$
           ;psym=2
         ;oplot,vxi(where(fi gt 0))*1e-8,-vyi(where(fi gt 0))*1e-8,$
           ;psym=2

     endcase

  1: begin
 
       wF=where(F gt 0)
       Frange=[min(F(wF)),max(F(wF))]
       plot_io,x(wF),F(wF),xrange=xrange,xstyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtitle=xtitle,xtickname=xtickname,ytitle='reduced log!d10!n F',$
         title=title,position=position,device=dev,yrange=[1.e-14,1.e-7],$
         ystyle=1,yticks=7,noerase=noerase,subtitle=subtitle

     endcase

  2: begin

       jplt_ds=1
       nvmx=iseefdat.inset
       ndet=6 & nsect=6
       Esp=2.85e-16*iseefdat.cc(indgen(nvmx))^2
       dir_vx=dblarr(nvmx,ndet*nsect)
       dir_vy=dblarr(nvmx,ndet*nsect)
       dir_vz=dblarr(nvmx,ndet*nsect)
       fe=fltarr(18,ndet*nsect)
       fe(*,*)=iseefdat.f
       v0=fltarr(nvmx) &   v180=fltarr(nvmx)
       f0=fltarr(nvmx) &   f180=fltarr(nvmx)
       ph0=fltarr(nvmx) &   ph180=fltarr(nvmx)
       th0=fltarr(nvmx) &   th180=fltarr(nvmx)
       counts_eds,mode,format,inset,cc,counts,ff,$
         spinp,b,cntsm,fm,Em,dx,dy,dz
       syz=size(cntsm)  ; syz(1)=nv=inset  syz(2)=ndet   syz(3)=nsect
       ;cntsp(0:syz(1)-1,0:syz(2)-1,0:syz(3)-1)=cntsm
       Esp(0:syz(1)-1)=Em
       dir_vx(0:syz(1)-1,0:syz(2)*syz(3)-1)=-dx
       dir_vy(0:syz(1)-1,0:syz(2)*syz(3)-1)=-dy
       dir_vz(0:syz(1)-1,0:syz(2)*syz(3)-1)=-dz
       dir_b=b/sqrt(total(b*b))
       phb=atan(dir_b(1),dir_b(0))*180./!pi
       if phb lt 0 then phb=phb+360.
       thb=asin(dir_b(2))*180./!pi

       for i=0,syz(1)-1 do begin 
         a0=min(acos(dir_vx(i,*)*dir_b(0)+dir_vy(i,*)*dir_b(1)+$
           dir_vz(i,*)*dir_b(2))*180./!pi,imn)
         a180=max(acos(dir_vx(i,*)*dir_b(0)+dir_vy(i,*)*dir_b(1)+$
           dir_vz(i,*)*dir_b(2))*180./!pi,imx)
         v0(i)=iseefdat.cc(i)*1e-8
         f0(i)=fe(i,imn)
         ph0(i)=atan(dir_vy(i,imn),dir_vx(i,imn))*180./!pi
         if ph0(i) lt 0 then ph0(i)=ph0(i)+360.
         th0(i)=asin(dir_vz(i,imn))*180./!pi
         v180(i)=-iseefdat.cc(i)*1e-8
         f180(i)=fe(i,imx)
         ph180(i)=atan(dir_vy(i,imx),dir_vx(i,imx))*180./!pi
         if ph180(i) lt 0 then ph180(i)=ph180(i)+360.
         th180(i)=asin(dir_vz(i,imx))*180./!pi 
       endfor
       ve=[v180,reverse(v0)]
       fe=[f180,reverse(f0)]
       wfe=where(fe gt 0)
       phe=[ph180,reverse(ph0)]
       the=[th180,reverse(th0)]
       print,' ' & print,'plotting parallel and perpendicular cuts...'

       if nstack eq 1 then x_style=1 else x_style=8
       if nstack eq 1 then title_p=' ' else title_p=title
       plot_io,ve(wfe),fe(wfe),xrange=xrange,xstyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtitle=xtitle,xtickname=xtickname,yrange=[1.e-32,1.e-24],$
         ystyle=1,ytitle='log!d10!nf',position=position,device=dev,$
         psym=5,noerase=noerase,title=title_p,symsize=0.75,subtitle=subtitle
       z12=z(nx/2-1:nx/2,*) & fperp=fltarr(my)
       for j=0,my-1 do begin
         if z12(0,j) eq 0 and z12(1,j) ne 0 then fperp(j)=z12(1,j) 
         if z12(0,j) ne 0 and z12(1,j) eq 0 then fperp(j)=z12(0,j) 
         if z12(0,j) ne 0 and z12(1,j) ne 0 then fperp(j)=(z12(0,j)+z12(1,j))/2
       endfor
       wz=where(fperp ne 0)
       ;oplot,y(wz),exp(fperp(wz)),color=color(wst.hardcopy),$
        oplot,y(wz),10.^(fperp(wz)),color=color(wst.hardcopy),$
          linestyle=linestyle(wst.hardcopy)
       fpara=fltarr(nx)
       for j=0,nx-1 do begin
         if z(j,my/2-1) eq 0 and z(j,my/2) ne 0 then fpara(j)=z(j,my/2) 
         if z(j,my/2-1) ne 0 and z(j,my/2) eq 0 then fpara(j)=z(j,my/2-1) 
         if z(j,my/2-1) ne 0 and z(j,my/2) ne 0 then $
           fpara(j)=(z(j,my/2-1)+z(j,my/2) )/2
       endfor
       wzp=where(fpara ne 0)
       ;oplot,y(wzp),exp(fpara(wzp))
       oplot,y(wzp),10.^(fpara(wzp))

       ;f1c=fltarr(syz(1)) & f1c=onecount(1,0:syz(1)-1) 
       ;v1c=fltarr(syz(1)) & v1c=sqrt(onecount(0,0:syz(1)-1)/2.85e-16)*1e-8
       ;oplot,[-v1c,reverse(v1c)],[f1c,reverse(f1c)],psym=3

       if nstack eq 1 then begin
  
         xyouts,!x.window(0)+0.05,$
           !y.window(0)+0.92*(!y.window(1)-!y.window(0)),$
           'opposing detector electron velocity angles phi,theta',$
           alignment=0.0,/normal
 
         xyouts,!x.window(0)+0.05,$
           !y.window(0)+0.89*(!y.window(1)-!y.window(0)),$
           'nearest magnetic field phi,theta '+string(format='(i3)',phb)+$
           ','+string(format='(i3)',thb)+' deg',alignment=0.0,/normal

         p=string(format='(i3)',[phe(0),phe(n_elements(ve)/2-1),$
           phe(n_elements(ve)/2),phe(n_elements(ve)-1)])
         t=string(format='(i3)',[the(0),the(n_elements(ve)/2-1),$
           the(n_elements(ve)/2),the(n_elements(ve)-1)])
         v=[ve(0),ve(n_elements(ve)/2-1),ve(n_elements(ve)/2),$
           ve(n_elements(ve)-1)]
         align=[0.0,1.0,0.0,1.0]
         for i=0,3 do xyouts,!x.window(0)+$
            ((v(i)-!x.crange(0))/(!x.crange(1)-!x.crange(0)))*$
            (!x.window(1)-!x.window(0)),!y.window(0)+0.96*$
            (!y.window(1)-!y.window(0)),/normal,p(i)+','+t(i),alignment=align(i)

         axis,xaxis=1,xticks=3,xtickv=$
           [ve(0),ve(n_elements(ve)/2-1),ve(n_elements(ve)/2),$
           ve(n_elements(ve)-1)],xtickname=replicate(string(32b),4),$
           xtitle=datetime
       endif

     endcase

  endcase

endfor


if wst.hardcopy then begin   
  device,/close
  set_plot,'x'
  print,' ' & print,'printing hardcopy.....'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  wst.hardcopy=0
  clrtbl_indx
  goto,start

endif

 end
