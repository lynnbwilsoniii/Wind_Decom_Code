pro fplot_fcutsbuff,x_im_sz,y_im_sz,$
  pltype=fpl_sel_ind,$
  labl1=labl1,labl2=labl2,nlevels=nlevels,maxzval=maxzval,yrange=y_range,$
  ion=ion,seqno=seqno

;routine to produce contour plots of f(vpara,vperp),
;  plots of computed reduced F(vpara) and para and perp cuts of f.

common sharewidg,wa
common display,plotspinphase,plotsurface
common wstuff,wst
common sharebuffer,fbuff,fcutsbuff


if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0

fpara=fcutsbuff(seqno).fpara
fperp=fcutsbuff(seqno).fperp
F=fcutsbuff(seqno).F
nx=n_elements(fpara)
ny=n_elements(fperp)/2
vmax=fcutsbuff(seqno).vmax
f0cut=fcutsbuff(seqno).f0cut
v0cut=fcutsbuff(seqno).v0cut
f180cut=fcutsbuff(seqno).f180cut
v180cut=fcutsbuff(seqno).v180cut



if keyword_set(ion) eq 0 then ions=0 else ions=ion

if ions eq 1 then begin
  vscal=1e7
  vfmt='(f5.1)'
  frange=[1.e-26,1.e-18]
  vlabl='100 km/s'
  fminor=2
  nopts=200e5
endif else begin
  vscal=1e8
  vfmt='(f5.1)'
  frange=[1.e-31,1.e-25] ;[1.e-32,1.e-24]
  vlabl='1000 km/s'
  fminor=2
  nopts=4e8

endelse

erase

start:

!p.charsize=1.25

if keyword_set(labl1) eq 0 then labl1=' '
if keyword_set(labl2) eq 0 then labl2=' '

;color=[225,0]
color=[175,225]
linestyle=[0,1]
symsize=[0.15,0.25]

;if keyword_set(fpl_sel_ind) eq 0 then fpl_sel_ind=[0,2]
;print,'entering fplot;  fpl_sel_ind= ',fpl_sel_ind
plot_set=fpl_sel_ind

nstack=n_elements(fpl_sel_ind)
posn=fltarr(4,nstack)
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

x_title=strarr(nstack)
x_title(0)='v!m!d#!n '+vlabl
xticks=4
xrange=[-vmax,vmax]/vscal;range of vx
xminor=2

xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
xtickn=strarr(xticks+1,nstack)
for i=0,nstack-1 do begin
  xtickn(*,i)=replicate(string(32b),xticks+1)
endfor 
xtickn(*,0)=string(xtickv,format='(i4)')
no_erase=1+intarr(nstack)
no_erase(nstack-1)=0

z_title=strarr(nstack)
z_title(nstack-1)=labl1

xtickf=strarr(nstack)
xtickf(0)=vfmt

stitle=strarr(nstack)
stitle(0)=labl2


if keyword_set(y_range) eq 0 then y_range=frange 

x=xrange(0)+indgen(nx)*(xrange(1)-xrange(0))/(nx-1)
vx=x*vscal
 
yrange=[-vmax,vmax]/vscal;range of vy

y=yrange(0)+indgen(2*ny-1)*(yrange(1)-yrange(0))/(2*ny-1-1)
;y=yrange(0)+indgen(my)*(yrange(1)-yrange(0))/(my-1)

vy=y*vscal
yticks=4

;time=string(format='(i2,":",i2,":",i2," UT")',hr,min,fix(sec))
time=''
;print,'contouring f(vpara,vperp)....'

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
  xtickformat=xtickf(nstack-1-istack)
  title=z_title(nstack-1-istack)
  subtitle=stitle(nstack-1-istack)
  noerase=no_erase(nstack-1-istack)
  if istack eq nstack-1 then xcharsize=1.0 else xcharsize=0.001

  case iplot of
 

  1: begin
 
       wF=where(F gt 0)
       ;Frange=[10.^fix(alog10(min(F(wF)))),10.^fix(alog10(max(F(wF))))]
       Frange=[1.e-12,1.e-7]
       yticksF=Frange(1)-Frange(0)
       plot_io,x(wF),F(wF),xrange=xrange,xstyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         xtitle=xtitle,xtickname=xtickname,ytitle='reduced log!d10!n F',$
         title=title,position=position,device=dev,yrange=Frange,$
         ystyle=1,noerase=noerase,subtitle=subtitle,$
         xtickformat=xtickformat,xcharsize=xcharsize  ;,yticks=yticksF

     endcase

  2:  begin
        ;if nstack eq 1 then x_style=1 else x_style=8
        ;if nstack eq 1 then title_p=' ' else title_p=title

        plot_io,x(where(fpara ne 0)),fpara(where(fpara ne 0)),$
          xrange=xrange,xstyle=1,xticks=xticks,$
          xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
          xtitle=xtitle,xtickname=xtickname,yrange=y_range,$
          ystyle=1,ytitle='log!d10!nf',position=position,device=dev,$
          psym=0,noerase=noerase,title=title,subtitle=subtitle,$
          xtickformat=xtickformat,xcharsize=xcharsize,yminor=fminor

  
        oplot,y(where(fperp ne 0)),fperp(where(fperp ne 0)),$
          color=color(wst.hardcopy),linestyle=5

        oplot,v0cut/vscal,f0cut,psym=5,symsize=0.75
        oplot,v180cut/vscal,f180cut,psym=5,symsize=0.75


      endcase
   else:
endcase
endfor


if wst.hardcopy then begin   
  device,/close
 
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  wst.hardcopy=0
  goto,start
endif


 end
