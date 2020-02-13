;plots reduced F on velocity-time grid 
;data stored as idl save file

COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
common colorr,clrtbl,clrgrn,clrtbl_hc,clrgrn_hc


;set selected color table and line color
  ;clrtbl_indx,clrtbl,clrgrn,clrtbl_hc,clrgrn_hc

filename1='/home/u3rjf/idlsav/19941221_11:36:6:11:42:14.redf'
filename2='/home/u3rjf/idlsav/19941202_3:53:46:4:4:31.redf'
filename3='/home/u3rjf/idlsav/19941201_19:27:47:19:42:19.redf'

path=getenv('IDLSAV')
filename=pickfile(/read,get_path=path,path=path,$
               filter='*.redf',$
               title='Reduced Distribution Files')

hardcopy=1
start:

restore,filename
help,redfbuff
help,redfbuff,/str

siz=size(redfbuff.F)
vmax=redfbuff(0).vmax/1e8
ny_in=siz(1)
nx_in=siz(2)

yrange_in=[-vmax,vmax]
y_in=yrange_in(0)+indgen(ny_in)*(yrange_in(1)-yrange_in(0))/(ny_in-1)
xrange_in=$
  [redfbuff(0).pb5tim(2)/3600000.d,redfbuff(nx_in-1).pb5tim(2)/3600000.d]
x_in=xrange_in(0)+indgen(nx_in)*(xrange_in(1)-xrange_in(0))/(nx_in-1)


;---------- select interval ------------------------------------------------

jx0=0   
jx1=nx_in-1
jy0=0 
jy1=ny_in-1 
az=315
if filename eq filename2 then begin
   jx0=17 & jx1=36 
   jy0=25 
   jy1=64;103
   az=315
endif
if filename eq filename1 then begin
   jx0=13 & jx1=43;nx_in-1 
   jy0=64 & jy1=110 ;ny_in-1
   az=145
endif
if filename eq filename3 then begin
   ;jx0=17 & jx1=36 
   jy0=30 
   jy1=64
   az=315
endif   
xrange=[x_in(jx0),x_in(jx1)]
yrange=[y_in(jy0),y_in(jy1)]
x=x_in(jx0:jx1)
y=y_in(jy0:jy1)
z=fltarr(jx1-jx0+1,jy1-jy0+1)
z=transpose(redfbuff(jx0:jx1).F(jy0:jy1))

    
xticks=4
xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
xminor=2
if filename eq filename3 then begin
  xtickv=[28.,30.,32.,34.,36.,38.,40.,42.]/60.+19.
  xticks=n_elements(xtickv)-1
  xminor=2
endif
hour_hms,xtickv,xtickn
xtickname=strmid(xtickn,3,5)
xtitle=string(redfbuff(jx0).date,format='(i8)')+' '+redfbuff(jx0).hms


yticks=4
ytickv=yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks
ytickname=string(yrange(0)+indgen(yticks+1)*(yrange(1)-yrange(0))/yticks,$
  format='(f5.1)')
ytitle='v!m!d#!n'

wne0=where(z ne 0)
z(wne0)=alog10(z(wne0))
maxzval=-0.0001

color=[175,0]


if hardcopy then begin
  pflnm=getenv('IDLSAV')+'redf_surface.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=7.,filename=pflnm
endif

if hardcopy ne 1 then window,0
surface,z,x,y,xstyle=1,ystyle=4,zstyle=4,xtickv=xtickv,xtitle=xtitle,$
  xtickname=xtickname,max_value=maxzval,charsize=1.25,$
  xrange=xrange,yrange=yrange,xticks=xticks,xminor=xminor,$
  az=az  

if hardcopy then begin
  device,/close
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
  set_plot,'x'
endif

if hardcopy then begin
  pflnm=getenv('IDLSAV')+'idl.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=6.,ysize=6.,filename=pflnm
endif

if hardcopy ne 1 then window,2
clinc=0.50  ;0.75
nc=fix((fix(max(z(wne0))+0.5)-fix(min(z(wne0))-0.5) ) / clinc)
cl=float(fix(min(z(wne0))-0.5)) + findgen(nc) * clinc
contour,z,x,y,xstyle=1,ystyle=1,xtickv=xtickv,$
  xtitle=xtitle,ytitle='log!d10!nf',$
  xtickname=xtickname,max_value=maxzval,levels=cl,charsize=1.25,$
  xrange=xrange,xticks=xticks,xminor=xminor,$
  yrange=yrange,yticks=yticks,ytickname=ytickname

for i=0,n_elements(x)-1 do begin
  consecutiv_pos=0
  ypos=intarr(n_elements(y))
  for j=1,n_elements(y)-1 do begin
    if z(i,j) ne 0 and z(i,j-1) then begin
      slope=z(i,j)-z(i,j-1)
      if slope gt 0 then begin
        ypos(j)=1
        consecutiv_pos=consecutiv_pos+1
      endif 
    endif 
  endfor
  if consecutiv_pos ge 6 then ypos(*)=0
  for j=1,n_elements(y)-1 do begin
    if ypos(j) eq 1 and y(j) lt 10 then $
      oplot,[x(i),x(i)],[y(j),y(j)],psym=1,color=color(hardcopy) 
  endfor

endfor

if hardcopy then begin
  device,/close
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp '+pflnm
endif


;------------------------- do image -------------------------------------------

if hardcopy then begin
  pflnm=getenv('IDLSAV')+'redf_image.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps',/interpolate
  device,/portrait,/color,/inches,ysize=5.0,yoffset=5.0,bits_per_pixel=8, $
      /helvetica,filename=pflnm

  ;device,/inches,xoffset=1.0,yoffset=1.0,xsize=6.,ysize=6.,filename=pflnm,$
  ;bits_per_pixel=8,/color
endif

if hardcopy ne 1 then window,1
npl=1
pos=fltarr(4,npl)
pos,npl,pos
pos = [.1,.1,.8,.9]
title='SWE electrons reduced f'
charsize=1.0
charthick=1.0
xcharsize=1.0

mn=min(z(where(z ne 0)),max=mx)
;scale non-zero elements of image array to byte scale
   g=z
   w0=where(z eq 0)
   g=bytscl(z,min=mn,max=mx,top=!d.table_size-2);scale to  colors
   if w0(0) ne -1 then g(w0)=0


img_redf_tim,g,y,x,pos=pos(*,0),zmn=mn, zmx=mx, rlbl=' ',ytickn=ytickname,$
  ytitle=ytitle,ytickv=ytickv,xrange=xrange, xtickv=xtickv,xtickn=xtickname,$
  subtitle=' ',xticks=xticks,xtitle=xtitle,title=title,xminor=xminor,$
  ispin_gaps=0,charsize=charsize,charthick=charthick,xcharsize=xcharsize,$
  hardcopy=hardcopy

if hardcopy then begin
  device,/close
  set_plot,'x'
  print,' ' & print,'printing hardcopy file ',pflnm
  spawn, 'lp -d leptek '+pflnm
  hardcopy=0
  loadct,18
  goto, start
endif

  
end
