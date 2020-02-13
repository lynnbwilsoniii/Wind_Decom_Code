start:

hardcopy=1

savpath=getenv('IDLSAV')
filename=pickfile(/read,get_path=savpath,path=savpath,$
               filter='*fmulti',$
               title='IDLsav fmulti files')
restore,filename
help,fmulti
help,fmulti,/str

!p.multi=[0,2,3,0,0]
charsize=1.75

if hardcopy then begin
  pltfil=filename+'.ps'
  print,' ' & print,'making hardcopy..... ' 
  set_plot,'ps'
  device,/inches,$
   xoffset=0.5,xsize=8.,ysize=10.,filename=pltfil,/color
  dev=0 
endif else begin
  window,0,xsize=600,ysize=800 
  dev=1
endelse     
 
 
for ipl=0,n_elements(fmulti)-1 do begin

    x=fmulti(ipl).x
    y=fmulti(ipl).y
    yrange=fmulti(ipl).yrange
    xrange=fmulti(ipl).xrange
    xticks=fmulti(ipl).xticks
    subtitle=fmulti(ipl).subtitle
    xminor=fmulti(ipl).xminor
         
    wy=where(y gt 0)
    yticks=yrange(1)-yrange(0)
    plot_io,x(wy),y(wy),xrange=xrange,xstyle=1,xticks=xticks,$
         xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks,$
         title=title,device=dev,yrange=yrange,$
         ystyle=1,subtitle=subtitle,xminor=xminor,charsize=charsize
endfor

if hardcopy then begin
  device,/close 
  set_plot,'x'
  print,' ' & print,'printing hardcopy: ',pltfil
  spawn, 'lpr '+pltfil
  hardcopy=0
endif

!p.multi=0

goto,start

end
               