hcopy=0

goto, plott

flnm='plasma_density_19941227_v01.dat'
openr,lun,flnm,/get_lun
hrday=dblarr(50000)
plsmdens=fltarr(50000)
qfit=fltarr(50000)
pb5=lonarr(3)
dens=0.
wvint=0.
fit=0.
i=-1l
while not eof(lun) do begin
  readf,lun,pb5,dens,wvint,fit
  i=i+1
  hrday(i)=pb5(2)/3600000.d
  plsmdens(i)=dens
  qfit(i)=fit
endwhile
n=i
hrday=hrday(0:n-1)
plsmdens=plsmdens(0:n-1)
qfit=qfit(0:n-1)

plott: 
if hcopy eq 0 then window,0 else set_plot,'ps'

w=where(qfit ge 0.95)
!p.multi=[0,0,2,0,0]
x=hrday(w)
y=plsmdens(w)
plot,x,y,yrange=[0,10],yticks=5,yminor=2,ystyle=1,ytitle='plasma density',$
         xrange=[0,24],xticks=8,xminor=3,xstyle=1,psym=3,charsize=2.0,$
         xtitle='hour day '+strmid(flnm,strlen(flnm)-16,8)

y=qfit(w)
plot,x,y,yrange=[0,1],yticks=2,yminor=2,ystyle=1,ytitle='quality of fit',$
         xrange=[0,24],xticks=8,xminor=3,xstyle=1,psym=3,charsize=2.0,$
         xtitle='hour day '+strmid(flnm,strlen(flnm)-16,8)
!p.multi=0

if hcopy then begin
  print,'making hardcopy'
  device,/close
  set_plot,'x'
  spawn,'lpr idl.ps'
endif
end 
