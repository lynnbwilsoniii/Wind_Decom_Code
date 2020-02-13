COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

loadct,18
multi,256/float(!d.table_size) 
r_orig(!d.table_size-1)=255     ;white plot axes, etc
g_orig(!d.table_size-1)=255
b_orig(!d.table_size-1)=255
tvlct,r_orig,g_orig,b_orig
!p.background=0                      ;black background

n_colors=!d.table_size-1

rcvr='tnr'
case rcvr of
'rad1':begin
flow=20
fhigh=1040
fspace=4.
funit='kHz'
rcvr12='R1'
linlog=0
        end
'rad2': begin
flow=1.075
fhigh=13.825
fspace=.050
funit='MHz'
rcvr12='R2'
linlog=0
        end
'tnr':  begin
flow=4.
fhigh=245.146
fspace=.188144e-1
funit='kHz'
linlog=1
        end
endcase
start:
ymd=0l
hms=0l
yymmdd1=0l
read,' Enter start YYMMDD :',yymmdd1
hour1=' '
read,' Enter start time in hours [0]',hour1
if (hour1 eq '') then begin
hour1=0.
endif else begin
hour1=float(hour1)
endelse
yymmdd2=0l
read,' Enter stop YYMMDD :',yymmdd2
hour2=' '
read,' Enter stop time in hours [24]',hour2
if (hour2 eq '') then begin
hour2=24.
endif else begin
hour2=float(hour2)
endelse
fhz1=' '
fhz2=' '
print,' Enter low frequency in ',funit,' [',flow,']: '
read,fhz1
print,' Enter high frequency in ',funit,' [',fhigh,']: '
read,fhz2
if(fhz1 eq '') then begin
fhz1=flow
endif else begin
fhz1=float(fhz1)
endelse
if(fhz2 eq '') then begin
fhz2=fhigh
endif else begin
fhz2=float(fhz2)
endelse
if(rcvr eq 'tnr') then begin
chan1=fix((alog10(fhz1)-alog10(flow))/fspace)
chan2=fix((alog10(fhz2)-alog10(flow))/fspace)
fhz1=alog10(flow)+chan1*fspace
fhz2=alog10(flow)+chan2*fspace
endif else begin
chan1=fix((fhz1-flow)/fspace)
chan2=fix((fhz2-flow)/fspace)
fhz1=flow+chan1*fspace
fhz2=flow+chan2*fspace
endelse
ydim=(chan2-chan1)+1
av_interval=' '
read,' Enter averaging interval in minutes [1]:',av_interval
if(av_interval eq '') then begin
av_interval=60
endif else begin
av_interval=fix(av_interval)*60
endelse
ih1=fix(hour1)*10000l
ih2=fix(hour2)*10000l
iday=0l
isec=0l
doy1=0l
diff,(yymmdd1/10000l)*10000l+100l,0l,yymmdd1,0l,doy1,isec
diff,yymmdd1,ih1,yymmdd2,ih2,iday,isec
xdim=fix((iday*86400+isec)/av_interval)
; read data
diff,yymmdd1,0l,yymmdd2,0l,iday,isec
ind1=fix(hour1)*60
ind2=fix(hour2)*60-1+iday*1440
hold=fltarr(1440*(iday+1),ydim)
yymmdd=yymmdd1
for dayno=0,iday do begin
yymmdda=strtrim(string(yymmdd),1)
if(rcvr eq 'tnr')$ 
then file=getenv('WAVESTNR')+'tnr'+yymmdda else $
if(rcvr eq 'rad1')$
then file='/home/waves1/'+rcvr+'/'+rcvr12+yymmdda else $
if (rcvr eq 'rad2')$
then file='/home/waves2/'+rcvr+'/'+rcvr12+yymmdda 
restore,filename=file
i1=dayno*1440
i2=i1+1439
hold(i1:i2,0:*)=arrayb(0:1439,chan1:chan2)
addymd,yymmdd,1l
endfor
; form array
array=rebin(hold(ind1:ind2,0:*),xdim,ydim)
stop
if(rcvr eq 'tnr') then begin
lt1=where(array lt 1.)
array(lt1)=1.
array=10.*alog10(array)
endif else begin
array=(array-3.)/4.>0.
endelse
plot_data:
scale1=0.
scale2=10.
nticks=10
;******
if(yymmdd1 eq yymmdd2) then begin
datefm,yymmdd1,date
endif else begin
datefm,yymmdd1,date1
datefm,yymmdd2,date2
date=date1+' to '+date2
endelse
hour=findgen(xdim+1)*av_interval/3600.+hour1
hour2=hour1+float(xdim)*av_interval/3600.
hints=fix(hour2-hour1)
xlabel='GMT (HRS)'
if(hints gt 24) then begin
htics=fix(float(hints)/12.+1.)
if(htics eq 5) then htics=6
if(htics gt 6) then htics=24
midnoon=where((hour mod htics) eq 0)
tickv=hour(midnoon)
if(htics lt 24) then $
hlabels=strtrim(string(fix(hour(midnoon)) mod 24),1)$
else hlabels=strtrim(string(doy1+indgen(iday+2)),1)
if(htics gt 6) then xlabel='DOY'
ok=size(tickv)
hints=ok(1)-1
endif else begin
hrs=where((hour mod 1.) eq 0)
tickv=hour(hrs)
hlabels=strtrim(string(fix(hour(hrs)) mod 24),1)
ok=size(tickv)
hints=ok(1)-1
endelse
freqlo=findgen(ydim)*fspace+fhz1
if(rcvr eq 'tnr') then freqlo=10.^freqlo
colors=indgen(n_colors)
bar=intarr(n_colors,5)
for j=0,4 do begin
bar(*,j)=colors
endfor
bar2=rebin(bar,2*n_colors,20)
set_plot,'x'
loadct,18
!p.background=n_colors
!p.color=0
erase
main:
iplt=wmenu(['RAD MENU','Screen','Hardcopy','Zoom',$
'New scale','New Day','Quit'],$
title=0,init=1)
; ***** New Plot ************************************
if(iplt eq 1) then begin
replot:
erase
contour,bar2,position=[.2,.075,.8,.1],$
xrange=[scale1,scale2],xticks=nticks,$
xstyle=1,ystyle=4,$
xtitle='intensity (dB) relative to background',/nodata,/noerase
px=!x.window*!d.x_vsize
py=!y.window*!d.y_vsize
sx=px(1)-px(0)+1
sy=py(1)-py(0)+1
tv,poly_2d(bytscl(bar2),[[0,0],[2*n_colors/sx,0]],[[0,20/sy],$
[0,0]],0,sx,sy),px(0),py(0)
contour,array,hour(0:xdim-1),freqlo,position=[.1,.2,.9,.5],$
/noerase,/nodata,xstyle=4,ystyle=4,ticklen=-.01,ytype=linlog
px=!x.window*!d.x_vsize
py=!y.window*!d.y_vsize
sx=px(1)-px(0)+1
sy=py(1)-py(0)+1
arrayb=bytscl(array,min=scale1,max=scale2,top=!d.table_size-2)
tv,poly_2d(arrayb,$
[[0,0],[xdim/sx,0]],[[0,ydim/sy],$
[0,0]],0,sx,sy),px(0),py(0)
contour,array,hour(0:xdim-1),freqlo,position=[.1,.2,.9,.5],$
/noerase,/data,xstyle=1,ystyle=1,ytype=linlog,$
xtitle=xlabel,xticks=hints,xtickv=tickv,xtickname=hlabels,$
title=date,ytitle=funit,ticklen=-.01,/nodata
goto,main
endif
;***** Hardcopy ********************************************
if(iplt eq 2) then begin
jplt=wmenu(['HC MENU','B & W','Color'],title=0,init=1)
set_plot,'ps'
if jplt eq 1 then begin
device,/landscape,bits_per_pixel=8,scale_factor=1.
!p.background=0
!p.color=0
rev=n_colors
endif else begin device,/color,/landscape,bits_per_pixel=8
loadct,18
!p.background=n_colors
!p.color=0
rev=0
endelse
erase
contour,bar2,position=[.2,.075,.8,.10],$
xrange=[scale1,scale2],xticks=nticks,font=0,$
xstyle=1,ystyle=4,$
xtitle='intensity (dB) relative to background',/nodata,/noerase
tv,abs(rev-bar2),!x.window(0),!y.window(0),$
xsize=!x.window(1)-!x.window(0),ysize=!y.window(1)-!y.window(0),/norm
contour,array,hour(0:xdim-1),freqlo,position=[.15,.2,.85,.5],$
/nodata,/noerase,xstyle=4,ystyle=1,ytype=linlog,$
ytitle=funit,ticklen=-.01,font=0
arrayb=bytscl(array,min=scale1,max=scale2,top=!d.table_size-2)
tv,abs(rev-arrayb),!x.window(0),!y.window(0),$
xsize=!x.window(1)-!x.window(0),ysize=!y.window(1)-!y.window(0),$
/norm
contour,arrayb,hour(0:xdim-1),freqlo,position=[.15,.2,.85,.5],$
/noerase,/nodata,xstyle=1,ystyle=4,xtickv=tickv,$
xtitle=xlabel,font=0,xticks=hints,xtickname=hlabels,$
ytype=linlog,title=date,ticklen=-.01
device,/close
if (jplt eq 1) then spawn,'lpr idl.ps'
if (jplt eq 2) then spawn,'lpr -Pleptek idl.ps'
set_plot,'x'
loadct,18
!p.background=n_colors
!p.color=0
erase
goto,main
endif
;***** Zoom *******************************************************
if(iplt eq 3) then begin
zoom,Interp=1
goto,main
endif
;***** New Scale ************************************************
if(iplt eq 4) then begin
print,'enter new min and max in dB'
read,scale1,scale2
scale=scale2-scale1
if(scale le 10) then nticks=scale
if((scale gt 10) and (scale lt 20)) then begin
scale=2*fix(scale/2+.9)
scale2=scale1+scale
nticks=scale/2
endif
if(scale ge 20) then begin
scale=10*fix(scale/10+.9)
scale2=scale1+scale
nticks=10
iplt=2
endif
goto,replot
endif
;***** New Day ********************************************
if(iplt eq 5) then goto, start
;***** Quit ******************************************************
if(iplt eq 6) then goto,exit
exit:
end


