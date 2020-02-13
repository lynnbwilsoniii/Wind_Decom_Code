


FUNCTION marray,xx
 
ON_ERROR,1
n = n_elements(xx)
aa = long(xx(0))
for i = 1,n-1 do aa = aa*xx(i)
return,aa
end



;
; The following program will load all of the data for a specific CDF file and
; variable into IDL. 
; 

pro loadcdf,CDF_file,CDF_var,x

ON_ERROR,1

;
; Open CDF file
;

id = cdf_open(CDF_file)

;
; Get file CDF structure information
; 

inq = cdf_inquire(id)

;
; Get variable structure information
;

vinq = cdf_varinq(id,CDF_var)
dims = total(vinq.dimvar)
dimc = vinq.dimvar * inq.dim
dimw = where(dimc eq 0)
if (dimw(0) ne -1) then dimc(dimw) = 1

CDF_varget,id,CDF_var,x,COUNT=dimc,REC_COUNT=inq.maxrec+1

sa = size(x)
sa = sa(1:sa(0))

if (vinq.recvar eq 'VARY' and dims ne 0) then begin
   x = reform(x,[marray(sa(0:(n_elements(sa)-2))),sa(n_elements(sa)-1)])
   x = transpose(x)
   sa = shift(sa,1)
   x = reform(x,sa)
endif

saw = where(sa ne 1)
x = reform(x,sa(saw))

CDF_close,id
return
end



;
; The following program will load specified records for a specific CDF file and
; variable into IDL. 
; 
; Modification of Jim Byrne's loadcdf.pro (RJF Dec 94)

pro loadcdf_rec,CDF_file,CDF_var,x,recn

ON_ERROR,1

;
; Open CDF file
;

id = cdf_open(CDF_file)

;
; Get file CDF structure information
; 

inq = cdf_inquire(id)

;
; Get variable structure information
;

vinq = cdf_varinq(id,CDF_var)
dims = total(vinq.dimvar)
dimc = vinq.dimvar * inq.dim
dimw = where(dimc eq 0)
if (dimw(0) ne -1) then dimc(dimw) = 1

CDF_varget,id,CDF_var,x,COUNT=dimc,REC_START=recn
;help,CDF_var,x

CDF_close,id
return
end

;---------------------- plot mag dat1 --------------------------------------- 

pro plotb,magfile,tpb5,bgse,hcpy=hcpy,win=win,xrange=xrange,xticks=xticks

hardcopy=hcpy

 help,tpb5,bgse
  fill=-1.e31
  wok=where(bgse(*,0) ne fill)

plot1:
if hardcopy ne 1 then window,win,xsize=600,ysize=750 else begin
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.
endelse
!p.multi=[0,0,3,0,0]
!p.charsize=2.5
s=size(tpb5)
hrdy=dblarr(s(1))  ;hour of day relative to begin of interval
refmsec=tpb5(0,1)*86400000.d
hrdy(*)=( tpb5(*,1)*86400000.d + tpb5(*,2)*1.d - refmsec)/3600000.d
xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
hour_hms,xtickv,hms
xtickname=hms
yrange=[-24,24]

;plot gse angles
   tim=hrdy(wok)
   bm=sqrt(bgse(wok,0)^2+bgse(wok,1)^2+bgse(wok,2)^2)
   phi=atan(bgse(wok,1),bgse(wok,0))/!dtor
   w=where(phi lt 0)
   if w(0) ne -1 then phi(w)=phi(w) + 360.
   theta=$
     asin(bgse(wok,2)/ sqrt(bgse(wok,0)^2+bgse(wok,1)^2+bgse(wok,2)^2))/!dtor

   plot,tim,bm,psym=3,yrange=[0,25],ystyle=1,yticks=5,xticks=xticks,xstyle=1,$
     xrange=xrange,xtickv=xtickv,xtickname=xtickname,ytitle='B',$
     title='3sec mfi data (gse)',xminor=6
 
   plot,tim,theta,psym=3,yrange=[-90,90],ystyle=1,yticks=4,xticks=xticks,$
     xstyle=1,xrange=xrange,xtickv=xtickv,xtickname=xtickname,$
     ytitle='theta',xminor=6
   oplot,[tim(0),tim(n_elements(tim)-1)],[0,0],linestyle=1

   plot,tim,phi,psym=3,yrange=[0,360],ystyle=1,yticks=4,xticks=xticks,xstyle=1,$
     xrange=xrange,xtickv=xtickv,xtickname=xtickname,ytitle='phi',$
     xtitle=magfile,xminor=6
   oplot,[tim(0),tim(n_elements(tim)-1)],[135-25,135-25],linestyle=1
   oplot,[tim(0),tim(n_elements(tim)-1)],[135+25,135+25],linestyle=1
   oplot,[tim(0),tim(n_elements(tim)-1)],[315-25,315-25],linestyle=1
   oplot,[tim(0),tim(n_elements(tim)-1)],[315+25,315+25],linestyle=1


;stop,'plotb'
if hardcopy then begin
   device,/close
   set_plot,'x'
   print,' ' & print,'printing hardcopy file idl.ps.....'
   spawn,'lpr idl.ps'
   !p.color=125
   hardcopy=0 & goto,plot1
endif

end



;======================== calling procedure ===================================
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_cur

;set top color (white in GSFC and IOWA color tables) to next to top color (RJF)
   loadct,18  ;GSFC color table
   ;loadct,13  ;RAINBOW color table
   r_orig(n_elements(r_orig)-1)=r_orig(n_elements(r_orig)-2)
   g_orig(n_elements(g_orig)-1)=g_orig(n_elements(r_orig)-2)
   b_orig(n_elements(b_orig)-1)=b_orig(n_elements(r_orig)-2)
   tvlct,r_orig,g_orig,b_orig
   !p.color=125

; tselct=given pb5 time (year, day of year, millisec of day)  lonarr(3)
; tpb5 = pb5 time at center of minute for given record recn   lonarr(1440,3)
; bgse = mag vector at 3 sec intervals in minute tpb5         fltarr(1440,3,20)


;cdfpath='/lepswe_data2/mfi/1min/'
cdfpath=getenv('MAGPATH')
hcpy=1

next:
magfile=pickfile(/read,get_path=cdfpath,path=cdfpath(0),filter='*cdf',$
          title='mfi 3sec files',/must_exist)
print,'selected file ',magfile

;get 3sec mag data for entire day
  loadcdf,magfile,'Time_PB5',tpb5   
  loadcdf,magfile,'B3GSE',bgse

;plot mag data for entire day
  magtim=tpb5
  magdat=bgse
  plotb,magfile,magtim,magdat,hcpy=hcpy,win=0,xrange=[0,24],xticks=4
 
readp:
print,' ' & print,'plot sub interval of time for date ',ymd(tpb5(0,*)) 
print,' ' & print,'enter begin time (hh mm ss)'
hms1=lonarr(3) & read,hms1 
print,'enter end time (hh mm ss)'
hms2=lonarr(3) & read,hms2

;get pb5 times
  tselct1=tpb5(0,*) & tselct2=tselct1   ;initialize
  tselct1(2)=hms1(0)*3600000+hms1(1)*60000+hms1(2)*1000
  tselct2(2)=hms2(0)*3600000+hms2(1)*60000+hms2(2)*1000
print,' ' & print,'time interval selected (pb5 time)'
print,'begin ', tselct1  
print,'end ', tselct2

;get indices of begin and end times (to the full minute)
  mindx1=fix((tselct1(2))/60000)  ;magfile record number from minute index
  mindx2=fix((tselct2(2))/60000)
  sindx1=0;3sec index in minute interval
  sindx2=19

;plot mag data for subinterval of day
  magtim=tpb5(mindx1:mindx2,*)
  magdat=bgse(mindx1:mindx2,*,sindx1:sindx2)
  ;xrange=[tpb5(mindx1,2),tpb5(mindx2,2)]/3600000.
  xrange=[tselct1(2),tselct2(2)]/3600000.
  xticks=4
  plotb,magfile,magtim,magdat,hcpy=hcpy,win=1,xrange=xrange,xticks=xticks


stop,'.con to continue'

goto,readp



;get mag data for 3sec interval closest to selected time, tselct
  mindx=fix((tselct(2))/60000)  ;magfile record number from minute index
  sindx=fix((tselct(2)-mindx*60000 )/3000) ;3sec index in minute interval
  bs=bgse(mindx,0:2,sindx)
  ;b=[bs(0),-bs(1),-bs(2)] ;a crude transformation from gse to swe payload

print,' ' & print,'selected time ',tselct
print,'data for minute containing selected time:'
print,' min ',transpose(tpb5(mindx,*))
print,'msec, bgse :'
for k=0,19 do print,tpb5(mindx,2)-(30000l - 1500l - k*3000l),$
  bgse(mindx,0,k),bgse(mindx,1,k),bgse(mindx,2,k)
print,'minute and second indices ',mindx,sindx
print,'msec of closest 3sec interval ',$
  tpb5(mindx,2)-(30000l - 1500l - sindx*3000l)
print,'bgse in nearest 3sec interval ',$
  bgse(mindx,0,sindx),bgse(mindx,1,sindx),bgse(mindx,2,sindx)


end
