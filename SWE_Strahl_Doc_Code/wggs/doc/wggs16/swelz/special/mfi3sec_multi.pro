


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

;the current strahl sampling rang in phi
strl1=[135-25,135+25]
strl2=[315-25,315+25]

cdfpath='/lepswe_data2/mfi/1min/'

magfile0=pickfile(/read,get_path=cdfpath,path=cdfpath(0),filter='*cdf',$
          title='First mfi 3sec file to plot:',/must_exist)
print,'selected first file ',magfile0

magfile1=pickfile(/read,get_path=cdfpath,path=cdfpath(0),filter='*cdf',$
          title='Last mfi 3sec file to plot:',/must_exist)
print,'selected last file ',magfile1

spawn,'ls '+cdfpath+'*cdf',list
wfirst=where(list eq magfile0)
wlast=where(list eq magfile1)

if wfirst(0) eq -1 or wlast(0) eq -1 then stop,'error: no match'
if wlast(0)-wfirst(0) lt 0 then stop,'files selected out of order'

for i=wfirst(0),wlast(0) do begin

magfile=list(i)
print,'magfile ',magfile

;get 3sec mag data for entire day
  loadcdf,magfile,'Time_PB5',tpb5   
  loadcdf,magfile,'B3GSE',bgse
  help,tpb5,bgse
  fill=-1.e31
  wok=where(bgse(*,0) ne fill)

hardcopy=1

plot1:
if hardcopy ne 1 then window,0,xsize=600,ysize=750 else begin
  set_plot,'ps'
  device,/inches,xoffset=1.0,yoffset=1.0,xsize=7.,ysize=8.
endelse
!p.multi=[0,0,3,0,0]
!p.charsize=2.5
s=size(tpb5)
hrdy=dblarr(s(1))  ;hour of day relative to begin of interval
hrdy(*)=(tpb5(*,1)*86400000.d + tpb5(*,2)*1.d - tpb5(0,1)*86400000.d +$
  tpb5(0,2)*1.d) / 3600000.d
yrange=[-24,24]
xticks=4
xrange=[0.,24.]
xtickv=indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
hour_hms,xtickv,hms
xtickname=hms

pltype=1
case pltype of
0: begin  ;plot gse xyz components

  plot,hrdy(wok),bgse(wok,0),psym=3,yrange=yrange,ystyle=1,yticks=4,xticks=4,$
    xstyle=1,ytitle='bgse_x',xtickformat='(f4.1)',$
    title=magfile+'  3sec data'

  plot,hrdy(wok),bgse(wok,1),psym=3,yrange=yrange,ystyle=1,yticks=4,xticks=4,$
    xstyle=1,ytitle='bgse_y',xtickformat='(f4.1)'

  plot,hrdy(wok),bgse(wok,2),psym=3,yrange=yrange,ystyle=1,yticks=4,xticks=4,$
    xstyle=1,ytitle='bgse_z',xtickformat='(f4.1)',xtitle='hour of day  '+$
    string(tpb5(0,0),format='(i4)')+string(tpb5(0,1),format='(i4)')
    
endcase

1: begin  ;plot gse angles
   tim=hrdy(wok)
   bm=sqrt(bgse(wok,0)^2+bgse(wok,1)^2+bgse(wok,2)^2)
   phi=atan(bgse(wok,1),bgse(wok,0))/!dtor
   w=where(phi lt 0)
   if w(0) ne -1 then phi(w)=phi(w) + 360.
   theta=$
     asin(bgse(wok,2)/ sqrt(bgse(wok,0)^2+bgse(wok,1)^2+bgse(wok,2)^2))/!dtor

   plot,tim,bm,psym=3,yrange=[0,25],ystyle=1,yticks=5,xticks=xticks,xstyle=1,$
     xrange=xrange,xtickv=xtickv,xtickname=xtickname,ytitle='B',$
     title='3sec mfi data (gse)'
 
   plot,tim,theta,psym=3,yrange=[-90,90],ystyle=1,yticks=4,xticks=xticks,$
     xstyle=1,xrange=xrange,xtickv=xtickv,xtickname=xtickname,$
     ytitle='theta'
   oplot,[tim(0),tim(n_elements(tim)-1)],[0,0],linestyle=1

   plot,tim,phi,psym=3,yrange=[0,360],ystyle=1,yticks=4,xticks=xticks,xstyle=1,$
     xrange=xrange,xtickv=xtickv,xtickname=xtickname,ytitle='phi',xtitle=magfile
   oplot,[tim(0),tim(n_elements(tim)-1)],[strl1(0),strl1(0)],linestyle=1
   oplot,[tim(0),tim(n_elements(tim)-1)],[strl1(1),strl1(1)],linestyle=1
   oplot,[tim(0),tim(n_elements(tim)-1)],[strl2(0),strl2(0)],linestyle=1
   oplot,[tim(0),tim(n_elements(tim)-1)],[strl2(1),strl2(1)],linestyle=1

   endcase
endcase

if hardcopy then begin
   device,/close
   set_plot,'x'
   print,' ' & print,'printing hardcopy file idl.ps.....'
   spawn,'lpr idl.ps'
   !p.color=125
   hardcopy=0 & goto,plot1
endif

endfor
for i=wfirst(0),wlast(0) do print,list(i)
print,'mfi3sec_multi finished'

end
