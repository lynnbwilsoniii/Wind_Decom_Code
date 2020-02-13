pro new_time_scale,newticks,x0,x1,xrange,xtickv,xtickname,xticks,tstyle=tstyle,$
  mod24=mod24,refpb5=refpb5

; x0, x1  = begin and end of input time scale in time units hour of day 
; newticks= input time interval in minutes of new scale of xticks
; xrange = new time scale range defined by this procedure
; xtickname = new tim scale tick label in form hhmmss, or hhmm, or hh
; xtickv = time in decimal hours corresponding to xtickname
; xticks = number of ticks of new tick label

if keyword_set(mod24) eq 0 then mod24=0

d_xtick=double(newticks)/60.d0 
if keyword_set(tstyle) eq 0 then begin
  x_begin=fix(x0/d_xtick)*d_xtick 
  n_end=fix(x1/d_xtick)
  if x0-n_end*d_xtick eq 0 then x_end=n_end*d_xtick $
    else x_end=(n_end+1)*d_xtick
  xticks=fix((x_end-x_begin)/d_xtick + 0.00001)
endif else begin
  x_begin=x0
  x_end=x1
  xticks=4
endelse
  

xtickv=x_begin+indgen(xticks+1)*d_xtick
xrange=[xtickv(0),xtickv(n_elements(xtickv)-1)]

hour_hms,xtickv,hms,hm=hm,hh=hh,mod24=mod24

 
if strlen(hm(0)) eq 0 and strlen(hh(0)) eq 0 then xtickname=hms 
if strlen(hm(0)) ne 0 then xtickname=hm 
if strlen(hh(0)) ne 0 then begin
  if fix(hh(n_elements(hh)-1)) gt 48 then xtickname=$
  string(pb5_ymd(refpb5)-100*(pb5_ymd(refpb5)/100)+float(hh)/24.,format='(f5.2)') $
  else xtickname=hh
  xtickname=strtrim(xtickname,1)
endif

 end
