tjd_calendar,year,tjdjan1

;computes truncated julian day numbers for jan1 of years 1977-2005

 
;print,'epoch 1'
tjd1_epoch1=83l
year_epoch1=1996l
nyrs_epoch1=10
year_epoch1=long(1996)+lindgen(nyrs_epoch1)
tjdjan1_epoch1=lonarr(nyrs_epoch1)
diff=-365l
for i=0,nyrs_epoch1-1 do begin
  ;test if previous year was leap year
  a=float(year_epoch1(i)-1)/4
  b=float(fix(a))
  if a - b eq 0 then $
  ndays=366l else ndays=365l
  diff=diff+ndays
  tjdjan1_epoch1(i)=tjd1_epoch1+diff
  ;print,year_epoch1(i),a,b,ndays,diff,tjdjan1_epoch1(i)
endfor

;print,'epoch 0'
tjd1_epoch0=9718l
year_epoch0=1995l
nyrs_epoch0=19
year_epoch0=long(1977)+lindgen(nyrs_epoch0)
tjdjan1_epoch0=lonarr(nyrs_epoch0)
diff=-365l
for i=0,nyrs_epoch0-1 do begin
  j=nyrs_epoch0-1-i
  ;test if leap year
  a=float(year_epoch0(j))/4
  b=float(fix(a))
  if a - b eq 0 then $
  ndays=366l else ndays=365l
  diff=diff+ndays
  tjdjan1_epoch0(j)=tjd1_epoch0-diff
  ;print,year_epoch0(j),a,b,ndays,diff,tjdjan1_epoch0(j)
endfor

year=[year_epoch0,year_epoch1]
tjdjan1=[tjdjan1_epoch0,tjdjan1_epoch1]
;for i=0,n_elements(year)-1 do print,year(i),tjdjan1(i)

;test
ltest=0
if ltest then begin
  yr=1995l
  cday=299
  if yr ge 1977l and yr le 2005l then begin
    w=where(year eq yr)
    if w(0) ne -1 then begin
      tjd=tjdjan1(w(0))-1 +cday  
      if tjd ge 10000l then tjd=tjd-10000l
    endif else begin
      print,'year out of range of truncated julian day calendar' 
      tjdjan1=-9999l
    endelse
    
    print,yr,cday,tjd
  endif 
    
endif


  
end
