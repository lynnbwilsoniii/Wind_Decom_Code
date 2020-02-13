function tjd_pb5,tjd,ms,err=err

;computes pb5 time (long([year,day of year, msec of day]) from given
;truncated Julian day  and msec of day (long words)

;uses the following generated table of truncated julian day at jan1 of years 
;beginning 1994 and extending up to 2020 (but not further) :

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<< table <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;tjdays=[9353l + lindgen(10000l-9353l),lindgen(9353l)]
n=28   ;maximum=28
yzro=[9353l,9353l,-647+lonarr(n-2)]
tjd1=yzro+indgen(n)*365+fix((indgen(n)+1)/4)  ;tjd on jan1
y=1994l+lindgen(n)  ;year
ny=n-1
tjdjan1=tjd1(0:ny-1)     ;tjd on jan1
tjddec31=tjd1(1:ny)-1    ;tjd on dec31
;for k=0,ny-1 do print,k,y(k),tjdjan1(k),tjddec31(k)
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


timpb5=lonarr(3)
err=''
wchy=where(tjd ge tjdjan1 and tjd le tjddec31)
if wchy(0) ne -1 then begin
  k=wchy(0)
  timpb5(0)=y(k)
  timpb5(1)=tjd-tjdjan1(k)+1
  timpb5(2)=ms
endif else begin
  ;test for 1995
  ;(rollover in truncated julian day to tjd=0 occurred pb5=[1995 283 0], Oct10)
  if tjd ge tjdjan1(1) and tjd le 9999l then begin
    timpb5(0)=y(1)
    timpb5(1)=tjd-tjdjan1(1)+1
    timpb5(2)=ms
  endif else if tjd ge 0 and tjd le tjddec31(1) then begin
    timpb5(0)=y(1)
    timpb5(1)=10000l+tjd-tjdjan1(1)+1
    timpb5(2)=ms
  endif else err='time error'
endelse  
;print,tjd,ms, timpb5, err

return,timpb5

end 


