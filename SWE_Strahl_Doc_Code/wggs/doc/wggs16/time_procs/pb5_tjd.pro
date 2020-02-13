function pb5_tjd,timpb5

;converts pb5 time (y d msec of day) to truncated julian day and msec of day

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

tjd=0l
wchy=where(timpb5(0) eq y)
if wchy(0) eq -1 then return,[-1,-1]
k=wchy(0)
tjd=tjdjan1(k) +timpb5(1)-1
if tjd ge 10000l then tjd=tjd-10000l
;stop
return,[tjd,timpb5(2)]
  
 
end
  


