
;generates table of truncated julian day at jan1 of years beginning 1994 and
;extending up to 2022 


;tjdays=[9353l + lindgen(10000l-9353l),lindgen(9353l)]
n=30
yzro=[9353l,9353l,-647+lonarr(n-2)]
tjd1=yzro+indgen(n)*365+fix((indgen(n)+1)/4)  ;tjd on jan1
y=1994l+lindgen(n)  ;year

ny=n-1
tjdjan1=tjd1(0:ny-1)     ;tjd on jan1
tjddec31=tjd1(1:ny)-1    ;tjd on dec31
for k=0,ny-1 do print,k,y(k),tjdjan1(k),tjddec31(k)


end
  


