pro black,a,xin,yin,nin,x,y,n

;transform integrand xin(0)-infinity to an integrand yput over unit interval
n=nin+1
x=fltarr(n)
y=fltarr(n)
x(0:n-2)=(xin-xin(0))/(a+xin-xin(0))
y(0:n-2)=yin*(a+xin-xin(0))^2/a
x(n-1)=1.
y(n-1)=0
return
end
