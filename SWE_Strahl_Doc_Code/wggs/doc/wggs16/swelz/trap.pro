pro trap,x,y,n,res,fer1,fer2

del=x(1:n-1)-x(0:n-2)
yave=(y(1:n-1)+y(0:n-2))/2.
res=total(yave*del)
fer1=(((y(1)+y(0))/2)*(x(1)-x(0)))/res
fer2=(((y(n-1)+y(n-2))/2)*(x(n-1)+x(n-2)))/res
return
end
