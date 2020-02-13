;(Jim Byrnes)
FUNCTION marray,xx
 
ON_ERROR,1
n = n_elements(xx)
aa = long(xx(0))
for i = 1,n-1 do aa = aa*xx(i)
return,aa
end
 
