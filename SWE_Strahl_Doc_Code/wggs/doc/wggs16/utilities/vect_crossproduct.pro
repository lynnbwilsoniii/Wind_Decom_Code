function vect_crossproduct,a,b

;Given input 3-vectors in cartesian coords a and b, 
;the cross product is returned as a unit vector.

c0=a(1)*b(2)-a(2)*b(1)
c1=a(2)*b(0)-a(0)*b(2)
c2=a(0)*b(1)-a(1)*b(0) 
c=[c0,c1,c2]
return,c/sqrt(total(c*c))

end