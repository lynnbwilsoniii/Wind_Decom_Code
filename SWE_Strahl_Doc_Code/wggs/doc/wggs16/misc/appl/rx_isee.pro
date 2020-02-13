pro rx,dens,ux,uy,uz,bx,by,bz,gsex,gsey,gsez,th,x0r,y0r,z0r,r,x

rad=!pi/180.

;shock parameters for 77309-310
a10=15.576
a20=0.0375399
dens0=12.5
u0=280.e5
c=((dens/dens0)*(sqrt(ux^2+uy^2+uz^2)/u0)^2)^0.16667
a1=a10/c
a2=a20*c

;rotate s/c gse coords about xgse so that new xy plane contains b and u

byz=sqrt(by^2+bz^2)
sina=bz/byz
cosa=by/byz

;s/c position in rotated gse
xgser=gsex
ygser=gsey*cosa    + gsez*sina
zgser=gsey*(-sina) + gsez*cosa

;mag field in rotated gse
bxr=bx
byr=byz
bzr=0
bmag=sqrt(bx^2+byz^2)

;angle between b and xgse
th=acos(bxr/bmag)/rad
sinth=sin(th*rad)
costh=cos(th*rad)

;coords (rotated gse) of magnetic tangent point = origin of r-x (foreshock) coord system
z0r=zgser
y0r=-(0.5/a2)*costh/sinth
x0r=a1-a2*(y0r^2+z0r^2)

;transform s/c position from rotated gse to foreshock r-x coords
r=(xgser-x0r)*costh+(ygser-y0r)*sinth
x=(xgser-x0r)*(-sinth)+(ygser-y0r)*costh

return
end
