pro reducf,nx,ny,zgrd,vperp,F

a=0.05*max(vperp)
F=fltarr(nx)
for i=0,nx-1 do begin
  wh=where(zgrd(i,0:ny-1) ne 0, nwh)
  if nwh gt 3 then begin
    ;fv=2*!pi*exp(zgrd(i,wh))*vperp(wh)
    fv=2*!pi*10.^(zgrd(i,wh))*vperp(wh)
    v=vperp(wh)    
    black,a,v,fv,nwh,x,y,n
    trap,x,y,n,res,fer1,fer2
    F(i)=res
    endif
  endfor
return 
end
