
;------------------- fitfunct ----------------------------------------------
pro fitfunct,x,c,f,pder
f=c(0)+c(1)*x
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
return
end


;------------------ fintt --------------------------------------------------
function fintt,vx,vy,vz,bn

arg=bn(0) + bn(1)*vx   + bn(2)*vy   + bn(3)*vz   +$
            bn(4)*vx^2 + bn(5)*vy^2 + bn(6)*vz^2 +$
            bn(7)*vx*vy + bn(8)*vx*vz + bn(9)*vy*vz
return, exp(50.*arg)
end


;--------------------- patchfun ----------------------------------------------
function patchfun,x,m
common patchdata,vxm,vym,vzm,wg

; Model is ln(f)=b0+b1*vx+b2*vy+b3*vz+b4*vx^2+b5*vy^2+b6*vz^2
;                b7*vx*vy+b8*vx*vz+b9*vy*vz

xx=x[0]
i=fix(xx)
sz=reverse(size(xx))
if sz[n_elements(sz)-2] eq 5 then $
   yf=dblarr(m) else yf=fltarr(m)
yf[0]=1.0/wg(i)
yf[1]=vxm(i)/wg(i)
yf[2]=vym(i)/wg(i)
yf[3]=vzm(i)/wg(i)
yf[4]=vxm(i)*vxm(i)/wg(i)
yf[5]=vym(i)*vym(i)/wg(i)
yf[6]=vzm(i)*vzm(i)/wg(i)
yf[7]=vxm(i)*vym(i)/wg(i)
yf[8]=vxm(i)*vzm(i)/wg(i)
yf[9]=vym(i)*vzm(i)/wg(i)
;stop
return, yf
end

;------------------------------ Main Patch Procedure --------------------------

pro lzpatch,fblk,cnts,vunit,velocity,vpot,nd,nv,ns,lchan,bn,chisqr,$
  patch_include,nvmin=nvmin,err=err

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl  
common patchdata,vxm,vym,vzm,wg

err=''
vc=1.e8 & fc=50.0
m=10

;correct velocity magnitude for spacecraft potential
v_vpot2=velocity^2 - vpot^2
wge0=where(v_vpot2 gt 0,nwge0)
if nwge0 lt lchan then begin
  err='scpot is so high that there are not enough energy steps for patch'
  return
endif 
v_vpot=sqrt(v_vpot2(wge0))
nvmin=wge0(0)

j0=wge0(0)
j1=j0+lchan-1

wfit=where(patch_include(0:nd-1,j0:j1,0:ns-1) eq 1,nfit)
vxm=dblarr(nfit)
vym=dblarr(nfit)
vzm=dblarr(nfit)
x=double(findgen(nfit))
y=dblarr(nfit)
wg=dblarr(nfit)
ic=-1
for j=j0,j1 do for k=0,ns-1 do for i=0,nd-1 do begin
  if patch_include(i,j,k) then begin
    ic=ic+1
    vxm(ic)=v_vpot(j-j0)*vunit(i,j,k,0)/vc 
    vym(ic)=v_vpot(j-j0)*vunit(i,j,k,1)/vc
    vzm(ic)=v_vpot(j-j0)*vunit(i,j,k,2)/vc
    wg(ic)=1./sqrt(double(cnts(i,j,k)))     ;poisson sigma of log f
    y(ic)=alog(fblk(i,j,k))/wg(ic)/fc 
    ;print,i,j,k,patch_include(i,j,k),cnts(i,j,k),fblk(i,j,k) 
  endif    
endfor  

rslts=svdfit(x,y,m,FUNCTION_NAME='patchfun',SINGULAR=sing,YFIT=yfit,/double)

chisqr=total(((y-yfit)/wg)^2)/float(nfit-m)

bn=rslts

end



