pro fcore,bn,dne_cld,te_cld,u_core
;
; Physical Parameters in CGS-Units
;
emass=9.1095e-28 & boltzk=1.3807e-16 & c0=sqrt(!pi^3)
;
ndim=3
A=dblarr(ndim,ndim)*0.
B=dblarr(ndim)*0.
eigval=dblarr(ndim)*0.
eigvec=dblarr(ndim,ndim)*0.
x=dblarr(ndim)*0.
;
A(0,0)=bn(4)
A(0,1)=0.5*bn(7)
A(0,2)=0.5*bn(8)
A(1,0)=A(0,1)
A(1,1)=bn(5)
A(1,2)=0.5*bn(9)
A(2,0)=A(0,2)
A(2,1)=A(1,2)
A(2,2)=bn(6)
;
A_temp=A
;
B(0)=-0.5*bn(1)
B(1)=-0.5*bn(2)
B(2)=-0.5*bn(3)
;
ludc,A,index,/double
uc=lusol(A, index, B, /double)
u_core=uc*1.e8
;
A=-A_temp
tr_A=A(0,0)+A(1,1)+A(2,2)
A=A/tr_A
;
eigval=HQR(ELMHES(A,/double),/double)
residual=1.
eigvec=EIGENVEC(A,eigval,residual=residual,/double)
eigval=float(eigval)
eigvec=float(eigvec)
;
x=float(eigval)*tr_A*50.*1.e-16
tec=emass/(2.*boltzk*x)
;
tsta=abs(eigval(0)-eigval(1))
tstb=abs(eigval(1)-eigval(2))
rt=tsta/tstb
;
if((rt-1.) lt 0.) then begin
   ir=2
   tpalc=tec(ir)
   tperc=0.5*(tec(0)+tec(1))
   dt=tsta
endif else begin
   ir=0
   tpalc=tec(ir)
   tperc=0.5*(tec(1)+tec(2))
   dt=tstb
endelse
taxis=eigvec(ir,*)
te_cld=total(tec)/n_elements(tec)
tec_ans=tperc/tpalc
tec_gyr=dt/te_cld
wtpalc=sqrt(2.*boltzk*tpalc/emass)
wtperc=sqrt(2.*boltzk*tperc/emass)
arg=bn(0)-bn(4)*uc(0)^2-bn(5)*uc(1)^2-bn(6)*uc(2)^2   $
         -bn(7)*uc(0)*uc(1)-bn(8)*uc(0)*uc(2)-bn(9)*uc(1)*uc(2)
dne_cld=c0*wtperc*wtperc*wtpalc*exp(50.*arg)
;
;stop
return
end
