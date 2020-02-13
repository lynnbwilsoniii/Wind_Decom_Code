;---Conversion to idl of Adolfo Vinas' fortran routine to find s/c potential--
;---RJF


;----- evaluate function and derivative ------------------------------
pro fun_pot,x,y,dydx
common param,dnrat
erfc=1.d - errorf(x)
y=exp(x*x)*erfc+2*x/sqrt(double(!pi))-dnrat
dydx=2*x*exp(x*x)*erfc
end



;----- find s/c potential --------------------------------------------
pro fndspcpot,rdn,te,pot
common param,dnrat
const=8.623625916d-5

; RDN IS THE DENSITY RATIO GIVEN AS: RDN = Ne_SWE / Ne_TNR

dnrat=double(rdn)
x1=0.d
x2=10.d
xacc=1.d-6
if rdn gt 1.d then x=rtsafe('fun_pot',x1,x2,xacc) else x=0.d

; POTENTIAL IN VOLTS UNITS 
pot=const*x*x*double(te)

end
