;---Conversion to idl of Adolfo Vinas' fortran routine to find s/c potential--
;---RJF


function rtsafe,funcd,x1,x2,xacc

maxit=100
call_procedure,funcd,x1,fl,df
call_procedure,funcd,x2,fh,df
if fl eq 0 then return,x1 else $
if fh eq 0 then return,x2 else $
if fl lt 0 then begin
  xl=x1
  xh=x2
endif else begin
  xh=x1
  xl=x2
endelse
fvalue=(x1+x2)/2
dxold=abs(x2-x1)
dx=dxold
call_procedure,funcd,fvalue,f,df
for j=1,maxit do begin
 if(((fvalue-xh)*df-f)*((fvalue-xl)*df-f) ge 0 or abs(2*f) gt abs(dxold*df)) $
 then begin
   dxold=dx
   dx=(xh-xl)/2
   fvalue=xl+dx
   if xl eq fvalue then return,fvalue
 endif else begin
   dxold=dx
   dx=f/df
   temp=fvalue
   fvalue=fvalue-dx
   if temp eq fvalue then return,fvalue
 endelse
 if abs(dx) lt xacc then return,fvalue
 call_procedure,funcd,fvalue,f,df
 if f lt 0 then xl=fvalue else xh=fvalue
endfor
return,fvalue
end


