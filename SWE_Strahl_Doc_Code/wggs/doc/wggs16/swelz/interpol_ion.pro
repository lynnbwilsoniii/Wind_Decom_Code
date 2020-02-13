 pro hrmite,x1,y1,dy1,x2,y2,dy2,coef

;IDL version (RJF) of fortran interpolation (A. F. Vinas & R. F. Thompson)

;hrmite=hermite polynomial 3rd degree
;computes coefficients coeff(0:3) which define cubic hermite polynomial
;h(x)=coef(0)+coef(1)*x+coef(2)*x*x+coef(3)*x*x*x
;satisfying h(x1)=y1, h(x2)=y2, dh(x1)/dx=dy1, dh(x2)/dx=dy2

dx=x2-x1
sum=x2+x1
a24=sum*sum-x2*x1
a34=-(sum+x1)*dx
dy=y2-y1
dydx=dy/dx

d=(dy1+dy2-2.0*dydx)/(dx*dx)
c=-(dy1-dydx-a34*d)/dx
b=dydx-sum*c-a24*d
a=y1-x1*(b+x1*(c+x1*d))

coef=[a,b,c,d]

end



;=================== main ==================================================

pro interpol_ion,xion,yion,indx,xelec,xref,yion_interp

;interpolates ion density at times of xelectron data 
;using sliding three-point interpolation written by A. F. Vinas

;xion,yion=ion time, ion density  :  dblarr(n_iondata)
;indx=current ion data index  
;xelec=xelectron time  : double
;yion_interp=interpolated ion density at xelectron times : float


start:  
i=indx

  c0=xelec lt xion(i) and xelec lt xion(i+1) and xelec lt xion(i+2)
  c1=xelec ge xion(i) and xelec lt xion(i+1) and xelec lt xion(i+2)
  c2=xelec ge xion(i) and xelec ge xion(i+1) and xelec lt xion(i+2)
  c3=xelec ge xion(i) and xelec ge xion(i+1) and xelec ge xion(i+2)
  
  wc=where([c0,c1,c2,c3] eq 1)
  ;stop
  case wc(0) of
  0: yion_interp=yion(i)
  1: begin
       u1=xion(i)-xref & v1=double(yion(i))
       u2=xion(i+1)-xref & v2=double(yion(i+1))
       d1=(yion(i+1)-yion(i))/(xion(i+1)-xion(i))
       if (yion(i+1)-yion(i)) * (yion(i+2)-yion(i+1)) lt 0 then d2=0.d $
       else d2=(yion(i+2)-yion(i+1)) / (xion(i+2)-xion(i+1))
       hrmite,u1,v1,d1,u2,v2,d2,coef
       yion_interp=coef(0)+$
                     coef(1)*(xelec-xref)+$
                     coef(2)*(xelec-xref)*(xelec-xref)+$
                     coef(3)*(xelec-xref)*(xelec-xref)*(xelec-xref)
     endcase
  2: begin
       u1=xion(i+1)-xref & v1=double(yion(i+1))
       u2=xion(i+2)-xref & v2=double(yion(i+2))
       d2=(yion(i+2)-yion(i+1))/(xion(i+2)-xion(i+1))
       if (yion(i+1)-yion(i)) * (yion(i+2)-yion(i+1)) lt 0 then d1=0.d $
       else d1=(yion(i+1)-yion(i)) / (xion(i+1)-xion(i))
       hrmite,u1,v1,d1,u2,v2,d2,coef
       yion_interp=coef(0)+$
                     coef(1)*(xelec-xref)+$
                     coef(2)*(xelec-xref)*(xelec-xref)+$
                     coef(3)*(xelec-xref)*(xelec-xref)*(xelec-xref)
                     ;stop
     endcase
                     
  3: begin
        if i+2 ge n_elements(xion)-1 then yion_interp=yion(n_elements(xion)-1) $
        else begin
          indx=i+1
          goto,start
        endelse
     endcase
  endcase
  
  
  
  
  indx=i
  
  end                  
                     
                                     
                     
                     
                      
