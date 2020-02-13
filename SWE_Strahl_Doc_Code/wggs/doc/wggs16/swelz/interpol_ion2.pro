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

pro interpol_ion2,xion,yiond,yionv,indx,xelec,xref,yiond_interp,yionv_interp

;interpolates ion density at times of xelectron data 
;using sliding three-point interpolation written by A. F. Vinas

;xion,yiond,yionv=ion time, ion density & velocity  :  dblarr(n_iondata)
;indx=current ion data index  
;xelec=xelectron time  : double
;yiond_interp=interpolated ion density at xelectron times : float
;yionv_interp=interpolated ion velocity at xelectron times : float

start:  
i=indx

  c0=xelec lt xion(i) and xelec lt xion(i+1) and xelec lt xion(i+2)
  c1=xelec ge xion(i) and xelec lt xion(i+1) and xelec lt xion(i+2)
  c2=xelec ge xion(i) and xelec ge xion(i+1) and xelec lt xion(i+2)
  c3=xelec ge xion(i) and xelec ge xion(i+1) and xelec ge xion(i+2)
  
  wc=where([c0,c1,c2,c3] eq 1)
  
  case wc(0) of
  0: begin
   yiond_interp=yiond(i)
   yionv_interp=yionv(i)
     endcase
  1: begin
       u1=xion(i)-xref & v1=double(yiond(i)) & w1=double(yionv(i))
       u2=xion(i+1)-xref & v2=double(yiond(i+1)) & w2=double(yionv(i+1))
       d1=(yiond(i+1)-yiond(i))/(xion(i+1)-xion(i))
       e1=(yionv(i+1)-yionv(i))/(xion(i+1)-xion(i))
       if (yiond(i+1)-yiond(i)) * (yiond(i+2)-yiond(i+1)) lt 0 then d2=0.d $
       else d2=(yiond(i+2)-yiond(i+1)) / (xion(i+2)-xion(i+1))
       if (yionv(i+1)-yionv(i)) * (yionv(i+2)-yionv(i+1)) lt 0 then e2=0.d $
       else e2=(yionv(i+2)-yionv(i+1)) / (xion(i+2)-xion(i+1))
       hrmite,u1,v1,d1,u2,v2,d2,coefd
       hrmite,u1,w1,e1,u2,w2,e2,coefv
       yiond_interp=coefd(0)+$
                     coefd(1)*(xelec-xref)+$
                     coefd(2)*(xelec-xref)*(xelec-xref)+$
                     coefd(3)*(xelec-xref)*(xelec-xref)*(xelec-xref)
       yionv_interp=coefv(0)+$
                     coefv(1)*(xelec-xref)+$
                     coefv(2)*(xelec-xref)*(xelec-xref)+$
                     coefv(3)*(xelec-xref)*(xelec-xref)*(xelec-xref)              
     endcase
  2: begin
       u1=xion(i+1)-xref & v1=double(yiond(i+1)) & w1=double(yionv(i+1))
       u2=xion(i+2)-xref & v2=double(yiond(i+2)) & w2=double(yionv(i+2))
       d2=(yiond(i+2)-yiond(i+1))/(xion(i+2)-xion(i+1))
       e2=(yionv(i+2)-yionv(i+1))/(xion(i+2)-xion(i+1))
       if (yiond(i+1)-yiond(i)) * (yiond(i+2)-yiond(i+1)) lt 0 then d1=0.d $
       else d1=(yiond(i+1)-yiond(i)) / (xion(i+1)-xion(i))
       if (yionv(i+1)-yionv(i)) * (yionv(i+2)-yionv(i+1)) lt 0 then e1=0.d $
       else e1=(yionv(i+1)-yionv(i)) / (xion(i+1)-xion(i))
       hrmite,u1,v1,d1,u2,v2,d2,coefd
       hrmite,u1,w1,e1,u2,w2,e2,coefv
       yiond_interp=coefd(0)+$
                     coefd(1)*(xelec-xref)+$
                     coefd(2)*(xelec-xref)*(xelec-xref)+$
                     coefd(3)*(xelec-xref)*(xelec-xref)*(xelec-xref)
       yionv_interp=coefv(0)+$
                     coefv(1)*(xelec-xref)+$
                     coefv(2)*(xelec-xref)*(xelec-xref)+$
                     coefv(3)*(xelec-xref)*(xelec-xref)*(xelec-xref)              
     endcase
                     
  3: begin
        if i+2 ge n_elements(xion)-1 then begin
          yiond_interp=yiond(n_elements(xion)-1)
          yionv_interp=yionv(n_elements(xion)-1) 
        endif else begin
          indx=i+1
          goto,start
        endelse
     endcase
  endcase
  
  
  
  
  indx=i
  
  end                  
                     
                                     
                     
                     
                      
