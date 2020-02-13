	PROGRAM RYLATEST
C
      double precision al
      double complex y,rc(2,2)
C
	AL = 1.D00
	YI = 0.
	DO NR = 1,20
	  YR = 2. + .1*NR
	  Y = CMPLX(YR,YI)
	  CALL RYLA(Y,AL,RC)
	  PRINT*,YR,YI,AL
	  PRINT*,(RC(I,1),I=1,2)
	  PRINT*,(RC(I,2),I=1,2)
	ENDDO
	STOP
	END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine ryla(y,al,rc)
C
      double precision al
      double complex y,rc(2,2)
c
c	I think
c	this routine evaluates the plasma dispersion function of
c	  Fried and Conte, using approximations from Ronnmark,
c	  Plasma Physics 25, 699, (1983)
c	  y is therefore (w/k)/vt
c
c           ****  choose method of evaluation ****
c	PRINT*,'RYLA CALLED, Y,AL,RC=',Y,AL,RC
      if(al.lt.4.d0) goto 1
      ay=abs(y)
      if(ay**2.gt.75.d0*al) goto 1
      if(ay.gt.40.d0+al/3) goto 1
      if(3.d0*(al-10.d0).gt.ay.and.ay**2.lt.15.d0*al) goto 3
c          ******** numerical integration ********
      call rint(y,al,rc)
      return
c        ******** taylor series ********
    1 call rtay(y,al,rc)
      return
c         ******** asymptotic series ********
    3 call rasy(y,al,rc)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine rasy(y,al,rc)
	implicit double precision (a-h,o-z)
      double complex y,y2,cot,p,py,pp,ppy,pn,pyn,qn,qyn,rc(2,2)
c          ******** asymptotic series ********
      call epsil(y)
      pi=3.14159265358979
      y2=y*y
      ya=dimag(y)
      if(abs(ya).gt.110)then
      cot=(0.d0,-1.d0)
      go to 60
      endif
      cot=cos(pi*y)/sin(pi*y)
 60   continue
*                  1.e99 is too big for s/370 hardware. set to largest
*                  possible for ibm machines
*     c=1.e99
*     c=7.2e+32
CL sur le VAX :
      c=1.d+38
      pn=-y/al
      pyn=pn
      a=1.d0/(al*sqrt(2.d0*pi*al))
      qn=pi*y2*cot*a
      qyn=qn*(2.d0-y*pi*cot)-y*pi**2*y2*a
c
      p=pn+qn
      py=pyn+qyn
      pp=-pn-1.5d0*qn
      ppy=-pyn-1.5d0*qyn
      ay=abs(y)+2.d0
c
      do 4 n=1,100
           m=n-1
           pyn=(pyn*(m*m-y2)-2.d0*y2*pn)/((2*m+1)*al)
           pn =pn*(m*m-y2)/((2*m+1)*al)
           qyn=(qyn*((m+.5d0)**2-y2)-2.d0*y2*qn)/(2.d0*n*al)
           qn =qn*((m+.5d0)**2-y2)/(2.d0*n*al)
           if(m.lt.ay) goto 3
           c=n*(abs(pn)+abs(qn))
           if(c.le.1.d-7*abs(pp)) goto 5
           if(c.ge.t) goto 5
    3      p =p + pn + qn
           py =py + pyn + qyn
           pp =pp -(n + 1.d0)*pn -(n +1.5d0)*qn
           ppy=ppy-(n + 1.d0)*pyn-(n +1.5d0)*qyn
    4 t=c
c
    5 rc(1,1)=p + pn + qn
      rc(2,1)=py+ pyn+ qyn
      rc(1,2)=pp+p
      rc(2,2)=ppy+py
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine rtay(y,al,rc)
	implicit double precision (a-h,o-z)
      double complex y,y2,rc(2,2),pn,pyn,cot
c                  ******** taylor series ********
      call epsil(y)
      y2=y*y
   10 pn=y/(y2-1.d0)
      pyn=-y*(y2+1.d0)/(y2-1.d0)**2
      rc(1,1)=pn
      rc(1,2)=pn
      rc(2,1)=pyn
      rc(2,2)=pyn
c
      do 1 i=2,100
           cot=(2*i-1)/(y2-i**2)*al
           pyn=cot*(pyn-2.d0*y2/(y2-i**2)*pn)
           pn=cot*pn
           rc(1,1)=rc(1,1)+pn
           rc(2,1)=rc(2,1)+pyn
           rc(1,2)=rc(1,2)+i*pn
           rc(2,2)=rc(2,2)+i*pyn
           t=abs(pn)*1.d8
           if(t.lt.abs(rc(1,1))) goto 2
    1 continue
    2 continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine rint(yy,al,rc)
c                  ******** numerical integration ********
	implicit double precision (a-h,o-z)
      double complex rc(2,2),y,yy,cot,d,exf,f,h,o,p,r,ry,rp,rpy,s
      common itest
      dimension a(16), w(16)
c       abscissas for gaussian integration
      data a/               -.98940 09349 91649,-.94457 50230 73232,
     + -.86563 12023 87831, -.75540 44083 55003,-.61787 62444 02643,
     + -.45801 67776 57227, -.28160 35507 79258,-.09501 25098 37637,
     +                       .98940 09349 91649, .94457 50230 73232,
     +  .86563 12023 87831,  .75540 44083 55003, .61787 62444 02643,
     +  .45801 67776 57227,  .28160 35507 79258, .09501 25098 37637/,
     +   w /                 .02715 24594 11754, .06225 35239 38647,
     +  .09515 85116 82492,  .12462 89712 55533, .14959 59888 16576,
     +  .16915 65193 95002,  .18260 34150 44923, .18945 06104 55068,
     +                       .02715 24594 11754, .06225 35239 38647,
     +  .09515 85116 82492,  .12462 89712 55533, .14959 59888 16576,
     +  .16915 65193 95002,  .18260 34150 44923, .18945 06104 55068/,
     + pi/3.14159265358979/
c
      itest=0
	do 1 i=1,2
	do 1 j=1,2
 1    rc(i,j)=0.d0
      call epsil(yy)
      y=yy
      if(dble(y).lt.0) y=-yy
      sig=1.d0
      if(dble(yy).lt.0) sig=-1.d0
      ya=dimag(y)
      yr=dble(y)
      ul=pi-2.8d0*y/(36.d0+y)
      if(ul.gt.pi) ul=pi
      if(abs(ya).gt.110.d0)then
      cot=(0.d0,-1.d0)
      go to 60
      endif
      cot=cos(pi*y)/sin(pi*y)
 60   continue
      d=pi*(1.d0+cot**2)
      c=yr/al
      xo=log(c+sqrt(1.d0+c**2))
c
      do 10 i=1,16
           x=ul/2.d0*(1.d0+a(i))
           z=sin(x)
           c=cos(x)
           g=yr/al*x/z
           t=sqrt(1.d0+g**2)
           b=log(g+t)
           g=(1.d0/x-c/z)*g/t
           t=al*(t*c-1.d0)
c
cc         overflow si x gt 700 dans exp(x)
c
      xya=x*ya
      if(xya.gt.700)then
      z=1.d32
      else
           z=exp(x*ya)
      endif
           c=.5d0*(z+1.d0/z)
           s=(0.d0,.5d0)*(z-1.d0/z)
           f=cot+g
           h=1.d0-g*cot
           test=dble(t-y*b)
           if(test.gt.-670.d0) goto 20
           exf=0.d0
           itest=1
           goto 30
   20      exf=exp(t-y*b)
   30      o=b*c+x*s
           p=x*c-b*s
           xy=x*yr
           r=(f*c+h*s)*exf
           ry=(f*o-h*p+d*(c-g*s))*exf
           rp=((f*t-h*xy)*c+(h*t+f*xy)*s)*exf
           rpy=(f*(t*o-xy*p)-h*(t*p+xy*o)+((t+xy*g)*c-(g*t-xy)*s)*d)*
     +     exf
c
           x=xo/2.d0*(1.d0+a(i))
           z=exp(x)
           c=(z+1.d0/z)/2.d0-1.d0
           p=exp(al*c-y*x)
           rc(1,1)=rc(1,1)+w(i)*(ul*r+xo*p)
           rc(2,1)=rc(2,1)-w(i)*(ul*ry+xo*x*p)
           rc(1,2)=rc(1,2)+w(i)*(ul*rp+xo*al*c*p)
           rc(2,2)=rc(2,2)-w(i)*(ul*rpy+xo*al*x*c*p)
   10 continue
c
      o=y/al
      p=y**2/2.d0
      rc(1,1)=o*(y*rc(1,1)/2.d0-1.d0)*sig
      rc(2,1)=2.d0*rc(1,1)+o*(p*rc(2,1)+1.d0)*sig
      rc(1,2)=y*o*rc(1,2)/2.d0*sig
      rc(2,2)=2.d0*rc(1,2)+o*p*rc(2,2)*sig
      end

      subroutine epsil(y)
c
c  ce s/p corrige la valeur ' y ' de epsilon pour eviter une singularite
c
      double complex y
C
      yr=real(y)
      yi=dimag(y)
      n=ifix(yr+.5)
      delta=yr-float(n)
      eps=1.e-14*n
      if(abs(delta).lt.eps.and.yi.eq.0) yr=float(n)+sign(eps,delta)
      y=dcmplx(yr,yi)
      return
      end
