      SUBROUTINE WAVPOL (A,B,E,F,WR,WI,XKX,XKZ) 
c	calculate vector E from the dispersion determinant (A + iB)
c	normalized to unit length
c	and B (called F here) from del x E + (1/c)dB/dt = 0
c
	implicit integer (i,j,k,l,m,n)
	implicit real (a-h,o-z)
      DIMENSION A(3,3), B(3,3), E(6), F(6), DMR(3,3), DMI(3,3)
c	find largest minor of the dispersion determinant
C**************
C	TRANSPOSE A + iB
	do i = 1,3
	do j = 1,3
	  dmr(i,j) = a(j,i)
	  dmi(i,j) = b(j,i)
	enddo
	enddo
	do i = 1,3
	do j = 1,3
	  a(i,j) = dmr(i,j)
	  b(i,j) = dmi(i,j)
	enddo
	enddo
c***********
      DM=0. 
      DO 105 I=1,3
        DO 105 J=1,3  
        N1=MOD(I,3)+1 
        N2=MOD(I+1,3)+1 
        M1=MOD(J,3)+1 
        M2=MOD(J+1,3)+1 
        DMR(I,J)=PR(A(N1,M1),B(N1,M1),A(N2,M2),B(N2,M2))-PR(A(N1,M2),B(N
     1  1,M2),A(N2,M1),B(N2,M1))
        DMI(I,J)=PI(A(N1,M1),B(N1,M1),A(N2,M2),B(N2,M2))-PI(A(N1,M2),B(N
     1  1,M2),A(N2,M1),B(N2,M1))
        DMT=ABS(DMR(I,J))+ABS(DMI(I,J)) 
	print 1005,i,j,n1,n2,m1,m2,dmt
 1005	format(6i5,e13.4)
        IF (DM-DMT) 100,100,105 
  100   DM=DMT
        IMAX=I
        JMAX=J
  105   CONTINUE
c	use Kramer;s rule to solve for relative E
      I=IMAX
      J=JMAX
      N1=MOD(I,3)+1 
      N2=MOD(I+1,3)+1 
      M1=MOD(J,3)+1 
      M2=MOD(J+1,3)+1
      DO K=1,6
	E(K)=0.
      ENDDO 
      E(2*J-1)=1. 
      E(2*J)=0.
	print*,'set one E to 1',(e(k),k=1,6) 
      F(1)=PR(A(N2,M2),B(N2,M2),A(N1,J),B(N1,J))-PR(A(N1,M2),B(N1,M2),A(
     1N2,J),B(N2,J))  
      F(2)=PI(A(N2,M2),B(N2,M2),A(N1,J),B(N1,J))-PI(A(N1,M2),B(N1,M2),A(
     1N2,J),B(N2,J))  
      F(3)=PR(A(N1,M1),B(N1,M1),A(N2,J),B(N2,J))-PR(A(N2,M1),B(N2,M1),A(
     1N1,J),B(N1,J))  
      F(4)=PI(A(N1,M1),B(N1,M1),A(N2,J),B(N2,J))-PI(A(N2,M1),B(N2,M1),A(
     1N1,J),B(N1,J))  
	print*,'result f',(f(k),k=1,6)
      E(2*M1-1)=-QR(F(1),F(2),DMR(I,J),DMI(I,J))
      E(2*M1)=-QI(F(1),F(2),DMR(I,J),DMI(I,J))
      E(2*M2-1)=-QR(F(3),F(4),DMR(I,J),DMI(I,J))
      E(2*M2)=-QI(F(3),F(4),DMR(I,J),DMI(I,J))
	print*,'result E',(E(k),k=1,6)
	print*,'m1,m2,e',m1,m2,(e(k),k=1,6)
      T=0.  
      DO 110 N=1,6
  110   T=T+E(N)**2 
      T=SQRT(T) 
      DO 115 N=1,6
  115   E(N)=E(N)/T 
c	find B using Maxwell equation.
      F(1)=-XKZ*QR(E(3),E(4),WR,WI) 
      F(2)=-XKZ*QI(E(3),E(4),WR,WI) 
      F(3)=XKZ*QR(E(1),E(2),WR,WI)-XKX*QR(E(5),E(6),WR,WI)
      F(4)=XKZ*QI(E(1),E(2),WR,WI)-XKX*QI(E(5),E(6),WR,WI)
      F(5)=XKX*QR(E(3),E(4),WR,WI)
      F(6)=XKX*QI(E(3),E(4),WR,WI)
	print*,'result B',(f(k),k=1,6)
	print*,'check equations'    ! order of indices checks
	k = 1
	f(1) = pr(A(k,1),B(k,1),E(1),E(2)) + pr(A(k,2),B(k,2),E(3),E(4))
     1		+ pr(A(k,3),B(k,3),E(5),E(6))
	f(2) = pi(A(k,1),B(k,1),E(1),E(2)) + pi(A(k,2),B(k,2),E(3),E(4))
     1		+ pi(A(k,3),B(k,3),E(5),E(6))
        print*,k,f(1),f(2)
	k = 2
	f(3) = pr(A(k,1),B(k,1),E(1),E(2)) + pr(A(k,2),B(k,2),E(3),E(4))
     1		+ pr(A(k,3),B(k,3),E(5),E(6))
	f(4) = pi(A(k,1),B(k,1),E(1),E(2)) + pi(A(k,2),B(k,2),E(3),E(4))
     1		+ pi(A(k,3),B(k,3),E(5),E(6))
        print*,k,f(3),f(4)
	k = 3
	f(5) = pr(A(k,1),B(k,1),E(1),E(2)) + pr(A(k,2),B(k,2),E(3),E(4))
     1		+ pr(A(k,3),B(k,3),E(5),E(6))
	f(6) = pi(A(k,1),B(k,1),E(1),E(2)) + pi(A(k,2),B(k,2),E(3),E(4))
     1		+ pi(A(k,3),B(k,3),E(5),E(6))
	print*,k,f(5),f(6)
      RETURN
      END 

