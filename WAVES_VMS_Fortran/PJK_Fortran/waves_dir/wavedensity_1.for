----------
X-Sun-Data-Type: text
X-Sun-Data-Description: text
X-Sun-Data-Name: text
X-Sun-Content-Lines: 0

----------
X-Sun-Data-Type: default
X-Sun-Data-Description: default
X-Sun-Data-Name: paul.mai
X-Sun-Content-Lines: 163



Hi again Paul,

I've cut out the bits I added, all of them I think, to calculate density fluctuations.
If you want, you can just copy my OSCARS.for from user_a:[bale.numeric.dispersion].  
I was doing this for 3 ion species and hard-coded all three outputs.  You should just
delete ions 2 and 3 if you don't need them.  


This writes to units 10 and 11, the absolute value and phase of the density variations
as a function of the frequency and k returned in unit 8.  


Stuart



The first few lines below are from the virgin OSCARS.for.  




  235   IF (NPR.LT.2) GO TO 250 
        CALL WAVPOL (EPSR,EPSI,E,B,VAR(NF-3),VAR(NF-2),XKX,XKZ) 
        WRITE (7,350) 
  999	format(5(f16.8,x))
c
c
c
c	..attempt to calculate density fluctuations from the continuity 
c		equation	11/23/92
c
c
c	..fixed up on 6/18/93
c
c
	emag = sqrt(e(1)**2. + e(2)**2. + e(3)**2. + e(4)**2. + e(5)**2.
	1		+ e(6)**2.) 
c
c	print*,'emag = ',emag		!this should be 1
c
	del_nr = 0
	del_ni = 0
c
c	electrons (n=1)
c
c	..first calculate the complex matrix product
c
c
	do jj=1,3
		del_nr = del_nr + 			!real part
	1		xd(4)*(dieli(1,1,jj)*E(jj) + dielr(1,1,jj)*E(jj+3)) 
	1		+ xd(5)*(dieli(1,3,jj)*E(jj)+ dielr(1,3,jj)*E(jj+3))
c
		del_ni = del_ni +			!imaginary part 
	1		xd(4)*(dielr(1,1,jj)*E(jj) - dieli(1,1,jj)*E(jj+3))
	1		+ xd(5)*(dielr(1,3,jj)*E(jj) - dielr(1,3,jj)*E(jj+3))
c
	end do
c
c
c	..then include the real scalars
c
c
	del_ner = del_nr*ratiom(1)/(dens(1)*abs(ratiom(1))
	1		*(xd(1)**2.))
	del_nei = del_ni*ratiom(1)/(dens(1)*abs(ratiom(1))
	1		*( xd(1)**2.))
	del_nr = 0
	del_ni = 0
c
c	ions  1 (n=2)
c
c	..first calculate the complex matrix product
c
c
	do jj=1,3
		del_nr = del_nr + 			!real part
	1		xd(4)*(dieli(2,1,jj)*E(jj) + dielr(2,1,jj)*E(jj+3)) 
	1		+ xd(5)*(dieli(2,3,jj)*E(jj)+ dielr(2,3,jj)*E(jj+3))
c
		del_ni = del_ni +			!imaginary part 
	1		xd(4)*(dielr(2,1,jj)*E(jj) - dieli(2,1,jj)*E(jj+3))
	1		+ xd(5)*(dielr(2,3,jj)*E(jj) - dielr(2,3,jj)*E(jj+3))
c
	end do
c
c
	del_n1r = del_nr*ratiom(2)/(dens(2)*abs(ratiom(2))
	1		*(xd(1)**2.))
	del_n1i = del_ni*ratiom(2)/(dens(2)*abs(ratiom(2))
	1		*( xd(1)**2.))
	del_nr = 0
	del_ni = 0
c
c	ions  2 (n=3)
c
c	..first calculate the complex matrix product
c
c
	do jj=1,3
		del_nr = del_nr + 			!real part
	1		xd(4)*(dieli(3,1,jj)*E(jj) + dielr(3,1,jj)*E(jj+3)) 
	1		+ xd(5)*(dieli(3,3,jj)*E(jj)+ dielr(3,3,jj)*E(jj+3))
c
		del_ni = del_ni +			!imaginary part 
	1		xd(4)*(dielr(3,1,jj)*E(jj) - dieli(3,1,jj)*E(jj+3))
	1		+ xd(5)*(dielr(3,3,jj)*E(jj) - dielr(3,3,jj)*E(jj+3))
c
	end do
c
c
	del_n2r = del_nr*ratiom(3)/(dens(3)*abs(ratiom(3))
	1		*(xd(1)**2.))
	del_n2i = del_ni*ratiom(3)/(dens(3)*abs(ratiom(3))
	1		*( xd(1)**2.))
	del_nr = 0
	del_ni = 0
c
c	ions 3 (n=4)
c
c	..first calculate the complex matrix product
c
c
	do jj=1,3
		del_nr = del_nr + 			!real part
	1		xd(4)*(dieli(4,1,jj)*E(jj) + dielr(4,1,jj)*E(jj+3)) 
	1		+ xd(5)*(dieli(4,3,jj)*E(jj)+ dielr(4,3,jj)*E(jj+3))
c
		del_ni = del_ni +			!imaginary part 
	1		xd(4)*(dielr(4,1,jj)*E(jj) - dieli(4,1,jj)*E(jj+3))
	1		+ xd(5)*(dielr(4,3,jj)*E(jj) - dielr(4,3,jj)*E(jj+3))
c
	end do
c
c
	del_n3r = del_nr*ratiom(4)/(dens(4)*abs(ratiom(4))
	1		*(xd(1)**2.))
	del_n3i = del_ni*ratiom(4)/(dens(4)*abs(ratiom(4))
	1		*( xd(1)**2.))
c
c
c
c	..write density fluctuations--real and imaginary
c	..density fluctuations are real, the fact that there exist real
c		and imaginary part (positive and negative) is 
c		phase information
c
c
	del_ne = sqrt(del_ner**2 + del_nei**2)
	del_n1 = sqrt(del_n1r**2 + del_n1i**2)
	del_n2 = sqrt(del_n2r**2 + del_n2i**2)
	del_n3 = sqrt(del_n3r**2 + del_n3i**2)
c
c
	dnphase_e = atand(del_nei/del_ner)
	dnphase_1 = atand(del_n1i/del_n1r)
	dnphase_2 = atand(del_n2i/del_n2r)
	dnphase_3 = atand(del_n3i/del_n3r)
c
        WRITE(10,*)  del_ne, del_n1,del_n2,del_n3
        WRITE(11,*)  dnphase_e, dnphase_1,dnphase_2,dnphase_3
================== RFC 822 Headers ==================
Received: from laplace.maths.qmw.ac.uk by epsilon.qmw.ac.uk with SMTP-DNS (PP) 
          id <14501-0@epsilon.qmw.ac.uk>; Fri, 12 May 1995 10:19:19 +0100
Received: from ariel.maths.qmw.ac.uk by laplace.maths.qmw.ac.uk;
          Fri, 12 May 95 10:18:20 BST
From: Stuart D Bale <S.D.Bale@qmw.ac.uk>
Date: Fri, 12 May 95 10:19:11 BST
Message-Id: <8635.9505120919@ariel.maths.qmw.ac.uk>
To: kellogg@waves.spa.umn.edu
Subject: disper
Content-Type: X-sun-attachment
