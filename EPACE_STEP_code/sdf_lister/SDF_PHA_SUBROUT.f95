c    Subroutines for the program Sdf_lister
c
c     Contents:
c
c             subroutine evaluate_pha
c
c             subroutine convert_data
c
c             real function lininterp
c
c             subroutine search
c
c             real function Einc_MeVnuc
c
c             real function mass_amu
c
c             function tof_fraction
c
c             real function spread1
c
c
c
c
c*******************************************************
c
      subroutine evaluate_pha
c
c*******************************************************
c
c     Processes PHA data
c
c     6/17/98  by J. Dwyer
c       11/10/98 removed data_flag, bas event mass= -99.9
c
c

      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real        mass_amu, Einc_MeVnuc,p1,p2   ! function declaration
      real        spread1
c
      do 100 i=1, NPHA   ! loop over PHA events
        effic_pha(i) = 0.0
        if (randomize_flag.eq.0) then
          call convert_data (tof,hi,lo,1.0*epha(i),1.0*tpha(i),
     1    ramp(i),tel(i),MeV(i),nsec(i),cal,ntof,nlo,nhi)   ! convert channel number to useful quantities
        else
          p1 = 1.0*epha(i)+spread1()
          p2 = 1.0*tpha(i)+spread1()
          call convert_data (tof,hi,lo,
     1    p1,p2,
     1    ramp(i),tel(i),MeV(i),nsec(i),cal,ntof,nlo,nhi)   ! convert channel number to useful quantities
        end if
        if ((nsec(i).gt.0.0).and.(MeV(i).gt.0.0)) then   ! require ns and MeV to be non-negative
          mass(i) = mass_amu(nsec(i), MeV(i))    ! get particle mass
          Einc(i) = Einc_MeVnuc(nsec(i),
     1                      einc_cal,tof_cal,N_cal)     ! get particle energy from TOF
          mass(i) = 1.01*mass(i)
          if ((mass(i).gt.8.0).and.
     1     (mass(i).lt.40.0)) mass(i) = 0.965*mass(i)  ! added 12/8/95 to correct element locations
        else
          mass(i) = -99.9    ! bad event
        end if
100        continue
c
      return
      end  ! end evaluate_pha
c
c
c
c
c*******************************************************
c
      subroutine convert_data(tof,hi,lo,energy,time,ramp,
     1                  tel,MeV,ns,cal,ntof,nlo,nhi)
c
c*******************************************************
c
c     Uses calibration data to calculate real quantities from channel numbers
c        modifications:
c         6/17/98  by J. Dwyer made time & energy real
c
      integer*4 cal,ntof,nlo,nhi,num   !  calibration data
      real   time, energy    ! channel numbers
      real tof(3,cal),hi(3,cal),lo(3,cal),p   ! calibration data parameters
      real lininterp,MeV,ns
      integer*4 ramp,tel,icol,high,low
c
c first do tof
      icol=2-tel
      p=real(time)
c      type *,p
      call search(tof,p,icol,cal,ntof,high,low)
c      type *,low,high
c      if ((low.eq.0).or.(high.eq.0)) goto 500
      ns=lininterp(tof(icol,high),tof(icol,low),tof(3,high),
     1                     tof(3,low),p)
c      type *,ns
c now do energy
      icol=2-tel

      icol=icol+1
      p=real(energy)
c ramp = 1 ?=? lo gain
      if (ramp.eq.1) then
        call search(lo,p,icol,cal,nlo,high,low)
c        if ((low.eq.0).or.(high.eq.0)) goto 500
        MeV=lininterp(lo(icol,high),lo(icol,low),lo(1,high),
     1                        lo(1,low),p)
      else
c telescope 1 (tel=1) has an additional calibration pt.
        num=nhi+tel
        call search(hi,p,icol,cal,num,high,low)
c        if ((low.eq.0).or.(high.eq.0)) goto 500
        MeV=lininterp(hi(icol,high),hi(icol,low),hi(1,high),
     1                        hi(1,low),p)
      endif
c      type *,MeV,ns
      return
      end    ! end convert_data
c
c
c
c
c****************************************************
c
      real function lininterp(x1,x2,y1,y2,p)
c
c****************************************************
c
      real x1,x2,y1,y2,p
c
      lininterp=(p*(y1-y2)+(x1*y2-x2*y1))/(x1-x2)
      end  ! end lininterp
c
c
c
c
c****************************************************
c
      subroutine search(x,p,icol,max,num,high,low)
c
c****************************************************
c
      integer*4 icol,max,num,high,low,i,j,half
      real x(3,max),p
c
      high=1
      low=1
      i=1
      j=num
      if (p.lt.x(icol,i)) then
        low=i
        high=i+1
        return
      end if
      if (p.gt.x(icol,j)) then
        low=j-1
        high=j
        return
      endif
10    half = (j-i+1)/2 + i
      if (p.lt.x(icol,half)) then
        j=half
      else
        i=half
      end if
      if ((j-i).gt.1) goto 10
      high=j
      low=i
      return
      end  ! end search
c
c
c
c
c****************************************************
c
      real function Einc_MeVnuc(tof_arg,
     1      einc_cal,tof_cal,N_cal)
c
c****************************************************
c
c	looks up TOF from Fe calculation with STEPMR
c	for TOF below 8 nsec uses analytic expression
c
c	gm 7/10/95  Modified by J.Dwyer 11/3/95
c
c
      real   tof_arg
      real	 einc_cal(100),tof_cal(100)
      integer  N_cal
c
100	  tof_nsec=tof_arg
      if(tof_nsec.le.0) goto 400
	    if(tof_nsec.lt.3.) goto 300
	    tof_nsec=alog(tof_nsec)
      if(tof_nsec.gt.tof_cal(1)) goto 400
	    do 110 i=2, N_cal
	      ientry=i
	      if(tof_nsec.ge.tof_cal(i)) goto 120
110	  continue
	    ientry=N_cal
120	   Einc_Mevnuc = (tof_nsec-tof_cal(ientry-1))/
     1     (tof_cal(ientry)-tof_cal(ientry-1))*
     1 (Einc_cal(ientry)-Einc_cal(ientry-1))
     1      + Einc_cal(ientry-1)
c	now exponentiate
	     Einc_MeVnuc=exp(Einc_MeVnuc)
	     return
c
c
c	for higher energies, just use analytic expression
c		to average path length > 10.0 cm 7/7/95
c
300   	Einc_mevnuc = 51.79/tof_nsec**2
c
c	but don't return a value above 10 MeV/nuc
c
      if(Einc_mevnuc.gt.10.) Einc_mevnuc=10.
	    return
c	trop for negative or very large TOFs
400	  Einc_mevnuc = 0.001
      return
	    end  ! end Einc_MeVnuc
c
c
c
c
c****************************************************
c
      real function mass_amu(nsec_arg,mev_arg)
c
c****************************************************
c
c	finds mass in AMU for STEP telescope given tof, en deposit
c
c	gm	19-Jul-95
c   12/1/00 by J. Dwyer, changed max mass to 300
c
      real  nsec, mev,tof_test,mass_test,
     1                   nsec_arg,mev_arg
      nsec=nsec_arg
	    mev=mev_arg
	    mass_amu=0.01
	    if(nsec.le.0.) return
	    if(mev.le.0.) return
	    approx_mass=mev*nsec*nsec/51.79
	    mass_test=approx_mass
c	now scan upwards approx mass:
c	jump out of routine if mass exceeds 300 (instabilities
c		pop up in tof_fraction at very high masses)
10	  if((mass_test.gt.300).or.(mass_test.le.0.)) goto 300
	    tof_test=sqrt(51.79*mass_test/mev)*
     *            tof_fraction(mass_test,mev)
      amul=nsec/tof_test
c	type *,mass_test,nsec,tof_test,amul
c	adjust mass_test
      mass_test=mass_test*amul
	    if((tof_test/nsec).lt.0.999) goto 10

c	if(((tof_test/nsec).gt.0.999).and.
c     *     ((tof_test/nsec).lt.1.001)) goto 20
c	goto 10
c
20	   mass_amu=mass_test
c
       if(mass_amu.lt.0.01) mass_amu=0.01
c	type *,' Mass: ',mass_amu
	     return
c	trap for large mass objects!
300  	mass_amu=0.01
	     return
	     end  ! end mass_amu
c
c
c
c
c****************************************************
c
      function tof_fraction(mass_test,mev)
c
c****************************************************
c
      real mass_test,mev,intercept
	    intercept = 1.0851 + 0.1443*alog10(mass_test/16.)
	    slope = 0.10978 + 0.1689*alog10(mass_test/16.)
	    tof_fraction = intercept + slope*alog10(mev/mass_test)
	    if(tof_fraction.gt.1.) tof_fraction = 1.
c	type *,' tof fraction, meV/n: ',tof_fraction,mev/mass_test
	    return
	    end  ! end tof_fraction
c
c
c
c
c***************************************************
c
      real function spread1()
c
c***************************************************
      DATA I1/987654321/
c       calls to random function made in one place in order to generate
c       a single list of random numbers in the program;  if function
c       ran() is located in separate places in the program, each separate
c       location will generate the same set of 'random' numbers
c
c       spread1 produces random numbers between 0. and +1.0
	call random_number(harvest)
        spread1 = harvest
c
        return
        end     ! end spread1
c
