c    Subroutines for the program sdf_lister
c    
c     Contents:
c             logical function open_output_pha
c
c             logical function evaluate_pha    
c
c             logical function write_output_pha
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
c****************************************************
c
       logical function open_output_pha(unit_pha)
c
c****************************************************
c 
c     Open output data file and write the header fo PHA data
c     returns .true. if successful, .false. otherwise
c
c     12/1/95 by J. Dwyer
c          modified 2/28/96 by J. Dwyer added headerlines output to write_header
c                         3/25/96 by J. Dwyer added 1/efficiency
c                         4/4/96 by J. Dwyer removed 1/efficiency
c                         4/4/96 by J. Dwyer added matrix counts to PHA output
c                         4/8/96 by J. Dwyer changed headerlines to 40
c                         4/9/96 by J. Dwyer removed matrix counts
c                         4/9/96 by J. Dwyer added year to PHA output
c                         6/17/96 by J. Dwyer added sector to titles
c
      include 'sdf_include.inc'   ! include type declarations
c 
      integer   unit_pha   ! file unit of output file
      character*64     filename       ! complete name of output file
      character*4       file_prefix   ! prefix to be attached to output file name
      integer   headerlines
c
      headerlines = 40
c    
      file_prefix = ' PHA'
      write(filename, '(a4,a60)') file_prefix, outputfile
      open(unit=unit_pha,name=outputdir//filename,
     1     status='NEW',DISP='KEEP',RECL=32766, err=1000) ! open output data file
      print *, 'Opened output file: ', filename
c
      call write_header(unit_pha,file_prefix, headerlines)
c
      write(unit_pha,200)  ! write column label
200	 format(' Time,',
     1   ' year,',
     1   ' day of the year,', 
     1   ' delta_t/2,',
     1   ' start rate (cnts/sec),',
     1   ' Einc (MeV/nuc), Mass (AMU),',
     1   ' MeV, ns, energy ch, time ch, ramp (1=lo gain), tel (1=D1),',
     1   ' Cal/normal (1=cal), slant (1=slant fired), SSd2 (1=fired),',
     1   ' State A/B (1=A), ROM box number, S/C spin counter,sector')
c
      open_output_pha = .true.
      return  !  succussful
1000   open_output_pha = .false.  ! on error go to here
      print *, 'Error opening output pha data file:', filename
      return      
      end  ! end open_output_pha
c
c
c
c
c*******************************************************
c
      logical function evaluate_pha(unit_pha, nrow)
c
c*******************************************************
c   
c     Processes PHA data and writes to PHA output file
c     returns .false. if number of output lines written exceeds
c     specified value, .true. otherwise.
c
c     12/8/95  by J. Dwyer
c                   modifications:
c                         3/25/96 by J. Dwyer added efficiency calculation
c                         4/3/96 by J. Dwyer corrected typo in startrate calculation
c                         4/4/96 by J. Dwyer removed efficiency calculation
c                         4/4/96 by J. Dwyer added matrix counts to PHA output
c                         4/4/96 by J. Dwyer added matrix counts to PHA output
c                         4/9/96 by J. Dwyer removed matrix counts
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real         mass_amu, Einc_MeVnuc   ! function declaration
      real         startrate   ! start discriminator rate (cnts/sec)
      real         MeV_upper,nsec_upper   ! used to randomize MeV and nsec
      real         MeV_lower,nsec_lower   ! used to randomize MeV and nsec
      real         lambda_t   ! spectral index wrt tof
      logical    startrate_flag  ! .true.=start rate within specified limits
      logical      write_output_pha     ! function declaration
      integer    nrow   !  number of rows written to output file    
      integer   unit_pha   ! output file unit
c    
      evaluate_pha = .true.  
      lambda_t = -2.0*lambda-3.0
      do 100 i=1, NPHA   ! loop over PHA events
             effic_pha(i) = 0.0
             if (data_flag(i)) then  ! O.K. PHA events
                if (randomize_flag.eq.0) then
                   call convert_data (tof,hi,lo,epha(i),tpha(i),
     1            ramp(i),tel(i),MeV(i),nsec(i),cal,ntof,nlo,nhi)   ! convert channel number to useful quantities
                else
                   call convert_data (tof,hi,lo,epha(i)+1,tpha(i)+1,
     1            ramp(i),tel(i),MeV_upper,nsec_upper,cal,ntof,nlo,nhi)   ! convert channel number to useful quantities
                   call convert_data (tof,hi,lo,epha(i),tpha(i),
     1            ramp(i),tel(i),MeV_lower,nsec_lower,cal,ntof,nlo,nhi)   ! convert channel number to useful quantities
                   if ((nsec_upper.gt.0.0).and. 
     1                (MeV_upper.gt.0.0).and.
     1                (nsec_lower.gt.0.0).and.
     1                (MeV_lower.gt.0.0)) then 
                       MeV(i) = (spread1()*((MeV_upper**(lambda+1))-
     1                (MeV_lower**(lambda+1.0)))+
     1                (MeV_lower**(lambda+1.0)))
     1                **(1.0/(lambda+1.0))
                       nsec(i) = (spread1()*
     1                ((nsec_upper**(lambda_t+1.0))-
     1                (nsec_lower**(lambda_t+1.0)))+
     1                (nsec_lower**(lambda_t+1.0)))
     1                **(1.0/(lambda_t+1.0))
                   else
                       MeV(i) = 0.0
                       nsec(i) = 0.0
                   end if
                end if
                if ((nsec(i).gt.0.0).and.(MeV(i).gt.0.0)) then   ! require ns and MeV to be non-negative
                    mass(i) = mass_amu(nsec(i), MeV(i))    ! get particle mass         
                    Einc(i) = Einc_MeVnuc(nsec(i), 
     1                           einc_cal,tof_cal,N_cal)     ! get particle energy from TOF  
                    if ((Einc(i).lt.min_Einc).or.
     1                 (Einc(i).ge.max_Einc).or.
     1                 (mass(i).lt.min_mass).or.
     1                 (mass(i).ge.max_mass)) 
     1                            data_flag(i) = .false.
c
                    mass(i) = 1.01*mass(i)
                    if ((mass(i).gt.8.0).and.
     1             (mass(i).lt.40.0)) mass(i) = 0.965*mass(i)  ! added 12/8/95 to correct element locations
c                

                 else
                    data_flag(i) = .false.    ! bad event
                 end if
             end if
100        continue
c
c
      startrate_flag = .false.    
      startrate = start_1mf/(spin_period*nspins)
      if ((startrate.ge.min_start).and.
     1      (startrate.lt.max_start)) startrate_flag = .true.
c     
      if ((write_pha_data.eq.1).and.
     1   (startrate_flag)) evaluate_pha =
     1       write_output_pha(unit_pha, nrow, startrate)  ! write processed data to output file
c
      return
      end  ! end evaluate_pha
c
c
c
c

c
c
c
c****************************************************
c
       logical function write_output_pha(unit_pha, 
     1               nrow,startrate)
c
c****************************************************
c
c      writes PHA data to output file with file unit=unit_out
c      returns .false. if number of output lines written exceeds
c      specified value, .true. otherwise.
c
c     11/20/95 by J. Dwyer
c                  modifications:
c                         3/25/96 by J. Dwyer added 1/efficiency
c                         4/4/96 by J. Dwyer removed 1/efficiency
c                         4/4/96 by J. Dwyer added matrix counts to PHA output
c                         4/9/96 by J. Dwyer removed matrix counts
c                         4/9/96 by J. Dwyer added year to PHA output
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real         startrate   ! start discriminator rate (cnts/sec)
      real  doy_real
      real  half_delta_t  ! 1/2 of elapsed time interval (sec)
      integer    mid_time
      integer   yr, mo, dy, hr, mn, se, doy
      integer   unit_pha   ! output file unit
      integer    nrow   !  number of rows written to output file   
      character*20   isotime   ! sampex_timecon isotime array
c
      mid_time = (end_time+start_time)/2
      half_delta_t = (end_time-start_time)/2.0
      call sampex_timcon(mid_time,yr,mo,dy,
     1           hr,mn,se,doy,isotime)
      doy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0
c
      do 200 i=1, NPHA   ! loop over PHA events
         if (data_flag(i)) then  ! O.K. events
            if (nrow.lt.maximum_rows) then           
              write(unit_pha,100) mo, dy, yr, hr, mn, se,
     1              yr, doy_real, half_delta_t, 
     1              startrate,
     1              Einc(i), mass(i), MeV(i), nsec(i),
     1              epha(i), tpha(i), ramp(i), 
     1              tel(i), cn(i), slant(i),
     1              ssd2(i), ab(i), rom(i), spin(i), sect(i)
100 	     format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1            ', 'i4', 'f12.8', 'f12.4', ',
     1            E12.5', ',
     1            4(f8.3', '),i4', 'i3,', ',6(i1,', '),3(i2,', ')', ')
              nrow = nrow + 1
            else
               print *, 'Number of rows exceeds maximum specified:',
     1          maximum_rows
               write_output_pha = .false.
               return
            end if
          end if
200    continue
c
      write_output_pha = .true.
      return
      end   ! end write_output
c
c
c
c
c*******************************************************
c
      subroutine convert_data (tof,hi,lo,energy,time,ramp,
     1                  tel,MeV,ns,cal,ntof,nlo,nhi)
c
c*******************************************************
c
c     Uses calibration data to calculate real quantities from channel numbers
c
      Integer cal,ntof,nlo,nhi,num   !  calibration data
      integer   time, energy    ! channel numbers
      Real tof(3,cal),hi(3,cal),lo(3,cal),p   ! calibration data parameters
      Integer ramp,tel,icol,high,low  
      Real lininterp,MeV,ns    
c   
c first do tof
      icol=2-tel
      p=real(time)
c      print *,p
      call search(tof,p,icol,cal,ntof,high,low)
c      print *,low,high
c      if ((low.eq.0).or.(high.eq.0)) goto 500
      ns=lininterp(tof(icol,high),tof(icol,low),tof(3,high),
     1                     tof(3,low),p)
c      print *,ns
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
c      print *,MeV,ns
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
      integer icol,max,num,high,low,i,j,half
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
      endif
      if (p.gt.x(icol,j)) then
        low=j-1
        high=j
        return
      endif
 10   half = (j-i+1)/2 + i
      if (p.lt.x(icol,half)) then
        j=half
      else
        i=half              
      endif
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
c	jump out of routine if mass exceeds 100 (instabilities
c		pop up in tof_fraction at very high masses)
10	  if((mass_test.gt.100).or.(mass_test.le.0.)) goto 300
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
        spread1 = harveat
c
        return
        end     ! end spread1
c
