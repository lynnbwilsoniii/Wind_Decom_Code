c    Subroutines for the program sdf_lister
c    
c     Contents:
c        subroutine read_pha_cal
c
c        subroutine read_matrix_cal
c
c        subroutine read_pha_flux_cal
c
c
c
c***************************************************
c
      subroutine read_pha_cal(calib_file_tof,
     1      calib_file_lo, calib_file_hi,
     1      calib_file_einctof,
     1      tof,hi,lo,cal,
     1      ntof,nlo,nhi,einc_cal,tof_cal, N_cal)
c
c***************************************************
c
c  Gets calibration data for STEP PHA data
c
c
c
	    data d2_factor/0.98/
c
      Integer cal,i,ntof,nlo,nhi,N_cal
      real tof(3,cal),hi(3,cal),lo(3,cal),fwhm1,fwhm2
      real	 einc_cal(100),tof_cal(100)
      character*120     calib_file_tof
      character*120     calib_file_lo
      character*120     calib_file_hi
      character*120     calib_file_einctof
	include 'include_paths.inc'
c
c read tof calibration data
	calib_file_tof = adjustl(trim(calibration_path))//adjustl(trim(calib_file_tof))
c	type *, 'opening: ',calib_file_tof
      Open(unit=20,file=calib_file_tof,
     1   status='OLD',action='read',err=1000)
      do 20 i=1,ntof
        Read (20,*,end=50) tof(3,i),tof(1,i),tof(2,i)
c      print *,i,tof(1,i),tof(2,i),tof(3,i)
20     continue
50     continue
      close (20,err=1000)
c
c read lo gain calibration data
	calib_file_lo = adjustl(trim(calibration_path))//adjustl(trim(calib_file_lo))
c	type *, 'opening: ',calib_file_lo
      open (unit=20,file=calib_file_lo,
     1   status='OLD', action='read',err=1000)
      Do 120 i=1,nlo-1
      Read (20,*,end=150) lo(1,i),lo(2,i),fwhm1,lo(3,i),fwhm2
c      print *,lo(1,i),lo(2,i),lo(3,i)
120    continue
c Last calibration pt has no fwhm data
      Read (20,*,end=150) lo(1,nlo),lo(2,nlo),lo(3,nlo)
c      print *,lo(1,nlo),lo(2,nlo),lo(3,nlo)
150   close (20,err=1000)
c
c read hi gain calibration data
	calib_file_hi = adjustl(trim(calibration_path))//adjustl(trim(calib_file_hi))
c	type *, 'opening: ',calib_file_hi	
      open (unit=20,file=calib_file_hi,
     1  status='OLD', action='read',err=1000)
      Do 220 i=1,nhi
      Read (20,*,end=250) hi(1,i),hi(2,i),fwhm1,hi(3,i),fwhm2
c      print *,hi(1,i),hi(2,i),hi(3,i)
220   continue
c telescope 1 has an additional data pt
      Read (20,*,end=250) hi(1,nhi+1),hi(2,nhi+1),fwhm1
c      print *,hi(1,nhi+1),hi(2,nhi+1),hi(3,nhi+1)
250   close (20,err=1000)
c
c	multiply D2 calibration by the d2_factor
c
      do 300 i=1,nhi
        hi(3,i)=d2_factor*hi(3,i)
300	continue
      do 301 i=1,nlo
	      lo(3,i)=d2_factor*lo(3,i)
301	continue
c
c
c     
	calib_file_einctof = adjustl(trim(calibration_path))//adjustl(trim(calib_file_einctof))
c	type *, 'opening: ',calib_file_einctof
      open(unit=20,name=calib_file_einctof,
     1   status='old',action='read',err=1000)
      do 315 i=1,21
c	skip over header lines
315	   read(20,316)
316	   format(a1)
      N_cal=1
320	  read(20,*,end=380) einc_cal(N_cal),tof_cal(N_cal)
      N_cal=N_cal+1
      goto 320
380  	N_cal=N_cal-1
	    close(20)
c	take logs of einc_cal and tof_cal
	    do 385 i=1,N_cal
	       einc_cal(i)=alog(einc_cal(i))
	       tof_cal(i)=alog(tof_cal(i))
385	  continue
c
      return
1000  print *,'error opening or closing cal. file'
      stop
      end  ! end read_pha_cal
c
c
c
c
c******************************************************
c
      subroutine read_matrix_cal(calib_file_geom,
     1                   calib_file_rombox,
     1                   calib_file_track,
     1                    aom, rate_name,
     1                    elo,ehi,de,el,eh,effic,effic_flag)
c
c******************************************************
c
c     read in calibration data for matrix rates
c     11/13/95
c     modifications:
c                4/8/96 by J. Dwyer added calib_file_track file input
c
      real		elo(41,2)       ! lower energy bound for flux bin
	    real		ehi(41,2)	! upper energy bound for flux bin
	    real		aom(2)		! geometry factors tel 1,2
	    real		effic(41,2)	! efficiencies for each flux bin
      real    track_effic(41,2)  ! fraction of mass track in ROM box
      real   de,el,eh
      logical effic_flag(41,2)  ! .true.= ROM box O.K. to use,.false.=don't use this box
      character*10      rate_name(41),
     1     rate_name_track(41)      
      character*120     calib_file_geom
      character*120     calib_file_rombox
      character*120     calib_file_track
	include 'include_paths.inc'
c
c	read in STEP geometry factors
	calib_file_geom = adjustl(trim(calibration_path))//adjustl(trim(calib_file_geom))
	type *, 'opening: ',calib_file_geom
      open(unit=21,name=calib_file_geom,
     1  status='old',action='read')
	    read(21,5)
5	   format(x)
      read(21,*) aom  ! geometry factors
	    close(21)
c
c	read in STEP calibrate file
c      calibfile = 'umstep::$step_data:
c     1     [data.cal]STEP_ROM_BOX_EFF.KAL'
	calib_file_rombox = adjustl(trim(calibration_path))//adjustl(trim(calib_file_rombox))
	type *, 'opening: ',calib_file_rombox
      open(unit=21,name=calib_file_rombox,
     1    status='old',action='read')
      inquire(21,name=calib_file_rombox)
	    read(21,5)
c	first fill up elo, ehi, effic with dummy values for
c	state rates, and junk; then read in rates with real
c	intervals and efficiencies
      do 9 i=1,41
        do 9 j=1,2
          elo(i,j)=1.
          ehi(i,j)=2.
9	      effic(i,j)=1.
      do 10 i=2,38
	       read(21,*) rate_name(i),
     1     (elo(i,j),ehi(i,j),de,el,eh,effic(i,j),j=1,2)        
10	continue
      close(21)
c
c	read in STEP track efficiency calibration file
	calib_file_track = adjustl(trim(calibration_path))//adjustl(trim(calib_file_track))
	type *, 'opening: ',calib_file_track
      open(unit=21,name=calib_file_track,
     1    status='old',action='read')
      inquire(21,name=calib_file_track)
	    read(21,5)
      track_effic(1,1) = 1.0   ! state 1
      track_effic(1,2) = 1.0   ! state 2
      track_effic(39,1) = 1.0   ! junk
      track_effic(39,2) = 1.0   ! junk
      track_effic(40,1) = 1.0   ! junk
      track_effic(40,2) = 1.0   ! junk
      track_effic(41,1) = 1.0   ! junk
      track_effic(41,2) = 1.0   ! junk
      do 15 i=2,38
	       read(21,*) rate_name_track(i),
     1          track_effic(i,1), track_effic(i,2) 
15	continue
      close(21)
c
      do 20 i=1,41
        do 20 j=1,2            
           if ((effic(i,j).le.0.0).or. 
     1       (track_effic(i,j).le.0.0)) then
              effic_flag(i,j) = .false.
              effic(i,j) = -1.0
           else
              effic_flag(i,j) = .true.
              effic(i,j) = effic(i,j)*track_effic(i,j)             
           end if
20      continue
c
      return
      end  ! end read_matrix_cal
c
c
c
c
c*******************************************************
c
      subroutine read_pha_flux_cal
     1     (calib_file_phaflux,
     1     pha_flux_names, atomic_charge,
     1     atomic_mass_mid,
     1     atomic_mass_lo,atomic_mass_hi,
     1     PHA_E_mid,
     1     PHA_E_lo, PHA_E_hi,
     1     PHA_slant_flag, PHA_tel_flag,
     1     nfluxes)
c
c*******************************************************
c
c	  read in STEP PHA flux calibartion file
c    12/5/95 by J. Dwyer
c        modifications
c            3/21/96 by J. Dwyer added atomic_charge
c            4/4/96 by J. Dwyer  changed format slightly
c            4/25/96 by J. Dwyer added PHA_tel_flag
c            4/26/96 by J. Dwyer changed PHA_stateab_flag to PHA_slant_flag
c
      character*120   calib_file_phaflux  ! file name
      real    atomic_charge(1000)   ! atomic charge
      real    atomic_mass_lo(1000), 
     1          atomic_mass_hi(1000)   !  mass range for PHA fluxes
      real   PHA_E_lo(100), PHA_E_hi(1000)  !  energy range for PHA fluxes
      real   atomic_mass_mid(1000) ! mass of each bin used to calculate efficiency
      real   PHA_E_mid(1000) ! energy of each bin used to calculate efficiency
      integer   nfluxes  ! number of PHA fluxes to be calculated	 
      integer   PHA_slant_flag(1000)  ! state A/B flag (0= B only,1= A only, -1=both) 
      integer   PHA_tel_flag(1000)  ! telescope flag (1= tel1,2= tel2, -1=both)     
      character*20   pha_flux_names(1000)  ! names of PHA fluxes   

c      
	calib_file_phaflux = '/Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/STEP_PHA_FLUX_CAL.DAT;37'
		type *, 'opening: ',calib_file_phaflux
      open(unit=21,name=calib_file_phaflux,
     1    status='old',action='read')
c
      read(21,*,err=500)     ! read comment line
      read(21,*,err=500)     ! read comment line
      read(21,*,err=500)     ! read comment line    
      read(21,*,err=500)     ! read comment line
      nfluxes=0
      do 50 i=1,1000
         read(21,*,end=100,err=500) 
     1     pha_flux_names(i), atomic_charge(i),
     1     atomic_mass_mid(i),
     1     atomic_mass_lo(i),atomic_mass_hi(i),
     1     PHA_E_mid(i),
     1     PHA_E_lo(i), PHA_E_hi(i),
     1     PHA_slant_flag(i),
     1     PHA_tel_flag(i)      
	       nfluxes=i
50     continue	 
c
100      continue
       close (21)
       return      ! successful
c
500   print *, 'Error reading calibration file:', 
     1      calib_file_phaflux
      close (21)
      return   ! problems
      end  ! end read_flux_cal
c
