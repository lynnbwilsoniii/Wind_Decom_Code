c    Subroutines for the program sdf_lister
c    
c     Contents:
c
c        subroutine read_pha_cal
c
c        subroutine get_geofactor
c
c        subroutine get_track_effic
c
c        subroutine get_ROMboxes
c
c
c
c
c***************************************************
c
      subroutine read_pha_cal(unit_cal)
c
c***************************************************
c
c     Gets calibration data for STEP PHA data
c     4/21/98  by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
      include 'include_paths.inc'
c
	 data d2_factor/0.98/
c
      real fwhm1,fwhm2
      integer*4   unit_cal
      integer*4    i      
c
c
c read tof calibration data
      open(unit=unit_cal,file=adjustl(trim(calibration_path)) // calib_file_tof,
     1   status='OLD',action='read',err=1000)
      do i=1,ntof
        read(unit_cal,*,end=50,err=1000)
     1     tof(3,i),tof(1,i),tof(2,i)
      end do
50     continue
      close(unit_cal,err=1000)
c
c read lo gain calibration data
      open (unit=unit_cal,file=adjustl(trim(calibration_path)) // calib_file_lo,
     1   status='OLD', action='read',err=1000)
      do i=1,nlo-1
         read(unit_cal,*,end=150,err=1000)
     1    lo(1,i),lo(2,i),fwhm1,lo(3,i),fwhm2
      end do
c Last calibration pt has no fwhm data
      read(unit_cal,*,end=150,err=1000)
     1    lo(1,nlo),lo(2,nlo),lo(3,nlo)
c      type *,lo(1,nlo),lo(2,nlo),lo(3,nlo)
150   close (unit_cal,err=1000)
c
c read hi gain calibration data
      open(unit=unit_cal,file=adjustl(trim(calibration_path)) // calib_file_hi,
     1  status='OLD', action='read',err=1000)
      do  i=1,nhi
        read(unit_cal,*,end=250,err=1000)
     1    hi(1,i),hi(2,i),fwhm1,hi(3,i),fwhm2
c      type *,hi(1,i),hi(2,i),hi(3,i)
      end do
c
c telescope 1 has an additional data pt
      read(unit_cal,*,end=250,err=1000)
     1    hi(1,nhi+1),hi(2,nhi+1),fwhm1
c      type *,hi(1,nhi+1),hi(2,nhi+1),hi(3,nhi+1)
250   close(unit_cal,err=1000)
c
c	multiply D2 calibration by the d2_factor
c
      do i=1,nhi
        hi(3,i)=d2_factor*hi(3,i)
      end do
      do  i=1,nlo
	   lo(3,i)=d2_factor*lo(3,i)
      end do
c
c     
      open(unit=unit_cal,name=adjustl(trim(calibration_path)) // calib_file_einctof,
     1   status='old',action='read',err=1000)
c	skip over header lines
      do i=1,21
	   read(unit_cal,300,err=1000)
	 end do  
300	   format(a1)
      N_cal = 1
320	  read(unit_cal,*,end=380,err=1000)
     1      einc_cal(N_cal),tof_cal(N_cal)
      N_cal = N_cal + 1
      goto 320
380  	 N_cal = N_cal-1
      close(unit_cal,err=1000)
c      
c	take logs of einc_cal and tof_cal
	 do i=1,N_cal
	   einc_cal(i)=alog(einc_cal(i))
	   tof_cal(i)=alog(tof_cal(i))
      end do
c
      return
1000  type *,'Error getting pha calibration data!:   ',  name, file
      stop
      end  ! end read_pha_cal
c
c
c
c
c******************************************************
c
      subroutine get_geofactor(unit_cal)
c
c******************************************************
c
c     read in geometry factor
c     4/21/98 by J. Dwyer   
c     
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
      include 'include_paths.inc'
c      
      integer*4     unit_cal	
c
c	 read in STEP geometry factors
      open(unit=unit_cal,name=adjustl(trim(calibration_path)) // calib_file_geom,
     1  status='old',action='read',err=1000)
	    read(unit_cal,5,err=1000)
5	   format(x)
      read(unit_cal,*,err=1000) 
     1    geofactor(1),geofactor(2)  ! geometry factors
	 close(unit_cal,err=1000)
c
      return
1000  type *,'Error getting geofactor!'
      stop
      end  ! end get_geofactor
c
c
c
c
c******************************************************
c
      subroutine get_track_effic(unit_cal)
c
c******************************************************
c
c	 read in STEP track efficiency calibration file
c     4/21/98 by J. Dwyer   
c     
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
      include 'include_paths.inc'
c      
      integer*4     unit_cal
      integer*4    i      
      character*20  rate_name_track	
c
      open(unit=unit_cal,name=adjustl(trim(calibration_path)) // calib_file_track,
     1    status='old',action='read',err=1000)
c      inquire(unit_cal,name=calib_file_track)
	    read(unit_cal,5,err=1000)
5	   format(x)
      track_effic(1,1) = 1.0   ! state 1
      track_effic(1,2) = 1.0   ! state 2
      track_effic(39,1) = 1.0   ! junk
      track_effic(39,2) = 1.0   ! junk
      track_effic(40,1) = 1.0   ! junk
      track_effic(40,2) = 1.0   ! junk
      track_effic(41,1) = 1.0   ! junk
      track_effic(41,2) = 1.0   ! junk
      do i= 2,38
	   read(unit_cal,*,err=1000) rate_name_track,
     1          track_effic(i,1),track_effic(i,2) 
      end do
      close(unit_cal,err=1000)
c
      return
1000  type *,'Error getting track effic!'
      stop
      end  ! end get_track_effic
c
c
c
c
c
c******************************************************
c
      subroutine get_ROMboxes(unit_cal)
c
c******************************************************
c
c	 read in STEP ROM box calibration file
c     7/2/98 by J. Dwyer   
c     
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
      include 'include_paths.inc'
c      
      real    de
      integer*4     unit_cal
      integer*4    i,j
      character*20   rate_name
	character*120	rombox_version
	
	

c
      do 900,  i_vers = 30, 1, -1
      if( i_vers .ge. 10 )write(rombox_version, 910) adjustl(trim(calibration_path)),
     *         adjustl(trim(current_ROMboxfile)),i_vers
      if( i_vers .lt. 10 )write(rombox_version, 911) adjustl(trim(calibration_path)),
     *         adjustl(trim(current_ROMboxfile)),i_vers
910	format(a, a, ';', i2)
911	format(a, a, ';', i1)

c	type *, 'attempting to open: ', rombox_version
      open(unit=unit_cal,name=rombox_version,
     1    status='old',action='read',err=900)
c	opened file, so leave loop
	goto 950
900	continue
	type *, 'failed to open file'
	stop
950		type *, 'opened: ', rombox_version
c      inquire(unit_cal,name=calib_file_rombox)
	    read(unit_cal,5,err=1000)
5	   format(x)
c	first fill up elo, ehi, effic with dummy values for
c	state rates, and junk; then read in rates with real
c	intervals and efficiencies
      do i=1,41
        do j=1,2
          elo(i,j) = 1.0
          ehi(i,j) = 2.0
	     effic(i,j)=1.0
	     effic_low(i,j) = 1.0
          effic_high(i,j) = 1.0
        end do
      end do

      do  i= 2,38
	   read(unit_cal,*,err=1000) rate_name,
     1    (elo(i,j),ehi(i,j),de,
     1     effic_low(i,j),effic_high(i,j),effic(i,j),j=1,2) 
      end do
      close(unit_cal,err=1000)
c
      do i=1,41
        do j=1,2            
           if ((effic(i,j).le.0.0).or. 
     1       (track_effic(i,j).le.0.0)) then
              effic_flag(i,j) = .false.
              effic(i,j) = -1.0
              effic_low(i,j) = -1.0
              effic_high(i,j) = -1.0
           else
             effic_flag(i,j) = .true.
             effic(i,j) = effic(i,j)*track_effic(i,j)
             effic_low(i,j) = effic_low(i,j)*track_effic(i,j) 
             effic_high(i,j) = effic_high(i,j)*track_effic(i,j)              
           end if
        end do
      end do
c
      return
1000  type *,'Error getting ROM box data:  ',  adjustl(trim(calibration_path)) //current_ROMboxfile
      stop
      end  ! end get_ROMboxes
c
c
c
