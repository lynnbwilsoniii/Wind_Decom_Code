c
c    Subroutines for the program sdf_lister
c    
c     Contents:
c
c          logical function get_expose
c     
c          subroutine check_expose
c
c          logical function get_mask
c     
c          subroutine check_mask
c
c
c
c*******************************************************
c
       logical function get_expose()
c
c*******************************************************
c
c    Open the expose file and read time interval window 
c    function returns .true. if file successfully opened and read,
c    .false. otherwise
c
c     11/27/95 by J. Dwyer
c         modified 11/15/96 by J. Dwyer: got rid of Nexpose read
c
      include 'sdf_include.inc'   ! include type declarations
c         
      integer     unit_expose        ! file unit for config file                                   
      
c  
      unit_expose = 19
      get_expose = .true.
c
      open(unit=unit_expose,name=expose_file, 
     1status='old',action='read', disp='keep', err=400)  ! open the config file
c
      Nexpose = 0
      do 50 i=1,10
      read(unit_expose,*,err=500,end=200)   ! skip past header
50      continue      
      do 100, i=1,5000
         read(unit_expose,*,err=500,end=200)   
     1       startyear_expose(i),  starttime_expose(i),
     1       stopyear_expose(i),  stoptime_expose(i)  ! start and stop times for selected interval         
      Nexpose = Nexpose + 1
100     continue 
200     continue
      if (Nexpose.ge.4999) 
     1     print *,'Warning expose file too big!'
      close(unit=unit_expose, err=400) 
      return        !  successful
c
400      get_expose = .false.   ! on error go to here
      print *, 'Error opening or closing expose file:', 
     1     expose_file
      return
500      get_expose = .false.   ! on error go to here
      print *, 'Error reading expose file:', expose_file
      close(unit=unit_expose, err=400) 
      return
      end  ! end get_expose
c
c
c
c
c*******************************************************
c
       subroutine check_expose
c
c*******************************************************
c
c    checks if data packet is in selected time interval
c
c     11/27/95 by J. Dwyer
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real  doy_real
      integer   midtime
      integer   yr, mo, dy, hr, mn, se, doy
      integer    newexposeflag
      character*20   isotime   ! sampex_timecon isotime array
c
      mid_time = (end_time+start_time)/2.0     
      call sampex_timcon(mid_time,yr,mo,dy,hr,mn,se,doy,isotime)
      doy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0 
c
      newexposeflag = 0     
      do 100, i=1,Nexpose             
          if ((((doy_real.ge.starttime_expose(i)).and.
     1         (yr.eq.startyear_expose(i))).or.
     1         (yr.gt.startyear_expose(i))).and.
     1         (((doy_real.lt.stoptime_expose(i)).and.
     1         (yr.eq.stopyear_expose(i))).or.
     1         (yr.lt.startyear_expose(i)))) then
             newexposeflag = i
          end if
100     continue            
c
      if (expose_interval_flag.eq.0) then 
         if (newexposeflag.eq.0) then         
             return   
         end if  ! do nothing--no good time intervals yet 
         if (newexposeflag.gt.0) then
             call  reset_time   !  reset accumulated variables
             expose_interval_flag = newexposeflag
             return
         end if ! start accumulating data--starting good time interval
      else
         if ((newexposeflag.eq.0).or. 
     1     (expose_interval_flag.ne.newexposeflag)) then
             expose_interval_flag = newexposeflag
             reset_interval = 1
             return  
         end if ! left current good time interval, write results
         if (expose_interval_flag.eq.newexposeflag) then                       
             return  
         end if ! continue accumulating data--still in current good time interval
      end if
c
      return
      end   ! end check_expose
c
c
c
c
c
c*******************************************************
c
       logical function get_mask()
c
c*******************************************************
c
c    Open the mask file and read time interval mask 
c    function returns .true. if file successfully opened and read,
c    .false. otherwise
c
c     11/27/95 by J. Dwyer
c             modified 9/4/96 by J. Dwyer: New format
c             modified 11/15/96 by J. Dwyer: got rid of flags in read
c
      include 'sdf_include.inc'   ! include type declarations
c         
      integer     unit_mask        ! file unit for config file                                   
      integer     satflag(8)        ! flag that tells what saturated
c  
      unit_mask = 19
      get_mask = .true.
c
      open(unit=unit_mask,name=mask_file, status='old',     
     1    action='read', disp='keep', err=400)  ! open the config file
c
      do 50 i=1,10
      read(unit_mask,*,err=500)   ! skip past header
50      continue
      mask_last_index = 1  ! last mask index searched
      Nmask = 0 
      do 100, i=1,5000
         read(unit_mask,*,err=500,end=200)   
     1       startyear_mask(i),  starttime_mask(i),
     1       stopyear_mask(i),  stoptime_mask(i)
c     1       (satflag(j),j=1,8)      ! start and stop times for selected interval  
         Nmask = Nmask+1     
c          print *,   startyear_mask(i),  starttime_mask(i),
c     1       stopyear_mask(i),  stoptime_mask(i),
c     1       (satflag(j),j=1,8)   
100     continue 
200     continue
c      print *, Nmask
      if (Nmask.ge.4999) 
     1     print *,'Warning Mask file too big!'
      close(unit=unit_mask, err=400) 
      return        !  successful
c
400      get_mask = .false.   ! on error go to here
      print *, 'Error opening or closing mask file:', 
     1     mask_file
      return
500      get_mask = .false.   ! on error go to here
      print *, 'Error reading mask file:', mask_file
      close(unit=unit_mask, err=400) 
      return
      end  ! end get_mask
c
c
c
c
c*******************************************************
c
       subroutine check_mask
c
c*******************************************************
c
c    checks if data packet is in selected time interval
c
c     11/27/95 by J. Dwyer
c         modified 9/4/96 by J. Dwyer, added mask_last_index to loop for efficiency
c         modified 11/18/96 by J. Dwyer, removed mask_last_index to loop 
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real  doy_start, doy_stop
      integer   startyr, stopyr,
     1     mo, dy, hr, mn, se, doy
      character*20   isotime   ! sampex_timecon isotime array
c         
      call sampex_timcon(start_time,
     1         startyr,mo,dy,hr,mn,se,doy,isotime)
      doy_start = doy+(3600.0*hr+60.0*mn+se)/86400.0
      call sampex_timcon(end_time,
     1         stopyr,mo,dy,hr,mn,se,doy,isotime)
      doy_stop = doy+(3600.0*hr+60.0*mn+se)/86400.0
c     
      mask_interval_flag = 0 
      mask_last_index = 0  
      do 100, i=mask_last_index,Nmask              
c          if (((doy_start.gt.stoptime_mask(i)).and.
c     1         (startyr.eq.stopyear_mask(i))).or.
c     1         (startyr.gt.stopyear_mask(i)))  then
c              mask_last_index = i  ! start loop past this value next time
c          endif
          if ((((doy_start.le.stoptime_mask(i)).and.
     1         (startyr.eq.stopyear_mask(i))).or.
     1         (startyr.lt.stopyear_mask(i))).and. 
     1         (((doy_stop.ge.starttime_mask(i)).and.
     1         (stopyr.eq.startyear_mask(i))).or.
     1         (stopyr.gt.startyear_mask(i)))) then
             mask_interval_flag = 1
             goto 200  ! exit loop
          end if
100     continue 
200     continue  ! exit loop here      
c
      return
      end   ! end check_mask
c
