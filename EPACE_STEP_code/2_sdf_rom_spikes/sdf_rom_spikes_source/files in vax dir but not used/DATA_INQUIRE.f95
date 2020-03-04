c
      include  'time_lib.f95'     
c
c
c
c
c***************************************************

      program data_inquire

c***************************************************
c
c   searches for missing data files in a directory 
c     2/7/96 by J. Dwyer
c
c   Commonly used directories:
c      $scratch_step:[wortman]
c      $step_data:[dwyer.sdf]
c      $step_data:[dwyer.sdfkp]
c      UMSMS::WIND_KP_DEV:[spha]
c      UMSMS::WIND_KP_DEV:[mfi]
c      UMSMS::WIND_KP_DEV:[swe]
c      $step_data:[wind.kpdata.spha]
c      $step_data:[wind.kpdata.mfi]
c      $step_data:[wind.kpdata.swe]
c
c
      character*50   inputdir   ! name of input (SDF) directory     
      character*60   configfile   ! name of configuration file for data_inquire
      character*20   isotime    ! string containing time info     
      integer   day_of_year   ! function declaration
      integer   unit_cfg   ! file unit for config. file
      integer  startyear,startmon,startday   ! first SDF file year, month, day (e.g. 1995,02,26)
      integer  stopyear,stopmon,stopday   ! last SDF file year, month, day (e.g. 1995,02,27)
      integer  yyyy,mm,dd,ddd   !   year, month, day, day of year
      integer  hr,min,sec
      integer  filetype   ! file type: 0 = hex, 1 = sdf .w, 2 = sdf .v, 3 = spha, 4 = mfi, 5 = swe
      integer*4  thistime, stoptime  ! sampex times
c
      unit_cfg = 19   ! file unit of config file
      configfile = 'data_inquire.cfg'   ! name of configuration file to open
c        
      open(unit=unit_cfg,name=configfile, status='old',     
     1    action='read', disp='keep', err=400)  ! open the config file
100     format(a60)
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,100,err=500)     inputdir   
      print *,'input directory: ', inputdir   
      read(unit_cfg,*,err=500)    filetype
      read(unit_cfg,*,err=500)     ! read comment line
	    read(unit_cfg,*,err=500)     startyear,startmon,startday
      print *,  'start date: ',startyear,startmon,startday
      read(unit_cfg,*,err=500)     ! read comment line
	    read(unit_cfg,*,err=500)     stopyear,stopmon,stopday
      print *, 'stop date: ', stopyear,stopmon,stopday
      close(unit_cfg, err=400) 
c     
      ddd = day_of_year(stopyear,stopmon,stopday)  ! get the stop day of the year
      call convert_to_sampextime(stopyear,
     1  ddd,12,0,0,stoptime)   ! get sampex stop time
c
      ddd = day_of_year(startyear,startmon,startday)  ! get the start day of the year
      yyyy = startyear
      call convert_to_sampextime(yyyy,
     1  ddd,0,0,0,thistime)    ! get current sampex time from start year and day of year
      call sampex_timcon(thistime,yyyy,mm,
     1          dd,hr,min,sec,ddd,isotime)  ! get current date
c    
      do while (thistime.le.stoptime)  ! continue until stop time is reached        
c
         call do_inquire(inputdir, filetype,
     1                              yyyy,mm,dd,ddd)         
c
         ddd= ddd + 1  ! go to next day 
c  	   check for a year boundary crossing       
	       if (ddd.gt.idays_per_year(yyyy)) then
	            ddd = 1
	            yyyy = yyyy + 1
         end if
         call convert_to_sampextime(yyyy,
     1     ddd,0,0,0,thistime)  
         call sampex_timcon(thistime,yyyy,mm,
     1       dd,hr,mn,se,ddd,isotime)  ! get current date       
      end do
c    
      print *, 'data_inquire is finished'     
      stop
c    
400    print *, 'Error opening or closing configuration file:', 
     1     configfile     ! on error go to here
          write(30,*) 'Error opening or closing config file:', 
     1     configfile     ! on error go to here
500    print *, 'Error reading configuration file:', 
     1     configfile     ! on error go to here
          write(30,*) 'Error reading config file:', 
     1     configfile     ! on error go to here
      close(unit_cfg, err=400) 
c     
      end  ! end data_inquire
c
c
c
c
c***************************************************
c
      subroutine do_inquire(inputdir,filetype,
     1                                  yyyy,mm,dd,ddd)  
c
c***************************************************
c
c
      character*50   inputdir   ! name of input (SDF) directory    
      character*30    filename    ! name of file to be checked 
      character*4     stryear   ! string containing year,e.g. '1995'  
      integer  yyyy,mm,dd,ddd   !   year, month, day, day of year
      integer  filetype   ! file type: 0 = hex, 1 = sdf .u, 2 = sdf .v, 3 = spha, 4 = mfi, 5 = swe    
      logical      ifile, goodfile                ! test for inputfile inquire    
c      
      write(stryear,'(i4)')  yyyy  
c
      if (filetype.eq.0) then 
         write (filename,'(a4,a2,a1,i2.2,a1,i2.2,a4)') 
     1       'STEP', stryear(3:4),'_', mm,'_',dd, '.HEX'     ! get current SDF file name
c          print *, 'filename:', filename
          inquire(file=inputdir//filename,exist=ifile)  ! does file exist?
          if (.not.ifile) then     
             print *, '***** The file does not exist!  *****'      
             print *,  inputdir//filename
             print *,'year:',yyyy,'  DOY:',ddd,'  Mo:',mm,'  day:',dd    
          end if
      end if
c
      if (filetype.eq.1) then   
         write (filename,'(a3,a2,i3.3,a4)') 
     1       'SDF', stryear(3:4), ddd, '.w00'     ! get current SDF  file name
c          print *, 'filename: ', filename
          inquire(file=inputdir//filename,exist=ifile)  ! does file exist?
          if (.not.ifile) then               
             print *, '***** The file does not exist!  *****'      
             print *,  inputdir//filename
             print *,'year:',yyyy,'  DOY:',ddd,'  Mo:',mm,'  day:',dd        
          end if
      end if
c
      if (filetype.eq.2) then 
         write (filename,'(a3,a2,i3.3,a4)') 
     1       'SDF', stryear(3:4), ddd, '.v00'     ! get current SDF file name
c          print *, 'filename: ', filename
          inquire(file=inputdir//filename,exist=ifile)  ! does file exist?
          if (.not.ifile) then             
             print *, '***** The file does not exist!  *****'      
             print *,  inputdir//filename
             print *,'year:',yyyy,'  DOY:',ddd,'  Mo:',mm,'  day:',dd       
          end if
      end if
c
      if (filetype.eq.3) then   
         goodfile =  .false.       
         write (filename,'(a11,a4,i2.2,i2.2,a8)') 
     1       'WI_K0_SPHA_', stryear(1:4), mm, dd, '_V01.cdf'     ! get current file name
          do 10 i = 1,9
             write (filename(23:23),'(i1)') i 
c              print *, 'filename: ', filename
              inquire(file=inputdir//filename,exist=ifile)  ! does file exist?
              if (ifile) goodfile = .true.
10      continue
          if (.not.goodfile) then            
             print *, '***** The file does not exist!  *****'      
             print *,  inputdir//filename
             print *,'year:',yyyy,'  DOY:',ddd,'  Mo:',mm,'  day:',dd        
          end if
      end if
c
       if (filetype.eq.4) then   
         goodfile =  .false.       
         write (filename,'(a10,a4,i2.2,i2.2,a8)') 
     1       'WI_K0_MFI_', stryear(1:4), mm, dd, '_V01.cdf'     ! get current file name
          do 20 i = 1,9
             write (filename(22:22),'(i1)') i 
c              print *, 'filename: ', filename
              inquire(file=inputdir//filename,exist=ifile)  ! does file exist?
              if (ifile) goodfile = .true.
20      continue
          if (.not.goodfile) then            
             print *, '***** The file does not exist!  *****'      
             print *,  inputdir//filename
             print *,'year:',yyyy,'  DOY:',ddd,'  Mo:',mm,'  day:',dd        
          end if
      end if
c
      if (filetype.eq.5) then   
         goodfile =  .false.       
         write (filename,'(a10,a4,i2.2,i2.2,a8)') 
     1       'WI_K0_SWE_', stryear(1:4), mm, dd, '_V01.cdf'     ! get current file name
          do 30 i = 1,9
             write (filename(22:22),'(i1)') i 
c              print *, 'filename: ', filename
              inquire(file=inputdir//filename,exist=ifile)  ! does file exist?
              if (ifile) goodfile = .true.
30      continue
          if (.not.goodfile) then            
             print *, '***** The file does not exist!  *****'      
             print *,  inputdir//filename
             print *,'year:',yyyy,'  DOY:',ddd,'  Mo:',mm,'  day:',dd        
          end if
      end if
c
      return
      end  ! end do_inquire
c
