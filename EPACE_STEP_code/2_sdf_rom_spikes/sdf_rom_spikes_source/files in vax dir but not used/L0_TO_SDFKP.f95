c
      include  'L0_subrout.f95'
      include  'time_lib.f95'
      include  'open_cdf_file.f95'
      include  'read_cdf_data.f95'
c
c
c
c
c***************************************************

      program L0_to_sdfkp

c***************************************************
c
c      L0_to_sdfkp is a program for the STEP instrument. 
c      It converts kristin's hex dumps to a similar file of 
c      unformatted records that includes kp data.
c
c      This module loops through all the files between the first date and the last,
c      as specified by the first and last file names.  The main program for opening
c      and processing the hex and sdf files is the subroutine L0_main in this file.
c      Additional subroutines and functions used by this program are in the files
c          L0_subrout.f95
c          time_lib.f95
c     Type declarations are in the include file hex_include.inc
c
c    2/11/97 by J. Dwyer
c          modifications:
c             3/12/97 by J. Dwyer changed close to call cdf_close (mfi_id,status)
c
      character*60   inputdir   ! name of input (hex) directory
      character*60   outputdir   ! name of output (sdf) directory
      character*60   inputfile   ! name of hex (ASCII) file currently being read    
      character*60   configfile   ! name of configuration file for hex_to_unf
      character*20   isotime  ! string containing time info
      character*4     stryear   ! string containing year,e.g. '1995'
      integer   day_of_year   ! function declaration
      integer   unit_cfg   ! file unit for config. file
      integer  startyear,startmon,startday   ! first hex file year, month, day (e.g. 1995,02,26)
      integer  stopyear,stopmon,stopday   ! last hex file year, month, day (e.g. 1995,02,27)
      integer  yyyy,mm,dd,ddd   !  hex file year, month, day, day of year
      integer  hr,min,sec
      integer*4  thistime, stoptime  ! sampex times
      integer  directoryflag  ! 1 = kp directory is on $step_data; 0 = UMSMS::WIND_KP_DEV
c
      unit_cfg = 19   ! file unit of config file
      configfile = 'L0_to_sdfkp.cfg'  ! name of configuration file
c    
c    The input data directory is usually '$scratch_step:[wortman]'
c    Hex file names are of the form STEPyy_mm_dd.hex,
c    where yy = year, mm = month, dd = day, e.g. 'STEP95_02_27.hex'        
c
      open(unit=unit_cfg,name=configfile, status='old',     
     1    action='read', disp='keep', err=400)  ! open the config file
100     format(a60)
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,100,err=500)     inputdir   
      print *,'input directory: ', inputdir
      read(unit_cfg,*,err=500)     ! read comment line
      read(unit_cfg,100,err=500)    outputdir  
      print *, 'output directory: ',outputdir  
      read(unit_cfg,*,err=500)    directoryflag
      read(unit_cfg,*,err=500)     ! read comment line
	    read(unit_cfg,*,err=500)     startyear,startmon,startday
      print *,  'start date: ',startyear,startmon,startday
      read(unit_cfg,*,err=500)     ! read comment line
	    read(unit_cfg,*,err=500)     stopyear,stopmon,stopday
      print *, 'stop date: ', stopyear,stopmon,stopday
      close(unit_cfg, err=400) 
c      
      call open_logfile(inputdir,outputdir, 
     1                        startyear,startmon,startday,
     1                        stopyear,stopmon,stopday,
     1                        directoryflag)  ! open log file for output
c
      ddd = day_of_year(stopyear,stopmon,stopday)  ! get the stop day of the year
      call convert_to_sampextime(stopyear,
     1  ddd,12,0,0,stoptime)   ! get sampex stop time
c
      ddd = day_of_year(startyear,startmon,startday)  ! get the start day of the year
      yyyy = startyear
      call convert_to_sampextime(yyyy,
     1  ddd,0,0,0,thistime)   ! get current sampex time from start year and day of year
      call sampex_timcon(thistime,yyyy,mm,
     1          dd,hr,min,sec,ddd,isotime)  ! get current date
      write(stryear,'(i4)')  yyyy
      write (inputfile,'(a4,a2,a1,i2.2,a1,i2.2,a4)') 
     1    'STEP', stryear(3:4), '_', mm, '_', dd, '.hex'   ! get hex file name
c
      do while (thistime.le.stoptime)   ! continue until stop time is reached
         print *, 'input file: ',inputfile
         write(30,*)  'input file: ',inputfile
c
         call L0_main(inputdir,inputfile, outputdir)  ! process files       
c
         ddd= ddd + 1  ! go to next day 
c  	   check for a year boundary crossing       
	       if (ddd.gt.idays_per_year(yyyy)) then
	            ddd = 1
	            yyyy = yyyy + 1
         end if
         call convert_to_sampextime(yyyy,
     1     ddd,0,0,0,thistime)    ! get current sampex time
         call sampex_timcon(thistime,yyyy,mm,
     1       dd,hr,mn,se,ddd,isotime)  ! get current date
         write(stryear,'(i4)')  yyyy
         write (inputfile,'(a4,a2,a1,i2.2,a1,i2.2,a4)') 
     1    'STEP', stryear(3:4), '_', mm, '_', dd, '.hex'   ! get next hex file name
      end do
c    
      print *, 'L0_to_sdfkp finished'
      write(30,*)  'L0_to_sdfkp finished'
      close(30)
      stop
c    
400    print *, 'Error opening or closing configuration file:', 
     1     configfile     ! on error go to here
          write(30,*) 'Error opening or closing config file:', 
     1     configfile 
500    print *, 'Error reading configuration file:', 
     1     configfile     ! on error go to here
          write(30,*) 'Error reading config file:', 
     1     configfile
      close(unit_cfg, err=400) 
c     
      end  ! end L0_tosdfkp
c
c
c
c
c***************************************************

      subroutine L0_main(inputdir,inputfile, outputdir)

c***************************************************
c
c    Main body of program.  Loop through data packets
c
c    3/7/97 by J. Dwyer
c
      include 'hex_include.inc'   ! include type declarations
      include 'l0_include.inc'   ! include type declarations
c
      character*60   inputdir   ! name of input (hex) directory
      character*60   outputdir   ! name of output (sdf) directory
      character*60   inputfile   ! name of hex (ASCII) input data file 
      character*60   outputfile   ! name of unformatted output data file
      character*40  packet_time ! start and stop time for major frame  
      character*20  spha_isotime
      character*20  mfi_isotime
      character*20  swe_isotime
      character*20  tdp_isotime
      character*20  epa_isotime
      logical   get_hex_packet    ! function declaration    
      logical   end_flag         ! flag set at end of hex file 
      logical    spha_lastmf_flag, mfi_lastmf_flag,
     1              swe_lastmf_flag, tdp_lastmf_flag,
     1              epa_lastmf_flag       ! flag true if a previous measurements was good
      logical      ifile                ! test for inputfile inquire      
      integer    unit_in, unit_out
      integer    id
      integer    status
      integer shour   ! start time hour
      integer    advanced_spha,
     1              advanced_mfi,    
     1              advanced_swe,  
     1              advanced_3dp,  
     1              advanced_epa   ! keeps track of how many days have been advanced 
c   
      real  spha_asr_lastmf,
     1            mfi_bgse_lastmf(3),
     1            mfi_rms_lastmf,
     1            mfi_position_lastmf(3),
     1            swe_vgse_lastmf(3),
     1            swe_vth_lastmf,
     1            swe_protdens_lastmf,
     1            tdp_eflux_lastmf(7),
     1            tdp_ionflux_lastmf(7),
     1            epa_apeb1_lastmf,
     1            epa_apeb2_lastmf,
     1            epa_apeb3_lastmf,
     1            epa_apeb4_lastmf,
     1            epa_apeb5_lastmf,
     1            epa_lemt1_lastmf,
     1            epa_lemt2_lastmf,
     1            epa_lemt3_lastmf
      integer    Nspha, Nmfi, Nswe, N3dp, Nepa
c
c     	   
      unit_in = 1
      unit_out = 2    
      spha_lastmf_flag = .false.
      mfi_lastmf_flag = .false.
      swe_lastmf_flag = .false.
      tdp_lastmf_flag = .false.
      epa_lastmf_flag = .false.
      start_time_last = 0
      end_time_last = 0
      advanced_spha = 0
      advanced_mfi = 0    
      advanced_swe = 0  
      advanced_3dp = 0  
      advanced_epa = 0
c
      inquire(file=inputdir//inputfile,exist=ifile)  ! does file exist?
      if (.not.ifile) then                 ! if it exists, open it and read data
         print *, 'Input file', inputdir//inputfile,' does not exist!'
         write(30,*) 'Input file', inputdir//inputfile,
     1                     ' does not exist!'
         return
      end if
c
       open(unit=unit_in,file=inputdir//inputfile,
     1           status='old',action='read', err=1000)   ! open hex file
c     
      end_flag = get_hex_packet(packet_time)    ! read in hex data packet (= 1 major frame)
      call get_time(packet_time)      ! decode time and date      
      call get_outputfile(packet_time, outputfile)  ! get the output file name 
c
c      read (start_time_string(12:13),'(i2)') shour
c      do while (shour.eq.23)  ! first mf is sometimes from previous day. skip.
c        end_flag = get_hex_packet(packet_time)    ! read in hex data packet (= 1 major frame)
c        call get_time(packet_time)      ! decode time and date      
c        read (start_time_string(12:13),'(i2)') shour
c      end do     
c  
      spha_isotime = start_time_string   ! used to keep track of current file that is opened
      mfi_isotime = start_time_string
      swe_isotime = start_time_string
      tdp_isotime = start_time_string
      epa_isotime = start_time_string
c
      call open_spha_file(spha_isotime, 
     1                             directoryflag)    ! open KP data files (CDF files)
      call open_mfi_file(mfi_isotime, 
     1                             directoryflag) 
      call open_swe_file(swe_isotime, 
     1                             directoryflag) 
      call open_3dp_file(tdp_isotime, 
     1                             directoryflag) 
      call open_epa_file(epa_isotime, 
     1                             directoryflag)
c
      if (.not.spha_open) then   ! if didn't open, get next CDF file
         call advance_day(spha_isotime)  ! get time string for next day     
         advanced_spha = advanced_spha+1 
         call open_spha_file(spha_isotime,
     1                             directoryflag)   ! open new file                  
      endif
      if (.not.mfi_open) then   ! if didn't open, get next CDF file
         call advance_day(mfi_isotime)  ! get time string for next day      
         advanced_mfi = advanced_mfi+1
         call open_mfi_file(mfi_isotime,
     1                             directoryflag)   ! open new file                  
      endif
      if (.not.swe_open) then   ! if didn't open, get next CDF file
         call advance_day(swe_isotime)  ! get time string for next day   
         advanced_swe = advanced_swe+1   
         call open_swe_file(swe_isotime,
     1                             directoryflag)   ! open new file                  
      endif
      if (.not.tdp_open) then   ! if didn't open, get next CDF file
         call advance_day(tdp_isotime)  ! get time string for next day 
         advanced_3dp = advanced_3dp+1     
         call open_3dp_file(tdp_isotime,
     1                             directoryflag)   ! open new file                  
      endif
      if (.not.epa_open) then   ! if didn't open, get next CDF file
         call advance_day(epa_isotime)  ! get time string for next day  
         advanced_epa = advanced_epa+1    
         call open_epa_file(epa_isotime,
     1                             directoryflag)   ! open new file                  
      endif
      if (spha_open)  call  read_spha    ! read one record from KP data files (CDF files)
      if (mfi_open)  call  read_mfi
      if (swe_open)  call  read_swe
      if (tdp_open)  call  read_3dp
      if (epa_open)  call  read_epa
c          
      open(unit=unit_out,file=outputdir//outputfile,status='new',
     1    form='unformatted', err=2000)   ! open the unformatted output file
c  
      do while (.not.end_flag)  ! read data until the EOF is reached  
c
         if ((start_time_last.lt.start_time).and.
     1       (end_time_last.lt.end_time)) then
          id = 1      ! STEP id
          call trim_PHA(PHA_index)  ! select only PHA bytes with events
          call write_new_sdfkp(unit_out,id)   ! write output file
c
c   ************** get spha data  ****************  
c
          spha_asr = 0.0 
          Nspha = 0
          id = 2
          do while (spha_open.and.
     1         ((spha_time.le.end_time).or.(.not.spha_read)))         
            if (spha_rec.gt.spha_max_rec) then   ! get next CDF file
               call advance_day(spha_isotime)  ! get time string for next day
               advanced_spha = advanced_spha+1
               call cdf_close (spha_id,status)   ! close file
               print *, 'closed spha file, advancing date'
               write(30,*) 'closed spha file, advancing date'
               spha_open=.false.
               if ((spha_time.le.end_time).and. 
     1            (advanced_spha.lt.3))
     1            call open_spha_file(spha_isotime,
     1                             directoryflag)   ! open new file                  
            endif
            if (spha_read) then
               spha_asr_lastmf = spha_asr_1mf
               spha_lastmf_flag = .true.
            endif
            if ((spha_read).and.(spha_time.gt.start_time).and.
     1         (spha_time.le.end_time)) then
               spha_asr = spha_asr+spha_asr_1mf
               Nspha = Nspha+1
            end if
            if (spha_open)  call  read_spha
          end do
          if (Nspha.gt.0) then
             spha_asr = spha_asr/Nspha
             call write_new_sdfkp(unit_out,id) 
          else
             if ((spha_lastmf_flag).and.(spha_read)) then
                spha_asr = (spha_asr_1mf+spha_asr_lastmf)/2.0
                call write_new_sdfkp(unit_out,id) 
             end if
          end if
c
c
c   ************** get mfi data  **************** 
c
          mfi_bgse(1) = 0.0
          mfi_bgse(2) = 0.0
          mfi_bgse(3) = 0.0
          mfi_rms=0.0   
          mfi_position(1) = 0.0 
          mfi_position(2) = 0.0
          mfi_position(3) = 0.0
          Nmfi = 0
          id = 3
          do while (mfi_open.and.
     1     ((mfi_time.le.end_time).or.(.not.mfi_read))) 
            if (mfi_rec.gt.mfi_max_rec) then
               call advance_day(mfi_isotime)
               advanced_mfi = advanced_mfi+1
c               call CDF_lib (CLOSE_,CDF_, mfi_id)
               call cdf_close (mfi_id,status)
               print *, 'closed mfi file, advancing date'
               write(30,*)  'closed mfi file, advancing date'
               mfi_open=.false.
               if ((mfi_time.le.end_time).and.
     1            (advanced_mfi.lt.3))  
     1            call open_mfi_file(mfi_isotime,
     1                             directoryflag)     
            endif
            if (mfi_read) then
              mfi_bgse_lastmf(1) = mfi_bgse_1mf(1)
              mfi_bgse_lastmf(2) = mfi_bgse_1mf(2)
              mfi_bgse_lastmf(3) = mfi_bgse_1mf(3)
              mfi_rms_lastmf = mfi_rms_1mf
              mfi_position_lastmf(1) = mfi_position_1mf(1)
              mfi_position_lastmf(2) = mfi_position_1mf(2)
              mfi_position_lastmf(3) = mfi_position_1mf(3)
              mfi_lastmf_flag = .true.
            endif
            if ((mfi_read).and.(mfi_time.ge.start_time).and.
     1         (mfi_time.le.end_time)) then
               mfi_bgse(1)=mfi_bgse(1)+mfi_bgse_1mf(1)
               mfi_bgse(2)=mfi_bgse(2)+mfi_bgse_1mf(2)
               mfi_bgse(3)=mfi_bgse(3)+mfi_bgse_1mf(3)
               mfi_rms=mfi_rms+mfi_rms_1mf
               mfi_position(1)=mfi_position(1)+mfi_position_1mf(1)
               mfi_position(2)=mfi_position(2)+mfi_position_1mf(2)
               mfi_position(3)=mfi_position(3)+mfi_position_1mf(3)
               Nmfi = Nmfi+1
            end if
            if (mfi_open)  call  read_mfi
          end do
          if (Nmfi.gt.0) then
             mfi_bgse(1)=mfi_bgse(1)/Nmfi
             mfi_bgse(2)=mfi_bgse(2)/Nmfi
             mfi_bgse(3)=mfi_bgse(3)/Nmfi
             mfi_rms=mfi_rms/Nmfi
             mfi_position(1)=mfi_position(1)/Nmfi
             mfi_position(2)=mfi_position(2)/Nmfi
             mfi_position(3)=mfi_position(3)/Nmfi
             call write_new_sdfkp(unit_out,id) 
          else
             if ((mfi_lastmf_flag).and.(mfi_read)) then
                mfi_bgse(1)=(mfi_bgse_lastmf(1)+ 
     1                mfi_bgse_1mf(1))/2.0
                mfi_bgse(2)=(mfi_bgse_lastmf(2)+ 
     1                mfi_bgse_1mf(2))/2.0
                mfi_bgse(3)=(mfi_bgse_lastmf(3)+ 
     1                mfi_bgse_1mf(3))/2.0
                mfi_rms=(mfi_rms_lastmf+mfi_rms_1mf)/2.0
                mfi_position(1)=(mfi_position_lastmf(1)+
     1                                mfi_position_1mf(1))/2.0
                mfi_position(2)=(mfi_position_lastmf(2)+
     1                                mfi_position_1mf(2))/2.0
                mfi_position(3)=(mfi_position_lastmf(3)+
     1                                mfi_position_1mf(3))/2.0
                call write_new_sdfkp(unit_out,id)  
             end if
          end if
c
c
c   ************** get swe data  **************** 
c
          swe_vgse(1) = 0.0
          swe_vgse(2) = 0.0
          swe_vgse(3) = 0.0
          swe_vth = 0.0
          swe_protdens = 0.0  
          Nswe = 0
          id = 4
          do while (swe_open.and.
     1     ((swe_time.le.end_time).or.(.not.swe_read)))       
            if (swe_rec.gt.swe_max_rec) then
               call advance_day(swe_isotime)
               advanced_swe = advanced_swe+1 
               call cdf_close (swe_id,status)
               print *, 'closed swe file, advancing date'
               write(30,*) 'closed swe file, advancing date'
               swe_open=.false.
               if ((swe_time.le.end_time).and.
     1            (advanced_swe.lt.3))   
     1          call open_swe_file(swe_isotime,
     1                             directoryflag) 
            endif
            if (swe_read) then
              swe_vgse_lastmf(1) = swe_vgse_1mf(1)
              swe_vgse_lastmf(2) = swe_vgse_1mf(2)
              swe_vgse_lastmf(3) = swe_vgse_1mf(3)
              swe_vth_lastmf = swe_vth_1mf
              swe_protdens_lastmf = swe_protdens_1mf
              swe_lastmf_flag = .true.
            endif
            if ((swe_read).and.(swe_time.gt.start_time).and.
     1         (swe_time.le.end_time)) then
               swe_vgse(1)=swe_vgse(1)+swe_vgse_1mf(1)
               swe_vgse(2)=swe_vgse(2)+swe_vgse_1mf(2)
               swe_vgse(3)=swe_vgse(3)+swe_vgse_1mf(3)
               swe_vth = swe_vth+swe_vth_1mf
               swe_protdens=swe_protdens+swe_protdens_1mf
               Nswe = Nswe+1
            end if
            if (swe_open)  call  read_swe
          end do
          if (Nswe.gt.0) then
             swe_vgse(1)=swe_vgse(1)/Nswe
             swe_vgse(2)=swe_vgse(2)/Nswe
             swe_vgse(3)=swe_vgse(3)/Nswe
             swe_vth = swe_vth/Nswe
             swe_protdens=swe_protdens/Nswe
             call write_new_sdfkp(unit_out,id) 
          else
             if ((swe_lastmf_flag).and.(swe_read)) then
                swe_vgse(1)=(swe_vgse_lastmf(1)+
     1                                swe_vgse_1mf(1))/2.0
                swe_vgse(2)=(swe_vgse_lastmf(2)+
     1                                swe_vgse_1mf(2))/2.0
                swe_vgse(3)=(swe_vgse_lastmf(3)+
     1                                swe_vgse_1mf(3))/2.0
                swe_vth = (swe_vth_lastmf+swe_vth_1mf)/2.0
                swe_protdens=(swe_protdens_lastmf+
     1                                swe_protdens_1mf)/2.0
                call write_new_sdfkp(unit_out,id) 
             end if
          end if
c
c
c   ************** get 3dp data  **************** 
c
          do 100 i = 1,7
            tdp_eflux(i) = 0.0
            tdp_ionflux(i) = 0.0
100      continue
          N3dp = 0
          id = 5
          do while (tdp_open.and.
     1     ((tdp_time.le.end_time).or.(.not.tdp_read)))          
            if (tdp_rec.gt.tdp_max_rec) then
               call advance_day(tdp_isotime)
               advanced_3dp = advanced_3dp+1
               call cdf_close (tdp_id,status)
               print *, 'closed 3dp file, advancing date'
               write(30,*) 'closed 3dp file, advancing date'
               tdp_open=.false.
               if  ((tdp_time.le.end_time).and.
     1            (advanced_3dp.lt.3)) 
     1          call open_3dp_file(tdp_isotime,
     1                             directoryflag)
            endif
            if (tdp_read) then
              do 200 i = 1,7
                tdp_eflux_lastmf(i) = tdp_eflux_1mf(i)
                tdp_ionflux_lastmf(i) = tdp_ionflux_1mf(i)
200        continue
              tdp_lastmf_flag = .true.
            endif
            if ((tdp_read).and.(tdp_time.gt.start_time).and.
     1         (tdp_time.le.end_time)) then
              do 300 i = 1,7
                tdp_eflux(i)=tdp_eflux(i)+tdp_eflux_1mf(i)
                tdp_ionflux(i)=tdp_ionflux(i)+tdp_ionflux_1mf(i)
300      continue
               N3dp = N3dp+1
            end if
            if (tdp_open)  call  read_3dp
          end do
          if (N3dp.gt.0) then
              do 400 i = 1,7
                tdp_eflux(i)=tdp_eflux(i)/N3dp
                tdp_ionflux(i)=tdp_ionflux(i)/N3dp
400      continue
             call write_new_sdfkp(unit_out,id) 
          else
             if ((tdp_lastmf_flag).and.(tdp_read)) then
               do 500 i = 1,7
                 tdp_eflux(i)=(tdp_eflux_lastmf(i)+
     1                tdp_eflux_1mf(i))/2.0
                 tdp_ionflux(i)=(tdp_ionflux_lastmf(i)+
     1                tdp_ionflux_1mf(i))/2.0
500      continue
                call write_new_sdfkp(unit_out,id) 
             end if
          end if
c
c
c   ************** get epact data  **************** 
c
          epa_apeb1 = 0.0
          epa_apeb2 = 0.0
          epa_apeb3 = 0.0
          epa_apeb4 = 0.0
          epa_apeb5 = 0.0
          epa_lemt1 = 0.0
          epa_lemt2 = 0.0
          epa_lemt3 = 0.0
          Nepa = 0
          id = 6
          do while (epa_open.and.
     1     ((epa_time.le.end_time).or.(.not.epa_read)))       
            if (epa_rec.gt.epa_max_rec) then
               call advance_day(epa_isotime)
              advanced_epa = advanced_epa+1 
               call cdf_close (epa_id,status)
               print *, 'closed epact file, advancing date'
               write(30,*) 'closed epact file, advancing date'
               epa_open=.false.
               if ((epa_time.le.end_time).and.
     1            (advanced_epa.lt.3)) 
     1          call open_epa_file(epa_isotime,
     1                             directoryflag) 
            endif
            if (epa_read) then
              epa_apeb1_lastmf = epa_apeb1_1mf
              epa_apeb2_lastmf = epa_apeb2_1mf
              epa_apeb3_lastmf = epa_apeb3_1mf
              epa_apeb4_lastmf = epa_apeb4_1mf
              epa_apeb5_lastmf = epa_apeb5_1mf
              epa_lemt1_lastmf = epa_lemt1_1mf
              epa_lemt2_lastmf = epa_lemt2_1mf
              epa_lemt3_lastmf = epa_lemt3_1mf
              epa_lastmf_flag = .true.
            endif
            if ((epa_read).and.(epa_time.gt.start_time).and.
     1         (epa_time.le.end_time)) then
               epa_apeb1=epa_apeb1+epa_apeb1_1mf
               epa_apeb2=epa_apeb2+epa_apeb2_1mf
               epa_apeb3=epa_apeb3+epa_apeb3_1mf
               epa_apeb4=epa_apeb4+epa_apeb4_1mf
               epa_apeb5=epa_apeb5+epa_apeb5_1mf
               epa_lemt1=epa_lemt1+epa_lemt1_1mf
               epa_lemt2=epa_lemt2+epa_lemt2_1mf
               epa_lemt3=epa_lemt3+epa_lemt3_1mf
               Nepa = Nepa+1
            end if
            if (epa_open)  call  read_epa
          end do
          if (Nepa.gt.0) then
             epa_apeb1=epa_apeb1/Nepa
             epa_apeb2=epa_apeb2/Nepa
             epa_apeb3=epa_apeb3/Nepa
             epa_apeb4=epa_apeb4/Nepa
             epa_apeb5=epa_apeb5/Nepa
             epa_lemt1=epa_lemt1/Nepa
             epa_lemt2=epa_lemt2/Nepa
             epa_lemt3=epa_lemt3/Nepa
             call write_new_sdfkp(unit_out,id) 
          else
             if ((epa_lastmf_flag).and.(epa_read)) then
                epa_apeb1=(epa_apeb1_lastmf+
     1                           epa_apeb1_1mf)/2.0
                epa_apeb2=(epa_apeb2_lastmf+
     1                           epa_apeb2_1mf)/2.0
                epa_apeb3=(epa_apeb3_lastmf+
     1                           epa_apeb3_1mf)/2.0
                epa_apeb4=(epa_apeb4_lastmf+
     1                           epa_apeb4_1mf)/2.0
                epa_apeb5=(epa_apeb5_lastmf+
     1                           epa_apeb5_1mf)/2.0
                epa_lemt1=(epa_lemt1_lastmf+
     1                           epa_lemt1_1mf)/2.0
                epa_lemt2=(epa_lemt2_lastmf+
     1                           epa_lemt2_1mf)/2.0
                epa_lemt3=(epa_lemt3_lastmf+
     1                           epa_lemt3_1mf)/2.0
                call write_new_sdfkp(unit_out,id) 
             end if
          end if
c
c *************** end mf *****************
          id = -1
          call write_new_sdfkp(unit_out,id)    ! write end of mf id   
c      
         endif  ! check on start time advancing
         start_time_last = start_time
         end_time_last = end_time
c
         end_flag = get_hex_packet(packet_time) 
         call get_time(packet_time)      ! decode time and date       
c    
        end do  ! end loop that reads in packets      
c
      close (unit=unit_in,err=1000)   ! close input file
      close (unit=unit_out,err=2000)   ! close output file
      if (spha_open) then
        call cdf_close (spha_id,status)    ! close CDF files
        print *, 'closed spha file, leaving L0_main'
        write(30,*) 'closed spha file, leaving L0_main'
        spha_open = .false.
      endif
      if (mfi_open) then
c        call CDF_lib (CLOSE_,CDF_, mfi_id)
        call cdf_close (mfi_id,status)
        print *, 'closed mfi file, leaving L0_main'
        write(30,*)  'closed mfi file, leaving L0_main'
        mfi_open = .false.
      endif
      if (swe_open) then
        call cdf_close (swe_id,status)
        print *, 'closed swe file, leaving L0_main'
        write(30,*) 'closed swe file, leaving L0_main'
        swe_open = .false.
      endif
      if (tdp_open) then
        call cdf_close (tdp_id,status)    
        print *, 'closed 3dp file, leaving L0_main'
        write(30,*) 'closed 3dp file, leaving L0_main'
        tdp_open = .false.
      endif
      if (epa_open) then
        call cdf_close (epa_id,status)
        print *, 'closed epact file, leaving L0_main'
        write(30,*) 'closed epact file, leaving L0_main'
        epa_open = .false.
      endif

      return   ! successful
c
c    On errors come to here
1000  print *,'Error opening or closing Input file!',
     1            inputdir//inputfile
       write(30,*)  'Error opening or closing Input file!',
     1            inputdir//inputfile
      return
2000  print *,'Error opening or closing SDF!',
     1            outputdir//outputfile
       write(30,*) 'Error opening or closing SDF!',
     1            outputdir//outputfile
      return
c
      end  ! end main_L0
c

