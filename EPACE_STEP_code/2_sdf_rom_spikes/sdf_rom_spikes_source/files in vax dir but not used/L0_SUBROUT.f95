c
c    Subroutines for the program hex_to_sdfkp
c    
c     Contents:
c             subroutine get_hex_packet
c
c             subroutine trim_PHA
c
c             subroutine get_outputfile
c
c             subroutine get_time
c
c             subroutine open_logfile
c
c             subroutine write_new_sdfkp
c
c            subroutine advance_day
c
c
c*****************************************************
c
      logical function get_hex_packet(packet_time)
c
c*****************************************************
c   
c     Reads data from ASCII hex file
c     return .true. if EOF reached, .false. otherwise
c
c     11/2/95 by J. Dwyer
c          modifications:
c          1/24/96 by J. Dwyer added logfile output
c
      include 'hex_include.inc'  ! define the variables read from the data file 
c      
      character*40 line            ! used to read in blank lines
      character*40 packet_time   ! start and stop time for major frame      
     
c
100      continue
      read(1,'(a40)',end=10000) line     
      read(1,'(a40)',end=10000) packet_time
      read(1,*,err=600,end=10000) 
     1   STEPHV,STEPtherm,STEPt1,STEPt2,STEPt3,MFnum
c
200    format (16z3)   ! hex format
250    format (13z3)
      do 300 i=1,45
        read(1,200,err=500,end=10000)
     1   (packet((i-1)*16+j), j=1,16)
300   continue
c  
      read(1,250,err=500,end=10000) 
     1    (packet((i-1)*16+j), j=1,13)
      read(1,*,err=500,end=10000) (STEP_KP_fluxes(j), j=1,6)
c          
c
      get_hex_packet = .false.  ! EOF not reached
      return  ! successful
c
500   print *, packet_time   ! if error come here
      print *,'error reading packet; assuming it was missing;'
      print *,'trying next packet'
      write(30,*) packet_time   ! if error come here
      write(30,*) 'error reading packet; assumed missing;'
      write(30,*) 'trying next packet'
      goto 100
c
600   print *, packet_time  ! if error come here
      print *,'error reading HK line; assuming bad data associated'
      print *,'with missing packet; skip line (fluxes) and try next'
      print *,'packet'
      write(30,*) packet_time  ! if error come here
      write(30,*) 'error reading HK line; assumed bad data in'
      write(30,*) 'with missing packet; skip line and try next'
      write(30,*) 'packet'
      read(1,'(a40)',end=10000) line
      goto 100
c
10000  get_hex_packet = .true.   ! reached end of file
      return
      end   ! end get_hex_packet
c
c
c
c
c*************************************************
c
        subroutine trim_PHA
c
c*************************************************
c
c   Used to write out only nonzero PHA data to the unformatted output file
c   PHA data is in packet(483-732). There are up to 50 events, each 5 bytes long.
c
c   10/30/95 by J. Dwyer
c
      include 'hex_include.inc'   ! include type declarations
      include 'l0_include.inc'   ! include type declarations
c      
c
      NPHA = 0     
c
      do 100 i=1,50
        if ((packet(483+(i-1)*5).eq.0).and.
     1      (packet(483+(i-1)*5+1).eq.0).and.
     1      (packet(483+(i-1)*5+2).eq.0).and.
     1      (packet(483+(i-1)*5+3).eq.0).and.
     1      (packet(483+(i-1)*5+4).eq.0)) then  ! if all the data in an event is zero, cut it 
        else
            PHA_index(NPHA+1) = i  ! index of valid PHA event
            NPHA = NPHA+1              
        end if 
100     continue
c
      return
      end  ! end trim_PHA
c
c
c
c
c*************************************************
c
        subroutine get_outputfile(packet_time, outputfile) 
c
c*************************************************
c
c      Gets the name of the output unformatted data file from the first start time
c      in the hex file.
c      ptime is a string containing the start and end time of the major frame
c      outputfile is a string containing the name of the output file
c      The format of the output file is :
c      sdfyyddd.u## where yy = year, ddd=day of the year, ## = version number.
c      e.g.  'sdf95273.w00'
c
c      10/6/95 by J. Dwyer
c          modifications:
c          2/12/97 by J. Dwyer added logfile output
c
c
        integer  yy, mm, dd, ddd, hh, day_of_year
        character*40   packet_time     ! packet_time  
        character*60   outputfile   ! output data file name
c
        read (packet_time(2:3),*) yy
        read (packet_time(5:6),*) mm
        read (packet_time(8:9),*) dd
        read (packet_time(11:12),*) hh
        if (hh .eq. 23) dd=dd+1
        if (dd .eq. 32 .and. mm .eq. 12) then
          yy=yy+1
          dd=1
          mm=1
        endif
        ddd = day_of_year(yy,mm,dd)
c
        write (outputfile,'(a3,i2.2,i3.3,a4)') 
     1    'SDF',yy,ddd,'.w00'
        print *,'SDF filename is:', outputfile
        write(30,*) 'SDF filename is:', outputfile
c        
        return
        end  ! end get_outputfile
c
c
c
c
c*************************************************
c
        subroutine get_time(packet_time)
c
c*************************************************
c    
c     packet_time is the input string, containing start and end dates and times for
c     each major frame.
c     start_time_string is a string containing the start date and time.
c     start_time = sampex time (seconds since midnight 1 January 1992) of start date
c                 and time
c     end_time = sampex time (seconds since midnight 1 January 1992) of finish date
c                 and time
c
c      10/6/95 by J. Dwyer
c
        include 'hex_include.inc'  ! define the variables read from the data file
c
        character*40  packet_time
        character*24  fktime    ! string containing mf end time
        integer syear,smonth,sday,shour,smin,sdoy
        integer fyear,fmonth,fday,fhour,fmin,fdoy
        integer   day_of_year
        integer*4   ssec, fsec, stime, ftime
c
        call convert_time(packet_time,start_time_string,fktime)
c
        read (start_time_string(1:4),*) syear
        read (start_time_string(6:7),*) smonth
        read (start_time_string(9:10),*) sday
        read (start_time_string(12:13),*) shour
        read (start_time_string(15:16),*) smin
        read (start_time_string(18:19),*) ssec
        sdoy =  day_of_year(syear,smonth,sday)
        call convert_to_sampextime (syear,sdoy,shour,
     1                   smin,ssec,start_time)
        read (fktime(1:4),*) fyear
        read (fktime(6:7),*) fmonth
        read (fktime(9:10),*) fday
        read (fktime(12:13),*) fhour
        read (fktime(15:16),*) fmin
        read (fktime(18:19),*) fsec
        fdoy =  day_of_year(fyear,fmonth,fday)
        call convert_to_sampextime (fyear,fdoy,fhour,
     1                   fmin,fsec,end_time)
c    
        return
        end  ! end get_time
c
c
c
c
c
c****************************************************
c
       subroutine open_logfile(inputdir,outputdir, 
     1                        startyear,startmon,startday,
     1                        stopyear,stopmon,stopday,
     1                        directoryflag)
c
c****************************************************
c 
c     Opens an output log file and write a header for the run
c
c     1/24/96 by J. Dwyer
c      
      byte  datew(9)       ! run date
      byte  timew(8)       ! run time  
      integer  startyear,startmon,startday   ! first sdf file year, month, day (e.g. 1995,02,26)
      integer  stopyear,stopmon,stopday   ! last sdf file year, month, day (e.g. 1995,02,27)  
      integer  directoryflag  ! 1 = kp directory is on $step_data; 0 = UMSMS::WIND_KP_DEV 
      character*60   inputdir   ! name of input (sdf) directory
      character*60   outputdir   ! name of output (sdf) directory
      character*80     line    ! used to write line in header 
      character*64     filename       ! complete name of output file     
c    
      
      filename = '$step_data:[dwyer.log]L0_to_sdfkp.log'
      open(unit=30 ,name=filename,
     1     status='NEW',DISP='KEEP',RECL=32766)   ! open output log file
c  
      line=' *** Log file generated by L0_to_sdfkp ***'
      call date(datew)	! get run time and date info
      call time(timew)
c
c	Write the contents of the config file to the output data file header
      write(30,100) line,
     1    datew,timew,
     1    inputdir,outputdir,
     1    directoryflag,
     1    startyear,startmon,startday,
     1    stopyear,stopmon,stopday 
c
100   format(a80,/,
     1  ' run time: '9a1,2x,8a1,/,
     1  ' input (hex) directory = 'a60,/,
     1  ' output (sdf) directory = 'a60,/,
     1  ' Input KP directory flag = 'i2,/,
     1  ' first hex file (year, month, day) = 'i4,2x,i2,2x,i2,/,
     1  ' last hex file (year, month, day) = 'i4,2x,i2,2x,i2,/,
     1  ' comments: ',/)
c
      return      
      end  ! end open_logfile
c
c
c
c
c****************************************************
c
      subroutine write_new_sdfkp(unit,id)
c
c****************************************************
c
c    write data to unformatted out put file
c
c    2/1197 by J. Dwyer
c
      include 'hex_include.inc'
      include 'l0_include.inc'
c
      integer unit, id
c
      write (unit) id
      if (id.eq.1) then     
c     ! write time info.
          write (unit) start_time_string, 
     1      start_time, end_time   
c
c     ! write housekeeping info.
          write(unit) STEPHV,STEPtherm, STEPt1, STEPt2, STEPt3, MFnum
c
c     ! write STEP data packet without PHA data 
          write(unit) (packet(j), j=1,482), packet(733)
c
c     ! write PHA data
         write(unit) NPHA
         if (NPHA.gt.0) then         
             write(unit)  ((packet(483+(PHA_index(i)-1)*5),
     1            packet(483+(PHA_index(i)-1)*5+1),
     1            packet(483+(PHA_index(i)-1)*5+2),
     1            packet(483+(PHA_index(i)-1)*5+3),
     1            packet(483+(PHA_index(i)-1)*5+4)), i=1,NPHA)
        end if
c
c     ! write STEP KP flux data
          write(unit) STEP_KP_fluxes
          goto 100
      end if
      if (id.eq.2) then
c         print *, id
c         print *, spha_asr
         write (unit) spha_asr
         goto 100
      end if
      if (id.eq.3) then
c         print *, id
c         print *, mfi_bgse,mfi_rms
c         print *, mfi_position
        write (unit) mfi_bgse,mfi_rms,
     1                   mfi_position
        goto 100
      end if
      if (id.eq.4) then
c         print *, id
c         print *, swe_vgse,swe_vth,
c     1                    swe_protdens
         write (unit) swe_vgse,swe_vth,
     1                    swe_protdens
         goto 100
      end if   
      if (id.eq.5) then
c         print *, id
c         print *,  tdp_eflux, tdp_ionflux
         write (unit)  tdp_eflux, tdp_ionflux
         goto 100
      end if 
c
      if (id.eq.6) then
c         print *, id
c         print *,  
c     1         epa_apeb2, epa_apeb3,
c     1         epa_apeb4, epa_apeb5,
c     1         epa_lemt1, epa_lemt2,
c     1         epa_lemt3
         write (unit)  epa_apeb2, epa_apeb3,
     1         epa_apeb4, epa_apeb5,
     1         epa_lemt1, epa_lemt2,
     1         epa_lemt3
         goto 100
      end if  
c
100     return
      end  ! end write_new_sdf
c
c
c
c
c
c****************************************************
c
      subroutine advance_day(isotime)
c
c****************************************************
c
c    calculates the date for the next day
c
c    2/11/97 by J. Dwyer
c
        character*20 isotime
	      Integer*4   thistime
        integer yyyy,mm,dd,hr,min,sec
        integer    ddd
        integer   day_of_year, idays_per_year
c
        read (isotime(1:4),*) yyyy
        read (isotime(6:7),*) mm
        read (isotime(9:10),*) dd
c 
        ddd = day_of_year(yyyy,mm,dd)
        ddd = ddd + 1  ! go to next day of year 
c  	  check for a year boundary crossing       
	      if (ddd.gt.idays_per_year(yyyy)) then
	         ddd = 1
	         yyyy = yyyy + 1
        end if
       call convert_to_sampextime(yyyy,
     1  ddd,12,0,0,thistime)    ! get current sampex time from start year and day of year
      call sampex_timcon(thistime,yyyy,mm,
     1          dd,hr,min,sec,ddd,isotime)  ! get current date from sampex time
c
c
      return
      end    ! end advance_day
c
