c    Subroutines for the program sdf_lister
c    
c     Contents:
c             logical function open_output_abr
c
c             logical function write_output_abr
c
c		
c****************************************************
c
       logical function open_output_abr(unit_abr)
c
c****************************************************
c 
c     Open output data file and write the header for abridged average matrix rates data
c     returns .true. if successful, .false. otherwise
c
c     1/23/96 by J. Dwyer
c          modified 2/28/96 by J. Dwyer added headerlines output to write_header
c                         4/8/96 by J. Dwyer changed headerlines to 40
c                         4/24/96 changed energy labels
c                          2/13/97 by J. Dwyer  added new KP list
c                          2/14/97 by J. Dwyer  added write_kptitles
c
      include 'sdf_include.inc'   ! include type declarations
c     
      integer   unit_abr   ! file unit of output file    
      character*64     filename       ! complete name of output file
      character*4       file_prefix   ! prefix to be attached to output file name
      character*400   kpstring     !  string containing KP data names
      integer   headerlines
      integer   stringlength   ! length of KP string
c
      headerlines = 40
c    
      file_prefix = ' ABR'
      write(filename, '(a4,a60)') file_prefix, outputfile
      open(unit=unit_abr ,name=outputdir//filename,
     1     status='NEW',DISP='KEEP',RECL=32766, err=1000) ! open output data file
      print *, 'Opened output file: ', filename
c
      call write_header(unit_abr,file_prefix,headerlines)
c
      call write_kptitles(kpstring,stringlength)  !  get string containing KP data names
c
      write(unit_abr,200) kpstring(1:stringlength)
200  	format(' Time,',
     1         ' year,',
     1         ' day of the year,', 
     1         ' delta_t/2,',  
     1         ' saturation flag,', 
     1         ' VSE, sigma,',
     1         ' He E2, sigma,', 
     1         ' He E3, sigma,', 
     1         ' He E4, sigma,', 
     1         ' He E5, sigma,', 
     1         ' He E6, sigma,', 
     1         ' CNO E1, sigma,', 
     1         ' CNO E2, sigma,', 
     1         ' CNO E3, sigma,', 
     1         ' CNO E4, sigma,', 
     1         ' CNO E5, sigma,', 
     1         ' CNO E6, sigma,', 
     1         ' CNO E7, sigma,', 
     1         ' NeS E1, sigma,', 
     1         ' NeS E2, sigma,', 
     1         ' NeS E3, sigma,', 
     1         ' NeS E4, sigma,', 
     1         ' NeS E5, sigma,', 
     1         ' NeS E6, sigma,', 
     1         ' NeS E7, sigma,', 
     1         ' Fe E1, sigma,', 
     1         ' Fe E2, sigma,', 
     1         ' Fe E3, sigma,', 
     1         ' Fe E4, sigma,', 
     1         ' Fe E5, sigma,', 
     1         ' Fe E6, sigma,',
     1         ' J (sect1;tel1), sigma,',
     1         ' J (sect2;tel1), sigma,',
     1         ' J (sect3;tel1), sigma,',
     1         ' J (sect4;tel1), sigma,',
     1         ' J (sect5;tel1), sigma,',
     1         ' J (sect6;tel1), sigma,',
     1         ' J (sect7;tel1), sigma,',
     1         ' J(sect8;tel1), sigma,',
     1         ' J (sect1;tel2), sigma,',
     1         ' J (sect2;tel2), sigma,',
     1         ' J (sect3;tel2), sigma,',
     1         ' J (sect4;tel2), sigma,',
     1         ' J (sect5;tel2), sigma,',
     1         ' J (sect6;tel2), sigma,',
     1         ' J (sect7;tel2), sigma,',
     1         ' J (sect8;tel2), sigma,',
     1          a<stringlength>)
c    
      open_output_abr = .true.
      return  !  succussful
1000   open_output_abr = .false.  ! on error go to here
      print *, 'Error opening output abridged matrix rates file:', 
     1     filename
      return      
      end  ! end open_output_abr
c
c
c
c
c****************************************************
c
       logical function write_output_abr(unit_abr, nrow_abr,
     1             vse_ave, sigma_vse_ave, vse_ave_flag,
     1             flux_ave, sigma_flux_ave,flux_ave_flag, disc, 
     1             sigma_disc, disc_flag,
     1             J_sect_tel1, J_sect_tel1_sigma, 
     1             J_sect_tel1_flag,
     1             J_sect_tel2, J_sect_tel2_sigma, 
     1             J_sect_tel2_flag) 
c
c****************************************************
c
c      writes ABR data to output file with file unit=unit_abr
c      returns .false. if number of output lines written exceeds
c      specified value, .true. otherwise.
c
c     1/23/96 by J. Dwyer
c            modifications:
c            5/6/96 changed outputstring to 4096 characters
c            2/13/97 by J. Dwyer   added new KP list
c            2/14/97 by J. Dwyer added write_kpdata
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real   disc(4), sigma_disc(4)
      real   flux_ave(41), sigma_flux_ave(41)   ! arrays containing the fluxes and sigmas for all the elements 
      real   vse_ave, sigma_vse_ave
      real   J_sect_tel1(8), J_sect_tel1_sigma(8), 
     1        J_sect_tel2(8), J_sect_tel2_sigma(8)    
      real  doy_real
      real  half_delta_t  ! 1/2 of elapsed time interval (sec)
      integer   vse_ave_flag
      integer   flux_ave_flag(41)   ! .true. when flux is measured and is not saturated, .false. otherwise
      integer   disc_flag(4)   
      integer   J_sect_tel1_flag(8),
     1             J_sect_tel2_flag(8)
      integer    mid_time
      integer   unit_abr   ! output file unit
      integer   nrow_abr   !  number of rows written to output file
      integer      saturation_flag   ! 1 if any rate is saturated in mf, 0 otherwise
      integer   yr, mo, dy, hr, mn, se, doy
      integer    index     ! last index of outputstring written to 
      character*20   isotime   ! sampex_timecon isotime array
      character*4096   outputstring
c
      mid_time = (stop_time_avg+start_time_avg)/2
      half_delta_t = (stop_time_avg-start_time_avg)/2.0
      call sampex_timcon(mid_time,yr,mo,dy,hr,mn,se,doy,isotime)
      doy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0    
c     
      if (nrow_abr.lt.maximum_rows) then  
c
         saturation_flag = 0
         if (vse_ave_flag.eq.-1) saturation_flag = 1
         do 5 k=1,8                  
           if (J_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (J_sect_tel2_flag(k).eq.-1)
     1            saturation_flag = 1
5     continue 
      do 6 i=13,17
            if (flux_ave_flag(i).eq.-1) saturation_flag = 1
6     continue
      do 7 i=19,38
            if (flux_ave_flag(i).eq.-1) saturation_flag = 1
7     continue
c
         index = 1
         call write_line(index, outputstring, 
     1          vse_ave, (vse_ave_flag.eq.1), .true.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1          sigma_vse_ave, (vse_ave_flag.eq.1), .true.,
     1                dataformat_flag)
c              
         do 40 i=13,17
            call write_line(index, outputstring, 
     1     flux_ave(i), (flux_ave_flag(i).eq.1), .true.,
     1                dataformat_flag)
            call write_line(index, outputstring, 
     1     sigma_flux_ave(i), (flux_ave_flag(i).eq.1), .true.,
     1                dataformat_flag)           
40     continue  
         do 50 i=19,38
            call write_line(index, outputstring, 
     1     flux_ave(i), (flux_ave_flag(i).eq.1), .true.,
     1                dataformat_flag)
            call write_line(index, outputstring, 
     1     sigma_flux_ave(i), (flux_ave_flag(i).eq.1), .true.,
     1                dataformat_flag)           
50     continue 
c
         do 70 k=1,8
            call write_line(index, outputstring, 
     1         J_sect_tel1(k), (J_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        J_sect_tel1_sigma(k),
     1        (J_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
70     continue  
c
      do 80 k=1,8
            call write_line(index, outputstring, 
     1         J_sect_tel2(k), (J_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        J_sect_tel2_sigma(k),
     1        (J_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
80     continue       
c
      call write_kpdata(index, outputstring)
c
          write(unit_abr,100) mo, dy, yr, hr, mn, se,
     1   yr, doy_real,  
     1   half_delta_t, saturation_flag, 
     1   outputstring(1:(index-1))     
c   
100 	   format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1      ', 'i4', 'f12.8', 'f12.4', 'i1',',a<index-1>)
              nrow_abr = nrow_abr + 1       
      else
         print *, 'Number of rows exceeds maximum specified:'
     1            , maximum_rows
         write_output_abr = .false.
         return
      end if
c
      write_output_abr = .true.
      return
      end   ! end write_output_abr
c
