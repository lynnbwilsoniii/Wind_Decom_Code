c    Subroutines for the program sdf_lister
c    
c     Contents:
c             logical function open_output_sect        
c
c             logical function write_output_sect
c
c		
c****************************************************
c
       logical function open_output_sect(unit_sect)
c
c****************************************************
c 
c     Open output data file and write the header for sectored matrix rates data
c     returns .true. if successful, .false. otherwise
c
c     12/1/95 by J. Dwyer
c          modified 2/28/96 by J. Dwyer added headerlines output to write_header
c                         4/8/96 by J. Dwyer changed headerlines to 40
c                         4/9/96 by J. Dwyer added year to output
c                         12/31/96 by J. Dwyer changed format to vse,E2,...,E6
c                         1/20/97 by J. Dwyer removed VSE sectors, added E1 tel 1 and 2,
c                                         removed anisotropy data, added VSE_AVE,VSEBAR and START
c                         2/6/97 by J. Dwyerincluded all E1 sectors
c                         2/7/97 by J. Dwyer added E7 - E10 for protons
c                          2/13/97 by J. Dwyer   added new KP list
c                          2/14/97 by J. Dwyer added write_kptitles
c
      include 'sdf_include.inc'   ! include type declarations
c
      integer   unit_sect   ! file unit of output file
      character*64     filename       ! complete name of output file
      character*4       file_prefix   ! prefix to be attached to output file name
      character*400   kpstring     !  string containing KP data names
      integer   headerlines
      integer   stringlength   ! length of KP string
c
      headerlines = 40
c    
      file_prefix = 'SECT'
      write(filename, '(a4,a60)') file_prefix, outputfile
      open(unit=unit_sect,name=outputdir//filename,
     1     status='NEW',DISP='KEEP',RECL=32766, err=1000) ! open output data file
      print *, 'Opened output file: ', filename
c
      call write_header(unit_sect,file_prefix,headerlines)
c
      call write_kptitles(kpstring,stringlength)  !  get string containing KP data names
c
      write(unit_sect,200) kpstring(1:stringlength)
200  	format(' Time,',
     1         ' year,',
     1         ' day of the year,', 
     1         ' delta_t/2,',
     1         ' saturation flag,',
     1         ' VSE, sigma,',
     1         ' VSEBAR, sigma,', 
     1         ' START, sigma,',
     1         ' E1 (sect1;tel1), sigma,',' E1 (sect2;tel1), sigma,',
     1         ' E1 (sect3;tel1), sigma,',' E1 (sect4;tel1), sigma,',
     1         ' E1 (sect5;tel1), sigma,',' E1 (sect6;tel1), sigma,',
     1         ' E1 (sect7;tel1), sigma,',' E1 (sect8;tel1), sigma,',
     1         ' E1 (sect1;tel2), sigma,',' E1 (sect2;tel2), sigma,',
     1         ' E1 (sect3;tel2), sigma,', ' E1 (sect4;tel2), sigma,',
     1         ' E1 (sect5;tel2), sigma,',' E1 (sect6;tel2), sigma,',
     1         ' E1 (sect7;tel2), sigma,',' E1 (sect8;tel2), sigma,',
     1         ' E2 (sect1;tel1), sigma,',' E2 (sect2;tel1), sigma,',
     1         ' E2 (sect3;tel1), sigma,',' E2 (sect4;tel1), sigma,',
     1         ' E2 (sect5;tel1), sigma,',' E2 (sect6;tel1), sigma,',
     1         ' E2 (sect7;tel1), sigma,',' E2 (sect8;tel1), sigma,',
     1         ' E2 (sect1;tel2), sigma,',' E2 (sect2;tel2), sigma,',
     1         ' E2 (sect3;tel2), sigma,',' E2 (sect4;tel2), sigma,',
     1         ' E2 (sect5;tel2), sigma,', ' E2 (sect6;tel2), sigma,',
     1         ' E2 (sect7;tel2), sigma,', ' E2 (sect8;tel2), sigma,',
     1         ' E3 (sect1;tel1), sigma,',' E3 (sect2;tel1), sigma,',
     1         ' E3 (sect3;tel1), sigma,',' E3 (sect4;tel1), sigma,',
     1         ' E3 (sect5;tel1), sigma,',' E3 (sect6;tel1), sigma,',
     1         ' E3 (sect7;tel1), sigma,',' E3 (sect8;tel1), sigma,',
     1         ' E3 (sect1;tel2), sigma,', ' E3 (sect2;tel2), sigma,',
     1         ' E3 (sect3;tel2), sigma,',' E3 (sect4;tel2), sigma,',
     1         ' E3 (sect5;tel2), sigma,', ' E3 (sect6;tel2), sigma,',
     1         ' E3 (sect7;tel2), sigma,',' E3 (sect8;tel2), sigma,',
     1         ' E4 (sect1;tel1), sigma,', ' E4 (sect2;tel1), sigma,',
     1         ' E4 (sect3;tel1), sigma,', ' E4 (sect4;tel1), sigma,',
     1         ' E4 (sect5;tel1), sigma,', ' E4 (sect6;tel1), sigma,',
     1         ' E4 (sect7;tel1), sigma,', ' E4 (sect8;tel1), sigma,',
     1         ' E4 (sect1;tel2), sigma,',' E4 (sect2;tel2), sigma,',
     1         ' E4 (sect3;tel2), sigma,', ' E4 (sect4;tel2), sigma,',
     1         ' E4 (sect5;tel2), sigma,', ' E4 (sect6;tel2), sigma,',
     1         ' E4 (sect7;tel2), sigma,', ' E4 (sect8;tel2), sigma,',   
     1         ' E5 (sect1;tel1), sigma,',' E5 (sect2;tel1), sigma,',
     1         ' E5 (sect3;tel1), sigma,', ' E5 (sect4;tel1), sigma,',
     1         ' E5 (sect5;tel1), sigma,', ' E5 (sect6;tel1), sigma,',
     1         ' E5 (sect7;tel1), sigma,', ' E5 (sect8;tel1), sigma,',
     1         ' E5 (sect1;tel2), sigma,', ' E5 (sect2;tel2), sigma,',
     1         ' E5 (sect3;tel2), sigma,', ' E5 (sect4;tel2), sigma,',
     1         ' E5 (sect5;tel2), sigma,', ' E5 (sect6;tel2), sigma,',
     1         ' E5 (sect7;tel2), sigma,', ' E5 (sect8;tel2), sigma,',  
     1         ' E6 (sect1;tel1), sigma,', ' E6 (sect2;tel1), sigma,',
     1         ' E6 (sect3;tel1), sigma,', ' E6 (sect4;tel1), sigma,',
     1         ' E6 (sect5;tel1), sigma,',' E6 (sect6;tel1), sigma,',
     1         ' E6 (sect7;tel1), sigma,', ' E6 (sect8;tel1), sigma,',
     1         ' E6 (sect1;tel2), sigma,', ' E6 (sect2;tel2), sigma,',
     1         ' E6 (sect3;tel2), sigma,',' E6 (sect4;tel2), sigma,',
     1         ' E6 (sect5;tel2), sigma,',' E6 (sect6;tel2), sigma,',
     1         ' E6 (sect7;tel2), sigma,', ' E6 (sect8;tel2), sigma,',   
     1         ' E7 (sect1;tel1), sigma,', ' E7 (sect2;tel1), sigma,',
     1         ' E7 (sect3;tel1), sigma,', ' E7 (sect4;tel1), sigma,',
     1         ' E7 (sect5;tel1), sigma,', ' E7 (sect6;tel1), sigma,',
     1         ' E7 (sect7;tel1), sigma,', ' E7 (sect8;tel1), sigma,',
     1         ' E7 (sect1;tel2), sigma,',' E7 (sect2;tel2), sigma,',
     1         ' E7 (sect3;tel2), sigma,', ' E7 (sect4;tel2), sigma,',
     1         ' E7 (sect5;tel2), sigma,', ' E7 (sect6;tel2), sigma,',
     1         ' E7 (sect7;tel2), sigma,', ' E7 (sect8;tel2), sigma,', 
     1         ' E8 (sect1;tel1), sigma,',' E8 (sect2;tel1), sigma,',
     1         ' E8 (sect3;tel1), sigma,', ' E8 (sect4;tel1), sigma,',
     1         ' E8 (sect5;tel1), sigma,', ' E8 (sect6;tel1), sigma,',
     1         ' E8 (sect7;tel1), sigma,', ' E8 (sect8;tel1), sigma,',
     1         ' E8 (sect1;tel2), sigma,', ' E8 (sect2;tel2), sigma,',
     1         ' E8 (sect3;tel2), sigma,', ' E8 (sect4;tel2), sigma,',
     1         ' E8 (sect5;tel2), sigma,', ' E8 (sect6;tel2), sigma,',
     1         ' E8 (sect7;tel2), sigma,', ' E8 (sect8;tel2), sigma,', 
     1         ' E9 (sect1;tel1), sigma,', ' E9 (sect2;tel1), sigma,',
     1         ' E9 (sect3;tel1), sigma,', ' E9 (sect4;tel1), sigma,',
     1         ' E9 (sect5;tel1), sigma,',' E9 (sect6;tel1), sigma,',
     1         ' E9 (sect7;tel1), sigma,', ' E9 (sect8;tel1), sigma,',
     1         ' E9 (sect1;tel2), sigma,', ' E9 (sect2;tel2), sigma,',
     1         ' E9 (sect3;tel2), sigma,', ' E9 (sect4;tel2), sigma,',
     1         ' E9 (sect5;tel2), sigma,', ' E9 (sect6;tel2), sigma,',
     1         ' E9 (sect7;tel2), sigma,', ' E9 (sect8;tel2), sigma,', 
     1         ' E10 (sect1;tel1), sigma,', ' E10 (sect2;tel1), sigma,',
     1         ' E10 (sect3;tel1), sigma,', ' E10 (sect4;tel1), sigma,',
     1         ' E10 (sect5;tel1), sigma,', ' E10 (sect6;tel1), sigma,',
     1         ' E10 (sect7;tel1), sigma,', ' E10 (sect8;tel1), sigma,',
     1         ' E10 (sect1;tel2), sigma,', ' E10 (sect2;tel2), sigma,',
     1         ' E10 (sect3;tel2), sigma,', ' E10 (sect4;tel2), sigma,',
     1         ' E10 (sect5;tel2), sigma,',' E10 (sect6;tel2), sigma,',
     1         ' E10 (sect7;tel2), sigma,', ' E10 (sect8;tel2), sigma,', 
     1           a)
c    
      open_output_sect = .true.
      return  !  succussful
1000   open_output_sect = .false.  ! on error go to here
      print *, 'Error opening output sectored matrix rates file:',
     1      filename
      return      
      end  ! end open_output_sect
c
c
c

c
c
c
c
c****************************************************
c
       logical function write_output_sect
     1       (unit_sect, nrow_sect,
     1        vse_ave, sigma_vse_ave, vse_ave_flag,
     1        disc, sigma_disc, disc_flag,
     1        E1_sect_tel1, E1_sect_tel1_sigma, 
     1        E1_sect_tel1_flag,
     1        E1_sect_tel2, E1_sect_tel2_sigma, 
     1        E1_sect_tel2_flag,
     1        E2_sect_tel1, E2_sect_tel1_sigma, 
     1        E2_sect_tel1_flag,
     1        E2_sect_tel2, E2_sect_tel2_sigma, 
     1        E2_sect_tel2_flag,
     1        E3_sect_tel1, E3_sect_tel1_sigma, 
     1        E3_sect_tel1_flag,
     1        E3_sect_tel2, E3_sect_tel2_sigma, 
     1        E3_sect_tel2_flag,
     1        E4_sect_tel1, E4_sect_tel1_sigma, 
     1        E4_sect_tel1_flag,
     1        E4_sect_tel2, E4_sect_tel2_sigma,
     1        E4_sect_tel2_flag,
     1        E5_sect_tel1, E5_sect_tel1_sigma, 
     1        E5_sect_tel1_flag,
     1        E5_sect_tel2, E5_sect_tel2_sigma, 
     1        E5_sect_tel2_flag,
     1        E6_sect_tel1, E6_sect_tel1_sigma, 
     1        E6_sect_tel1_flag,
     1        E6_sect_tel2, E6_sect_tel2_sigma, 
     1        E6_sect_tel2_flag,
     1        E7_sect_tel1, E7_sect_tel1_sigma, 
     1        E7_sect_tel1_flag,
     1        E7_sect_tel2, E7_sect_tel2_sigma, 
     1        E7_sect_tel2_flag,
     1        E8_sect_tel1, E8_sect_tel1_sigma, 
     1        E8_sect_tel1_flag,
     1        E8_sect_tel2, E8_sect_tel2_sigma,
     1        E8_sect_tel2_flag,
     1        E9_sect_tel1, E9_sect_tel1_sigma, 
     1        E9_sect_tel1_flag,
     1        E9_sect_tel2, E9_sect_tel2_sigma, 
     1        E9_sect_tel2_flag,
     1        E10_sect_tel1, E10_sect_tel1_sigma, 
     1        E10_sect_tel1_flag,
     1        E10_sect_tel2, E10_sect_tel2_sigma, 
     1        E10_sect_tel2_flag) 
c
c****************************************************
c
c      writes SECT data to output file with file unit=unit_sect
c      returns .false. if number of output lines written exceeds
c      specified value, .true. otherwise.
c
c     11/27/95 by J. Dwyer
c                 modified     4/9/96 by J. Dwyer added year to output
c            5/6/96 changed outputstring to 4096 characters
c            12/31/96 by J. Dwyer changed format to vse,E2,...,E6
c            1/15/97 by J. Dwyer fixed bug with E4_sect_tel2_sigma
c            1/20/97 by J. Dwyer removed VSE sectors, added E1 tel 1 and 2,
c                               removed anisotropy data, added VSE_AVE, VSEBAR and START.
c            2/6/97 by J. Dwyerincluded all E1 sectors
c            2/7/97 by J. Dwyer added E7 - E10 for protons
c            2/13/97 by J. Dwyer   added new KP list
c            2/14/97 by J. Dwyer  added write_kpdata
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real     E1_sect_tel1(8), E1_sect_tel1_sigma(8), 
     1        E1_sect_tel2(8), E1_sect_tel2_sigma(8),
     1        E2_sect_tel1(8), E2_sect_tel1_sigma(8), 
     1        E2_sect_tel2(8), E2_sect_tel2_sigma(8), 
     1        E3_sect_tel1(8), E3_sect_tel1_sigma(8), 
     1        E3_sect_tel2(8), E3_sect_tel2_sigma(8), 
     1        E4_sect_tel1(8), E4_sect_tel1_sigma(8), 
     1        E4_sect_tel2(8), E4_sect_tel2_sigma(8),
     1        E5_sect_tel1(8), E5_sect_tel1_sigma(8), 
     1        E5_sect_tel2(8), E5_sect_tel2_sigma(8),
     1        E6_sect_tel1(8), E6_sect_tel1_sigma(8), 
     1        E6_sect_tel2(8), E6_sect_tel2_sigma(8),
     1        E7_sect_tel1(8), E7_sect_tel1_sigma(8), 
     1        E7_sect_tel2(8), E7_sect_tel2_sigma(8), 
     1        E8_sect_tel1(8), E8_sect_tel1_sigma(8), 
     1        E8_sect_tel2(8), E8_sect_tel2_sigma(8),
     1        E9_sect_tel1(8), E9_sect_tel1_sigma(8), 
     1        E9_sect_tel2(8), E9_sect_tel2_sigma(8), 
     1        E10_sect_tel1(8), E10_sect_tel1_sigma(8), 
     1        E10_sect_tel2(8), E10_sect_tel2_sigma(8)
      real   vse_ave, sigma_vse_ave
      real   disc(4), sigma_disc(4)
      real  doy_real
      real  half_delta_t  ! 1/2 of elapsed time interval (sec)
      integer      E1_sect_tel1_flag(8),
     1             E1_sect_tel2_flag(8),
     1             E2_sect_tel1_flag(8),
     1             E2_sect_tel2_flag(8),
     1             E3_sect_tel1_flag(8),
     1             E3_sect_tel2_flag(8),
     1             E4_sect_tel1_flag(8),
     1             E4_sect_tel2_flag(8),
     1             E5_sect_tel1_flag(8),
     1             E5_sect_tel2_flag(8),
     1             E6_sect_tel1_flag(8),
     1             E6_sect_tel2_flag(8),
     1             E7_sect_tel1_flag(8),
     1             E7_sect_tel2_flag(8),
     1             E8_sect_tel1_flag(8),
     1             E8_sect_tel2_flag(8),
     1             E9_sect_tel1_flag(8),
     1             E9_sect_tel2_flag(8),
     1             E10_sect_tel1_flag(8),
     1             E10_sect_tel2_flag(8)
      integer   vse_ave_flag
      integer   disc_flag(4)
      integer   mid_time
      integer   unit_sect   ! output file unit
      integer    nrow_sect   !  number of rows written to output file
      integer    saturation_flag   ! 1 if any rate is saturated in mf, 0 otherwise
      integer    yr, mo, dy, hr, mn, se, doy     
      integer    index     ! last index of outputstring written to 
      character*20   isotime   ! sampex_timecon isotime array
      character*4096   outputstring
c
      mid_time = (stop_time_avg+start_time_avg)/2
      half_delta_t = (stop_time_avg-start_time_avg)/2.0
      call sampex_timcon(mid_time,yr,mo,dy,hr,mn,se,doy,isotime)
      doy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0 
c     
      if (nrow_sect.lt.maximum_rows) then  
c     
       saturation_flag = 0   
       if (vse_ave_flag.eq.-1) saturation_flag = 1
       if (disc_flag(1).eq.-1) saturation_flag = 1   ! VSEBAR
       if (disc_flag(2).eq.-1) saturation_flag = 1   ! START
c      
       do 5 k=1,8       
           if (E1_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E1_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E2_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E2_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E3_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E3_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E4_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E4_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E5_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E5_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E6_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E6_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E7_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E7_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E8_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E8_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E9_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E9_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E10_sect_tel1_flag(k).eq.-1) 
     1            saturation_flag = 1
           if (E10_sect_tel2_flag(k).eq.-1) 
     1            saturation_flag = 1
5     continue   
c        
         index = 1
c
         call write_line(index, outputstring, 
     1          vse_ave, (vse_ave_flag.eq.1), .true.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1          sigma_vse_ave, (vse_ave_flag.eq.1), .true.,
     1                dataformat_flag)
c         
            call write_line(index, outputstring,    
     1          disc(1), (disc_flag(1).eq.1), .true.,
     1                dataformat_flag)
            call write_line(index, outputstring, 
     1          sigma_disc(1), (disc_flag(1).eq.1), .true.,
     1                dataformat_flag)  ! VSEBAR
c
            call write_line(index, outputstring,  
     1          disc(2), (disc_flag(2).eq.1), .true.,
     1                dataformat_flag)
            call write_line(index, outputstring, 
     1          sigma_disc(2), (disc_flag(2).eq.1), .true.,
     1                dataformat_flag)   ! START
c
      do 10 k=1,8
            call write_line(index, outputstring, 
     1         E1_sect_tel1(k), (E1_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E1_sect_tel1_sigma(k),
     1        (E1_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
10     continue  
c
      do 20 k=1,8
            call write_line(index, outputstring, 
     1         E1_sect_tel2(k), (E1_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E1_sect_tel2_sigma(k),
     1        (E1_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
20     continue        
c
      do 70 k=1,8            
            call write_line(index, outputstring, 
     1         E2_sect_tel1(k), (E2_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E2_sect_tel1_sigma(k),
     1        (E2_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
70     continue  
c
      do 80 k=1,8
            call write_line(index, outputstring, 
     1         E2_sect_tel2(k), (E2_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E2_sect_tel2_sigma(k),
     1        (E2_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
80     continue         

         do 90 k=1,8
            call write_line(index, outputstring, 
     1         E3_sect_tel1(k), (E3_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E3_sect_tel1_sigma(k),
     1        (E3_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
90     continue  
c
      do 100 k=1,8
             call write_line(index, outputstring, 
     1         E3_sect_tel2(k), (E3_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E3_sect_tel2_sigma(k),
     1        (E3_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
100     continue   

         do 110 k=1,8
            call write_line(index, outputstring, 
     1         E4_sect_tel1(k), (E4_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E4_sect_tel1_sigma(k),
     1        (E4_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag)  
110     continue  

      do 120 k=1,8
            call write_line(index, outputstring, 
     1         E4_sect_tel2(k), (E4_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E4_sect_tel2_sigma(k),
     1        (E4_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
120     continue   
c
      do 130 k=1,8
            call write_line(index, outputstring, 
     1         E5_sect_tel1(k), (E5_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E5_sect_tel1_sigma(k),
     1        (E5_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
130     continue  
c
      do 140 k=1,8
            call write_line(index, outputstring, 
     1         E5_sect_tel2(k), (E5_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E5_sect_tel2_sigma(k),
     1        (E5_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
140     continue  
c
      do 150 k=1,8
            call write_line(index, outputstring, 
     1         E6_sect_tel1(k), (E6_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E6_sect_tel1_sigma(k),
     1        (E6_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
150     continue  
c
      do 160 k=1,8
            call write_line(index, outputstring, 
     1         E6_sect_tel2(k), (E6_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E6_sect_tel2_sigma(k),
     1        (E6_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
160     continue 
c
      do 170 k=1,8
            call write_line(index, outputstring, 
     1         E7_sect_tel1(k), (E7_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E7_sect_tel1_sigma(k),
     1        (E7_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
170     continue  
c
      do 180 k=1,8
            call write_line(index, outputstring, 
     1         E7_sect_tel2(k), (E7_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E7_sect_tel2_sigma(k),
     1        (E7_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
180     continue 
c
      do 190 k=1,8
            call write_line(index, outputstring, 
     1         E8_sect_tel1(k), (E8_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E8_sect_tel1_sigma(k),
     1        (E8_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
190     continue  
c
      do 200 k=1,8
            call write_line(index, outputstring, 
     1         E8_sect_tel2(k), (E8_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E8_sect_tel2_sigma(k),
     1        (E8_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
200     continue 
c
      do 210 k=1,8
            call write_line(index, outputstring, 
     1         E9_sect_tel1(k), (E9_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E9_sect_tel1_sigma(k),
     1        (E9_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
210     continue  
c
      do 220 k=1,8
            call write_line(index, outputstring, 
     1         E9_sect_tel2(k), (E9_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E9_sect_tel2_sigma(k),
     1        (E9_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
220     continue 
c
      do 230 k=1,8
            call write_line(index, outputstring, 
     1         E10_sect_tel1(k), (E10_sect_tel1_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E10_sect_tel1_sigma(k),
     1        (E10_sect_tel1_flag(k).eq.1), .true.,
     1                dataformat_flag) 
230     continue  
c
      do 240 k=1,8
            call write_line(index, outputstring, 
     1         E10_sect_tel2(k), (E10_sect_tel2_flag(k).eq.1),
     1         .true., dataformat_flag)
            call write_line(index, outputstring, 
     1        E10_sect_tel2_sigma(k),
     1        (E10_sect_tel2_flag(k).eq.1), .true.,
     1                dataformat_flag) 
240     continue 
c
      call write_kpdata(index, outputstring)
c    
         write(unit_sect,300) mo, dy, yr, hr, mn, se, 
     1   yr, doy_real,
     1   half_delta_t, saturation_flag,   
     1   outputstring(1:(index-1))     
c   
300 	   format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1       ', 'i4', 'f12.8', 'f12.4', 'i1',',a<index-1>)
              nrow_sect = nrow_sect + 1       
      else
         print *, 'Number of rows exceeds maximum specified:'
     1            , maximum_rows
         write_output_sect = .false.
         return
      end if
c
      write_output_sect = .true.
      return
      end   ! end write_output_sect
c
