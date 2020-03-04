c    Subroutines for the program sdf_lister
c    
c     Contents:
c             logical function open_output_omni
c
c             subroutine assign_fluxes_omni
c
c             logical function write_output_omni
c
c		
c****************************************************
c
       logical function open_output_omni(unit_omni)
c
c****************************************************
c 
c     Open output data file and write the header for omni matrix rates data
c     returns .true. if successful, .false. otherwise
c
c     12/1/95 by J. Dwyer
c          modified 2/28/96 by J. Dwyer added headerlines output to write_header
c                         4/8/96 by J. Dwyer changed headerlines to 40
c                         4/9/96 by J. Dwyer added year to output
c                         4/24/96 changed energy labels
c                         2/13/97 by J. Dwyer   added new KP list, removed ani data
c                          2/14/97 by J. Dwyer   removed junk, added write_kptitles
c
      include 'sdf_include.inc'   ! include type declarations
c   
      integer   unit_omni   ! file unit of output file    
      character*64     filename       ! complete name of output file
      character*4       file_prefix   ! prefix to be attached to output file name
      character*400   kpstring     !  string containing KP data names
      integer   headerlines
      integer   stringlength   ! length of KP string
c
      headerlines = 40
c    
      file_prefix = 'OMNI'
      write(filename, '(a4,a60)') file_prefix, outputfile
      open(unit=unit_omni ,name=outputdir//filename,
     1     status='NEW',DISP='KEEP',RECL=32766, err=1000) ! open output data file
      print *, 'Opened output file: ', filename
c
      call write_header(unit_omni,file_prefix, headerlines)
c
      call write_kptitles(kpstring,stringlength)  !  get string containing KP data names
c
      write(unit_omni,200) kpstring(1:stringlength)
200  	format(' Time,', 
     1         ' year,',
     1         ' day of the year,', 
     1         ' delta_t/2,',
     1         ' saturation flag,',   
     1         ' VSE, sigma,',
     1         ' VSEBAR,sigma,', 
     1         ' START,sigma,',
     1         ' STOP, sigma, D, sigma,',
     1         ' STATE, sigma,',   
     1         ' H E1, sigma,', 
     1         ' H E2, sigma,', 
     1         ' H E3, sigma,', 
     1         ' H E4, sigma,', 
     1         ' H E5, sigma,', 
     1         ' H E6, sigma,', 
     1         ' H E7, sigma,', 
     1         ' H E8, sigma,', 
     1         ' H E9, sigma,', 
     1         ' H E10, sigma,', 
     1         ' He E1, sigma,', 
     1         ' He E2, sigma,', 
     1         ' He E3, sigma,', 
     1         ' He E4, sigma,', 
     1         ' He E5, sigma,', 
     1         ' He E6, sigma,', 
     1         ' He E7, sigma,', 
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
c     1         ' Junk-1, sigma, Junk-2, sigma, Junk-3, sigma,',
     1          a<stringlength>)
c    
      open_output_omni = .true.
      return  !  succussful
1000   open_output_omni = .false.  ! on error go to here
      print *, 'Error opening output averege matrix rates file:', 
     1     filename
      return      
      end  ! end open_output_omni
c
c
c
c
c****************************************************
c
       logical function write_output_omni(unit_omni, nrow_omni,
     1             vse_ave, sigma_vse_ave, vse_ave_flag,
     1             flux_ave, sigma_flux_ave,flux_ave_flag, disc, 
     1             sigma_disc, disc_flag) 
c
c****************************************************
c
c      writes OMNI data to output file with file unit=unit_out
c      returns .false. if number of output lines written exceeds
c      specified value, .true. otherwise.
c
c     11/27/95 by J. Dwyer
c         modified 2/26/96 by J. Dwyer corrected 2x and 4x blanl spaces to 10
c                         4/9/96 by J. Dwyer added year to output
c                          5/6/96 changed outputstring to 4096 characters
c                          2/13/97 by J. Dwyer   added new KP list, removed ani data
c                          2/14/97 by J. Dwyer   removed junk, added write_kpdata
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real   disc(4), sigma_disc(4)
      real   flux_ave(41), sigma_flux_ave(41)   ! arrays containing the fluxes and sigmas for all the elements 
      real   vse_ave, sigma_vse_ave     
      real  doy_real_1x
      real  doy_real_2x
      real  doy_real_4x
      real  half_delta_t_1x  ! 1/2 of elapsed time interval (sec)
      real  half_delta_t_2x  ! 1/2 of elapsed time interval (sec)
      real  half_delta_t_4x  ! 1/2 of elapsed time interval (sec)      
      integer    mid_time_1x
      integer    mid_time_2x
      integer    mid_time_4x
      integer   yr_1x, mo_1x, dy_1x, hr_1x,
     1             mn_1x, se_1x, doy_1x
      integer   yr_2x, mo_2x, dy_2x, hr_2x,
     1             mn_2x, se_2x, doy_2x
      integer   yr_4x, mo_4x, dy_4x, hr_4x,
     1             mn_4x, se_4x, doy_4x
      integer   vse_ave_flag
      integer   flux_ave_flag(41)   ! .true. when flux is measured and is not saturated, .false. otherwise
      integer   disc_flag(4)
      integer   unit_omni   ! output file unit
      integer   nrow_omni   !  number of rows written to output file
      integer      saturation_flag   ! 1 if any rate is saturated in mf, 0 otherwise      
      integer    index     ! last index of outputstring written to 
      character*20   isotime   ! sampex_timecon isotime array
      character*4096   outputstring
c
      mid_time_1x = (stop_time_avg+start_time_avg)/2
      half_delta_t_1x = (stop_time_avg-start_time_avg)/2.0
      call sampex_timcon(mid_time_1x,yr_1x,mo_1x,dy_1x,
     1                             hr_1x,mn_1x,se_1x,doy_1x,isotime)
      doy_real_1x = doy_1x+
     1                (3600.0*hr_1x+60.0*mn_1x+se_1x)/86400.0
c
      mid_time_2x = (stop_time_avg+start_time_avg-
     1                     spin_period*ave_spins(mf_type))/2
      half_delta_t_2x = (stop_time_avg-start_time_avg+
     1                    spin_period*ave_spins(mf_type))/2.0
      call sampex_timcon(mid_time_2x,yr_2x,mo_2x,dy_2x,
     1                             hr_2x,mn_2x,se_2x,doy_2x,isotime)
      doy_real_2x = doy_2x+
     1                (3600.0*hr_2x+60.0*mn_2x+se_2x)/86400.0   
c
      mid_time_4x = (stop_time_avg+start_time_avg-
     1                     3.0*spin_period*ave_spins(mf_type))/2
      half_delta_t_4x = (stop_time_avg-start_time_avg+
     1                    3.0*spin_period*ave_spins(mf_type))/2.0
      call sampex_timcon(mid_time_4x,yr_4x,mo_4x,dy_4x,
     1                             hr_4x,mn_4x,se_4x,doy_4x,isotime)
      doy_real_4x = doy_4x+
     1                (3600.0*hr_4x+60.0*mn_4x+se_4x)/86400.0
c     
      if (nrow_omni.lt.(maximum_rows-3)) then  
c
c                4 x matrices
c           *****************
         saturation_flag = 0          
         do 20 i=1,38
            if ((flux_ave_flag(i).eq.-1).and.
     1          (mdata_x(i).eq.4)) saturation_flag = 1
20     continue
c
         index=1
         do 30 i=1,10    
               call write_line(index, outputstring, 
     1           0.0, .false., .false.,
     1                dataformat_flag)  ! fill in blank spaces                     
30     continue  
         do 40 i=1,38
               call write_line(index, outputstring, 
     1        flux_ave(i), ((flux_ave_flag(i).eq.1).and.
     1                (mdata_x(i).eq.4)), .true.,
     1                dataformat_flag)
               call write_line(index, outputstring, 
     1        sigma_flux_ave(i),((flux_ave_flag(i).eq.1).and.
     1                (mdata_x(i).eq.4)), .true.,
     1                dataformat_flag)           
40     continue 
          do 60 i=1,15
               call write_line(index, outputstring, 
     1           0.0, .false., .false.,
     1                dataformat_flag)    ! fill in blank spaces                  
60     continue 
c
         write(unit_omni,1000) mo_4x, dy_4x, yr_4x,
     1         hr_4x, mn_4x, se_4x,
     1         yr_4x,doy_real_4x,  
     1         half_delta_t_4x, saturation_flag, 
     1         outputstring(1:(index-1))     
c   
c                2 x matrices
c           *****************
         saturation_flag = 0          
         do 120 i=1,38
            if ((flux_ave_flag(i).eq.-1).and.
     1          (mdata_x(i).eq.2)) saturation_flag = 1
120     continue
c
         index=1
         do 130 i=1,10    
               call write_line(index, outputstring, 
     1           0.0, .false., .false.,
     1                dataformat_flag)  ! fill in blank spaces                     
130     continue  
         do 140 i=1,38
               call write_line(index, outputstring, 
     1        flux_ave(i), ((flux_ave_flag(i).eq.1).and.
     1                (mdata_x(i).eq.2)), .true.,
     1                dataformat_flag)
               call write_line(index, outputstring, 
     1        sigma_flux_ave(i),((flux_ave_flag(i).eq.1).and.
     1                (mdata_x(i).eq.2)), .true.,
     1                dataformat_flag)           
140     continue 
          do 160 i=1,15
               call write_line(index, outputstring, 
     1           0.0, .false., .false.,
     1                dataformat_flag)    ! fill in blank spaces                  
160     continue 
c
         write(unit_omni,1000) mo_2x, dy_2x, yr_2x,
     1         hr_2x, mn_2x, se_2x,
     1         yr_2x,doy_real_2x,  
     1         half_delta_t_2x, saturation_flag, 
     1         outputstring(1:(index-1))     
c   
c
c               1x matrices
c          *****************
         saturation_flag = 0
          if (vse_ave_flag.eq.-1) saturation_flag = 1
         do 310 i=1,4
            if (disc_flag(i).eq.-1) saturation_flag = 1
310     continue   
         do 320 i=1,38
            if ((flux_ave_flag(i).eq.-1).and.
     1          (mdata_x(i).eq.1)) saturation_flag = 1
320     continue
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
         do 330 i=1,4
           call write_line(index, outputstring, 
     1          disc(i), (disc_flag(i).eq.1), .true.,
     1                dataformat_flag)
            call write_line(index, outputstring, 
     1          sigma_disc(i), (disc_flag(i).eq.1), .true.,
     1                dataformat_flag)
c
330     continue            
         do 340 i=1,38
               call write_line(index, outputstring, 
     1        flux_ave(i), ((flux_ave_flag(i).eq.1).and.
     1                (mdata_x(i).eq.1)), .true.,
     1                dataformat_flag)
               call write_line(index, outputstring, 
     1        sigma_flux_ave(i),((flux_ave_flag(i).eq.1).and.
     1                (mdata_x(i).eq.1)), .true.,
     1                dataformat_flag)           
340     continue  
c
      call write_kpdata(index, outputstring)
c               
          write(unit_omni,1000) mo_1x, dy_1x, yr_1x,
     1         hr_1x, mn_1x, se_1x,
     1         yr_1x,doy_real_1x,  
     1         half_delta_t_1x, saturation_flag, 
     1         outputstring(1:(index-1))     
c   
1000 	   format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1          ', 'i4', 'f12.8', 'f12.4', 'i1',',a<index-1>)
c
         nrow_omni = nrow_omni + 3
c
      else
         print *, 'Number of rows exceeds maximum specified:'
     1            , maximum_rows
         write_output_omni = .false.
         return
      end if
c
      write_output_omni = .true.
      return
      end   ! end write_output_omni
c
