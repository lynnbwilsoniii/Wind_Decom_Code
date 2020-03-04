c    Subroutines for the program sdf_lister
c    
c     Contents:
c             logical function open_output_ave
c
c             subroutine assign_fluxes_ave
c
c             logical function write_output_ave
c
c		
c****************************************************
c
       logical function open_output_ave(unit_ave)
c
c****************************************************
c 
c     Open output data file and write the header for average matrix rates data
c     returns .true. if successful, .false. otherwise
c
c     12/4/95 by J. Dwyer
c          modified 2/28/96 by J. Dwyer added headerlines output to write_header
c                         4/8/96 by J. Dwyer changed headerlines to 40
c                         4/9/96 by J. Dwyer added year to output
c                         4/24/96 changed energy labels
c                          1/20/97 by J. Dwyer removed anisotropy data
c                          2/13/97 by J. Dwyer   added new KP list
c                          2/14/97 by J. Dwyer   removed junk, added write_kptitles
c
      include 'sdf_include.inc'   ! include type declarations
c    
      integer   unit_ave   ! file unit of output file    
      character*64     filename       ! complete name of output file
      character*4       file_prefix   ! prefix to be attached to output file name
      character*400   kpstring     !  string containing KP data names
      integer   headerlines
      integer   stringlength   ! length of KP string
c
      headerlines = 40
c    
      file_prefix = ' AVE'
      write(filename, '(a4,a60)') file_prefix, outputfile
      open(unit=unit_ave,name=outputdir//filename,
     1     status='NEW',DISP='KEEP',RECL=32766, err=1000) ! open output data file
      print *, 'Opened output file: ', filename
c
      call write_header(unit_ave,file_prefix, headerlines)
c
      call write_kptitles(kpstring,stringlength)  !  get string containing KP data names
c
      write(unit_ave,200) kpstring(1:stringlength)
200  	format(' Time,', 
     1         ' year,',
     1         ' day of the year,', 
     1         ' delta_t/2,',
     1         ' saturation flag,',    
     1         ' VSE, sigma,',
     1         ' VSEBAR, sigma,', 
     1         ' START, sigma,',
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
     1           a<stringlength>)
c    
      open_output_ave = .true.
      return  !  succussful
1000   open_output_ave = .false.  ! on error go to here
      print *, 'Error opening output averege matrix rates file:', 
     1     filename
      return      
      end  ! end open_output_ave
c
c
c
c
c**********************************************************
c
      subroutine assign_fluxes_ave(vse_ave, 
     1              sigma_vse_ave, vse_ave_flag,
     1          flux_ave, sigma_flux_ave, flux_ave_flag)
c
c**********************************************************
c
c    calculates sector averaged fluxes and sigmas for 1 or both telescopes
c
c     10/12/95 by J. Dwyer
c                 modified 4/22/96 corrected addition of two tel. when effic_flag = -1
c
      include 'sdf_include.inc'   ! include type declarations
c    
      real    delta_tvse  ! delta time over which disc rate is calculated
      real    vse1, vse2
      real    vse_ave, sigma_vse_ave      
      real    flux_tel(41,2), sigma_tel(41,2) ! arrays containing the fluxes and sigmas for all the elements
      real    flux_ave(41), sigma_flux_ave(41)   ! arrays containing the fluxes and sigmas for all the elements   
      integer   flux_flag_tel(41,2)   ! .true. when flux is measured and is not saturated, .false. otherwise
      integer   flux_ave_flag(41)   ! .true. when flux is measured and is not saturated, .false. otherwise
      integer   vse1_flag, vse2_flag  
      integer   vse_ave_flag
      integer   Ntel
c 
      vse1 = 0.0
      vse2 = 0.0  
      vse_ave = 0.0
      sigma_vse_ave = 0.0
      do 5 i=1,41
        flux_ave_flag(i) = 0
        flux_ave(i) = 0.0       
        sigma_flux_ave(i) = 0.0
        do 5 j=1,2        
           flux_flag_tel(i,j) = 0
           flux_tel(i,j) = 0.0       
5         sigma_tel(i,j) = 0.0     
c
      delta_tvse = time_interval   ! delta time over which disc rate is calculated	   
      if ((stateab_flag.eq.0).or.(stateab_flag.eq.1))   ! if selecting only state A or B
     1      delta_tvse = time_interval/2.0
c
c     
      do 10 k=1,8   
         if (vse_flag(1,k).eq.-1) then   ! don't include saturated counts
            vse1_flag = -1  
            vse1 = 0.0
            goto 15 
         else
            if (vse_flag(1,k).eq.1) then  ! include only values measured in this mf
               vse1_flag = 1
               vse1=vse1+vse(1,k)  ! do sum over sectors   
            end if   
         end if
10     continue
15    continue
      do 20 k=1,8
         if (vse_flag(2,k).eq.-1) then   ! don't include saturated counts
            vse2_flag = -1 
            vse2 = 0.0
            goto 25   
         else
            if (vse_flag(2,k).eq.1) then  ! include only values measured in this mf
               vse2_flag = 1
               vse2=vse2+vse(2,k)  ! do sum over sectors   
            end if   
         end if
20     continue  
25     continue
c
       if (tel_flag.eq.1) then   ! telescope 1 only           
         if (vse1_flag.eq.1) then
             vse_ave_flag = 1
             vse_ave = vse1/delta_tvse
             sigma_vse_ave = sqrt(1.0*vse1)/delta_tvse  
         else
             vse_ave_flag = -1      
         end if       
      end if
c
       if (tel_flag.eq.2) then   ! telescope 2 only
        if (vse2_flag.eq.1) then
             vse_ave_flag = 1
             vse_ave = vse2/delta_tvse
             sigma_vse_ave = sqrt(1.0*vse2)/delta_tvse  
         else
             vse_ave_flag = -1      
         end if  
      end if
c
       if (tel_flag.eq.-1) then   ! telescope 1 and 2       
          if ((vse1_flag.eq.1).and.(vse2_flag.eq.1)) then
             vse_ave_flag = 1
             vse_ave = (vse1+vse2)/delta_tvse
             sigma_vse_ave = sqrt(1.0*(vse1+vse2))/delta_tvse  
         else
              vse_ave_flag = -1     
         end if         
      end if         
c
c
       do 60 i=1,41
         do 50 j=1,2                     
            do 40 k=1,8   
                if (mdata_flag(i,j,k).eq.-1) then   ! don't include saturated counts
                   flux_flag_tel(i,j) = -1   
                   flux_tel(i,j) = 0.0
                   goto 50
                end if
                if (mdata_flag(i,j,k).eq.-2) then   ! don't include saturated counts
                   flux_flag_tel(i,j) = -2   
                   flux_tel(i,j) = 0.0
                end if
                if (mdata_flag(i,j,k).eq.1) then  ! include only values measured in this mf
                    flux_flag_tel(i,j) = 1
                    flux_tel(i,j)=flux_tel(i,j)+mdata(i,j,k)  ! do sum over sectors   
                 end if   
40        continue
45          if (flux_flag_tel(i,j).eq.1) then        
              sigma_tel(i,j)=sqrt(flux_tel(i,j))/exposure(i,j)   ! calculate error in flux
              flux_tel(i,j) = flux_tel(i,j)/exposure(i,j)  ! calculate flux from counts and exposure
            end if
50        continue
60        continue
c     
c           calculate total fluxes (telescope 1, 2 or 1+2)
c        **************************************
      if (tel_flag.eq.1) then   ! telescope 1 only
         do 100 i=1,41      
            flux_ave_flag(i) = flux_flag_tel(i,1)
            flux_ave(i) = flux_tel(i,1)
100      sigma_flux_ave(i) = sigma_tel(i,1)
      end if
       if (tel_flag.eq.2) then   ! telescope 2 only
         do 200 i=1,40
            flux_ave_flag(i) = flux_flag_tel(i,2)
            flux_ave(i) = flux_tel(i,2)
200      sigma_flux_ave(i) = sigma_tel(i,2)   
      end if     
      if (tel_flag.eq.-1) then   ! both telescopes      
         do 300 i=1,41     
            Ntel = 0 
            flux_ave_flag(i) = 0  
            flux_ave(i) =0.0
            if (flux_flag_tel(i,1).eq.1) then
                Ntel = 1
                flux_ave_flag(i) = 1
                flux_ave(i) = flux_tel(i,1)           
                sigma_flux_ave(i) = 
     1           sigma_tel(i,1)*sigma_tel(i,1)
            end if
            if (flux_flag_tel(i,2).eq.1) then
                Ntel = Ntel+1
                flux_ave_flag(i) = 1
                flux_ave(i) = flux_ave(i)+flux_tel(i,2)           
                sigma_flux_ave(i) = sigma_flux_ave(i)+
     1           sigma_tel(i,2)*sigma_tel(i,2)
            end if
            if (Ntel.gt.0) then
              flux_ave(i) = flux_ave(i)/(1.0*Ntel)
              sigma_flux_ave(i) = 
     1            sqrt(sigma_flux_ave(i))/(1.0*Ntel)
            end if
            if ((flux_flag_tel(i,1).eq.-2).and.
     1        (flux_flag_tel(i,2).eq.-2))  flux_ave_flag(i) = -2
            if ((flux_flag_tel(i,1).eq.-1).or.
     1        (flux_flag_tel(i,2).eq.-1))  flux_ave_flag(i) = -1                                
300     continue 
      end if      
c
      return
      end  ! end assign_flux_ave
c
c
c
c
c****************************************************
c
       logical function write_output_ave(unit_ave, nrow_ave,
     1             vse_ave, sigma_vse_ave, vse_ave_flag,
     1             flux_ave, sigma_flux_ave,flux_ave_flag, disc, 
     1             sigma_disc, disc_flag) 
c
c****************************************************
c
c      writes AVE data to output file with file unit=unit_ave
c      returns .false. if number of output lines written exceeds
c      specified value, .true. otherwise.
c
c     11/27/95 by J. Dwyer
c                       modified  4/9/96 by J. Dwyer added year to output
c                                       5/6/96 changed outputstring to 4096 characters
c                          1/20/97 by J. Dwyer removed anisotropy data
c                          2/13/97 by J. Dwyer   added new KP list
c                          2/14/97 by J. Dwyer   removed junk, added write_kpdata
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real   disc(4), sigma_disc(4)
      real   flux_ave(41), sigma_flux_ave(41)   ! arrays containing the fluxes and sigmas for all the elements 
      real   vse_ave, sigma_vse_ave
      real  phi, theta,ani
      real  sigma_phi, sigma_theta, sigma_ani
      real  chi2     
      real  doy_real
      real  half_delta_t  ! 1/2 of elapsed time interval (sec)
      integer   vse_ave_flag
      integer   flux_ave_flag(41)   ! .true. when flux is measured and is not saturated, .false. otherwise
      integer   disc_flag(4)
      integer    mid_time
      integer   unit_ave   ! output file unit
      integer   nrow_ave   !  number of rows written to output file
      integer      saturation_flag   ! 1 if any rate is saturated in mf, 0 otherwise
      integer   yr, mo, dy, hr, mn, se, doy
      integer    index     ! last index of outputstring written to 
      character*20   isotime   ! sampex_timecon isotime array
      character*4096   outputstring
      logical   fit_flag
c
      mid_time = (stop_time_avg+start_time_avg)/2
      half_delta_t = (stop_time_avg-start_time_avg)/2.0
      call sampex_timcon(mid_time,yr,mo,dy,hr,mn,se,doy,isotime)
      doy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0    
c     
      if (nrow_ave.lt.maximum_rows) then  
c
         saturation_flag = 0
          if (vse_ave_flag.eq.-1) saturation_flag = 1
         do 10 i=1,4
            if (disc_flag(i).eq.-1) saturation_flag = 1
10     continue   
         do 20 i=1,41
            if (flux_ave_flag(i).eq.-1) saturation_flag = 1
20     continue
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
         do 30 i=1,4
            call write_line(index, outputstring, 
     1          disc(i), (disc_flag(i).eq.1), .true.,
     1                dataformat_flag)
            call write_line(index, outputstring, 
     1          sigma_disc(i), (disc_flag(i).eq.1), .true.,
     1                dataformat_flag)
c
30      continue            
         do 40 i=1,38
            call write_line(index, outputstring, 
     1     flux_ave(i), (flux_ave_flag(i).eq.1), .true.,
     1                dataformat_flag)
            call write_line(index, outputstring, 
     1     sigma_flux_ave(i), (flux_ave_flag(i).eq.1), .true.,
     1                dataformat_flag)           
40     continue  
c
      call write_kpdata(index, outputstring)
c
          write(unit_ave,100) mo, dy, yr, hr, mn, se,
     1   yr, doy_real,  
     1   half_delta_t, saturation_flag, 
     1   outputstring(1:(index-1))     
c   
100 	   format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1      ', 'i4', 'f12.8', 'f12.4', 'i1',',a<index-1>)
              nrow_ave = nrow_ave + 1       
      else
         print *, 'Number of rows exceeds maximum specified:'
     1            , maximum_rows
         write_output_ave = .false.
         return
      end if
c
      write_output_ave = .true.
      return
      end   ! end write_output_ave
c
