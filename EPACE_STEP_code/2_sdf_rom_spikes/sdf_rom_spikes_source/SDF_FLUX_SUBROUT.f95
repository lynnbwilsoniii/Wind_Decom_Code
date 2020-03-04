c    Subroutines for the program sdf_lister
c    
c     Contents:
c             logical function open_output_flux
c
c             subroutine step_effic_sub
c
c             subroutine acquire_counts
c
c             subroutine evaluate_pha_flux
c
c             function get_sigma_flux
c     
c             function get_sigma_flux2
c
c             logical function write_output_flux
c
c
c		
c****************************************************
c
       logical function open_output_flux(unit_flux)
c
c****************************************************
c 
c     Open output data file and write the header for pha flux data
c     returns .true. if successful, .false. otherwise
c
c     12/6/95 by J. Dwyer
c          modified 2/28/96 by J. Dwyer added calibration data output to header
c          modified 3/21/96 by J. Dwyer added atomic charge to header
c                         4/4/96 by J. Dwyer  changed format slightly
c                         4/8/96 by J. Dwyer changed headerlines to 40+...
c                         4/9/96 by J. Dwyer added year to output
c                         5/10/96 by J. Dwyer added tel flag to output header
c                         2/13/97 by J. Dwyer   added new KP list
c                         2/14/97 by J. Dwyer added write_kptitles
c
      include 'sdf_include.inc'   ! include type declarations
c    
      integer   unit_flux   ! file unit of output file    
      integer   headerlines
      character*64     filename       ! complete name of output file
      character*30000     line
      character*400   kpstring     !  string containing KP data names
      character*4       file_prefix   ! prefix to be attached to output file name
      integer    index
      integer   stringlength   ! length of KP string
c    
      file_prefix = 'FLUX'
      write(filename, '(a4,a60)') file_prefix, outputfile
      open(unit=unit_flux,name=outputdir//filename,
     1     status='NEW',DISP='KEEP',RECL=32766, err=1000) ! open output data file
      print *, 'Opened output file: ', filename
c
      headerlines = 40+nfluxes+4   ! number of lines in header before titles
      call write_header(unit_flux,file_prefix,headerlines)
c  
      write(unit_flux,31)   
      write(unit_flux,32) nfluxes
      write(unit_flux,33)   
c
      do 30 i=1,nfluxes
         write(unit_flux,35) 
     1     pha_flux_names(i), atomic_charge(i),
     1     atomic_mass_mid(i),
     1     atomic_mass_lo(i),atomic_mass_hi(i),
     1     PHA_E_mid(i),
     1     PHA_E_lo(i), PHA_E_hi(i),
     1     PHA_slant_flag(i), PHA_tel_flag(i)        
30     continue	
      write(unit_flux,*)  ' '
c
31      format(' **** PHA flux calibration data ****  ')
32      format(' N fluxes = ',i4)
33      format(' Name,Z,M(AMU),Mmin(AMU),Mmax(AMU),E(MeV/AMU),',
     1                ' Emin(MeV/AMU),Emax(MeV/AMU),slant,tel')
35      format(' ',a20,','F7.3,','F7.3,','F7.3,',' 
     1             F7.3,','F7.3,','F7.3,','F7.3,','i2,','i2)
c 
      index = 1
      do 50 i=1,nfluxes   
        write(line(index:(index+29-1)), 100)  pha_flux_names(i)
        index= index+29
50      continue 
c
       call write_kptitles(kpstring,stringlength)  !  get string containing KP data names
c
       line(index:(index+stringlength)) = kpstring(1:stringlength)
       index = index+stringlength
100     format(' ', a20,', sigma,')
c
      write(unit_flux,200) line
200  	format(' Time,',
     1         ' year,',
     1         ' day of the year,', 
     1         ' delta_t/2,',  
     1         ' saturation flag,', 
     1         ' VSE, sigma,',
     1         a<index-1>)
c    
      open_output_flux = .true.
      return  !  succussful
1000   open_output_flux = .false.  ! on error go to here
      print *, 'Error opening output pha flux rates file:', 
     1     filename
      return      
      end  ! end open_output_flux
c
c
c
c
c*****************************************************
c
        subroutine step_effic_sub(am,az,energy,sampex_time,eff)
c
c*****************************************************
c
c       calculates average efficiencies for rom boxes, taking
c       account of (1) MCP triggering, and (2) scattering in telescope
c
c       gm 8/7/95
c
c       Modification history:
c               8/7/95  adapt from program step_rom_box_eff
c               3/21/96 adapted to sdf_lister by J. Dwyer
c               3/25/96 adapted to sdf_lister by J. Dwyer
c               3/26/96 by J. Dwyer added az to trigger_effic call
c
        integer       sampex_time     ! dummy for trigger_effic call
        real            am              ! atomic mass
        real            az              ! atomic number
        real           energy              ! incident energy
        real           eff               !  efficiency
c       
c
        call trigger_effic(az,am,energy,sampex_time,eff)
c
c       now fold in the Coulomb scattering function:
c
        Cconst = 0.00428
        Cscatt = exp(-Cconst*(az/(Energy*am))**2)
        eff = eff*Cscatt
c
        return
        end     ! end step_effic_sub
c
c
c
c
c*****************************************************
c
      subroutine acquire_counts
c
c******************************************************
c
c      accumulate PHA counts for fluxes
c      12/6/95 by J. Dwyer
c              modifications
c                  3/22/96 by J. Dwyer added efficiency calculation for each PHA event
c                  3/25/96 by J. Dwyer moved efficiency calculation to function evaluate_pha
c                  3/26/96 by J. Dwyer removed efficiency calculation 
c                  4/9/96 by J. Dwyer added  (mass(i).gt.0.01).and.(Einc(i).gt.0.01) requirement
c                  4/23/96 by J. Dwyer removed  (mass(i).gt.0.01).and.(Einc(i).gt.0.01) requirement
c                  4/23/96 by J. Dwyer moved PHA_counts_sum outside data_flag if
c                  5/6/96 by J. Dwyer moved PHA_counts_sum inside data_flag if
c                  5/10/96 by J. Dwyer added option to bin with MeV or Einc
c                  5/14/96 by J. Dwyer added option to bin with Epha (E channel)
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real   Energy
c  
      Energy = 0.0
c
      do 200 i=1, NPHA   ! loop over PHA events   
           if (write_flux_matrix.eq.1) 
     1          Energy = Einc(i)   ! use K.E./nuc  to histogram
           if (write_flux_matrix.eq.2) 
     1          Energy = MeV(i)   ! use K.E.  to histogram
          if (write_flux_matrix.eq.3) 
     1          Energy = 1.0*Epha(i)   ! use energy channel  to histogram                              
           if (data_flag(i)) then  ! O.K. PHA events 
            PHA_counts_sum(2-tel(i),2-ab(i),slant(i)+1)=
     1     PHA_counts_sum(2-tel(i),2-ab(i),slant(i)+1)+1.0    
            do 100 j=1, nfluxes   ! loop over PHA flux boxes                                                                    
               if ((mass(i).gt.atomic_mass_lo(j)).and.
     1           (mass(i).le.atomic_mass_hi(j)).and.
     1           (Energy.gt.PHA_E_lo(j)).and.
     1           (Energy.le.PHA_E_hi(j))) then                                                           
                   PHA_counts(j,2-tel(i),2-ab(i),slant(i)+1)=
     1            PHA_counts(j,2-tel(i),2-ab(i),slant(i)+1)+1.0 
                   PHA_Ncounts(j) = PHA_Ncounts(j) + 1.0          
               end if
100        continue
           end if
200      continue   
c
      return
      end  ! end acquire_counts
c
c
c
c
c*****************************************************
c
      logical function evaluate_pha_flux
     1             (unit_flux, nrow_flux,
     1             vse_ave, sigma_vse_ave,
     1             vse_ave_flag)
c
c******************************************************
c
c	  calculates PHA fluxes from accumulated PHA data
c    and vse discriminator rates
c    12/6/95 by J. Dwyer
c         modified 3/22/96 by J. Dwyer changed PHA_exposure calculation
c         modified 3/26/96 by J. Dwyer changed PHA_exposure calculation, added delta E
c                                                         to exposure to get diff flux if desired.
c                        4/9/96 by J. Dwyer modified to use flux calculation 
c                        4/11/96 by J. Dwyer modified sigma_flux_pha when adding A and B and
c                                                               the two telescopes
c                        4/22/96 by J. Dwyer corrected combining of A and B
c                        4/25/96 by J. Dwyer corrected  sumcountsA (took out slant fired counts)
c                        4/25/96 by J. Dwyer added PHA_tel_flag
c                        4/26/96 by J. Dwyer took out PHA_counts(*,*,1,2) term from state A fluxes
c                        5/10/96 by J. Dwyer change to write_flux_matrix.ge.1
c                        5/10/96 by J. Dwyer use get_sigma_flux2 to get errors
c
      include 'sdf_include.inc'   ! include type declarations
c    
      real   flux_pha(100), sigma_flux_pha(100)   ! arrays containing the fluxes and sigmas
      real   flux_pha1_noslant(100), 
     1        sigma_flux_pha1_noslant(100)   ! tel 1, slant=0
      real   flux_pha1_slant(100), 
     1        sigma_flux_pha1_slant(100)   ! tel 1, slant=1
      real   flux_pha2_noslant(100), 
     1        sigma_flux_pha2_noslant(100)   ! tel 2, slant=0
      real   flux_pha2_slant(100), 
     1        sigma_flux_pha2_slant(100)   ! tel 2, slant=1
      real   flux_pha1(100), sigma_flux_pha1(100)   ! tel 1
      real   flux_pha2(100), sigma_flux_pha2(100)   ! tel 2
      real   sigma_tempA, sigma_tempB
      real   sumcounts1_slant, sumcounts1_noslant   ! sum of all counts in tel 1, slant=0,1
      real   sumcounts2_slant, sumcounts2_noslant  ! sum of all counts in tel 2, slant=0,1
      real   exposure_pha(100,2), ehilo(100,2)  
      real   vse_ave, sigma_vse_ave  ! vse rate and error averaged over the sectors
      real    dehilo   ! delta E
      integer  vse_ave_flag 
      integer   unit_flux   ! output file unit
      integer   nrow_flux   !  number of rows written to output file
      logical      write_output_flux    ! function declaration
c      

      do 10 i=1,Nfluxes          
         if (fluxformat_flag.eq.1) 
     1            dehilo = 1.0
         if (fluxformat_flag.eq.0) 
     1            dehilo = PHA_E_hi(i)-PHA_E_lo(i)                   
          PHA_exposure(i,1) = aom(1)*effic_pha_ave(i)*
     1                       0.5*time_interval*dehilo
          PHA_exposure(i,2) = aom(2)*effic_pha_ave(i)*
     1                       0.5*time_interval*dehilo                     
10      continue        
c                
         sumcounts1_noslant = PHA_counts_sum(1,1,1)
         sumcounts2_noslant = PHA_counts_sum(2,1,1)
         sumcounts1_slant = PHA_counts_sum(1,1,2)+
     1                                   PHA_counts_sum(1,2,2)
         sumcounts2_slant = PHA_counts_sum(2,1,2)+
     1                                   PHA_counts_sum(2,2,2)         
c
      do 100 i=1, nfluxes   ! loop over PHA flux boxes
         if ((sumcounts1_noslant.gt.0.0).and.
     1      (PHA_exposure(i,1).gt.0.0).and.
     1      ((PHA_slant_flag(i).eq.0).or.
     1        (PHA_slant_flag(i).eq.-1))) then
               flux_pha1_noslant(i) =  PHA_counts(i,1,1,1)*
     1       (vse1A/sumcounts1_noslant)/
     1        PHA_exposure(i,1)
         sigma_flux_pha1_noslant(i) = get_sigma_flux2
     1    (PHA_counts(i,1,1,1),vse1A,sumcounts1_noslant)
     1     /PHA_exposure(i,1)
         else
             flux_pha1_noslant(i) = 0.0
             sigma_flux_pha1_noslant(i) = 0.0
         end if
c
         if ((sumcounts2_noslant.gt.0.0).and.
     1      (PHA_exposure(i,2).gt.0.0).and.
     1      ((PHA_slant_flag(i).eq.0).or.
     1       (PHA_slant_flag(i).eq.-1))) then
               flux_pha2_noslant(i) =  PHA_counts(i,2,1,1)*
     1       (vse2A/sumcounts2_noslant)/
     1        PHA_exposure(i,2)
         sigma_flux_pha2_noslant(i) = get_sigma_flux2
     1    (PHA_counts(i,2,1,1),vse2A,sumcounts2_noslant)
     1     /PHA_exposure(i,2)
         else
             flux_pha2_noslant(i) = 0.0
             sigma_flux_pha2_noslant(i) = 0.0
         end if
c
         if ((sumcounts1_slant.gt.0.0).and.
     1      (PHA_exposure(i,1).gt.0.0).and.
     1      ((PHA_slant_flag(i).eq.1).or.
     1       (PHA_slant_flag(i).eq.-1))) then
               flux_pha1_slant(i) =  (PHA_counts(i,1,1,2)+
     1        PHA_counts(i,1,2,2))*
     1       (vse1B/sumcounts1_slant)/
     1        PHA_exposure(i,1)
         sigma_flux_pha1_slant(i) = get_sigma_flux2
     1    ((PHA_counts(i,1,1,2)+PHA_counts(i,1,2,2)),
     1      vse1B,sumcounts1_slant)
     1     /PHA_exposure(i,1)
         else
             flux_pha1_slant(i) = 0.0
             sigma_flux_pha1_slant(i) = 0.0
         end if
c
         if ((sumcounts2_slant.gt.0.0).and.
     1      (PHA_exposure(i,2).gt.0.0).and.
     1      ((PHA_slant_flag(i).eq.1).or.
     1        (PHA_slant_flag(i).eq.-1))) then
               flux_pha2_slant(i) =  (PHA_counts(i,2,1,2)+
     1        PHA_counts(i,2,2,2))*
     1       (vse2B/sumcounts2_slant)/
     1        PHA_exposure(i,2)
         sigma_flux_pha2_slant(i) = get_sigma_flux2
     1    ((PHA_counts(i,2,1,2)+PHA_counts(i,2,2,2)),
     1      vse2B,sumcounts2_slant)
     1     /PHA_exposure(i,2)
         else
             flux_pha2_slant(i) = 0.0
             sigma_flux_pha2_slant(i) = 0.0
         end if
c
c            get requested slant (0,1 or both (-1))
c          ****************************            
        if (PHA_slant_flag(i).eq.0) then   ! slant = 0 only
            flux_pha1(i) = flux_pha1_noslant(i)    
            flux_pha2(i) = flux_pha2_noslant(i)
            sigma_flux_pha1(i)=
     1             sigma_flux_pha1_noslant(i)
            sigma_flux_pha2(i)=
     1            sigma_flux_pha2_noslant(i)
         end if  ! end if slant = 0
c
         if (PHA_slant_flag(i).eq.1) then   ! slant = 1 only
            flux_pha1(i) = flux_pha1_slant(i)    
            flux_pha2(i) = flux_pha2_slant(i)
            sigma_flux_pha1(i)=
     1             sigma_flux_pha1_slant(i)
            sigma_flux_pha2(i)=
     1            sigma_flux_pha2_slant(i)
         end if  ! end if slant = 1
c
        if (PHA_slant_flag(i).eq.-1) then   ! combine slants 0 and 1
            flux_pha1(i) = flux_pha1_slant(i)+
     1                            flux_pha1_noslant(i) 
            flux_pha2(i) = flux_pha2_slant(i)+
     1                            flux_pha2_noslant(i)
            sigma_flux_pha1(i) = 
     1     sqrt(sigma_flux_pha1_slant(i)*
     1     sigma_flux_pha1_slant(i)+
     1     sigma_flux_pha1_noslant(i)*
     1     sigma_flux_pha1_noslant(i))
           sigma_flux_pha2(i) = 
     1     sqrt(sigma_flux_pha2_slant(i)*
     1     sigma_flux_pha2_slant(i)+
     1     sigma_flux_pha2_noslant(i)*
     1     sigma_flux_pha2_noslant(i))
        end if  ! end if both slants          
c
c            get requested telescope (1,2 or both (-1))
c          ***********************************
      if (PHA_tel_flag(i).eq.1) then   ! telescope 1 only              
             flux_pha(i) = flux_pha1(i)    
             sigma_flux_pha(i)=sigma_flux_pha1(i)              
      end if
c
       if (PHA_tel_flag(i).eq.2) then   ! telescope 2 only  
             flux_pha(i) = flux_pha2(i)    
             sigma_flux_pha(i)=sigma_flux_pha2(i)              
      end if
c
       if (PHA_tel_flag(i).eq.-1) then   ! telescope 1 and 2            
             flux_pha(i) = 
     1            0.5*(flux_pha1(i)+flux_pha2(i))               
             sigma_flux_pha(i)=
     1      0.5*sqrt((sigma_flux_pha1(i)**2)+
     1             (sigma_flux_pha2(i)**2))                 
      end if  
100     continue     
c
c
      evaluate_pha_flux = .true.
      if (write_flux_matrix.ge.1) then   
         evaluate_pha_flux = write_output_flux
     1             (unit_flux, nrow_flux,
     1             flux_pha, sigma_flux_pha,
     1              vse_ave, sigma_vse_ave,
     1             vse_ave_flag) 
      end if
c
      return
      end  ! end evaluate_pha_fluxes
c
c
c
c
c*****************************************************
c
      real function get_sigma_flux(N1,N2,N3)
c
c******************************************************
c       
c   calculates Poisson error of N1*N2/N3
c   12/5/95 by J. Dwyer
c
      real N1, N2, N3   
c
      if ((N1.gt.0.0).and.
     1    (N2.gt.0.0).and.
     1    (N3.gt.0.0)) then
         get_sigma_flux = (N1*N2/N3)*
     1       sqrt((1.0/N1)+(1.0/N2)+(1.0/N3))
       else
         get_sigma_flux = 0.0
      end if 
      return
      end  ! end get_sigma_flux
c
c
c
c
c*****************************************************
c
      real function get_sigma_flux2(N1,N2,N3)
c
c******************************************************
c       
c   calculates Poisson error of N1*N2/N3 assuming error associated with N3 is 0
c   5/10/96 by J. Dwyer
c
      real N1, N2, N3   
c
      if ((N1.gt.0.0).and.
     1    (N2.gt.0.0).and.
     1    (N3.gt.0.0)) then
         get_sigma_flux2 = (N1*N2/N3)*
     1       sqrt((1.0/N1)+(1.0/N2)+(0.0/N3))
       else
         get_sigma_flux2 = 0.0
      end if
      return
      end  ! end get_sigma_flux2
c
c
c
c
c****************************************************
c
       logical function write_output_flux
     1             (unit_flux, nrow_flux,
     1             flux_pha, sigma_flux_pha,
     1             vse_ave, sigma_vse_ave,
     1             vse_ave_flag) 
c
c****************************************************
c
c      writes FLUX data to output file with file unit=unit_flux
c      returns .false. if number of output lines written exceeds
c      specified value, .true. otherwise.
c
c     12/6/95 by J. Dwyer
c                modified  4/9/96 by J. Dwyer added year to output
c                                5/6/96 changed outputstring to 4096 characters
c                                2/13/97 by J. Dwyer   added new KP list
c                                2/14/97 by J. Dwyer added write_kpdata
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      
      real   flux_pha(100), sigma_flux_pha(100)   ! arrays containing the fluxes and sigmas     
      real  doy_real
      real  half_delta_t  ! 1/2 of elapsed time interval (sec)  
      real   vse_ave, sigma_vse_ave  ! vse rate and error averaged over the sectors
      integer  vse_ave_flag  
      integer      saturation_flag   ! 1 if any rate is saturated in mf, 0 otherwise
      integer   mid_time
      integer   unit_flux   ! output file unit
      integer   nrow_flux   !  number of rows written to output file   
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
      if (nrow_flux.lt.maximum_rows) then  
c
         saturation_flag = 0
         if (vse_ave_flag.eq.-1) saturation_flag = 1
c       
         index = 1            
         call write_line(index, outputstring, 
     1          vse_ave, (vse_ave_flag.eq.1), .true.,
     1                dataformat_flag)
         call write_line(index, outputstring, 
     1          sigma_vse_ave, (vse_ave_flag.eq.1), .true.,
     1                dataformat_flag)  
c 
         do 50 i=1,nfluxes
            call write_line(index, outputstring, 
     1     flux_pha(i), .true., .true.,
     1                dataformat_flag)
            call write_line(index, outputstring, 
     1     sigma_flux_pha(i), .true., .true.,
     1                dataformat_flag)           
50     continue  
c
      call write_kpdata(index, outputstring)
c  
          write(unit_flux,100) mo, dy, yr, hr, mn, se,
     1   yr, doy_real,  
     1   half_delta_t, saturation_flag,
     1   outputstring(1:(index-1))     
c   
100 	   format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1      ', 'i4', 'f12.8', 'f12.4', 'i1',',a<index-1>)
              nrow_flux = nrow_flux + 1       
      else
         print *, 'Number of rows exceeds maximum specified:'
     1            , maximum_rows
         write_output_flux = .false.
         return
      end if
c
      write_output_flux = .true.
      return
      end   ! end write_output_flux
c
