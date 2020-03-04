c    Subroutines for the program sdf_lister
c    
c     Contents:
c            logical function evaluate_matrix
c
c            subroutine get_exposure
c
c            subroutine assign_disc
c
c
c********************************************************
c
           logical function evaluate_matrix
     1                                          (unit_ave, unit_sect,
     1                                           unit_omni, unit_abr,
     1                                           nrow_ave, nrow_sect,
     1                                           nrow_omni,nrow_abr,
     1               vse_ave, sigma_vse_ave, vse_ave_flag)
c
c*********************************************************
c   
c     Processes matrix fluxes and disc rates and writes to output files
c     returns .false. if number of output lines written exceeds
c     specified value, .true. otherwise.
c
c     1/23/96  by J. Dwyer
c                modified   1/6/97 by J. Dwyer changed format to vse,E2,...,E6
c                 1/15/97 by J. Dwyer fixed bug with E4_sect_tel2_sigma
c                 1/20/97 by J. Dwyer removed VSE sectors, added E1 tel 1 and 2
c                 1/20/97 by J. Dwyer removed anisotropy data from AVE and SECT
c                  2/7/97 by J. Dwyer added E7 - E10 for protons
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      real   flux_ave(41), sigma_flux_ave(41)   ! arrays containing the fluxes and errors for all the elements
      real   He_sect_tel1(8), He_sect_tel1_sigma(8), 
     1        He_sect_tel2(8), He_sect_tel2_sigma(8), 
     1        CNO_sect_tel1(8), CNO_sect_tel1_sigma(8), 
     1        CNO_sect_tel2(8), CNO_sect_tel2_sigma(8), 
     1        Fe_sect_tel1(8), Fe_sect_tel1_sigma(8), 
     1        Fe_sect_tel2(8), Fe_sect_tel2_sigma(8),
     1        E1_sect_tel1(8), E1_sect_tel1_sigma(8), 
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
      real   disc(4), sigma_disc(4)     ! arrays containing all the nonsectored disc rates and errors
      real   vse_ave, sigma_vse_ave  ! vse rate and error averaged over the sectors
      real  phi, theta,ani
      real  sigma_phi, sigma_theta, sigma_ani
      real  chi2  ! reduced chi-squared
      logical      write_output_ave     ! function declaration
      logical      write_output_sect     ! function declaration
      logical      write_output_omni     ! function declaration
      logical      write_output_abr     ! function declaration
      integer   flux_ave_flag(41)   ! 1 when flux is measured, 0 not measured, -1 saturated
      integer   He_sect_tel1_flag(8),
     1             He_sect_tel2_flag(8),
     1             CNO_sect_tel1_flag(8),
     1             CNO_sect_tel2_flag(8),
     1             Fe_sect_tel1_flag(8),
     1             Fe_sect_tel2_flag(8),
     1             E1_sect_tel1_flag(8),
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
      integer  disc_flag(4)  ! 1 when disc is measured, 0 not measured, -1 saturated
      integer  vse_ave_flag  
      integer    nrow_ave, nrow_sect, 
     1              nrow_omni, nrow_abr  !  number of rows written to output file
      integer   unit_sect, unit_ave,
     1             unit_omni, unit_abr  ! output file unit
      logical    output_flag_ave, output_flag_sect,
     1              output_flag_omni, output_flag_abr  ! .false. = number of output rows exceeds limit     
c
      output_flag_ave = .true.
      output_flag_sect = .true.
      output_flag_omni = .true.
      output_flag_abr = .true.
c                 
c                             
      call assign_disc(disc, sigma_disc, disc_flag)   ! converts summed discriminator counts into rates                  
c     
      call assign_fluxes_ave(vse_ave, sigma_vse_ave, vse_ave_flag,
     1     flux_ave, sigma_flux_ave, flux_ave_flag)   ! get fluxes and rates
c         
      call assign_sectored_fluxes(He_sectored_flag,1,
     1     He_sect_tel1, He_sect_tel1_sigma, He_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(He_sectored_flag,2,
     1     He_sect_tel2, He_sect_tel2_sigma, He_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(CNO_sectored_flag,1,
     1     CNO_sect_tel1, CNO_sect_tel1_sigma, CNO_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(CNO_sectored_flag,2,
     1     CNO_sect_tel2, CNO_sect_tel2_sigma, CNO_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(Fe_sectored_flag,1,
     1     Fe_sect_tel1, Fe_sect_tel1_sigma, Fe_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(Fe_sectored_flag,2,
     1     Fe_sect_tel2, Fe_sect_tel2_sigma, Fe_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E1_sectored_flag,1,
     1     E1_sect_tel1, E1_sect_tel1_sigma, E1_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E1_sectored_flag,2,
     1     E1_sect_tel2, E1_sect_tel2_sigma, E1_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E2_sectored_flag,1,
     1     E2_sect_tel1, E2_sect_tel1_sigma, E2_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E2_sectored_flag,2,
     1     E2_sect_tel2, E2_sect_tel2_sigma, E2_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E3_sectored_flag,1,
     1     E3_sect_tel1, E3_sect_tel1_sigma, E3_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E3_sectored_flag,2,
     1     E3_sect_tel2, E3_sect_tel2_sigma, E3_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E4_sectored_flag,1,
     1     E4_sect_tel1, E4_sect_tel1_sigma, E4_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E4_sectored_flag,2,
     1     E4_sect_tel2, E4_sect_tel2_sigma, E4_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E5_sectored_flag,1,
     1     E5_sect_tel1, E5_sect_tel1_sigma, E5_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E5_sectored_flag,2,
     1     E5_sect_tel2, E5_sect_tel2_sigma, E5_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E6_sectored_flag,1,
     1     E6_sect_tel1, E6_sect_tel1_sigma, E6_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E6_sectored_flag,2,
     1     E6_sect_tel2, E6_sect_tel2_sigma, E6_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E7_sectored_flag,1,
     1     E7_sect_tel1, E7_sect_tel1_sigma, E7_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E7_sectored_flag,2,
     1     E7_sect_tel2, E7_sect_tel2_sigma, E7_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E8_sectored_flag,1,
     1     E8_sect_tel1, E8_sect_tel1_sigma, E8_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E8_sectored_flag,2,
     1     E8_sect_tel2, E8_sect_tel2_sigma, E8_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E9_sectored_flag,1,
     1     E9_sect_tel1, E9_sect_tel1_sigma, E9_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E9_sectored_flag,2,
     1     E9_sect_tel2, E9_sect_tel2_sigma, E9_sect_tel2_flag)    ! get fluxes and rates
c
      call assign_sectored_fluxes(E10_sectored_flag,1,
     1     E10_sect_tel1, E10_sect_tel1_sigma, E10_sect_tel1_flag)    ! get fluxes and rates
      call assign_sectored_fluxes(E10_sectored_flag,2,
     1     E10_sect_tel2, E10_sect_tel2_sigma, E10_sect_tel2_flag)    ! get fluxes and rates
c
c      call assign_sectored_vse
c     1               (1, vse_sect_tel1,
c     1                vse_sect_tel1_sigma, vse_sect_tel1_flag)  ! get sectored VSE rates
c      call assign_sectored_vse
c     1               (2, vse_sect_tel2,
c     1                vse_sect_tel2_sigma, vse_sect_tel2_flag)  ! get sectored VSE rates
c                            
c               
      if (write_ave_matrix.eq.1) then                                  
          output_flag_ave = write_output_ave(unit_ave, nrow_ave,
     1             vse_ave, sigma_vse_ave, vse_ave_flag,
     1             flux_ave, sigma_flux_ave,flux_ave_flag, disc, 
     1             sigma_disc, disc_flag)  ! write processed data to output file
      end if  
c
      if (write_abr_matrix.eq.1) then                                  
          output_flag_abr = write_output_abr
     1            (unit_abr, nrow_abr,
     1             vse_ave, sigma_vse_ave, vse_ave_flag,
     1             flux_ave, sigma_flux_ave,flux_ave_flag, disc, 
     1             sigma_disc, disc_flag, 
     1             E2_sect_tel1, E2_sect_tel1_sigma, 
     1             E2_sect_tel1_flag,
     1             E2_sect_tel2, E2_sect_tel2_sigma, 
     1             E2_sect_tel2_flag)  ! write processed data to output file
      end if  
c
      if (write_omni_matrix.eq.1) then                                  
          output_flag_omni = 
     1             write_output_omni(unit_omni, nrow_omni,
     1             vse_ave, sigma_vse_ave, vse_ave_flag,
     1             flux_ave, sigma_flux_ave,flux_ave_flag, disc, 
     1             sigma_disc, disc_flag)  ! write processed data to output file
      end if  
c    
      if (write_sect_matrix.gt.0) then
          output_flag_sect = write_output_sect
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
     1        E10_sect_tel2_flag)  ! write processed data to output file
      end if      
c
      evaluate_matrix = ((output_flag_ave).and.
     1     (output_flag_sect).and.
     1     (output_flag_omni).and.
     1      (output_flag_abr))  
c        
      return
      end  ! end evaluate_matrix
c
c
c
c
c*****************************************************
c
      subroutine get_exposure
c
c*****************************************************
c
c     calculates accumulated exposure 
c
c     11/20/95 by J. Dwyer
c              modifications:
c                     5/1/96 by J. Dwyer fixed bug with time_interval_4x accumulation
c
      include 'sdf_include.inc'   ! include type declarations
c  
      real    exposure_1mf(41,2)  ! exposure for 1 mf
      real    ehilo(41,2)    ! delta E
      real    delta_tmf, delta_t1x  ! livetimes for 1 mf
      real    delta_t2x
      real    delta_t4x      
c
      do 5 i=1,41
         do 5 j=1,2
            if (fluxformat_flag.eq.1) 
     1            ehilo(i,j) = 1.0
             if (fluxformat_flag.eq.0) 
     1            ehilo(i,j) = ehi(i,j)-elo(i,j) 
            exposure_1mf(i,j) =  0.0
5      continue
c
      delta_tmf = spin_period*nspins   ! elapsed time in 1 mf
      delta_t1x = delta_tmf/2.0     ! elapsed time in state A in 1 mf    
      delta_t2x = delta_tmf+
     1     spin_period*ave_spins(mf_type)          
      delta_t4x = delta_tmf+
     1     3.0*spin_period*ave_spins(mf_type)   
      if ((stateab_flag.eq.0).or.(stateab_flag.eq.1))  then  ! if selecting only state A or B
        delta_tmf = delta_tmf/2.0    ! use for state1, state2 disc rates and junk rates 
        delta_t2x = delta_t2x /2.0 
        delta_t4x = delta_t4x /2.0          
      end if      
c  1x exposures
      time_interval = time_interval+ 
     1         spin_period*nspins    ! record time elapsed
      do 7 j=1,2
              exposure_1mf(1,j) = aom(j)*
     1       ehilo(1,j)*effic(1,j)*delta_tmf    ! state rates
7      continue
      do 10 i=2,18
         do 10 j=1,2
              exposure_1mf(i,j) = aom(j)*
     1       ehilo(i,j)*effic(i,j)*delta_t1x 
10      continue       
      do 15 i=39,41
          do 15 j=1,2
15           exposure_1mf(i,j) = aom(j)*
     1        ehilo(i,j)*effic(i,j)*delta_tmf    ! junk rates
c
c   2x exposures
      if ((phase.eq.0).or.(phase.eq.2)) then
         time_interval_2x(1) = time_interval_2x(1)+ 
     1         spin_period*nspins+
     1         spin_period*ave_spins(mf_type)     ! record time elapsed
         do 20 i=19,21
            do 20 j=1,2
                exposure_1mf(i,j) =aom(j)*
     1         ehilo(i,j)*effic(i,j)*delta_t2x
20    continue
         exposure_1mf(22,2) =aom(2)*
     1       ehilo(22,2)*effic(22,2)*delta_t2x
      else
         time_interval_2x(2) = time_interval_2x(2)+ 
     1         spin_period*nspins+
     1         spin_period*ave_spins(mf_type)     ! record time elapsed
         exposure_1mf(22,1) =aom(1)*
     1     ehilo(22,1)*effic(22,1)*delta_t2x
         do 25 i=23,25
            do 25 j=1,2
                exposure_1mf(i,j) =aom(j)*
     1         ehilo(i,j)*effic(i,j)*delta_t2x
25    continue         
      end if
c
c   4x exposures
      if (phase.eq.0) then
         time_interval_4x(1) = time_interval_4x(1)+ 
     1         spin_period*nspins+
     1         3.0*spin_period*ave_spins(mf_type)     ! record time elapsed
         do 30 i=26,33
            do 30 j=1,2
         exposure_1mf(i,j) =aom(j)*
     1     ehilo(i,j)*effic(i,j)*delta_t4x  
30    continue     
        exposure_1mf(34,2) = aom(2)*
     1     ehilo(34,2)*effic(34,2)*delta_t4x
      end if
c
      if (phase.eq.1) then
        time_interval_4x(2) = time_interval_4x(2)+ 
     1         spin_period*nspins+
     1         3.0*spin_period*ave_spins(mf_type)     ! record time elapsed
        exposure_1mf(34,1) =aom(1)*
     1     ehilo(34,1)*effic(34,1)*delta_t4x 
        exposure_1mf(35,2) =aom(2)*
     1     ehilo(35,2)*effic(35,2)*delta_t4x
        exposure_1mf(35,1) =aom(1)*
     1     ehilo(35,1)*effic(35,1)*delta_t4x          
      end if
c
      if (phase.eq.2) then      
         time_interval_4x(3) = time_interval_4x(3)+ 
     1         spin_period*nspins+
     1         3.0*spin_period*ave_spins(mf_type)     ! record time elapsed
         exposure_1mf(36,2) =aom(2)*
     1     ehilo(36,2)*effic(36,2)*delta_t4x 
        exposure_1mf(36,1) =aom(1)*
     1     ehilo(36,1)*effic(36,1)*delta_t4x
        exposure_1mf(37,2) =aom(2)*
     1     ehilo(37,2)*effic(37,2)*delta_t4x
      end if
c
      if (phase.eq.3) then
        time_interval_4x(4) = time_interval_4x(4)+ 
     1         spin_period*nspins+
     1         3.0*spin_period*ave_spins(mf_type)     ! record time elapsed
         exposure_1mf(37,1) =aom(1)*
     1     ehilo(37,1)*effic(37,1)*delta_t4x 
        exposure_1mf(38,2) =aom(2)*
     1     ehilo(38,2)*effic(38,2)*delta_t4x
        exposure_1mf(38,1) = aom(1)*
     1     ehilo(38,1)*effic(38,1)*delta_t4x
      end if
c
c   add result to total exposure
      do 40 i=1,41
         do 40 j=1,2
40        exposure(i,j) =  exposure(i,j)+exposure_1mf(i,j)
c
      return
      end  ! end get_exposure
c
c
c
c
c*****************************************************************
c
      subroutine assign_disc(disc, sigma_disc, disc_flag)  
c
c*****************************************************************         

c    calculate nonsectored discriminator rates
c    10/17/95  by J. Dwyer
c    
      include 'sdf_include.inc'   ! include type declarations     
c             
      real    disc(4), sigma_disc(4) ! array containing VSEBAR,START,STOP,D
      integer    disc_flag(4)  
c
      if (vsebar_flag.eq.1) then
         disc_flag(1) = 1
         disc(1) = vsebar/time_interval
         sigma_disc(1) = sqrt(1.0*vsebar)/time_interval      
      else
         disc_flag(1) = -1
      end if
c
      if (start_flag.eq.1) then
         disc_flag(2) = 1 
         disc(2) = start/time_interval
         sigma_disc(2) = sqrt(1.0*start)/time_interval 
      else
         disc_flag(2) = -1
      end if  
c 
      if (stop_flag.eq.1) then
         disc_flag(3) = 1
         disc(3) = stop/time_interval
         sigma_disc(3) = sqrt(1.0*stop)/time_interval 
      else
         disc_flag(3) = -1
      end if
c
      if (tel_flag.eq.1) then   ! telescope 1 only
         if (d1_flag.eq.1) then
            disc_flag(4) =1
            disc(4) = d1/time_interval
            sigma_disc(4) = sqrt(1.0*d1)/time_interval 
         else
             disc_flag(4) = -1       
         end if          
      end if
c
       if (tel_flag.eq.2) then   ! telescope 2 only
         if (d2_flag.eq.1) then
            disc_flag(4) = 1
            disc(4) = d2/time_interval
            sigma_disc(4) = sqrt(1.0*d2)/time_interval  
         else
             disc_flag(4) = -1      
         end if         
      end if
c
       if (tel_flag.eq.-1) then   ! telescope 1 and 2
         if ((d1_flag.eq.1).and.(d2_flag.eq.1)) then
            disc_flag(4) = 1
            disc(4) = (d1+d2)/time_interval
            sigma_disc(4) = sqrt(1.0*(d1+d2))/time_interval
         else
             disc_flag(4) = -1      
         end if    
      end if       
c
      return
      end  ! end assign_disc
c
