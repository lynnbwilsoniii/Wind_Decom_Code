c
c    Subroutines for the program Sdf_lister
c    
c     Contents:
c    
c                 subroutine  reset_phaflux
c
c                 subroutine calc_effic
c
c                 logical function fill_phaflux
c
c                 subroutine calculate_phaflux
c
c                 subroutine calculate_phacounts
c
c                 real function get_sigma_flux
c
c                 real function get_sigma_flux2
c
c                 logical function write_phaflux_output
c
c
c

c************************************************
c
        subroutine reset_phaflux(script_number)
c
c************************************************
c
c    reset phaflux arrays
c 
c     4/1/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      integer*4    i,j,k,l,j0,j1   ! loop indices
      integer*4   script_number
c
c 
      do i =1,2
        do j = 1,2
          do k = 1,2
            PHA_counts_sum(script_number,i,j,k) = 0.0
            do l = 1,Ncolumns_max
              PHA_counts(script_number,l,i,j,k) = 0.0
            end do
          end do
        end do
      end do
      do l = 1,Ncolumns_max
        PHA_Ncounts(script_number,l) = 0.0
      end do
      sumphaflux(script_number) = 0 
      
c  
      return
      end    ! end reset_phaflux
c
c
c
c
c*****************************************************
c
        subroutine calc_effic(am,az,energy,sampex_time,eff)
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
        integer*4       sampex_time     ! dummy for trigger_effic call
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
c        Cconst = 0.00428
c        Cscatt = exp(-Cconst*(az/(Energy*am))**2)
c        eff = eff*Cscatt
c
        return
        end     ! end calc_effic
c
c
c
c
c************************************************
c
        logical function fill_phaflux(script_number)
c
c************************************************
c
c     fill phaflux data
c 
c     6/17/98 by J. Dwyer
c       11/10/98 removed data_flag
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real   Energy
      integer*4   script_number
c  
      Energy = 0.0
c
      do 200 i=1, NPHA   ! loop over PHA events     
        Energy = Einc(i)   ! use K.E./nuc  to histogram                                 
          PHA_counts_sum
     1       (script_number,2-tel(i),2-ab(i),slant(i)+1) =
     1       PHA_counts_sum
     1       (script_number,2-tel(i),2-ab(i),slant(i)+1)+1.0    
          do 100 j=1, script_Nphafluxcolumns(script_number)   ! loop over PHA flux boxes                                                                    
            if ((mass(i).gt.
     1         script_phaflux_Mbins(script_number,1,j)).and.
     1         (mass(i).le.
     1         script_phaflux_Mbins(script_number,2,j)).and.
     1         (Energy.gt.
     1         script_phaflux_Ebins(script_number,1,j)).and.
     1           (Energy.le.
     1         script_phaflux_Ebins(script_number,2,j))) then                                                           
                 PHA_counts
     1           (script_number,j,2-tel(i),2-ab(i),slant(i)+1)=
     1           PHA_counts
     1           (script_number,j,2-tel(i),2-ab(i),slant(i)+1) + 
     1            1.0*PHA_data_flag(i,script_number) 
                 PHA_Ncounts(script_number,j) = 
     1             PHA_Ncounts(script_number,j) + 
     1             1.0*PHA_data_flag(i,script_number) 
                 sumphaflux(script_number) = 
     1            sumphaflux(script_number) + 
     1            1.0*PHA_data_flag(i,script_number)        
             end if
100        continue
200      continue   
c
      fill_phaflux = .true. 
      if (script_output_type(script_number).eq.4) then 
        if (sumphaflux(script_number).lt.
     1    script_idelta_Nphaf(script_number)) then
            fill_phaflux = .false. 
        end if
      end if
c 
      return
      end    ! end fill_phaflux
c
c
c
c
c************************************************
c
        subroutine calculate_phaflux(script_number,
     1      start_t, stop_t)
c
c************************************************
c
c     calculates PHA fluxes after finished accumulating
c     divides by, delta t, efficiency, delta E, and geometry factor
c
c     6/18/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      real   flux_pha(Ncolumns_max), 
     1       sigma_flux_pha(Ncolumns_max)   ! arrays containing the fluxes and sigmas
      real   flux_pha1_noslant(Ncolumns_max), 
     1        sigma_flux_pha1_noslant(Ncolumns_max)   ! tel 1, slant=0
      real   flux_pha1_slant(Ncolumns_max), 
     1        sigma_flux_pha1_slant(Ncolumns_max)   ! tel 1, slant=1
      real   flux_pha2_noslant(Ncolumns_max), 
     1        sigma_flux_pha2_noslant(Ncolumns_max)   ! tel 2, slant=0
      real   flux_pha2_slant(Ncolumns_max), 
     1        sigma_flux_pha2_slant(Ncolumns_max)   ! tel 2, slant=1
      real   flux_pha1(Ncolumns_max), 
     1       sigma_flux_pha1(Ncolumns_max)   ! tel 1
      real   flux_pha2(Ncolumns_max),
     1       sigma_flux_pha2(Ncolumns_max)   ! tel 2
      real   sigma_tempA, sigma_tempB
      real   sumcounts1_slant, sumcounts1_noslant   ! sum of all counts in tel 1, slant=0,1
      real   sumcounts2_slant, sumcounts2_noslant  ! sum of all counts in tel 2, slant=0,1
      real   PHA_exposure(Ncolumns_max,2), 
     1        ehilo(Ncolumns_max,2)      
      real    dehilo,E   ! delta E
      real    eff,eff1,eff2
      integer*4	    start_t
      integer*4	    stop_t
      integer*4   mid_time     
      integer*4   script_number

c      
      mid_time = (stop_t+start_t)/2
      do 10 i=1,script_Nphafluxcolumns(script_number)
        E= script_phaflux_Ebins(script_number,2,i)         
        call calc_effic(phaflux_m(script_number,i),
     1     phaflux_z(script_number,i),E,mid_time ,eff2)
        E= script_phaflux_Ebins(script_number,1,i)  
        call calc_effic(phaflux_m(script_number,i),
     1      phaflux_z(script_number,i),E,mid_time ,eff1)
        eff = (eff1+eff2)/2.0             
        dehilo = script_phaflux_Ebins(script_number,2,i)-
     1           script_phaflux_Ebins(script_number,1,i)                   
        PHA_exposure(i,1) = 
     1     geofactor(1)*eff*
     1          0.5*time_interval(script_number)*dehilo
        PHA_exposure(i,2) = 
     1     geofactor(2)*eff*
     1          0.5*time_interval(script_number)*dehilo                     
10      continue        
c                
         sumcounts1_noslant = 
     1     PHA_counts_sum(script_number,1,1,1)
         sumcounts2_noslant = 
     1     PHA_counts_sum(script_number,2,1,1)
         sumcounts1_slant = 
     1     PHA_counts_sum(script_number,1,1,2)+
     1     PHA_counts_sum(script_number,1,2,2)
         sumcounts2_slant = 
     1     PHA_counts_sum(script_number,2,1,2)+
     1     PHA_counts_sum(script_number,2,2,2)         
c
      do 100 i=1,script_Nphafluxcolumns(script_number)   ! loop over PHA flux boxes
         if ((sumcounts1_noslant.gt.0.0).and.
     1      (PHA_exposure(i,1).gt.0.0).and.
     1      ((PHA_slant_flag(script_number,i).eq.0).or.
     1        (PHA_slant_flag(script_number,i).eq.-1))) then
               flux_pha1_noslant(i) = 
     1           PHA_counts(script_number,i,1,1,1)*
     1       (vse1A(script_number)/sumcounts1_noslant)/
     1        PHA_exposure(i,1)
         sigma_flux_pha1_noslant(i) = get_sigma_flux2
     1    (PHA_counts(script_number,i,1,1,1),
     1     vse1A(script_number),sumcounts1_noslant)
     1     /PHA_exposure(i,1)
         else
             flux_pha1_noslant(i) = 0.0
             sigma_flux_pha1_noslant(i) = 0.0
         end if
c
         if ((sumcounts2_noslant.gt.0.0).and.
     1      (PHA_exposure(i,2).gt.0.0).and.
     1      ((PHA_slant_flag(script_number,i).eq.0).or.
     1       (PHA_slant_flag(script_number,i).eq.-1))) then
               flux_pha2_noslant(i) = 
     1           PHA_counts(script_number,i,2,1,1)*
     1       (vse2A(script_number)/sumcounts2_noslant)/
     1        PHA_exposure(i,2)
         sigma_flux_pha2_noslant(i) = get_sigma_flux2
     1    (PHA_counts(script_number,i,2,1,1),
     1     vse2A(script_number),sumcounts2_noslant)
     1     /PHA_exposure(i,2)
         else
             flux_pha2_noslant(i) = 0.0
             sigma_flux_pha2_noslant(i) = 0.0
         end if
c
         if ((sumcounts1_slant.gt.0.0).and.
     1      (PHA_exposure(i,1).gt.0.0).and.
     1      ((PHA_slant_flag(script_number,i).eq.1).or.
     1       (PHA_slant_flag(script_number,i).eq.-1))) then
               flux_pha1_slant(i) =  
     1        (PHA_counts(script_number,i,1,1,2)+
     1        PHA_counts(script_number,i,1,2,2))*
     1       (vse1B(script_number)/sumcounts1_slant)/
     1        PHA_exposure(i,1)
         sigma_flux_pha1_slant(i) = get_sigma_flux2
     1    ((PHA_counts(script_number,i,1,1,2)+
     1     PHA_counts(script_number,i,1,2,2)),
     1      vse1B(script_number),sumcounts1_slant)
     1     /PHA_exposure(i,1)
         else
             flux_pha1_slant(i) = 0.0
             sigma_flux_pha1_slant(i) = 0.0
         end if
c
         if ((sumcounts2_slant.gt.0.0).and.
     1      (PHA_exposure(i,2).gt.0.0).and.
     1      ((PHA_slant_flag(script_number,i).eq.1).or.
     1        (PHA_slant_flag(script_number,i).eq.-1))) then
               flux_pha2_slant(i) = 
     1        (PHA_counts(script_number,i,2,1,2)+
     1        PHA_counts(script_number,i,2,2,2))*
     1       (vse2B(script_number)/sumcounts2_slant)/
     1        PHA_exposure(i,2)
         sigma_flux_pha2_slant(i) = get_sigma_flux2
     1    ((PHA_counts(script_number,i,2,1,2)+
     1     PHA_counts(script_number,i,2,2,2)),
     1      vse2B(script_number),sumcounts2_slant)
     1     /PHA_exposure(i,2)
         else
             flux_pha2_slant(i) = 0.0
             sigma_flux_pha2_slant(i) = 0.0
         end if
c
c            get requested slant (0,1 or both (-1))
c          ****************************            
        if (PHA_slant_flag(script_number,i).eq.0) then   ! slant = 0 only
            flux_pha1(i) = flux_pha1_noslant(i)    
            flux_pha2(i) = flux_pha2_noslant(i)
            sigma_flux_pha1(i)=
     1             sigma_flux_pha1_noslant(i)
            sigma_flux_pha2(i)=
     1            sigma_flux_pha2_noslant(i)
         end if  ! end if slant = 0
c
         if (PHA_slant_flag(script_number,i).eq.1) then   ! slant = 1 only
            flux_pha1(i) = flux_pha1_slant(i)    
            flux_pha2(i) = flux_pha2_slant(i)
            sigma_flux_pha1(i)=
     1             sigma_flux_pha1_slant(i)
            sigma_flux_pha2(i)=
     1            sigma_flux_pha2_slant(i)
         end if  ! end if slant = 1
c
        if (PHA_slant_flag(script_number,i).eq.-1) then   ! combine slants 0 and 1
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
      if (script_tel_flag(script_number,i).eq.'TEL1') then   ! telescope 1 only              
            pha_fluxes(i) = flux_pha1(i)    
            pha_fluxes_err(i)=sigma_flux_pha1(i)              
      end if
c
       if (script_tel_flag(script_number,i).eq.'TEL2') then   ! telescope 2 only  
            pha_fluxes(i) = flux_pha2(i)    
            pha_fluxes_err(i)=sigma_flux_pha2(i)              
      end if
c
      if (script_tel_flag(script_number,i).eq.'TEL1&2') then   ! telescope 1 and 2            
             pha_fluxes(i) = 
     1            0.5*(flux_pha1(i)+flux_pha2(i))               
            pha_fluxes_err(i)=
     1      0.5*sqrt((sigma_flux_pha1(i)**2)+
     1             (sigma_flux_pha2(i)**2))                 
      end if  
100     continue     
c
      return
c
      end   ! end calculate_phaflux
c 
c
c
c
c************************************************
c
        subroutine calculate_phacounts(script_number)
c
c************************************************
c
c     calculates PHA counts after finished accumulating    
c
c     6/18/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c

      real   flux_pha(Ncolumns_max), 
     1       sigma_flux_pha(Ncolumns_max)   ! arrays containing the fluxes and sigmas
      real   flux_pha1_noslant(Ncolumns_max), 
     1        sigma_flux_pha1_noslant(Ncolumns_max)   ! tel 1, slant=0
      real   flux_pha1_slant(Ncolumns_max), 
     1        sigma_flux_pha1_slant(Ncolumns_max)   ! tel 1, slant=1
      real   flux_pha2_noslant(Ncolumns_max), 
     1        sigma_flux_pha2_noslant(Ncolumns_max)   ! tel 2, slant=0
      real   flux_pha2_slant(Ncolumns_max), 
     1        sigma_flux_pha2_slant(Ncolumns_max)   ! tel 2, slant=1
      real   flux_pha1(Ncolumns_max), 
     1       sigma_flux_pha1(Ncolumns_max)   ! tel 1
      real   flux_pha2(Ncolumns_max),
     1       sigma_flux_pha2(Ncolumns_max)   ! tel 2
      real   sigma_tempA, sigma_tempB
      real   sumcounts1_slant, sumcounts1_noslant   ! sum of all counts in tel 1, slant=0,1
      real   sumcounts2_slant, sumcounts2_noslant  ! sum of all counts in tel 2, slant=0,1
      real   PHA_exposure(Ncolumns_max,2)   
      integer*4   script_number
c      
      do 10 i=1,script_Nphafluxcolumns(script_number)                         
        PHA_exposure(i,1) = 1.0    
        PHA_exposure(i,2) = 1.0                   
10      continue        
c                
         sumcounts1_noslant = 
     1     PHA_counts_sum(script_number,1,1,1)
         sumcounts2_noslant = 
     1     PHA_counts_sum(script_number,2,1,1)
         sumcounts1_slant = 
     1     PHA_counts_sum(script_number,1,1,2)+
     1     PHA_counts_sum(script_number,1,2,2)
         sumcounts2_slant = 
     1     PHA_counts_sum(script_number,2,1,2)+
     1     PHA_counts_sum(script_number,2,2,2)         
c
      do 100 i=1,script_Nphafluxcolumns(script_number)   ! loop over PHA flux boxes
         if ((sumcounts1_noslant.gt.0.0).and.
     1      (PHA_exposure(i,1).gt.0.0).and.
     1      ((PHA_slant_flag(script_number,i).eq.0).or.
     1        (PHA_slant_flag(script_number,i).eq.-1))) then
               flux_pha1_noslant(i) = 
     1           PHA_counts(script_number,i,1,1,1)*
     1       (vse1A(script_number)/sumcounts1_noslant)/
     1        PHA_exposure(i,1)
         sigma_flux_pha1_noslant(i) = get_sigma_flux2
     1    (PHA_counts(script_number,i,1,1,1),
     1     vse1A(script_number),sumcounts1_noslant)
     1     /PHA_exposure(i,1)
         else
             flux_pha1_noslant(i) = 0.0
             sigma_flux_pha1_noslant(i) = 0.0
         end if
c
         if ((sumcounts2_noslant.gt.0.0).and.
     1      (PHA_exposure(i,2).gt.0.0).and.
     1      ((PHA_slant_flag(script_number,i).eq.0).or.
     1       (PHA_slant_flag(script_number,i).eq.-1))) then
               flux_pha2_noslant(i) = 
     1           PHA_counts(script_number,i,2,1,1)*
     1       (vse2A(script_number)/sumcounts2_noslant)/
     1        PHA_exposure(i,2)
         sigma_flux_pha2_noslant(i) = get_sigma_flux2
     1    (PHA_counts(script_number,i,2,1,1),
     1     vse2A(script_number),sumcounts2_noslant)
     1     /PHA_exposure(i,2)
         else
             flux_pha2_noslant(i) = 0.0
             sigma_flux_pha2_noslant(i) = 0.0
         end if
c
         if ((sumcounts1_slant.gt.0.0).and.
     1      (PHA_exposure(i,1).gt.0.0).and.
     1      ((PHA_slant_flag(script_number,i).eq.1).or.
     1       (PHA_slant_flag(script_number,i).eq.-1))) then
               flux_pha1_slant(i) =  
     1        (PHA_counts(script_number,i,1,1,2)+
     1        PHA_counts(script_number,i,1,2,2))*
     1       (vse1B(script_number)/sumcounts1_slant)/
     1        PHA_exposure(i,1)
         sigma_flux_pha1_slant(i) = get_sigma_flux2
     1    ((PHA_counts(script_number,i,1,1,2)+
     1     PHA_counts(script_number,i,1,2,2)),
     1      vse1B(script_number),sumcounts1_slant)
     1     /PHA_exposure(i,1)
         else
             flux_pha1_slant(i) = 0.0
             sigma_flux_pha1_slant(i) = 0.0
         end if
c
         if ((sumcounts2_slant.gt.0.0).and.
     1      (PHA_exposure(i,2).gt.0.0).and.
     1      ((PHA_slant_flag(script_number,i).eq.1).or.
     1        (PHA_slant_flag(script_number,i).eq.-1))) then
               flux_pha2_slant(i) = 
     1        (PHA_counts(script_number,i,2,1,2)+
     1        PHA_counts(script_number,i,2,2,2))*
     1       (vse2B(script_number)/sumcounts2_slant)/
     1        PHA_exposure(i,2)
         sigma_flux_pha2_slant(i) = get_sigma_flux2
     1    ((PHA_counts(script_number,i,2,1,2)+
     1     PHA_counts(script_number,i,2,2,2)),
     1      vse2B(script_number),sumcounts2_slant)
     1     /PHA_exposure(i,2)
         else
             flux_pha2_slant(i) = 0.0
             sigma_flux_pha2_slant(i) = 0.0
         end if
c
c            get requested slant (0,1 or both (-1))
c          ****************************            
        if (PHA_slant_flag(script_number,i).eq.0) then   ! slant = 0 only
            flux_pha1(i) = flux_pha1_noslant(i)    
            flux_pha2(i) = flux_pha2_noslant(i)
            sigma_flux_pha1(i)=
     1             sigma_flux_pha1_noslant(i)
            sigma_flux_pha2(i)=
     1            sigma_flux_pha2_noslant(i)
         end if  ! end if slant = 0
c
         if (PHA_slant_flag(script_number,i).eq.1) then   ! slant = 1 only
            flux_pha1(i) = flux_pha1_slant(i)    
            flux_pha2(i) = flux_pha2_slant(i)
            sigma_flux_pha1(i)=
     1             sigma_flux_pha1_slant(i)
            sigma_flux_pha2(i)=
     1            sigma_flux_pha2_slant(i)
         end if  ! end if slant = 1
c
        if (PHA_slant_flag(script_number,i).eq.-1) then   ! combine slants 0 and 1
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
      if (script_tel_flag(script_number,i).eq.'TEL1') then   ! telescope 1 only              
            pha_fluxes(i) = flux_pha1(i)    
            pha_fluxes_err(i)=sigma_flux_pha1(i)              
      end if
c
       if (script_tel_flag(script_number,i).eq.'TEL2') then   ! telescope 2 only  
            pha_fluxes(i) = flux_pha2(i)    
            pha_fluxes_err(i)=sigma_flux_pha2(i)              
      end if
c
      if (script_tel_flag(script_number,i).eq.'TEL1&2') then   ! telescope 1 and 2            
             pha_fluxes(i) = 
     1            0.5*(flux_pha1(i)+flux_pha2(i))               
            pha_fluxes_err(i)=
     1      0.5*sqrt((sigma_flux_pha1(i)**2)+
     1             (sigma_flux_pha2(i)**2))                 
      end if  
100     continue     
c
      return
c
      end   ! end calculate_phacounts
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
c************************************************
c
       logical function 
     1   write_phaflux_output(unit_out,script_number,
     1   start_t, stop_t) 
c
c************************************************
c
c    write output of PHA flux data
c 
c     4/1/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      real  doy_real
      real  half_delta_t  ! 1/2 of elapsed time interval (sec)
      real  dt       !   elapsed time interval (sec)
      integer*4	    start_t
      integer*4	    stop_t
      integer*4     unit_out        ! file unit for script file  
      integer*4     script_number    ! number of script that is being processed 
      integer*4     Npha_do   ! number of PHA events to do
      integer*4     i,j,k,l,kk    ! loop indices
      integer*4     mid_time
      integer*4     yr, mo, dy, hr, mn, se, doy
      integer*4     indexi     ! last index of outputstring written to 
      character*20   isotime   ! sampex_timecon isotime array
      character*12288   outputstring
c
      mid_time = (stop_t+start_t)/2
      half_delta_t = (stop_t-start_t)/2.0
      call sampex_timcon(mid_time,yr,mo,dy,hr,mn,se,doy,isotime)
      doy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0    
c               
      if (fluence_flag.eq.0) dt = 1.0
      if (fluence_flag.eq.1) 
     1    dt = (stop_t-start_t)
c
      indexi = 1 
      do l = 1, script_Nphafluxcolumns(script_number)       
         call write_line(indexi, outputstring, 
     1        dt*pha_fluxes(l), 
     1           .true., .false.,
     1                dataformat_flag)
          call write_line(indexi, outputstring, 
     1           dt*pha_fluxes_err(l), 
     1           .true., .false.,
     1                dataformat_flag)
      end do
      write(unit_out,100) mo, dy, yr, hr, mn, se,
     1   yr, doy_real,  
     1   half_delta_t,
     1   outputstring(1:(indexi-1))   
c
c
      Noutput_lines(script_number) = 
     1      Noutput_lines(script_number)+1
c      
c
100 	   format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1      ', 'i4', 'f12.8', 'f12.4',',a)      
c    
c    
      write_phaflux_output = .true.
      if (Noutput_lines(script_number).ge.
     1   maximum_rows) write_phaflux_output = .false.
      return
      end    ! end write_phaflux_output
c



