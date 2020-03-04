c    Subroutines for the program sdf_lister
c    
c     Contents:
c
c             subroutine  assign_sectored_fluxes         
c
c             subroutine assign_sectored_vse
c
c
c**********************************************************
c
      subroutine assign_sectored_fluxes
     1         (local_sectored_flag,
     1          local_tel_flag,
     1          flux_sect, flux_sect_sigma, flux_sect_flag)
c
c**********************************************************
c
c    calculates sectored fluxes and sigmas for 1 or both telescopes
c
c     11/10/95 by J. Dwyer
c             modified 1/8/96 by J. Dwyer divide sector exposures by 8.0
c
      include 'sdf_include.inc'   ! include type declarations
c           modified 4/22/96 corrected addition of two tel. when effic_flag = -1
c         
      real    flux_tel(2,8), sigma_tel2(2,8) ! arrays containing the fluxes and sigmas for all the elements
      real    flux_sect(8), flux_sect_sigma(8)   ! arrays containing the fluxes and sigmas for all the elements   
      integer   flux_flag_tel(2,8)   ! .true. when flux is measured and is not saturated, .false. otherwise
      integer   flux_sect_flag(8)   ! .true. when flux is measured and is not saturated, .false. otherwise      
      integer  local_tel_flag   ! 1=use tel. 1 only, 2=use tel. 1 only,-1=use tel. 1 and tel. 2
      integer   Ntel
      logical  local_sectored_flag(41)  ! .true.=include in sectored flux calculation, .false.=don't include
c     
      do 5 k=1,8          
          flux_sect_flag(k) = 0
          flux_sect(k) = 0.0       
          flux_sect_sigma(k) = 0.0
          do 5 j=1,2    
              flux_flag_tel(j,k) = 0
              flux_tel(j,k) = 0.0       
5            sigma_tel2(j,k) = 0.0
c           
      do 30 j=1,2          
         do 30 k=1,8   
            do 20 i=1,41
                if (local_sectored_flag(i)) then
                   if (mdata_flag(i,j,k).eq.-1) then   ! don't include saturated counts
                      flux_flag_tel(j,k) = -1   
                      flux_tel(j,k) = 0.0
                      goto 30
                   end if
                   if (mdata_flag(i,j,k).eq.-2) then   ! don't include saturated counts
                      flux_flag_tel(j,k) = -2   
                      flux_tel(j,k) = 0.0
                   end if
                   if (mdata_flag(i,j,k).eq.1) then  ! include only values measured in this mf
                        flux_flag_tel(j,k) = 1
                        flux_tel(j,k)=flux_tel(j,k)+
     1                             mdata(i,j,k)/(exposure(i,j)/8.0)  
                        sigma_tel2(j,k)= sigma_tel2(j,k)+
     1                   mdata(i,j,k)/(exposure(i,j)*exposure(i,j)/64.0)  ! variance                     
                   end if
                end if
20         continue   
30        continue           
c     
c           calculate total fluxes (telescope 1, 2 or 1+2)
c        **************************************
      if (local_tel_flag.eq.1) then   ! telescope 1 only
         do 100 k=1,8      
            flux_sect_flag(k) = flux_flag_tel(1,k)
            flux_sect(k) = flux_tel(1,k)
100      flux_sect_sigma(k) = sqrt(sigma_tel2(1,k))
      end if
       if (local_tel_flag.eq.2) then   ! telescope 2 only
         do 200 k=1,8      
            flux_sect_flag(k) = flux_flag_tel(2,k)
            flux_sect(k) = flux_tel(2,k)
200      flux_sect_sigma(k) = sqrt(sigma_tel2(2,k))  
      end if     
      if (local_tel_flag.eq.-1) then   ! both telescopes
         do 300 k=1,8     
           Ntel = 0 
           flux_sect_flag(k) = 0
           flux_sect(k) = 0.0
           flux_sect_sigma(k) = 0.0
            if (flux_flag_tel(1,k).eq.1) then
                Ntel  = Ntel+1
                flux_sect_flag(k) = 1
                flux_sect(k) = flux_tel(1,k)           
                flux_sect_sigma(k) = sigma_tel2(1,k)
            end if
             if (flux_flag_tel(2,k).eq.1) then
                Ntel  = Ntel+1
                flux_sect_flag(k) = 1
                flux_sect(k) = flux_sect(k)+flux_tel(2,k)           
                flux_sect_sigma(k) = flux_sect_sigma(k)+
     1              sigma_tel2(2,k)
            end if
            if (Ntel.gt.0) then
               flux_sect(k) = flux_sect(k)/Ntel
               flux_sect_sigma(k) = sqrt(flux_sect_sigma(k))/Ntel
            end if
             if ((flux_flag_tel(1,k).eq.-2).and.
     1        (flux_flag_tel(2,k).eq.-2))  then
                flux_sect(k) = 0.0
                flux_sect_flag(k) = -2         
            end if   
            if ((flux_flag_tel(1,k).eq.-1).or.
     1        (flux_flag_tel(2,k).eq.-1))  then
                flux_sect(k) = -1.0
                flux_sect_flag(k) = -1         
            end if                              
300     continue 
      end if      
c
      return
      end  ! end  assign_sectored_fluxes
c
c
c
c
c**********************************************************
c
      subroutine assign_sectored_vse
     1               (local_tel_flag, vse_sect,
     1                vse_sect_sigma, vse_sect_flag)
c
c**********************************************************
c
c    calculates sectored vse rates and sigmas for 1 or both telescopes
c
c     11/10/95 by J. Dwyer
c             modified 1/8/96 by J. Dwyer divide sector exposures by 8.0
c
      include 'sdf_include.inc'   ! include type declarations
c    
      real    delta_tvse  ! delta time over which disc rate is calculated
      real    vse_sect(8), vse_sect_sigma(8)      
      integer   vse_sect_flag(8)
      integer  local_tel_flag   ! 1=use tel. 1 only, 2=use tel. 1 only,-1=use tel. 1 and tel. 2
      logical  local_sectored_flag(41)  ! .true.=include in sectored flux calculation, .false.=don't include
c
       do 5 k=1,8 
          vse_sect(k) = 0.0
          vse_sect_sigma(k) = 0.0
5      continue  
c
      delta_tvse = time_interval   ! delta time over which disc rate is calculated	   
      if ((stateab_flag.eq.0).or.(stateab_flag.eq.1))   ! if selecting only state A or B
     1      delta_tvse = time_interval/2.0
c
c     
      do 10 k=1,8   
         if (local_tel_flag.eq.1) then   ! telescope 1 only
            vse_sect_flag(k) = vse_flag(1,k)
            vse_sect(k) = vse(1,k)/(delta_tvse/8.0)
            vse_sect_sigma(k) = sqrt(1.0*vse(1,k))/(delta_tvse/8.0)
         end if  
          if (local_tel_flag.eq.2) then   ! telescope 2 only
            vse_sect_flag(k) = vse_flag(2,k)
            vse_sect(k) = vse(2,k)/(delta_tvse/8.0)
            vse_sect_sigma(k) = sqrt(1.0*vse(2,k))/(delta_tvse/8.0)
         end if
         if (local_tel_flag.eq.-1) then   ! both telescopes
            if ((vse_flag(1,k).eq.1).and.
     1        (vse_flag(2,k).eq.1)) then
                 vse_sect_flag(k) = 1
                 vse_sect(k) = (vse(1,k)+
     1                  vse(2,k))/(delta_tvse/8.0)
                 vse_sect_sigma(k) = sqrt(1.0*
     1                    (vse(1,k)+vse(2,k)))/(delta_tvse/8.0)
            else
                vse_sect_flag(k) = -1      
            end if       
         end if
10      continue
c
      return
      end   ! end  assign_sectored_vse
c
c
c
