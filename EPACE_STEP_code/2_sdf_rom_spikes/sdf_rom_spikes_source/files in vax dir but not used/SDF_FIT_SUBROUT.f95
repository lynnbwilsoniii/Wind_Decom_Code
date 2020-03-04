c    Subroutines for the program sdf_lister
c    
c     Contents:
c             subroutine function do_fit
c
c             subroutine mfit
c
c		
c**********************************************************
c
      subroutine do_fit(ani, sigma_ani,
     1     phi, sigma_phi, theta, sigma_theta, 
     1     chi2, fit_flag)
c
c**********************************************************
c
c    do least squares fit of spherical harminics to sectored matrix data
c
c    11/16/95 by J. Dwyer
c
      include 'sdf_include.inc'   ! include type declarations
c
      real    fudge     ! fudge factor for adjusting relative fluxes between tel 1 and 2.       
      real    flux_tel1(8), sigma_flux_tel1(8)   ! arrays containing the fluxes and sigmas
      real    flux_tel2(8), sigma_flux_tel2(8)   ! arrays containing the fluxes and sigmas
      real    y(16), yerr(16)   !  data and error to used fit
      real  phi, theta, ani   ! coords. of maximum of fitted function
      real  sigma_phi, sigma_theta, sigma_ani ! error of coords. of maximum of fitted function
      real  chi2  ! reduced chi-squared of fit
      real   p(8),cosp(8),sinp(8)  
      real  f(16,4)   ! array containing spherical harmonics
      real  a(4), sigma_a(4)  ! parameters determined by fit
      integer   flux_flag_tel1(8)   ! .true. when flux is measured and is not saturated, .false. otherwise
      integer   flux_flag_tel2(8)   ! .true. when flux is measured and is not saturated, .false. otherwise
      integer  local_tel_flag
      logical  fit_flag   ! .true.=good fit was found
      logical  local_sectored_flag(41)
c
      fudge = 0.65   ! change flux in tel 2 by this amount
      fit_flag = .true.
      cost1 = cos(73.0*3.141592654/180.0)
      cost2 = cos(107.0*3.141592654/180.0)
      sint1 = sin(73.0*3.141592654/180.0)
      sint2 = sin(107.0*3.141592654/180.0)
      do 5 i=1,8        
         p(i) = 2.0*3.141592654*(1.0+77.0/360.0-(i-0.5)/8.0)
         cosp(i) = cos(p(i))
         sinp(i) = sin(p(i))
         f(i,1) = 1.0
         f(i+8,1) = 1.0
         f(i,2) = cost1
         f(i+8,2) = cost2
         f(i,3) = cosp(i)*sint1
         f(i+8,3) = cosp(i)*sint2
         f(i,4) = sinp(i)*sint1
         f(i+8,4) = sinp(i)*sint2  
5     continue
      do 7 i=1,41
7      local_sectored_flag(i) = (He_sectored_flag(i).or.
     1      CNO_sectored_flag(i).or.Fe_sectored_flag(i))
c
      local_tel_flag = 1
      call assign_sectored_fluxes(local_sectored_flag,
     1              local_tel_flag,
     1             flux_tel1, sigma_flux_tel1, flux_flag_tel1)
c
      local_tel_flag = 2
      call assign_sectored_fluxes(local_sectored_flag,
     1              local_tel_flag, 
     1             flux_tel2, sigma_flux_tel2, flux_flag_tel2)
c
      do 10 i=1,8  
        y(i) = flux_tel1(i)
        yerr(i) = sigma_flux_tel1(i)
        y(i+8) = fudge*flux_tel2(i)
        yerr(i+8) = fudge*sigma_flux_tel2(i)
        if ((flux_flag_tel1(i).le.0).or.
     1         (flux_tel1(i).eq.0.0)) fit_flag = .false.
        if ((flux_flag_tel2(i).le.0).or.
     1         (flux_tel1(i).eq.0.0)) fit_flag = .false.
10    continue     
      if (fit_flag) 
     1    fit_flag =  mfit(y, yerr, f,
     1    a, sigma_a, chi2)
      if (.not.fit_flag) return             
c 
      if (a(1).le.0.0) then
         fit_flag = .false.
         return
      end if
c
      ani = sqrt(a(2)*a(2)+a(3)*a(3)+a(4)*a(4))/a(1)
      sigma_ani = sqrt(a(2)*a(2)*sigma_a(2)*sigma_a(2)+
     1          a(3)*a(3)*sigma_a(3)*sigma_a(3)+
     1          a(4)*a(4)*sigma_a(4)*sigma_a(4)+
     1          (ani**4)*a(1)*a(1)*sigma_a(1)*sigma_a(1))/
     1          (ani*a(1)*a(1))
c
      if ((a(3).ne.0.0).or.(a(4).ne.0.0)) then
          phi =  atan2d(a(4),a(3))
          if (phi.lt.0.0) phi=phi+360.0
          sigma_phi = sqrt(a(4)*a(4)*sigma_a(3)*sigma_a(3)+
     1          a(3)*a(3)*sigma_a(4)*sigma_a(4))/
     1          (a(4)*a(4)+a(3)*a(3))
          sigma_phi = sigma_phi*180.0/3.141592654
      else
          phi = 0.0   ! phi not defined
          sigma_phi = 0.0
      end if  
c
      c = sqrt(a(3)*a(3)+a(4)*a(4))         
      if ((c.ne.0.0).or.(a(2).ne.0.0)) then
         theta =  atan2d(c,a(2))-90.0
         sigma_theta = sqrt(c*c*sigma_a(1)*sigma_a(1)+
     1          (a(1)*a(1)/(c*c))*
     1          (a(4)*a(4)*sigma_a(4)*sigma_a(4)+
     1          a(3)*a(3)*sigma_a(3)*sigma_a(3)))/
     1          (a(1)*a(1)+c*c)
         sigma_theta = (sigma_theta*180.0/3.141592654)
      else
         theta = 0.0   ! theta not defined
         sigma_theta = 0.0
      end if  
c
      return
      end  ! end do_fit
c
c
c
c
c***********************************************
c
      logical function mfit(y, yerr, f,
     1    a, sigma_a, chi2) 
c
c***********************************************
c
c        least squares fit to 4 linear functions of dim 16 
c       11/2/95 by J. Dwyer from IDL code mfit2.pro
c
c        y(16), yerr(16) are input data and errors. 
c        f(16,n4)  are 4 functions to be fit to data 
c 
c  
      real  y(16), yerr(16),g(16)
      real  f(16,4)
      real  a(4)
      real  sigma_a(4)
      real  chi2  ! reduced chi-squared
      real  w(16), R(4,4), S(4,4), b(4)
      integer ny,nf
      logical   invert4   ! function declaration
      ny = 16
      nf = 4
c     
c    weight of each point = 1 /(yerr^2) where yerr is the 1-sigma error   
      do 10 i=1,ny   
          if (yerr(i).gt.0.0) then           
             w(i) = 1.0/(yerr(i)*yerr(i))  
          else
              mfit = .false.
              return
          end if
10    continue
c
      do 15 k=1,ny
15     g(k)=0.0
c
      do 20 i=1,nf 
         b(i) = 0.0
         a(i) = 0.0
         do 20 j=1,nf      
20       R(i,j) = 0.0        
c
      do 30 i=1,nf
        do 30 j=1,nf   
           do 30 k=1,ny        
              R(i,j) =  R(i,j)+f(k,i)*f(k,j)*w(k)
30    continue
c
      do 40 i=1,nf      
           do 40 k=1,ny        
             b(i) = b(i) + y(k)*f(k,i)*w(k)
40    continue
c
c    Inverse of R
c      call LINRG(nf,R,nf,S,nf)    ! This is an IMSL routine.  
      if (.not.invert4(R,S)) then
          print *, 'Determinant=0. No Inverse possible in mfit!'
          mfit = .false.
          return
      end if
c
      do 50 i=1,nf
        if (S(i,i).gt.0.0) then
          sigma_a(i)  = sqrt(S(i,i))  ! errors on parameters
        else
          mfit=.false.
          return
        end if
        do 50 j=1,nf 
        a(j) = a(j)+S(i,j)*b(i)      
50    continue
c
      chi2 = 0.0    
      do 200 k=1,ny
         do 100 i=1,nf
100       g(k) = g(k)+a(i)*f(k,i)
          
             chi2 = chi2+(y(k)-g(k))*
     1              (y(k)-g(k))*w(k)
200    continue 
      chi2 = chi2/(ny-nf)  ! reduced chi^2
c
      mfit = .true.
      return 
      end  ! end mfit
c
