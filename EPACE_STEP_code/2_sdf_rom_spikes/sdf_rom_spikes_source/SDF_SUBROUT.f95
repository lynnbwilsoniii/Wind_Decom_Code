c
c    Subroutines for the program sdf_lister
c    
c     Contents:
c
c             logical function get_data_packet
c
c             integer function check_time
c
c             subroutine  reset_time
c
c             subroutine sum_kp
c
c             subroutine ave_kp
c
c             subroutine get_spin_period
c
c             subroutine write_line
c
c
c
c
c*****************************************************
c
      logical function get_data_packet(unit_in)
c
c*****************************************************
c   
c     Reads 1 mf data packet from unformatted sdf file
c     return .true. if EOF reached, .false. otherwise
c
c     11/2/95 by J. Dwyer
c        modified    2/13/97 by J. Dwyer changed kp data, added 3dp
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      integer   unit_in        ! file unit for input sdf file  
      integer   id    ! data type id 
	character*120 test_line
c
      get_data_packet = .false.  ! not at EOF
      spha_flag_1mf = .false.   ! spha kp not read
      mfi_flag_1mf = .false.   ! mfi kp not read
      swe_flag_1mf = .false.   ! swe kp not read
      tdp_flag_1mf  = .false.   ! 3dp kp not read
      epa_flag_1mf = .false.   ! epact kp not read
c

		
      read(unit_in,4000,err=700,end=10000) id
4000    format(50z8)
c
      do while (id.ne.-1)       
c        print *, id    
         if (id.eq.1) then   ! STEP data
             read(unit_in,4010,err=700,end=10000) start_time_string,
     1      start_time, end_time

4010    	format(a24,2z8)

c           print *, start_time_string, 
c     1     start_time, end_time   
             read(unit_in,4020,err=600,end=10000) 
     1      STEPHV, STEPtherm, STEPt1, STEPt2, STEPt3, MFnum  ! housekeeping
4020    	format(5(1pe12.4),z8)         
             read(unit_in,4030,err=500,end=10000)
     1        (packet(j), j=1,482), packet(733)
4030    	format(483z2)
		
            read(unit_in,4040,err=500,end=10000) NPHA
4040    format(z2)
	

            if (NPHA.gt.0) then 
               read(unit_in,4030,err=500,end=10000)
     1            ((packet(483+(i-1)*5),
     1            packet(483+(i-1)*5+1),
     1            packet(483+(i-1)*5+2),
     1            packet(483+(i-1)*5+3),
     1            packet(483+(i-1)*5+4)), i=1,NPHA)
            end if

        read(unit_in,4050,err=500,end=10000) (STEP_KP_fluxes(j), j=1,6)
4050    format(20(1pe12.4))

            goto 400   ! go to next record
         end if  ! end STEP data type
c  
         if (id.eq.2) then  ! spha kp data
            spha_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000)
     1        spha_asr
c           print *,  spha_asr
            goto 400   ! go to next record
         end if
c
         if (id.eq.3) then  ! mfi kp data
            mfi_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000) 
     1        mfi_bgse, mfi_rms, mfi_position
c            print *, mfi_bgse,mfi_rms,mfi_position
            goto 400   ! go to next record
         end if
c
         if (id.eq.4) then  ! swe kp data
            swe_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000) 
     1       swe_vgse,swe_vth,swe_protdens
c            print *, swe_vgse,swe_vth,swe_protdens
            goto 400   ! go to next record
         end if
c
         if (id.eq.5) then  ! 3dp kp data
            tdp_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000) 
     1       tdp_eflux, tdp_ionflux
c            print *, tdp_eflux, tdp_ionflux
            goto 400   ! go to next record
         end if
c
         if (id.eq.6) then  ! epact kp data
            epa_flag_1mf = .true.
            read(unit_in,4050,err=800,end=10000) 
     1         epa_apeb2, epa_apeb3,
     1         epa_apeb4, epa_apeb5,
     1         epa_lemt1, epa_lemt2,
     1         epa_lemt3
c           print *, epa_apeb1,
c     1         epa_apeb2, epa_apeb3,
c     1         epa_apeb4, epa_apeb5,
c     1         epa_lemt1, epa_lemt2,
c     1         epa_lemt3
            goto 400   ! go to next record
         end if
c
         print *, 'Unknown data type: ', id
         read(unit_in,err=800,end=10000)     ! skip data for unknown id
c
400       read(unit_in,4000,err=700,end=10000) id   ! read next record
      end do
c
      return  
c
500    print *, 'start_time_string: ', 
     1     start_time_string   ! if error come here
      print *,'Error reading packet data!'
      print *,'Stopped in function get_data_packet'
      stop
600    print *, 'start_time_string: ', 
     1     start_time_string   ! if error come here
      print *,'Error reading HK line!'
      print *,'Stopped in function get_data_packet'
      stop
700    print *, 'start_time_string: ', 
     1     start_time_string   ! if error come here
      print *,'Error reading packet time!'
      print *,'Stopped in function get_data_packet'
      stop
c
800    print *, 'start_time_string: ', 
     1     start_time_string   ! if error come here
      print *,'Error reading kp data!'
      print *,'Stopped in function get_data_packet'
      stop
c
10000  get_data_packet = .true.   ! reached end of file
      return
      end   ! end get_hex_packet
c
c
c
c
c************************************************
c
        function check_time()
c
c************************************************
c
c       Compares start time and finish time of the major frame in data file with
c       the requested start and stop time.
c
c       function returns
c                 0 = continue to skip forward
c                 1 = continue and analyze
c                 2 = stop reading and exit
c
c       10/15/95 by J. Dwyer, based on code in pha_lister  by G. Mason
c
       include 'sdf_include.inc'   ! include type declarations
       include 'hex_include.inc'   ! include type declarations
c
       integer     major_frame_length, check_time     ! duration of major frame in seconds
       check_time = 1
c
       if(start_time.lt.stime_start) check_time = 0 
	     if(start_time.ge.stime_stop)  check_time = 2
c
	     major_frame_length = end_time-start_time
	     mf_type = 1
	     if (major_frame_length.eq.92) mf_type = 2
	     if ((major_frame_length.ne.46).and.
     1     (major_frame_length.ne.92) ) then
	        print *,' Illegal major frame length at: ', start_time_string
          print *,  'major_frame_length: ',  major_frame_length
	        print *,'  Packet skipped '
          check_time = 0
       end if
c
       return
       end  ! end check_time
c
c
c
c
c************************************************
c
        subroutine  reset_time
c
c************************************************
c
c     clears all accumulated variables
c 
c     12/4/95 by J. Dwyer
c             modifications:
c                    2/6/96 by J. Dwyer added STEP_KP_fluxes time averaging
c                    2/13/97 by J. Dwyer   changed KP data 
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
c  start accumulating counts at beginning of time interval
      reset_interval = 0
      time_interval = 0.0
      time_interval_2x(1) = 0.0
      time_interval_2x(2) = 0.0
      time_interval_4x(1) = 0.0
      time_interval_4x(2) = 0.0
      time_interval_4x(3) = 0.0
      time_interval_4x(4) = 0.0
      start_time_avg = start_time
c
      do 50 i = 1, 6
        STEP_KP_fluxes_ave(i) = 0.0      
        NSTEP_KP_fluxes(i) = 0
50      continue
      Nspha = 0.0
      Nmfi(1) = 0.0 
      Nmfi(2) = 0.0
      Nswe = 0.0
      N3dp(1) = 0.0
      N3dp(2) = 0.0
      Nepa(1) = 0.0
      Nepa(2) = 0.0
      spin_rate =  0.0
      Bx = 0.0
      By = 0.0
      Bz = 0.0
      Brms = 0.0
      Bfield = 0.0
      SCx = 0.0
      SCy = 0.0
      SCz = 0.0         
      SWVx = 0.0
      SWVy = 0.0
      SWVz = 0.0
      Nproton = 0.0
      SWVth = 0.0
      do 60 i=1,7
          eflux(i)=0.0
          ionflux(i)=0.0
60      continue
c      J_apeb1 = 0.0
      J_apeb2 = 0.0   
      J_apeb3 = 0.0
      J_apeb4 = 0.0
      J_apeb5 = 0.0
      J_lemt1 = 0.0
      J_lemt2 = 0.0
      J_lemt3 = 0.0
      spha_flag = .false.
      mfi_flag(1) = .false.
      mfi_flag(2) = .false.
      swe_flag = .false.
      tdp_flag(1) = .false.
      tdp_flag(2) = .false.
      epa_flag(1) = .false.
      epa_flag(2) = .false.
c      
      vse1A = 0.0 
      vse1B = 0.0
      vse2A = 0.0
      vse2B = 0.0  
      vse_master_flag = 0
      vsebar = 0
      vsebar_flag = 0
      start = 0
      start_flag = 0
      stop = 0
      stop_flag = 0
      d1 = 0
      d1_flag = 0
      d2 = 0
      d2_flag = 0           
      do 100 j=1,2
         do 100 k=1,8
             vse(j,k) = 0.0
             vse_flag(j,k) = 0
             do 100 i=1,41                
                mdata(i,j,k) = 0.0
100          mdata_flag(i,j,k) = 0
      do 200 i=1,41 
          do 200 j=1,2
200         exposure(i,j) = 0.0
c      
      do 300 i=1,nfluxes 
         PHA_Ncounts(i) = 0.0
         do 300 j=1,2
             PHA_exposure(i,j) = 0.0
             do 300 k=1,2
                do 300 l=1,2
300        PHA_counts(i,j,k,l) = 0.0
c
      do 400 j=1,2
          do 400 k=1,2
              do 400 l=1,2
400        PHA_counts_sum(j,k,l) = 0.0    
c  
      return
      end    ! end reset_time
c
c
c
c
c************************************************
c
        subroutine sum_kp
c
c************************************************
c
c     sum key parameters
c
c     12/5/95 by J. Dwyer
c          modified  2/13/97 by J. Dwyer   changed KP data 
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c      
      real    Bmag   ! B field magnitude
      real    Bnorm   ! B field weighting factor for average direction
      real   KPweight   ! weighting factor for averaging KP values
c
      KPweight = 1.0
      if (KPweight_flag.eq.1) then  ! weight with vse
          KPweight = 0.0
          do 100, k=1,8
              do 100, j=1,2
                  if (vse_flag(j,k).eq.1) then
                      KPweight = KPweight+vse(j,k)
                  end if
100        continue                
      end if
c
      if ((spha_flag_1mf).and.
     1   (spha_asr.ge.0.0).and.
     1   (spha_asr.le.6.28319).and.
     1    (KPweight.gt.0.0)) then
         spha_flag = .true.
         spin_rate =  spin_rate+KPweight*spha_asr
         Nspha = Nspha+KPweight
      end if
c
      if ((mfi_flag_1mf).and.
     1    (KPweight.gt.0.0)) then
         if ((mfi_bgse(1).ge.-1000).and.
     1      (mfi_bgse(1).le.1000).and.
     1      (mfi_bgse(2).ge.-1000).and.
     1      (mfi_bgse(2).le.1000).and.
     1      (mfi_bgse(3).ge.-1000).and.
     1      (mfi_bgse(3).le.1000).and.
     1      (mfi_rms.ge.0).and.
     1      (mfi_rms.le.10)) then   
            Bmag = sqrt(mfi_bgse(1)*mfi_bgse(1)+ 
     1         mfi_bgse(2)*mfi_bgse(2)+
     1         mfi_bgse(3)*mfi_bgse(3)) 
            Bnorm = Bmag   ! average unit vectors
            if ((Bdir_flag.eq.0).or.(Bmag.eq.0.0)) Bnorm = 1.0  ! average B vectors
            Bfield = Bfield+KPweight*Bmag
            Bx = Bx+KPweight*mfi_bgse(1)/Bnorm   ! use cart. coords. to ave. B
            By = By+KPweight*mfi_bgse(2)/Bnorm
            Bz = Bz+KPweight*mfi_bgse(3)/Bnorm
            Brms = Brms+KPweight*mfi_rms
            mfi_flag(1) = .true.
            Nmfi(1) = Nmfi(1)+KPweight
         end if
         if ((mfi_position(1).ge.-220).and.
     1      (mfi_position(1).le.320).and.
     1      (mfi_position(2).ge.-220).and.
     1      (mfi_position(2).le.220).and.
     1      (mfi_position(3).ge.-220).and.
     1      (mfi_position(3).le.220)) then
            SCx = SCx + KPweight*mfi_position(1)
            SCy = SCy + KPweight*mfi_position(2)
            SCz = SCz + KPweight*mfi_position(3)  
            mfi_flag(2) = .true. 
            Nmfi(2) = Nmfi(2)+KPweight
         end if
      end if    
c
      if ((swe_flag_1mf).and.
     1    (KPweight.gt.0.0)) then
         if ((swe_vgse(1).ge.-1800).and.
     1      (swe_vgse(1).le.0).and.
     1      (swe_vgse(2).ge.-900).and.
     1      (swe_vgse(2).le.900).and.
     1      (swe_vgse(3).ge.-900).and.
     1      (swe_vgse(3).le.900).and.
     1      (swe_vth.ge.0).and.
     1      (swe_vth.le.500).and.
     1      (swe_protdens.ge.0).and.
     1      (swe_protdens.le.1000)) then
            swe_flag = .true.
            SWVx = SWVx + KPweight*swe_vgse(1)
            SWVy = SWVy + KPweight*swe_vgse(2)
            SWVz = SWVz + KPweight*swe_vgse(3)
            Nproton = Nproton+KPweight*swe_protdens            
            SWVth= SWVth+KPweight*swe_vth
            Nswe = Nswe+KPweight
         end if
      end if
c
      if ((tdp_flag_1mf).and.
     1    (KPweight.gt.0.0)) then
         if ((tdp_eflux(1).ge.0).and.
     1      (tdp_eflux(1).le.2.1E08).and.
     1      (tdp_eflux(2).ge.0).and.
     1      (tdp_eflux(2).le.4.0E07).and.
     1      (tdp_eflux(3).ge.0).and.
     1      (tdp_eflux(3).le.8.8E06).and.
     1      (tdp_eflux(4).ge.0).and.
     1      (tdp_eflux(4).le.2.0E06).and.
     1      (tdp_eflux(5).ge.0).and.
     1      (tdp_eflux(5).le.9000).and.
     1      (tdp_eflux(6).ge.0).and.
     1      (tdp_eflux(6).le.9000).and.
     1      (tdp_eflux(7).ge.0).and.
     1      (tdp_eflux(7).le.9000)) then
            
            do 200 i=1,7
               eflux(i)=eflux(i)+KPweight*tdp_eflux(i)
200         continue
            tdp_flag(1) = .true.
            N3dp(1) = N3dp(1)+KPweight
         end if
         if ((tdp_ionflux(1).ge.0).and.
     1      (tdp_ionflux(1).le.2.5E09).and.
     1      (tdp_ionflux(2).ge.0).and.
     1      (tdp_ionflux(2).le.4.5E08).and.
     1      (tdp_ionflux(3).ge.0).and.
     1      (tdp_ionflux(3).le.7.7E07).and.
     1      (tdp_ionflux(4).ge.0).and.
     1      (tdp_ionflux(4).le.1.3E07).and.
     1      (tdp_ionflux(5).ge.0).and.
     1      (tdp_ionflux(5).le.6600).and.
     1      (tdp_ionflux(6).ge.0).and.
     1      (tdp_ionflux(6).le.6600).and.
     1      (tdp_ionflux(7).ge.0).and.
     1      (tdp_ionflux(7).le.6600)) then
            do 300 i=1,7
               ionflux(i)=ionflux(i)+KPweight*tdp_ionflux(i)
300       continue
            tdp_flag(2) = .true.
            N3dp(2) = N3dp(2)+KPweight
         end if
      end if
c
      if ((epa_flag_1mf).and. 
     1     (KPweight.gt.0.0)) then
         if ((epa_apeb2.ge.0.00001).and.
     1   (epa_apeb2.le.100000).and.
     1   (epa_apeb3.ge.0.00001).and.
     1   (epa_apeb3.le.100000).and.
     1   (epa_apeb4.ge.0.00001).and.
     1   (epa_apeb4.le.100000).and.
     1   (epa_apeb5.ge.0.00001).and.
     1   (epa_apeb5.le.100000)) then           
c           J_apeb1 =  J_apeb1+KPweight*epa_apeb1
            J_apeb2 =  J_apeb2+KPweight*epa_apeb2
            J_apeb3 =  J_apeb3+KPweight*epa_apeb3
            J_apeb4 =  J_apeb4+KPweight*epa_apeb4
            J_apeb5 =  J_apeb5+KPweight*epa_apeb5
            epa_flag(1) = .true.
            Nepa(1) = Nepa(1)+KPweight
         end if
         if ((epa_lemt1.ge.0.00001).and.
     1   (epa_lemt1.le.100000).and.
     1   (epa_lemt2.ge.0.00001).and.
     1   (epa_lemt2.le.100000).and.
     1   (epa_lemt3.ge.0.00001).and.
     1   (epa_lemt3.le.100000)) then
            J_lemt1 =  J_lemt1+KPweight*epa_lemt1
            J_lemt2 =  J_lemt2+KPweight*epa_lemt2
            J_lemt3 =  J_lemt3+KPweight*epa_lemt3
            epa_flag(2) = .true.
            Nepa(2) = Nepa(2)+KPweight
        end if
      end if
c
      return
      end  ! end sum_kp
c
c
c
c
c************************************************
c
        subroutine ave_kp
c
c************************************************
c
c     calculates average values of key parameters
c
c     12/1/95 by J. Dwyer
c          modified  2/13/97 by J. Dwyer   changed KP data 
c          modified  3/18/97 by J. Dwyer   fixed Brms bug 
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c 
      real   Bmag
c
      if (spha_flag) then
            spin_rate =  spin_rate/Nspha
      end if
c
      if (mfi_flag(1)) then
           Bphi = -9.99E32
           Btheta = -9.99E32
           Bmag = sqrt(Bx*Bx+By*By+Bz*Bz)  
           Bfield = Bfield/Nmfi(1)
           Brms = Brms/Nmfi(1)
           if (Bmag.gt.0.0) then      
              Bx = Bx/Bmag
              By = By/Bmag
              Bz = Bz/Bmag                  
              Btheta = asind(Bz)        
              if ((Bx.ne.0.0).or.(By.ne.0.0)) then
                 Bphi = atan2d(By, Bx)
                 if (Bphi.lt.0.0) Bphi = 360.0+Bphi            
              end if
           end if
      end if
      if (mfi_flag(2)) then
           SCx = SCx/Nmfi(2)
           SCy = SCy/Nmfi(2)
           SCz = SCz/Nmfi(2)
      end if
c
      if (swe_flag) then
           SWVx = SWVx/Nswe
           SWVy = SWVy/Nswe
           SWVz = SWVz/Nswe    
           Nproton = Nproton/Nswe
           SWVth= SWVth/Nswe
      end if
c
      if (tdp_flag(1)) then
        do 100 i=1,7
          eflux(i)=eflux(i)/N3dp(1)
100      continue
      end if
      if (tdp_flag(2)) then
        do 200 i=1,7
          ionflux(i)=ionflux(i)/N3dp(2)
200      continue
      end if
c
      if (epa_flag(1)) then
c         J_apeb1 =  J_apeb1/Nepa(1)
         J_apeb2 =  J_apeb2/Nepa(1)
         J_apeb3 =  J_apeb3/Nepa(1)
         J_apeb4 =  J_apeb4/Nepa(1)
         J_apeb5 =  J_apeb5/Nepa(1)
      end if
      if (epa_flag(2)) then
         J_lemt1 =  J_lemt1/Nepa(2)
         J_lemt2 =  J_lemt2/Nepa(2)
         J_lemt3 =  J_lemt3/Nepa(2)
      end if
c
      return
      end  ! end ave_kp
c
c
c
c
c************************************************
c
        subroutine get_spin_period
c
c************************************************
c
c    assigns the correct
c    spin period using the kp data, if available
c
c    11/2/95 by J. Dwyer
c          modified  2/13/97 by J. Dwyer   changed KP data 
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c
      if ((spha_flag_1mf).and.
     1   (spha_asr.ge.0.0).and.
     1   (spha_asr.le.6.28319)) then     
         spin_period = 2.0*3.141592654/spha_asr   ! new spin_period            
      end if
c
      return
      end   ! end  get_spin_period
c
c
c
c
c************************************************
c
        subroutine write_line(index, outputstring, x, 
     1                                       xflag, nonzero_flag,
     1                                       dataformat_flag)
c
c************************************************
c
c    write real variable, x, to output string, outputstring, used to write
c    row of data to output file.
c
c    11/16/95 by J. Dwyer
c            5/6/96 changed outputstring to 4096 characters
c
      character*4096   outputstring      
      real     x   ! variable to be written to string
      integer    index     ! last index written to outputstring  
      integer   dataformat_flag    ! Output data format (0=kaleidagraph format, 1=IDL format)      
      logical    xflag   ! .true.= write x. .false.=don't write x
      logical    xnonzero  ! .true.= x.ne.0, .false.=x.eq.0
      logical    nonzero_flag  ! if .true. write only nonzero values of x. if .false. write all x values
c   
      xnonzero = .true.
      if (nonzero_flag) xnonzero = (x.ne.0.0)       
c    
      if (dataformat_flag.eq.0) then
         if ((xflag).and.(xnonzero)) then
             write(outputstring(index:(index+14)), 100)   x                 
             index = index+14
         else
             write(outputstring(index:(index+1)), 200) 
             index = index+1
         end if 
      end if 
c
      if (dataformat_flag.eq.1) then
         if (xflag) then
             write(outputstring(index:(index+14)), 100)   x                 
             index = index+14
         else
             write(outputstring(index:(index+14)), 100)  -9.99E32
             index = index+14
         end if 
      end if 
c
100     format(' 'E12.5',')
200     format(',') 

c
      return
      end   ! end  write_line
c
