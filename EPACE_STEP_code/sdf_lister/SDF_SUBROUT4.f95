c
c    Subroutines for the program Sdf_lister
c    
c     Contents:
c
c
c             subroutine update_ROMbox
c
c
c             logical function get_ROMbox_history
c
c
c             subroutine get_mdata
c
c
c
c
c************************************************
c
        subroutine  update_ROMbox(unit_ROM)
c
c************************************************
c
c    updates ROMbox settings
c 
c     4/10/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      integer*4   temp_ROMbox_time
      integer*4       i    ! loop variable
      integer*4       unit_ROM    ! file unit 
c
      do i = 1, Ninput_max  
          if (start_time.ge.ROMbox_time(i)) then
             current_ROMboxfile = ROMboxfile(i)
             temp_ROMbox_time = ROMbox_time(i)
         else
            goto 100
         end if 
      end do
c
100   continue
      if (temp_ROMbox_time.gt.current_ROMbox_time) then
         current_ROMbox_time = temp_ROMbox_time 
         call get_ROMboxes(unit_ROM)          
      end if
c    
      return
      end    ! end update_ROMbox
c
c
c
c
c
c
c****************************************************
c
       logical function 
     1      get_ROMbox_history(unit_ROMbox_hist)
c
c****************************************************
c
c      Get the ROM box history data from file
c
c     4/10/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
      include 'include_paths.inc'
c
      integer*4     unit_ROMbox_hist        ! file unit 
      integer*4    i   ! loop index
c
      get_ROMbox_history = .true.
c
c
      open(unit=unit_ROMbox_hist,
     1     name=adjustl(trim(calibration_path)) // ROMboxhistoryfile, status='old',     
     1    action='read',  err=600)  ! open the file
c
      do i = 1, 4
          read(unit_ROMbox_hist,*,err=700)     ! read comment line
      end do
c
      do i = 1, Ninput_max 
        read(unit_ROMbox_hist,*,err=700,end=500)   
     1    ROMbox_time(i),
     1    ROMboxfile(i)  
        if (ROMbox_time(i).eq.999999999)
     1       goto 500
      end do    
c    
500      close(unit=unit_ROMbox_hist, err=600) 
      return   ! successful
c
600      get_ROMbox_history = .false.   ! on error go to here
      type *, 'Error opening or closing ROM box hist. file:',
     1     ROMboxhistoryfile
      return
700     get_ROMbox_history = .false.   ! on error go to here
      type *, 'Error reading ROM box history file:',
     1     ROMboxhistoryfile
      close(unit=unit_ROMbox_hist, err=600) 
      return
      end   ! end get_ROMbox_history
c
c
c
c
c*****************************************************
c
      subroutine get_mdata(script_number)
c
c*****************************************************            
c
c   Takes the integer matrix counts (m1rate(302), m2rate(84) and m4rate(48)),
c   selects either state A, state B, or both, and stuffs the data into a real matrix
c   mdata(42, 2, 8) where the indices are the element, the telescope, and the
c   sector, respectively. Element 42 is the VSE rates. The following lists the order 
c   of the elements, where the number following the element is the energy bin.
c
c	1	STATE				21	CNO3	
c	2	H1				22	CNO4						
c	3	H2				23	CN05
c	4	H3				24	CNO6
c	5	H4				25	CNO7
c	6	H5				26	NeS1
c	7	H6				27	NeS2						
c	8	H7				28	NeS3
c	9	H8				29	NeS4
c	10	H9				30	NeS5
c	11	H10				31	NeS6
c	12	He1				32	NeS7
c	13	He2				33	Fe1
c	14	He3				34	Fe2
c	15	He4				35	Fe3
c	16	He5				36	Fe4
c	17	He6				37	Fe5
c	18	He7				38	Fe6
c	19	CNO1				39	JNK1
c	20	CNO2				40	JNK2
c						41	JNK3
c 						42	VSE
c
c   6/17/98 by J. Dwyer
c     11/6/98 added saturation = 0 at beginning and sat_1mf
c
      include 'Hex_include.inc'
      include 'Sdf_include.inc' 
c 
      real   mdataA(42,2,8)   ! matrix counts for state A
      real   mdataB(42,2,8)   ! matrix counts for state B 
      real   vseA(2,8)   ! disc counts for state A
      real   vseB(2,8)   ! disc counts for state A
      real   vse_1mf(2,8)  ! total counts for this mf
      real   startdoy_real, enddoy_real  ! fractional DOY for starttime and endtime of MF
c    matrix flags: 0=no measurement,1=good measurement,-1 saturated measurement
      integer*4  mdata_flagA(42,2,8)  ! matrix flag for state A
      integer*4  mdata_flagB(42,2,8)  ! matrix flag for state B    
      integer*4  startyr, endyr, mo, dy, hr, mn, se, doy
      integer*4  script_number
      integer*4  sat_1mf  ! saturation in this mf
      integer*4  i,j,k,m
      character*20   isotime   ! sampex_timecon isotime array
c      
c     
      sat_1mf = 0
      if (script_output_type(script_number).eq.2) 
     1     saturation(script_number) = 0
c
      do 50 j=1,2
         do 50 k = 1,8                     
             vseA(j,k) = 0.0
             vseB(j,k) = 0.0
             do 50 i = 1,42 
               mdata_flagA(i,j,k) = 0
               mdata_flagB(i,j,k) = 0
               mdata_flag_1mf(script_number,i,j,k) = 0
               mdata_1mf(script_number,i,j,k) = 0.0
               mdataA(i,j,k) = 0.0
50             mdataB(i,j,k) = 0.0
c

c  sectored discriminator counts 
      do 70 j=1,2
         do 70 k = 1,8
            vseA(j,k) = sdrate(k+(j-1)*16)    
            vseB(j,k) = sdrate(k+(j-1)*16+8)
70    continue
c
c                       state A 
c                 ******************
      if ((script_stateab_flag(script_number).eq.1).or.
     1   (script_stateab_flag(script_number).eq.-1))  then   ! state A or state A + B
c
c
cccccccccc 1x rates ccccccccccccc
c
c    matrix counts
      do 400 j=1,2        
        do 400 k=1, 8
           mdata_flagA(1,j,k) = 1                                              ! state flag
           mdataA(1,j,k) = m1rate((2-j)*16+(k-1)+1)                 ! state matrices
c
           do 100 i = 2,11
               mdata_flagA(i,j,k) = 1                                           ! H1-H10 flags
100         mdataA(i,j,k) = m1rate((i-2)*
     1                       16+(2-j)*8+(k-1)+33)   ! H1-H10 matrices 
c             
           mdata_flagA(12,j,k) = 1                                            ! He1 flag
           mdataA(12,j,k) = m1rate((2-j)+193)/8.0                   ! He1 matrices   
c
           do 200 i = 13,18
              mdata_flagA(i,j,k) = 1                                                    ! He2-He7 flags
        mdataA(i,j,k) = m1rate((i-13)
     1               *16+(2-j)*8+(k-1)+195)  ! He2-He7 matrices
200      continue
c
           m = 360    ! offset
           do 300 i=39, 41
              mdata_flagA(i,j,k) = 1                                                   ! junk1-junk3 flags                  
300        mdataA(i,j,k) = m1rate((i-39)*4+(2-j)*2+651-m)/8.0      ! junk1-junk3 matrices
c
400     continue  ! end j, k loop
c
cccccccccc 2x rates ccccccccccccc
      if ((phase.eq.0).or.(phase.eq.2)) then
         m = 290   ! offset
         do 500 k=1, 8            
            mdata_flagA(19,2,k) = 1                           ! CNO1 flag
            mdataA(19,2,k) =m2rate(291-m)/8.0            ! CNO1 matrix
            mdata_flagA(19,1,k) = 1                           ! CNO1 flag
            mdataA(19,1,k) =m2rate(293-m)/8.0            ! CNO1 matrix
            mdata_flagA(20,2,k) = 1                           ! CNO2 flag
            mdataA(20,2,k) =m2rate((k-1)+295-m)         ! CNO2 matrix    
            mdata_flagA(20,1,k) = 1                           ! CNO2 flag
            mdataA(20,1,k) =m2rate((k-1)+311-m)         ! CNO2 matrix    
            mdata_flagA(21,2,k) = 1                           ! CNO3 flag
            mdataA(21,2,k) =m2rate((k-1)+327-m)         ! CNO3 matrix
            mdata_flagA(21,1,k) = 1                           ! CNO3 flag
            mdataA(21,1,k) =m2rate((k-1)+343-m)         ! CNO3 matrix           
            mdata_flagA(22,2,k) = 1                           ! CNO4 flag
            mdataA(22,2,k) =m2rate((k-1)+359-m)         ! CNO4 matrix    
500      continue
      else
         m = 374   ! offset
         do 600 k=1, 8            
            mdata_flagA(22,1,k) = 1                           ! CNO4 flag  
            mdataA(22,1,k) =m2rate((k-1)+375-m)         ! CNO4 matrix 
            mdata_flagA(23,2,k) = 1                           ! CNO5 flag 
            mdataA(23,2,k) =m2rate((k-1)+391-m)         ! CNO5 matrix
            mdata_flagA(23,1,k) = 1                           ! CNO5 flag 
            mdataA(23,1,k) =m2rate((k-1)+407-m)         ! CNO5 matrix 
            mdata_flagA(24,2,k) = 1                           ! CNO6 flag
            mdataA(24,2,k) =m2rate((k-1)+423-m)         ! CNO6 matrix 
            mdata_flagA(24,1,k) = 1                           ! CNO6 flag
            mdataA(24,1,k) =m2rate((k-1)+439-m)         ! CNO6 matrix 
            mdata_flagA(25,2,k) = 1                            ! CNO7 flag
            mdataA(25,2,k) =m2rate(455-m)/8.0             ! CNO7 matrix 
            mdata_flagA(25,1,k) = 1                            ! CNO7 flag
            mdataA(25,1,k) =m2rate(457-m)/8.0             ! CNO7 matrix      
600      continue
      endif
c
cccccccccc 4x rates ccccccccccccc
      if (phase.eq.0) then
        m = 458   ! offset
        do 800 k=1, 8
           do 700 i=26, 33
             do 700 j=1, 2
                mdata_flagA(i,j,k) = 1                             ! NeS1-NeS7, Fe1 flags                  
700          mdataA(i,j,k) = m4rate((i-26)*4+
     1               (2-j)*2+459-m)/8.0      ! NeS1-NeS7, Fe1 matrices     
             mdata_flagA(34,2,k) =1                             !  Fe2 flag
800       mdataA(34,2,k) = m4rate((k-1)+491-m)         !  Fe2 matrix
      end if 
c
      if (phase.eq.1) then
           m = 506   ! offset
           do 900 k=1, 8 
               mdata_flagA(34,1,k) = 1                          !  Fe2 flag
               mdataA(34,1,k) = m4rate((k-1)+507-m)       !  Fe2 matrix
               mdata_flagA(35,2,k) = 1                          !  Fe3 flag
               mdataA(35,2,k) = m4rate((k-1)+523-m)       !  Fe3 matrix
               mdata_flagA(35,1,k) = 1                          !  Fe3 flag
900         mdataA(35,1,k) = m4rate((k-1)+539-m)       !  Fe3 matrix
      end if    
      if (phase.eq.2) then
           m = 554   ! offset
           do 1000 k=1, 8 
               mdata_flagA(36,2,k) = 1                         !  Fe4 flag
               mdataA(36,2,k) = m4rate((k-1)+555-m)      !  Fe4 matrix
               mdata_flagA(36,1,k) = 1                         !  Fe4 flag
               mdataA(36,1,k) = m4rate((k-1)+571-m)      !  Fe4 matrix
               mdata_flagA(37,2,k) = 1                         !  Fe5 flag
1000       mdataA(37,2,k) = m4rate((k-1)+587-m)      !  Fe5 matrix
      end if 
      if (phase.eq.3) then
            m = 602   ! offset
            do 1050 k=1, 8 
               mdata_flagA(37,1,k) = 1                         !  Fe5 flag
               mdataA(37,1,k) = m4rate((k-1)+603-m)      !  Fe5 matrix
               mdata_flagA(38,1,k) = 1                          !  Fe6 flag
               mdataA(38,1,k) = m4rate((k-1)+619-m)      !  Fe6 matrix
               mdata_flagA(38,2,k) = 1                          !  Fe6 flag
1050       mdataA(38,2,k) = m4rate((k-1)+635-m)      !  Fe6 matrix
      end if     
c
      end if  ! end if state A
c
c                     state B only
c                 ******************
        if ((script_stateab_flag(script_number).eq.0).or.
     1   (script_stateab_flag(script_number).eq.-1))  then   ! state B or state A + B
c
cccccccccc 1x rates ccccccccccccc
c
c    matrix counts
      do 1400 j=1,2        
        do 1400 k=1, 8
           mdata_flagB(1,j,k) = 1                                              ! state flag
           mdataB(1,j,k) = m1rate((2-j)*16+(k-1)+9)                           ! state matrices
c          
           m = 360    ! offset
           do 1300 i=39, 41
              mdata_flagB(i,j,k) = 1                                                   ! junk1-junk3 flags                  
1300        mdataB(i,j,k) = m1rate((i-39)*4+(2-j)
     1                   *2+652-m)/8.0     ! junk1-junk3 matrices
c
1400     continue  ! end j, k loop
c
cccccccccc 2x rates ccccccccccccc
      if ((phase.eq.0).or.(phase.eq.2)) then
         m = 290   ! offset
         do 1500 k=1, 8
            mdata_flagB(19,2,k) = 1                           ! CNO1 flag
            mdataB(19,2,k) =m2rate(292-m)/8.0            ! CNO1 matrix
            mdata_flagB(19,1,k) = 1                           ! CNO1 flag
            mdataB(19,1,k) =m2rate(294-m)/8.0            ! CNO1 matrix
            mdata_flagB(20,2,k) = 1                           ! CNO2 flag
            mdataB(20,2,k) =m2rate((k-1)+303-m)         ! CNO2 matrix    
            mdata_flagB(20,1,k) = 1                           ! CNO2 flag
            mdataB(20,1,k) =m2rate((k-1)+319-m)         ! CNO2 matrix    
            mdata_flagB(21,2,k) = 1                           ! CNO3 flag
            mdataB(21,2,k) =m2rate((k-1)+335-m)         ! CNO3 matrix
            mdata_flagB(21,1,k) = 1                           ! CNO3 flag
            mdataB(21,1,k) =m2rate((k-1)+351-m)         ! CNO3 matrix           
            mdata_flagB(22,2,k) = 1                           ! CNO4 flag
            mdataB(22,2,k) =m2rate((k-1)+367-m)         ! CNO4 matrix    
1500      continue
      else
         m = 374   ! offset
         do 1600 k=1, 8 
            mdata_flagB(22,1,k) = 1                           ! CNO4 flag  
            mdataB(22,1,k) =m2rate((k-1)+383-m)         ! CNO4 matrix 
            mdata_flagB(23,2,k) = 1                           ! CNO5 flag 
            mdataB(23,2,k) =m2rate((k-1)+399-m)         ! CNO5 matrix
            mdata_flagB(23,1,k) = 1                           ! CNO5 flag 
            mdataB(23,1,k) =m2rate((k-1)+415-m)         ! CNO5 matrix 
            mdata_flagB(24,2,k) = 1                           ! CNO6 flag
            mdataB(24,2,k) =m2rate((k-1)+431-m)         ! CNO6 matrix 
            mdata_flagB(24,1,k) = 1                           ! CNO6 flag
            mdataB(24,1,k) =m2rate((k-1)+447-m)         ! CNO6 matrix 
            mdata_flagB(25,2,k) = 1                            ! CNO7 flag
            mdataB(25,2,k) =m2rate(456-m)/8.0             ! CNO7 matrix 
            mdata_flagB(25,1,k) = 1                            ! CNO7 flag
            mdataB(25,1,k) =m2rate(458-m)/8.0             ! CNO7 matrix      
1600      continue
      endif
c
cccccccccc 4x rates ccccccccccccc
      if (phase.eq.0) then
        m = 458   ! offset
        do 1800 k=1, 8
           do 1700 i=26, 33
             do 1700 j=1, 2
                mdata_flagB(i,j,k) = 1                             ! NeS1-NeS7, Fe1 flags                  
1700          mdataB(i,j,k) = m4rate((i-26)*4+
     1                (2-j)*2+460-m)/8.0         ! NeS1-NeS7, Fe1 matrices     
             mdata_flagB(34,2,k) =1                             !  Fe2 flag
1800       mdataB(34,2,k) = m4rate((k-1)+499-m)         !  Fe2 matrix
      end if 
c
      if (phase.eq.1) then
           m = 506   ! offset
           do 1900 k=1, 8 
               mdata_flagB(34,1,k) = 1                          !  Fe2 flag
               mdataB(34,1,k) = m4rate((k-1)+515-m)       !  Fe2 matrix
               mdata_flagB(35,2,k) = 1                          !  Fe3 flag
               mdataB(35,2,k) = m4rate((k-1)+531-m)       !  Fe3 matrix
               mdata_flagB(35,1,k) = 1                          !  Fe3 flag
1900         mdataB(35,1,k) =m4rate((k-1)+547-m)       !  Fe3 matrix
      end if    
      if (phase.eq.2) then
           m = 554   ! offset
           do 2000 k=1, 8 
               mdata_flagB(36,2,k) = 1                         !  Fe4 flag
               mdataB(36,2,k) = m4rate((k-1)+563-m)      !  Fe4 matrix
               mdata_flagB(36,1,k) = 1                         !  Fe4 flag
               mdataB(36,1,k) = m4rate((k-1)+579-m)      !  Fe4 matrix
               mdata_flagB(37,2,k) = 1                         !  Fe5 flag
2000       mdataB(37,2,k) = m4rate((k-1)+595-m)      !  Fe5 matrix
      end if 
      if (phase.eq.3) then
           m = 602   ! offset
            do 2050 k=1, 8 
               mdata_flagB(37,1,k) = 1                         !  Fe5 flag
               mdataB(37,1,k) = m4rate((k-1)+611-m)      !  Fe5 matrix
               mdata_flagB(38,1,k) = 1                          !  Fe6 flag
               mdataB(38,1,k) = m4rate((k-1)+627-m)      !  Fe6 matrix
               mdata_flagB(38,2,k) = 1                          !  Fe6 flag
2050       mdataB(38,2,k) = m4rate((k-1)+643-m)      !  Fe6 matrix
      end if     
c
      end if  ! end if state B
c                  
c            get requested state (A,B or A+B)
c          ****************************
      if (script_stateab_flag(script_number).eq.1) then   ! state A only 
c         
         do 2100 j=1,2
           do 2100 k = 1,8    
             if (vseA(j,k).ge.0.0) then
               mdata_flag_1mf(script_number,42,j,k) = 1
               mdata_1mf(script_number,42,j,k) = vseA(j,k)   
             else
              mdata_flag_1mf(script_number,42,j,k) = -1
              mdata_1mf(script_number,42,j,k) = 0.0
             end if
             do i = 1,41
               if (mdataA(i,j,k).ge.0.0) then                     
                 mdata_flag_1mf(script_number,i,j,k) = 
     1            mdata_flagA(i,j,k)
                 mdata_1mf(script_number,i,j,k) = 
     1            mdataA(i,j,k)
               else                  
                mdata_flag_1mf(script_number,i,j,k) = -1
                mdata_1mf(script_number,i,j,k) = 0.0
               end if
            end do
2100     continue
      end if  ! end if state A 
c
      if (script_stateab_flag(script_number).eq.0) then   ! state B only 
         do 2200 j=1,2
           do 2200 k = 1,8   
             if (vseB(j,k).ge.0.0) then
               mdata_flag_1mf(script_number,42,j,k) = 1
               mdata_1mf(script_number,42,j,k) = vseB(j,k)   
             else 
               mdata_flag_1mf(script_number,42,j,k) = -1
               mdata_1mf(script_number,42,j,k) = 0.0
             end if                     
             do i = 1,41
               if (mdataB(i,j,k).ge.0.0) then                     
                 mdata_flag_1mf(script_number,i,j,k) =
     1              mdata_flagB(i,j,k)
                 mdata_1mf(script_number,i,j,k) = mdataB(i,j,k)
               else
                 mdata_flag_1mf(script_number,i,j,k) = -1
                 mdata_1mf(script_number,i,j,k) = 0.0
               end if
           end do
2200     continue
      end if  ! end if state B 
c
      if (script_stateab_flag(script_number).eq.-1) then   ! combine state A and B 
         do 2300 j=1,2
           do 2300 k = 1,8 
             if ((vseA(j,k).ge.0.0).and.
     1        (vseB(j,k).ge.0.0)) then
                mdata_flag_1mf(script_number,42,j,k) = 1
                mdata_1mf(script_number,42,j,k) = 
     1             vseA(j,k)+vseB(j,k)   
             else
                mdata_flag_1mf(script_number,42,j,k) = -1
                mdata_1mf(script_number,42,j,k) = 0.0
             end if                       
             do i = 1,41
               if ((mdataA(i,j,k).ge.0.0).and.
     1           (mdataB(i,j,k).ge.0.0)) then
                 if ((mdata_flagA(i,j,k).eq.1).or.
     1              (mdata_flagB(i,j,k).eq.1)) then
                    mdata_flag_1mf(script_number,i,j,k) = 1
                    mdata_1mf(script_number,i,j,k) = 
     1                   mdataA(i,j,k) + mdataB(i,j,k)
                 end if
               else
                  mdata_flag_1mf(script_number,i,j,k) = -1
                  mdata_1mf(script_number,i,j,k) = 0.0
               end if
            end do
2300     continue
      end if  ! end if state A and B   
c       
c                        combine with accumulated counts 
c                         *****************************
      do 2400 j=1,2
        do 2400 k = 1,8                                       
          do i = 1,42
            if ((mdata_flag_1mf(script_number,i,j,k).eq.-1).or.
     1         (mdata_flag(script_number,i,j,k).eq.-1)) then
               mdata_flag(script_number,i,j,k) = -1
               mdata(script_number,i,j,k) = 0.0
            else
              if ((mdata_flag_1mf(script_number,i,j,k).eq.1).or.
     1           (mdata_flag(script_number,i,j,k).eq.1)) then
                 mdata(script_number,i,j,k) = 
     1             mdata(script_number,i,j,k) +
     1             mdata_1mf(script_number,i,j,k)
                 mdata_flag(script_number,i,j,k) = 1
              end if 
            end if
          end do      
2400       continue 


c
      do 2500 i = 1,41    
        do 2500 j=1,2
          do 2500 k = 1,8    
            if (.not.effic_flag(i,j)) then   
              mdata_flag(script_number,i,j,k) = -2    ! don't use this ROM box
              mdata(script_number,i,j,k) = 0.0  
            end if
2500      continue
c     
c          combine with accumulated nonsectored discriminator counts
c            ********************************************
      if ((vsebar_flag(script_number).eq.-1).or.
     1   (vsebar_1mf.lt.0))  then
         vsebar(script_number) = 0
         vsebar_flag(script_number) = -1
      else
         vsebar(script_number) = 
     1   vsebar(script_number)+vsebar_1mf
         vsebar_flag(script_number) = 1
      end if
c
       if ((start_flag(script_number).eq.-1).or.
     1   (start_1mf.lt.0))  then
         start(script_number) = 0
         start_flag(script_number) = -1
      else
         start(script_number) =
     1    start(script_number)+start_1mf
         start_flag(script_number) = 1
      end if
c
       if ((stop_flag(script_number).eq.-1).or.
     1   (stop_1mf.lt.0))  then
         stop(script_number) = 0
         stop_flag(script_number) = -1
      else
         stop(script_number) = 
     1    stop(script_number)+stop_1mf
         stop_flag(script_number) = 1
      end if
c
      if ((d1_flag(script_number).eq.-1).or.
     1   (d1_1mf.lt.0))  then
         d1(script_number) = 0
         d1_flag(script_number) = -1
      else
         d1(script_number) = 
     1     d1(script_number)+d1_1mf
         d1_flag(script_number) = 1
      end if
c
       if ((d2_flag(script_number).eq.-1).or.
     1   (d2_1mf.lt.0))  then
         d2(script_number) = 0
         d2_flag(script_number) = -1
      else
         d2(script_number) = 
     1     d2(script_number)+d2_1mf
         d2_flag(script_number) = 1
      end if
c    
      do 3000 k = 1,8
       if ((vse_master_flag(script_number).eq.-1).or.
     1      (vseA(1,k).lt.0.0).or.
     1      (vseA(2,k).lt.0.0).or.
     1      (vseB(1,k).lt.0.0).or.
     1      (vseB(2,k).lt.0.0)) then
             vse1A(script_number) = 0.0 
             vse1B(script_number) = 0.0
             vse2A(script_number) = 0.0
             vse2B(script_number) = 0.0  
             vse_master_flag(script_number) = -1
        else
             vse1A(script_number) = 
     1         vse1A(script_number)+vseA(1,k)
             vse1B(script_number) = 
     1         vse1B(script_number)+vseB(1,k)
             vse2A(script_number) = 
     1         vse2A(script_number)+vseA(2,k)
             vse2B(script_number) = 
     1         vse2B(script_number)+vseB(2,k)
             vse_master_flag(script_number) = 1
         end if
3000      continue
c 
       Nr_counts(script_number) = 0
       if (accum_rate_which(script_number).eq.1) then
          do j = 1, 8
           Nr_counts(script_number)  = 
     1       Nr_counts(script_number) +
     1       mdata
     1        (script_number,
     1        accum_rate_index(script_number),1,j)+
     1       mdata
     1        (script_number,
     1        accum_rate_index(script_number),2,j)
           end do
       end if
      if (accum_rate_which(script_number).eq.2) then
         if (accum_rate_index(script_number).eq.1) 
     1       Nr_counts(script_number)  = 
     1           vsebar(script_number) 
        if (accum_rate_index(script_number).eq.2) 
     1       Nr_counts(script_number)  = 
     1           start(script_number)
        if (accum_rate_index(script_number).eq.3) 
     1       Nr_counts(script_number)  = 
     1           stop(script_number)
       end if
      if( accum_rate_which(script_number).eq.3) then          
           Nr_counts(script_number)  = 
     1       d1(script_number) +
     1       d2(script_number)
       end if
c
      do 3500 j=1,2
        do 3500 k = 1,8                            
          do 3500 i = 1,42
            if (mdata_flag_1mf(script_number,i,j,k).eq.-1)
     1          sat_1mf = 1
3500      continue
      if (d1_1mf.lt.0) 
     1    sat_1mf = 1
      if (d2_1mf.lt.0) 
     1    sat_1mf = 1
      if (stop_1mf.lt.0) 
     1   sat_1mf = 1
      if (start_1mf.lt.0) 
     1   sat_1mf = 1
      if (vsebar_1mf.lt.0) 
     1   sat_1mf = 1
c
      if (sat_1mf.eq.1) then
      call sampex_timcon
     1  (start_time,startyr,mo,dy,hr,mn,se,doy,isotime)
      startdoy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0
      call sampex_timcon
     1  (end_time,endyr,mo,dy,hr,mn,se,doy,isotime)
      enddoy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0
      type *, 'Saturation within MF between:'
      type *, startyr,'  ', startdoy_real, '     ', 
     1            endyr,'  ', enddoy_real
      end if
      saturation(script_number) = 
     1     saturation(script_number) + sat_1mf
c
      return
      end  ! end get_mdata
c
