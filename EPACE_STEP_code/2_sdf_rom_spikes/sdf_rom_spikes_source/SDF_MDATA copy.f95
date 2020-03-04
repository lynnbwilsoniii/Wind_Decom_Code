c    Subroutines for the program sdf_lister
c    
c     Contents:
c            subroutine  get_mdata
c
c
c
c
c*****************************************************
c
      subroutine get_mdata
c
c*****************************************************            
c
c   Takes the integer matrix counts (m1rate(302), m2rate(84) and m4rate(48)),
c   selects either state A, state B, or both, and stuffs the data into a real matrix
c   mdata(41, 2, 8) where the indices are the element, the telescope, and the
c   sector, respectively.  The following lits the order of the elements, where the
c   number following the element is the energy bin.
c
c	1	STATE	21	CNO3	
c	2	H1				22	CNO4						
c	3	H2				23	CN05
c	4	H3				24	CNO6
c	5	H4				25	CNO7
c	6	H5				26	NeS1
c	7	H6				27	NeS2						
c	8	H7				28	NeS3
c	9	H8				29	NeS4
c	10	H9			30	NeS5
c	11	H10		31	NeS6
c	12	He1		32	NeS7
c	13	He2		33	Fe1
c	14	He3		34	Fe2
c	15	He4		35	Fe3
c	16	He5		36	Fe4
c	17	He6		37	Fe5
c	18	He7		38	Fe6
c	19	CNO1	39	JNK1
c	20	CNO2	40	JNK2
c									41	JNK3
c
c   12/4/95 by J. Dwyer
c        modification:
c             2/26/96 by J. Dwyer mistake in state rates
c             7/18/96 by J. Dwyer added saturation warning output
c		9/6/96 to include tests for spikes & write out 
c			spike and saturation file /gm
c		9/9/96 change output file to "mask_saturation.tmp" /gm
c		3/20/97 re-link with new sdf_lister.f95; add scr calc/gm

      include 'hex_include.inc'   ! include type declarations
      include 'sdf_include.inc'   ! include type declarations
c 
	integer saturation_buf(8)
	real test_sum(41) ! looks for large tel values
	byte timew(8),datew(9)
	logical satspike
	integer start_time_last,end_time_last
      real   mdataA(41,2,8)   ! matrix counts for state A
      real   mdataB(41,2,8)   ! matrix counts for state B 
      real   vseA(2,8)   ! disc counts for state A
      real   vseB(2,8)   ! disc counts for state A
      real   vse_1mf(2,8)  ! total counts for this mf
      real   startdoy_real, enddoy_real  ! fractional DOY for starttime and endtime of MF
c    matrix flags: 0=no measurement,1=good measurement,-1 saturated measurement
      integer  mdata_flagA(41,2,8)  ! matrix flag for state A
      integer  mdata_flagB(41,2,8)  ! matrix flag for state B 
      integer  vse_flag_1mf(2,8)  ! vse flag for this mf    
      integer      saturation_flag   ! 1 if anything is saturated in mf, 0 otherwise
      integer   startyr, endyr, mo, dy, hr, mn, se, doy
      character*20   isotime   ! sampex_timecon isotime array
      character*120	test_filename, filename, filename_last
      logical	file_exist

c**********************************************************************
	data ifirst/0/, satspike/.false./
	if(ifirst.eq.1) goto 40
	ifirst=1
	do 1498 iversion=999,1,-1
	if( (iversion.ge.100) .and. (iversion.le.999) ) then
	write(test_filename,505)
     *    '/Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/'
     *    ,'sdf_rom_spikes.kal',iversion
           endif
           if( (iversion.ge.10) .and. (iversion.le.99) ) then
	write(test_filename,510)
     *    'sdf_rom_spikes.kal',iversion
           endif 	
	if(iversion.lt.10) write(test_filename,520)
     *    '/Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/'
     *    ,'sdf_rom_spikes.kal',iversion
	inquire(file=test_filename,named=file_exist)
	if(file_exist) goto 1497
	filename_last=test_filename
1498	continue


c      write(filename, '(a8,a60)')
c    1     script_file_prefix(script_number), outputfile

1497		      filename=filename_last
	type *, ' opening: ', filename
	open(unit=77,name=filename,status='new',recl=32000)
	write(77,38) (i,i=1,41)
38	format(' SAMPEX time,year,day of year,'
     *    '# mf(1),# mf(2),Dist,Xgse,Ygse,Zgse,',
     *    'VSE,State rate sum,M box sum,|VSE-mbox|,Mbox sum/VSE,',
     *    41('Rate'i3','))
     
c	**********************************     
	do 1499 iversion=999,1,-1
	if( (iversion.ge.100) .and. (iversion.le.999) ) then
	write(test_filename,505) '/Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/'
     *    ,'mask_saturation.tmp',iversion
           endif
           if( (iversion.ge.10) .and. (iversion.le.99) ) then
	write(test_filename,510) '/Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/'
     *    ,'mask_saturation.tmp',iversion
	endif
	if(iversion.lt.10) write(test_filename,520) '/Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/'
     *    ,'mask_saturation.tmp',iversion
505	format(a,a,";",i3)
510	format(a,a,";",i2)
520	format(a,a,";",i1)
	inquire(file=test_filename,named=file_exist)
	if(file_exist) goto 550
	filename_last=test_filename
1499	continue


c      write(filename, '(a8,a60)')
c    1     script_file_prefix(script_number), outputfile

550		      filename=filename_last
		open(unit=78,name=filename,
     1     status='NEW',RECL=32766, err=1000) ! open output data file
      type *, 'Opened output file: ', adjustl(trim(filename))


     
     
c	**********************************
	call date(datew)
	call time(timew)
	write(78,39) datew,timew
39	format('c ******* Saturation Mask Time Interval File *******',
     *   ///,'c Time intervals that contain saturations in either ',
     *     'a ROM box or disc rate'//' Run time: '9a1,2x,8a1,////
     *  ' Start year,Start Day,End Year,End Day,VSE,Matrix data,',
     *  'D1,D2,Stop,Start,VSEbar,ROM spike,')


40	continue
c**********************************************************************

c      
      saturation_flag = 0
	if(satspike) goto 49   ! don't clear buffers if still in
	do 48 i=1,8	       ! a spike interval!
48	saturation_buf(i)=0
c     
49      do 50 j=1,2
         do 50 k = 1,8                     
             vseA(j,k) = 0.0
             vseB(j,k) = 0.0
             do 50 i = 1,41 
                 mdata_flagA(i,j,k) = 0
                 mdata_flagB(i,j,k) = 0
                 mdata_flag_1mf(i,j,k) = 0
                 mdata_1mf(i,j,k) = 0.0
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
c                            state A 
c                 ******************
      if ((stateab_flag.eq.1).or.
     1   (stateab_flag.eq.-1))  then   ! state A or state A + B
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
     1                                       (2-j)*2+459-m)/8.0      ! NeS1-NeS7, Fe1 matrices     
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
c                         state B only
c                 ******************
        if ((stateab_flag.eq.0).or.
     1   (stateab_flag.eq.-1))  then   ! state B or state A + B
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
     1                                       (2-j)*2+460-m)/8.0      ! NeS1-NeS7, Fe1 matrices     
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
      if (stateab_flag.eq.1) then   ! state A only 
c         
         do 2100 j=1,2
            do 2100 k = 1,8    
               if (vseA(j,k).ge.0.0) then
                  vse_flag_1mf(j,k) = 1
                  vse_1mf(j,k) = vseA(j,k)   
               else
                  vse_flag_1mf(j,k) = -1
                  vse_1mf(j,k) = 0.0
               end if
               do 2100 i = 1,41
                  if (mdataA(i,j,k).ge.0.0) then                     
                      mdata_flag_1mf(i,j,k) = mdata_flagA(i,j,k)
                      mdata_1mf(i,j,k) = mdataA(i,j,k)
                  else                  
                      mdata_flag_1mf(i,j,k) = -1
                      mdata_1mf(i,j,k) = 0.0
                  end if
2100     continue
      end if  ! end if state A 
c
      if (stateab_flag.eq.0) then   ! state B only 
         do 2200 j=1,2
            do 2200 k = 1,8   
               if (vseB(j,k).ge.0.0) then
                  vse_flag_1mf(j,k) = 1
                  vse_1mf(j,k) = vseB(j,k)   
               else 
                  vse_flag_1mf(j,k) = -1
                  vse_1mf(j,k) = 0.0
               end if                     
               do 2200 i = 1,41
                  if (mdataB(i,j,k).ge.0.0) then                     
                      mdata_flag_1mf(i,j,k) = mdata_flagB(i,j,k)
                      mdata_1mf(i,j,k) = mdataB(i,j,k)
                  else
                      mdata_flag_1mf(i,j,k) = -1
                      mdata_1mf(i,j,k) = 0.0
                  end if
2200     continue
      end if  ! end if state B 
c
      if (stateab_flag.eq.-1) then   ! combine state A and B 
         do 2300 j=1,2
            do 2300 k = 1,8 
               if ((vseA(j,k).ge.0.0).and.
     1        (vseB(j,k).ge.0.0)) then
                  vse_flag_1mf(j,k) = 1
                  vse_1mf(j,k) = vseA(j,k)+vseB(j,k)   
               else
                  vse_flag_1mf(j,k) = -1
                  vse_1mf(j,k) = 0.0
               end if                       
               do 2300 i = 1,41
                  if ((mdataA(i,j,k).ge.0.0).and.
     1           (mdataB(i,j,k).ge.0.0)) then
                      if ((mdata_flagA(i,j,k).eq.1).or.
     1               (mdata_flagB(i,j,k).eq.1)) then
                         mdata_flag_1mf(i,j,k) = 1
                         mdata_1mf(i,j,k) = mdataA(i,j,k)
     1                                       +mdataB(i,j,k)
                      end if
                  else
                      mdata_flag_1mf(i,j,k) = -1
                      mdata_1mf(i,j,k) = 0.0
                  end if
2300     continue
      end if  ! end if state A and B   
c       
c                            combine with accumulated counts 
c                         *****************************
      do 2400 j=1,2
          do 2400 k = 1,8     
              if ((vse_flag(j,k).eq.-1).or.
     1        (vse_flag_1mf(j,k).eq.-1)) then
                  vse_flag(j,k) = -1
                  vse(j,k) = 0.0  
               else                
                  if ((vse_flag(j,k).eq.1).or.
     1              (vse_flag_1mf(j,k).eq.1)) then
                         vse(j,k) = vse(j,k)+vse_1mf(j,k)
                         vse_flag(j,k) = 1
                   end if
               end if                               
               do 2400 i = 1,41
                  if ((mdata_flag_1mf(i,j,k).eq.-1).or.
     1               (mdata_flag(i,j,k).eq.-1)) then
                      mdata_flag(i,j,k) = -1
                      mdata(i,j,k) = 0.0
                  else
                     if ((mdata_flag_1mf(i,j,k).eq.1).or.
     1               (mdata_flag(i,j,k).eq.1)) then
                         mdata(i,j,k) = 
     1                  mdata(i,j,k)+mdata_1mf(i,j,k)
                         mdata_flag(i,j,k) = 1
                     end if 
                  end if
2400       continue 
c
      do 2500 i = 1,41    
         do 2500 j=1,2
            do 2500 k = 1,8    
                 if (.not.effic_flag(i,j)) then   
                     mdata_flag(i,j,k) = -2    ! don't use this ROM box
                     mdata(i,j,k) = 0.0  
                 end if
2500      continue
c     
c          combine with accumulated nonsectored discriminator counts
c            ********************************************
      if ((vsebar_flag.eq.-1).or.
     1   (vsebar_1mf.lt.0))  then
           vsebar = 0
           vsebar_flag = -1
      else
           vsebar = vsebar+vsebar_1mf
           vsebar_flag = 1
      end if
c
       if ((start_flag.eq.-1).or.
     1   (start_1mf.lt.0))  then
           start = 0
           start_flag = -1
      else
           start = start+start_1mf
           start_flag = 1
      end if
c
       if ((stop_flag.eq.-1).or.
     1   (stop_1mf.lt.0))  then
           stop = 0
           stop_flag = -1
      else
           stop = stop+stop_1mf
           stop_flag = 1
      end if
c
      if ((d1_flag.eq.-1).or.
     1   (d1_1mf.lt.0))  then
           d1 = 0
           d1_flag = -1
      else
           d1 = d1+d1_1mf
           d1_flag = 1
      end if
c
       if ((d2_flag.eq.-1).or.
     1   (d2_1mf.lt.0))  then
           d2 = 0
           d2_flag = -1
      else
           d2 = d2+d2_1mf
           d2_flag = 1
      end if
c    
      do 3000 k = 1,8
         if ((vse_master_flag.eq.-1).or.
     1      (vseA(1,k).lt.0.0).or.
     1      (vseA(2,k).lt.0.0).or.
     1      (vseB(1,k).lt.0.0).or.
     1      (vseB(2,k).lt.0.0)) then
             vse1A = 0.0 
             vse1B = 0.0
             vse2A = 0.0
             vse2B = 0.0  
             vse_master_flag = -1
        else
             vse1A = vse1A+vseA(1,k)
             vse1B = vse1B+vseB(1,k)
             vse2A = vse2A+vseA(2,k)
             vse2B = vse2B+vseB(2,k)
             vse_master_flag = 1
         end if
3000      continue
c

      do 3500 j=1,2
          do 3500 k = 1,8     
              if (vse_flag_1mf(j,k).eq.-1) saturation_flag = 1
              if (vse_flag_1mf(j,k).eq.-1) saturation_buf(1)=1

               do 3500 i = 1,41
                  if (mdata_flag_1mf(i,j,k).eq.-1)
     1                 saturation_flag = 1
                  if (mdata_flag_1mf(i,j,k).eq.-1)
     1                 saturation_buf(2)=1
3500      continue
      if (d1_1mf.lt.0) saturation_flag = 1
      if (d1_1mf.lt.0) saturation_buf(3) = 1
      if (d2_1mf.lt.0) saturation_flag = 1
      if (d2_1mf.lt.0) saturation_buf(4) = 1
      if (stop_1mf.lt.0) saturation_flag = 1
      if (stop_1mf.lt.0) saturation_buf(5) = 1
      if (start_1mf.lt.0) saturation_flag = 1
      if (start_1mf.lt.0) saturation_buf(6) = 1
      if (vsebar_1mf.lt.0) saturation_flag = 1
      if (vsebar_1mf.lt.0) saturation_buf(7) = 1
c


c**********************************************************************
c
c	check for bit errors in matrix boxes!  /gm 8/30/96
c

	state_sum=0.    ! summed state rate
	ambox_sum=0.    ! summed matrix rates (w/o state rate)
	amions_sum=0.   ! sum of ion rates (w/o state or JUNK)
	vse_sum=0.
	do 6299 i=1,41
6299	test_sum(i)=0.
	do 6300 j=1,2
	do 6300 k=1,8
	vse_sum=vse_sum+vseA(j,k)+vseB(j,k)
	state_sum=state_sum+mdata_1mf(1,j,k)
	do 6300 i=1,41
	if((i.ge.2).and.(i.le.38)) 
     *    amions_sum=amions_sum+mdata_1mf(i,j,k)
	test_sum(i)=test_sum(i)+mdata_1mf(i,j,k)
6300	ambox_sum=ambox_sum+mdata_1mf(i,j,k)

	if(amions_sum.lt.100) goto 4000
	if(vse_sum.eq.0) vse_sum=1.
	if((abs(ambox_sum-vse_sum)/vse_sum).lt.20.) goto 4000
	if(state_sum.eq.0) state_sum=1.
	if((ambox_sum/state_sum).lt.2.5) goto 4000
	
	saturation_flag=1
	saturation_buf(8)=1

	denom=nmfi(2)
        if(denom.eq.0.) denom=1

c	write out bad data line! -- one for each spike
      call sampex_timcon(start_time,startyr,mo,dy,hr,mn,
     *     se,doy,isotime)
      startdoy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0
	scr = sqrt(scx**2+scy**2+scz**2)

	write(77,78) start_time,startyr,
     *     startdoy_real,nmfi, scr/denom,
     *     scx/denom,scy/denom,scz/denom,
     *     vse_sum,state_sum,ambox_sum,abs(vse_sum-
     *     ambox_sum),(abs(ambox_sum-vse_sum)/vse_sum),
     *     (test_sum(i),i=1,41)
78	format(1x,i15','i5',', 12(f15.5,','),41(f10.1,','))
c**********************************************************************
c
c	section to write out bad data line for mask_saturation file
c

4000	if(saturation_flag.eq.0) goto 4100
	goto 4500
4100	if(.not.satspike) return   ! not in a sat/spike interval

      call sampex_timcon(start_time_last,startyr,mo,dy,hr,mn,
     *     se,doy,isotime)
      startdoy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0
      call sampex_timcon(end_time_last,endyr,mo,dy,hr,mn,
     *     se,doy,isotime)
      enddoy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0
	write(78,4110) startyr,startdoy_real,endyr,enddoy_real
     *    ,saturation_buf
4110	format(1x,2(i5','f11.6','),8(i2','))
	satspike=.false.
	return
c
c	saturation flag set
c
4500	if(satspike) goto 4510
	satspike=.true.
	start_time_last=start_time  ! save start time
	end_time_last=end_time      ! save initial end time
	return
4510	end_time_last=end_time      ! extend end time

      return
      end  ! end get_mdata
c

 
