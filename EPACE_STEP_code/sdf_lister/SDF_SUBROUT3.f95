c
c    Subroutines for the program sdf_lister
c    
c     Contents:
c
c
c            logical function cut_interval_test
c
c            subroutine  cut_PHA_data
c
c            subroutine  assign_script_flags
c
c            subroutine  write_titles
c
c            subroutine strip_blanks20
c
c            subroutine strip_blanks41
c
c
c
c
c*******************************************************
c
      logical function cut_interval_test
     1    (script_number,whichtest)
c
c*******************************************************
c 
c     puts cuts on output
c
c     6/16/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c 
c         
      integer*4   whichtest  ! 1 = ("_cutflag.eq.1),2 = ("_cutflag.eq.2)     
      integer*4   script_number
      integer*4   i,j k,l,ii,jj ! loop index

c
      cut_interval_test = .true.
c        
      do ii = 1,100
        if ((ratio_cutflag(script_number,ii).eq.
     1    whichtest).or.
     1    (ratio_cutflag(script_number,ii).eq.3)) then
          if ((rates_ratio(ii).lt.
     1        ratio_minvalue(script_number,ii)).or.
     1       (rates_ratio(ii).ge.
     1        ratio_maxvalue(script_number,ii)))
     1          cut_interval_test = .false.
        end if
       end do
       do ii = 1, 42
         do k = 1,3
           if ((mdata_cutflag(script_number,ii,k).eq.
     1         whichtest).or.
     1       (mdata_cutflag(script_number,ii,k).eq.3)) then
             if ((mdata_ave(ii,k).lt.
     1          mdata_minvalue(script_number,ii,k)).or.
     1         (mdata_ave(ii,k).ge.
     1          mdata_maxvalue(script_number,ii,k)))
     1          cut_interval_test = .false.
           end if
         end do
       end do
c  
       do ii = 1, 3
         if ((disc_cutflag(script_number,ii).eq.
     1     whichtest).or.
     1     (disc_cutflag(script_number,ii).eq.3)) then
           if ((disc_ave(ii).lt.
     1       disc_minvalue(script_number,ii)).or.
     1       (disc_ave(ii).ge.
     1       disc_maxvalue(script_number,ii)))
     1       cut_interval_test = .false.
         end if
       end do
c
       do k = 1,3
         if ((D_cutflag(script_number,k).eq.
     1     whichtest).or.
     1     (D_cutflag(script_number,k).eq.3)) then
           if ((D_ave(k).lt.
     1       D_minvalue(script_number,k)).or.
     1       (D_ave(k).ge.
     1       D_maxvalue(script_number,k)))
     1       cut_interval_test = .false.
         end if
       end do
c  
       do ii = 1, 6
         if ((STEPkp_cutflag(script_number,ii).eq.
     1     whichtest).or.
     1     (STEPkp_cutflag(script_number,ii).eq.3)) then
           if ((STEPkp_ave(ii).lt.
     1       STEPkp_minvalue(script_number,ii)).or.
     1       (STEPkp_ave(ii).ge.
     1        STEPkp_maxvalue(script_number,ii)))
     1          cut_interval_test = .false.
         end if
       end do
c  
       do ii = 1, 6
         if ((hk_cutflag(script_number,ii).eq.
     1     whichtest).or.
     1     (hk_cutflag(script_number,ii).eq.3)) then
           if ((hk_ave(ii).lt.
     1       hk_minvalue(script_number,ii)).or.
     1       (hk_ave(ii).ge.
     1        hk_maxvalue(script_number,ii)))
     1          cut_interval_test = .false.
         end if
       end do
c
       if ((sat_cutflag(script_number).eq.
     1     whichtest).or.
     1     (sat_cutflag(script_number).eq.3)) then
         if ((saturation(script_number).lt.
     1      sat_minvalue(script_number)).or.
     1      (saturation(script_number).ge.
     1       sat_maxvalue(script_number)))
     1       cut_interval_test = .false.
       end if      
c   
       do ii = 1, 37
         if ((kp_cutflag(script_number,ii).eq.
     1     whichtest).or.
     1     (kp_cutflag(script_number,ii).eq.3)) then
           if ((kp_ave(ii).lt.
     1       kp_minvalue(script_number,ii)).or.
     1       (kp_ave(ii).ge.
     1       kp_maxvalue(script_number,ii)))
     1       cut_interval_test = .false.
         end if
       end do       

c       
       return
       end  ! end  cut_interval_test       
c
c
c
c
c
c*******************************************************
c
       subroutine cut_PHA_data
     1         (script_number)
c
c*******************************************************
c 
c     puts cuts of PHA data
c
c     6/16/98 by J. Dwyer
c     1/21/99 added script_stateab_flag cut
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c  
c    
      real        phavar
      integer*4   script_number
      integer*4   i,k,ii ! loop index      
      logical     passtest
c
      Npha_good(script_number) = 0
      if (NPHA.gt.0) then
        k = 1
        do i = 1, NPHA                
          PHA_data_flag(i,script_number) = 0    
          passtest = .true.
          
          if ((script_stateab_flag(script_number).eq.0).and.
     1     (ab(i).eq.1)) passtest = .false.
          if ((script_stateab_flag(script_number).eq.1).and.
     1     (ab(i).eq.0)) passtest = .false. 
          do ii = 1,16
           if (pha_cutflag(script_number,ii).ge.1) then  
             if (ii.eq.1) phavar = 1.0*Einc(i)
             if (ii.eq.2) phavar =  1.0*mass(i)  
             if (ii.eq.3) phavar =  1.0*MeV(i)
             if (ii.eq.4) phavar = 1.0*nsec(i)
             if (ii.eq.5) phavar = 1.0*epha(i)
             if (ii.eq.6) phavar = 1.0*tpha(i)
             if (ii.eq.7) phavar = 1.0*ramp(i)
             if (ii.eq.8) phavar = 1.0*tel(i)
             if (ii.eq.9) phavar = 1.0*cn(i)
             if (ii.eq.10) phavar = 1.0*slant(i)
             if (ii.eq.11) phavar = 1.0*ssd2(i)
             if (ii.eq.12) phavar = 1.0*ab(i)
             if (ii.eq.13) phavar =  1.0*rom(i)
             if (ii.eq.14) phavar = 1.0*spin(i)
             if (ii.eq.15) phavar = 1.0*sect(i) 
             if (ii.eq.16) phavar = 1.0*effic_pha(i)          
c          
             if ((phavar.lt.pha_minvalue(script_number,ii)).or.
     1       (phavar.ge.pha_maxvalue(script_number,ii)))
     1         passtest = .false.
           end if     
         end do        
         if (passtest) then
           PHA_data_flag(i,script_number) = 1 
           PHA_index_good(k,script_number) = i
           k = k+1
         end if   
        end do
        Npha_good(script_number) = k-1
      end if 
c       
       return
       end  ! end cut_PHA_data
c
c
c
c
c
c************************************************
c
        subroutine  assign_script_flags
c
c************************************************
c
c    sets output flags according to script file instructions
c 
c    4/10/98 by J. Dwyer
c 
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      integer*4    i,j,k,l   ! loop indeces
      character*41   line41
      character*20   line20n, line20d
      logical   numfound, denfound
c      
c
      do i = 1, Nscripts  
         Nratios(i) = 0             
         do k = 1, 100
           ratio_writeflag(i,k) = -1
           ratio_cutflag(i,k) = -1
         end do
         do j = 1, 42
           do k = 1, 3
             mdata_ave_writeflag(i,j,k) = -1
             mdata_sect_writeflag(i,j,k) = -1
             mdata_cutflag(i,j,k)  = -1
           end do
         end do
         do j = 1, 3
           disc_writeflag(i,j) = -1  
           disc_cutflag(i,j) = -1
         end do
         do k = 1, 3
           D_writeflag(i,k) = -1  
           D_cutflag(i,k) = -1
         end do
         do j = 1, 16
           pha_writeflag(i,j) = -1
           pha_cutflag(i,j) = -1
         end do   
         do j = 1, 6
           hk_writeflag(i,j) = -1
           hk_cutflag(i,j)  = -1
         end do
         do j = 1, 6
           STEPkp_writeflag(i,j) = -1
           STEPkp_cutflag(i,j)  = -1
         end do 
         do j = 1, 37
           kp_writeflag(i,j) = -1
           kp_cutflag(i,j)  = -1
         end do     
         sat_writeflag(i) = -1
         sat_cutflag(i) = -1         
         do k = 1, script_Ncolumns(i)
           titlefound_flag(i,k) = .false.
         end do
      end do 
c
      do i = 1, Nscripts
c
ccccccccccccccccc rates ratios cccccccccccccccccccc
c
        numfound = .false.
        denfound = .false.
        Nratios(i) = 1
        do k = 1, script_Ncolumns(i)
          line41 = script_titles_raw(i,k)   
          do j = 1,41
            if (line41(j:j).eq.'/') then  
              if (Nratios(i).gt.100) then
            type *, 'max number of ratios exceeded!'
               type *, 'script #:',i
               stop  
              end if                      
              line20n = line41(1:(j-1))
              line20d = line41((j+1):(j+21))                          
              script_titles_ratio(i,Nratios(i)) = line41
              script_titles(i,k) =
     1                '--------------------' 
              do l = 1, 42
               if (mdata_key(l).eq.line20n) then
                  ratio_index(i,Nratios(i),1) = l
                  ratio_which(i,Nratios(i),1) = 1
                  numfound = .true.
               end if
               if (mdata_key(l).eq.line20d) then
                  ratio_index(i,Nratios(i),2) = l
                  ratio_which(i,Nratios(i),2) = 1
                  denfound = .true.
               end if
              end do             
              do l = 1,3
               if (disc_key(l).eq.line20n) then
                 ratio_index(i,Nratios(i),1) = l
                 ratio_which(i,Nratios(i),1) = 2
                 numfound = .true.
               end if
               if (disc_key(l).eq.line20d) then
                 ratio_index(i,Nratios(i),2) = l
                 ratio_which(i,Nratios(i),2) = 2
                 denfound = .true.
               end if
              end do  
              if (D_key.eq.line20n) then
                 ratio_index(i,Nratios(i),1) = 0
                 ratio_which(i,Nratios(i),1) = 3
                 numfound = .true.
              end if
              if (D_key.eq.line20d) then
                 ratio_index(i,Nratios(i),2) = 0
                 ratio_which(i,Nratios(i),2) = 3
                 denfound = .true.
              end if                   
              ratio_writeflag(i,Nratios(i)) = 
     1             script_writeflag(i,k)                                                              
              ratio_cutflag(i,Nratios(i)) = 
     1           script_cutflag(i,k)                                       
        ratio_minvalue(i,Nratios(i)) = script_minvalue(i,k)
        ratio_maxvalue(i,Nratios(i)) = script_maxvalue(i,k)
              if ((numfound).and.(denfound)) then
                Nratios(i) = Nratios(i)+1
              else
                type *, 'ratio not found!'
                type *, 'script #:',i
                type *, 'title:',line41
              end if
              goto 100
            end if ! end if (line41(j:j).eq.'/')
          end do  ! end j loop
          script_titles(i,k) = line41(1:20)
100       continue                            
        end do ! end k loop
        Nratios(i) = Nratios(i) - 1
c
ccccccccccccccccc saturation cccccccccccccccccccc
c
      do k = 1, script_Ncolumns(i)
        if ('saturation'.eq.script_titles(i,k)) then
          titlefound_flag(i,k) = .true.       
          sat_writeflag(i) = 
     1           script_writeflag(i,k)                                         
          sat_cutflag(i) = script_cutflag(i,k)
          sat_minvalue(i) = script_minvalue(i,k)
          sat_maxvalue(i) = script_maxvalue(i,k)                             
        end if 
        if (script_output_type(i).eq.5)
     1        sat_writeflag(i) = 1                                              
      end do  
c
c
ccccccccccccccccc KP data cccccccccccccccccccc
c
      do k = 1, script_Ncolumns(i)
        do j = 1, 37          
          if (kp_key(j).eq.script_titles(i,k)) then
            titlefound_flag(i,k) = .true.        
            kp_writeflag(i,j) = 
     1             script_writeflag(i,k)                                                     
            kp_cutflag(i,j) = script_cutflag(i,k)
            kp_minvalue(i,j) = script_minvalue(i,k)
            kp_maxvalue(i,j) = script_maxvalue(i,k)                             
          end if         
        end do      
      end do  
c
ccccccccccccccccc ROM rates cccccccccccccccccccc
c                  
      do j = 1, 42           
        do k = 1, script_Ncolumns(i)
          if (mdata_key(j).eq.script_titles(i,k)) then
            titlefound_flag(i,k) = .true.    
            if (script_sector_flag(i,k).eq.'SECT') then
              if (script_tel_flag(i,k).eq.'TEL1') then
                mdata_sect_writeflag(i,j,1) = 
     1               script_writeflag(i,k)
              end if
              if (script_tel_flag(i,k).eq.'TEL2') then
                mdata_sect_writeflag(i,j,2) = 
     1               script_writeflag(i,k)
              end if
              if (script_tel_flag(i,k).eq.'TEL1&2') then
                mdata_sect_writeflag(i,j,3) = 
     1               script_writeflag(i,k)
              end if
            end if
            if (script_sector_flag(i,k).eq.'AVER') then
              if (script_tel_flag(i,k).eq.'TEL1') then
                mdata_ave_writeflag(i,j,1) = 
     1               script_writeflag(i,k)
              end if
              if (script_tel_flag(i,k).eq.'TEL2') then
                mdata_ave_writeflag(i,j,2) = 
     1               script_writeflag(i,k)
              end if
              if (script_tel_flag(i,k).eq.'TEL1&2') then
                mdata_ave_writeflag(i,j,3) = 
     1               script_writeflag(i,k)
              end if
            end if            
            if (script_tel_flag(i,k).eq.'TEL1') then
              mdata_cutflag(i,j,1) = script_cutflag(i,k)
              mdata_minvalue(i,j,1) = script_minvalue(i,k)
              mdata_maxvalue(i,j,1) = script_maxvalue(i,k)
            end if
            if (script_tel_flag(i,k).eq.'TEL2') then
              mdata_cutflag(i,j,2) = script_cutflag(i,k)
              mdata_minvalue(i,j,2) = script_minvalue(i,k)
              mdata_maxvalue(i,j,2) = script_maxvalue(i,k)
            end if
            if (script_tel_flag(i,k).eq.'TEL1&2') then
              mdata_cutflag(i,j,3) = script_cutflag(i,k)
              mdata_minvalue(i,j,3) = script_minvalue(i,k)
              mdata_maxvalue(i,j,3) = script_maxvalue(i,k)
            end if
          end if
          if (script_output_type(i).eq.5)
     1       mdata_ave_writeflag(i,j,3) = 1    
        end do
      end do   
c
ccccccccccccccccc disc rates cccccccccccccccccccc
c           
      do j = 1, 3
        do k = 1, script_Ncolumns(i)
          if (disc_key(j).eq.script_titles(i,k)) then
            titlefound_flag(i,k) = .true.                              
             disc_writeflag(i,j) = 
     1               script_writeflag(i,k)
            disc_cutflag(i,j) = script_cutflag(i,k)
            disc_minvalue(i,j) = script_minvalue(i,k)
            disc_maxvalue(i,j) = script_maxvalue(i,k)                        
          end if
          if (script_output_type(i).eq.5)
     1       disc_writeflag(i,j) = 1
        end do           
      end do 
c
ccccccccccccccccc D rates cccccccccccccccccccc
c           
      do k = 1, script_Ncolumns(i)
        if (D_key.eq.script_titles(i,k)) then
          titlefound_flag(i,k) = .true.                                
          if (script_tel_flag(i,k).eq.'TEL1') then
            D_writeflag(i,1) = 
     1               script_writeflag(i,k)
            D_cutflag(i,1) = script_cutflag(i,k)
            D_minvalue(i,1) = script_minvalue(i,k)
            D_maxvalue(i,1) = script_maxvalue(i,k)
          end if
          if (script_tel_flag(i,k).eq.'TEL2') then
            D_writeflag(i,2) = 
     1               script_writeflag(i,k)
            D_cutflag(i,2) = script_cutflag(i,k)
            D_minvalue(i,2) = script_minvalue(i,k)
            D_maxvalue(i,2) = script_maxvalue(i,k)
          end if
          if (script_tel_flag(i,k).eq.'TEL1&2') then
            D_writeflag(i,3) = 
     1               script_writeflag(i,k)
            D_cutflag(i,3) = script_cutflag(i,k)
            D_minvalue(i,3) = script_minvalue(i,k)
            D_maxvalue(i,3) = script_maxvalue(i,k)
          end if             
        end if            
        if (script_output_type(i).eq.5)
     1       D_writeflag(i,3) = 1       
      end do 
c
ccccccccccccccccc STEPKP data cccccccccccccccccccc
c
      do k = 1, script_Ncolumns(i)
        do j = 1, 6          
          if (STEPkp_key(j).eq.script_titles(i,k)) then
            titlefound_flag(i,k) = .true.  
            STEPkp_writeflag(i,j) = 
     1             script_writeflag(i,k)                                                      
            STEPkp_cutflag(i,j) = script_cutflag(i,k)
            STEPkp_minvalue(i,j) = script_minvalue(i,k)
            STEPkp_maxvalue(i,j) = script_maxvalue(i,k)                             
          end if         
        end do      
      end do  
c 
ccccccccccccccccc HK data cccccccccccccccccccc
c
      do k = 1, script_Ncolumns(i)
        do j = 1, 6          
          if (hk_key(j).eq.script_titles(i,k)) then
            titlefound_flag(i,k) = .true.          
            hk_writeflag(i,j) = 
     1             script_writeflag(i,k)                                                     
            hk_cutflag(i,j) = script_cutflag(i,k)
            hk_minvalue(i,j) = script_minvalue(i,k)
            hk_maxvalue(i,j) = script_maxvalue(i,k)                             
          end if         
        end do      
      end do 
c          
c                
      if ((script_output_type(i).eq.2).or.
     1   (script_output_type(i).eq.3).or.
     1   (script_output_type(i).eq.4)) then               
        do j = 1, 16
          do k = 1, script_Ncolumns(i)
            if (pha_key(j).eq.script_titles(i,k)) then
              titlefound_flag(i,k) = .true.            
              if (script_output_type(i).eq.2) then
                 pha_writeflag(i,j) = script_writeflag(i,k)  
              end if                                                                    
              pha_cutflag(i,j) = script_cutflag(i,k) 
              pha_minvalue(i,j) = script_minvalue(i,k)
              pha_maxvalue(i,j) = script_maxvalue(i,k)                                                           
            end if 
          end do           
        end do           
      end if    
c                
       do k = 1, script_Ncolumns(i)
          if ((.not.titlefound_flag(i,k)).and.
     1 (script_titles(i,k).ne.'--------------------')) then
            type *, 'title not found!'
            type *, 'script #:',i
            type *, 'title: ', script_titles(i,k)
          end if
       end do
c     
c
c    identify rate to use when accumulating to specified counts
        if (script_output_type(i).ne.2) then
          accum_rate_which(i) = 0      
          do j = 1, 42   
            if (mdata_key(j).eq.
     1      script_accum_rate_title(i)) then
              accum_rate_index(i) = j
              accum_rate_which(i) = 1
            end if
          end do         
          do j = 1, 3           
            if (disc_key(j).eq.
     1       script_accum_rate_title(i)) then
              accum_rate_index(i) = j
              accum_rate_which(i) = 2
            end if
          end do            
          if (D_key.eq.
     1       script_accum_rate_title(i)) then
              accum_rate_which(i) = 3
           end if
          if (accum_rate_which(i).eq.0) then
            type *, 'accum. rate title not found!'
            type *, 'script # ', i
            type *, 'title:', script_accum_rate_title(I)
            stop
          end if
        end if
      end do  ! end i loop 
c
      return
      end  ! end assign_script_flags
c
c
c
c
c************************************************
c
        subroutine  write_titles
     1      (script_number,titlestring, Ntitlestring)
c
c************************************************
c
c    creates string for title line of output
c 
c     6/29/98 by J. Dwyer
c      10/5/98 fixed bug in disc output
c      11/4/98 fixed bug in junk output
c      8/9/99 added index number to sigma so each title is unique
c 
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      real       Emid
      real       eflag(41,2)  ! used to calculate Emid
      real       neflag(41)   ! used to calculate Emid
      integer*4     script_number    ! number of script that is being processed 
      integer*4    i,j,k,isect   ! loop indices
      integer*4   c1, dc, Ntitlestring
      integer*4   lastchar
      integer*4   nt
      integer*4   indexsigma
      character*12288   titlestring
      character*41   line41
      character*20   line
      character*1    numstring
      character*9    Emidstring
      character*6    telline
      character*3   sigmastring            
c     
      do i = 1,41
        eflag(i,1) = 0.0
        eflag(i,2) = 0.0
        neflag(i) = 0.0
        if (effic_flag(i,1)) then
          eflag(i,1) = 1.0
          neflag(i) = neflag(i)+1.0
        end if 
        if (effic_flag(i,2)) then
          eflag(i,2) = 1.0
          neflag(i) = neflag(i)+1.0
        end if     
      end do
c   
100   format(F9.4)
c
200   format(i3)
      indexsigma = 1
c
      titlestring(1:41) = 
     1    ' Time, year, day of the year, delta_t/2, '
c
         c1 = 42
         dc =  4
c      
cccccccccccccc sat ccccccccccccccccccc         
         if (sat_writeflag(script_number)
     1        .eq.1) then
              titlestring(c1:(c1+dc)) =
     1         'SAT,'
            c1 = c1+dc+1
         end if        
c      
cccccccccccccc PHA data ccccccccccccccccccc         
      do j = 1, 16              
          if (pha_writeflag(script_number,j).eq.1) then
            line = pha_key(j)
            call strip_blanks20(line,lastchar)
            dc = lastchar-1+2
            titlestring(c1:(c1+dc)) = 
     1         line(1:lastchar)//', '
            c1 = c1+dc+1
          end if
      end do   
c      
cccccccccccccc disc data ccccccccccccccccccc      
      do j = 1, 3     
         if (disc_writeflag(script_number,j)
     1        .eq.1) then
           line = disc_key(j)
           call strip_blanks20(line,lastchar)
           dc = lastchar-1+9+4
           write(sigmastring, 200) 
     1       indexsigma
          indexsigma=indexsigma+1
           titlestring(c1:(c1+dc)) =
     1      line(1:lastchar)//
     1      ', sigma '//sigmastring//', '
           c1 = c1+dc+1
         end if
      end do 
c 
c      
cccccccccccccc D data ccccccccccccccccccc
      do k = 1,3          
        if (D_writeflag(script_number,k)
     1        .eq.1) then
          if (k.eq.1) then
            telline = ' TEL1,'
            nt = 6
          end if
          if (k.eq.2) then 
            telline = ' TEL2,'
            nt = 6
          end if
          if (k.eq.3) then 
            telline = ',     '
            nt = 1
          end if
           line = D_key
           call strip_blanks20(line,lastchar)
           dc = lastchar-1+9+nt+4
           write(sigmastring, 200) 
     1       indexsigma
          indexsigma=indexsigma+1
           titlestring(c1:(c1+dc)) =
     1      line(1:lastchar)//
     1   telline(1:nt)//' sigma '//sigmastring//', '
           c1 = c1+dc+1
        end if
      end do           
c      
cccccccccccccc mdata data (AVER) ccccccccccccccccccc
      do k = 1,3 
        if (mdata_ave_writeflag(script_number,42,k)  
     1       .eq.1) then  ! VSE rates
          if (k.eq.1) then
            telline = ' TEL1,'
            nt = 6
          end if
          if (k.eq.2) then 
            telline = ' TEL2,'
            nt = 6
          end if
          if (k.eq.3) then 
            telline = ',     '
            nt = 1
          end if
          line = mdata_key(42)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+8+nt+4    
          write(sigmastring, 200) 
     1       indexsigma
          indexsigma=indexsigma+1   
          titlestring(c1:(c1+dc)) =
     1   line(1:lastchar)//telline(1:nt)//
     1      ' sigma '//sigmastring//', '
            c1 = c1+dc+1
        end if
      end do
      do k = 1,3 
        if (mdata_ave_writeflag(script_number,1,k) 
     1       .eq.1) then   ! state rate
          if (k.eq.1) then
            telline = ' TEL1,'
            nt = 6
          end if
          if (k.eq.2) then 
            telline = ' TEL2,'
            nt = 6
          end if
          if (k.eq.3) then 
            telline = ',     '
            nt = 1
          end if
          line = mdata_key(1)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+8+nt+4   
          write(sigmastring, 200) 
     1       indexsigma
          indexsigma=indexsigma+1    
          titlestring(c1:(c1+dc)) =
     1   line(1:lastchar)//telline(1:nt)//
     1    ' sigma '//sigmastring//', '
            c1 = c1+dc+1
        end if
      end do
      do j = 2, 38 
        do k = 1,3     
          Emid = 0.0
          if (mdata_ave_writeflag(script_number,j,k) 
     1       .eq.1) then   ! ROM boxes
          if (k.eq.1) then
            telline = ' TEL1 '
            nt = 6
            Emid = eflag(j,1)*(elo(j,1)+ehi(j,1))/2.0
          end if
          if (k.eq.2) then 
            telline = ' TEL2 '
            nt = 6
            Emid = eflag(j,2)*(elo(j,2)+ehi(j,2))/2.0
          end if
          if (k.eq.3) then 
            telline = '      '
            nt = 1
            if (neflag(j).gt.0.0)
     1        Emid = (eflag(j,1)*(elo(j,1)+ehi(j,1))+
     1              eflag(j,2)*(elo(j,2)+ehi(j,2)))/
     1              (neflag(j)*2.0)     
          end if
          line = mdata_key(j)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+26+nt+4
          write(Emidstring, 100) Emid
          write(sigmastring, 200) 
     1       indexsigma
          indexsigma=indexsigma+1
          titlestring(c1:(c1+dc)) =
     1 line(1:lastchar)//telline(1:nt)//
     1      Emidstring//
     1   ' MeV/nuc, sigma '//sigmastring//', '
            c1 = c1+dc+1
          end if
        end do
      end do 
      do j = 1, 3
        do k = 1,3 
          if (mdata_ave_writeflag(script_number,38+j,k)  
     1       .eq.1) then   ! junk
          if (k.eq.1) then
            telline = ' TEL1,'
            nt = 6
          end if
          if (k.eq.2) then 
            telline = ' TEL2,'
            nt = 6
          end if
          if (k.eq.3) then 
            telline = ',     '
            nt = 1            
          end if
          line = mdata_key(38+j)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+8+nt+4     
          write(sigmastring, 200) 
     1       indexsigma
          indexsigma=indexsigma+1 
          titlestring(c1:(c1+dc)) =
     1     line(1:lastchar)//telline(1:nt)//
     1     ' sigma '//sigmastring//', '
            c1 = c1+dc+1
          end if
        end do
      end do 
c      
cccccccccccccc mdata data (SECT) ccccccccccccccccccc  
      do k = 1,3 
        if (mdata_sect_writeflag(script_number,42,k) 
     1       .eq.1) then   ! VSE rates
          if (k.eq.1) then
            telline = ' TEL1 '
            nt = 6
          end if
          if (k.eq.2) then 
            telline = ' TEL2 '
            nt = 6
          end if
          if (k.eq.3) then 
            telline = '      '
            nt = 1
          end if
          line = mdata_key(42)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+17+nt+4     
          do isect = 1, 8   
            write(sigmastring, 200) 
     1       indexsigma
            indexsigma=indexsigma+1
            write(numstring,'(I1)') isect
            titlestring(c1:(c1+dc)) = 
     1  line(1:lastchar)//telline(1:nt)//
     1  '(sect '//numstring//
     1     '), sigma '//sigmastring//', '
            c1 = c1+dc+1
          end do     
        end if
      end do
      do k = 1,3 
        if (mdata_sect_writeflag(script_number,1,k) 
     1       .eq.1) then  ! state rate
          if (k.eq.1) then
            telline = ' TEL1 '
            nt = 6
          end if
          if (k.eq.2) then 
            telline = ' TEL2 '
            nt = 6
          end if
          if (k.eq.3) then 
            telline = '      '
            nt = 1
          end if
          line = mdata_key(1)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+17+nt+4     
          do isect = 1, 8  
           write(sigmastring, 200) 
     1       indexsigma
           indexsigma=indexsigma+1 
            write(numstring,'(I1)') isect
            titlestring(c1:(c1+dc)) = 
     1    line(1:lastchar)//telline(1:nt)//
     1  '(sect '//numstring//
     1     '), sigma '//sigmastring//', '
            c1 = c1+dc+1
          end do     
        end if
      end do  
      do j = 2,38     
        do k = 1,3   
          Emid = 0.0  
          if (mdata_sect_writeflag(script_number,j,k)  
     1        .eq.1) then  ! ROM boxes
          if (k.eq.1) then
            telline = ' TEL1 '
            nt = 6
            Emid = eflag(j,1)*(elo(j,1)+ehi(j,1))/2.0
          end if
          if (k.eq.2) then 
            telline = ' TEL2 '
            nt = 6
            Emid = eflag(j,2)*(elo(j,2)+ehi(j,2))/2.0
          end if
          if (k.eq.3) then 
            telline = '      '
            nt = 1
            if (neflag(j).gt.0.0) 
     1        Emid = (eflag(j,1)*(elo(j,1)+ehi(j,1))+
     1              eflag(j,2)*(elo(j,2)+ehi(j,2)))/
     1              (neflag(j)*2.0)
          end if
            line = mdata_key(j)
            call strip_blanks20(line,lastchar)
            dc = lastchar-1+35+nt+4
            write(Emidstring, 100) Emid
            do isect = 1, 8
             write(sigmastring, 200) 
     1       indexsigma
             indexsigma=indexsigma+1
              write(numstring,'(I1)') isect
              titlestring(c1:(c1+dc)) = 
     1  line(1:lastchar)//telline(1:nt)//Emidstring//
     1      ' MeV/nuc (sect '//numstring//
     1      '), sigma '//sigmastring//', '     
             c1 = c1+dc+1
            end do
          end if
        end do
      end do
      do j = 1, 3  
        do k = 1,3 
          if (mdata_sect_writeflag(script_number,38+j,k)   
     1       .eq.1) then  ! junk
          if (k.eq.1) then
            telline = ' TEL1 '
            nt = 6
          end if
          if (k.eq.2) then 
            telline = ' TEL2 '
            nt = 6
          end if
          if (k.eq.3) then 
            telline = '      '
            nt = 1
          end if
            line = mdata_key(38+j)
            call strip_blanks20(line,lastchar)
            dc = lastchar-1+17+nt+4     
            do isect = 1, 8   
             write(sigmastring, 200) 
     1       indexsigma
             indexsigma=indexsigma+1
              write(numstring,'(I1)') isect
              titlestring(c1:(c1+dc)) = 
     1  line(1:lastchar)//telline(1:nt)//
     1  '(sect '//numstring//
     1    '), sigma '//sigmastring//', '
              c1 = c1+dc+1
            end do     
          end if
        end do 
      end do
c       
cccccccccccccc ratios data ccccccccccccccccccc      
      if (Nratios(script_number).gt.0) then
        do j = 1, Nratios(script_number) 
          if (ratio_writeflag(script_number,j).eq.1) then
             line41 = script_titles_ratio(script_number,j)
             call strip_blanks41(line41,lastchar)
             dc = lastchar-1+9+4 
            write(sigmastring, 200) 
     1      indexsigma
           indexsigma=indexsigma+1         
          titlestring(c1:(c1+dc)) = 
     1  line41(1:lastchar)//
     1    ', sigma '//sigmastring//', '
          c1 = c1+dc+1   
          end if
        end do  
      end if
c
cccccccccccccc STEPkp data ccccccccccccccccccc      
      do j =1, 6    
         if (STEPkp_writeflag(script_number,j)
     1       .eq.1) then
          line = STEPkp_key(j)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+2        
          titlestring(c1:(c1+dc)) =
     1 line(1:lastchar)//', '
            c1 = c1+dc+1
         end if
      end do   
c      
cccccccccccccc HK data ccccccccccccccccccc      
      do j =1, 6     
         if (hk_writeflag(script_number,j)
     1       .eq.1) then
          line = hk_key(j)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+2    
          titlestring(c1:(c1+dc)) =
     1 line(1:lastchar)//', '
            c1 = c1+dc+1
         end if
      end do   
c      
cccccccccccccc KP data ccccccccccccccccccc      
      do j =1, 37     
         if (kp_writeflag(script_number,j)
     1       .eq.1) then
          line = kp_key(j)
          call strip_blanks20(line,lastchar)
          dc = lastchar-1+2        
          titlestring(c1:(c1+dc)) =
     1 line(1:lastchar)//', '
            c1 = c1+dc+1
         end if
      end do   
c      
      Ntitlestring = c1-1
c    
      return
      end  ! end write_titles
c
c
c
c
c************************************************
c
        subroutine strip_blanks20(line,lastchar)
c
c************************************************
c
c     finds last nonbank character in a string of length 20
c
      integer*4       lastchar, i
      character*20    line
c
      lastchar = 0
      do i = 1,20
        lastchar = 20+1-i
        if (line(lastchar:lastchar).ne.' ') goto 100
      end do
100   continue
c
      return
c
      end   ! end strip_blanks20
c
c
c
c
c************************************************
c
        subroutine strip_blanks41(line,lastchar)
c
c************************************************
c
c     finds last nonbank character in a string of length 20
c
      integer*4       lastchar, i
      character*41    line
c
      lastchar = 0
      do i = 1,41
        lastchar = 41+1-i
        if (line(lastchar:lastchar).ne.' ') goto 100
      end do
100   continue
c
      return
c
      end   ! end strip_blanks41
c
c

