c
c    Subroutines for the program Sdf_lister
c    
c     Contents:
c
c                 subroutine initial_histogram
c 
c                 subroutine reset_histogram
c
c                 logical function fill_histogram
c
c                 logical function write_hist_output
c
c
c
c************************************************
c
        subroutine  initial_histogram
c
c************************************************
c
c    initialize histograms
c    identifies PHA variables
c 
c     6/17/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      integer*4    i,j,k   ! loop indeces
c
c
      do i = 1, Nscripts
        if (script_output_type(i).eq.3) then
          do j = 1,16
            do k = 1, script_Nhistcolumns(i)
             if (pha_key(j).eq.script_histtitles(i,k)) then                
               hist_pha_index(i,k) = j               
             end if                                                         
            end do
          end do       
        end if       
      end do 
c
      return
      end    ! end initial_histogram
c
c
c
c
c************************************************
c
        subroutine  reset_histogram(script_number)
c
c************************************************
c
c    reset histograms 
c 
c     6/17/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      integer*4    j,j0,j1  
      integer*4   script_number
c
c
      sumhist(script_number) = 0                     
      j0 = hist_starting_index(script_number)  ! first index of array to write
      j1 = j0-1+
     1   submatrixsize(script_number,
     1      script_Nhistcolumns(script_number))*
     1      script_Nhist(script_number,
     1      script_Nhistcolumns(script_number))   ! last index of array to write
      do j = j0, j1
         hist(j) =  0         
      end do 
c

      return
      end    ! end reset_histogram
c
c
c
c
c************************************************
c
        logical function fill_histogram(script_number)
c
c************************************************
c
c     fill histograms
c 
c     6/17/98 by J. Dwyer
c       11/11/98 fixed bug 
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      real       phavar      !  variable to be histogramed
      real       xmin, xmax   !  range of variable to be histogrammed
      integer*4   indexi(Ncolumns_max)  ! bin number of variable 
      integer*4   indexall  !  rearranged index #, putting all data in 1-dim array
      integer*4    i, j,k,l,ii   ! loop indices
      integer*4    script_number   ! number of script that is being processed 
      logical     withrange  ! index is within specified range
c
      fill_histogram = .false.  
      phavar =  -99999.9
c
      do i = 1, Npha_good(script_number)   ! loop over selected PHA data
        l  = PHA_index_good(i,script_number)  ! index of selected PHA data
        withrange = .true.
        indexall = hist_starting_index(script_number)  ! first index for this script
        do k = 1, script_Nhistcolumns(script_number)  ! loop over dimensions of histogram
          if (script_logflag(script_number,k).eq.1) then   ! use log scale
                xmin = log(script_histmin(script_number,k))
                xmax = log(script_histmax(script_number,k))
          else   ! use linear scale
                xmin = script_histmin(script_number,k)
                xmax = script_histmax(script_number,k)
          end if
c    identify PHA variable to be histogrammed:         
             ii = hist_pha_index(script_number,k)
             if (ii.eq.1) phavar = 1.0*Einc(l)
             if (ii.eq.2) phavar =  1.0*mass(l)  
             if (ii.eq.3) phavar =  1.0*MeV(l)
             if (ii.eq.4) phavar = 1.0*nsec(l)
             if (ii.eq.5) phavar = 1.0*epha(l)
             if (ii.eq.6) phavar = 1.0*tpha(l)
             if (ii.eq.7) phavar = 1.0*ramp(l)
             if (ii.eq.8) phavar = 1.0*tel(l)
             if (ii.eq.9) phavar = 1.0*cn(l)
             if (ii.eq.10) phavar = 1.0*slant(l)
             if (ii.eq.11) phavar = 1.0*ssd2(l)
             if (ii.eq.12) phavar = 1.0*ab(l)
             if (ii.eq.13) phavar =  1.0*rom(l)
             if (ii.eq.14) phavar = 1.0*spin(l)
             if (ii.eq.15) phavar = 1.0*sect(l) 
             if (ii.eq.16) phavar = 1.0*effic_pha(l) 
c
          if (script_logflag(script_number,k).eq.1) then
             if (phavar.gt.0.0) then  ! use log scale
                 phavar = log(phavar)
             else
                phavar =  -9999.9  ! problem value
             end if
          end if      
          indexi(k) = int(1+script_Nhist(script_number,k)* 
     1           (phavar-xmin)/(xmax -xmin))  ! locate bin number
          if ((indexi(k).lt.1).or.
     1     (indexi(k).gt.script_Nhist(script_number,1))) 
     1         withrange = .false.  ! out of range
          indexall = indexall+
     1       (indexi(k)-1)*
     1        submatrixsize(script_number,k)   ! rearrange into 1-dim array
        end do   ! end k loop
        if (withrange) then
           hist(indexall) = hist(indexall)+1   ! add to hist
           sumhist(script_number) = 
     1            sumhist(script_number)+1   ! keep track of total counts
        end if                                        
      end do      ! end i loop   
c
      if (sumhist(script_number).ge.
     1   script_idelta_Nh(script_number)) then  ! have we reached the required number?
          fill_histogram = .true.   
      end if
c
      return
      end    ! end fill_histogram
c
c
c
c
c************************************************
c
       logical function 
     1     write_hist_output(unit_out,script_number,
     1     start_t, stop_t) 
c
c************************************************
c
c     write output histograms
c 
c     6/17/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c
      real  doy_real
      real  half_delta_t  ! 1/2 of elapsed time interval (sec)
      integer*4	    start_t
      integer*4	    stop_t
      integer*4     unit_out        ! file unit for script file  
      integer*4     script_number    ! number of script that is being processed 
      integer*4     j, j0, j1   ! loop indices
      integer*4     mid_time  ! middle of time interval
      integer*4    yr, mo, dy, hr, mn, se, doy
      integer*4    indexi     ! last index of outputstring written to 
      character*20   isotime   ! sampex_timecon isotime array
      character*12288   outputstring
c
      mid_time = (stop_t+start_t)/2
      half_delta_t = (stop_t-start_t)/2.0
      call sampex_timcon(mid_time,yr,mo,dy,hr,mn,se,doy,isotime)
      doy_real = doy+(3600.0*hr+60.0*mn+se)/86400.0        
c      
      write(unit_out,100) mo, dy, yr, hr, mn, se,
     1   yr, doy_real,  
     1   half_delta_t
c
      indexi = 1
      j0 = hist_starting_index(script_number)  ! first index of array to write
      j1 = j0-1+
     1   submatrixsize(script_number,
     1      script_Nhistcolumns(script_number))*
     1      script_Nhist(script_number,
     1      script_Nhistcolumns(script_number))   ! last index of array to write      
        do j = j0, j1
             call write_line(indexi, outputstring, 
     1          1.0*hist(j), .true., .false.,
     1                dataformat_flag)
         if (indexi.ge.100) then     ! max number of columns = 100
            write(unit_out,200)  
     1     outputstring(1:(indexi-1))   
            indexi = 1    ! start new row
         end if
      end do
      if (indexi.gt.1) then
            write(unit_out,200)  
     1     outputstring(1:(indexi-1))   
      end if
c
      Noutput_lines(script_number) = 
     1      Noutput_lines(script_number)+1
c      
c
100 	   format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1      ', 'i4', 'f12.8', 'f12.4',')
200 	   format('       'a)      
c    
      write_hist_output = .true.
      if (Noutput_lines(script_number).ge.
     1   maximum_rows) write_hist_output = .false.
c
      return
      end    ! end write_hist_output
c



