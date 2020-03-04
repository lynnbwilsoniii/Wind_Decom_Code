c    Subroutines for the program sdf_lister
c    
c     Contents:
c             logical function open_output_STEPKP
c
c             logical function write_output_STEPKP
c
c		
c****************************************************
c
       logical function open_output_STEPKP(unit_STEPKP)
c
c****************************************************
c 
c     Open output data file and write the header for STEP KP fluxes
c     returns .true. if successful, .false. otherwise
c
c     2/6/96 by J. Dwyer
c          modified 2/28/96 by J. Dwyer added headerlines output to write_header
c                         4/8/96 by J. Dwyer changed headerlines to 40
c
      include 'sdf_include.inc'   ! include type declarations
c     
      integer   unit_STEPKP   ! file unit of output file    
      character*66     filename       ! complete name of output file
      character*6       file_prefix   ! prefix to be attached to output file name
      integer   headerlines
c
      headerlines = 40
c    
      file_prefix = 'STEPKP'
      write(filename, '(a6,a60)') file_prefix, outputfile
      open(unit=unit_STEPKP ,name=outputdir//filename,
     1     status='NEW',DISP='KEEP',RECL=32766, err=1000) ! open output data file
      print *, 'Opened output file: ', filename
c
      call write_header(unit_STEPKP,file_prefix,headerlines)
c
      write(unit_STEPKP,200) 
200  	format(' Time,',
     1         ' year,',
     1         ' day of the year,', 
     1         ' delta_t/2,',  
     1         ' He 3,',
     1         ' He 5,',
     1         ' CNO 3,',
     1         ' CNO 5,',
     1         ' Fe 3,',
     1         ' Fe 6')
c    
      open_output_STEPKP = .true.
      return  !  succussful
1000   open_output_STEPKP = .false.  ! on error go to here
      print *, 'Error opening output STEP KP flux file:', 
     1     filename
      return      
      end  ! end open_output_STEPKP
c
c
c
c
c****************************************************
c
       logical function write_output_STEPKP(unit_STEPKP,
     1                             nrow_STEPKP) 
c
c****************************************************
c
c      writes STEPKP data to output file with file unit=unit_STEPKP
c      returns .false. if number of output lines written exceeds
c      specified value, .true. otherwise.
c
c     2/6/96 by J. Dwyer
c            5/6/96 changed outputstring to 4096 characters
c
      include 'sdf_include.inc'   ! include type declarations
      include 'hex_include.inc'   ! include type declarations
c     
      integer   unit_STEPKP  ! output file unit
      integer   nrow_STEPKP   !  number of rows written to output file
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
      if (nrow_STEPKP.lt.maximum_rows) then  
c
         index = 1     
         do 40 i=1,6
            call write_line(index, outputstring, 
     1     STEP_KP_fluxes_ave(i), .true., .true.,
     1                dataformat_flag)                     
40     continue  
c       
          write(unit_STEPKP,100) mo, dy, yr, hr, mn, se,
     1   yr, doy_real,  
     1   half_delta_t, 
     1   outputstring(1:(index-1))     
c   
100 	   format(1x,i2'/'i2'/'i4,2x,i2':'i2':'i2,
     1      ', 'i4', 'f12.8', 'f12.4',',a<index-1>)
              nrow_STEPKP = nrow_STEPKP + 1       
      else
         print *, 'Number of rows exceeds maximum specified:'
     1            , maximum_rows
         write_output_STEPKP = .false.
         return
      end if
c
      write_output_STEPKP = .true.
      return
      end   ! end write_output_STEPKP
c
