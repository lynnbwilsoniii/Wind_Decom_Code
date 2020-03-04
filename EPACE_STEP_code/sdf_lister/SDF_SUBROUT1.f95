c
c    Subroutines for the program udf_lister
c    
c     Contents:
c
c             subroutine  initial_global
c
c             logical function get_config
c
c             logical function read_script
c
c             subroutine script_key
c
c
c
c************************************************
c
        subroutine initial_global
c
c************************************************
c
c     set initial values for global variables
c     6/16/98 by J. Dwyer
c 
c 
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      integer*4    i, j   ! loop indices
c
c    flags for output scripts
      do i = 1, Nscripts_max   
         reset_interval(i) = 0      
         hist_starting_index(i) = 1
         Noutput_lines(i) = 0
         do j = 1, 16
           pha_writeflag(i,j) = 0 
         end do
         do j = 1, Ncolumns_max
           script_sector_flag(i,j) = 'AVER'
           script_writeflag(i,j) = 0  ! default is don't write
           script_cutflag(i,j) = 0  ! default is don't cut
           script_tel_flag(i,j) = 'TEL1&2'  ! if not specified in script, then use both telescopes
         end do
      end do
      current_ROMboxfile = '  '
      current_ROMbox_time = 0         
      hist_size = 0
      dophafluxflag = 0  ! no pha flux scripts
      dohistflag = 0     ! no hist scripts
      expose_interval_flag = 0  ! initial value (pos integer = process time interval ,0 = do not)    
      mask_interval_flag = 0  ! initial value (1 = skip time interval ,0 = do not) 
c
      ave_spins(1) = ave_spins46    ! ave number of spins per 46 s mf                                 
      ave_spins(2) = ave_spins92    ! ave number of spins per 92 s mf
      spin_period = spin_period_default 
c     
      do i=1,18
        mdata_x(i) = 1    ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      end do              ! used to write omni rates
      mdata_x(39) = 1     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      mdata_x(40) = 1     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      mdata_x(41) = 1     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      mdata_x(42) = 1     ! VSE
      do i=19,25        
        mdata_x(i) = 2     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      end do
      do i=26,38
        mdata_x(i) = 4     ! 1 = 1x , 2 = 2x, 4 = 4x matrix values
      end do
c
      return
      end  ! end initial_global
c		
c
c
c
c*******************************************************
c
       logical function get_config(unit_cfg)
c
c*******************************************************
c
c    Open the configuration file and read instructions for running Sdf_lister
c    function returns .true. if file successfully opened and read,
c    .false. otherwise
c
c     6/16/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
c         
      integer*4     unit_cfg        ! file unit for config file                                   
      integer*4     iseconds        ! used in convert_to_sampextime call
      integer*4     i                    !   loop index 
      integer*4     doflag 
      character*80     scriptfilename
c  
      Nscripts = 0
      get_config = .true.
c
      open(unit=unit_cfg,name=configfile, status='old',     
     1    action='read',  err=600)  ! open the config file
c
100     format(a)
200     format(a)
300     format(a)
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,100,err=700)     file_extent_prefix   ! used in name of input data file
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,200,err=700)     outputfile  ! read output data file name
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,200,err=700)     outputdir  ! read output data file directory
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,300,err=700)    calib_file_tof  ! tof calibration file name
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,300,err=700)     calib_file_lo   ! lo calibration file name
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,300,err=700)     calib_file_hi   ! hi calibration file name
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,300,err=700)    calib_file_einctof  ! Einc vs tof calibration file name
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,300,err=700)    calib_file_geom  ! Geometrical factor calibration file name
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,300,err=700)    ROMboxhistoryfile  ! ROM box effic calibration file name
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,300,err=700)    calib_file_track  ! ROM box track eff calibration file name
      read(unit_cfg,*,err=700)     ! read comment line      
      read(unit_cfg,300,err=700)   expose_file  ! time interval mask file
      read(unit_cfg,*,err=700)     expose_flag  ! use time interval mask
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,300,err=700)   mask_file  ! time interval mask file
      read(unit_cfg,*,err=700)     mask_flag  ! use time interval mask 
      read(unit_cfg,*,err=700)     KPweight_flag   ! KP weighting flag (0: weight=1.0;1: weight=vse)
      read(unit_cfg,*,err=700)     Bdir_flag  ! B direction weighting flag (0=avg B vectors;1=avg unit vectors)
      read(unit_cfg,*,err=700)     randomize_flag	! 1 = randomize PHA tof and energy, 0 = don't randomize     
      read(unit_cfg,*,err=700)     raw_counts_flag  !  1= raw # counts only, 0 = calculate rates and intensities    
      read(unit_cfg,*,err=700)     select_calib_mode  ! 0 = non cal mode, 1 = cal mode, -1 = no cut
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,*,err=700)     istart_year,istart_day,
     1                             istart_hour,istart_min
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,*,err=700)     istop_year,istop_day,
     1                             istop_hour,istop_min
      read(unit_cfg,*,err=700)     ! read comment line
      read(unit_cfg,*,err=700)     maximum_rows  ! maximum # of data rows to write to output file
      read(unit_cfg,*,err=700)     dataformat_flag    ! Output data format (0=kaleidagraph format, 1=IDL format)
      read(unit_cfg,*,err=700)     fluence_flag  ! flux or fluence format (0=flux, 1=fluence)   
      read(unit_cfg,*,err=700)     ! read comment line
      do i = 1, 1000
          read(unit_cfg,*,err=700,end=500)  doflag, scriptfilename    ! script file name
          if (doflag.eq.1) then
             if (Nscripts.gt.Nscripts_max) 
     1            goto 550
             Nscripts = Nscripts +1
             scriptfile(Nscripts) = scriptfilename
          end if
      end do
      

c
                 
500		iseconds = 0  ! specify time only to within a minute
      call convert_to_sampextime(istart_year,istart_day,
     1   istart_hour,istart_min,iseconds,stime_start)       
      call convert_to_sampextime(istop_year,istop_day,
     1   istop_hour,istop_min,iseconds,stime_stop)     
c
      start_time = stime_start   ! used to get first ROM files
      close(unit=unit_cfg, err=600) 
      return        !  successful
c
550     type *, 'Maximum # of script files exceeded!' 
      close(unit=unit_cfg, err=600)
      get_config = .false. 
      return  
c
600      get_config = .false.   ! on error go to here
      type *, 'Error opening or closing configuration file:', 
     1     configfile
      return
700      get_config = .false.   ! on error go to here
      type *, 'Error reading configuration file:', configfile
      close(unit=unit_cfg, err=600) 
      return
      end  ! end get_config
c
c
c
c
c*******************************************************
c
       logical function read_script(unit_script,script_number)
c
c*******************************************************
c
c    Open the script file and reads instructions for writing output to Sdf_lister
c    function returns .true. if file successfully opened and read,
c    .false. otherwise
c
c     6/16/98 by J. Dwyer
c     4/21/00 by J. Dwyer added script_stateab_flag = -1 to hist input 
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
      include 'include_paths.inc'
c
c         
      real          histmin, histmax
      real          Emin,Emax,Mmin,Mmax
      real          amu, charge
      real          minvalue,maxvalue
      integer*4     Nhist, logflag
      integer*4     unit_script        ! file unit for script file  
      integer*4     script_number    ! number of script that is being processed 
      integer*4     Ncolumns         
      integer*4     doflag,writeflag, cutflag         
      integer*4     this_hist_size
      integer*4     this_phaflux_size  
      integer*4     i, k
      integer*4    slantflag
      character*41    title1
      character*20    title2
      character*4     sector_flag
      character*6     tel_flag                   
c  
      script_Ncolumns(script_number) = 0
      script_Nhistcolumns(script_number) = 0
      script_Nphafluxcolumns(script_number) = 0
      read_script = .true.
c
      open(unit=unit_script,name=adjustl(trim(control_path)) // scriptfile(script_number), 
     1    status='old',action='read',  err=600)  ! open the config file
c
100     format(a100)
200     format(a)
      read(unit_script,100,err=700) scriptheader(script_number,1)  ! comment line
      read(unit_script,100,err=700) scriptheader(script_number,2)  ! comment line
      read(unit_script,100,err=700) scriptheader(script_number,3)  ! comment line
      read(unit_script,100,err=700) scriptheader(script_number,4)  ! comment line
      read(unit_script,100,err=700) scriptheader(script_number,5)  ! comment line
      read(unit_script,100,err=700) scriptheader(script_number,6)  ! comment line 
      read(unit_script,100,err=700) scriptheader(script_number,7)  ! comment line
      read(unit_script,100,err=700) scriptheader(script_number,8)  ! comment line
      read(unit_script,100,err=700) scriptheader(script_number,9)  ! comment line
      read(unit_script,100,err=700) scriptheader(script_number,10)  ! comment line
      read(unit_script,*,err=700) script_output_type(script_number)  !  (1 = standard output, 2=pha data, 3 = histogram, 4 = PHA fluxes)
      read(unit_script,*,err=700)     ! read comment line
      read(unit_script,200,err=700)    
     1     script_file_prefix(script_number)    ! used in name of input data file 
c
c          
      if (script_output_type(script_number).eq.1) then  ! ROM data   
        read(unit_script,*,err=700)     ! read comment line
        read(unit_script,*,err=700) 
     1     output_sych_flag(script_number)   ! 0 = output asychronous outputs,1 = only output together when all scripts are ready 
        read(unit_script,*,err=700)    
     1   script_accum_t_flag(script_number), 
     1   script_idelta_t(script_number)   ! time interval in seconds to average matrix rates.  
        read(unit_script,*,err=700)    
     1   script_accum_Nr_flag(script_number), 
     1   script_idelta_Nr(script_number),  
     1   script_accum_rate_title(script_number)   ! accumulate until reach this many counts in this rate  
        read(unit_script,*,err=700)    
     1     script_stateab_flag(script_number)   ! accumulates either state A , state B or both
        read(unit_script,*,err=700)     ! read comment line     
        do i = 1, 10000
          read(unit_script,*,err=700,end=400)  
     1      doflag, writeflag,title1, sector_flag,
     1      tel_flag,cutflag, minvalue, maxvalue   
          if (doflag.ge.1) then
            if (script_Ncolumns(script_number).gt.Ncolumns_max) 
     1            goto 500  
               script_Ncolumns(script_number) = 
     1           script_Ncolumns(script_number) +1
               script_writeflag(script_number, 
     1             script_Ncolumns(script_number)) = writeflag
               script_titles_raw(script_number, 
     1             script_Ncolumns(script_number)) = title1   
               script_sector_flag(script_number,
     1             script_Ncolumns(script_number)) = sector_flag
               script_tel_flag(script_number,
     1             script_Ncolumns(script_number)) = tel_flag
               script_cutflag(script_number, 
     1             script_Ncolumns(script_number)) = cutflag
               script_minvalue(script_number, 
     1             script_Ncolumns(script_number)) = minvalue
               script_maxvalue(script_number, 
     1             script_Ncolumns(script_number)) = maxvalue
          end if  ! end if doflag
        end do   ! end i loop
      end if    ! end if type 1
c
c
      if (script_output_type(script_number).eq.2) then  ! PHA data 
         read(unit_script,*,err=700)    
     1     script_stateab_flag(script_number)   ! accumulates either state A , state B or both              
         read(unit_script,*,err=700)     ! read comment line     
         do i = 1, 10000
           read(unit_script,*,err=700,end=400)  
     1       doflag, writeflag,title1, 
     1       cutflag, minvalue, maxvalue     
           if (doflag.ge.1) then
             if (script_Ncolumns(script_number).gt.Ncolumns_max) 
     1            goto 500  
               script_Ncolumns(script_number) = 
     1           script_Ncolumns(script_number) +1
               script_writeflag(script_number, 
     1             script_Ncolumns(script_number)) = writeflag
               script_titles_raw(script_number, 
     1             script_Ncolumns(script_number)) = title1  
               script_cutflag(script_number, 
     1             script_Ncolumns(script_number)) = cutflag
               script_minvalue(script_number, 
     1             script_Ncolumns(script_number)) = minvalue
               script_maxvalue(script_number, 
     1             script_Ncolumns(script_number)) = maxvalue
          end if  ! end if doflag
        end do  ! end i loop
      end if     ! end if type 2
c 
c
      if (script_output_type(script_number).eq.3) then  ! histogram output
        script_stateab_flag(script_number) = -1
        this_hist_size = 1
        dohistflag = 1
        read(unit_script,*,err=700)     ! read comment line
        read(unit_script,*,err=700) 
     1     output_sych_flag(script_number)   ! 0 = output asychronous outputs,1 = only output together when all scripts are ready 
        read(unit_script,*,err=700)    
     1   script_accum_t_flag(script_number), 
     1   script_idelta_t(script_number)   ! time interval in seconds to average matrix rates.  
        read(unit_script,*,err=700)    
     1   script_accum_Nr_flag(script_number), 
     1   script_idelta_Nr(script_number),  
     1   script_accum_rate_title(script_number)   ! accumulate until reach this many counts in this rate  
        read(unit_script,*,err=700)    
     1   script_accum_Nh_flag(script_number), 
     1   script_idelta_Nh(script_number)   ! accumulate until reach this many counts in each histograms
c       
        read(unit_script,*,err=700)     ! read comment line 
        doflag = 0
        do while (doflag.ne.-1)
          read(unit_script,*,err=700)  
     1          doflag, title2, Nhist,histmin,histmax,logflag   
          if (doflag.eq.1) then
            if (script_Nhistcolumns(script_number).gt.
     1         Ncolumns_max) 
     1            goto 500 
              script_Nhistcolumns(script_number) = 
     1    script_Nhistcolumns(script_number) +1
                script_histtitles(script_number,
     1    script_Nhistcolumns(script_number)) = title2   
                script_Nhist(script_number,
     1    script_Nhistcolumns(script_number)) = Nhist
                script_histmin(script_number,
     1    script_Nhistcolumns(script_number)) = histmin
                script_histmax(script_number,
     1    script_Nhistcolumns(script_number)) = histmax
                script_logflag(script_number,
     1    script_Nhistcolumns(script_number)) = logflag
                this_hist_size = this_hist_size*Nhist                  
          end if  ! end if doflag  
        end do ! end while    
        read(unit_script,*,err=700)     ! read comment line 
        do i = 1, 10000
          read(unit_script,*,err=700,end=300)  
     1      doflag, cutflag, title1, minvalue, maxvalue    
          if (doflag.ge.1) then
            if (script_Ncolumns(script_number).gt.Ncolumns_max) 
     1         goto 500  
               script_Ncolumns(script_number) = 
     1           script_Ncolumns(script_number) +1
               script_cutflag(script_number, 
     1             script_Ncolumns(script_number)) = cutflag
               script_titles_raw(script_number, 
     1             script_Ncolumns(script_number)) = title1   
               script_minvalue(script_number, 
     1             script_Ncolumns(script_number)) = minvalue
               script_maxvalue(script_number, 
     1             script_Ncolumns(script_number)) = maxvalue
          end if! end if doflag
        end do  ! end i loop
300     continue
        submatrixsize(script_number,1) = 1
        do k = 2, script_Nhistcolumns(script_number)
          submatrixsize(script_number,k) = 
     1      script_Nhist(script_number,k-1)*
     1      submatrixsize(script_number,k-1)
        end do
        hist_starting_index(script_number)=
     1      hist_starting_index(script_number)+hist_size  
        hist_size = hist_size+this_hist_size
        if (hist_size.gt.Nhistbins_max) then
           type *, 'Histogram array overflow!'
           type *, 'Reduce # or size of histograms.'
           type *, 'stopping at read_script.'
           stop
        end if
      end if  ! end if type 3
c
c
      if (script_output_type(script_number).eq.4) then  ! PHA fluxes
        dophafluxflag = 1
        read(unit_script,*,err=700)     ! read comment line
        read(unit_script,*,err=700) 
     1     output_sych_flag(script_number)   ! 0 = output asychronous outputs,1 = only output together when all scripts are ready 
        read(unit_script,*,err=700)    
     1   script_accum_t_flag(script_number), 
     1   script_idelta_t(script_number)   ! time interval in seconds to average matrix rates.  
        read(unit_script,*,err=700)    
     1   script_accum_Nr_flag(script_number), 
     1   script_idelta_Nr(script_number),  
     1   script_accum_rate_title(script_number)   ! accumulate until reach this many counts in this rate  
        read(unit_script,*,err=700)   
     1   script_accum_Nphaf_flag(script_number), 
     1   script_idelta_Nphaf(script_number)   ! accumulate until reach this many counts in each pha flux array  
c                     
        read(unit_script,*,err=700)     ! read comment line 
        doflag = 0
        do while (doflag.ne.-1)
          read(unit_script,*,err=700)  
     1       doflag, title2, Emin, Emax, Mmin, 
     1         Mmax, amu, charge, slantflag,tel_flag
          if (doflag.eq.1) then
            if (script_Nphafluxcolumns(script_number).gt.
     1         Ncolumns_max) 
     1            goto 500  
            script_Nphafluxcolumns(script_number) = 
     1  script_Nphafluxcolumns(script_number) +1
            script_phafluxtitles(script_number,
     1  script_Nphafluxcolumns(script_number)) = title2   
            script_phaflux_Ebins(script_number,1,
     1  script_Nphafluxcolumns(script_number)) = Emin 
            script_phaflux_Ebins(script_number,2,
     1  script_Nphafluxcolumns(script_number)) = Emax   
            script_phaflux_Mbins(script_number,1,
     1  script_Nphafluxcolumns(script_number)) = Mmin 
            script_phaflux_Mbins(script_number,2,
     1  script_Nphafluxcolumns(script_number)) = Mmax
            phaflux_m(script_number,
     1  script_Nphafluxcolumns(script_number)) = amu
            phaflux_z(script_number,
     1  script_Nphafluxcolumns(script_number)) = charge 
            PHA_slant_flag(script_number,
     1  script_Nphafluxcolumns(script_number)) = slantflag
            script_tel_flag(script_number,
     1  script_Nphafluxcolumns(script_number)) = tel_flag
          end if   ! end if doflag  
        end do  ! end while  
        read(unit_script,*,err=700)     ! read comment line 
        do i = 1, 10000
          read(unit_script,*,err=700,end=350)  
     1      doflag, cutflag, title1, minvalue, maxvalue    
          if (doflag.ge.1) then
            if (script_Ncolumns(script_number).gt.Ncolumns_max) 
     1         goto 500  
               script_Ncolumns(script_number) = 
     1           script_Ncolumns(script_number) +1
               script_cutflag(script_number, 
     1             script_Ncolumns(script_number)) = cutflag
               script_titles_raw(script_number, 
     1             script_Ncolumns(script_number)) = title1   
               script_minvalue(script_number, 
     1             script_Ncolumns(script_number)) = minvalue
               script_maxvalue(script_number, 
     1             script_Ncolumns(script_number)) = maxvalue
          end if ! end if doflag
        end do  ! end i loop
350     continue        
      end if  ! end if type 4
c
c
      if (script_output_type(script_number).eq.5) then  ! OMNI rates     
         read(unit_script,*,err=700)    
     1     script_stateab_flag(script_number)   ! accumulates either state A , state B or both                                   
        read(unit_script,*,err=700)     ! read comment line 
        do i = 1, 10000
          read(unit_script,*,err=700,end=400)  
     1       doflag, writeflag,title1, 
     1       cutflag, minvalue, maxvalue     
          if (doflag.ge.1) then
             if (script_Ncolumns(script_number).gt.Ncolumns_max) 
     1            goto 500  
               script_Ncolumns(script_number) = 
     1           script_Ncolumns(script_number) +1
               script_writeflag(script_number, 
     1             script_Ncolumns(script_number)) = writeflag
               script_titles_raw(script_number, 
     1             script_Ncolumns(script_number)) = title1  
               script_cutflag(script_number, 
     1             script_Ncolumns(script_number)) = cutflag
               script_minvalue(script_number, 
     1             script_Ncolumns(script_number)) = minvalue
               script_maxvalue(script_number, 
     1             script_Ncolumns(script_number)) = maxvalue
          end if  ! end if doflag
        end do  ! end i loop
      end if      
c
400      close(unit=unit_script, err=600) 
      return        !  successful
c
500     type *, 'Maximum lines exceeded in script file #', 
     1     script_number 
       type *, scriptfile(script_number)
       close(unit=unit_script, err=600) 
      stop
600      read_script = .false.   ! on error go to here
      type *, 'Error opening or closing script file #', 
     1     script_number
      type *, scriptfile(script_number)
      return
700       read_script = .false.   ! on error go to here
      type *, 'Error reading script file #', 
     1     script_number
      type *, scriptfile(script_number)
      close(unit=unit_script, err=600) 
      return
      end  ! end  read_script 
c
c
c
c
c****************************************************
c
       subroutine script_key
c
c****************************************************
c
c      ROM box, discriminator and PHA titles, etc. for the
c      scripts
c
c      6/16/98 by J. Dwyer      
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c      
c
      mdata_key(1) = 'STATE               '
      mdata_key(2) = 'H1                  '
      mdata_key(3) = 'H2                  '
      mdata_key(4) = 'H3                  '
      mdata_key(5) = 'H4                  '
      mdata_key(6) = 'H5                  '
      mdata_key(7) = 'H6                  '
      mdata_key(8) = 'H7                  '
      mdata_key(9) = 'H8                  '
      mdata_key(10) = 'H9                 '
      mdata_key(11) = 'H10                '
      mdata_key(12) = 'He1                '
      mdata_key(13) = 'He2                '
      mdata_key(14) = 'He3                '
      mdata_key(15) = 'He4                '
      mdata_key(16) = 'He5                '
      mdata_key(17) = 'He6                '
      mdata_key(18) = 'He7                '
      mdata_key(19) = 'CNO1               '
      mdata_key(20) = 'CNO2               '
      mdata_key(21) = 'CNO3               '
      mdata_key(22) = 'CNO4               '
      mdata_key(23) = 'CNO5               '
      mdata_key(24) = 'CNO6               '
      mdata_key(25) = 'CNO7               '
      mdata_key(26) = 'NeS1               '
      mdata_key(27) = 'NeS2               '
      mdata_key(28) = 'NeS3               '
      mdata_key(29) = 'NeS4               '
      mdata_key(30) = 'NeS5               '
      mdata_key(31) = 'NeS6               '
      mdata_key(32) = 'NeS7               '
      mdata_key(33) = 'Fe1                '
      mdata_key(34) = 'Fe2                '
      mdata_key(35) = 'Fe3                '
      mdata_key(36) = 'Fe4                '
      mdata_key(37) = 'Fe5                '
      mdata_key(38) = 'Fe6                '
      mdata_key(39) = 'JUNK1              '
      mdata_key(40) = 'JUNK2              '
      mdata_key(41) = 'JUNK3              '
      mdata_key(42) = 'VSE                '
c
      PHA_key(1) = 'Einc (MeV per nuc)'  
      PHA_key(2) = 'Mass (nuc)'
      PHA_key(3) = 'Essd (MeV)'
      PHA_key(4) = 'TOF (nsec)'  
      PHA_key(5) = 'SSD (chan)'
      PHA_key(6) = 'TOF (chan)'
      PHA_key(7) = 'ramp (1=lo gain)'
      PHA_key(8) = 'telescope (1=D1)'
      PHA_key(9) = 'calib mode (1=cal)'
      PHA_key(10) = 'slant (1=fired)'
      PHA_key(11) = 'SSD2 (1=fired)'
      PHA_key(12) = 'state A or B (1=A)'
      PHA_key(13) = 'ROM box'
      PHA_key(14) = 'spin (chan)'
      PHA_key(15) = 'sector (chan)'
      PHA_key(16) = 'efficiency'                          
c
      disc_key(1) = 'VSEBAR              '
      disc_key(2) = 'START               '
      disc_key(3) = 'STOP                '
      D_key =       'D                   '       
c    
      kp_key(1) =  'B (nT)'
      kp_key(2) =  'B theta (deg)' 
      kp_key(3) =  'B phi (deg)' 
      kp_key(4) =  'B rms (nT)' 
      kp_key(5) =  'R (RE)' 
      kp_key(6) =  'Xgse (RE)' 
      kp_key(7) =  'Ygse (RE)' 
      kp_key(8) =  'Zgse (RE)'
      kp_key(9) =  'Vsw (km per sec)' 
      kp_key(10) =  'Vx (km per sec)'
      kp_key(11) =  'Vy (km per sec)' 
      kp_key(12) =  'Vz (km per sec)'
      kp_key(13) =  'Np (per cc)' 
      kp_key(14) =  'Vth (km per sec)'
      kp_key(15) =  'spin rate'
      kp_key(16) =  '3dp e1'
      kp_key(17) =  '3dp e2'
      kp_key(18) =  '3dp e3'
      kp_key(19) =  '3dp e4'
      kp_key(20) =  '3dp e5'
      kp_key(21) =  '3dp e6'
      kp_key(22) =  '3dp e7'
      kp_key(23) =  '3dp ion1'
      kp_key(24) =  '3dp ion2'
      kp_key(25) =  '3dp ion3'
      kp_key(26) =  '3dp ion4'
      kp_key(27) =  '3dp ion5'
      kp_key(28) =  '3dp ion6'
      kp_key(29) =  '3dp ion7'
      kp_key(30) =  'APEB1'
      kp_key(31) =  'APEB2'
      kp_key(32) =  'APEB3'
      kp_key(33) =  'APEB4'
      kp_key(34) =  'APEB5'
      kp_key(35) =  'LEMT1'
      kp_key(36) =  'LEMT2'
      kp_key(37) =  'LEMT3'    
c       
      STEPkp_key(1) = 'STEPKP He 3'
      STEPkp_key(2) = 'STEPKP He 5'
      STEPkp_key(3) = 'STEPKP CNO 3'
      STEPkp_key(4) = 'STEPKP CNO 5'
      STEPkp_key(5) = 'STEPKP Fe 3'
      STEPkp_key(6) = 'STEPKP Fe 6'  
c       
      HK_key(1) = 'STEPHV' 
      HK_key(2) = 'STEPtherm'
      HK_key(3) = 'STEPt1'
      HK_key(4) = 'STEPt2'
      HK_key(5) = 'STEPt3'
      HK_key(6) = 'MFnum'  
c
      return
      end   ! end script_key
c
c
