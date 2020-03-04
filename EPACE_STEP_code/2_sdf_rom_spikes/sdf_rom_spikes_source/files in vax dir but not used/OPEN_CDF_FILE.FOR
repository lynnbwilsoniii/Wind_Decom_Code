c
c
c      subroutine open_spha_file
c
c      subroutine open_mfi_file
c
c      subroutine open_swe_file
c
c      subroutine open_3dp_file
c
c      subroutine open_epact_file
c
c
c***************************************************
c
      subroutine open_spha_file(isotime,
     1                               directoryflag)  
c
c***************************************************
c
c    opens spha cdf files for reading
c    2/11/97 by J. Dwyer
c    modifications:
c             3/3/97 by J. Dwyer changed print statements
c             3/19/97 by J. Dwyer added status.eq.future_cdf
c
      include 'L0_include.for'
      include 'cdf$inc:cdf.inc'     ! include file for CDF library functions    
c
      integer  directoryflag  ! 1 = kp directory is on $step_data; 0 = UMSMS::WIND_KP_DEV
      integer  lastindex  ! used to write kp file name version #
      character*20    isotime
      character*60    filename
      integer*4  num_dims, dim_sizes, encoding, majority,
     1      num_vars, num_attrs, status    ! used in CDF calls      
      character message*(cdf_statustext_len)
c
      if (directoryflag.eq.0) then
          filename(1:36)=
     1   'UMSMS::WIND_KP_DEV:[SPHA]WI_K0_SPHA_'
          filename(37:40)=isotime(1:4)
          filename(41:42)=isotime(6:7)
          filename(43:44)=isotime(9:10)
          filename(45:48)='_V01'
          lastindex = 48
      end if
c
      if (directoryflag.eq.1) then
        filename(1:40)=
     1   '$step_data:[wind.kpdata.spha]WI_K0_SPHA_'
        filename(41:44)=isotime(1:4)
        filename(45:46)=isotime(6:7)
        filename(47:48)=isotime(9:10)
        filename(49:52)='_V01'
        lastindex = 52
      end if
c
      if (directoryflag.eq.2) then
        filename(1:28)=
     1   'STPOPT1:[kp.spha]WI_K0_SPHA_'
        filename(29:32)=isotime(1:4)
        filename(33:34)=isotime(6:7)
        filename(35:36)=isotime(9:10)
        filename(37:40)='_V01'
        lastindex = 40
      end if
c      
      call cdf_open(filename,spha_id, status)
      if (status.le.cdf_warn) then
       do 10 i=2,9
         write (filename(lastindex:lastindex),'(i1)') i
         call cdf_open(filename,spha_id, status)
         if (status.eq.cdf_ok) i=10
10     continue
      endif
c  Get CDF file ID, maximum number of records, etc., from CDF file.
c
       if ((status.eq.cdf_ok).or.
     1     (status.eq.future_cdf)) then
         print *, 'opened spha file: ',filename
         write(30,*) 'opened spha file: ',filename
         spha_open=.TRUE.
         spha_rec = 1
         call cdf_inquire(spha_id, num_dims,
     1          dim_sizes, encoding, majority,
     1          spha_max_rec, num_vars, num_attrs, status)
         if ((status.ne.cdf_ok).and. 
     1     (status.ne.future_cdf)) then
           print *,
     1       'cdf_inquire not normal for spha - will not be used'
           write(30,*) 
     1       'cdf_inquire not normal for spha - will not be used'
           call cdf_error(status,message)
           print *, filename
           write(30,*) filename
           print *,message
           write(30,*) message
           call cdf_close (spha_id,status)
           print *, 'closed spha file: ',filename
           write(30,*) 'closed spha file: ',filename
           spha_open=.false.
           goto 20
         endif
         vn_spha_epoch = cdf_var_num(spha_id,'Epoch')
         vn_spha_asr = cdf_var_num(spha_id,'AVG_SPIN_RATE')
       else   
         print *, isotime
         write(30,*) isotime
         print *, filename
         write(30,*) filename
         print *,'spha file did not open normally - will not be used'
         write(30,*) 'spha file did not open normally-will not be used'
         call cdf_error(status,message)
         print *,message
         write(30,*) message
         call cdf_close (spha_id,status)
         print *, 'closed spha file: ',filename
         write(30,*) 'closed spha file: ',filename
         spha_open=.false.
      endif
c
c
20      return
      end   ! end open_spha_file
c
c
c
c***************************************************
c
      subroutine open_mfi_file(isotime, 
     1                               directoryflag)  
c
c***************************************************
c
c    opens mfi cdf files for reading
c    2/11/97 by J. Dwyer
c    modifications:
c             3/3/97 by J. Dwyer changed print statements
c             3/12/97 by J. Dwyer changed close to call cdf_close (mfi_id,status)
c             3/19/97 by J. Dwyer added status.eq.future_cdf
c
      include 'L0_include.for'
      include 'cdf$inc:cdf.inc'     ! include file for CDF library functions    
c
      integer  directoryflag  ! 1 = kp directory is on $step_data; 0 = UMSMS::WIND_KP_DEV
      integer  lastindex  ! used to write kp file name version #
      character*20    isotime
      character*60    filename
      integer*4  num_dims, dim_sizes, encoding, majority,
     1      num_vars, num_attrs, status    ! used in CDF calls      
      character message*(cdf_statustext_len)    
c
      if (directoryflag.eq.0) then
         filename(1:34) =
     1  'UMSMS::WIND_KP_DEV:[MFI]WI_K0_MFI_'
         filename(35:38)=isotime(1:4) 
         filename(39:40)=isotime(6:7)
         filename(41:42)=isotime(9:10)
         filename(43:46)='_V01'
         lastindex = 46
      end if
c
      if (directoryflag.eq.1) then
         filename(1:38) =
     1  '$step_data:[wind.kpdata.mfi]WI_K0_MFI_'
         filename(39:42)=isotime(1:4) 
         filename(43:44)=isotime(6:7)
         filename(45:46)=isotime(9:10)
         filename(47:50)='_V01'
         lastindex = 50
      end if
c
      if (directoryflag.eq.2) then
         filename(1:26) =
     1  'STPOPT1:[kp.mfi]WI_K0_MFI_'
         filename(27:30)=isotime(1:4) 
         filename(31:32)=isotime(6:7)
         filename(33:34)=isotime(9:10)
         filename(35:38)='_V01'
         lastindex = 38
      end if
c
      call CDF_lib (OPEN_, CDF_, filename, mfi_id, NULL_, status)
      if (status.le.cdf_warn) then
       do 10 i=2,9
        write (filename(lastindex:lastindex),'(i1)') i
        call CDF_lib (OPEN_, CDF_, filename, mfi_id, NULL_, status)
        if (status.eq.cdf_ok) i=10
10     continue
      endif
c
       if ((status.eq.cdf_ok).or.
     1     (status.eq.future_cdf)) then
         print *, 'opened mfi file: ',filename
         write(30,*)  'opened mfi file: ',filename
         mfi_open=.TRUE.
         mfi_rec = 1
         call CDF_lib (SELECT_, CDF_, mfi_id,
     1                         GET_, zVARs_MAXREC_, mfi_max_rec,
     1                         NULL_, status)
c       print *, 'mfi_max_rec = ', mfi_max_rec
c       write(30,*) 'mfi_max_rec = ', mfi_max_rec
         if ((status.ne.cdf_ok).and. 
     1     (status.ne.future_cdf)) then
           print *,
     1       'cdf_inquire not normal for mfi - will not be used'
           write(30,*) 
     1       'cdf_inquire not normal for mfi - will not be used'
           call cdf_error(status,message)
           print *, filename
          write(30,*) filename
           print *,message
           write(30,*) message
c           call CDF_lib (CLOSE_,CDF_, mfi_id)
           call cdf_close (mfi_id,status)
           print *, 'closed mfi file: ',filename
           write(30,*)  'closed mfi file: ',filename
           mfi_open=.false.
           goto 20
         end if
         CALL CDF_lib (SELECT_, CDF_, mfi_id,
     1                     GET_, zVAR_NUMBER_, 'Epoch', 
     1                     vn_mfi_epoch,
     1                     zVAR_NUMBER_, 'BGSEc', vn_mfi_bgse,
     1                     zVAR_NUMBER_, 'RMS', vn_mfi_rms,
     1                     zVAR_NUMBER_, 'PGSE', vn_mfi_position,
     1                     NULL_, status)
         if ((status.ne.cdf_ok).and. 
     1     (status.ne.future_cdf)) then
            print *,'problem setting up mfi - will not be used'
            write(30,*) 'problem setting up mfi-will not be used'
            call cdf_error(status,message)
            print *,message
            write(30,*) message
c            call CDF_lib (CLOSE_,CDF_, mfi_id)
            call cdf_close (mfi_id,status)
            print *, 'closed mfi file: ',filename
            write(30,*)  'closed mfi file: ',filename
            mfi_open=.false.
         end if
      else  
        print *, isotime
        write(30,*) isotime 
        print *, filename
        write(30,*) filename
        print *,'mfi file did not open normally - will not be used'
        write(30,*) 'mfi file did not open normally-will not be used'
        call cdf_error(status,message)
        print *,message
        write(30,*) message
c        call CDF_lib (CLOSE_,CDF_, mfi_id)
        call cdf_close (mfi_id,status)
        print *, 'closed mfi file: ',filename
        write(30,*)  'closed mfi file: ',filename
        mfi_open=.false.
      end if
c
c
20      return
c
      end    ! end open_mfi_file
c
c
c
c
c***************************************************
c
      subroutine open_swe_file(isotime,
     1                               directoryflag)  
c
c***************************************************
c
c    opens swe cdf files for reading
c    2/11/97 by J. Dwyer
c    modifications:
c             3/3/97 by J. Dwyer changed print statements
c             3/19/97 by J. Dwyer added status.eq.future_cdf
c
      include 'L0_include.for'
      include 'cdf$inc:cdf.inc'     ! include file for CDF library functions    
c
      integer  directoryflag  ! 1 = kp directory is on $step_data; 0 = UMSMS::WIND_KP_DEV
      integer  lastindex  ! used to write kp file name version #
      character*20    isotime
      character*60    filename
      integer*4  num_dims, dim_sizes, encoding, majority,
     1      num_vars, num_attrs, status    ! used in CDF calls      
      character message*(cdf_statustext_len)
c
      if (directoryflag.eq.0) then
         filename(1:34)=
     1     'UMSMS::WIND_KP_DEV:[SWE]WI_K0_SWE_'
         filename(35:38)=isotime(1:4)
         filename(39:40)=isotime(6:7)
         filename(41:42)=isotime(9:10)
         filename(43:46)='_V01'
         lastindex = 46
      end if
c
      if (directoryflag.eq.1) then
         filename(1:38)=
     1     '$step_data:[wind.kpdata.swe]WI_K0_SWE_'
         filename(39:42)=isotime(1:4)
         filename(43:44)=isotime(6:7)
         filename(45:46)=isotime(9:10)
         filename(47:50)='_V01'
         lastindex = 50
      end if
c
      if (directoryflag.eq.2) then
         filename(1:26)=
     1     'STPOPT1:[kp.swe]WI_K0_SWE_'
         filename(27:30)=isotime(1:4)
         filename(31:32)=isotime(6:7)
         filename(33:34)=isotime(9:10)
         filename(35:38)='_V01'
         lastindex = 38
      end if
c
      call cdf_open(filename, swe_id, status)
      if (status.le.cdf_warn) then
       do 10 i=2,9
        write (filename(lastindex:lastindex),'(i1)') i
        call cdf_open(filename,swe_id,status)
        if (status.eq.cdf_ok) i=10
10     continue
      endif
c  Get CDF file ID, maximum number of records, etc., from CDF file.
c
       if ((status.eq.cdf_ok).or.
     1     (status.eq.future_cdf)) then
         print *, 'opened swe file: ',filename
         write(30,*) 'opened swe file: ',filename
         swe_open=.TRUE.
         swe_rec = 1    
         call cdf_inquire(swe_id, num_dims,
     1          dim_sizes, encoding, majority,
     1          swe_max_rec, num_vars, num_attrs, status)
         if ((status.ne.cdf_ok).and. 
     1     (status.ne.future_cdf)) then
           print *,
     1       'cdf_inquire not normal for swe - will not be used'
           write(30,*) 
     1       'cdf_inquire not normal for swe - will not be used'
           call cdf_error(status,message)
           print *, filename
           write(30,*) filename
           print *,message
           write(30,*) message
           call cdf_close (swe_id,status)
           print *, 'closed swe file: ',filename
           write(30,*) 'closed swe file: ',filename
           swe_open=.false.
           goto 20
         endif
         vn_swe_epoch = cdf_var_num(swe_id,'Epoch')
         vn_swe_vgse = cdf_var_num(swe_id,'V_GSE')
         vn_swe_vth = cdf_var_num(swe_id,'THERMAL_SPD')
         vn_swe_protdens = cdf_var_num(swe_id,'Np')
         vn_swe_position = cdf_var_num(swe_id,'SC_pos_gse')
       else   
         print *, isotime
         write(30,*) isotime
         print *, filename
         write(30,*) filename
         print *,'swe file did not open normally - will not be used'
         write(30,*) 'swe file did not open normally-will not be used'
         call cdf_error(status,message)
         print *,message
         write(30,*) message
         call cdf_close (swe_id,status)
         print *, 'closed swe file: ',filename
         write(30,*) 'closed swe file: ',filename
         swe_open=.false.
      endif
c
c
20      return
c
      end    ! end open_swe_file
c
c
c
c
c***************************************************
c
      subroutine open_3dp_file(isotime,
     1                               directoryflag)  
c
c***************************************************
c
c    opens 3dp cdf files for reading
c    2/11/97 by J. Dwyer
c    modifications:
c             3/3/97 by J. Dwyer changed print statements
c             3/19/97 by J. Dwyer added status.eq.future_cdf
c
      include 'L0_include.for'
      include 'cdf$inc:cdf.inc'     ! include file for CDF library functions    
c
      integer  directoryflag  ! 1 = kp directory is on $step_data; 0 = UMSMS::WIND_KP_DEV
      integer  lastindex  ! used to write kp file name version #
      character*20    isotime
      character*60    filename
      integer*4  num_dims, dim_sizes, encoding, majority,
     1      num_vars, num_attrs, status    ! used in CDF calls      
      character message*(cdf_statustext_len)
c
      if (directoryflag.eq.0) then
         filename(1:34)=
     1     'UMSMS::WIND_KP_DEV:[3DP]WI_K0_3dp_'
         filename(35:38)=isotime(1:4)
         filename(39:40)=isotime(6:7)
         filename(41:42)=isotime(9:10)
         filename(43:46)='_V01'
         lastindex = 46
      end if
c
      if (directoryflag.eq.1) then
         filename(1:38)=
     1     '$step_data:[wind.kpdata.3dp]WI_K0_3dp_'
         filename(39:42)=isotime(1:4)
         filename(43:44)=isotime(6:7)
         filename(45:46)=isotime(9:10)
         filename(47:50)='_V01'
         lastindex = 50
      end if
c
      if (directoryflag.eq.2) then
         filename(1:26)=
     1     'STPOPT1:[kp.3dp]WI_K0_3dp_'
         filename(27:30)=isotime(1:4)
         filename(31:32)=isotime(6:7)
         filename(33:34)=isotime(9:10)
         filename(35:38)='_V01'
         lastindex = 38
      end if
c
      call cdf_open(filename, tdp_id, status)
      if (status.le.cdf_warn) then
       do 10 i=2,9
        write (filename(lastindex:lastindex),'(i1)') i
        call cdf_open(filename,tdp_id,status)
        if (status.eq.cdf_ok) i=10
10     continue
      endif
c  Get CDF file ID, maximum number of records, etc., from CDF file.
c
       if ((status.eq.cdf_ok).or.
     1     (status.eq.future_cdf)) then
         print *, 'opened 3dp file: ',filename
         write(30,*) 'opened 3dp file: ',filename
         tdp_open=.TRUE.
         tdp_rec = 1    
         call cdf_inquire(tdp_id, num_dims,
     1          dim_sizes, encoding, majority,
     1          tdp_max_rec, num_vars, num_attrs, status)
         if ((status.ne.cdf_ok).and. 
     1     (status.ne.future_cdf)) then
           print *,
     1       'cdf_inquire not normal for 3dp - will not be used'
           write(30,*) 
     1       'cdf_inquire not normal for 3dp - will not be used'
           call cdf_error(status,message)
           print *, filename
           write(30,*) filename
           print *,message
           write(30,*) message
           call cdf_close (tdp_id,status)
           print *, 'closed 3dp file: ',filename
           write(30,*) 'closed 3dp file: ',filename
           tdp_open=.false.
           goto 20
         endif
         vn_tdp_epoch = cdf_var_num(tdp_id, 'Epoch')
         vn_tdp_eflux = cdf_var_num(tdp_id, 'elect_flux')
         vn_tdp_ionflux = cdf_var_num(tdp_id, 'ion_flux')       
       else   
         print *, isotime
         write(30,*) isotime
         print *, filename
         write(30,*) filename
         print *,'3dp file did not open normally - will not be used'
         write(30,*) '3dp file did not open normally-will not be used'
         call cdf_error(status,message)
         print *,message
         write(30,*) message
         call cdf_close (tdp_id,status)
         print *, 'closed 3dp file: ',filename
         write(30,*) 'closed 3dp file: ',filename
         tdp_open=.false.
      endif
c
c
20      return
c
      end     ! end open_3dp_file
c
c
c
c
c***************************************************
c
      subroutine open_epa_file(isotime,
     1                               directoryflag)  
c
c***************************************************
c
c    opens epact cdf files for reading
c    2/11/97 by J. Dwyer
c             modifications:
c             3/19/97 by J. Dwyer added status.eq.future_cdf
c
      include 'L0_include.for'
      include 'cdf$inc:cdf.inc'     ! include file for CDF library functions    
c
      integer  directoryflag  ! 1 = kp directory is on $step_data; 0 = UMSMS::WIND_KP_DEV
      integer  lastindex  ! used to write kp file name version #
      character*20    isotime
      character*60    filename
      integer*4  num_dims, dim_sizes, encoding, majority,
     1      num_vars, num_attrs, status    ! used in CDF calls      
      character message*(cdf_statustext_len)
c
      if (directoryflag.eq.0) then
         filename(1:34)=
     1     'UMSMS::WIND_KP_DEV:[EPA]WI_K0_EPA_'
         filename(35:38)=isotime(1:4)
         filename(39:40)=isotime(6:7)
         filename(41:42)=isotime(9:10)
         filename(43:46)='_V01'
         lastindex = 46
      end if
c
      if (directoryflag.eq.1) then
         filename(1:38)=
     1     '$step_data:[wind.kpdata.epa]WI_K0_EPA_'
         filename(39:42)=isotime(1:4)
         filename(43:44)=isotime(6:7)
         filename(45:46)=isotime(9:10)
         filename(47:50)='_V01'
         lastindex = 50
      end if
c
      if (directoryflag.eq.2) then
         filename(1:26)=
     1     'STPOPT1:[kp.epa]WI_K0_EPA_'
         filename(27:30)=isotime(1:4)
         filename(31:32)=isotime(6:7)
         filename(33:34)=isotime(9:10)
         filename(35:38)='_V01'
         lastindex = 38
      end if
c
      call cdf_open(filename, epa_id, status)
      if (status.le.cdf_warn) then
       do 10 i=2,9
        write (filename(lastindex:lastindex),'(i1)') i
        call cdf_open(filename,epa_id,status)
        if (status.eq.cdf_ok) i=10
10     continue
      endif
c  Get CDF file ID, maximum number of records, etc., from CDF file.
c
       if ((status.eq.cdf_ok).or.
     1     (status.eq.future_cdf)) then
        print *, 'opened epact file: ',filename
        write(30,*) 'opened epact file: ',filename
         epa_open=.TRUE.
         epa_rec = 1    
         call cdf_inquire(epa_id, num_dims,
     1          dim_sizes, encoding, majority,
     1          epa_max_rec, num_vars, num_attrs, status)
         if ((status.ne.cdf_ok).and. 
     1     (status.ne.future_cdf)) then
           print *,
     1       'cdf_inquire not normal for epa - will not be used'
           write(30,*) 
     1       'cdf_inquire not normal for epa - will not be used'
           call cdf_error(status,message)
           print *, filename
           write(30,*) filename
           print *,message
           write(30,*) message
           call cdf_close (epa_id,status)
           print *, 'closed epact file: ',filename
           write(30,*) 'closed epact file: ',filename
           epa_open=.false.
           goto 20
         endif
         vn_epa_epoch = cdf_var_num(epa_id,'Epoch')
         vn_epa_apeb1 = cdf_var_num(epa_id,'APEB1')
         vn_epa_apeb2 = cdf_var_num(epa_id,'APEB2')
         vn_epa_apeb3 = cdf_var_num(epa_id,'APEB3')
         vn_epa_apeb4 = cdf_var_num(epa_id,'APEB4')
         vn_epa_apeb5 = cdf_var_num(epa_id,'APEB5')
         vn_epa_lemt1 = cdf_var_num(epa_id,'LEMT1')
         vn_epa_lemt2 = cdf_var_num(epa_id,'LEMT2')
         vn_epa_lemt3 = cdf_var_num(epa_id,'LEMT3')
       else   
         print *, isotime
         write(30,*) isotime
         print *, filename
         write(30,*) filename
         print *,'epa file did not open normally - will not be used'
         write(30,*) 'epa file did not open normally-will not be used'
         call cdf_error(status,message)
         print *,message
         write(30,*) message
         call cdf_close (epa_id,status)
          print *, 'closed epact file: ',filename
          write(30,*) 'closed epact file: ',filename
         epa_open=.false.
      endif
c
c
20      return
c
      end    ! end open_epa_file
c
