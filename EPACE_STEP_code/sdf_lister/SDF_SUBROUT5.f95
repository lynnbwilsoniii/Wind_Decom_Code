c
c    Subroutines for the program udf_lister
c
c     Contents:
c
c             subroutine get_exposure
c
c             subroutine sum_kp
c
c             subroutine ave_kp
c
c             subroutine ave_kp_1mf
c
c             subroutine calculate_rates
c
c             subroutine calculate_counts
c
c             subroutine calculate_rates_1mf
c
c             subroutine calculate_counts_1mf
c
c
c
c
c*****************************************************
c
      subroutine get_exposure(script_number)
c
c*****************************************************
c
c     calculates accumulated exposure
c
c     6/17/98 by J. Dwyer
c      12/22/98 changed state from flux to rate
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real    ehilo(41,2)    ! delta E
      real    delta_tmf, delta_t1x  ! livetimes for 1 mf
      real    delta_t2x
      real    delta_t4x
      integer*4   script_number
      integer*4   i,j,k
c
      do 5 i=1,41
        do 5 j=1,2
           ehilo(i,j) = ehi(i,j)-elo(i,j)
           exposure_1mf(script_number,i,j) =  0.0
5      continue
c
      delta_tmf = spin_period*nspins   ! elapsed time in 1 mf
      delta_t1x = delta_tmf/2.0     ! elapsed time in state A in 1 mf
      delta_t2x = delta_tmf+
     1     spin_period*ave_spins(mf_type)
      delta_t4x = delta_tmf+
     1     3.0*spin_period*ave_spins(mf_type)
      if ((script_stateab_flag(script_number).eq.0).or.
     1   (script_stateab_flag(script_number).eq.1))  then  ! if selecting only state A or B
        delta_tmf = delta_tmf/2.0    ! use for state1, state2 disc rates and junk rates
        delta_t2x = delta_t2x/2.0
        delta_t4x = delta_t4x/2.0
      end if
c  1x exposures
      time_interval(script_number) =
     1    time_interval(script_number)+
     1         spin_period*nspins    ! record time elapsed
      do 7 j=1,2
          exposure_1mf(script_number,1,j) = delta_tmf    ! state rates
          exposure_1mf(script_number,42,j) = delta_tmf    ! VSE rates
7      continue
      do 10 i=2,18
         do 10 j=1,2
            exposure_1mf(script_number,i,j) = geofactor(j)*
     1       ehilo(i,j)*effic(i,j)*delta_t1x
10      continue
      do 15 i=39,41
          do 15 j=1,2
15           exposure_1mf(script_number,i,j) = geofactor(j)*
     1        ehilo(i,j)*effic(i,j)*delta_tmf    ! junk rates
c
c   2x exposures
      if ((phase.eq.0).or.(phase.eq.2)) then
         time_interval_2x(script_number,1) =
     1       time_interval_2x(script_number,1)+
     1         spin_period*nspins+
     1         spin_period*ave_spins(mf_type)     ! record time elapsed
         do 20 i=19,21
            do 20 j=1,2
              exposure_1mf(script_number,i,j) =geofactor(j)*
     1         ehilo(i,j)*effic(i,j)*delta_t2x
20    continue
         exposure_1mf(script_number,22,2) =geofactor(2)*
     1       ehilo(22,2)*effic(22,2)*delta_t2x
      else
         time_interval_2x(script_number,2) =
     1    time_interval_2x(script_number,2)+
     1         spin_period*nspins+
     1         spin_period*ave_spins(mf_type)     ! record time elapsed
         exposure_1mf(script_number,22,1) =geofactor(1)*
     1     ehilo(22,1)*effic(22,1)*delta_t2x
         do 25 i=23,25
            do 25 j=1,2
                exposure_1mf(script_number,i,j) =geofactor(j)*
     1         ehilo(i,j)*effic(i,j)*delta_t2x
25    continue
      end if
c
c   4x exposures
      if (phase.eq.0) then
         time_interval_4x(script_number,1) =
     1    time_interval_4x(script_number,1)+
     1         spin_period*nspins+
     1         3.0*spin_period*ave_spins(mf_type)     ! record time elapsed
         do 30 i=26,33
            do 30 j=1,2
         exposure_1mf(script_number,i,j) =geofactor(j)*
     1     ehilo(i,j)*effic(i,j)*delta_t4x
30    continue
        exposure_1mf(script_number,34,2) = geofactor(2)*
     1     ehilo(34,2)*effic(34,2)*delta_t4x
      end if
c
      if (phase.eq.1) then
        time_interval_4x(script_number,2) =
     1   time_interval_4x(script_number,2)+
     1         spin_period*nspins+
     1         3.0*spin_period*ave_spins(mf_type)     ! record time elapsed
        exposure_1mf(script_number,34,1) =geofactor(1)*
     1     ehilo(34,1)*effic(34,1)*delta_t4x
        exposure_1mf(script_number,35,2) =geofactor(2)*
     1     ehilo(35,2)*effic(35,2)*delta_t4x
        exposure_1mf(script_number,35,1) =geofactor(1)*
     1     ehilo(35,1)*effic(35,1)*delta_t4x
      end if
c
      if (phase.eq.2) then
         time_interval_4x(script_number,3) =
     1    time_interval_4x(script_number,3)+
     1         spin_period*nspins+
     1         3.0*spin_period*ave_spins(mf_type)     ! record time elapsed
         exposure_1mf(script_number,36,2) =geofactor(2)*
     1     ehilo(36,2)*effic(36,2)*delta_t4x
        exposure_1mf(script_number,36,1) =geofactor(1)*
     1     ehilo(36,1)*effic(36,1)*delta_t4x
        exposure_1mf(script_number,37,2) =geofactor(2)*
     1     ehilo(37,2)*effic(37,2)*delta_t4x
      end if
c
      if (phase.eq.3) then
        time_interval_4x(script_number,4) =
     1   time_interval_4x(script_number,4)+
     1         spin_period*nspins+
     1         3.0*spin_period*ave_spins(mf_type)     ! record time elapsed
         exposure_1mf(script_number,37,1) =geofactor(1)*
     1     ehilo(37,1)*effic(37,1)*delta_t4x
        exposure_1mf(script_number,38,2) =geofactor(2)*
     1     ehilo(38,2)*effic(38,2)*delta_t4x
        exposure_1mf(script_number,38,1) = geofactor(1)*
     1     ehilo(38,1)*effic(38,1)*delta_t4x
      end if
c
c   add result to total exposure
      do  i=1,42
         do  j=1,2
        exposure(script_number,i,j) =
     1      exposure(script_number,i,j)+
     1      exposure_1mf(script_number,i,j)
        end do
      end do
c
      return
      end  ! end get_exposure
c
c
c
c
c
c************************************************
c
        subroutine sum_kp(script_number)
c
c************************************************
c
c     sum key parameters
c
c     6/17/98 by J. Dwyer
c     1/9/01  allowed position to be gt 220 RE and lt -220 RE
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real    Bmag   ! B field magnitude
      real    Bnorm   ! B field weighting factor for average direction
      real    KPweight   ! weighting factor for averaging KP values
      integer*4   i,j,k
      integer*4   script_number
c
      KPweight = 1.0
      if (KPweight_flag.eq.1) then  ! weight with vse
        KPweight = 0.0
        do 100, k=1,8
          do 100, j=1,2
            if (mdata_flag_1mf(script_number,42,j,k).eq.1) then
              KPweight = KPweight+
     1          mdata_1mf(script_number,42,j,k)
            end if
100      continue
      end if
c
      if ((spha_flag_1mf).and.
     1   (spha_asr.ge.0.0).and.
     1   (spha_asr.le.6.28319).and.
     1    (KPweight.gt.0.0)) then
         spha_flag(script_number) = .true.
         spin_rate_accum(script_number) =
     1      spin_rate_accum(script_number)+
     1      KPweight*spha_asr
         Nspha(script_number) =
     1      Nspha(script_number) + KPweight
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
            if ((Bdir_flag.eq.0)
     1        .or.(Bmag.eq.0.0))
     1         Bnorm = 1.0  ! average B vectors
            Bfield_accum(script_number) =
     1        Bfield_accum(script_number)+KPweight*Bmag
            Bx_accum(script_number) =
     1        Bx_accum(script_number)+
     1        KPweight*mfi_bgse(1)/Bnorm   ! use cart. coords. to ave. B
            By_accum(script_number) =
     1        By_accum(script_number)+
     1        KPweight*mfi_bgse(2)/Bnorm
            Bz_accum(script_number) =
     1        Bz_accum(script_number)+
     1        KPweight*mfi_bgse(3)/Bnorm
            Brms_accum(script_number) =
     1        Brms_accum(script_number)+
     1        KPweight*mfi_rms
            mfi_flag(script_number,1) = .true.
            Nmfi(script_number,1) =
     1        Nmfi(script_number,1)+KPweight
         end if
         if ((mfi_position(1).ge.-1000).and.
     1      (mfi_position(1).le.1000).and.
     1      (mfi_position(2).ge.-1000).and.
     1      (mfi_position(2).le.1000).and.
     1      (mfi_position(3).ge.-1000).and.
     1      (mfi_position(3).le.1000)) then
            SCx_accum(script_number) =
     1        SCx_accum(script_number) +
     1        KPweight*mfi_position(1)
            SCy_accum(script_number) =
     1        SCy_accum(script_number) +
     1        KPweight*mfi_position(2)
            SCz_accum(script_number) =
     1        SCz_accum(script_number) +
     1        KPweight*mfi_position(3)
            mfi_flag(script_number,2) = .true.
            Nmfi(script_number,2) =
     1        Nmfi(script_number,2)+KPweight
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
            swe_flag(script_number)  = .true.
            SWVx_accum(script_number) =
     1        SWVx_accum(script_number) +
     1        KPweight*swe_vgse(1)
            SWVy_accum(script_number) =
     1        SWVy_accum(script_number) +
     1        KPweight*swe_vgse(2)
            SWVz_accum(script_number) =
     1        SWVz_accum(script_number) +
     1        KPweight*swe_vgse(3)
            Nproton_accum(script_number) =
     1        Nproton_accum(script_number)+
     1        KPweight*swe_protdens
            SWVth_accum(script_number) =
     1        SWVth_accum(script_number)+
     1        KPweight*swe_vth
            Nswe(script_number) =
     1        Nswe(script_number)+KPweight
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
              eflux_accum(script_number,i)=
     1          eflux_accum(script_number,i)+
     1           KPweight*tdp_eflux(i)
200         continue
            tdp_flag(script_number,1) = .true.
            N3dp(script_number,1) =
     1        N3dp(script_number,1)+KPweight
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
              ionflux_accum(script_number,i)=
     1          ionflux_accum(script_number,i)+
     1          KPweight*tdp_ionflux(i)
300       continue
            tdp_flag(script_number,2) = .true.
            N3dp(script_number,2) =
     1        N3dp(script_number,2)+KPweight
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
           J_apeb1_accum(script_number) =
     1        J_apeb1_accum(script_number)+
     1        KPweight*epa_apeb1
            J_apeb2_accum(script_number) =
     1        J_apeb2_accum(script_number)+
     1        KPweight*epa_apeb2
            J_apeb3_accum(script_number) =
     1        J_apeb3_accum(script_number)+
     1        KPweight*epa_apeb3
            J_apeb4_accum(script_number) =
     1        J_apeb4_accum(script_number)+
     1        KPweight*epa_apeb4
            J_apeb5_accum(script_number) =
     1        J_apeb5_accum(script_number)+
     1        KPweight*epa_apeb5
            epa_flag(script_number,1) = .true.
            Nepa(script_number,1) =
     1        Nepa(script_number,1)+KPweight
         end if
         if ((epa_lemt1.ge.0.00001).and.
     1   (epa_lemt1.le.100000).and.
     1   (epa_lemt2.ge.0.00001).and.
     1   (epa_lemt2.le.100000).and.
     1   (epa_lemt3.ge.0.00001).and.
     1   (epa_lemt3.le.100000)) then
            J_lemt1_accum(script_number) =
     1   J_lemt1_accum(script_number)+KPweight*epa_lemt1
            J_lemt2_accum(script_number) =
     1   J_lemt2_accum(script_number)+KPweight*epa_lemt2
            J_lemt3_accum(script_number) =
     1   J_lemt3_accum(script_number)+KPweight*epa_lemt3
            epa_flag(script_number,2) = .true.
            Nepa(script_number,2) =
     1        Nepa(script_number,2)+KPweight
        end if
      end if
c
c    accumulate hk and STEP kp data
        Nhk(script_number) = Nhk(script_number)+1
        hk_accum(script_number,1) =
     1     hk_accum(script_number,1)+STEPtherm
        hk_accum(script_number,2) =
     1     hk_accum(script_number,2)+STEPt1
        hk_accum(script_number,3) =
     1     hk_accum(script_number,3)+STEPt2
        hk_accum(script_number,4) =
     1     hk_accum(script_number,4)+STEPt3
        hk_accum(script_number,5) =
     1     hk_accum(script_number,5)+STEPHV
        hk_accum(script_number,6) =
     1     hk_accum(script_number,6)+1.0*MFnum
      do i = 1, 6
        STEP_KP_fluxes_accum(script_number,i) =
     1     STEP_KP_fluxes_accum(script_number,i) +
     1     STEP_KP_fluxes(i)
      end do
c
      return
      end  ! end sum_kp
c
c
c
c
c************************************************
c
        subroutine ave_kp(script_number)
c
c************************************************
c
c     calculates average values of key parameters
c
c     12/1/95 by J. Dwyer
c          modified  2/13/97 by J. Dwyer   changed KP data
c          modified  3/18/97 by J. Dwyer   fixed Brms bug
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real   Bmag,Bx,By,Bz
      integer*4   i
      integer*4   script_number
c
      do i = 1,37
        kp_ave_flag(i) = 0
      end do
c
      if (spha_flag(script_number)) then
            kp_ave_flag(15) = 1
            kp_ave(15) =
     1        spin_rate_accum(script_number)/
     1        Nspha(script_number)
      end if
c
      if (mfi_flag(script_number,1)) then
         kp_ave(2) = -9.99E32
         kp_ave(3) = -9.99E32
         Bmag =
     1  sqrt(Bx_accum(script_number)*Bx_accum(script_number)+
     1  By_accum(script_number)*By_accum(script_number)+
     1  Bz_accum(script_number)*Bz_accum(script_number))
         kp_ave_flag(1) = 1
         kp_ave(1) =
     1   Bfield_accum(script_number)/Nmfi(script_number,1)
         kp_ave_flag(4) = 1
         kp_ave(4) =
     1   Brms_accum(script_number)/Nmfi(script_number,1)
         if (Bmag.gt.0.0) then
            Bx = Bx_accum(script_number)/Bmag
            By = By_accum(script_number)/Bmag
            Bz = Bz_accum(script_number)/Bmag
            kp_ave_flag(2) = 1
            kp_ave(2) = asind(Bz)
            if ((Bx.ne.0.0).or.(By.ne.0.0)) then
              kp_ave_flag(3) = 1
              kp_ave(3) = atan2d(By,Bx)
              if (kp_ave(3).lt.0.0)
     1           kp_ave(3) = 360.0+kp_ave(3)
            end if
         end if
      end if
      if (mfi_flag(script_number,2)) then
        kp_ave_flag(5) = 1
        kp_ave_flag(6) = 1
        kp_ave_flag(7) = 1
        kp_ave_flag(8) = 1
        kp_ave(6) = SCx_accum(script_number)/Nmfi(script_number,2)
        kp_ave(7) = SCy_accum(script_number)/Nmfi(script_number,2)
        kp_ave(8) = SCz_accum(script_number)/Nmfi(script_number,2)
        kp_ave(5)=sqrt(kp_ave(6)*kp_ave(6)+
     1  kp_ave(7)*kp_ave(7)+kp_ave(8)*kp_ave(8))
      end if
c
      if (swe_flag(script_number)) then
        kp_ave_flag(9) = 1
        kp_ave_flag(10) = 1
        kp_ave_flag(11) = 1
        kp_ave_flag(12) = 1
        kp_ave_flag(13) = 1
        kp_ave_flag(14) = 1
        kp_ave(10)=SWVx_accum(script_number)/Nswe(script_number)
        kp_ave(11)=SWVy_accum(script_number)/Nswe(script_number)
        kp_ave(12)=SWVz_accum(script_number)/Nswe(script_number)
        kp_ave(13)=Nproton_accum(script_number)/Nswe(script_number)
        kp_ave(14)=SWVth_accum(script_number)/Nswe(script_number)
        kp_ave(9)=sqrt(kp_ave(10)*kp_ave(10)+
     1  kp_ave(11)*kp_ave(11)+kp_ave(12)*kp_ave(12))
      end if
c
      if (tdp_flag(script_number,1)) then
        do 100 i=1,7
          kp_ave_flag(15+i) = 1
          kp_ave(15+i)=
     1  eflux_accum(script_number,i)/N3dp(script_number,1)
100      continue
      end if
      if (tdp_flag(script_number,2)) then
        do 200 i=1,7
          kp_ave_flag(22+i) = 1
          kp_ave(22+i)=
     1     ionflux_accum(script_number,i)/
     1      N3dp(script_number,2)
200      continue
      end if
c
      if (epa_flag(script_number,1)) then
         kp_ave_flag(30) = 1
         kp_ave_flag(31) = 1
         kp_ave_flag(32) = 1
         kp_ave_flag(33) = 1
         kp_ave_flag(34) = 1
         kp_ave(30) =
     1     J_apeb1_accum(script_number)/Nepa(script_number,1)
         kp_ave(31) =
     1     J_apeb2_accum(script_number)/Nepa(script_number,1)
         kp_ave(32) =
     1     J_apeb3_accum(script_number)/Nepa(script_number,1)
         kp_ave(33) =
     1     J_apeb4_accum(script_number)/Nepa(script_number,1)
         kp_ave(34) =
     1     J_apeb5_accum(script_number)/Nepa(script_number,1)
      end if
      if (epa_flag(script_number,2)) then
        kp_ave_flag(35) = 1
        kp_ave_flag(36) = 1
        kp_ave_flag(37) = 1
         kp_ave(35) =
     1     J_lemt1_accum(script_number)/Nepa(script_number,2)
         kp_ave(36) =
     1     J_lemt2_accum(script_number)/Nepa(script_number,2)
         kp_ave(37) =
     1     J_lemt3_accum(script_number)/Nepa(script_number,2)
      end if
c
c    accumulate hk and STEP kp data
      do i = 1, 6
        hk_ave_flag(i) = 0
        STEPkp_ave_flag(i) = 0
        if (Nhk(script_number).gt.0) then
          hk_ave(i) = hk_accum(script_number,i)/
     1      (1.0*Nhk(script_number))
          STEPKP_ave(i) =
     1       STEP_KP_fluxes_accum(script_number,i)/
     1       (1.0*Nhk(script_number))
          hk_ave_flag(i) = 1
          STEPkp_ave_flag(i) = 1
        end if
      end do
c
c
      return
      end  ! end ave_kp
c
c
c
c
c************************************************
c
        subroutine ave_kp_1mf(script_number)
c
c************************************************
c
c     calculates key parameters for 1 mf
c
c     6/17/98 by J. Dwyer
c     1/9/01  allowed position to be gt 220 RE and lt -220 RE
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real    Bmag   ! B field magnitude
      real    Bnorm   ! B field weighting factor for average direction
      real    KPweight   ! weighting factor for averaging KP values
      integer*4   i,j,k
      integer*4   script_number
c
      KPweight = 1.0
      do i = 1,37
        kp_ave_flag(i) = 0
      end do
c
      if ((spha_flag_1mf).and.
     1   (spha_asr.ge.0.0).and.
     1   (spha_asr.le.6.28319).and.
     1    (KPweight.gt.0.0)) then
         kp_ave_flag(15) = 1
         kp_ave(15) = spha_asr
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
            if ((Bdir_flag.eq.0)
     1        .or.(Bmag.eq.0.0))
     1         Bnorm = 1.0  ! average B vectors
            kp_ave_flag(1) = 1
            kp_ave(1) = Bmag
            Bx = mfi_bgse(1)/Bnorm   ! use cart. coords. to ave. B
            By = mfi_bgse(2)/Bnorm
            Bz = mfi_bgse(3)/Bnorm
            kp_ave_flag(2) = 1
            kp_ave(2) = asind(Bz)
            if ((Bx.ne.0.0).or.(By.ne.0.0)) then
              kp_ave_flag(3) = 1
              kp_ave(3) = atan2d(By,Bx)
              if (kp_ave(3).lt.0.0)
     1           kp_ave(3) = 360.0+kp_ave(3)
            end if
            kp_ave_flag(4) = 1
            kp_ave(4) = mfi_rms
         end if
         if ((mfi_position(1).ge.-1000).and.
     1      (mfi_position(1).le.1000).and.
     1      (mfi_position(2).ge.-1000).and.
     1      (mfi_position(2).le.1000).and.
     1      (mfi_position(3).ge.-1000).and.
     1      (mfi_position(3).le.1000)) then
            kp_ave_flag(5) = 1
            kp_ave_flag(6) = 1
            kp_ave_flag(7) = 1
            kp_ave_flag(8) = 1
            kp_ave(6) = mfi_position(1)
            kp_ave(7) = mfi_position(2)
            kp_ave(8) = mfi_position(3)
            kp_ave(5)=sqrt(kp_ave(6)*kp_ave(6)+
     1      kp_ave(7)*kp_ave(7)+kp_ave(8)*kp_ave(8))
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
            kp_ave_flag(9) = 1
            kp_ave_flag(10) = 1
            kp_ave_flag(11) = 1
            kp_ave_flag(12) = 1
            kp_ave_flag(13) = 1
            kp_ave_flag(14) = 1
            kp_ave(10) = swe_vgse(1)
            kp_ave(11) = swe_vgse(2)
            kp_ave(12) = swe_vgse(3)
            kp_ave(13) = swe_protdens
            kp_ave(14) = swe_vth
            kp_ave(9)=sqrt(kp_ave(10)*kp_ave(10)+
     1  kp_ave(11)*kp_ave(11)+kp_ave(12)*kp_ave(12))
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
              kp_ave_flag(15+i) = 1
              kp_ave(15+i) = tdp_eflux(i)
200         continue
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
             kp_ave_flag(22+i) = 1
              kp_ave(22+i) = tdp_ionflux(i)
300       continue
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
            kp_ave_flag(30) = 1
            kp_ave_flag(31) = 1
            kp_ave_flag(32) = 1
            kp_ave_flag(33) = 1
            kp_ave_flag(34) = 1
c           kp_ave(30) = epa_apeb1
            kp_ave(31) = epa_apeb2
            kp_ave(32) = epa_apeb3
            kp_ave(33) = epa_apeb4
            kp_ave(34) = epa_apeb5
         end if
         if ((epa_lemt1.ge.0.00001).and.
     1   (epa_lemt1.le.100000).and.
     1   (epa_lemt2.ge.0.00001).and.
     1   (epa_lemt2.le.100000).and.
     1   (epa_lemt3.ge.0.00001).and.
     1   (epa_lemt3.le.100000)) then
            kp_ave_flag(35) = 1
            kp_ave_flag(36) = 1
            kp_ave_flag(37) = 1
            kp_ave(35)  = epa_lemt1
            kp_ave(36)  = epa_lemt2
            kp_ave(37)  = epa_lemt3
        end if
      end if
c
c    accumulate hk and STEP kp data
        hk_ave(1) = STEPtherm
        hk_ave(2) = STEPt1
        hk_ave(3) = STEPt2
        hk_ave(4) = STEPt3
        hk_ave(5) = STEPHV
        hk_ave(6) = 1.0*MFnum
      do i = 1, 6
        STEPkp_ave(i) = STEP_KP_fluxes(i)
        hk_ave_flag(i) = 1
        STEPkp_ave_flag(i) = 1
      end do
c
c
c
      return
      end  ! end ave_kp_1mf
c
c
c
c
c
c************************************************
c
        subroutine calculate_rates(script_number)
c
c************************************************
c
c     calculates average values of rates data after finished accumulating
c
c     6/17/98 by J. Dwyer
c       10/27/98 corrected factor of 2 error in Tel1&2 VSE rate
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real  rate1, rate2
      integer*4  script_number
      integer*4  Ntel, Nsect
      integer*4   i,j,k
c
c
cccccccccccccccccc matrix rates cccccccccccccccccccccc
      do i=1,42
       do k=1,8
          Ntel = 0
          mdata_sect_flag(i,3,k) = -1
          mdata_sect(i,3,k) = 0.0
          mdata_sect_err(i,3,k) = 0.0
          do j = 1,2
            if ((mdata_flag(script_number,i,j,k).eq.1).and.
     1         (exposure(script_number,i,j).gt.0.0)) then  ! include only values measured in this mf
              mdata_sect_flag(i,j,k) = 1
              mdata_sect(i,j,k) =
     1  mdata(script_number,i,j,k)/(exposure(script_number,i,j)/8.0)
              mdata_sect_err(i,j,k) =
     1  sqrt(mdata(script_number,i,j,k))/
     1  (exposure(script_number,i,j)/8.0)  ! variance
              Ntel  = Ntel+1
              mdata_sect_flag(i,3,k) = 1
              mdata_sect(i,3,k) =
     1    mdata_sect(i,3,k)+mdata_sect(i,j,k)
              mdata_sect_err(i,3,k) =
     1    mdata_sect_err(i,3,k)+
     1    mdata_sect_err(i,j,k)*mdata_sect_err(i,j,k)
             else
               mdata_sect_flag(i,j,k) = -1
             end if
           end do
           if (Ntel.gt.0) then
             if (i.lt.42) then
               mdata_sect(i,3,k) = mdata_sect(i,3,k)/(1.0*Ntel)
               mdata_sect_err(i,3,k) =
     1           sqrt(mdata_sect_err(i,3,k))/(1.0*Ntel)
             end if
             if (i.eq.42) then
               mdata_sect_err(i,3,k) =
     1           sqrt(mdata_sect_err(i,3,k))
             end if
           end if
         end do
       end do
c
       do i=1,42
         do j=1,3
           Nsect = 0
           mdata_ave_flag(i,j) = -1
           mdata_ave(i,j) = 0.0
           mdata_ave_err(i,j) = 0.0
           do k=1,8
             if (mdata_sect_flag(i,j,k).eq.1) then
               Nsect = Nsect+1
               mdata_ave(i,j) = mdata_ave(i,j)+mdata_sect(i,j,k)
               mdata_ave_err(i,j) = mdata_ave_err(i,j)+
     1           mdata_sect_err(i,j,k)*mdata_sect_err(i,j,k)
             end if
           end do
           if (Nsect.eq.8) then
             mdata_ave_flag(i,j) = 1
             mdata_ave(i,j) = mdata_ave(i,j)/(1.0*Nsect)
             mdata_ave_err(i,j) =
     1          sqrt(mdata_ave_err(i,j))/(1.0*Nsect)
           end if
         end do
       end do
c
      if ((vsebar_flag(script_number).eq.1).and.
     1   (time_interval(script_number).gt.0.0)) then
         disc_ave_flag(1) = 1
         disc_ave(1) = vsebar(script_number)/
     1              time_interval(script_number)
         disc_ave_err(1) = sqrt(1.0*vsebar(script_number))/
     1              time_interval(script_number)
      else
         disc_ave_flag(1) = -1
      end if
c
      if ((start_flag(script_number).eq.1).and.
     1   (time_interval(script_number).gt.0.0)) then
         disc_ave_flag(2) = 1
         disc_ave(2) = start(script_number)/
     1         time_interval(script_number)
         disc_ave_err(2) =
     1        sqrt(1.0*start(script_number))/
     1        time_interval(script_number)
      else
         disc_ave_flag(2) = -1
      end if
c
      if ((stop_flag(script_number).eq.1).and.
     1   (time_interval(script_number).gt.0.0)) then
         disc_ave_flag(3) = 1
         disc_ave(3) = stop(script_number)/
     1           time_interval(script_number)
         disc_ave_err(3) =
     1          sqrt(1.0*stop(script_number))/
     1          time_interval(script_number)
      else
         disc_ave_flag(3) = -1
      end if
c

      if ((D1_flag(script_number).eq.1).and.
     1  (time_interval(script_number).gt.0.0)) then
         D_ave_flag(1) = 1
         D_ave(1) = d1(script_number)/
     1        time_interval(script_number)
         D_ave_err(1) = sqrt(1.0*d1(script_number))/
     1        time_interval(script_number)
      else
          D_ave_flag(1) = -1
      end if
      if ((D2_flag(script_number).eq.1).and.
     1  (time_interval(script_number).gt.0.0)) then
         D_ave_flag(2) = 1
         D_ave(2) = d2(script_number)/
     1       time_interval(script_number)
         D_ave_err(2) = sqrt(1.0*d2(script_number))/
     1       time_interval(script_number)
      else
          D_ave_flag(2) = -1
      end if
      if ((D1_flag(script_number).eq.1).and.
     1       (D2_flag(script_number).eq.1).and.
     1  (time_interval(script_number).gt.0.0)) then
         D_ave_flag(3) = 1
         D_ave(3) = (d1(script_number)+
     1       d2(script_number))/time_interval(script_number)
         D_ave_err(3) =
     1     sqrt(1.0*(d1(script_number)+d2(script_number)))/
     1   time_interval(script_number)
      else
          D_ave_flag(3) = -1
      end if
c
c   *********  rate ratios ***********
      if (Nratios(script_number).gt.0) then
        do j = 1, Nratios(script_number)
          ratio_good(j) = -1
          rate1 = -1.0
          rate2 = -1.0
          if (ratio_which(script_number,j,1).eq.1) then
            rate1 =
     1      mdata_ave(ratio_index(script_number,j,1),3)
            rate1_err =
     1      mdata_ave_err(ratio_index(script_number,j,1),3)
          end if
          if (ratio_which(script_number,j,2).eq.1) then
            rate2 =
     1      mdata_ave(ratio_index(script_number,j,2),3)
            rate2_err =
     1      mdata_ave_err(ratio_index(script_number,j,2),3)
          end if
          if (ratio_which(script_number,j,1).eq.2) then
            rate1 =
     1      disc_ave(ratio_index(script_number,j,1))
            rate1_err =
     1      disc_ave_err(ratio_index(script_number,j,1))
          end if
         if (ratio_which(script_number,j,2).eq.2) then
            rate2 =
     1      disc_ave(ratio_index(script_number,j,2))
            rate2_err =
     1      disc_ave_err(ratio_index(script_number,j,2))
          end if
          if (ratio_which(script_number,j,1).eq.3) then
            rate1 = D_ave(3)
            rate1_err = D_ave_err(3)
          end if
          if (ratio_which(script_number,j,2).eq.3) then
            rate2 = D_ave(3)
            rate2_err = D_ave_err(3)
          end if
          if (((rate1.gt.0.0).and.(rate2.ge.0.0)).or.
     1          ((rate1.ge.0.0).and.(rate2.gt.0.0))) then
              ratio_good(j) = 1
             if ((rate1.gt.0.0).and.(rate2.gt.0.0)) then
                 rates_ratio(j) = rate1/rate2
                 rates_ratio_err(j) = rates_ratio(j)*
     1           sqrt((rate1_err*rate1_err/(rate1*rate1))+
     1           (rate1_err*rate1_err/(rate1*rate1)))
             end if
             if ((rate1.gt.0.0).and.(rate2.eq.0.0)) then
                 rates_ratio(j) = -1.0E32
                 rates_ratio_err(j) = -1.0E32
             end if
             if ((rate1.eq.0.0).and.(rate2.gt.0.0)) then
                 rates_ratio(j) = 0.0
                 rates_ratio_err(j) = 0.0
             end if
           end if
        end do
      end if
c
c
      return
      end  ! end calculate_rates
c
c
c
c
c
c************************************************
c
        subroutine calculate_counts(script_number)
c
c************************************************
c
c     calculates total values of counts after accumulating
c
c     6/17/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real  rate1, rate2
      integer*4  script_number
      integer*4   i,j,k
c
cccccccccccccccccc matrix rates cccccccccccccccccccccc
      do i=1,42
       do k=1,8
          mdata_sect_flag(i,3,k) = -1
          mdata_sect(i,3,k) = 0.0
          mdata_sect_err(i,3,k) = 0.0
          do j = 1,2
       if (mdata_flag(script_number,i,j,k).eq.1) then  ! include only values measured in this mf
              mdata_sect_flag(i,j,k) = 1
              mdata_sect(i,j,k) =
     1  mdata(script_number,i,j,k)
              mdata_sect_err(i,j,k) =
     1  sqrt(mdata(script_number,i,j,k))  ! variance
              mdata_sect_flag(i,3,k) = 1
              mdata_sect(i,3,k) =
     1    mdata_sect(i,3,k)+mdata_sect(i,j,k)
              mdata_sect_err(i,3,k) =
     1    mdata_sect_err(i,3,k)+
     1    mdata_sect_err(i,j,k)*mdata_sect_err(i,j,k)
             else
               mdata_sect_flag(i,j,k) = -1
             end if
          end do
          mdata_sect_err(i,3,k) =
     1         sqrt(mdata_sect_err(i,3,k))
        end do
      end do
c
       do i=1,42
         do j=1,3
           mdata_ave_flag(i,j) = -1
           mdata_ave(i,j) = 0.0
           mdata_ave_err(i,j) = 0.0
           do k=1,8
             if (mdata_sect_flag(i,j,k).eq.1) then
               mdata_ave(i,j) = mdata_ave(i,j)+mdata_sect(i,j,k)
               mdata_ave_err(i,j) = mdata_ave_err(i,j)+
     1           mdata_sect_err(i,j,k)*mdata_sect_err(i,j,k)
             end if
           end do
           mdata_ave_flag(i,j) = 1
           mdata_ave_err(i,j) =
     1          sqrt(mdata_ave_err(i,j))
         end do
       end do
c
      if (vsebar_flag(script_number).eq.1) then
         disc_ave_flag(1) = 1
         disc_ave(1) = vsebar(script_number)
         disc_ave_err(1) =
     1      sqrt(1.0*vsebar(script_number))
      else
         disc_ave_flag(1) = -1
      end if
c
      if (start_flag(script_number).eq.1) then
         disc_ave_flag(2) = 1
         disc_ave(2) = start(script_number)
         disc_ave_err(2) =
     1   sqrt(1.0*start(script_number))
      else
         disc_ave_flag(2) = -1
      end if
c
      if (stop_flag(script_number).eq.1) then
         disc_ave_flag(3) = 1
         disc_ave(3) = stop(script_number)
         disc_ave_err(3) = sqrt(1.0*stop(script_number))
      else
         disc_ave_flag(3) = -1
      end if
c

      if (D1_flag(script_number).eq.1) then
         D_ave_flag(1) = 1
         D_ave(1) = d1(script_number)
         D_ave_err(1) = sqrt(1.0*d1(script_number))
      else
          D_ave_flag(1) = -1
      end if
      if (D2_flag(script_number).eq.1) then
         D_ave_flag(2) = 1
         D_ave(2) = d2(script_number)
         D_ave_err(2) = sqrt(1.0*d2(script_number))
      else
          D_ave_flag(2) = -1
      end if
      if ((D1_flag(script_number).eq.1).and.
     1     (D2_flag(script_number).eq.1)) then
         D_ave_flag(3) = 1
         D_ave(3) = d1(script_number)+d2(script_number)
         D_ave_err(3) = sqrt(1.0*D_ave(3))
      else
          D_ave_flag(3) = -1
      end if
c
c   *********  rate ratios ***********
      if (Nratios(script_number).gt.0) then
        do j = 1, Nratios(script_number)
          ratio_good(j) = -1
          rate1 = -1.0
          rate2 = -1.0
          if (ratio_which(script_number,j,1).eq.1) then
            rate1 =
     1      mdata_ave(ratio_index(script_number,j,1),3)
            rate1_err =
     1      mdata_ave_err(ratio_index(script_number,j,1),3)
          end if
          if (ratio_which(script_number,j,2).eq.1) then
            rate2 =
     1      mdata_ave(ratio_index(script_number,j,2),3)
            rate2_err =
     1      mdata_ave_err(ratio_index(script_number,j,2),3)
          end if
          if (ratio_which(script_number,j,1).eq.2) then
            rate1 =
     1      disc_ave(ratio_index(script_number,j,1))
            rate1_err =
     1      disc_ave_err(ratio_index(script_number,j,1))
          end if
         if (ratio_which(script_number,j,2).eq.2) then
            rate2 =
     1      disc_ave(ratio_index(script_number,j,2))
            rate2_err =
     1      disc_ave_err(ratio_index(script_number,j,2))
          end if
          if (ratio_which(script_number,j,1).eq.3) then
            rate1 = D_ave(3)
            rate1_err = D_ave_err(3)
          end if
          if (ratio_which(script_number,j,2).eq.3) then
            rate2 = D_ave(3)
            rate2_err = D_ave_err(3)
          end if
          if (((rate1.gt.0.0).and.(rate2.ge.0.0)).or.
     1          ((rate1.ge.0.0).and.(rate2.gt.0.0))) then
              ratio_good(j) = 1
             if ((rate1.gt.0.0).and.(rate2.gt.0.0)) then
                 rates_ratio(j) = rate1/rate2
                 rates_ratio_err(j) = rates_ratio(j)*
     1           sqrt((rate1_err*rate1_err/(rate1*rate1))+
     1           (rate1_err*rate1_err/(rate1*rate1)))
             end if
             if ((rate1.gt.0.0).and.(rate2.eq.0.0)) then
                 rates_ratio(j) = 1.0E32
                 rates_ratio_err(j) = 1.0E32
             end if
             if ((rate1.eq.0.0).and.(rate2.gt.0.0)) then
                 rates_ratio(j) = 0.0
                 rates_ratio_err(j) = 0.0
             end if
           end if
        end do
      end if
c
c
c
      return
      end  ! end calculate_counts
c
c
c************************************************
c
        subroutine calculate_rates_1mf(script_number)
c
c************************************************
c
c     calculates rates data for 1 mf
c
c     6/17/98 by J. Dwyer
c       11/6/98 corrected factor of 2 error in Tel1&2 VSE rate
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real  rate1, rate2
      real  delta_tmf
      integer*4  script_number
      integer*4  Ntel, Nsect
      integer*4   i,j,k
c
      delta_tmf = spin_period*nspins
c
cccccccccccccccccc matrix rates cccccccccccccccccccccc
      do i=1,42
       do k=1,8
          Ntel = 0
          mdata_sect_flag(i,3,k) = -1
          mdata_sect(i,3,k) = 0.0
          mdata_sect_err(i,3,k) = 0.0
          do j = 1,2
           if ((mdata_flag_1mf(script_number,i,j,k).eq.1).and.
     1        (exposure_1mf(script_number,i,j).gt.0.0)) then  ! include only values measured in this mf
              mdata_sect_flag(i,j,k) = 1
              mdata_sect(i,j,k) =
     1  mdata_1mf(script_number,i,j,k)/
     1    (exposure_1mf(script_number,i,j)/8.0)
              mdata_sect_err(i,j,k) =
     1  sqrt(mdata_1mf(script_number,i,j,k))/
     1  (exposure_1mf(script_number,i,j)/8.0)  ! variance
              Ntel  = Ntel+1
              mdata_sect_flag(i,3,k) = 1
              mdata_sect(i,3,k) =
     1    mdata_sect(i,3,k)+mdata_sect(i,j,k)
              mdata_sect_err(i,3,k) =
     1    mdata_sect_err(i,3,k)+
     1    mdata_sect_err(i,j,k)*mdata_sect_err(i,j,k)
             else
               mdata_sect_flag(i,j,k) = -1
             end if
           end do
           if (Ntel.gt.0) then
             if (i.lt.42) then
               mdata_sect(i,3,k) = mdata_sect(i,3,k)/(1.0*Ntel)
               mdata_sect_err(i,3,k) =
     1         sqrt(mdata_sect_err(i,3,k))/(1.0*Ntel)
             end if
             if (i.eq.42) then
                mdata_sect_err(i,3,k) =
     1           sqrt(mdata_sect_err(i,3,k))
             end if
           end if
         end do
       end do
c
       do i=1,42
         do j=1,3
           Nsect = 0
           mdata_ave_flag(i,j) = -1
           mdata_ave(i,j) = 0.0
           mdata_ave_err(i,j) = 0.0
           do k=1,8
             if (mdata_sect_flag(i,j,k).eq.1) then
               Nsect = Nsect+1
               mdata_ave(i,j) = mdata_ave(i,j)+mdata_sect(i,j,k)
               mdata_ave_err(i,j) = mdata_ave_err(i,j)+
     1           mdata_sect_err(i,j,k)*mdata_sect_err(i,j,k)
             end if
           end do
           if (Nsect.eq.8) then
             mdata_ave_flag(i,j) = 1
             mdata_ave(i,j) = mdata_ave(i,j)/(1.0*Nsect)
             mdata_ave_err(i,j) =
     1          sqrt(mdata_ave_err(i,j))/(1.0*Nsect)
           end if
         end do
       end do
c
      if ((vsebar_1mf.ge.0).and.
     1   (delta_tmf.gt.0.0)) then
         disc_ave_flag(1) = 1
         disc_ave(1) =
     1       vsebar_1mf/delta_tmf
         disc_ave_err(1) =
     1       sqrt(1.0*vsebar_1mf)/
     1      delta_tmf
      else
         disc_ave_flag(1) = -1
      end if
c
      if ((start_1mf.ge.0).and.
     1   (delta_tmf.gt.0.0)) then
         disc_ave_flag(2) = 1
         disc_ave(2) = start_1mf/delta_tmf
         disc_ave_err(2) =
     1   sqrt(1.0*start_1mf)/delta_tmf
      else
         disc_ave_flag(2) = -1
      end if
c
      if ((stop_1mf.ge.0).and.
     1   (delta_tmf.gt.0.0)) then
         disc_ave_flag(3) = 1
         disc_ave(3) = stop_1mf/delta_tmf
         disc_ave_err(3) = sqrt(1.0*stop_1mf)/
     1   delta_tmf
      else
         disc_ave_flag(3) = -1
      end if
c

      if ((D1_1mf.ge.0).and.
     1  (delta_tmf.gt.0.0)) then
         D_ave_flag(1) = 1
         D_ave(1) = D1_1mf/delta_tmf
         D_ave_err(1) = sqrt(1.0*D1_1mf)/
     1   delta_tmf
      else
          D_ave_flag(1) = -1
      end if
      if ((D2_1mf.ge.0).and.
     1  (delta_tmf.gt.0.0)) then
         D_ave_flag(2) = 1
         D_ave(2) = d2_1mf/delta_tmf
         D_ave_err(2) = sqrt(1.0*d2_1mf)/
     1   delta_tmf
      else
          D_ave_flag(2) = -1
      end if
      if ((D1_1mf.ge.0).and.
     1    (D2_1mf.ge.0).and.
     1    (delta_tmf.gt.0.0)) then
         D_ave_flag(3) = 1
         D_ave(3) =
     1  (d1_1mf+d2_1mf)/delta_tmf
         D_ave_err(3) =
     1  sqrt(1.0*(d1_1mf+d2_1mf))/
     1   delta_tmf
      else
          D_ave_flag(3) = -1
      end if
c
c   *********  rate ratios ***********
      if (Nratios(script_number).gt.0) then
        do j = 1, Nratios(script_number)
          ratio_good(j) = -1
          rate1 = -1.0
          rate2 = -1.0
          if (ratio_which(script_number,j,1).eq.1) then
            rate1 =
     1      mdata_ave(ratio_index(script_number,j,1),3)
            rate1_err =
     1      mdata_ave_err(ratio_index(script_number,j,1),3)
          end if
          if (ratio_which(script_number,j,2).eq.1) then
            rate2 =
     1      mdata_ave(ratio_index(script_number,j,2),3)
            rate2_err =
     1      mdata_ave_err(ratio_index(script_number,j,2),3)
          end if
          if (ratio_which(script_number,j,1).eq.2) then
            rate1 =
     1      disc_ave(ratio_index(script_number,j,1))
            rate1_err =
     1      disc_ave_err(ratio_index(script_number,j,1))
          end if
         if (ratio_which(script_number,j,2).eq.2) then
            rate2 =
     1      disc_ave(ratio_index(script_number,j,2))
            rate2_err =
     1      disc_ave_err(ratio_index(script_number,j,2))
          end if
          if (ratio_which(script_number,j,1).eq.3) then
            rate1 = D_ave(3)
            rate1_err = D_ave_err(3)
          end if
          if (ratio_which(script_number,j,2).eq.3) then
            rate2 = D_ave(3)
            rate2_err = D_ave_err(3)
          end if
          if (((rate1.gt.0.0).and.(rate2.ge.0.0)).or.
     1          ((rate1.ge.0.0).and.(rate2.gt.0.0))) then
              ratio_good(j) = 1
             if ((rate1.gt.0.0).and.(rate2.gt.0.0)) then
                 rates_ratio(j) = rate1/rate2
                 rates_ratio_err(j) = rates_ratio(j)*
     1           sqrt((rate1_err*rate1_err/(rate1*rate1))+
     1           (rate1_err*rate1_err/(rate1*rate1)))
             end if
             if ((rate1.gt.0.0).and.(rate2.eq.0.0)) then
                 rates_ratio(j) = 1.0E32
                 rates_ratio_err(j) = 1.0E32
             end if
             if ((rate1.eq.0.0).and.(rate2.gt.0.0)) then
                 rates_ratio(j) = 0.0
                 rates_ratio_err(j) = 0.0
             end if
           end if
        end do
      end if
c
c
      return
      end  ! end calculate_rates_1mf
c
c
c
c
c
c************************************************
c
        subroutine calculate_counts_1mf(script_number)
c
c************************************************
c
c     calculates average values of rates data after finished accumulating
c     divides by, delta t, efficiency, delta E, and geometry factor
c
c     6/17/98 by J. Dwyer
c
      include 'Sdf_include.inc' 
      include 'Hex_include.inc'
c
      real  rate1, rate2
      integer*4  script_number
      integer*4   i,j,k
c
cccccccccccccccccc matrix rates cccccccccccccccccccccc
      do i=1,42
       do k=1,8
          mdata_sect_flag(i,3,k) = -1
          mdata_sect(i,3,k) = 0.0
          mdata_sect_err(i,3,k) = 0.0
          do j = 1,2
       if (mdata_flag_1mf(script_number,i,j,k).eq.1) then  ! include only values measured in this mf
              mdata_sect_flag(i,j,k) = 1
              mdata_sect(i,j,k) =
     1  mdata_1mf(script_number,i,j,k)
              mdata_sect_err(i,j,k) =
     1  sqrt(mdata_1mf(script_number,i,j,k))  ! variance
              mdata_sect_flag(i,3,k) = 1
              mdata_sect(i,3,k) =
     1    mdata_sect(i,3,k)+mdata_sect(i,j,k)
              mdata_sect_err(i,3,k) =
     1    mdata_sect_err(i,3,k)+
     1    mdata_sect_err(i,j,k)*mdata_sect_err(i,j,k)
             else
               mdata_sect_flag(i,j,k) = -1
             end if
           end do
           mdata_sect(i,3,k) = mdata_sect(i,3,k)
           mdata_sect_err(i,3,k) =
     1         sqrt(mdata_sect_err(i,3,k))
         end do
       end do
c
       do i=1,42
         do j=1,3
           mdata_ave_flag(i,j) = -1
           mdata_ave(i,j) = 0.0
           mdata_ave_err(i,j) = 0.0
           do k=1,8
             if (mdata_sect_flag(i,j,k).eq.1) then
               mdata_ave(i,j) = mdata_ave(i,j)+mdata_sect(i,j,k)
               mdata_ave_err(i,j) = mdata_ave_err(i,j)+
     1           mdata_sect_err(i,j,k)*mdata_sect_err(i,j,k)
             end if
           end do
           mdata_ave_flag(i,j) = 1
           mdata_ave(i,j) = mdata_ave(i,j)
           mdata_ave_err(i,j) =
     1          sqrt(mdata_ave_err(i,j))
         end do
       end do
c
      if (vsebar_1mf.ge.0) then
         disc_ave_flag(1) = 1
         disc_ave(1) = vsebar_1mf
         disc_ave_err(1) =
     1     sqrt(1.0*vsebar_1mf)
      else
         disc_ave_flag(1) = -1
      end if
c
      if (start_1mf.ge.0) then
         disc_ave_flag(2) = 1
         disc_ave(2) = start_1mf
         disc_ave_err(2) =
     1    sqrt(1.0*start_1mf)
      else
         disc_ave_flag(2) = -1
      end if
c
      if (stop_1mf.ge.0) then
         disc_ave_flag(3) = 1
         disc_ave(3) = stop_1mf
         disc_ave_err(3) =
     1    sqrt(1.0*stop_1mf)
      else
         disc_ave_flag(3) = -1
      end if
c

      if (D1_1mf.ge.0) then
         D_ave_flag(1) = 1
         D_ave(1) = d1_1mf
         D_ave_err(1) = sqrt(1.0*d1_1mf)
      else
          D_ave_flag(1) = -1
      end if
      if (D2_1mf.ge.0) then
         D_ave_flag(2) = 1
         D_ave(2) = d2_1mf
         D_ave_err(2) = sqrt(1.0*d2_1mf)
      else
          D_ave_flag(2) = -1
      end if
      if ((D1_1mf.ge.0).and.
     1   (D2_1mf.ge.0)) then
         D_ave_flag(3) = 1
         D_ave(3) = d1_1mf+d2_1mf
         D_ave_err(3) = sqrt(1.0*D_ave(3))
      else
          D_ave_flag(3) = -1
      end if
c
c   *********  rate ratios ***********
      if (Nratios(script_number).gt.0) then
        do j = 1, Nratios(script_number)
          ratio_good(j) = -1
          rate1 = -1.0
          rate2 = -1.0
          if (ratio_which(script_number,j,1).eq.1) then
            rate1 =
     1      mdata_ave(ratio_index(script_number,j,1),3)
            rate1_err =
     1      mdata_ave_err(ratio_index(script_number,j,1),3)
          end if
          if (ratio_which(script_number,j,2).eq.1) then
            rate2 =
     1      mdata_ave(ratio_index(script_number,j,2),3)
            rate2_err =
     1      mdata_ave_err(ratio_index(script_number,j,2),3)
          end if
          if (ratio_which(script_number,j,1).eq.2) then
            rate1 =
     1      disc_ave(ratio_index(script_number,j,1))
            rate1_err =
     1      disc_ave_err(ratio_index(script_number,j,1))
          end if
         if (ratio_which(script_number,j,2).eq.2) then
            rate2 =
     1      disc_ave(ratio_index(script_number,j,2))
            rate2_err =
     1      disc_ave_err(ratio_index(script_number,j,2))
          end if
          if (ratio_which(script_number,j,1).eq.3) then
            rate1 = D_ave(3)
            rate1_err = D_ave_err(3)
          end if
          if (ratio_which(script_number,j,2).eq.3) then
            rate2 = D_ave(3)
            rate2_err = D_ave_err(3)
          end if
          if (((rate1.gt.0.0).and.(rate2.ge.0.0)).or.
     1          ((rate1.ge.0.0).and.(rate2.gt.0.0))) then
              ratio_good(j) = 1
             if ((rate1.gt.0.0).and.(rate2.gt.0.0)) then
                 rates_ratio(j) = rate1/rate2
                 rates_ratio_err(j) = rates_ratio(j)*
     1           sqrt((rate1_err*rate1_err/(rate1*rate1))+
     1           (rate1_err*rate1_err/(rate1*rate1)))
             end if
             if ((rate1.gt.0.0).and.(rate2.eq.0.0)) then
                 rates_ratio(j) = 1.0E32
                 rates_ratio_err(j) = 1.0E32
             end if
             if ((rate1.eq.0.0).and.(rate2.gt.0.0)) then
                 rates_ratio(j) = 0.0
                 rates_ratio_err(j) = 0.0
             end if
           end if
        end do
      end if
c
c
c
      return
      end  ! end calculate_counts_1mf
c
c
