
;=========================== MAIN: lzSWEnewmode ==============================
; Reads and process a SWE lz file (Based on processing code from RJF, Jan95)
;  ** Modified to accommodate new operational mode ("mode7"): MPH Jan03 **
;        --- Final version after further changes -- MPH 07/23/2004 ---

;     These allow sharing of state-variables with the WGGS tool environment...
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m7stuff,hkm7,sdatc7 ; ------ Stores data read by 'mode7map'
common log_delog,comp_tbl,dcomp_tbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common magstuff,magfile,tpb5,bgse
common wstuff,wst & common swestuff,swest & common shared,d

define_widgets & setpaths ;                  Initialize WGGS tool environvent.

; ----------------------------------------------------------------------------
; ------------------------- Define input parameters --------------------------
mom_version = 'v15' ;   New-mode moments version ID (oldest previous was v14).
pit_version = 'v06' ;  Same as pitch-v05, but counts coming from mode7 strahl.
avg_version = 'v06' ;  Same as pitch-avg-v05, counts coming from mode7 strahl.
str_version = 'v08' ;              Strahl version ID (oldest previous was v7).

doswf = 0 ;  If 1, find pitch angle in frame of solar wind (using ion KP vel.)
oknoatt = 0 ; If 1 then ok to NOT use attitude data--with 180 deg. rot. about
            ;  X-axis used.  Note: This selection only applies to reading LZ
            ;  data (using 'lzinput'), approximation is ALWAYS "ok" here.
lprnt = 1 & eleion = 0 ;     Strahl instrument does not take ion measurements.
scpot = 0 ; Spacecraft potential--this version does not use spacecraft
          ;       potential corrected data, spacecraft potential assumed zero.
univgmask = 0 ;   If 1 then use universal glint mask (not defined for strahl).

recs = [1,2000] ; Set to read all records in each daily file (2000 max. poss.)
; -------------------------- end input parameters ----------------------------
; ----------------------------------------------------------------------------

print,'Dates to be processed:' ;     Generate list of dates-to-be-processed...
openr,dateLUN,getenv('IDLSAV')+'lzpitch_dates',/Get_LUN
while (not eof(dateLUN)) do begin ;            For each date listed in file...
   date = '' & readf,dateLUN,date & print,date ;          Read and print date.
endwhile & free_LUN,dateLUN

print,'processing options:' ;                                 Screen output...
print,'oknoatt ',oknoatt & print,'scpot ',scpot & print,'doswf ',doswf
print,'record range ',recs & print,'univgmask ',univgmask

answ = '' & print,'Hit return to continue, or any other key to stop.' 
read,answ & if (answ ne '') then stop ;     To examine init. processing state.

; ***************************************************************************
; ---------------- Begin electron-data processing for each day --------------- 
dateLUN = 3 & idate = -1 ;         Initialize selected-date state variables...
close,dateLUN & openr,dateLUN,getenv('IDLSAV')+'lzpitch_dates'

while (not eof(dateLUN)) do begin ; For each date in list of selected dates...
  idate = idate+1 & date = '' & readf,dateLUN,date ;   Get next selected date.

  panelist & structuresw ; ------- Initialize WGGS structures (d,wst,swest)...
  ;       Set background removal flag (init. set to 'Yes' in structuresw.pro).
  swest.subtrbkg = 'No' ;  Note: "background" undefined for strahl instrument.
  ;           Store other relevant input parameters, including current date...
  swest.univgmask = univgmask & do_swf = doswf ;       Store input parameters.
  wst.date_file = 'Date' & wst.indate = date & wst.lzindate = wst.indate  

  for ilun=100,128 do free_LUN,ilun ;   Reset file units allocated by get_LUN.

  point1:
  if (do_swf ne 0) then begin ;   If user has opted to use solar-wind frame...
     print,"Use ion KP's to determine solar wind velocity..." ; Screen output.

     ; --------------------- Read SWE ion KP's for determining sc potential...
     idatype = where(d.datype eq 'swe_ionkp') & input,idatype,Err=err
     if (err ne '') then begin & print,err & do_swf = 0 & goto,point1 & endif  
  endif else print,"Solar wind velocity will not be used." ;    Screen output.

  ctmmode = ['u','m','s','e'] & ctmrate = ['s','f'] ;  Set useful constants...
  elecion = ['electrons','ions'] & rfill = -1.0E31 & lpr = 0 ; Verbose output.

  decompress_tbl ; ------- Read compress/decompress (8-bit <-> 12-bit) tables.
  ihkmap,ihk ; ---- Get inds. of instrument housekeeping into mjf array, lz.mf

  ; --- Get mode1, mode1 tm map of science and genl hk data offsets into lz.mf
  mode1map & mode6map & mode2map
  mode7map ; ----------- Added to this version to accomodate new mode (mode7).

  ; --------- Get mode1 and mode2 sun phase angles of detectors, unit vectors.
  phasem1 & phasem2

  ; ----- Open LZ data file and read header, read mag, orb-att, background
  ;        data, glint masks and process first two records to get spin period.
  lzinput,Err=err,OKnoAtt=oknoatt & if (err ne '') then stop,err
  print,'' & print,'scimode: ',vsmjf.scimode & print,'' ;       Screen output.

  ; --- Get size of orb-att arrays (& init. vals. of other state variables)...
  szpos = size(gse_pos) & szra = size(gse_ra) & szdec = size(gse_dec)
  last_scimode = -1 & last_atindx = -1 & espct = -1l & iondx = 0
  first_MOM_recnout = 1 & first_PITCH_recnout = 1 & first_STRL_recnout = 1
  first_AVG_recnout = 1 ; Initalize first-record flags (for making headers)...

  ; ------------------------------- Open electron NEWMOMdata file for writing.
  openw,momLUN,getenv('IDLSAV')+'newmom.dat',/Get_LUN

  ; -------------------------------- Open electron PITCHdata file for writing.
  openw,pitLUN,getenv('IDLSAV')+'pitch.dat',/Get_LUN

  ; ------------------------------- Open electron PTCHAVdata file for writing.
  openw,avgLUN,getenv('IDLSAV')+'ptchav.dat',/Get_LUN

  ; ------------------------------- Open electron STRAHLdata file for writing.
  openw,strLUN,getenv('IDLSAV')+'strahl.dat',/Get_LUN

  ; ---------------- Set record-number range for current file (current day)...
  recn_range = [recs[0],(recs[1]<fh.nmf)] ;   [1,fh.nmf] ==> read entire file.
  recn1 = recn_range[0] & recn2 = recn_range[1]
  print,'date, recn1, recn2: ',date,recn1,recn2 ;               Screen output.

  ; !!!!!!!!!!!!!!!!!!!!!!!!! Begin record loop !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  for recn=recn1,recn2 do begin ; For every rec. (major frame) in curr. day...
    ; --- Process selected LZ rec. (set keyword Lpr=1 to print each record)...
    proc_rec,date_time,TMmode_IHK=tmmode_ihk,Lpr=lpr,$
             Elec_Ion_m1=elec_ion_m1,Err=err & scimodechange = 0  
    if (err ne '') then begin ;  If an error occurs while processing record...
       print,recn,'  ',err & goto,endrecordloop ;    Print err. & skip record.
    endif

    ; ------------------------- Determine whether in telemetry science mode...
    tmmode_ihk = get_bits(lz.mf[ihk[2].offs],ihk[2].bv[1].p,ihk[2].bv[1].n)
    if (tmmode_ihk ne 2) then begin ;                If not in science mode...
      print,'' & print,'tm NOT in sci. mode, tm mode = ',ctmmode(tmmode_ihk)
      goto,endrecordloop ;                        Print error and skip record.
    endif

    ; ---- Check data quality--if one mnf flag set, then skip entire record...
    n_mnf = 250 ;                       Set number of minor frames per record.
    if (total(lz.qlty[0:(n_mnf-1)]) gt 0) then goto,endrecordloop ;  Skip rec.

    ; ------------------------------- Check measured spacecraft spin-period...
    if ((sp.spinp lt (0.9*3.05)) or (sp.spinp gt (1.1*3.05))) then begin 
       print,'Bad spin-period.' & goto,endrecordloop ;  Print err. & skip rec.
    endif

    ; -------------------------------- Science mode (special "mode7" check)...
    if (lz.mf[hkm7[4].offs] eq 7) then scimode_ihk = 7 else begin
       ; Note: There is not actually a "mode7"; only a "mode7 indicator" byte
       ;          in the housekeeping data, which takes on the values 0 and 7.
       print,'NOT in new mode (science mode 7)!' & goto,endrecordloop ;  Skip.
    endelse

    ; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    ; ---------------- Ready to process mode7 electrons.  -------------------
    ndets = vsmjf.n_strdets ;  Replace VEIS numbers with Strahl numbers here,
    nvsteps = vsmjf.n_strensteps ;      then replace spins with spectra below.
    nsectors = vsmjf.n_strsects ; Note: new mode (mode7) always uses exactly
    nspins = vsmjf.n_strspins ;    vsmjf.n_strspects spectra per major-frame,
    nspects = vsmjf.n_strspects ;    but vsmjf.n_strspins is only an estimate.

    ; -------------------------- Transform unit vectors from payload to GSE...
    atindx = fix((vsmjf.sec)/600) ; Atfile record number from 10 minute index.
    if ((scimode_ihk ne last_scimode) or (atindx ne last_atindx)) then begin  
       print,'recn, new atindx: ',recn,atindx ;              Print new status.
       wc_gse = dblarr(3) & vunit_gse = dblarr(ndets,nsectors,nvsteps,3)

       if (atfile ne '') then begin ; If spacecraft attitude data available...
          if ((atindx le (szra[1]-1)) and (atindx le (szdec[1]-1))) then begin
             for i=0,(ndets   -1) do $ ;      For each detector, sector and
                for j=0,(nsectors-1) do $ ;                     energy-step...
                   for k=0,(nvsteps -1) do begin
                      payload_to_gse,[vsmjf.vunitstrl[i,j,k,0],$
                                      vsmjf.vunitstrl[i,j,k,1],$
                                      vsmjf.vunitstrl[i,j,k,2]],$
                                     [gse_ra[atindx],gse_dec[atindx]],$
                                     pay_gse_mtx,wc_gse  
                      vunit_gse[i,j,k,*] = wc_gse ; Convert to GSE unit-vects.
             endfor
          endif
       endif else begin ;          Otherwise use approximate transformation...
          if lprnt then print,$
              'No attitude data--using 180 deg rotation about x-axis instead.'
          lprnt = 0     
          ; Approx transform from payload to GSE: SWE spin axis along -zgse...
          for i=0,(ndets   -1) do $ ;         For each detector, sector and
             for j=0,(nsectors-1) do $ ;                        energy-step...
                for k=0,(nvsteps -1) do begin
                   vunit_gse[i,j,k,0] =  vsmjf.vunitstrl[i,j,k,0]
                   vunit_gse[i,j,k,1] = -vsmjf.vunitstrl[i,j,k,1]
                   vunit_gse[i,j,k,2] = -vsmjf.vunitstrl[i,j,k,2] 
          endfor
       endelse

       rgse = fltarr(3) ;  Next, store spacecraft orbit data (if available)...
       if (orbfile ne '') then begin
          if (atindx le (szpos[1]-1)) then rgse = gse_pos(atindx,*)/6373.0
       endif     
    endif & last_scimode = scimode_ihk & last_atindx = atindx

    ; #######################################################################
    ; -------------------- Start spectrum loop... ---------------------------
    for ispect=0,(nspects-1) do begin ;      For each spectrum in curr. rec...
      if (do_swf ne 0) then begin ;  User has opted to use solar-wind frame...
         refsec = pb5_sec(ymd_pb5(long(date))) ;   Get KP solar-wind speed....
         interpol_ion,d.swe_ionkpdat.ta,d.swe_ionkpdat.v,iondx,$
                      vsmjf.suntim_vsbl[ispect],refsec,vsw 
      endif else vsw = 0 ;                 ...Otherwise, assume speed is zero.
    
      timpb5 = vsmjf.pb5tim_vsbl[*,ispect] ;       Store PB5 time, this spect.

      ; ------------------------------------------- Get MFI 3sec mag. field...
      if (magfile ne '') then begin ;              If MFI data is available...
         mindx = fix((timpb5[2])/60000) ; Magfile rec. number from min. index.
         sindx = fix((timpb5[2]-(mindx*60000))/3000) ; 3sec ind., min. intrvl.
         b = bgse[mindx,0:2,sindx] & if (b[0] eq rfill) then goto,endspectloop 
      endif       

      ; --- Voltage steps: Offsets into voltage table 
      ;                               assumes all sectors electrons or ions...
      cnvrt_to_f = fltarr(16) & strlsteps = reform(vsmjf.strlstep)
      for j=0,14 do cnvrt_to_f[j] = total(vsmjf.strl_cts_factor[*,j])/6.0 

      ; --------- Get electron energies and speeds each strahl voltage step...
      energy = volt_en_strl(strlsteps,/En) ; eV.
      velocity = double(volt_en_strl(strlsteps,/Vel)) ; cm/s.

      ; ------------- Get counts for each detector, sector, and energy-step...
      cblk = fltarr(ndets,nsectors,nvsteps)
      cblk[*,*,*] = dcomp_tbl[vsmjf.strl[*,*,*,ispect]]
                    ; (decompressed) new mode (mode7) strahl counts used.

      ; ---------------------------------------------------------------------
      ;     !!! Masking of "new-mode-glint" at offending energies,detectors...
      ; In the new mode, each given energy ALWAYS acculmulates counts at the
      ;  SAME azimuth, each sector.  Those energies always "seen" NEAR (+/- 7
      ;  deg.) or AT (+/- 1 deg.) the sun must have counts rem. to guarantee
      ;  meas. only genuine solar wind flux (not solar-photon-excited flux).

      ;for ith_energy=0,14 do $ ; View counts (each energy) BEFORE masking...
      ;   live_surface,cblk[*,*,ith_energy],Title='Energy-level: '+$
      ;                string(ith_energy,Format='(I2.2)')+' BEFORE masking'

      all_dets = [0,1,2,3,4,5] & middle_dets = [2,3] ;     Define offenders...
      near_sun_sector = 3 ;      Only this sector "looks" directly at the sun.
      ;   no_sun_energies = [0,1,      5,6,      10,11       ]  (for ref.)
      direct_sun_energies = [      3,        8,          13  ] ;   Remove all.
        near_sun_energies = [    2,  4,    7,  9,     12,  14] ;   Remove all.

      cblk[all_dets,near_sun_sector,direct_sun_energies] = 0 ; Removing
      cblk[all_dets,near_sun_sector,  near_sun_energies] = 0 ;         counts.

      ;for ith_energy=0,14 do $ ;   View counts (each energy) AFTER masking...
      ;   live_surface,cblk[*,*,ith_energy],Title='Energy-level: '+$
      ;                string(ith_energy,Format='(I2.2)')+' AFTER masking'
      ;stop ; ------------ End masking of "new-mode-glint". -----------------
      ; ---------------------------------------------------------------------

      ; ---------------------------------------------------------------------
      ; ----- Speed/energy-distribution fitting for new mode (mode7) counts...
      ; Two electron-moments parameters (density and temperature) are
      ;    derived here by Kappa-model-fitting, using A. F-Vinas' technique...

      ; ---------------------- Initialize output electron MOMENTS structure...
      mdata = {lzrec:fix(recn),spectm:fix(ispect),$ ;     Record-and-spectrum.
               tjdsec_spectm:vsmjf.suntim_vsbl[ispect],pb5tim:timpb5,$ ; Time.
               is_null:byte(vsmjf.is_null[ispect]),$ ; 1B ==> "null spectrum".
               neout:0.0,$ ;                   Electron density (#/cc) result.
               enthm:0.0,$ ;         Average thermal energy (eV) of electrons.
               teout:0.0,$ ;                  Electron temperature (K) result.
               bounds:fltarr(2,3),$ ;  Fit bounds: (A_Kappa,s,E_bar)[min,Max].
               fitres:fltarr(6),$ ; [A_Kappa,s,Kappa,E_bar,Chi_sq,time (sec)].
               misc:intarr(2),$ ; Num. of attempts, iterations (last attempt).
               rgse:rgse} ;             Spacecraft position in Re, GSE coords.

      ;        Initialize all fitting parameters and results to fill values...
      A_Kappa_min = !Values.d_NaN & A_Kappa_Max = !Values.d_NaN
      s_min = !Values.d_NaN & s_Max = !Values.d_NaN
      E_bar_min = !Values.d_NaN & E_bar_Max = !Values.d_NaN
      A_Kappa = !Values.d_NaN & s_Kappa = !Values.d_NaN
      E_bar = !Values.d_NaN & E_thermal = !Values.d_NaN
      Kappa = !Values.d_NaN & attempts = -1 & iterations = -1
      fitting_time = !Values.d_NaN & n_elec = !Values.d_NaN
      T_elec = !Values.d_NaN & Chi_sq = !Values.d_NaN

      if vsmjf.is_null[ispect] then goto,end_of_fitting ;   Null--so skip fit.

      T_fitting = systime(/Seconds) ; Note start-time (in seconds) of fitting.

      ; Integrate over all detectors, then sectors--each energy; afterwards
      ;    compensate for the "sun sector" loss in effected energy channels...
      ; Note: If, for a given energy E, all detectors of 1 (out of 8 total)
      ;        sector are masked, we would expect to see (7/8) of the "true"
      ;        flux: total_counts_measured(E) = (7/8)*total_counts_true(E).
      compensation_factor = double(8.0/7.0) ;   This factor should compensate.
      avg_counts_at_each_energy = total(total(float(cblk),1,/Double),1)/48.0d0
      avg_counts_at_each_energy[direct_sun_energies] = $ ;   Lost sector comp.
         avg_counts_at_each_energy[direct_sun_energies]*compensation_factor
      avg_counts_at_each_energy[  near_sun_energies] = $ ;   Lost sector comp.
         avg_counts_at_each_energy[  near_sun_energies]*compensation_factor

      ; From (compensated) counts at each energy E, find f(E).  This includes
      ;  averaging both f(E) and cnts. @ E over E's redundant energy-channels.
      f_at_each_energy = avg_counts_at_each_energy*double(cnvrt_to_f[0:14])
      mean_11_12 = mean(f_at_each_energy[11:12]) ; Average over energy-steps
      mean_13_14 = mean(f_at_each_energy[13:14]) ;     with repeated energies.
      f_at_each_energy = [f_at_each_energy[0:10],mean_11_12,mean_13_14]
      mean_11_12 = mean(avg_counts_at_each_energy[11:12]) ; Average over
      mean_13_14 = mean(avg_counts_at_each_energy[13:14]) ;     counts from 
      avg_counts_at_each_energy = $ ;                repeated energy channels.
         [avg_counts_at_each_energy[0:10],mean_11_12,mean_13_14]

      ; Find the energies(eV), standard-error in the COUNT value of each en...
      energies_used = [energy[0:11],energy[13]] ;  List of energies used (eV).
      error_at_each_energy = sqrt(avg_counts_at_each_energy) ;     Poisson SD.

      ; Define the (x,y) points to be fitted, initial model-parameter values
      ;  and model-parameter constraints--then map into unconstrained space...
      f = f_at_each_energy ;   Re-assign these values to shorter var. names...
      E = double(energies_used) & Y = alog(f_at_each_energy) ; x=E,y=ln[f(E)].
      A = double([-59.9,-3.00,0.050]) ;  (CONSTRAINED) Init. parameter values.

      common param_bounds,a_min,a_Max,b_min,b_Max,c_min,c_Max ;  Defined here.

      ;                 See MPH notes for derivation of these limiting values.
      dlnf_dE = (alog(f[1]/f[0])/(E[1]-E[0])) < (alog(f[2]/f[0])/(E[2]-E[0]))
      a_min = double(-60.0) ;  A_Kappa range: a_min < a = ln(A_Kappa) < a_Max.
      a_Max = alog(f[0])-(dlnf_dE*E[0]) ;     ln(f)-intercept, steepest slope.
      A_Kappa_min = exp(a_min) & A_Kappa_Max = exp(a_Max) ;  Save A_Kappa rng.
      s_min = 2.6d0 & s_Max = 7.0d0 & s_range = [s_min,s_Max] ;    Range of s.
      E_bar_min = 1.0 ;                     Range of E_bar (measured in eV)...
      E_bar_Max = ((E[12]*((f[12]/f[0])^(1.0d0/s_Max)))-E[0])>E[1]
      E_bar_range = [E_bar_min,E_bar_Max] ;     Note reversal of these ranges.
      b_range = -reverse(s_range) & c_range = 1.0d0/reverse(E_bar_range)
      b_min = b_range[0] & b_Max = b_range[1] ;             b_min < b < b_Max.
      c_min = c_range[0] & c_Max = c_range[1] ;             c_min < c < c_Max.
      A[0] = range_limit_map(A[0],a_min,a_Max,/Invert) ;   Map- A[0]: a -> a*.
      A[1] = range_limit_map(A[1],b_min,b_Max,/Invert) ;   Map- A[1]: b -> b*.
      A[2] = range_limit_map(A[2],c_min,c_Max,/Invert) ;   Map- A[2]: c -> c*.
      fitA = [1,1,1] & model_funct = 'av_kappa_funct' ;      Keyword settings.
      attempts = 0 & convergence = 0 ;              Initial convergence state.

      !Quiet = 1 ;     Set system variable to suppress informational messages.

      ;      If fewer than 4 attempts have been made and no convergence yet...
      while ((attempts lt 4) and (convergence ne 1)) do begin ; Try dropping
         model_fit_values = LMfit(E[attempts:*],Y[attempts:*],$ ;  energies...
                                  A,FitA=fitA,Function_Name=model_funct,$
                                  /Double,Iter=iterations,Itmin=10,Itmax=50,$
                                  Chisq=chi_sq,Convergence=convergence,$
                                  Tol=1.d-29,Weights=(1.0d0/$
                                           error_at_each_energy[attempts:*]))
         attempts = attempts+1 ;           Attempt completed, increment total.
      endwhile

      !Quiet = 0 ;        Return system variable to default ("noisy") setting.

      ; Retrieve the constrained model parameters from fit results by 
      ;    return-mapping--then "unpack" desired physical values from these...
      ;      Note: Unless specified otherwise below, energies are given in eV.
      A[0] = range_limit_map(A[0],a_min,a_Max) ; Map- A[0]: a* -> a = L_a[a*].
      A[1] = range_limit_map(A[1],b_min,b_Max) ; Map- A[1]: b* -> b = L_b[b*].
      A[2] = range_limit_map(A[2],c_min,c_Max) ; Map- A[2]: c* -> c = L_c[c*].
      ;     A_Kappa is in units of phase-space density: #/[{cm^3}*{(cm/s)^3}].
      A_Kappa = exp(A[0]) & s_Kappa = -A[1] & Kappa = s_Kappa-1 ; s,K Unitless
      E_bar = 1.0/A[2] & E_thermal = E_bar/Kappa ;            Energies, in eV.
      Boltzmann = 8.6173d-5 ;             Boltzmann's constant, given in eV/K.
      J_per_eV = 1.6022d-19 & m_electron = 9.1095d-31 ; En. conv. & m_e in kg.

      ;    Calc. final electron moments results (halo density, temperature)...
      n_elec = A_Kappa*(((2.0d0*!dPi)*$ ; Note: (J/kg)*10^4(cm/m)^2 = (cm/s)^2
                         (((E_bar*J_per_eV)/m_electron)*1.0d4))^(1.5d0))*$
                       (gamma(Kappa-0.5d0)/gamma(Kappa+1.0d0)) ;   ne, #/cm^3.
      T_elec = (Kappa/(Kappa-(3.0d0/2.0d0)))*(E_thermal/Boltzmann) ; Te, in K.

      ;    Fitting of this spectrum is finished, note elapsed time in seconds.
      fitting_time = systime(/Seconds)-T_fitting ;   Total fit time (in sec.).

      ;if ((n_elements(n_series) eq 0) or $ ;           If series are empty...
      ;    (n_elements(T_series) eq 0)) then begin
      ;   n_series = n_elec & T_series = T_elec ;   Save first elt. in series.
      ;endif else begin ;         Otherwise concatenate and plot new series...
      ;   n_series = [n_series,n_elec] & T_series = [T_series,T_elec]
      ;   wset,0 & plot,n_series,/Ylog & wset,1 & plot,T_series,/Ylog
      ;endelse

      ;wset,2 ;                   Set active window to energy-spectrum plot...
      ; Plot markers at ALL of the MEASURED (E,log_10[f(E)]) points in dist...
      ; plot,energies_used,alog10(f_at_each_energy),Yrange=[-33,-24],$
      ;      Ystyle=1,Title='Log-phase-density vs. Energy (new mode Strahl)',$
      ;      Xtitle='Energy (eV)',Ytitle='log_10(f)',Psym=2
      ;oplot,energies_used[(attempts-1):*],$ ;     Overplot one-count-level...
      ;      (alog10(cnvrt_to_f[[indgen(12),13]]))[(attempts-1):*],Psym=3

      ;     Overplot fitted curve, with error bars, at energies NOT skipped...
      ;oplot,energies_used[(attempts-1):*],alog10(exp(model_fit_values))
      ;oploterr,energies_used[(attempts-1):*],alog10(exp(model_fit_values)),$
      ;         alog10(1.0+(1.0/error_at_each_energy[(attempts-1):*])),3
      ;if (attempts gt 1) then $ ;      Add "fill" values, outside format rep.
      ;   model_fit_values = [replicate(1.0d5,(attempts-1)),model_fit_values]

      ;                         Output summary of fitting results to screen...
      ;print,'' & print," -!-!-!-!- Limits placed on model parameters:"
      ;print,"   A_Kappa_min, A_Kappa_Max, a_min, a_Max: ",$ ; Limits on a,
      ;           exp(a_min),  exp(a_Max), a_min, a_Max ;             A_Kappa.
      ;print,"   s_min, s_Max, b_min, b_Max: ",$ ;           Limits on b, s...
      ;          s_min, s_Max, b_min, b_Max
      ;print,"   E_bar_min, E_bar_Max, c_min, c_Max: ",$ ; Lim. on c, E_bar...
      ;          E_bar_min, E_bar_Max, c_min, c_Max
      ;print,"Results of model fit to this spectrum: " ;  Model-fit results...
      ;print,"   Total attempts (max. 4) to get convergence: ",attempts
      ;print,"   LM Iterations (last attempt): ",iterations
      ;print,"   Total time (in seconds) for fitting: ",fitting_time ;   Time.
      ;print,"   Chi-squared (log of normalized value): ",$ ;   Chi-squared...
      ;      alog10(chi_sq/(float(n_elements(f))-float(attempts)))
      ;print,"   E, Y (= ln[f(E)]), model_fit_values: " ;         Final fit...
      ;print,    E, Y,            model_fit_values,Format='(13F10.4)'
      ;print,"   A (a, b, c): " & print,A,Format='(3F12.6)' ;  Final params...
      ;print,"   A_Kappa, s, E_bar: ",A_Kappa,s_Kappa,E_bar ;  "phys." params.
      ;print,"   Kappa, E_thermal: ",Kappa,E_thermal ; Derived "phys." params.
      ;              Final results for moments calculation on this spectrum...
      ;print,"   n_elec, T_elec: " & print,n_elec,T_elec,Format='(2E12.4)'
      ;print," -!-!-!-!- "

      end_of_fitting: ;          Skip to here in the event of a null spectrum.
      ; ----- End calculation/plotting for speed/energy-distribution. -------
      ; ---------------------------------------------------------------------

      ; ----------- Make output electron MOMENTS data structure assignments...
      mdata.neout = float(n_elec) ;            Electron density (#/cc) result.
      mdata.enthm = float(E_thermal) ;  Avg. thermal energy (eV) of electrons.
      mdata.teout = float(T_elec) ;           Electron temperature (K) result.
      mdata.bounds = [[A_Kappa_min,A_Kappa_Max],$ ;     Fit bounds: (A_Kappa,
                      [      s_min,      s_Max],$                          s,
                      [  E_bar_min,  E_bar_Max]] ;                     E_bar).
      mdata.fitres = [A_Kappa,s_Kappa,Kappa,E_bar,chi_sq,fitting_time]
      mdata.misc = [attempts,iterations] ;          # att., iter. (last att.).

      writeu,momLUN,mdata ; -------------- Write electron MOMENTS output data.

      if first_MOM_recnout then begin ; ---------- Save MOMENTS header info...
         mhedr = {nrec:0l,thisdate:string('',Format='(I8)'),thisrecn:0l,$
                  date:string('',Format='(A8)'),scimode:0l,scimodechange:0l,$
                  oknoatt:0l,ndets:0l,nvsteps:0l,nsectors:0l,max_enstep:0l,$
                  ensteps:strlsteps,cnvrt_to_f:cnvrt_to_f,misc:intarr(3),$
                  comp_fact:compensation_factor} ;        For "glint-masking".

         mhedr.thisrecn=recn & mhedr.date = string(date,Format='(A8)')
         mhedr.scimode = scimode_ihk & mhedr.scimodechange = scimodechange
         mhedr.oknoatt = oknoatt & mhedr.ndets = vsmjf.n_strdets
         mhedr.max_enstep = volt_en_strl(max(strlsteps),/En) ;  Allowed att.,
         mhedr.nvsteps = vsmjf.n_strensteps & mhedr.misc = [4,10,50] ;   iter.
         mhedr.nsectors = vsmjf.n_strsects & first_MOM_recnout = 0
      endif 

      ;  Find pitch-angles of every detector-measurement wrt B (GSE) vector...
        w = vunit_gse & wp = dblarr(ndets,nsectors,nvsteps,3) ;    Velocities.
      mag = double(b) & pa = dblarr(ndets,nsectors,nvsteps) ; B, pitch-angles.
      for i=0,(ndets-1) do $ ;      For each detector, sector and
         for j=0,(nsectors-1) do $ ;                            energy-step...
            for k=0,(nvsteps-1) do begin
               wp[i,j,k,0] = velocity[k]*w[i,j,k,0] - (vsw*1.0E5)
               wp[i,j,k,1] = velocity[k]*w[i,j,k,1] ; Find "true" (sel. frm.)
               wp[i,j,k,2] = velocity[k]*w[i,j,k,2] ;  electron  velocities...
               wpmag = sqrt((wp[i,j,k,0]^2)+(wp[i,j,k,1]^2)+(wp[i,j,k,2]^2))
               vpar = ((wp[i,j,k,0]*mag[0])+$ ;        Calc. norm. dot-prod...
                       (wp[i,j,k,1]*mag[1])+$
                       (wp[i,j,k,2]*mag[2])  )/sqrt(total(mag*mag)) 
               pa[i,j,k] = float(acos(vpar/wpmag)/!DtoR) ;  Calc. pitch-angle.
      endfor
    
      ; ---------------------------------------------------------------------
      ; ---------- Do PITCH-ANGLE binning and energy spectrum for electrons...

      ; ------------------ Initialize output electron PITCH-ANGLE structure...
      pdata = {lzrec:0l,spinbl:0l,tjdsec_spinbl:0.0d0,pb5tim:lonarr(3),$
               vsteps:bytarr(16),fspa:bytarr(16),f:bytarr(30,16),$
               b:fltarr(3),eleion:0l,misc:fltarr(9),misc2:bytarr(8),$
               gains:fltarr(6),cnvrt_to_f:fltarr(16),rgse:fltarr(3)}

      ;  Replace "zeroed-out" glint-count values with invalidity flags (-1)...
      cblk[all_dets,near_sun_sector,direct_sun_energies] = -1 ; Flagging
      cblk[all_dets,near_sun_sector,  near_sun_energies] = -1 ;        counts.
      ; Note: When summing over bins, these flagged count-values are NOT USED.

      ; --- Arrange data arrays according to energy step and pitch angle, 
      ;                       organized as: nvsteps by ndets*nsectors...
      cp = reform(transpose(cblk,[2,0,1]) , [nvsteps,ndets*nsectors])
      pa = reform(transpose(pa  ,[2,0,1]) , [nvsteps,ndets*nsectors])

      ; ------ Measured count data will be put in pitch-angle bins = dp deg...
      np = 30 & dp = 180/np & ip = fix(pa/dp)*dp ;    Define pitch-angle bins.
     
      ; --------------------------------------- Do pitch-angle binning here...
      fbin = fltarr(np,nvsteps) & favg = fltarr(nvsteps) ;      Init. results.

      for i=0,(nvsteps-1) do begin ;                  For each energy step....
         sum = 0.0 & nsum = 0 ;    Initialize running-sum and # contrib. dets.

         for jp=0,np-1 do begin ;                  For each pitch-angle bin...
            wh = where(((ip[i,*] eq (jp*dp)) and (cp[i,*] ge 0)),nwh)

            if (nwh ne 0) then begin ;                  If bin is non-empty...
               fbin[jp,i] =     total(cp[i,wh])/nwh ;   Find counts-per-det.,
                      sum = sum+total(cp[i,wh]) & nsum = nsum+nwh ;   and tot.
            endif
         endfor & if (nsum ne 0) then favg[i] = sum/nsum ;     Counts-per-det.
      endfor
      ; -------- End doing electron PITCH-ANGLE calculations/binning. -------
      ; ---------------------------------------------------------------------

      ; ------- Make output electron PITCH-ANGLE data structure assignments...
      pdata.lzrec = long(recn) & pdata.spinbl = long(ispect) ;  Rec-and-spect.
      pdata.tjdsec_spinbl = vsmjf.suntim_vsbl[ispect] & pdata.pb5tim = timpb5
      pdata.vsteps[0:14] = strlsteps             ; --- Only 15 velocity steps.
      pdata.fspa[0:14] = comp_tbl[fix(favg+0.5)] ; --- Only 15 velocity steps.
      pdata.f[*,0:14] = comp_tbl[fix(fbin+0.5)]  ; --- Only 15 velocity steps.
      pdata.b = b & pdata.eleion = eleion & pdata.misc[0] = scpot ;    B, etc.
      pdata.misc[1] = vsw & pdata.misc2[1] = scimode_ihk ;               Misc.
      pdata.cnvrt_to_f[0:14] = cnvrt_to_f[0:14]  ; --- Only 15 velocity steps.
      pdata.rgse = rgse ;               Spacecraft position in Re, GSE coords.
        
      writeu,pitLUN,pdata ;--------------- Write electron output data to file.
      
      if first_PITCH_recnout then begin ; ---------- Save PITCH header info...
         phedr = {nrec:0l,thisdate:string('',Format='(I8)'),$
                  date:string('',Format='(A8)'),$
                  scimode:0l,oknoatt:0l,ndets:0l,nvsteps:0l,nsectors:0l,$
                  glnt:lonarr(3,64),ensteptbl:fltarr(64),$
                  max_enstep:0l,thisrecn:0l,scimodechange:0l,dummy:0l}

         phedr.date = string(date,Format='(A8)')
         phedr.scimode = scimode_ihk & phedr.oknoatt = oknoatt
         phedr.ndets = vsmjf.n_strdets & phedr.nvsteps = vsmjf.n_strensteps
         phedr.nsectors = vsmjf.n_strsects
         phedr.ensteptbl = volt_en_strl(indgen(64),/En)
         phedr.max_enstep = volt_en_strl(max(strlsteps),/En)
         phedr.scimodechange = scimodechange & first_PITCH_recnout = 0
      endif 

      ; ---------------------------------------------------------------------
      ; -------- Do PITCH-ANGLE AVERAGING and energy spectrum for electrons...

      ; ---------------- Initialize output electron PITCH-AVERAGE structure...
      adata = {lzrec:0l,spinbl:0l,tjdsec_spinbl:0.0d0,pb5tim:lonarr(3),$
               velocity:fltarr(16),fspa:fltarr(16),pbin:fltarr(5),$
               f:fltarr(4,16),b:fltarr(3),eleion:0l,misc:fltarr(9),$
               misc2:bytarr(8),gains:fltarr(6),rgse:fltarr(3)}

      f_bar_samples = fltarr(np,nvsteps) ;  <bins>x<en.-steps> samps. of f_bar.
      for i=0,(nvsteps-1) do $ ; For each en.-step, convert counts to f vals...
         f_bar_samples[*,i] = dcomp_tbl[reform(pdata.f[*,i])]*cnvrt_to_f[i]

      ; ------- Measured PHASE-DENSITIES intgrated by pitch-angle (see list)...
      ; Note: Computes phase-space integrals over large annular regions, using
      ;        pitch-angle-distribution results from above (see MPH notes)...
      ;       For each given energy, we use the LARGE pitch-angle bins:
      ;              0-60 deg, 60-90 deg, 90-120 deg, 120-180 deg
      ;        (these are each pi steradians in solid angle [see MPH notes]).
      np_avg = 4 & pbin = [0.0, 60.0, 90.0, 120.0, 180.0] ;  Pitch-angle bins.
      pbin = pbin*!DtoR ;   Temporarily map pitch-bin list into RADIAN values.

      ; ----------------------------------- Do pitch-angle integration here...
      fbin = fltarr(np_avg,nvsteps) & favg = fltarr(nvsteps) ;  Init. results.
      a_i = findgen(np)*(dp*!DtoR) ;  INITIATING angle (in radians), each bin.
      a_f = a_i+(dp*!DtoR) ;               FINAL angle (in radians), each bin.
      awf = (2.0*!pi)*(cos(a_i)-cos(a_f)) ; Angular weighting factor, per bin.

      for i=0,(nvsteps-1) do begin ;                  For each energy step....
         for jp=0,np_avg-1 do begin ;       Integrate over pitch-angle bins...
            wh = where(((a_i ge pbin[jp]) and (a_i lt pbin[jp+1])),nwh)
            if (nwh ne 0) then fbin[jp,i] = total(f_bar_samples[wh,i]*awf[wh])
         endfor & favg[i] = total(f_bar_samples[*,i]*awf)/(4.0*!pi) ; AVERAGE.
      endfor
      ; ------- End doing electron PITCH-AVERAGE calculations/binning. ------
      ; ---------------------------------------------------------------------

      ; ----- Make output electron PITCH-AVERAGE data structure assignments...
      adata.lzrec = long(recn) & adata.spinbl = long(ispect) ;  Rec-and-spect.
      adata.tjdsec_spinbl = vsmjf.suntim_vsbl[ispect] & adata.pb5tim = timpb5
      adata.velocity[0:14] = velocity            ; --- Only 15 velocity steps.
      adata.fspa[0:14] = favg[0:14]              ; --- Only 15 velocity steps.
      adata.pbin = pbin*!RADEG ;   List of UNIFORM SOLID-ANGLE averaging bins.
      adata.f[*,0:14] = fbin[*,0:14]             ; --- Only 15 velocity steps.
      adata.b = b & adata.eleion = eleion & adata.misc[0] = scpot ;    B, etc.
      adata.misc[1] = vsw & adata.misc2[1] = scimode_ihk ;               Misc.
      adata.misc2[2] = byte(vsmjf.is_null[ispect]) ;   1B ==> "null spectrum".
      adata.rgse = rgse ;               Spacecraft position in Re, GSE coords.

      writeu,avgLUN,adata ;--------------- Write electron output data to file.
      
      if first_AVG_recnout then begin ; ---------- Save AVERAGE header info...
         ahedr = {nrec:0l,thisdate:string('',Format='(I8)'),$
                  date:string('',Format='(A8)'),$
                  scimode:0l,oknoatt:0l,ndets:0l,nvsteps:0l,nsectors:0l,$
                  glnt:lonarr(3,64),ensteptbl:fltarr(64),$
                  max_enstep:0l,thisrecn:0l,scimodechange:0l,dummy:0l}

         ahedr.date = string(date,Format='(A8)')
         ahedr.scimode = scimode_ihk & ahedr.oknoatt = oknoatt
         ahedr.ndets = vsmjf.n_strdets & ahedr.nvsteps = vsmjf.n_strensteps
         ahedr.nsectors = vsmjf.n_strsects
         ahedr.ensteptbl = volt_en_strl(indgen(64),/En)
         ahedr.max_enstep = volt_en_strl(max(strlsteps),/En)
         ahedr.scimodechange = scimodechange & first_AVG_recnout = 0
      endif

      ; ---------------------------------------------------------------------
      ; ------- Begin electron STRAHL analysis on pitch-angle distributions...

      ; ----------------------- Initialize output electron STRAHL structure...
      sdata = {lzrec:fix(recn),spectm:fix(ispect),$ ;     Record-and-spectrum.
               tjdsec_spectm:vsmjf.suntim_vsbl[ispect],pb5tim:timpb5,$ ; Time.
               is_null:byte(vsmjf.is_null[ispect]),$ ; 1B ==> "null spectrum".
               mx_B_cnts:intarr(nvsteps),$ ; Peak (max. cts.) of p-a:[ 0, 90].
               mx_aB_cts:intarr(nvsteps),$ ; Peak (max. cts.) of p-a:[90,180].
               pa_mBcnts:intarr(nvsteps),$ ;  Location (p-a) of [ 0, 90] peak.
               pa_maBcts:intarr(nvsteps),$ ;  Location (p-a) of [90,180] peak.
               B_baw_hmx:intarr(nvsteps),$ ;   [ 0, 90] beam-width @ HALF-max.
               aB_bw_hmx:intarr(nvsteps),$ ;   [90,180] Beam-width @ HALF-max.
               b:b,$ ;   MFI (interpolated) 3-second B-field vector (GSE), nT.
               rgse:rgse} ;             Spacecraft position in Re, GSE coords.

      fill_values = replicate(-1,nvsteps) ;   Init. anly. res. to fill vals...
      sdata.mx_B_cnts = fill_values & sdata.mx_aB_cts = fill_values
      sdata.pa_mBcnts = fill_values & sdata.pa_maBcts = fill_values
      sdata.B_baw_hmx = fill_values & sdata.aB_bw_hmx = fill_values

      if vsmjf.is_null[ispect] then goto,end_of_strahl ; Null--so skip strahl.

      for ith_enstp=0,(nvsteps-1) do begin ;   Results for each energy-step...
         ;en_str = string(volt_en_strl(strlsteps[ith_enstp],/En),$
         ;                Format='(F10.4)') ; String with actual energy value.
         ;print,'' & print,'+-+-+-+-+-+-+-+-+' ;  Print header, this energy...
         ;print,'Results of strahl analysis for'+en_str+' eV...'

         ; Note: In what follows, "along B" refers to the pitch-angles of
         ;        electrons travelling nearly PARALLEL to the local magnetic
         ;        field direction (B); "against B" refers to the same, but for
         ;        travel nearly ANTI-PARALLEL to the magnetic field (aB).
         pa_bins = dcomp_tbl[reform(pdata.f[*,ith_enstp])] & hnp = np/2
         pa_B_bins = pa_bins[ 0 :hnp-1] ;    Extract "raw" pitch-angle counts
         pa_aB_bns = pa_bins[hnp: np-1] ;        "along B", and "against B"...
         max_B_cnts = max(pa_B_bins) ;  ...then find "raw" maximum "along B",
         max_aB_cts = max(pa_aB_bns) ;                        and "against B".

         sdata.mx_B_cnts[ith_enstp] = fix(max_B_cnts) ;     Store "raw" B-max.
         sdata.mx_aB_cts[ith_enstp] = fix(max_aB_cts) ;    Store "raw" aB-max.

         ;     Plot "raw" pitch-angle counts and "raw" maximum (both sides)...
         ;plot,pa_bins,Title='Pitch-angle bins, E ='+en_str+' eV',$
         ;     Xtitle='Bin index (6 degrees wide--nth bin '+$
         ;            'acculmulates [n*6,(n+1)*6] degree pitches)',$
         ;     Ytitle='Strahl counts',Charsize=1.25
         ;oplot,pa_bins,Psym=2,Color=140,/NoClip ; Overplot points on "curve".
         ;plots,[ 0,hnp-.5],[max_B_cnts,max_B_cnts] ; Draw "raw" B-max. level,
         ;plots,[hnp-.5,np],[max_aB_cts,max_aB_cts] ;           aB-max. level.
         ;plots,[hnp-.5,hnp-.5],[0,5000],NoClip=0,Thick=3 ;     Draw division.

         for rep=1,2 do pa_B_bins = smooth(pa_B_bins,3) ;  Smth. B-side, loc.
         max_counts = max(pa_B_bins,max_cnts_ind) ;          "smoothed" B-max.

         ; Store pitch-angle (in degrees) of "smoothed" maximum ("ALONG B")...
         sdata.pa_mBcnts[ith_enstp] = fix(max_cnts_ind*dp)+fix(dp/2.0)

         ;  Overplot "smoothed curve" and indicate "smoothed" max. position...
         ;oplot,pa_B_bins,Psym=-2,Color=40,/NoClip ;       Points AND "curve".
         ;plots,[max_cnts_ind,max_cnts_ind],[0,5000],Color=40,NoClip=0

         beam_start = (max_cnts_ind-1)>0 ;     "ALONG B": Initial "start" bin.
         beam_end   = (max_cnts_ind+1)<(hnp-1) ;          Initial "end"   bin.
         half_maximum = float(max_counts/2.0) & still_looking = 1 ; "smth."/2.

         ; Looking at the "ALONG B" pitch-angle counts...
         ;   Search for where "smoothed" counts fall away from max. by HALF...
         while (still_looking) do begin ;              While flag still set...
            if (pa_B_bins[beam_start] gt half_maximum) then $
               beam_start = (beam_start-1)>0 ;          Decrement "start" bin.
            if (pa_B_bins[beam_end  ] gt half_maximum) then $
               beam_end   = (beam_end  +1)<(hnp-1) ;    Increment "end"   bin.
            still_looking = (   ((pa_B_bins[beam_start] gt half_maximum) $
                                 and (beam_start ne 0      )            ) $
                             or ((pa_B_bins[beam_end  ] gt half_maximum) $
                                 and (beam_end   ne (hnp-1))            )  )
         endwhile

         sdata.B_baw_hmx[ith_enstp] = float((beam_end-beam_start)*dp) ;   (B).
         ;plots,[0,hnp-.5],[half_maximum,half_maximum],Color=254 ;    Display
         ;plots,[beam_start,beam_start],[0,5000],Color=254,NoClip=0 ; rslts...
         ;plots,[beam_end  ,beam_end  ],[0,5000],Color=254,NoClip=0

         for rep=1,2 do pa_aB_bns = smooth(pa_aB_bns,3) ; Smth. aB-side, loc.
         max_counts = max(pa_aB_bns,max_cnts_ind) ;         "smoothed" aB-max.
         if (max_counts eq 0) then max_cnts_ind = hnp-1l ;  Default to anti-p.

         ;  Store pitch-angle (in deg.) of "smoothed" maximum ("AGAINST B")...
         sdata.pa_maBcts[ith_enstp] = fix(max_cnts_ind*dp)+fix(dp/2.0)
         sdata.pa_maBcts[ith_enstp] = sdata.pa_maBcts[ith_enstp]+fix(hnp*dp)

         ;  Overplot "smoothed curve" and indicate "smoothed" max. position...
         ;oplot,(indgen(hnp)+hnp),pa_aB_bns,Psym=-2,Color=40,/NoClip ;  Shftd.
         ;plots,[max_cnts_ind,max_cnts_ind]+hnp,[0,5000],Color=40,NoClip=0

         beam_start = (max_cnts_ind-1)>0 ;   "AGAINST B": Initial "start" bin.
         beam_end   = (max_cnts_ind+1)<(hnp-1) ;          Initial "end"   bin.
         half_maximum = float(max_counts/2.0) & still_looking = 1 ; "smth."/2.

         ; Looking at the "AGAINST B" pitch-angle counts...
         ;   Search for where "smoothed" counts fall away from max. by HALF...
         while (still_looking) do begin ;              While flag still set...
            if (pa_aB_bns[beam_start] gt half_maximum) then $
               beam_start = (beam_start-1)>0 ;          Decrement "start" bin.
            if (pa_aB_bns[beam_end  ] gt half_maximum) then $
               beam_end   = (beam_end  +1)<(hnp-1) ;    Increment "end"   bin.
            still_looking = (   ((pa_aB_bns[beam_start] gt half_maximum) $
                                 and (beam_start ne 0      )            ) $
                             or ((pa_aB_bns[beam_end  ] gt half_maximum) $
                                 and (beam_end   ne (hnp-1))            )  )
         endwhile

         sdata.aB_bw_hmx[ith_enstp] = float((beam_end-beam_start)*dp) ;  (aB).
         ;plots,[hnp-.5,np],[half_maximum,half_maximum],Color=190 ; Results...
         ;plots,[beam_start,beam_start]+hnp,[0,5000],Color=190,NoClip=0
         ;plots,[beam_end  ,beam_end  ]+hnp,[0,5000],Color=190,NoClip=0

         ;print,'' ;                  Display final strahl analysis results...
         ;print,'Looking at the "ALONG B" pitch-angle counts...'
         ;print,'Counts at beam "core": ',         sdata.mx_B_cnts[ith_enstp]
         ;print,'Pitch-angle at beam "core": ',    sdata.pa_mBcnts[ith_enstp]
         ;print,'Beam angular (half-max.) width: ',sdata.B_baw_hmx[ith_enstp]
         ;print,''
         ;print,'Looking at the "AGAINST B" pitch-angle counts...'
         ;print,'Counts at beam "core": ',         sdata.mx_aB_cts[ith_enstp]
         ;print,'Pitch-angle at beam "core": ',    sdata.pa_maBcts[ith_enstp]
         ;print,'Beam angular (half-max.) width: ',sdata.aB_bw_hmx[ith_enstp]
         ;stop ;                 Stop here to display results for this energy.
      endfor

      end_of_strahl: ;           Skip to here in the event of a null spectrum.
      ; --------------------- End electron STRAHL analysis ------------------
      ; ---------------------------------------------------------------------

      writeu,strLUN,sdata ; --------------- Write electron STRAHL output data.

      if first_STRL_recnout then begin ; ---------- Save STRAHL header info...
         shedr = {nrec:0l,thisdate:string('',Format='(I8)'),thisrecn:0l,$
                  date:string('',Format='(A8)'),scimode:0l,scimodechange:0l,$
                  oknoatt:0l,ndets:0l,nvsteps:0l,nsectors:0l,max_enstep:0l,$
                  ensteps:strlsteps,misc:[np,dp,2,3]} ; [bins,deg.,smths,win.]

         shedr.thisrecn=recn & shedr.date = string(date,Format='(A8)')
         shedr.scimode = scimode_ihk & shedr.scimodechange = scimodechange
         shedr.oknoatt = oknoatt & shedr.ndets = vsmjf.n_strdets
         shedr.max_enstep = volt_en_strl(max(strlsteps),/En)
         shedr.nvsteps = vsmjf.n_strensteps
         shedr.nsectors = vsmjf.n_strsects & first_STRL_recnout = 0
      endif 

      ; @@@@@@@@@@@@@@@@@@@@ End processing mode7 electrons @@@@@@@@@@@@@@@@@
      espct = espct+1 ; ------------------ Increment processed-spectrum count.

      if lpr then $ ;        Produce verbose screen output per user request...
         print,"Processed: ",recn,ispect,vsmjf.tjd,vsmjf.sec,$
                             pb5_ymdhms(sec_pb5(vsmjf.suntim_vsbl[ispect]))

      endspectloop: 

    endfor ; ################### End of SPECTRUM loop. ######################

    skiprec = 10 ; ---------------- Send periodic progress report to screen...
    if ((fix(recn/skiprec)*skiprec) eq recn) then  begin
       print,'lztim: recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl[0]'
       print,        recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl[0]
    endif

    endrecordloop:

  endfor ; !!!!!!!!!!!!!!!!!!!!!! End of RECORD loop. !!!!!!!!!!!!!!!!!!!!!!!

  ; ------- Final cleanup before outputting results from current daily file...
  nrecele = long(espct+1) ; ------ Find total num. of elec. spect., this file.
  free_LUN,momLUN & free_LUN,pitLUN & free_LUN,avgLUN
  free_LUN,strLUN ;  Close data files (all of processed day's data is stored).

  ; ----------------- Find current date (on which processing was PERFORMED)...
  y = strmid(systime(0),20,4) & mos = ['Jan','Feb','Mar','Apr','May','Jun',$
                                       'Jul','Aug','Sep','Oct','Nov','Dec']
  m = string((where(mos eq strmid(systime(0),4,3))+1),Format='(I2)')
  d = strmid(systime(0),8,2) & thisdate = (y*10000l)+(m*100l)+(d*1l)
  print,'thisdate ',thisdate ;               Display date processing occurred.

  if (nrecele ne 0) then begin ; If electron data exists, make output files...
     ; -------------------------------- Create electron MOMENTS data header...
     mhedr.nrec = nrecele & mhedr.thisdate = string(thisdate,Format='(I8)')
     openw,momLUN,getenv('IDLSAV')+'mhedr',/Get_LUN
     writeu,momLUN,mhedr & free_LUN,momLUN

     ; ---------------------------------- Create electron PITCH data header...
     phedr.nrec = nrecele & phedr.thisdate = string(thisdate,Format='(I8)')
     openw,pitLUN,getenv('IDLSAV')+'phedr',/Get_LUN
     writeu,pitLUN,phedr & free_LUN,pitLUN

     ; -------------------------- Create electron PITCH-AVERAGE data header...
     ahedr.nrec = nrecele & ahedr.thisdate = string(thisdate,Format='(I8)')
     openw,avgLUN,getenv('IDLSAV')+'ahedr',/Get_LUN
     writeu,avgLUN,ahedr & free_LUN,avgLUN

     ; --------------------------------- Create electron STRAHL data header...
     shedr.nrec = nrecele & shedr.thisdate = string(thisdate,Format='(I8)')
     openw,strLUN,getenv('IDLSAV')+'shedr',/Get_LUN
     writeu,strLUN,shedr & free_LUN,strLUN

     ; --------- Concatenate electron MOMENTS header and MOMENTS data files...
     hfl = getenv('IDLSAV')+'mhedr' ;                             Header file.
     dfl = getenv('IDLSAV')+'newmom.dat' ;                          Data file.
     pfl = getenv('MPNEW')+wst.lzdate+'_'+mom_version+'.mom' ;   Product file.
     spawn,'cat '+hfl+' '+dfl+' > '+pfl ;    CAT: fails if pfl already exists.
     print,'electron MOMENTS data-product file created: ',pfl       

     print,'lzSWEnewmode: electron MOMENTS finished for date ',wst.lzdate

     ; ------------- Concatenate electron PITCH header and PITCH data files...
     hfl = getenv('IDLSAV')+'phedr' ;                             Header file.
     dfl = getenv('IDLSAV')+'pitch.dat' ;                           Data file.
     pfl = getenv('MPNEW')+wst.lzdate+'_'+pit_version+'.pit' ;   Product file.
     spawn,'cat '+hfl+' '+dfl+' > '+pfl ;    CAT: fails if pfl already exists.
     print,'electron PITCH data-product file created: ',pfl       

     print,'lzSWEnewmode: elec. PITCH-ANGLES finished for date ',wst.lzdate

     ; --------- Concatenate electron AVERAGE header and AVERAGE data files...
     hfl = getenv('IDLSAV')+'ahedr' ;                             Header file.
     dfl = getenv('IDLSAV')+'ptchav.dat' ;                          Data file.
     pfl = getenv('MPNEW')+wst.lzdate+'_'+avg_version+'.pitavg' ;  Prod. file.
     spawn,'cat '+hfl+' '+dfl+' > '+pfl ;    CAT: fails if pfl already exists.
     print,'electron PITCH-AVERAGE data-product file created: ',pfl       

     print,'lzSWEnewmode: elec. PITCH-AVERAGES finished for date ',wst.lzdate

     ; ----------- Concatenate electron STRAHL header and STRAHL data files...
     hfl = getenv('IDLSAV')+'shedr' ;                             Header file.
     dfl = getenv('IDLSAV')+'strahl.dat' ;                          Data file.
     pfl = getenv('MPNEW')+wst.lzdate+'_'+str_version+'.str' ;   Product file.
     spawn,'cat '+hfl+' '+dfl+' > '+pfl ;    CAT: fails if pfl already exists.
     print,'electron STRAHL data-product file created: ',pfl       

     print,'lzSWEnewmode: electron STRAHL DATA finished for date ',wst.lzdate
  endif
endwhile & close,dateLUN ; ************* End of DATE loop. ******************

print,"Finished LZ electron-data processing for ALL selected dates."
answ = '' & print,"Hit return to continue, or any other key to stop."
read,answ & if (answ ne '') then stop ;     To examine final processing state.

end
