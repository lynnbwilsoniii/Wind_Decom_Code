; Modified version of mode6.pro, used for the purpose of reading mode7 strahl
;  level-zero data--in the absence of VEIS data.     Last modified: (08/21/02).

;  ================================= Mode7 ==================================
PRO mode7,Lpr=lpr,$ ;     Set the 'lpr' flag to generate verbose screen output.
          Err=err   ;       The 'err' output keyword returns error information.

; These common-blocks allow data sharing among the reading/plotting routines...
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m7stuff,hkm7,sdatc7 & common log_delog,comp_tbl,dcomp_tbl
common phasemod1,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl

; ------------------------------ Initialization -----------------------------
err='' ;                                         Initially there are no errors.
;      Note: The 'lpr' keyword is never used; it is included for compatability.
;               To avoid the repeated checks that would be necessary with this
;                     flag, simply "comment out" any undesired "Screen output".
print,'' & print,'^%^%^%^%^%^%^%^%^%^%^%^%^'+$ ;    Display on-screen header...
                 ' Begin processing mode7 LZ data record '+'^%^%^%^%^%^%^%^%^'

phasem1 ;                      Get sun phase angles of detectors, unit vectors.
thestrl = (thestrl[[0,2,4,6,8,10]]+thestrl[[1,3,5,7,9,11]])/2.0 ;   Avg. pairs.

;            Determine science-mode (should be 7), exit with error otherwise...
if (lz.mf[hkm7[4].offs] eq 7) then scimode_ihk = 7 else begin
   ; Note: There is not actually a "mode7"; only a "mode7 indicator" byte in
   ;                  the housekeeping data, which takes on the values 0 and 7.
   err = 'mode7: wrong mode' & return ;       Error handler for incorrect mode.
endelse

print,'Science mode for this record (should be 7): ',scimode_ihk ; Scrn output.

time_utc,lz.mf[hkm7[1].offs:hkm7[1].offs+6],$ ;      Get time tagged spincnt...
         tjd,sec,hour,min,isec,ms,hms,spincnt ; OUTPUT parameters save results.
if ((tjd eq 0) and (lz.yr ne 1995) and (lz.dy ne 283)) then begin
   err='mode1: time error in time_utc.pro' & return & endif  

print,'MJF record number (widget disp.): ',long(recn) ;        Counting from 1.
print,'LZ record number (from lz.recn):  ',long(lz.recn) ;   proc_fw: timsellz.
print,'Time-tagged spin for this record: ',long(spincnt) ; Used to find all
print,'hh:mm:ss.msec time at time-tagged spin: ',hms ;    spectrum-begin times.

sp.tjd = tjd & sp.sec = sec & sp.spincnt = spincnt ;    Store spin information.
sp.mfrecn = lz.recn & sp.mfyr = lz.yr & sp.mfdy = lz.dy & sp.mfms = lz.ms  

;                                                Set data reading parameters...
iclicks_sunpulse = 4096.0 & iclicks_bci = 42.0 ;  Num. of clicks in a spin,BCI.
phi_bci = iclicks_bci*(360.d0/iclicks_sunpulse) & phistep = 2*phi_bci ; Degrees
n_strspects = 7 & phidly_sun = 42.54 & phistrl_offs = 154.5 ;     Fixed angles.
n_strspins = 21 & n_strdets = 6 & n_strsects = 8 & n_strensteps = 15
; ----------------------------------------------------------------------------

; ------------------------------ Reading MJF ---------------------------------
; Reading major-frame of strahl data into the multidimensional 'strl' array;
;  and finding spin phase angle of strahl data samples relative to direction
;  of the sun--with positive "phi" counterclockwise about spin axis (z-axis)...
; (The mode1 "phistrl" and "vunitstrl" from common phasemod1 are overridden.)

;                       Init. ENTIRE major-frame, and corresponding phi values.
   strl = fltarr(n_strdets,n_strsects,n_strensteps,n_strspects)
phistrl = fltarr(n_strdets,n_strsects,n_strensteps,n_strspects)
phistrl_bci0 = phidly_sun+phi_bci+phistrl_offs ;  Phi angle for strahl @ BCI=0.

;  Note: We assume here that phi=phistrl_bci0 DEGREES at the beginning of each
;        spin, and also that a "phi step" corresponds to an energy step--
;        except that there is one "extra" (settling) step each sector, and the
;        count acclumulation begins in the MIDDLE of each step.

; For each spectrum, determine the azimuth (phi) angles for every sector
;             and energy-step--then read ALL strahl counts for that spectrum...
for ispect=0,(n_strspects-1) do begin ;             For each strahl spectrum...
   for ienstep=0,(n_strensteps-1) do begin ;     For each strahl energy-step...
      for isect=0,(n_strsects-1) do begin ;           For each strahl sector...
         phistrl[*,isect,ienstep,ispect] = $ ;  Find "phi" (pstp="phi step")...
          (isect*6.0*phistep)$ ;              ((# sect)*(pstp/sect)*(deg/pstp))
          +((ienstep mod 5)*phistep)$ ;            +((# "odd" pstp)*(deg/pstp))
          +phistrl_bci0+(phistep/2) ;   Acculm. ONLY in each 2nd half-phi-step.
      endfor
   endfor

   ;                                        As a last step for each spectrum...
   strl[*,*,*,ispect] = lz.mf[sdatc7[ispect].ind] ;   Read ALL strahl counts...
endfor ; from lz.mf, using the index list in the 'ind' field of sdatc7[ispect].

;                      Create and display azimuth and elevation angle tables...
print,'' & print,'Azimuth angles (in deg.) for each spin of each spectrum:'
print,' Steps @ Sector:  0      1      2      3      4      5'+$
                         '      6      7' ;     Skip line, displ. table header.
for ienstep=0,((n_strensteps/3)-1) do $ ;  Fill in table values (az. angles)...
   print,((indgen(3)*5)+ienstep),(reform(phistrl[0,*,ienstep,0]) mod 360.0),$
         Format='(3(I2.2,:,","),"      ",8f7.2)' ;   Format for each table row.
print,'' & print,'Elevation angles (in deg.) for each det.:' & print,thestrl

vunitstrl = dblarr(n_strdets,n_strsects,n_strensteps,3) ;   Init. unit-vectors.
snth = sin(thestrl*!dtor) & csth = cos(thestrl*!dtor) ;    Depends ONLY on det.
snph = sin(phistrl*!dtor) & csph = cos(phistrl*!dtor) ;  Dep. on det,sect,step.

;   For each sector, energy-step (phi-step) and detector; find a unit-vector...
; Note: These unit-vectors have the INWARD orientation of an INCOMING particle.
for isect=0,(n_strsects-1) do begin ;                 For each strahl sector...
   for ienstep=0,(n_strensteps-1) do begin ;     For each strahl energy-step...
      for idet=0,(n_strdets-1) do begin ;           For each strahl detector...
          vunitstrl[idet,isect,ienstep,0] = $ ;   INCOMING unit-vector X-comp.,
                                       -snth[idet]*csph[idet,isect,ienstep,0]
          vunitstrl[idet,isect,ienstep,1] = $ ;   INCOMING unit-vector Y-comp.,
                                       -snth[idet]*snph[idet,isect,ienstep,0]
          vunitstrl[idet,isect,ienstep,2] = -csth[idet] ;      and Z-component.
      endfor
   endfor ;     Note: Unit vectors are not used except when forming vel. dists.
endfor

sp.lst_scimod = scimode_ihk ;                   Set science-mode for last spin.
; ----------------------------------------------------------------------------

;------------ Making strahl data structure assignments for this MJF ----------
vsmjf = {descr:'', $ ;         Define data structure for strahl data samples...
         tjd:0l,sec:0d,hour:0l,min:0l,isec:0l,ms:0l, $
         lzrecn:0l,mjfcnt:0l,scimode:0,spinp:0.d, $
         n_strspins:long(n_strspins),n_strdets:long(n_strdets), $
         n_strsects:long(n_strsects),n_strensteps:long(n_strensteps), $
         n_strspects:long(n_strspects),numstrlstps:0, $
         deltasp:double(iclicks_bci)/double(iclicks_sunpulse), $
         deadtim_ele:0.d,delt_ele:0.d,geomf:fltarr(n_strdets), $
         strl_en:fltarr(n_strensteps),strl_vel:fltarr(n_strensteps), $
         strlstep:bytarr(n_strensteps),is_null:bytarr(n_strspects), $
         strl:bytarr(n_strdets,n_strsects,n_strensteps,n_strspects), $
         fstrl:fltarr(n_strdets,n_strsects,n_strensteps,n_strspects), $
         phistrl:dblarr(n_strdets,n_strsects,n_strensteps,n_strspects), $
         thestrl:thestrl,pb5tim_vsbl:lonarr(3,n_strspects), $
         vunitstrl:dblarr(n_strdets,n_strsects,n_strensteps,3), $
         sunsec_vsbl:dblarr(n_strspects),suntim_vsbl:dblarr(n_strspects), $
         strl_cts_factor:fltarr(n_strdets,n_strensteps)}

;             Make Strahl velocity spectrum (this MJF) structure assignments...
vsmjf.descr = 'mode 7 strahl data samples' ; Description of structure contents.
vsmjf.tjd = tjd ;             Trucated julian day of time-tagged spin this mjf.
vsmjf.sec = sec ;                  Seconds of day of time-tagged spin this mjf.
vsmjf.hour = hour ;                   Hour of day of time-tagged spin this mjf.
vsmjf.min = min ;                  Minute of hour of time-tagged spin this mjf.
vsmjf.isec = isec ;                 Second of min of time-tagged spin this mjf.
vsmjf.ms = ms ;             Millisecond of second of time-tagged spin this mjf.
vsmjf.lzrecn = lz.recn ;                             LZ record number this mjf.
vsmjf.mjfcnt = lz.mf[ihk[1].offs] ;                 MJF counter value this mjf.
vsmjf.scimode = scimode_ihk ;                            Science mode this mjf.
vsmjf.spinp = sp.spinp ;               Calculated average spin period this mjf.
vsmjf.numstrlstps = lz.mf[hkm7[6].offs] ; Number of strl energy steps this mjf.
vsmjf.phistrl = phistrl ;      List of azimuth angles for each sample this mjf.
vsmjf.vunitstrl = vunitstrl ;    List of unit vectors for each sample this mjf.
; ----------------------------------------------------------------------------

; -------------- Determine begin time for each spectrum in this MJF ----------
; sdatc7[ispect].spect_spncnt_offs gives the major-frame-offset to find the
;        spincount-begin value for the ith spectrum of the current major frame.
spincnt_vsbl = intarr(n_strspects) ;  We get all of these spincount-begin vals.
spincnt_vsbl[0:n_strspects-1] = fix(lz.mf[sdatc7.spect_spncnt_offs])

print,'' ;           Skip line and display spincount-begin values read above...
print,'Spincount-begin values for each spectrum:' & print,spincnt_vsbl

; Then we look at the byte following the spincount-begin-bytes for each
;       spectrum in the major frame, outputting them in hex. and dec. format...
print,'' & print,'Spectrum header flags for this major frame (hex./dec.):'
print,lz.mf[sdatc7.spect_spncnt_offs+1],Format='(7Z4.2)'
print,lz.mf[sdatc7.spect_spncnt_offs+1],Format='(7I4.3)' & print,''
; For any spectrum where the corresponding byte is 'FF' (or 255 in decimal),
;  the spectrum is null, so the 'is_null' flag will be set for that spectrum...
is_null = (lz.mf[sdatc7.spect_spncnt_offs+1] eq 255) & print,'null spectra: '
print,where(is_null) & print,"'-1' means all spectra are non-null this mjf."
vsmjf.is_null = is_null & print,'' ;  Enter flags into structure and skip line.

ispin_diff = intarr(n_strspects) ;  For each spectrum find the number of spins
ispin_diff[0:n_strspects-1] = spincnt_vsbl-spincnt ; "after" the tagged spin...
; Note: A NEGATIVE difference is legal and indicates a spectrum BEFORE the tag.

; Perform a correction for the class of special cases where the spincount
;  "rolls over" from 255 (FF) to 0 (00) (unreasonably large spincount diff.)...
; Note the criterion of 128 was provided by J. Needell as a rollover indicator.
w = where((ispin_diff lt -128),nw) ;  Rollover between TAGGED SPIN and THESE...
                           if (nw gt 0) then ispin_diff[w] = ispin_diff[w]+256
w = where((ispin_diff gt  128),nw) ;  Rollover between THESE and TAGGED SPIN...
                           if (nw gt 0) then ispin_diff[w] = ispin_diff[w]-256

; Starting from the 'sec' tag, and using the corrected spincount differences
;  with the calculated spin period, find the 'sec' tags of all seven spectra...
sunsec_vsbl = dblarr(n_strspects) & sunsec_vsbl = sec+(ispin_diff*sp.spinp)
vsmjf.sunsec_vsbl = sunsec_vsbl & suntim_vsbl = dblarr(n_strspects)
suntim_vsbl = (tjd*86400.d0)+vsmjf.sunsec_vsbl ;     Seconds since epoch began.
vsmjf.suntim_vsbl = suntim_vsbl ;      Store time info. in 'vsmjf' structure...

print,'Truncated Julian Day of this record: ',tjd ;        Display time info...
print,"Second-of-day at each spectrum's beginning:" & print,vsmjf.sunsec_vsbl
print,"Seconds since epoch began at each spectrum's beginning:"
print,vsmjf.suntim_vsbl & print,'Calculated average spin period: ',sp.spinp
print,'Estimated spectrum-begin yyyymmdd hh:mm:ss times for each spectrum:'
   
for ispect=0,(n_strspects-1) do begin ;          Convert tjd,sec to pb5 time...
   vsmjf.pb5tim_vsbl[*,ispect] = sec_pb5(vsmjf.suntim_vsbl[ispect],Err=err)
   print,'spectrum,time: ',ispect,pb5_ymdhms(vsmjf.pb5tim_vsbl[*,ispect])+$
         (vsmjf.is_null[ispect]?' *NULL SPECTRUM (invalid time)*':'')
   if err ne '' then return ;                      Output spectrum-begin times.
endfor
; ----------------------------------------------------------------------------

; ------------- From counts get phase-density (instrument factors) -----------
vsmjf.deadtim_ele = 0.125d-6 ;                     Accumulation time (seconds).
vsmjf.delt_ele = (sp.spinp*vsmjf.deltasp)-vsmjf.deadtim_ele 

;                           Geometrical factor (dv/v * eff area * solid angle).
;vsmjf.geomf = fltarr(n_strdets)+2.9e-5 ;        J. Keller 
 vsmjf.geomf = fltarr(n_strdets)+5.8e-5 ;        M. Holland: doubled old value.

;       Strahl data (log compressed) (-1 is due to summing over adjacent dets).
vsmjf.strl[*,*,*,*] = strl-1 ;                    Calculated in loop way above.

strahl_hvtbl = lz.mf[hkm7[9].loc[0:n_strensteps-1]] ;      Strahl energy-steps.
vsmjf.strlstep = strahl_hvtbl ;    Step numbers used to calc. counts->f factor.
vsmjf.strl_en = volt_en_strl(strahl_hvtbl,/En) ; Energies (in eV) at each step.
vsmjf.strl_vel = volt_en_strl(strahl_hvtbl,/Vel) ; Velocities (in cm/s) @ each.
print,'' & print,'List of energy/velocity steps:' & print,strahl_hvtbl
print,'Energies (in eV) at each step:' & print,vsmjf.strl_en
print,'Velocities (in cm/s), each step:' & print,vsmjf.strl_vel

cts_f_strl,vsmjf.strlstep,strl_cts_factor,cf_strl,/Mode7 ; Mode 7 special case.
vsmjf.strl_cts_factor = strl_cts_factor ;              Strahl counts->f factor.

;      Convert strahl counts to f's using calculated strahl counts->f factor...
for ispect=0,(n_strspects-1) do begin ;             For each strahl spectrum...
   for ienstep=0,(n_strensteps-1) do begin ;     For each strahl energy-step...
      for isect=0,(n_strsects-1) do begin ;           For each strahl sector...
         for idet=0,(n_strdets-1) do begin ;        For each strahl detector...
            vsmjf.fstrl[idet,isect,ienstep,ispect] = $ ;  Decompress and scale
                              vsmjf.strl_cts_factor[idet,ienstep]* $ ; LZ data.
                              dcomp_tbl[vsmjf.strl[idet,isect,ienstep,ispect]]
         endfor                     
      endfor
   endfor
endfor
; ----------------------------------------------------------------------------

print,'^%^%^%^%^%^%^%^%^%^%^%^%^ Finished processing mode7 LZ data record '+$
      '^%^%^%^%^%^%^%^%^' & print,'' ;             Display on-screen trailer...

end
