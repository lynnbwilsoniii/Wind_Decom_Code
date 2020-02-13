;=========================== SWE_fParaPerp_Read ==============================
; Locate and read daily file(s) of new-mode (mode7) pitch-angle-averaged data.
; Note: Although this was re-written and re-commented to reflect the NEW-mode
;        data-product conventions, it is ENTIRELY OLD-mode-data compatible.
; Next, extract (and correct if necessary) contents of "raw" input records
;  to form completed survey-data records.  This includes replacing data
;  resulting from "null" spectra (containing no valid information) with
;  IEEE NaN (Not-a-Number) values to prevent the later display of these data.
; Finally, set begin- and end-time indices and initial TJD time value.
PRO swe_fparaperp_read,TJD0_thisfile=tjd0 ;     (MPH, Last modified: 07/26/04).

common shared,d ;  Provides access to:                 the main data structure.
common wstuff,wst ;                the main widget-interface control structure.

print,'' & print,'swe_fparaperp_read:' ;                 Initial screen output.

;           Determine the file containing the currently selected day of data...
idatype = where(d.datype eq 'swe_fparaperp') & flnm = d.flnm[idatype]
   
openr,lun_pa,flnm,/Get_LUN ;  Open (first) daily file for reading (obtain LUN).
print,'' & print,'Reading swe_fparaperp file ',flnm,' ...' ; More scrn. output.

nv = 15 ; Mode7 must have this number of velocity- (energy-) steps per spectra.
np_avg = 4 ; Number of pitch-angle bins the hi-res pitch data is averaged over.
;  New-mode PTCHAVG data-prod.: HEADER and DATA-RECORD structure definitions...
ahedr = {nrec:0l,thisdate:string('',Format='(I8)'),$ ; ---------------- HEADER.
         date:string('',Format='(A8)'),scimode:0l,oknoatt:0l,ndets:0l,$
         nvsteps:0l,nsectors:0l,glnt:lonarr(3,64),ensteptbl:fltarr(64),$
         max_enstep:0l,thisrecn:0l,scimodechange:0l,dummy:0l}
indat = {mfrec:0l,mfspinbl:0l,ta:0.d0,pb5tim:lonarr(3),$ ; ------- DATA RECORD.
         velocity:fltarr(nv+1),fspa:fltarr(nv+1),pbin:fltarr(np_avg+1),$
         f:fltarr(np_avg,nv+1),b:fltarr(3),eleion:0l,misc:fltarr(9),$
         misc2:bytarr(8),gains:fltarr(6),rgse:fltarr(3)}

readu,lun_pa,ahedr & data = replicate(indat,ahedr.nrec) ; Read header and recs.
readu,lun_pa,data & free_LUN,lun_pa & print,' ...end reading file ',flnm

if (wst.number_days gt 1) then begin ;  The user has requested multiple days...
   thisflnm = flnm & thisdate = wst.indate ; Store name/date of file just read.

   for inxt=1,(wst.number_days-1) do begin ;  For each of the requested days...
      thispb5 = ymd_pb5(long(thisdate)) & thissec = pb5_sec(thispb5) ;    Time.
      pb5_next = sec_pb5(thissec+long(86400)) & nullstr = '' ;  Increment time.
      date_next = string(long(pb5_ymd(pb5_next)),Format='(I8)') ;   Incr. date.
      file_next = get_flnm('swe_fparaperp',getenv(d.pathenv[idatype]),/Lpr,$
                           nullstr,'.pitavg',date_next,Err=err) ;    Next flnm.

      if (err ne '') then goto,getoutloop ; Jump out of loop on FILENAME ERROR.

      openr,lun_next,file_next,/Get_LUN & readu,lun_next,ahedr ; Read HEADER...
      data_next = replicate(indat,ahedr.nrec) ; Read remainder of daily file...
      readu,lun_next,data_next & free_LUN,lun_next
      print,'end reading next file ',file_next ;   ...and update screen output.

      data = [temporary(data),data_next] & data_next = 0 ;   Update input data.
      thisflnm = file_next & thisdate = date_next ;   Update filename and date.
   endfor
endif

getoutloop: ;        Jump to here on FILENAME ERROR when reading multiple days.

nrec_tot = n_elements(data) ;      Find total number of spectra read, all days.
fparaperpdat = replicate({mfrec:0l,mfspinbl:0l,ta:0.d,tpb5:lonarr(3),$
                          velocity:fltarr(nv+1),energy:fltarr(nv+1),$
                          pbin:fltarr(np_avg+1),fspa:fltarr(nv+1),$
                          f:fltarr(np_avg,nv+1),b:fltarr(3),$
                          rgse:fltarr(3),eleion:0l,misc:fltarr(9),$
                          misc2:bytarr(8),gains:fltarr(6)},nrec_tot)

; *********************** Extracting "raw" input spectra into survey records...
fparaperpdat.mfrec = data.mfrec & fparaperpdat.mfspinbl = data.mfspinbl
fparaperpdat.ta = data.ta & fparaperpdat.tpb5 = data.pb5tim ;        Timestamp.
fparaperpdat.velocity = data.velocity ;     Velocity and (calculated) energy...
fparaperpdat.energy = 2.85e-16*data.velocity*data.velocity 
fparaperpdat.fspa = data.fspa & fparaperpdat.f = data.f ;       Averaged dists.
fparaperpdat.b = data.b & fparaperpdat.eleion = data.eleion ; Other misc. data:
fparaperpdat.misc = data.misc & fparaperpdat.misc2 = data.misc2
fparaperpdat.gains = data.gains & fparaperpdat.rgse = data.rgse

data = 0 ;    Erase "raw" input data after transfer to ptchav-survey structure.

;    Create lists for replacing "null" data with the useful fill value:
f_fill_val_lst = replicate(!Values.f_NaN,np_avg,nv+1) ; float "Not-a-Number"...
f_spa_fval_lst = replicate(!Values.f_NaN,nv+1)

; ******************************************* Completing FIRST survey record...
spect_is_null = fparaperpdat[0].misc2[2] ;  Extract FIRST null-spectrum marker.
if spect_is_null then begin ;         1st spectrum is "null" (no valid data)...
   fparaperpdat[0].ta = fparaperpdat[1].ta - $ ;                Repair times...
                       (fparaperpdat[2].ta-fparaperpdat[1].ta)
   fparaperpdat[0].tpb5[2] = fparaperpdat[1].tpb5[2] - $
                            (fparaperpdat[2].tpb5[2] - $
                             fparaperpdat[1].tpb5[2])
   fparaperpdat[0].tpb5[1] = fparaperpdat[1].tpb5[1] ;  Copy day-of-year value.
   fparaperpdat[0].f = f_fill_val_lst ;    Supply fill values for NULL dists...
   fparaperpdat[0].fspa = f_spa_fval_lst
endif

for i=1l,(nrec_tot-2l) do begin ; For each non-boundry spectrum, i, of input...
   ; ****************************************** Completing ith survey record...
   spect_is_null = fparaperpdat[i].misc2[2] ; Extract ith null-spectrum marker.
   if spect_is_null then begin ;                      ith spectrum is "null"...
      fparaperpdat[i].ta = $
         mean([fparaperpdat[i-1l].ta,fparaperpdat[i+1l].ta]) ;  Repair times...
      fparaperpdat[i].tpb5[2] = mean([fparaperpdat[i-1l].tpb5[2], $
                                      fparaperpdat[i+1l].tpb5[2]])
      fparaperpdat[i].tpb5[1] = fparaperpdat[i+1].tpb5[1] ;     Copy doy value.
      fparaperpdat[i].f = f_fill_val_lst ; Supply fill values for NULL dists...
      fparaperpdat[i].fspa = f_spa_fval_lst
   endif
endfor

; ******************************************** Completing LAST survey record...
;        Store energy and velocity (speed) lists for the last input spectrum...
lst = nrec_tot-1l
spect_is_null = fparaperpdat[lst].misc2[2] ; Extract LAST null-spectrum marker.
if spect_is_null then begin ;                        Last spectrum is "null"...
   fparaperpdat[lst].ta = fparaperpdat[nrec_tot-2l].ta + $ ;    Repair times...
                         (fparaperpdat[nrec_tot-2l].ta - $
                          fparaperpdat[nrec_tot-3l].ta)
   fparaperpdat[lst].tpb5[2] = fparaperpdat[nrec_tot-2l].tpb5[2] + $
                              (fparaperpdat[nrec_tot-2l].tpb5[2] - $
                               fparaperpdat[nrec_tot-3l].tpb5[2])
   fparaperpdat[lst].tpb5[1] = fparaperpdat[nrec_tot-2l].tpb5[1] ;    Copy val.
   fparaperpdat[lst].f = f_fill_val_lst ;  Supply fill values for NULL dists...
   fparaperpdat[lst].fspa = f_spa_fval_lst
endif

wtimeok = where((fparaperpdat.ta-fparaperpdat[0].ta) ge 0) ; Timestamp errors:
help,fparaperpdat.ta,wtimeok & fparaperpdat = temporary(fparaperpdat[wtimeok])

;  Define and print to screen final begin- and end-time indices (start time)...
d.ndx[0,idatype] = 0l & d.ndx[1,idatype] = n_elements(fparaperpdat)-1l
d.ndx_orig[*,idatype] = d.ndx[*,idatype] ;  Initialize 'd.ndx_orig[*,idatype]'.
print,'d.ndx[*,idatype]: ',d.ndx[*,idatype] ;                  Print to screen.

nstrt = 20 ;  Get start-time for this file (after correcting timestamp err.)...
if n_elements(fparaperpdat) lt nstrt then nstrt = n_elements(fparaperpdat)
w = where(((long(fix(fparaperpdat[0:nstrt].ta/86400.d))- $
            long(fix(fparaperpdat[0      ].ta/86400.d))) ne 0),w_cnt)
if (w_cnt ne 0l) then k0 = w[n_elements(w)-1l] else k0 = 0l
tjd0 = long(fix(fparaperpdat[k0].ta/86400.0d0)) ; Get start time for selection.

d = create_struct(d,'swe_fparaperpdat',fparaperpdat) ;  Add new data to struct.
print,'Leaving swe_fparaperp_read...' & print,'' ;         Final screen output.

end
