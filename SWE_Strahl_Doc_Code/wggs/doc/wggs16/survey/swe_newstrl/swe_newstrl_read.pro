;============================ SWE_NewSTRL_Read ===============================
; Locate and read daily file(s) of new-mode (mode7) strahl survey data.
; Next, extract (and correct if necessary) contents of "raw" input records
;  to form completed survey-data records.  This includes replacing data
;  resulting from "null" spectra (containing no valid information) with
;  IEEE NaN (Not-a-Number) values to prevent the later display of these data.
; Finally, set begin- and end-time indices and initial TJD time value; then
;  replace certain default plot-list-element names for this data type (energy-
;  step numbers: 1, ..., 15) with energy values, in eV, read from the data.
PRO swe_newstrl_read,TJD0_thisfile=tjd0 ;       (MPH, Last modified: 08/18/03).

common shared,d ;  Provides access to:                 the main data structure.
common wstuff,wst ;                the main widget-interface control structure.

print,'' & print,'swe_newstrl_read:' ;                   Initial screen output.

;           Determine the file containing the currently selected day of data...
idatype = where(d.datype eq 'swe_newstrl') & flnm = d.flnm[idatype]

openr,lun_ns,flnm,/Get_LUN ;  Open (first) daily file for reading (obtain LUN).
print,'' & print,'Reading swe_newstrl file ',flnm,' ...' ;  More screen output.

nv = 15 ; Mode7 must have this number of velocity- (energy-) steps per spectra.
; New-mode STRAHL data-product: HEADER and DATA-RECORD structure definitions...
shedr = {nrec:0l,thisdate:string('',Format='(I8)'),thisrecn:0l,$
         date:string('',Format='(A8)'),scimode:0l,scimodechange:0l,$
         oknoatt:0l,ndets:0l,nvsteps:0l,nsectors:0l,max_enstep:0l,$
         ensteps:bytarr(nv),misc:intarr(4)} ; ---- total for header:  75 bytes.
sdata = {lzrec:0,spectm:0,tjdsec_spectm:0.0d0,pb5tim:lonarr(3),is_null:0B,$
         mx_B_cnts:intarr(nv),mx_aB_cts:intarr(nv),pa_mBcnts:intarr(nv),$
         pa_maBcts:intarr(nv),B_baw_hmx:intarr(nv),aB_bw_hmx:intarr(nv),$
         b:fltarr(3),rgse:dblarr(3)} ; ----------- total per record: 241 bytes.

readu,lun_ns,shedr ;   Read HEADER info. and print to screen with formatting...
;header_fstring = '("   nrec: ",I4.4,", thisdate: ",I8.8,'+$ ;      Format str.
;                 '", thisrecn: ",I4.4,/,"   date: ",A8,/,'+$
;                 '"   scimode: ",I2.2,/,"   misc. header info.:",'+$
;                          '5I3.2,I5.4,/,"                      ",'+$
;                              '15I3.2,/,"                      ",4I3.2)'
;print,shedr,Format=header_fstring ;             Output HEADER info. to screen.
nv_error_string = "Mismatch between expected number of"+$ ;   Define error
                  " velocity-steps-per-spectrum and"+$    ;           output...
                  " number indicated in current header."
;   Confirm that assumed # of vel.-steps-per-spect. matches read header data...
if (shedr.nvsteps ne nv) then begin ;       Mismatch in shedr.nvsteps vs. nv...
   beep & print,'' & print,"SWE_NewSTRL_Read ERROR!" & print,nv_error_string
   print,"expected, read: ",nv,fix(shedr.nvsteps) & stop ; Stop with err. msgs.
endif

data = replicate(sdata,shedr.nrec) ;  Read remainder of daily file into recs...
readu,lun_ns,data & free_LUN,lun_ns & print,' ...end reading file ',flnm

if (wst.number_days gt 1) then begin ;  The user has requested multiple days...
   thisflnm = flnm & thisdate = wst.indate ; Store name/date of file just read.

   for inxt=1,(wst.number_days-1) do begin ;  For each of the requested days...
      thispb5 = ymd_pb5(long(thisdate)) & thissec = pb5_sec(thispb5) ;    Time.
      pb5_next = sec_pb5(thissec+long(86400)) & nullstr = '' ;  Increment time.
      date_next = string(long(pb5_ymd(pb5_next)),Format='(I8)') ;   Incr. date.
      file_next = get_flnm('swe_newstrl',getenv(d.pathenv[idatype]),/Lpr,$
                           nullstr,'.str',date_next,Err=err) ;   Next filename.

      if (err ne '') then goto,getoutloop ; Jump out of loop on FILENAME ERROR.

      openr,lun_next,file_next,/Get_LUN & readu,lun_next,shedr ; Read HEADER...
      ;print,'' & print,'Reading next file ',file_next,' ...' ;   Prog. output.
      ;print,shedr,Format=header_fstring ;       Output HEADER info. to screen.
      ;  Confirm assumed # of vel.-steps-per-spect. matches read header data...
      if (shedr.nvsteps ne nv) then begin ; Mismatch in shedr.nvsteps vs. nv...
         beep & print,'' & print,"SWE_NewSTRL_Read ERROR!"
         print,nv_error_string & print,"expected, read: ",nv,fix(shedr.nvsteps)
         stop ;         Stop here with error messages in the event of an error.
      endif

      data_next = replicate(sdata,shedr.nrec) ; Read remainder of daily file...
      readu,lun_next,data_next & free_LUN,lun_next
      print,'end reading next file ',file_next ;   ...and update screen output.

      data = [temporary(data),data_next] & data_next = 0 ;   Update input data.
      thisflnm = file_next & thisdate = date_next ;   Update filename and date.
   endfor
endif

getoutloop: ;        Jump to here on FILENAME ERROR when reading multiple days.

nrec_tot = n_elements(data) ;      Find total number of spectra read, all days.
ns_dat = replicate({LZrec:0l,spectm:0l,ta:0.d,tpb5:lonarr(3),$ ; Survey record.
                    is_null:0B,energy:fltarr(nv),velocity:fltarr(nv),$
                    mx_B_cnts:fltarr(nv),mx_aB_cts:fltarr(nv),$
                    pa_mBcnts:fltarr(nv),pa_maBcts:fltarr(nv),$
                    B_baw_hmx:fltarr(nv),aB_bw_hmx:fltarr(nv),$
                    B:fltarr(3),Bmag:0.0,Bphi:0.0,Bth:0.0},nrec_tot)

; *********************** Extracting "raw" input spectra into survey records...
ns_dat.LZrec = data.lzrec & ns_dat.spectm = data.spectm ;  Record-and-spectrum.
ns_dat.ta = data.tjdsec_spectm & ns_dat.tpb5 = data.pb5tim ;         Timestamp.
ns_dat.is_null = data.is_null ; This list of flags tells if a spectrum is null.
ns_dat.mx_B_cnts = float(data.mx_B_cnts) ;    Maximum count value on  "B-side".
ns_dat.mx_aB_cts = float(data.mx_aB_cts) ;    Maximum count value on "aB-side".
ns_dat.pa_mBcnts = float(data.pa_mBcnts) ;    Pitch-angle of  B-side beam core.
ns_dat.pa_maBcts = float(data.pa_maBcts) ;    Pitch-angle of aB-side beam core.
ns_dat.B_baw_hmx = float(data.B_baw_hmx) ;   B-side Beam angular-width, in deg.
ns_dat.aB_bw_hmx = float(data.aB_bw_hmx) ;  aB-side Beam angular-width, in deg.
ns_dat.B = float(data.B) ; Extract B-fld.GSE info. (BAD during null spectra)...
ns_dat.Bmag = float(sqrt(total((double(data.B)^2),1))) ; |B|, nT -(GSE below)V.
ns_dat.Bphi = float(atan(double(ns_dat.B[1]),double(ns_dat.B[0])))*!Radeg ; az.
w_Bphi_lt_z = where((ns_dat.Bphi lt 0.0),w_Bphi_lt_z_count) ;  For B-az < 0 ...
if (w_Bphi_lt_z_count ne 0l) then $ ;   Transform range: [-180,180] -> [0,360].
   ns_dat[w_Bphi_lt_z].Bphi = ns_dat[w_Bphi_lt_z].Bphi+360.0 ; Full circle rot.
ns_dat.Bth = float(asin(double(ns_dat.B[2])/double(ns_dat.Bmag)))*!Radeg ;  el.

data = 0 ;    Erase "raw" input data after transfer to strahl-survey structure.

enlst = float(volt_en_strl(shedr.ensteps,/En )) ; List of ENERGY values, in eV.
velst = float(volt_en_strl(shedr.ensteps,/Vel)) ; List of VEL. values, in cm/s.

;    Create lists for replacing "null" data with the useful fill value:
fill_val_lst = replicate(!Values.f_NaN,nv) ;     IEEE (float) "Not-a-Number"...
B_fval_lst   = replicate(!Values.f_NaN, 3) & fill_val = !Values.f_NaN

;format_string = '("LZrec: ",I4.4,", spectm: ",I1.1,/,'+$ ; For survey records.
;                 '"ta: ",E,", tpb5: ",I4.4,I4.3,I9.8,/,'+$
;                 '"----- is_null: ",I1.1," ------",/,'+$
;                 '"energy   : ",15F9.2,/,"velocity : ",15E9.2,/,'+$
;                 '"mx_B_cnts: ",15F9.2,/,"mx_aB_cts: ",15F9.2,/,'+$
;                 '"pa_mBcnts: ",15F9.2,/,"pa_maBcts: ",15F9.2,/,'+$
;                 '"B_baw_hmx: ",15F9.2,/,"aB_bw_hmx: ",15F9.2,/,'+$
;                 '"B:",3F7.2,", |B|:" ,F6.2,'+$
;                            '", Bphi:",F9.3,", Bth:",F9.3)'

; ******************************************* Completing FIRST survey record...
ns_dat[0].energy = enlst & ns_dat[0].velocity = velst ; Store lists, 1st spect.
if ns_dat[0].is_null then begin ;     1st spectrum is "null" (no valid data)...
   ns_dat[0].ta = ns_dat[1].ta-(ns_dat[2].ta-ns_dat[1].ta) ;    Repair times...
   ns_dat[0].tpb5[2] = ns_dat[1].tpb5[2]-(ns_dat[2].tpb5[2]-ns_dat[1].tpb5[2])
   ns_dat[0].tpb5[1] = ns_dat[1].tpb5[1] ;              Copy day-of-year value.
   ns_dat[0].mx_B_cnts = fill_val_lst & ns_dat[0].mx_aB_cts = fill_val_lst 
   ns_dat[0].pa_mBcnts = fill_val_lst & ns_dat[0].pa_maBcts = fill_val_lst
   ns_dat[0].B_baw_hmx = fill_val_lst & ns_dat[0].aB_bw_hmx = fill_val_lst
   ns_dat[0].B = B_fval_lst & ns_dat[0].Bmag = fill_val
   ns_dat[0].Bphi = fill_val & ns_dat[0].Bth = fill_val
endif
;print,'' & print,"FIRST record read..." ;     Survey-record-processing HEADER.
;print,ns_dat[0],Format=format_string & stop ;      Output FIRST survey record.

for i=1l,(nrec_tot-2l) do begin ; For each non-boundry spectrum, i, of input...
   ; ****************************************** Completing ith survey record...
   ns_dat[i].energy = enlst & ns_dat[i].velocity = velst ; Store en./vel. lsts.
   if ns_dat[i].is_null then begin ;                  ith spectrum is "null"...
      ns_dat[i].ta = mean([ns_dat[i-1l].ta,ns_dat[i+1l].ta]) ;  Repair times...
      ns_dat[i].tpb5[2] = mean([ns_dat[i-1l].tpb5[2],ns_dat[i+1l].tpb5[2]])
      ns_dat[i].tpb5[1] = ns_dat[i+1].tpb5[1] ;         Copy day-of-year value.
      ns_dat[i].mx_B_cnts = fill_val_lst & ns_dat[i].mx_aB_cts = fill_val_lst
      ns_dat[i].pa_mBcnts = fill_val_lst & ns_dat[i].pa_maBcts = fill_val_lst
      ns_dat[i].B_baw_hmx = fill_val_lst & ns_dat[i].aB_bw_hmx = fill_val_lst
      ns_dat[i].B = B_fval_lst & ns_dat[i].Bmag = fill_val
      ns_dat[i].Bphi = fill_val & ns_dat[i].Bth = fill_val
   endif
   ;print,'' & print,ns_dat[i],Format=format_string & stop ; Output ith record.
endfor

; ******************************************** Completing LAST survey record...
;        Store energy and velocity (speed) lists for the last input spectrum...
lst = nrec_tot-1l & ns_dat[lst].energy = enlst & ns_dat[lst].velocity = velst
if ns_dat[lst].is_null then begin ;                  Last spectrum is "null"...
   ns_dat[lst].ta = ns_dat[nrec_tot-2l].ta+$ ;              Repair time info...
                               (ns_dat[nrec_tot-2l].ta-ns_dat[nrec_tot-3l].ta)
   ns_dat[lst].tpb5[2] = ns_dat[nrec_tot-2l].tpb5[2]+$
                                                (ns_dat[nrec_tot-2l].tpb5[2]-$
                                                 ns_dat[nrec_tot-3l].tpb5[2])
   ns_dat[lst].tpb5[1] = ns_dat[nrec_tot-2l].tpb5[1] ;  Copy day-of-year value.
   ns_dat[lst].mx_B_cnts = fill_val_lst & ns_dat[lst].mx_aB_cts = fill_val_lst
   ns_dat[lst].pa_mBcnts = fill_val_lst & ns_dat[lst].pa_maBcts = fill_val_lst
   ns_dat[lst].B_baw_hmx = fill_val_lst & ns_dat[lst].aB_bw_hmx = fill_val_lst
   ns_dat[lst].B = B_fval_lst & ns_dat[lst].Bmag = fill_val
   ns_dat[lst].Bphi = fill_val & ns_dat[lst].Bth = fill_val
endif
;print,'' & print,ns_dat[lst],Format=format_string ; Output LAST survey record.
;print,"LAST record read..." & print,'' ;     Survey-record-processing TRAILER.

;  Define and print to screen final begin- and end-time indices (start time)...
d.ndx[0,idatype] = 0l & d.ndx[1,idatype] = lst ;                Define indices.
d.ndx_orig[*,idatype] = d.ndx[*,idatype] ;  Initialize 'd.ndx_orig[*,idatype]'.
print,'d.ndx[*,idatype]: ',d.ndx[*,idatype] ;                  Print to screen.

; Replace the default plot-list-element names for this data type (energy-step
;     numbers: 1, ..., 15) with the energy values, in eV, read from the data...
wh_ns_list = where((d.pnlist.dtp eq d.datype[idatype]),wh_ns_list_count)
if (wh_ns_list_count ge nv) then $ ;    If list-elements found for this type...
   d.pnlist.list[wh_ns_list[0:(nv-1)]] = $ ;  Replace vals. with read energies.
      string(ns_dat[d.ndx[0,idatype]].energy,Format='(F8.1)')+' eV'
print,d.datype[idatype]+' panel list: ',d.pnlist.list[wh_ns_list] ;    Updated.

tjd0 = long(fix(ns_dat[0].ta/86400.d)) ;     Get start time for this selection.

d = create_struct(d,'swe_newstrldat',ns_dat) ;  Add survey data to main struct.

print,'Leaving swe_newstrl_read...' & print,'' ;           Final screen output.

end
