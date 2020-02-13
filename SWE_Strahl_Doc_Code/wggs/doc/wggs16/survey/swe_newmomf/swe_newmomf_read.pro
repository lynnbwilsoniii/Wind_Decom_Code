;============================ SWE_NewMOMf_Read ===============================
; Locate and read daily file(s) of new-mode (mode7) moments survey data
;  (derived from Kappa-model fits to measured velocity-distributions).
; Next, extract (and correct if necessary) contents of "raw" input records
;  to form completed survey-data records.
; Finally, set begin- and end-time indices and initial TJD time value.
PRO swe_newmomf_read,TJD0_thisfile=tjd0 ;       (MPH, Last modified: 08/15/03).

common shared,d ;  Provides access to:                 the main data structure.
common wstuff,wst ;                the main widget-interface control structure.

print,'' & print,'swe_newmomf_read:' ;                   Initial screen output.

;           Determine the file containing the currently selected day of data...
idatype = where(d.datype eq 'swe_newmomf') & flnm = d.flnm[idatype]

openr,lun_nm,flnm,/Get_LUN ;  Open (first) daily file for reading (obtain LUN).
print,'' & print,'Reading swe_newmomf file ',flnm,' ...' ;  More screen output.

nv = 15 ; Mode7 must have this number of velocity- (energy-) steps per spectra.
;  New-mode MOMENTS data-prod.: HEADER and DATA-RECORD structure definitions...
mhedr = {nrec:0l,thisdate:string('',Format='(I8)'),thisrecn:0l,$
         date:string('',Format='(A8)'),scimode:0l,scimodechange:0l,$
         oknoatt:0l,ndets:0l,nvsteps:0l,nsectors:0l,max_enstep:0l,$
         ensteps:bytarr(nv),cnvrt_to_f:fltarr(nv+1),misc:intarr(3),$
         comp_fact:0.0d0} ; ---------------------- total for header: 145 bytes.
mdata = {lzrec:0,spectm:0,tjdsec_spectm:0.0d0,pb5tim:lonarr(3),is_null:0B,$
         neout:0.0,enthm:0.0,teout:0.0,bounds:fltarr(2,3),fitres:fltarr(6),$
         misc:intarr(2),rgse:dblarr(3)} ; -------- total per record: 113 bytes.

readu,lun_nm,mhedr ;   Read HEADER info. and print to screen with formatting...
;header_fstring = '("   nrec: ",I4.4,", thisdate: ",I8.8,'+$ ;      Format str.
;                 '", thisrecn: ",I4.4,/,"   date: ",A8,/,'+$
;                 '"   scimode: ",I2.2,/,"   misc. header info.:",'+$
;                          '5I3.2,I5.4,/,"                      ",'+$
;                              '15I3.2,/,"                      ",'+$
;                               '5E9.2,/,"                      ",'+$
;                               '5E9.2,/,"                      ",'+$
;                               '6E9.2,/,"                      ",'+$
;                               '3I4.3,F5.2)'
;print,mhedr,Format=header_fstring ;             Output HEADER info. to screen.
nv_error_string = "Mismatch between expected number of"+$ ;   Define error
                  " velocity-steps-per-spectrum and"+$    ;           output...
                  " number indicated in current header."
;   Confirm that assumed # of vel.-steps-per-spect. matches read header data...
if (mhedr.nvsteps ne nv) then begin ;       Mismatch in mhedr.nvsteps vs. nv...
   beep & print,'' & print,"SWE_NewSTRL_Read ERROR!" & print,nv_error_string
   print,"expected, read: ",nv,fix(mhedr.nvsteps) & stop ; Stop with err. msgs.
endif

data = replicate(mdata,mhedr.nrec) ;  Read remainder of daily file into recs...
readu,lun_nm,data & free_LUN,lun_nm & print,' ...end reading file ',flnm

if (wst.number_days gt 1) then begin ;  The user has requested multiple days...
   thisflnm = flnm & thisdate = wst.indate ; Store name/date of file just read.

   for inxt=1,(wst.number_days-1) do begin ;  For each of the requested days...
      thispb5 = ymd_pb5(long(thisdate)) & thissec = pb5_sec(thispb5) ;    Time.
      pb5_next = sec_pb5(thissec+long(86400)) & nullstr = '' ;  Increment time.
      date_next = string(long(pb5_ymd(pb5_next)),Format='(I8)') ;   Incr. date.
      file_next = get_flnm('swe_newmomf',getenv(d.pathenv[idatype]),/Lpr,$
                           nullstr,'.mom',date_next,Err=err) ;   Next filename.

      if (err ne '') then goto,getoutloop ; Jump out of loop on FILENAME ERROR.

      openr,lun_next,file_next,/Get_LUN & readu,lun_next,mhedr ; Read HEADER...
      ;print,'' & print,'Reading next file ',file_next,' ...' ;   Prog. output.
      ;print,mhedr,Format=header_fstring ;       Output HEADER info. to screen.
      ;  Confirm assumed # of vel.-steps-per-spect. matches read header data...
      if (mhedr.nvsteps ne nv) then begin ; Mismatch in mhedr.nvsteps vs. nv...
         beep & print,'' & print,"SWE_NewSTRL_Read ERROR!"
         print,nv_error_string & print,"expected, read: ",nv,fix(mhedr.nvsteps)
         stop ;         Stop here with error messages in the event of an error.
      endif

      data_next = replicate(mdata,mhedr.nrec) ; Read remainder of daily file...
      readu,lun_next,data_next & free_LUN,lun_next
      print,'end reading next file ',file_next ;   ...and update screen output.

      data = [temporary(data),data_next] & data_next = 0 ;   Update input data.
      thisflnm = file_next & thisdate = date_next ;   Update filename and date.
   endfor
endif

getoutloop: ;        Jump to here on FILENAME ERROR when reading multiple days.

nrec_tot = n_elements(data) ;      Find total number of spectra read, all days.
nm_dat = replicate({LZrec:0l,spectm:0l,ta:0.d,tpb5:lonarr(3),$ ; Survey record.
                    is_null:0B,neout:0.0,enthm:0.0,teout:0.0,$
                    min_enstep:0l,Max_enstep:0l,rGSE:fltarr(3),$
                    bounds:fltarr(2,3),fitres:fltarr(6)},nrec_tot)

; *********************** Extracting "raw" input spectra into survey records...
nm_dat.LZrec = data.lzrec & nm_dat.spectm = data.spectm ;  Record-and-spectrum.
nm_dat.ta = data.tjdsec_spectm & nm_dat.tpb5 = data.pb5tim ;         Timestamp.
nm_dat.is_null = data.is_null ; This list of flags tells if a spectrum is null.
nm_dat.neout = data.neout ;     Derived electron DENSITY (#/cm^3) from fitting.
nm_dat.enthm = data.enthm ;       (model-average) electron THERMAL ENERGY (eV).
nm_dat.teout = data.teout ;     Derived electron TEMPERATURE (K)  from fitting.
nm_dat.rGSE = float(data.rgse) ;      Wind spacecraft [X,Y,Z] (GSE) orbit data.
nm_dat.bounds = data.bounds ; Mod-param. fit bnds.: (A_Kappa,s,E_bar)[min,Max].
nm_dat.fitres = data.fitres ; Rslts: [A_Kappa,s,Kappa,E_bar,Chi_sq,time (sec)].
nm_dat.Max_enstep = replicate(mhedr.max_enstep,nrec_tot) ;    Max. energy (eV).

enlst = float(volt_en_strl(mhedr.ensteps,/En )) ; List of ENERGY values, in eV.

;format_string = '("LZrec: ",I4.4,", spectm: ",I1.1,/,'+$ ; For survey records.
;                 '"ta: ",E,", tpb5: ",I4.4,I4.3,I9.8,/,'+$
;                 '"----- is_null: ",I1.1," ------",/,'+$
;                 '"neout: ",E,", enthm:",F7.2,", teout: ",E,/,'+$
;                 '"min_enstep: ",I5.4,", Max_enstep: ",I5.4,'+$
;                 '", rGSE:",3F9.2,/,"bounds: ",6G,/,"fitres: ",6G)'

; ******************************************* Completing FIRST survey record...
if nm_dat[0].is_null then begin ;     1st spectrum is "null" (no valid data)...
   nm_dat[0].ta = nm_dat[1].ta-(nm_dat[2].ta-nm_dat[1].ta) ;    Repair times...
   nm_dat[0].tpb5[2] = nm_dat[1].tpb5[2]-(nm_dat[2].tpb5[2]-nm_dat[1].tpb5[2])
   nm_dat[0].tpb5[1] = nm_dat[1].tpb5[1] ;              Copy day-of-year value.
   nm_dat[0].min_enstep = -1l & nm_dat[0].Max_enstep = -1l ;       Dummy range.
endif else begin ;  Otherwise 1st spect. is NOT "null" (contains valid data)...
   nm_dat[0].min_enstep = fix(enlst[(data[0].misc[0])-1]) ;   min. energy (eV).
endelse
;print,'' & print,"FIRST record read..." ;     Survey-record-processing HEADER.
;print,nm_dat[0],Format=format_string & stop ;      Output FIRST survey record.

for i=1l,(nrec_tot-2l) do begin ; For each non-boundry spectrum, i, of input...
   ; ****************************************** Completing ith survey record...
   if nm_dat[i].is_null then begin ;                  ith spectrum is "null"...
      nm_dat[i].ta = mean([nm_dat[i-1l].ta,nm_dat[i+1l].ta]) ;  Repair times...
      nm_dat[i].tpb5[2] = mean([nm_dat[i-1l].tpb5[2],nm_dat[i+1l].tpb5[2]])
      nm_dat[i].tpb5[1] = nm_dat[i+1].tpb5[1] ;         Copy day-of-year value.
      nm_dat[i].min_enstep = -1l & nm_dat[i].Max_enstep = -1l ;    Dummy range.
   endif else begin ;                 ...Otherwise, ith spect. is NOT "null"...
      nm_dat[i].min_enstep = fix(enlst[(data[i].misc[0])-1]) ;   min. en. (eV).
   endelse
   ;print,'' & print,nm_dat[i],Format=format_string & stop ; Output ith record.
endfor

; ******************************************** Completing LAST survey record...
lst = nrec_tot-1l ;                         Store index value of last spectrum.
if nm_dat[lst].is_null then begin ;                  Last spectrum is "null"...
   nm_dat[lst].ta = nm_dat[nrec_tot-2l].ta+$ ;              Repair time info...
                               (nm_dat[nrec_tot-2l].ta-nm_dat[nrec_tot-3l].ta)
   nm_dat[lst].tpb5[2] = nm_dat[nrec_tot-2l].tpb5[2]+$
                                                (nm_dat[nrec_tot-2l].tpb5[2]-$
                                                 nm_dat[nrec_tot-3l].tpb5[2])
   nm_dat[lst].tpb5[1] = nm_dat[nrec_tot-2l].tpb5[1] ;  Copy day-of-year value.
   nm_dat[lst].min_enstep = -1l & nm_dat[lst].Max_enstep = -1l ;   Dummy range.
endif else begin ;                   ...Otherwise, LAST spect. is NOT "null"...
   nm_dat[lst].min_enstep = fix(enlst[(data[lst].misc[0])-1]) ;  min. en. (eV).
endelse
;print,'' & print,nm_dat[lst],Format=format_string ; Output LAST survey record.
;print,"LAST record read..." & print,'' ;     Survey-record-processing TRAILER.

data = 0 ;   Erase "raw" input data after transfer to moments-survey structure.

;  Define and print to screen final begin- and end-time indices (start time)...
d.ndx[0,idatype] = 0l & d.ndx[1,idatype] = lst ;                Define indices.
d.ndx_orig[*,idatype] = d.ndx[*,idatype] ;  Initialize 'd.ndx_orig[*,idatype]'.
print,'d.ndx[*,idatype]: ',d.ndx[*,idatype] ;                  Print to screen.

tjd0 = long(fix(nm_dat[0].ta/86400.0d0)) ;   Get start time for this selection.

d = create_struct(d,'swe_newmomfdat',nm_dat) ;  Add survey data to main struct.

print,'Leaving swe_newmomf_read...' & print,'' ;           Final screen output.

end
