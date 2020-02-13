;============================= SWE_summ (v1.1) ===============================
;                   M. Holland (last modified: 01/25/02)
;                      Interplanetary Physics Branch
;                 Laboratory for Extraterrestrial Physics
;             Goddard Space Flight Center: Greenbelt, MD 20771
;
; **** Generating the 'swe_summary' data set (v1.0):
; This code is designed to operate within the SWEDAT tool environment--which
;  is why the 'basic tool-related initialization' (and re-initialization) is
;  necessary.  It uses the 'input' routine to find and collect the desired
;  data types for each day in a user-defined interval, and <datype>var.pro to
;  access the time series of each individual variable within each <datype>.
; After acculmulating all desired data from all requested days; the final
;  result is a structure whose fields are either time series of each desired
;  variable (spanning the requested time interval), or longword integer scalar
;  values giving the lengths of these time series.  This structure is saved in
;  an IDL-restorable file named according to the requested interval, using the
;  naming format:      <filename> = 'ss_<date_begin>_<date_end>.sav'.
;  For example:                     'ss_19990101_19991231.sav' is the name of
;  the file containing all data from the year 1999.
; The calling syntax is:             swe_summ,<date_begin>,<date_end>
;  where <date_begin> and <date_end> are the first and last desired dates
;  (<date_begin>=<date_end> is allowed), given as longword integers of the
;  form: yyyymmdd, so:               swe_summ,19990101,19991231 generates the
;  file containing all data from the year 1999.
; The final structure, 'swe_summary', has the form:
;
;         IDL> help,swe_summary,/Structures
;          ** Structure <id>, 19 tags, length=<length>, refs=1:
;             N_TIMES         LONG              times
;             PB5_TIMES       LONG      Array[3, times]
;             ION_DEN         FLOAT     Array[times]
;             ION_VEL         FLOAT     Array[times]
;             ION_TEM         FLOAT     Array[times]
;             ELE_TEM         FLOAT     Array[times]
;             ELE_ANI         FLOAT     Array[times]
;             ELE_QMG         FLOAT     Array[times]
;             ELE_QTH         FLOAT     Array[times]
;             ELE_QPH         FLOAT     Array[times]
;             ELE_BMG         FLOAT     Array[times]
;             ELE_BTH         FLOAT     Array[times]
;             ELE_BPH         FLOAT     Array[times]
;             ELE_PDB         FLOAT     Array[times]
;             ELE_QDB         FLOAT     Array[times]
;             STR_EN0         FLOAT     Array[times]
;             STR_E0W         FLOAT     Array[times]
;             STR_EN1         FLOAT     Array[times]
;             STR_E1W         FLOAT     Array[times]
;
; Note: <length> gives the total amount of data in the final result, measured
;        in bytes.  Some overhead is added to the final *.sav file.
;
; **** Version 1.1 (initiated 01/24/02):
; This version was needed to overcome the problem of having variables with
;  several different time resolutions--and to add the variables q_th and q_ph
;  (heat flux elevation and azimuth) to the data set.
; The 'interpol' function (from the IDL library) is used to perform LINEAR
;  interpolation from the electron and strahl time resolutions to the ion
;  time resolution.  Ultimately, all variables are represented as functions
;  of ion time; and only the ion (PB5) times are stored.
;=============================================================================

;============================= Gen_SWE_Summary ===============================
; Used in generating each day (given by 'date', a STRING with format:
;  'yyyymmdd') of the new 'swe_summary' data type.  Stores the (dynamically
;  growing) time series for each variable for each day, in the common block
;  'SWEsumm_shared'.  Some 'basic tool-related initialization' is necessary.
; Note: The state of the 'first_?date' flags determines whether the contents
;        of a time series are overwritten with or appended with new data.
function gen_swe_summary,date

common shared,d & common wstuff,wst ; Define/reference the common blocks used.
common SWEsumm_shared,ion_pb5,ion_den,ion_vel,ion_tem,ele_pb5,ele_tem,$
                      ele_ani,ele_qmg,ele_qth,ele_qph,ele_bmg,ele_bth,$
                      ele_bph,ele_pdb,ele_qdb,str_en0,str_e0w,str_en1,$
                      str_e1w,str_pb0,str_pb1,first_idate,first_edate,$
                      first_sdate,true,false,error_header,error_trailer
                      ; Contains all time series for each day (& flags, etc.).

panelist & structuresw ;       Re-initialize control structures for each date.
wst.indate = date ;           Set "current" date (string, format: 'yyyymmdd').
wst.strlen0 = 135 & wst.strlen1 = 251 ; Set strahl energies to: 135eV & 251eV.

;      Use 'input' to get one day of data for each desired survey data type...
input,where(d.datype eq 'swe_ionkp'),Err=ion_err ;         Ion Key Parameters.
ion_ndx0 = d.ndx[0,where(d.datype eq 'swe_ionkp')] ;     Save ionkp indices...
ion_ndx1 = d.ndx[1,where(d.datype eq 'swe_ionkp')]
input,where(d.datype eq 'swe_moments'),Err=ele_err ;      SWE (electron) data.
ele_ndx0 = d.ndx[0,where(d.datype eq 'swe_moments')] ; Save moments indices...
ele_ndx1 = d.ndx[1,where(d.datype eq 'swe_moments')]
input,where(d.datype eq 'swe_strahlen'),Err=str_err ; Strahl (135,251)eV data.
str_ndx0 = d.ndx[0,where(d.datype eq 'swe_strahlen')] ; Save strahl indices...
str_ndx1 = d.ndx[1,where(d.datype eq 'swe_strahlen')]
d.ndx[0,where(d.datype eq 'swe_ionkp')] = ion_ndx0 ;  Restore ionkp indices...
d.ndx[1,where(d.datype eq 'swe_ionkp')] = ion_ndx1
d.ndx[0,where(d.datype eq 'swe_moments')] = ele_ndx0 ; Restore mom. indices...
d.ndx[1,where(d.datype eq 'swe_moments')] = ele_ndx1
d.ndx[0,where(d.datype eq 'swe_strahlen')] = str_ndx0 ;  Restore strl. inds...
d.ndx[1,where(d.datype eq 'swe_strahlen')] = str_ndx1
;          Note: The 'input' routine stores ALL collected data in struct 'd',
;               the contents of which can be accessed through <datype>var.pro.
;               However, the 'input' routine also resets the list of datatype
;               indices, 'd.ndx', each time it is called (hence save/restore).

if (ion_err eq '') then begin ;      If no error in reading this day's data...
   if first_idate then begin ;     If this is the first date producing data...
      ion_pb5 = d.swe_ionkpdat[ion_ndx0:ion_ndx1].tpb5 ;      Ion time in PB5.
      ion_den = swe_ionkpvar('Ni density') ;                      Ion density.
      ion_vel = swe_ionkpvar('Ui flow speed') ;                Ion bulk speed.
      ion_tem = swe_ionkpvar('Ti temperature') ;              Ion temperature.
      first_idate = false ;     Set flag to indicate: no longer at first date.
   endif else begin ;   Otherwise append to dynamically growing time series...
      ion_pb5 = [[ion_pb5],$ ;                                Ion time in PB5.
                 [d.swe_ionkpdat[ion_ndx0:ion_ndx1].tpb5]]
      ion_den = [ion_den,swe_ionkpvar('Ni density')] ;            Ion density.
      ion_vel = [ion_vel,swe_ionkpvar('Ui flow speed')] ;      Ion bulk speed.
      ion_tem = [ion_tem,swe_ionkpvar('Ti temperature')] ;    Ion temperature.
   endelse
endif else begin ;                            Otherwise issue error message...
   print,'' & print,error_header+ion_err & print,error_trailer & print,''
endelse

if (ele_err eq '') then begin ;      If no error in reading this day's data...
   if first_edate then begin ;     If this is the first date producing data...
      ele_pb5 = d.swe_mdat[ele_ndx0:ele_ndx1].tpb5 ;     Electron time in PB5.
      ele_tem = swe_momentsvar('T temperature') ;        Electron temperature.
      ele_ani = swe_momentsvar('A anisotropy') ;      E-tron temp. anisotropy.
      ele_qmg = swe_momentsvar('Q heat flux') ;  Electron heat flux magnitude.
      ele_qth = swe_momentsvar('th_q') ;         Electron heat flux elevation.
      ele_qph = swe_momentsvar('ph_q') ;           Electron heat flux azimuth.
      ele_bmg = swe_momentsvar('B magnetic field') ; E-tron B-field magnitude.
      ele_bth = swe_momentsvar('th_b') ;           Electron B-field elevation.
      ele_bph = swe_momentsvar('ph_b') ;             Electron B-field azimuth.
      ele_pdb = swe_momentsvar('cos(Pa,B)') ;  E-tron cos(P,B) = P.B/(|P||B|).
      ele_qdb = swe_momentsvar('cos(Q,B)') ;   E-tron cos(Q,B) = Q.B/(|Q||B|).
      first_edate = false ;     Set flag to indicate: no longer at first date.
   endif else begin ;   Otherwise append to dynamically growing time series...
      ele_pb5 = [[ele_pb5],$ ;                           Electron time in PB5.
                 [d.swe_mdat[ele_ndx0:ele_ndx1].tpb5]]
      ele_tem = [ele_tem,swe_momentsvar('T temperature')] ;     Electron temp.
      ele_ani = [ele_ani,swe_momentsvar('A anisotropy')] ;  E-tron anisotropy.
      ele_qmg = [ele_qmg,swe_momentsvar('Q heat flux')] ;  Electron heat flux.
      ele_qth = [ele_qth,swe_momentsvar('th_q')] ; E-tron heat flux elevation.
      ele_qph = [ele_qph,swe_momentsvar('ph_q')] ;   E-tron heat flux azimuth.
      ele_bmg = [ele_bmg,swe_momentsvar('B magnetic field')] ;    B-field mag.
      ele_bth = [ele_bth,swe_momentsvar('th_b')] ; Electron B-field elevation.
      ele_bph = [ele_bph,swe_momentsvar('ph_b')] ;   Electron B-field azimuth.
      ele_pdb = [ele_pdb,swe_momentsvar('cos(Pa,B)')] ;     Electron cos(P,B).
      ele_qdb = [ele_qdb,swe_momentsvar('cos(Q,B)')] ;      Electron cos(Q,B).
   endelse
endif else begin ;                            Otherwise issue error message...
   print,'' & print,error_header+ele_err & print,error_trailer & print,''
endelse

if (str_err eq '') then begin ;      If no error in reading this day's data...
   if first_sdate then begin ;     If this is the first date producing data...
      n_en0 = n_elements((swe_strahlenvar('strlen0')).f) ; Find # values @en0.
      str_en0 = (swe_strahlenvar('strlen0')).f ;    SWE strahl intensity @en0.
      str_e0w = (swe_strahlenvar('strlen0-wdth')).f ;   SWE strahl width @en0.
      str_en1 = (swe_strahlenvar('strlen1')).f ;    SWE strahl intensity @en1.
      str_e1w = (swe_strahlenvar('strlen1-wdth')).f ;   SWE strahl width @en1.
      str_pb0 = $ ;                           SWE strahl PB5 time at energy 0.
                (d.swe_strahlendat[str_ndx0:str_ndx1].pb5)[*,0:(n_en0-1)]
      str_pb1 = $ ;                           SWE strahl PB5 time at energy 1.
                (d.swe_strahlendat[str_ndx0:str_ndx1].pb5)[*,n_en0:*]
      ;      Note: The PB5 times for the two energies were stored together--
      ;             they are separated here, and henceforth stored separately.
      first_sdate = false ;     Set flag to indicate: no longer at first date.
   endif else begin ;   Otherwise append to dynamically growing time series...
      n_en0 = n_elements((swe_strahlenvar('strlen0')).f) ; Find # values @en0.
      str_en0 = [str_en0,(swe_strahlenvar('strlen0')).f] ;    SWE strahl @en0.
      str_e0w = [str_e0w,(swe_strahlenvar('strlen0-wdth')).f] ; strl wdth @e0.
      str_en1 = [str_en1,(swe_strahlenvar('strlen1')).f] ;    SWE strahl @en1.
      str_e1w = [str_e1w,(swe_strahlenvar('strlen1-wdth')).f] ; strl wdth @e1.
      str_pb0 = [[str_pb0],$ ;                SWE strahl PB5 time at energy 0.
                 [(d.swe_strahlendat[str_ndx0:str_ndx1].pb5)[*,0:(n_en0-1)]]]
      str_pb1 = [[str_pb1],$ ;                SWE strahl PB5 time at energy 1.
                 [(d.swe_strahlendat[str_ndx0:str_ndx1].pb5)[*,n_en0:*]]]
      ;      Note: The PB5 times for the two energies were stored together--
      ;             they are separated here, and henceforth stored separately.
   endelse
endif else begin ;                            Otherwise issue error message...
   print,'' & print,error_header+str_err & print,error_trailer & print,''
endelse

return,((ion_err eq '') or (ele_err eq '') or (str_err eq '')) ;   Data found?

end


;================================= SWE_Summ =================================
; Given a begin date ('date_begin') and an end date ('date_end') of the form:
;  yyyymmdd (longword integer).  This routine finds data of interest by date
;  (from begin to end) and stores it in the common block, 'SWEsumm_shared'.
; Ultimately, the final collection of time series are stored in the
;  structure, 'swe_summary' and saved in an IDL-restorable '<dates>.sav'
;  file--where <dates> is taken from the begin and end dates.
PRO swe_summ,date_begin,date_end

common SWEsumm_shared ;                 Contains all time series for each day.

;                                 Perform basic tool-related initialization...
define_widgets & panelist & structuresw & decompress_tbl

;                                     Define constants for this application...
true = 1 & false = 0 & pb5ref = ymd_pb5(19941130l) ; <-WIND mission beginning.
date_begin = long(date_begin) & date_end = long(date_end) ;     Recast inputs.
elapsec_begin = pb5_elapsec(ymd_pb5(date_begin),pb5ref) ;   Seconds since ref.
elapsec_end = pb5_elapsec(ymd_pb5(date_end),pb5ref) ;  Secs. since ref. @ end.
secs_in_one_day = double(86400l) ;    Define the number of seconds in one day.
error_header = 'SWE_Summ: ' ;      These go before/after any error messages...
error_trailer = 'Some data from this day may not be included in the dataset.'
border = '#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*#@*'

elapsec = elapsec_begin ;   Initialize state variables for this application...
first_idate = true & first_edate = true & first_sdate = true ; Set some flags.
dates_not_found = strarr(1) ;       Initialize list of missing dates to empty.
some_dates_missing = false ;       Assume initially that no dates are missing.

while (elapsec le elapsec_end) do begin ;   While within sel. time-interval...
   pb5 = elapsec_pb5(elapsec,pb5ref) ;   Convert "current" time to PB5 format.
   date = string(pb5_ymd(pb5),Format='(i8)') ;   Set date info. for curr. day.

   ;  Send header & trailer information to screen and gather "current" data...
   date_header = border+' BEGIN: '+date & date_trailer = border+' END: '+date
   print,'' & print,'' & print,date_header & print,border ;    Skip TWO lines.
   success = gen_swe_summary(date) ; Call data finding routine, note exit val.
   print,border & print,date_trailer & print,'' & print,'' ;   Skip TWO lines.

   if (not success) then begin ;           If requested data was NOT found ...
      ;              Add "current" date to list of missing requested dates ...
      if (some_dates_missing) then begin ;      At least one date in the list.
         dates_not_found = [dates_not_found,date] ;      Add new date to list.
      endif else begin ;          Enter first filename into list and set flag.
         dates_not_found[0] = date & some_dates_missing = true
      endelse
   endif ;  Note: This list will be printed after all dates have been visited.

   catch,error_status ; Error handler.  When errors occur within this routine
   ;                     (and the routines it calls), the error index is
   ;                     stored in 'error_status' and execution resumes here.
   if (error_status ne 0) then begin ;            In the event of an error...
      print,error_header,!Err_String & print,error_trailer & print,''
   endif ;         ...and continue to the next selected day (do nothing else).
   
   elapsec = elapsec+secs_in_one_day ;    Increment "current" time by one day.
endwhile

if (some_dates_missing) then begin ;  Final output to screen for this routine.
   print,'List of missing requested dates:' & print,dates_not_found & print,''
endif

if ((not first_idate) and (not first_edate) and (not first_sdate)) then begin
   ; IF the flags are set to indicate AT LEAST ONE day of EACH data type has
   ;  been found and read into memory, then begin some final processing of
   ;  the located data (putting all variables at a common time resolution)...
   n_ion = (size(ion_pb5,/Dim))[1] ; Store # of recorded...         ION times.
   n_ele = (size(ele_pb5,/Dim))[1] ;                           ELECTRON times.
   n_st0 = (size(str_pb0,/Dim))[1] ;                        STRAHL times @en0.
   n_st1 = (size(str_pb1,/Dim))[1] ;                        STRAHL times @en1.
   ion_ta  = dblarr(n_ion) & ele_ta  = dblarr(n_ele) ;  Create 'elapsed secs.'
   str_ta0 = dblarr(n_st0) & str_ta1 = dblarr(n_st1) ;   arrays for each type.

   ;   For each PB5 time in each time series, create a "time elapsed" value...
   ;   Note: These are TEMPORARY, used only for interpolation--then discarded.
   ;         Also, the PB5 reference time (from which the elapsed seconds
   ;         start counting) used is the same as used above in time-loop.
   for i=0l,(n_ion-1l) do ion_ta[i]  = pb5_elapsec(ion_pb5[*,i],pb5ref)
   for i=0l,(n_ele-1l) do ele_ta[i]  = pb5_elapsec(ele_pb5[*,i],pb5ref)
   for i=0l,(n_st0-1l) do str_ta0[i] = pb5_elapsec(str_pb0[*,i],pb5ref)
   for i=0l,(n_st1-1l) do str_ta1[i] = pb5_elapsec(str_pb1[*,i],pb5ref)

   ; Here, all time series sampled (in time) differently than the ION data
   ;  are resampled (using IDL's 'interpol' function) at the ION times--so
   ;  that all of the resulting time series are vectors of ordinate values
   ;  with a common vector of abssisa values (the ION times).  The ion time
   ;  resolution is chosen because it is between that of the strahl and
   ;  electron data.  Also, the method of interpolation chosen is the default
   ;  LINEAR interpolation--because of its simplicity and the resemblance of
   ;  its results to the natural behavior of IDL's 'plot' routine.
   ele_tem = float(interpol(ele_tem,ele_ta ,ion_ta)) ;   Electron temperature.
   ele_ani = float(interpol(ele_ani,ele_ta ,ion_ta)) ; E-tron tmp. anisotropy.
   ele_qmg = float(interpol(ele_qmg,ele_ta ,ion_ta)) ;    Heat flux magnitude.
   ele_qth = float(interpol(ele_qth,ele_ta ,ion_ta)) ;    Heat flux elevation.
   ele_qph = float(interpol(ele_qph,ele_ta ,ion_ta)) ;      Heat flux azimuth.
   ele_bmg = float(interpol(ele_bmg,ele_ta ,ion_ta)) ;      B-field magnitude.
   ele_bth = float(interpol(ele_bth,ele_ta ,ion_ta)) ;      B-field elevation.
   ele_bph = float(interpol(ele_bph,ele_ta ,ion_ta)) ;        B-field azimuth.
   ele_pdb = float(interpol(ele_pdb,ele_ta ,ion_ta)) ;  cos(P,B)=P.B/(|P||B|).
   ele_qdb = float(interpol(ele_qdb,ele_ta ,ion_ta)) ;  cos(Q,B)=Q.B/(|Q||B|).
   str_en0 = float(interpol(str_en0,str_ta0,ion_ta)) ;  Strahl intensity @en0.
   str_e0w = float(interpol(str_e0w,str_ta0,ion_ta)) ;      Strahl width @en0.
   str_en1 = float(interpol(str_en1,str_ta1,ion_ta)) ;  Strahl intensity @en1.
   str_e1w = float(interpol(str_e1w,str_ta1,ion_ta)) ;      Strahl width @en1.
   
   ; Store the final results in a structure whose fields contain time series
   ;  of each of the desired parameters (except for the 'n_times' field
   ;  which contains the number of recorded times in any time series).
   swe_summary = {n_times:n_ion,pb5_times:ion_pb5,$ ;   # of rec. (ION) times.
                  ion_den:ion_den,ion_vel:ion_vel,ion_tem:ion_tem,$
                  ele_tem:ele_tem,ele_ani:ele_ani,ele_qmg:ele_qmg,$
                  ele_qth:ele_qth,ele_qph:ele_qph,ele_bmg:ele_bmg,$
                  ele_bth:ele_bth,ele_bph:ele_bph,ele_pdb:ele_pdb,$
                  ele_qdb:ele_qdb,str_en0:str_en0,str_e0w:str_e0w,$
                  str_en1:str_en1,str_e1w:str_e1w} ;     Final data structure.

   help,swe_summary,/Structures ;         Save final results (if any exist)...
   save,swe_summary,/Verbose,Filename='ss_'+string(date_begin,Format='(i8)')+$
                                     '_'+string(date_end,Format='(i8)')+'.sav'
endif

end
