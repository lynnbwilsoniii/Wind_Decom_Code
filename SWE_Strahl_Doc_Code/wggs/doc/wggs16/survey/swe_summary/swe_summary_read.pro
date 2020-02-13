;======================== pro swe_summary_read ================================
; Restore file containing selected YEAR of data.  Based on user-selected time
;  interval, discard all but the currently needed data.  Finally, set begin
;  and end time indices and initial TJD time value.  Last modified: (02/05/02)

PRO swe_summary_read,TJD0_thisfile=tjd0

common shared,d ;  Provides access to:                 the main data structure.
common wstuff,wst ;                the main widget interface control structure.

print, 'swe_summary_read:' ;                             Initial screen output.

;   Restore the file, 'flnm', containing the currently selected YEAR of data...
idatype = where(d.datype eq 'swe_summary') & flnm = d.flnm[idatype]
print,'' & print,'Reading swe_summary file ',flnm,'...' ;   More screen output.
restore,flnm,/Verbose & help,swe_summary,/Structures ; Result is 'swe_summary'.

; Locate slctd time interval within restored data and define vector of elapsed
;     times (since TJD epoch) during that interval (from restored PB5 times)...
n_times = swe_summary.n_times & ta = dblarr(n_times) & doy_pos = 1 ;      Init.
doy_begin = long((ymd_pb5(long(wst.indate)))[doy_pos]) ; Beginning of interval.
doy_end = doy_begin+(long(wst.number_days)-1l) ;      End of selected interval.
doy_vals = reform(swe_summary.pb5_times[doy_pos,*]) ;   List of available days.
b = (where(doy_vals ge doy_begin))[0] ;            Index of interval beginning.
e = (reverse(where(doy_vals le doy_end)))[0] ;  Index of selected interval end.
for i=b,e do ta[i] = pb5_sec(swe_summary.pb5_times[*,i]) ;  Cnvrt needed times.

;          Insert needed data into a structure which will be appended to 'd'...
swe_summarydat = {ta:ta[b:e],$ ;        Elapsed time since TJD epoch beginning.
                  tpb5:swe_summary.pb5_times[*,b:e],$ ;              PB5 times.
                  ion_den:swe_summary.ion_den[b:e],$ ;             Ion density.
                  ion_vel:swe_summary.ion_vel[b:e],$ ;          Ion flow speed.
                  ion_tem:swe_summary.ion_tem[b:e],$ ;         Ion temperature.
                  ele_tem:swe_summary.ele_tem[b:e],$ ;    Electron temperature.
                  ele_ani:swe_summary.ele_ani[b:e],$ ;     Electron anisotropy.
                  ele_qmg:swe_summary.ele_qmg[b:e],$ ;      Electron heat flux.
                  ele_qth:swe_summary.ele_qth[b:e],$ ;     Heat flux elevation.
                  ele_qph:swe_summary.ele_qph[b:e],$ ;       Heat flux azimuth.
                  ele_bmg:swe_summary.ele_bmg[b:e],$ ;       B-field magnitude.
                  ele_bth:swe_summary.ele_bth[b:e],$ ;       B-field elevation.
                  ele_bph:swe_summary.ele_bph[b:e],$ ;         B-field azimuth.
                  ele_pdb:swe_summary.ele_pdb[b:e],$ ;            P.B/(|P||B|).
                  ele_qdb:swe_summary.ele_qdb[b:e],$ ;            Q.B/(|Q||B|).
                  str_en0:swe_summary.str_en0[b:e],$ ;    Strahl en0 intensity.
                  str_e0w:swe_summary.str_e0w[b:e],$ ;        Strahl en0 width.
                  str_en1:swe_summary.str_en1[b:e],$ ;    Strahl en1 intensity.
                  str_e1w:swe_summary.str_e1w[b:e]} ;         Strahl en1 width.

help,swe_summarydat,/Structures ;         Output info. about dataset collected.

;   Define and print to screen final begin and end time indices (start time)...
d.ndx[0,idatype] = 0 & d.ndx[1,idatype] = e-b ;                 Define indices.
d.ndx_orig[*,idatype] = d.ndx[*,idatype] ;  Initialize 'd.ndx_orig[*,idatype]'.
print,'d.ndx[*,idatype]: ',d.ndx[*,idatype] ;                  Print to screen.
tjd0 = long(fix(swe_summarydat.ta[0]/86400.d)) ;  Get start time for this file.

d = create_struct(d,'swe_summarydat',swe_summarydat) ;   Append collected data.

print,'Leaving swe_summary_read...' ;                      Final screen output.

end
