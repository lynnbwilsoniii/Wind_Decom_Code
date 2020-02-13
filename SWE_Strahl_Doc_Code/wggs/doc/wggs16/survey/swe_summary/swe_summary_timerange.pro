;========================== pro swe_summary_timerange =========================
; Given the current begin ('tmn') and end ('tmx') times (measured in seconds
;  from truncated julian day epoch), this routine redefines these times for
;  the time series in the data type identified by 'idatyp'.

PRO swe_summary_timerange,tmn,tmx,idatyp ;           Last modified: (02/05/02)

common shared,d ;                   Provides access to the main data structure.

tmn = d.swe_summarydat.ta[d.ndx[0,idatyp]] < tmn ; Use first time or curr. min.
tmx = d.swe_summarydat.ta[d.ndx[1,idatyp]] > tmx ;  Use last time or curr. max.
                
end
