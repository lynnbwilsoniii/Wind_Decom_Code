;========================== SWE_NewSTRL_TimeRange ============================
; Given the current begin ('tmn') and end ('tmx') times (measured in seconds
;  from truncated julian day epoch), this routine redefines these times for
;  the time series in the data type identified by 'idatyp'.
PRO swe_newstrl_timerange,tmn,tmx,idatyp ;      (MPH, Last modified: 08/18/03).

common shared,d ;                   Provides access to the main data structure.

tmn = d.swe_newstrldat[d.ndx[0,idatyp]].ta < tmn ; Use first time or curr. min.
tmx = d.swe_newstrldat[d.ndx[1,idatyp]].ta > tmx ;  Use last time or curr. max.
                
end
