;======================= function swe_summary_datatime ========================
; Returns list of data times relative to a global reference time (all of type
;  'double'), measured in units of seconds.           Last modified: (02/05/02)

function swe_summary_datatime

common shared,d ;                   Provides access to the main data structure.

return,(d.swe_summarydat.ta-d.refsec) ;           Seconds since reference time.
  
end
