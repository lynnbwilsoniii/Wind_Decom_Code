;========================= SWE_NewSTRL_DataTime ==============================
; Returns list of data times relative to a global reference time (all of type
;  'double'), measured in units of seconds.     (MPH, Last modified: 08/11/03).
function swe_newstrl_datatime

common shared,d ;                   Provides access to the main data structure.

return,(d.swe_newstrldat.ta-d.refsec) ;           Seconds since reference time.
  
end
