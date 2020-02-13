; =========================== Range_Limit_Map ================================
; This function takes a parameter, 'param' and range, [p_min,p_Max] as input.
; Its purpose is to provide a useful range-limiting mapping and its inverse.
; This routine may also be used to return the derivative of the mapping.
;
; The "forward" (range-limiting) mapping takes an unconstrained parameter
;  from (-Infinity,Infinity) to (p_min,p_Max):
;                   param -> (1/2)*[(p_Max+p_min)+(p_Max-p_min)*tanh(param)].
;
; The "inverse" (range-liberating) mapping takes a range-limited parameter
;  from (p_min,p_Max) to (-Infinity,Infinity):
;                   param -> (1/2)*ln[(1+x)/(1-x)], where
;                                x = [(2*param)-(p_Max+p_min)]/(p_Max-p_min).
; Note: It is CRITICAL in this inverse case that the parameter be initially
;        within the proper (open) interval, since we MUST have -1 < x < 1.
;
; Setting the 'Deriv' keyword overrides the mapping request (i.e. the setting
;  of the 'Invert' keyword is irrelavent), and results in a return-value of:
;  [d<limited>/d<liberated>](<liberated>) = (1/2)*(p_Max-p_min)*sech^2(param).
;  (Response of the constrained parameter to changes in its UNconstrained
;   version, evaluated for a position in the UNconstrained parameter-space.)
;
; No error checking/handling is done in this routine.  It is assumed that all
;  inputs (except the 'invert'/'deriv' switches) are numerical and conform to
;  the above description.  param may be a vector or array (result will have
;  the same structure), but the extrema are expected as scalars (with
;  p_min < p_Max).  All calculations (and result) use double-precision.
;                                          M. Holland, Last modified: 7/7/2003.
function range_limit_map,param,$ ;    Parameter-value (or -array) to be mapped.
                         p_min,$ ;            Lower extreme of parameter-range.
                         p_Max,$ ;            Upper extreme of parameter-range.
                         Invert=invert,$ ;    Set to perform "inverse" mapping.
                         Deriv=deriv ;     Set to return derivative of limited
                                     ;       parameter wrt liberated parameter.

param = double(param) & p_min = double(p_min[0]) & p_Max = double(p_Max[0])
sum = p_Max+p_min & diff = p_Max-p_min ;         Sum and difference of extrema.

if keyword_set(deriv) then return,(0.5d0*diff)/(cosh(param)^2) ; Mapping deriv.
;       Note: param is UNconstrained here, and sech^2(param) = 1/cosh^2(param).

if keyword_set(invert) then begin ;                 If in the "inverse" case...
   x = ((2.0d0*param)-sum)/diff & return,0.5d0*alog((1+x)/(1-x))
endif else return,0.5d0*(sum+(diff*tanh(param))) ; ...otherwise "forward" case.

end