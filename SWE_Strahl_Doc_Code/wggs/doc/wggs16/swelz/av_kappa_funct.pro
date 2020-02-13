; ------------------------------- AV_Kappa_funct -----------------------------
; The purpose of this function is to calculate (using the Kappa-model
;  specified below, provided by A. Vinas) values of ln[f(E)], given values of
;  E; where E is expected to be a vector of kinetic energy values (in eV) of
;  solar-wind electrons, and each value in the vector f (which has the same
;  number of elements as E) is the model's predicted phase-density of
;  solar-wind electrons having the corresponding energy.
;
; Kappa-model: f(E) = A_Kappa/{[1+(E/E_bar)]^s} ==>
;                                     ln[f(E)] = ln(A_Kappa)-s*ln[1+(E/E_bar)].
;                                        (y    =      a     +b*ln[1+(  c*x  )])
;
; The parameter-list, A, for this Kappa-model is expected to have 3 elements:
;    A[0] (-> a = ln(A_Kappa)), A[1] (-> b = -s) (s = Kappa+1 > 5/2), and
;    A[2] (-> c = 1/E_bar) (E_bar = Kappa*E_th, E_th is e-tron thermal energy).
;  Also returned from this function are the three partial-derivatives of
;   ln[f(E)] with respect to these three (UNconstrained) parameters.
;
; Note: This function is intended to serve a single purpose: as a helper-
;        function for LMfit (IDL's Levenberg-Marquardt nonlinear-minimization
;        utility).  It was created according to that routine's specifications.
;       For physical reasons, the model parameters a,b,c must be constrained
;        to a_min < a < a_Max, b_min < b < b_Max, c_min < c < c_Max.  The
;        function 'range_limit_map' (see MPH notes) is used to accomplish
;        this.  The same function (with Deriv set) returns the additional
;        differential factors introduced by this additional mapping.
;                                        M. Holland, Last modified: 08/13/2003.
function av_kappa_funct,E,$ ;  Vector of energy (eV) values where f is desired.
                        A ;    Parameter-list for the 3 Kappa-model parameters.
                          ;       Note: these are the UNconstrained parameters.

common param_bounds,a_min,a_Max,b_min,b_Max,c_min,c_Max ;   Defined externally.

a_lim = range_limit_map(A[0],a_min,a_Max) ;   A[0] = a* -> a_lim = a = L_a[a*].
b_lim = range_limit_map(A[1],b_min,b_Max) ;   A[1] = b* -> b_lim = b = L_b[b*].
c_lim = range_limit_map(A[2],c_min,c_Max) ;   A[2] = c* -> c_lim = c = L_c[c*].
mdf_a = range_limit_map(A[0],a_min,a_Max,/Deriv) ;  map-deriv. fact.: L_a'[a*].
mdf_b = range_limit_map(A[1],b_min,b_Max,/Deriv) ;  map-deriv. fact.: L_b'[b*].
mdf_c = range_limit_map(A[2],c_min,c_Max,/Deriv) ;  map-deriv. fact.: L_c'[c*].
rt1 = 1+(c_lim*E) & rt2 = alog(rt1) ;   Simplify by evaluating recurring terms.
return,[[a_lim+(b_lim*rt2)],$ ;       model value:        y = ln[f(E)],
        [1.0*mdf_a],$ ;             partial deriv: dy/d(a*) = (dy/da)*L_a'[a*],
        [rt2*mdf_b],$ ;             partial deriv: dy/d(b*) = (dy/db)*L_b'[b*],
        [((b_lim*E)/rt1)*mdf_c]] ;  partial deriv: dy/d(c*) = (dy/dc)*L_c'[c*].

end
