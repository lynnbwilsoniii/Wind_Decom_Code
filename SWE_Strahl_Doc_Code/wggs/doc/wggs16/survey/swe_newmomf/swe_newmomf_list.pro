;=============================== SWE_NewMOMf_List ============================
; Defines the list of variable names, 'list', as an output parameter.
; Note: See MPH notes for details on the Kappa-model and fitting process.
PRO swe_newmomf_list,list ;                     (MPH, Last modified: 08/18/03).

list = $ ;              Output parameter, used to identify time-series by name.
   ['N fit density',$ ;                     Electron DENSITY (#/cm^3) from fit.
    'T fit temperature',$ ;                 Electron TEMPERATURE  (K) from fit.
    '(3/2)NkT pressure',$ ;      E-tron isotrop. th. PRESS. (eV/cm^3) from fit.
    '(1/2)kT th. energy',$ ;        Electron (1D) THERMAL ENERGY (eV) from fit.
    'sqrt(kT/m) th. spd.',$ ;           Electron THERMAL SPEED (km/s) from fit.
    'Kappa-mod. th. en.',$ ;        Model-average electron THERMAL ENERGY (eV).
    'Kappa-mod. th. spd.',$ ;      Model-average electron THERMAL SPEED (km/s).
    'X s/c posn. (GSE)',$ ;           Wind s/c X-GSE position (Re) at fit time.
    'Y s/c posn. (GSE)',$ ;           Wind s/c Y-GSE position (Re) at fit time.
    'Z s/c posn. (GSE)',$ ;           Wind s/c Z-GSE position (Re) at fit time.
    'Ni (interpolated)',$ ;     Interpolated ion DENSITY  (#/cm^3) at fit time.
    'Ui (interpolated)',$ ;     Interpolated ion BULK-SPEED (km/s) at fit time.
    'A_Kappa fit result',$ ;    A_Kappa (#/[cm^3*(cm/s)^3]) fit-param. results.
    's_Kappa fit result',$ ;             s_Kappa (unitless) fit-param. results.
    'Kappa fit result',$ ;                 Kappa (unitless) fit-param. results.
    'E_bar fit result',$ ;                       E_bar (eV) fit-param. results.
    'Energy range used',$ ;             Range of energy-steps (eV) used in fit.
    'fit Chi-squared',$ ;            Chi-squared (unitless) fit-param. results.
    'fit alg. runtime'] ;                Fitting algorithm runtime (s) for fit.

end
