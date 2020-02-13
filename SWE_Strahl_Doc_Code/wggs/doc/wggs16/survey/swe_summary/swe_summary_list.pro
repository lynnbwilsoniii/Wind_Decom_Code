;========================= pro swe_summary_list =====================
; Defines the list of variable names, 'list', as an output parameter.

PRO swe_summary_list,list ;                Last modified: (02/05/02)

; These variable names are identical to those of the corresponding
;  members of the 'swe_moments', 'swe_ionkp' and 'swe_strahlen' data
;  types from which the associated time series take their values.

list = $ ;            Output parameter, used to identify time series.
   ['Ni density (ss)',$ ;                                Ion density.
    'Ui flow speed (ss)',$ ;                          Ion flow speed.
    'Ti temperature (ss)',$ ;                        Ion temperature.
    'T temperature (ss)',$ ;                    Electron temperature.
    'A anisotropy (ss)',$ ;                Electron anisotropy index.
    'Q heat flux (ss)',$ ;           Magnitude of electron heat flux.
    'th_q (ss)',$ ;            Elevation (GSE) of electron heat flux.
    'ph_q (ss)',$ ;              Azimuth (GSE) of electron heat flux.
    'B magnetic field (ss)',$ ;          Magnitude of magnetic field.
    'th_b (ss)',$ ;                Elevation (GSE) of magnetic field.
    'ph_b (ss)',$ ;                  Azimuth (GSE) of magnetic field.
    'cos(Pa,B) (ss)',$ ;         Normalized "P dot B" (P.B/(|P||B|)).
    'cos(Q,B) (ss)',$ ;          Normalized "Q dot B" (Q.B/(|Q||B|)).
    'strlen0 (ss)',$ ;    Strahl, energy0=135eV maximum intensity...
    'strlen0-wdth (ss)',$ ;             ...and width at half-maximum.
    'strlen1 (ss)',$ ;    Strahl, energy1=251eV maximum intensity...
    'strlen1-wdth (ss)'] ;              ...and width at half-maximum.

end
