;=============================== SWE_NewSTRL_List ============================
; Defines the list of variable names, 'list', as an output parameter.
; Note: This default for the 'newstrl' list can be overridden at any time.
;        In particular, the energy-step numbers can be replaced with energies.
PRO swe_newstrl_list,list ;                     (MPH, Last modified: 08/18/03).

list = strarr(15+6) ;                       Initialize to list of null strings.
list[ 0:14] = string((indgen(15)+1),Format='(I2)') ; Energy-step #'s for mode7.
; Note: We assume that (because of how it is defined--see MPH notes) mode7
;        will never have other than the 15 energy-steps referrenced here.
list[15:20] = ['|B| (nearest 3sec)',$ ; Near-fit-time 3secMFI B-fld. MAGNITUDE.
               'Bx (nrst 3sec-GSE)',$ ; Near-fit-time 3secMFI GSE-Bx cmp. (nT).
               'By (nrst 3sec-GSE)',$ ; Near-fit-time 3secMFI GSE-By cmp. (nT).
               'Bz (nrst 3sec-GSE)',$ ; Near-fit-time 3secMFI GSE-Bz cmp. (nT).
               'Bphi (nrst 3s-GSE)',$ ; Near-fit-time 3secMFI B_GSE    AZIMUTH.
               'Bth  (nrst 3s-GSE)']  ; Near-fit-time 3secMFI B_GSE  ELEVATION.

end
