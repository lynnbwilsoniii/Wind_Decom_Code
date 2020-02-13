;============================ SWE_NewMOMfVar =================================
; Returns data variable (time series) corresponding to the variable name,
;  'varname', as defined in swe_newmomf_list.pro.    (Last modified: 08/18/03).
function swe_newmomfvar,varname

common shared,d ;                   Provides access to the main data structure.

idatype = where(d.datype eq 'swe_newmomf') ;        Position in data-type list.
idatype_swe_ionkp = where(d.datype eq 'swe_ionkp') ;  Pos.: ION data-type list.
id1 = d.ndx[0,idatype] & id2 = d.ndx[1,idatype] ;   Time series begin/end inds.

Boltzmann = 8.6173d-5 ; Boltzmann's constant (double-precision), given in eV/K.
J_per_eV = 1.6022d-19 ; Energy-conversion const. (double-prec.), given in J/eV.
oh = 1.0d0/2.0d0 & th = 3.0d0/2.0d0 ; 1/2 and 3/2, resp. (double-prec.) consts.
me = 9.1095d-31 ;          Mass of an electron (double-precision), given in kg.

case varname of ;   Output data selected by name (defined in swe_newmomf_list).

   'N fit density': return,d.swe_newmomfdat[id1:id2].neout ;  Electron density.

   'T fit temperature': return,d.swe_newmomfdat[id1:id2].teout ; E-tron K-temp.

   '(3/2)NkT pressure':  return,float(th*d.swe_newmomfdat[id1:id2].neout*$
                               Boltzmann*d.swe_newmomfdat[id1:id2].teout)

   '(1/2)kT th. energy': return,float((oh*Boltzmann)*$ ; E-tron th. en., in eV.
                                     d.swe_newmomfdat[id1:id2].teout)

   'sqrt(kT/m) th. spd.': return,0.001*float(sqrt(((J_per_eV*Boltzmann)/me)*$
                                     d.swe_newmomfdat[id1:id2].teout)) ;  km/s.

   'Kappa-mod. th. en.': return,d.swe_newmomfdat[id1:id2].enthm ; Mod. en., eV.

   'Kappa-mod. th. spd.': return,0.001*float(sqrt((J_per_eV/me)*$ ; Mod., km/s.
                                     d.swe_newmomfdat[id1:id2].enthm))

   'X s/c posn. (GSE)': return,d.swe_newmomfdat[id1:id2].rGSE[0] ; Xgse, in Re.

   'Y s/c posn. (GSE)': return,d.swe_newmomfdat[id1:id2].rGSE[1] ; Ygse, in Re.

   'Z s/c posn. (GSE)': return,d.swe_newmomfdat[id1:id2].rGSE[2] ; Zgse, in Re.

   'Ni (interpolated)': begin ; Interpolated ion DENSITY  (#/cm^3) at fit time.
      return,interpol(d.swe_ionkpdat[d.ndx[0,idatype_swe_ionkp]:$
                                     d.ndx[1,idatype_swe_ionkp]].n,$
                     (d.swe_ionkpdat[d.ndx[0,idatype_swe_ionkp]:$
                                     d.ndx[1,idatype_swe_ionkp]].ta-$
                                             d.refsec)/3600.0d0,$
                     (d.swe_newmomfdat[id1:id2].ta-$
                                             d.refsec)/3600.0d0)
   endcase

   'Ui (interpolated)': begin ; Interpolated ion BULK-SPEED (km/s) at fit time.
      return,interpol(d.swe_ionkpdat[d.ndx[0,idatype_swe_ionkp]:$
                                     d.ndx[1,idatype_swe_ionkp]].v,$
                     (d.swe_ionkpdat[d.ndx[0,idatype_swe_ionkp]:$
                                     d.ndx[1,idatype_swe_ionkp]].ta-$
                                             d.refsec)/3600.0d0,$
                     (d.swe_newmomfdat[id1:id2].ta-$
                                             d.refsec)/3600.0d0)
   endcase

   'A_Kappa min. bound': return,d.swe_newmomfdat[id1:id2].bounds[0,0] ; A_K mn.

   'A_Kappa fit result': return,d.swe_newmomfdat[id1:id2].fitres[0] ;  A_Kappa.

   'A_Kappa Max. bound': return,d.swe_newmomfdat[id1:id2].bounds[1,0] ; A_K Mx.

   's_Kappa min. bound': return,d.swe_newmomfdat[id1:id2].bounds[0,1] ; s_K mn.

   's_Kappa fit result': return,d.swe_newmomfdat[id1:id2].fitres[1] ;  s_Kappa.

   's_Kappa Max. bound': return,d.swe_newmomfdat[id1:id2].bounds[1,1] ; s_K Mx.

   'Kappa min. bound': return,d.swe_newmomfdat[id1:id2].bounds[0,1]-1.0 ; K mn.

   'Kappa fit result': return,d.swe_newmomfdat[id1:id2].fitres[2] ;      Kappa.

   'Kappa Max. bound': return,d.swe_newmomfdat[id1:id2].bounds[1,1]-1.0 ; K Mx.

   'E_bar min. bound': return,d.swe_newmomfdat[id1:id2].bounds[0,2] ; E_bar mn.

   'E_bar fit result': return,d.swe_newmomfdat[id1:id2].fitres[3] ; E_bar (eV).

   'E_bar Max. bound': return,d.swe_newmomfdat[id1:id2].bounds[1,2] ; E_bar Mx.

   'Energy range used': begin ;              Energy range (eV) used in fitting.
      min_enstep = float(d.swe_newmomfdat[id1:id2].min_enstep) ;   LOWER bound.
      whf = where((min_enstep eq -1.0),whf_cnt) ;      Find fill-value in list.

      if (whf_cnt ne 0l) then min_enstep[whf] = !Values.f_NaN ; Replace w/ NaN.

      return,min_enstep ;    Return series of LOWER-bound values with NaN fill.
   endcase

   'Energy range Max.': begin ;         Max. energy-value (eV) used in fitting.
      Max_enstep = float(d.swe_newmomfdat[id1:id2].Max_enstep) ;   UPPER bound.
      whf = where((Max_enstep eq -1.0),whf_cnt) ;      Find fill-value in list.

      if (whf_cnt ne 0l) then Max_enstep[whf] = !Values.f_NaN ; Replace w/ NaN.

      return,Max_enstep ;    Return series of UPPER-bound values with NaN fill.
   endcase

   'fit Chi-squared': return,d.swe_newmomfdat[id1:id2].fitres[4] ; Chi-squared.

   'fit alg. runtime': return,d.swe_newmomfdat[id1:id2].fitres[5] ; Runtime, s.

endcase

end
