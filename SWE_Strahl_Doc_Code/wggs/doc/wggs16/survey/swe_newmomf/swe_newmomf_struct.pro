;============================ SWE_NewMOMf_Struct =============================
; Sets plot parameters for the kth plot panel--corresponding to the variable
;  with name, 'varbl', as defined in swe_newmomf_list.pro.  This variable is
;  a member of the survey data type identified by 'idatyp'.  Also, the inputs
;  'swe_electrons' and 'swe_moments' are flags whose values indicate whether
;  those survey data types are among the variables to be plotted.
; Note: For each variable, the plot parameters seen below are not the only
;        ones available, rather they are cases where the default values
;        (defined elsewhere) are to be overidden.    (Last modified: 08/18/03).
PRO swe_newmomf_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d ;                   Provides access to the main data structure.

d.pnl[k].ztitle = 'SWE electrons' ;                      Set top-of-plot label.

case varbl of ;    Set plot params. by var. name (defined in swe_newmomf_list).

   'N fit density': begin  ;                Electron DENSITY (#/cm^3) from fit.
      d.pnl[k].labl = 'Ne!C#/cm!U3!N'
      wlt1 = where(d.swe_newmomfdat.neout lt 1.0) ;    Select range based upon
      if (wlt1[0] eq -1) then $ ;           the presence of low-density values.
         d.pnl[k].range = [1.0E0 ,1.0E2] else $
         d.pnl[k].range = [1.0E-1,1.0E2]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;          Small plus-signs...
      d.pnl[k].oplot = 1 & d.pnl[k].oplotvar = 'Ni density' ; ...ions ovrplttd.
   endcase

   'T fit temperature': begin ;             Electron TEMPERATURE  (K) from fit.
      d.pnl[k].labl = 'Te!CK'
      d.pnl[k].range = [1.0E4,1.0E6]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;          Small plus-signs...
      d.pnl[k].oplot = 1 & d.pnl[k].oplotvar = 'Ti temperature' ; ...with ions.
   endcase

   '(3/2)NkT pressure': begin ;  E-tron isotrop. th. PRESS. (eV/cm^3) from fit.
      d.pnl[k].labl = '(3/2)NkT!CeV/cm!U3!N'
      d.pnl[k].range = [1.0E1,1.0E3]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase

   '(1/2)kT th. energy' : begin ;   Electron (1D) THERMAL ENERGY (eV) from fit.
      d.pnl[k].labl = '(1/2)kT!CeV'
      d.pnl[k].range = [1.0E0,1.0E2]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase

   'sqrt(kT/m) th. spd.': begin ;       Electron THERMAL SPEED (km/s) from fit.
      d.pnl[k].labl = 'sqrt(kT/m)!Ckm/s'
      d.pnl[k].range = [0.0,4000.0]
      d.pnl[k].ticks = 4
      d.pnl[k].minor = 10
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;          Small plus-signs...
      d.pnl[k].oplot = 1 & d.pnl[k].oplotvar = 'Ui flow speed' ;  ...with ions.
   endcase

   'Kappa-mod. th. en.' : begin ;   Model-average electron THERMAL ENERGY (eV).
      d.pnl[k].labl = 'Eth_Kappa!CeV'
      d.pnl[k].range = [1.0E0,1.0E2]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase
   
   'Kappa-mod. th. spd.' : begin ; Model-average electron THERMAL SPEED (km/s).
      d.pnl[k].labl = 'Vth_Kappa!Ckm/s'
      d.pnl[k].range = [0.0,4000.0]
      d.pnl[k].ticks = 4
      d.pnl[k].minor = 10
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;          Small plus-signs...
      d.pnl[k].oplot = 1 & d.pnl[k].oplotvar = 'Ui flow speed' ;  ...with ions.
   endcase

   'X s/c posn. (GSE)' : begin ;      Wind s/c X-GSE position (Re) at fit time.
      d.pnl[k].labl = 'X_GSE!CRe'
      d.pnl[k].range = [-50.0,250.0]
      d.pnl[k].horizlin = 0.0 ;                                Draw Earth-line.
      d.pnl[k].ticks = 6
      d.pnl[k].minor = 5
   endcase

   'Y s/c posn. (GSE)' : begin ;      Wind s/c Y-GSE position (Re) at fit time.
      d.pnl[k].labl = 'Y_GSE!CRe'
      d.pnl[k].range = [-100.0,100.0]
      d.pnl[k].horizlin = 0.0 ;                                Draw Earth-line.
      d.pnl[k].ticks = 5
      d.pnl[k].minor = 4
   endcase

   'Z s/c posn. (GSE)' : begin ;      Wind s/c Z-GSE position (Re) at fit time.
      d.pnl[k].labl = 'Z_GSE!CRe'
      d.pnl[k].range = [-100.0,100.0]
      d.pnl[k].horizlin = 0.0 ;                                Draw Earth-line.
      d.pnl[k].ticks = 5
      d.pnl[k].minor = 4
   endcase

   'Ni (interpolated)': begin ; Interpolated ion DENSITY  (#/cm^3) at fit time.
      d.pnl[k].labl = 'Ni (interp.)!C#/cm!U3!N'
      wlt1 = where(d.swe_ionkpdat.n lt 1.0) ;          Select range based upon
      if (wlt1[0] eq -1) then $ ;           the presence of low-density values.
         d.pnl[k].range = [1.0E0 ,1.0E2] else $
         d.pnl[k].range = [1.0E-1,1.0E2]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
   endcase

   'Ui (interpolated)': begin ; Interpolated ion BULK-SPEED (km/s) at fit time.
      d.pnl[k].labl = 'Ui (interp.)!Ckm/s'
      d.pnl[k].range = [0.0,800.0]
      d.pnl[k].ticks = 2
      d.pnl[k].minor = 4
   endcase

   'A_Kappa fit result': begin ;    A_Kappa (#/[cm^3*(cm/s)^3]) fit-param. res.
      d.pnl[k].labl = 'A_Kappa!C#/[cm!U3!N*(cm/s)!U3!N]'
      d.pnl[k].range = [1.0E-27,1.0E-23]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
   endcase

   's_Kappa fit result': begin ;         s_Kappa (unitless) fit-param. results.
      d.pnl[k].labl = 's_Kappa!Cunitless'
      d.pnl[k].range = [2.0,8.0]
      d.pnl[k].horizlin = 4.0 ;                      Draw s_Kappa_typical-line.
      d.pnl[k].ticks = 6
      d.pnl[k].minor = 5
   endcase

   'Kappa fit result': begin ;             Kappa (unitless) fit-param. results.
      d.pnl[k].labl = 'Kappa!Cunitless'
      d.pnl[k].range = [1.0,7.0]
      d.pnl[k].horizlin = 3.0 ;                        Draw Kappa_typical-line.
      d.pnl[k].ticks = 6
      d.pnl[k].minor = 5
   endcase

   'E_bar fit result': begin ;                   E_bar (eV) fit-param. results.
      d.pnl[k].labl = 'E_bar!CeV'
      d.pnl[k].range = [1.0E-1,1.0E3]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
   endcase

   'Energy range used': begin ;         Range of energy-steps (eV) used in fit.
      d.pnl[k].labl = 'E range!CeV'
      d.pnl[k].range = [1.0E1,2.0E3]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].horizlin = 77.0 ;               Draw Max.-lower-bound (eV) line.
      d.pnl[k].psym = 10 ;                      Use "histogram" plotting style.
   endcase

   'fit Chi-squared': begin ;        Chi-squared (unitless) fit-param. results.
      d.pnl[k].labl = 'Chi-sq!Cunitless'
      d.pnl[k].range = [1.0E-2,1.0E2]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].psym = 3
   endcase

   'fit alg. runtime': begin ;           Fitting algorithm runtime (s) for fit.
      d.pnl[k].labl = 'rt_Kalg!Cs'
      d.pnl[k].range = [1.0E-2,1.0E0]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].psym = 3
   endcase

endcase

d.pnl[k].pltype = 'y(x)' ; Plot data values versus some abssisa (usually time).

end
