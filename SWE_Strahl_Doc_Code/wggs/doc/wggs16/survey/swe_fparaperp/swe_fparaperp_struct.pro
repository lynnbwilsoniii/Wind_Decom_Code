;=========================== SWE_fParaPerp_Struct ============================
; Sets plot parameters for the kth plot panel--corresponding to the variable
;  with name, 'varbl', as defined in swe_fparaperp_list.pro.  This variable is
;  a member of the survey data type identified by 'idatyp'.  Also, the inputs
;  'swe_electrons' and 'swe_moments' are flags whose values indicate whether
;  those survey data types are among the variables to be plotted.
; Note: For each variable, the plot parameters seen below are not the only
;        ones available, rather they are cases where the default values
;        (defined elsewhere) are to be overidden.   (Last modified: 07/20/04).
PRO swe_fparaperp_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d ;                  Provides access to the main data structure.

idatype = where(d.datype eq 'swe_fparaperp') ;  Based upon current datatype...
if (not swe_moments) then d.pnl[k].subtitle = $ ;  Set subtitle if no other...
   strmid(d.flnm[idatyp], strlen(getenv(d.pathenv[idatype])),$
   strlen(d.flnm[idatyp])-strlen(getenv(d.pathenv[idatype])))
           
case strmid(varbl,0,6) of ; Select label by TYPE of phase-density averaging...

  'f_omni': d.pnl[k].labl = 'f_OMNI' ; Average over ALL detectors (omni-dir.).
  'f_para': d.pnl[k].labl = 'f_PARA' ; Avg. over dets. in PARALLEL hemisphere.
  'f_perp': d.pnl[k].labl = 'f_PERP' ; Avg. over all quasi-PERPENDICULAR dets.
  'f_anti': d.pnl[k].labl = 'f_ANTI' ; Avg. over dets. in ANTIpara hemisphere.

endcase

case strmid(varbl,7,16) of ;  Select plot-range by SET of EN.-STEPS in plot...

   '[38-1005 eV, x2]': begin ;   Energies with: endx = [ 1, 3, 5, 7, 9,11,12].
      d.pnl[k].range = [1.0E-34,1.0E-24] & d.pnl[k].plotio = 1 ;    LOG range.
      d.pnl[k].ticks = 5 & d.pnl[k].minor = 2
   endcase

   '[58-1237 eV, x2]': begin ;   Energies with: endx = [ 2, 4, 6, 8,10,13,14].
      d.pnl[k].range = [1.0E-34,1.0E-26] & d.pnl[k].plotio = 1 ;    LOG range.
      d.pnl[k].ticks = 4 & d.pnl[k].minor = 2
   endcase

   '[38-1005 eV, x3]': begin ;      Energies with: endx = [ 1, 4, 7, 9,11,12].
      d.pnl[k].range = [1.0E-34,1.0E-24] & d.pnl[k].plotio = 1 ;    LOG range.
      d.pnl[k].ticks = 5 & d.pnl[k].minor = 2
   endcase

   '[58-1237 eV, x3]': begin ;      Energies with: endx = [ 2, 5, 8,10,13,14].
      d.pnl[k].range = [1.0E-34,1.0E-26] & d.pnl[k].plotio = 1 ;    LOG range.
      d.pnl[k].ticks = 4 & d.pnl[k].minor = 2
   endcase

   '[77-1237 eV, x3]': begin ;         Energies with: endx = [ 3, 6, 9,13,14].
      d.pnl[k].range = [1.0E-34,1.0E-26] & d.pnl[k].plotio = 1 ;    LOG range.
      d.pnl[k].ticks = 4 & d.pnl[k].minor = 2
   endcase

endcase

d.pnl[k].pltype = 'z(x,y)' ;  Note: This might not be the correct designation.

end
