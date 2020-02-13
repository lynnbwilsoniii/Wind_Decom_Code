;============================ SWE_NewSTRL_Struct =============================
; Sets plot parameters for the kth plot panel--corresponding to the variable
;  with name, 'varbl', as defined in swe_newstrl_list.pro.  This variable is
;  a member of the survey data type identified by 'idatyp'.  Also, the inputs
;  'swe_electrons' and 'swe_moments' are flags whose values indicate whether
;  those survey data types are among the variables to be plotted.
; Note: For each variable, the plot parameters seen below are not the only
;        ones available, rather they are cases where the default values
;        (defined elsewhere) are to be overidden.    (Last modified: 02/26/04).
PRO swe_newstrl_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d ;                   Provides access to the main data structure.

if (not swe_electrons) then d.pnl[k].ztitle = 'SWE Strahl Electrons'

case varbl of ;    Set plot params. by var. name (defined in swe_newmomf_list).

   '|B| (nearest 3sec)': begin ;        Near-fit-time 3secMFI B-fld. MAGNITUDE.
      d.pnl[k].labl = '|B|!CnT'
      d.pnl[k].range = [0.0,30.0]
      d.pnl[k].ticks = 3
      d.pnl[k].minor = 5
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase

   'Bx (nrst 3sec-GSE)': begin ;                   Near-fit-time 3secMFI BxGSE.
      d.pnl[k].labl = 'Bx!CGSE, nT'
      d.pnl[k].range = [-10.0,10.0]
      d.pnl[k].horizlin = 0.0 ;                            Draw B_YZplane line.
      d.pnl[k].ticks = 4
      d.pnl[k].minor = 5
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase

   'By (nrst 3sec-GSE)': begin ;                   Near-fit-time 3secMFI ByGSE.
      d.pnl[k].labl = 'By!CGSE, nT'
      d.pnl[k].range = [-10.0,10.0]
      d.pnl[k].horizlin = 0.0 ;                            Draw B_XZplane line.
      d.pnl[k].ticks = 4
      d.pnl[k].minor = 5
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase

   'Bz (nrst 3sec-GSE)': begin ;                   Near-fit-time 3secMFI BzGSE.
      d.pnl[k].labl = 'Bz!CGSE, nT'
      d.pnl[k].range = [-10.0,10.0]
      d.pnl[k].horizlin = 0.0 ;                            Draw B_XYplane line.
      d.pnl[k].ticks = 4
      d.pnl[k].minor = 5
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase

   'Bphi (nrst 3s-GSE)': begin ;           Near-fit-time 3secMFI B_GSE AZIMUTH.
      d.pnl[k].labl = 'Bph!CGSE, deg.'
      d.pnl[k].range = [0.0,360.0]
      d.pnl[k].horizlin = 180.0 ;                       Draw anti-sunward line.
      d.pnl[k].ticks = 3
      d.pnl[k].minor = 6
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase

   'Bth  (nrst 3s-GSE)': begin ;         Near-fit-time 3secMFI B_GSE ELEVATION.
      d.pnl[k].labl = 'Bth!CGSE, deg.'
      d.pnl[k].range = [-90.0,90.0]
      d.pnl[k].horizlin = 0.0 ;                      Draw eccliptic-plane line.
      d.pnl[k].ticks = 3
      d.pnl[k].minor = 3
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase

   else: begin ;    Any other variable name is assumed to be an energy-channel.
      d.pnl[k].labl = varbl         
      d.pnl[k].range = [0.0,500.0] ;               Default "Core-counts" range.
      d.pnl[k].ticks = 2
      d.pnl[k].minor = 5
      d.pnl[k].psym = 1 & d.pnl[k].symsize = 0.2 ;            Small plus-signs.
   endcase
endcase

d.pnl[k].pltype = 'y(x)' ; Plot data values versus some abssisa (usually time).

end
