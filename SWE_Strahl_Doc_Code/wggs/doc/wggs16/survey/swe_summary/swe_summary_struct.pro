;========================= pro swe_summary_struct ============================
; Sets plot parameters for the kth plot panel--corresponding to the variable
;  with name, 'varbl', as defined in swe_summary_list.pro.  This variable is
;  a member of the survey data type identified by 'idatyp'.  Also, the inputs
;  'swe_electrons' and 'swe_moments' are flags whose values indicate whether
;  those survey data types are among the variables to be plotted.
; Note: For each variable, the plot parameters seen below are not the only
;        ones available, rather they are cases where the default values
;        (defined elsewhere) are to be overidden.    Last modified: (02/25/02)

PRO swe_summary_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d ;                   Provides access to the main data structure.

if (not swe_electrons) then d.pnl[k].ztitle = 'SWE summary' ;   Set plot label.
;           Set subtitle as filename of data source (stripped of path info.)...
d.pnl[k].subtitle = strmid(d.flnm[idatyp],$ ;          FULL filename with path.
                           strlen(getenv(d.pathenv[idatyp])),$ ; char. to skip.
                           (strlen(d.flnm[idatyp])-$ ; Number of char. to keep.
                            strlen(getenv(d.pathenv[idatyp]))))

case varbl of ;    Set plot params. by var. name (defined in swe_summary_list).

   'Ni density (ss)': begin ;                              Ion density (cm^-3).
      d.pnl[k].labl = 'Ni'
      wlt1 = where(d.swe_summarydat.ion_den lt 1.) ;   Select range based upon
      if (wlt1[0] eq -1) then $ ;           the presence of low-density values.
         d.pnl[k].range = [1.,100.] else $
         d.pnl[k].range = [0.1,100.]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].ticks = 2
   endcase

   'Ui flow speed (ss)': begin ;                         Ion flow speed (km/s).
      d.pnl[k].labl = 'Ui'  
      d.pnl[k].range = [0.,800.]
      d.pnl[k].ticks = 2
   endcase

   'Ti temperature (ss)': begin ;                      Ion temperature (deg K).
      d.pnl[k].labl = 'Ti'
      d.pnl[k].range = [1.e4,1.e6]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
   endcase

   'T temperature (ss)': begin ;                  Electron temperature (deg K).
      d.pnl[k].labl = 'Te'
      d.pnl[k].range = [1.e4,1.e6]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
   endcase

   'A anisotropy (ss)': begin ;        Electron anisotropy index (Tpara/Tperp).
      d.pnl[k].labl = 'anis'
      d.pnl[k].range = [0.5,1.5]
      d.pnl[k].ticks = 2
      d.pnl[k].horizlin = 1.0 ;                   Draw a "horizon line" at y=1.
   endcase

   'Q heat flux (ss)': begin ;            Electron heat flux (ergs cm^-2 s^-1).
      d.pnl[k].labl = 'Q'
      d.pnl[k].range = [0.001,0.05]
      d.pnl[k].plotio = 1 ;                      Plot on a logarithmic y-scale.
      d.pnl[k].ticks = 2
   endcase

   'th_q (ss)': begin ;            Elevation (GSE) of electron heat flux (deg).
      d.pnl[k].labl = 'thq'
      d.pnl[k].range = [-90,90]
      d.pnl[k].ticks = 2
      d.pnl[k].minor = 4
      d.pnl[k].horizlin = 0. ;                    Draw a "horizon line" at y=0.
   endcase

   'ph_q (ss)': begin ;              Azimuth (GSE) of electron heat flux (deg).
      d.pnl[k].labl = 'phq'
      d.pnl[k].range = [0,360]
      d.pnl[k].ticks = 2
      d.pnl[k].minor = 4
   endcase
 
   'B magnetic field (ss)': begin ;           Magnitude of magnetic field (nT).
      d.pnl[k].labl = 'B'
      d.pnl[k].range = [0,30]
      d.pnl[k].ticks = 3
   endcase

   'th_b (ss)': begin ;                Elevation (GSE) of magnetic field (deg).
      d.pnl[k].labl = 'thb'
      d.pnl[k].range = [-90,90]
      d.pnl[k].ticks = 2
      d.pnl[k].minor = 4
      d.pnl[k].psym = 3
      d.pnl[k].horizlin = 0. ;                    Draw a "horizon line" at y=0.
   endcase

   'ph_b (ss)': begin ;                  Azimuth (GSE) of magnetic field (deg).
      d.pnl[k].labl = 'phb'
      d.pnl[k].range = [0,360]
      d.pnl[k].minor = 4
      d.pnl[k].psym = 3
   endcase
                  
   ; Inner product of unit vectors formed from: principle axis of pressure
   'cos(Pa,B) (ss)': begin ;  tensor, and magnetic field vector (P.B/(|P||B|)).
      d.pnl[k].labl = 'P.B'
      d.pnl[k].range = [-1.,1.]
      d.pnl[k].ticks = 2
      d.pnl[k].psym = 3
   endcase

   ; Inner product of unit vectors formed from: heat flux vector, and magnetic
   'cos(Q,B) (ss)': begin ;                        field vector (Q.B/(|Q||B|)).
      d.pnl[k].labl = 'Q.B'
      d.pnl[k].range = [-1.,1.]
      d.pnl[k].ticks = 2
      d.pnl[k].psym = 3
   endcase

   'strlen0 (ss)': begin ; Max. strahl beam intensity at energy0=135eV (cnts.).
      d.pnl[k].labl = 's135'
      d.pnl[k].range = [0,150] ; [0,300]
      d.pnl[k].psym = 4
      d.pnl[k].symsize = 0.2
   endcase

   ; Strahl beam width (angular extent) @ half-maximum for energy0=135eV (deg).
   'strlen0-wdth (ss)': begin
      d.pnl[k].labl = 'sw135'
      d.pnl[k].range = [0,40]
      d.pnl[k].ticks = 2
      d.pnl[k].minor = 4
      d.pnl[k].psym = 4
      d.pnl[k].symsize = 0.2
   endcase

   'strlen1 (ss)': begin ; Max. strahl beam intensity at energy1=251eV (cnts.).
      d.pnl[k].labl = 's251'
      d.pnl[k].range = [0,150] ; [0,300]
      d.pnl[k].psym = 4
      d.pnl[k].symsize = 0.2
   endcase

   ; Strahl beam width (angular extent) @ half-maximum for energy1=251eV (deg).
   'strlen1-wdth (ss)': begin
      d.pnl[k].labl = 'sw251'
      d.pnl[k].range = [0,40]
      d.pnl[k].ticks = 2
      d.pnl[k].minor = 4
      d.pnl[k].psym = 4
      d.pnl[k].symsize = 0.2
   endcase

endcase

d.pnl[k].pltype = 'y(x)' ; Plot data values versus some abssisa (usually time).

end

