;========================= function swe_summaryvar ============================
; Returns data variable (time series) corresponding to the variable name,
;  'varname', as defined in swe_summary_list.pro.    Last modified: (02/05/02)

function swe_summaryvar,varname

common shared,d ;                   Provides access to the main data structure.

idatype = where(d.datype eq 'swe_summary') ;        Position in data type list.
id1 = d.ndx[0,idatype] & id2 = d.ndx[1,idatype] ;   Time series begin/end inds.

case varname of ;   Output data selected by name (defined in swe_summary_list).

   'Ni density (ss)': return,d.swe_summarydat.ion_den[id1:id2] ;   Ion density.

   'Ui flow speed (ss)': return,d.swe_summarydat.ion_vel[id1:id2] ;  Ion speed.

   'Ti temperature (ss)': return,d.swe_summarydat.ion_tem[id1:id2] ;  Ion temp.

   'T temperature (ss)': return,d.swe_summarydat.ele_tem[id1:id2] ; E-tron tmp.

   'A anisotropy (ss)': return,d.swe_summarydat.ele_ani[id1:id2] ; E-tron anis.

   'Q heat flux (ss)': return,d.swe_summarydat.ele_qmg[id1:id2] ; E. Heat flux.

   'th_q (ss)': return,d.swe_summarydat.ele_qth[id1:id2] ; Heat flux elevation.

   'ph_q (ss)': return,d.swe_summarydat.ele_qph[id1:id2] ;   Heat flux azimuth.

   'B magnetic field (ss)': return,d.swe_summarydat.ele_bmg[id1:id2] ;     |B|.

   'th_b (ss)': return,d.swe_summarydat.ele_bth[id1:id2] ; Mag. fld. elevation.

   'ph_b (ss)': return,d.swe_summarydat.ele_bph[id1:id2] ;   Mag. fld. azimuth.

   'cos(Pa,B) (ss)': return,d.swe_summarydat.ele_pdb[id1:id2] ;   P.B/(|P||B|).

   'cos(Q,B) (ss)': return,d.swe_summarydat.ele_qdb[id1:id2] ;    Q.B/(|Q||B|).

   'strlen0 (ss)': return,d.swe_summarydat.str_en0[id1:id2] ; Strhlen0 intnsty,

   'strlen0-wdth (ss)': return,d.swe_summarydat.str_e0w[id1:id2] ;  ...& width.

   'strlen1 (ss)': return,d.swe_summarydat.str_en1[id1:id2] ; Strhlen1 intnsty,

   'strlen1-wdth (ss)': return,d.swe_summarydat.str_e1w[id1:id2] ;  ...& width.

endcase

end
