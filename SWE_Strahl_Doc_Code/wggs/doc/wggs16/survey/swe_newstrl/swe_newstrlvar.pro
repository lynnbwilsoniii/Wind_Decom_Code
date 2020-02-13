;============================ SWE_NewSTRLVar =================================
; Returns data variable (time series) corresponding to the variable name,
;  'varname', as defined in swe_newstrl_list.pro.    (Last modified: 02/27/04).
function swe_newstrlvar,varname

common shared,d ;                   Provides access to the main data structure.
common newstrl_stuff,pad_list_curr ;    Shared dynamic list of 'newstrl' selns.
common newstrl_plt_stuff,ith_descr,jth_en,$ ;  Shares 'newstrl' plotting state.
                         strl_cts_factor ; Vector of 15 c->f conv. (1 per en.).

idatype = where(d.datype eq 'swe_newstrl') ;        Position in data-type list.
id1 = d.ndx[0,idatype] & id2 = d.ndx[1,idatype] ;   Time series begin/end inds.

case varname of ;   Output data selected by name (defined in swe_newstrl_list).

   ; Note: The following are not the only variables within this data type, but
   ;         the remaining variables will be referenced differently than these.

   '|B| (nearest 3sec)': return,d.swe_newstrldat[id1:id2].Bmag ;  MFI mag., nT.

   'Bx (nrst 3sec-GSE)': return,d.swe_newstrldat[id1:id2].B[0] ; MFI BxGSE, nT.

   'By (nrst 3sec-GSE)': return,d.swe_newstrldat[id1:id2].B[1] ; MFI ByGSE, nT.

   'Bz (nrst 3sec-GSE)': return,d.swe_newstrldat[id1:id2].B[2] ; MFI BzGSE, nT.

   'Bphi (nrst 3s-GSE)': return,d.swe_newstrldat[id1:id2].Bphi ; GSEconv., deg.

   'Bth  (nrst 3s-GSE)': return,d.swe_newstrldat[id1:id2].Bth  ; GSEconv., deg.

   else: begin ;        Any other variable-names--assume jth energy-step and...
      ;    ...calculate the VECTOR of conversion factors (each en-step) then...
      strl_cts_factor = float(((0.3333d-27)*(65.41d-2))/(31.3d-3*$ ; Maps c->f.
                              (d.swe_newstrldat[id1].velocity/1.0d8)^4))
      ; See LZ processing routines ('mode7', 'cts_f_strl', 'lzswenewmode', ...)
      ;  as well as MPH notes on "Converting Survey-data (new-mode) Strahl
      ;  Counts to Phase-density Values" for derivation of these factors.

      case strmid(pad_list_curr[ith_descr],0,13) of ;  return ith descriptor...

         'C-Cnts. ( B):': return,d.swe_newstrldat[id1:id2].mx_B_cnts[jth_en]

         'C-PhDen ( B):': return,d.swe_newstrldat[id1:id2].mx_B_cnts[jth_en]*$
                                 strl_cts_factor[jth_en] ; Conv. fact., jth en.

         'C-Cnts. (aB):': return,d.swe_newstrldat[id1:id2].mx_aB_cts[jth_en]

         'C-PhDen (aB):': return,d.swe_newstrldat[id1:id2].mx_aB_cts[jth_en]*$
                                 strl_cts_factor[jth_en] ; Conv. fact., jth en.

         'C-Cnts. ( S):': begin ;   "core" counts--"strahl" (anti-sunward) dir.
            mxc_B = d.swe_newstrldat[id1:id2].mx_B_cnts[jth_en] ;  B-direction.
            mc_aB = d.swe_newstrldat[id1:id2].mx_aB_cts[jth_en] ; aB-direction.
            wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
            mxc_S = mxc_B & if (wBxp_cnt gt 0l) then mxc_S[wBxp] = mc_aB[wBxp]
            return,mxc_S ;    Overwrite B with aB when B-field-dir. is sunward.
         endcase

         'C-PhDen ( S):': begin ;   "core" f-val.--"strahl" (anti-sunward) dir.
            mxc_B = d.swe_newstrldat[id1:id2].mx_B_cnts[jth_en] ;  B-direction.
            mc_aB = d.swe_newstrldat[id1:id2].mx_aB_cts[jth_en] ; aB-direction.
            wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
            mxc_S = mxc_B & if (wBxp_cnt gt 0l) then mxc_S[wBxp] = mc_aB[wBxp]
            return,mxc_S*$ ;  Overwrite B with aB when B-field-dir. is sunward.
                   strl_cts_factor[jth_en] ;   Conversion factor at jth energy.
         endcase

         'C-Cnts. (aS):': begin ;   "core" counts--"anti-strahl" (sunward) dir.
            mxc_B = d.swe_newstrldat[id1:id2].mx_B_cnts[jth_en] ;  B-direction.
            mc_aB = d.swe_newstrldat[id1:id2].mx_aB_cts[jth_en] ; aB-direction.
            wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
            mc_aS = mc_aB & if (wBxp_cnt gt 0l) then mc_aS[wBxp] = mxc_B[wBxp]
            return,mc_aS ;    Overwrite aB with B when B-field-dir. is sunward.
         endcase

         'C-PhDen (aS):': begin ;   "core" f-val.--"anti-strahl" (sunward) dir.
            mxc_B = d.swe_newstrldat[id1:id2].mx_B_cnts[jth_en] ;  B-direction.
            mc_aB = d.swe_newstrldat[id1:id2].mx_aB_cts[jth_en] ; aB-direction.
            wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
            mc_aS = mc_aB & if (wBxp_cnt gt 0l) then mc_aS[wBxp] = mxc_B[wBxp]
            return,mc_aS*$ ;  Overwrite aB with B when B-field-dir. is sunward.
                   strl_cts_factor[jth_en] ;   Conversion factor at jth energy.
         endcase

         'C-PwrtB ( B):': return,d.swe_newstrldat[id1:id2].pa_mBcnts[jth_en]

         'C-PwrtB (aB):': return,d.swe_newstrldat[id1:id2].pa_maBcts[jth_en]

         'C-PwrtB ( S):': begin ;     Pitch angle--"strahl" (anti-sunward) dir.
            pan_B = d.swe_newstrldat[id1:id2].pa_mBcnts[jth_en] ;  B-direction.
            pa_aB = d.swe_newstrldat[id1:id2].pa_maBcts[jth_en] ; aB-direction.
            wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
            pan_S = pan_B & if (wBxp_cnt gt 0l) then pan_S[wBxp] = pa_aB[wBxp]
            return,pan_S ;    Overwrite B with aB when B-field-dir. is sunward.
         endcase

         'C-PwrtB (aS):': begin ;     Pitch angle--"anti-strahl" (sunward) dir.
            pan_B = d.swe_newstrldat[id1:id2].pa_mBcnts[jth_en] ;  B-direction.
            pa_aB = d.swe_newstrldat[id1:id2].pa_maBcts[jth_en] ; aB-direction.
            wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
            pa_aS = pa_aB & if (wBxp_cnt gt 0l) then pa_aS[wBxp] = pan_B[wBxp]
            return,pa_aS ;    Overwrite aB with B when B-field-dir. is sunward.
         endcase

         'C-BaWhM ( B):': return,d.swe_newstrldat[id1:id2].B_baw_hmx[jth_en]

         'C-BaWhM (aB):': return,d.swe_newstrldat[id1:id2].aB_bw_hmx[jth_en]

         'C-BaWhM ( S):': begin ;      Beam width--"strahl" (anti-sunward) dir.
            baw_B = d.swe_newstrldat[id1:id2].B_baw_hmx[jth_en] ;  B-direction.
            bw_aB = d.swe_newstrldat[id1:id2].aB_bw_hmx[jth_en] ; aB-direction.
            wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
            baw_S = baw_B & if (wBxp_cnt gt 0l) then baw_S[wBxp] = bw_aB[wBxp]
            return,baw_S ;    Overwrite B with aB when B-field-dir. is sunward.
         endcase

         'C-BaWhM (aS):': begin ;      Beam width--"anti-strahl" (sunward) dir.
            baw_B = d.swe_newstrldat[id1:id2].B_baw_hmx[jth_en] ;  B-direction.
            bw_aB = d.swe_newstrldat[id1:id2].aB_bw_hmx[jth_en] ; aB-direction.
            wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
            bw_aS = bw_aB & if (wBxp_cnt gt 0l) then bw_aS[wBxp] = baw_B[wBxp]
            return,bw_aS ;    Overwrite aB with B when B-field-dir. is sunward.
         endcase

      endcase
   endcase

endcase

end
