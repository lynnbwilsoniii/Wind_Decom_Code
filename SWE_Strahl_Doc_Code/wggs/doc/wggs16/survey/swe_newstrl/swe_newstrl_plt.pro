;============================= SWE_NewSTRL_Plt ===============================
; For the panel in a series of 'npl' stackplot panels identified by 'indx',
;  locate the desired absissa and ordinate vectors for the current time series.
; Perform any final array reduction and smoothing, set any remaining plot
;  parameters and perform plotting (and saving).     (Last modified: 02/26/04).
PRO swe_newstrl_plt,indx,npl

common shared,d ;  Provides access to:                 the main data structure.
common wstuff,wst ;                the main widget interface control structure.
common sharelevelzero,pltwin_frmt,oplot_sec ;       For LZ window overplotting.
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
                idlsave,lzwin,elaps_spns ;  For sharing global plot parameters.
common sharetimeslice,oplot_sec_slice ;    For strahl application overplotting.
common newstrl_stuff,pad_list_curr ;    Shared dynamic list of 'newstrl' selns.
common newstrl_plt_stuff,ith_descr,jth_en,$ ;  Shares 'newstrl' plotting state.
                         strl_cts_factor ; Vector of 15 c->f conv. (1 per en.).

;     Identify survey data type, define time series absissa (hour of day), etc.
idatype = where(d.datype eq 'swe_newstrl') & i = indx & fill = -1.0E31
hrday = (d.swe_newstrldat[d.ndx[0,idatype]:d.ndx[1,idatype]].ta-$
                                                            d.refsec)/3600.0d0

;             Examine descriptor-list and variable-name to set plot behavior...
n_descr = n_elements(pad_list_curr) & i_max = n_descr-1l & empty_val = 'empty'
en_lst = string(d.swe_newstrldat[d.ndx[0,idatype]].energy,$ ;    Energy list...
                                            Format='(F8.1)')+' eV' ; FIXED fmt.
jth_en = (where((en_lst eq pm[i].varname),jth_en_count))[0] ; Var. list locn...
if ((jth_en eq 11) or (jth_en eq 13)) then jth_en = pm[i].ev_val ; Repeat nrgs.
cv_is_en = (jth_en_count gt 0l) ;     Current variable-name is an energy-level.

;      Exit plotting routine when user selects an energy, but no descriptors...
if (n_descr gt 0l) then $ ;    If pitch-angle dist. descriptors ARE provided...
   ; Exit if 'pm[i].varname' appears in the energy list (i.e. something that
   ;                       needs a descriptor) AND descriptor-list is empty....
     exit_crit_met = (cv_is_en and (pad_list_curr[0] eq empty_val)) $
else exit_crit_met =  cv_is_en ;                         Define exit criterion.
if exit_crit_met then begin ;                   Exit with error if crit. met...
   print,'No descriptors selected for selected energy' & return ; Leaves blank.
endif

;   Pull all info. from descr. list giving type of descriptor for each entry...
descr_family_lst = strmid(pad_list_curr, 0,7) ;  Descriptor FAMILY (type) list.
descr_anti_list  = strmid(pad_list_curr, 9,1) ;  Descriptor B/S-DIRECTION list.
descr_pres_list  = strmid(pad_list_curr,13,5) ;  Descriptor PRESENTATION  list.
BorS_dir = ' ' & anti_BorS_dir = 'a' ;  Descriptor-information possibilities...
raw = '--raw' & smoothed = ' smth' & weighted = ' wgtM' & colored = ' wMiC'
core_counts = 'C-Cnts.' & pitch_angle = 'C-PwrtB' & beam_width = 'C-BaWhM'
core_ph_den = 'C-PhDen' ;    Added descriptor for f-values (calc. from counts).

;     Determine number of sub-panels needed, based upon descriptors selected...
dummy = where((descr_family_lst eq core_counts),w_CC_count) ;   Get # CC descr.
dummy = where((descr_family_lst eq core_ph_den),w_Cf_count) ;   Get # Cf descr.
w_CC_count = w_CC_count+w_Cf_count ;         Total number of "Core-counts" sel.
dummy = where((descr_family_lst eq pitch_angle),w_PA_count) ;   Get # PA descr.
dummy = where((descr_family_lst eq  beam_width),w_BW_count) ;   Get # BW descr.
some_CC = (w_CC_count gt 0l)?1:0 ;   1 if some "core counts" sel., 0 otherwise.
some_PA = (w_PA_count gt 0l)?1:0 ;   1 if some "pitch angle" sel., 0 otherwise.
some_BW = (w_BW_count gt 0l)?1:0 ;   1 if some "beam width"  sel., 0 otherwise.
number_of_subpanels = some_CC+some_PA+some_BW ;     Number of descriptor types.
if (number_of_subpanels eq 0) then number_of_subpanels = 1 ;  Min. of 1 subpnl.

; Define panel-position information for all possible divisions of this panel...
full_panel = pos[*,i] ; Position info., curr. panel: [x_bot,y_bot,x_top,y_top].
case number_of_subpanels of ;     Based upon the number of sub-panels needed...
   1: begin ;                  Variables all to be plotted to a SINGLE panel...
      subpanel_list = full_panel ;             Retain full-panel position info.
   endcase

   2: begin ;             Variables to be distributed between TWO sub-panels...
      half_panel_height = 0.49*(full_panel[3]-full_panel[1]) ;   Incl. margins.
      bot_half = [full_panel[0], full_panel[1],$ ;    Bottom subpanel of two...
                  full_panel[2],(full_panel[1]+half_panel_height)]
      top_half = [full_panel[0],(full_panel[3]-half_panel_height),$
                  full_panel[2], full_panel[3]] ;       ...Top subpanel of two.
      subpanel_list = [[top_half],[bot_half]] ;  Sub-panel posn. info. for two.
   endcase

   3: begin ;             Variables to be distributed among THREE sub-panels...
      third_of_pnl_hght = 0.33*(full_panel[3]-full_panel[1]) ;   Incl. margins.
      mddl_of_panel = mean([full_panel[1],full_panel[3]]) ; Panel vert.-center.
      sixth_of_pnl_hght = third_of_pnl_hght/2.0 ;  Middle third, split in half.
      bot_thrd = [full_panel[0], full_panel[1],$ ;  Bottom subpanel of three...
                  full_panel[2],(full_panel[1]+third_of_pnl_hght)]
      mid_thrd = [full_panel[0],(mddl_of_panel-sixth_of_pnl_hght),$ ; Middle...
                  full_panel[2],(mddl_of_panel+sixth_of_pnl_hght)]
      top_thrd = [full_panel[0],(full_panel[3]-third_of_pnl_hght),$
                  full_panel[2], full_panel[3]] ;     ...Top subpanel of three.
      subpanel_list = [[top_thrd],[mid_thrd],[bot_thrd]] ; 3 Sub-p. posn. info.
   endcase
endcase

ith_descr = 0l & first_pass = 1 & fp_p1 = 1 & fp_p2 = 1 & fp_p3 = 1 ;     Init.
no_colorbar = 1 ;    Set to show colorbar NOT drawn this panel--only draw ONCE.
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!! LOOPBACK-POINT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
loopback_point: if (ith_descr gt i_max) then return ;     Exit after last pass.
       last_pass = (ith_descr eq i_max) ;           Set to "true" on last pass.

;     swe_newstrlvar returns data corresponding to the selected variable given
nstrlvbl = swe_newstrlvar(pm[i].varname) ;        by the string, pm[i].varname.

ntlim = 2*wst.rebin_size_line ;    Define limit on the length of a time series.
ntimes = n_elements(hrday) ;        Find the length of the current time series.

if (ntimes gt ntlim) then begin ;            Reduce array sizes if necessary...
   sclfctr = fix(float(ntimes)/float(ntlim)+0.5) ;                Scale factor.
   ntmx = ntimes/sclfctr ;               Desired length of reduced time series.

   case wst.rebin of ;                      Has user opted for array reduction?
      0: begin & time = congrid(hrday,ntmx,/Interp) ; Array reduction SELECTED.
                 varbl = congrid(nstrlvbl,ntmx,/Interp) & endcase
      else: begin & time = hrday & varbl = nstrlvbl & endcase ;  Reduction OFF.
   endcase
endif else begin & time = hrday & varbl = nstrlvbl & endelse ;  Short time ser.

help,nstrlvbl,varbl,hrday,time ;       Send output to screen about time series.

;          If user has opted for data smoothing, then perform median filtering.
if (wst.spikesout gt 0) then varbl = median(temporary(varbl),wst.spikesout)

;    Set plot chars. according to whether variable is an energy-level or not...
if cv_is_en then begin ;         If current variable-name is an energy-level...
   ;              Define useful constants and set basic plot characteristics...
   nullstrng = '' & space_array = replicate(string(32B),(pm[0].tmticks)+1)
   if first_pass then curr_ztitle = ztitle[i] else curr_ztitle = nullstrng
   curr_noerase = (noerase[i] or (not first_pass)) ; NO erase after first pass.
   curr_label = pm[i].labl+'!C'+descr_family_lst[ith_descr] ;  Add descr. info.

   ;         Set plotting flags based upon descriptor-presentations selected...
   do_special_plot = ((descr_pres_list[ith_descr] eq weighted) or $ ;  Special?
                      (descr_pres_list[ith_descr] eq colored ))?1:0
        use_colors =  (descr_pres_list[ith_descr] eq colored ) ?1:0 ; In color?

   if (descr_pres_list[ith_descr] eq smoothed) then begin ;    If "smoothed"
      for nth_smooth=1,2 do $ ;                     presentation is selected...
         varbl = smooth(temporary(varbl),3,/Edge_Truncate,/NaN) ; Do smoothing.
      curr_psym = 0 ;           Set plot-symbol to "points-connected-by-lines".
   endif else curr_psym = pm[i].psym ;      Otherwise set to usual plot symbol.

   ocolor = !P.color ;             Initialize overplot-color to global default.
   if (wst.hardcopy or $ ;  If hardcopy desired or plotting B-or-S-direction...
       (descr_anti_list[ith_descr] eq      BorS_dir)) then $
      ocolor = !P.color ;              Retain overplot-color as global default.
   if ( descr_anti_list[ith_descr] eq anti_BorS_dir)  then $ ;      Otherwise..
      ocolor = pm[i].ocolor ; ...use designated overplot-color for "anti" dirs.

   ; If the user opts for counts represented as phase-densities, then set the
   ;   f-val flag to use f-value range, and use base-10 log of f like counts...
   if (descr_family_lst[ith_descr] eq core_ph_den) then begin
      use_fval_range = 1 & descr_family_lst[ith_descr] = core_counts
   endif else use_fval_range = 0 ;       Otherwise this flag is set to "false".

   ; Note: The following logic is designed to guarantee that "core counts"
   ;        always goes above "pitch-angle", with "beam width" always lowest...
   case descr_family_lst[ith_descr] of ;  Based upon current descriptor type...
      core_counts: begin ; ----------------------------------- "core counts"...
         if (not wst.minmax) then begin ;     Plot min-Max is NOT to be used...
            yrange = pm[i].range & ynozero = 0 ; User-settable core-cnts range.
         endif else begin ;               If line plot min-Max IS to be used...
            ymn = min(varbl,Max=ymx,/NaN) ;        Find min. and Max. values...
            yrange = [ymn-(0.05*abs(ymx-ymn)),$ ;           ...and set range...
                      ymx+(0.05*abs(ymx-ymn))] & ynozero = 1
         endelse

         if use_fval_range then begin ;   If using counts as phase-densities...
            varbl = alog10(varbl>1.0E-34) ;    Use LOG-mapping for var. values.

            if (not wst.minmax) then begin ;  Plot min-Max is NOT to be used...
               yrange = $ ; *LOG(f)-values* of user-settable core-cnts range...
                  alog10(((pm[i].range>1.0)*strl_cts_factor[jth_en])>1.0E-34)
            endif else begin ;            If line plot min-Max IS to be used...
               ymn = min(varbl,Max=ymx,/NaN) ;     Find min. and Max. values...
               yrange = [ymn-(0.05*abs(ymx-ymn)),ymx+(0.05*abs(ymx-ymn))]
            endelse

            ynozero = 1 & curr_label = curr_label+'!Clog!D10!N(f)!C !C '
         endif

         curr_ticks = pm[i].ticks & curr_minor = pm[i].minor ; Use 'newstrl'
         curr_horizlin = pm[i].horizlin ;       settings (intended for this)...

         case number_of_subpanels of ;     Number of sub-panels, this energy...
            1: begin ;                              ONE (and ONLY) sub-panel...
               curr_stitle = stitle[i] ;        Set plot chars. in usual way...
               curr_xlabl = xlabl[i] & curr_xtickn = xtickn[*,i]
               curr_tmtickv = pm[i].tmtickv[0:pm[0].tmticks]
               curr_posn = subpanel_list[*,0] ;     Use TOP sub-panel position.

               if fp_p1 then begin subpanel_first_pass = 1 & fp_p1 = 0
                  endif else       subpanel_first_pass = 0 ;     Sub-p1 update.
            endcase

            2: begin ;         TWO sub-panels--of which this must be the TOP...
               curr_stitle = nullstrng ;         NO bottom-plot-panel labels...
               curr_xlabl = nullstrng & curr_xtickn = space_array
               curr_tmtickv = pm[0].tmtickv[0:pm[0].tmticks]
               curr_posn = subpanel_list[*,0] ;     Use TOP sub-panel position.

               if fp_p1 then begin subpanel_first_pass = 1 & fp_p1 = 0
                  endif else       subpanel_first_pass = 0 ;     Sub-p1 update.
            endcase

            3: begin ;       THREE sub-panels--of which this must be the TOP...
               curr_stitle = nullstrng ;         NO bottom-plot-panel labels...
               curr_xlabl = nullstrng & curr_xtickn = space_array
               curr_tmtickv = pm[0].tmtickv[0:pm[0].tmticks]
               curr_posn = subpanel_list[*,0] ;     Use TOP sub-panel position.

               if fp_p1 then begin subpanel_first_pass = 1 & fp_p1 = 0
                  endif else       subpanel_first_pass = 0 ;     Sub-p1 update.
            endcase
         endcase
      endcase ; --------------------------------------------------------------

      pitch_angle: begin ; ----------------------------------- "pitch-angle"...
         yrange = [0.0,180.0] & ynozero = 0 ;     Preset range for pitch angle.
         ; Override 'swe_newstrl_struct' settings (intended for core-counts)...
         curr_ticks = 3 & curr_minor = 3 & curr_horizlin = 90.0 ;  Perp. gyrat.

         case number_of_subpanels of ;     Number of sub-panels, this energy...
            1: begin ;                              ONE (and ONLY) sub-panel...
               curr_stitle = stitle[i] ;        Set plot chars. in usual way...
               curr_xlabl = xlabl[i] & curr_xtickn = xtickn[*,i]
               curr_tmtickv = pm[i].tmtickv[0:pm[0].tmticks]
               curr_posn = subpanel_list[*,0] ;     Use TOP sub-panel position.

               if fp_p1 then begin subpanel_first_pass = 1 & fp_p1 = 0
                  endif else       subpanel_first_pass = 0 ;     Sub-p1 update.
            endcase

            2: begin ;              TWO sub-panels (this could go in EITHER)...
               if some_CC then begin ;         If ANY "core counts" selected...
                  curr_stitle = stitle[i] ;     Set plot chars. in usual way...
                  curr_xlabl = xlabl[i] & curr_xtickn = xtickn[*,i]
                  curr_tmtickv = pm[i].tmtickv[0:pm[0].tmticks]
                  curr_posn = subpanel_list[*,1] ;   Use BOTTOM sub-panel posn.

                  if fp_p2 then begin subpanel_first_pass = 1 & fp_p2 = 0
                     endif else       subpanel_first_pass = 0 ;  Sub-p2 update.
               endif else begin ;       Otherwise, NO "core counts" selected...
                  curr_stitle = nullstrng ;      NO bottom-plot-panel labels...
                  curr_xlabl = nullstrng & curr_xtickn = space_array
                  curr_tmtickv = pm[0].tmtickv[0:pm[0].tmticks]
                  curr_posn = subpanel_list[*,0] ;  Use TOP sub-panel position.

                  if fp_p1 then begin subpanel_first_pass = 1 & fp_p1 = 0
                     endif else       subpanel_first_pass = 0 ;  Sub-p1 update.
               endelse
            endcase

            3: begin ;    THREE sub-panels--of which this must be the MIDDLE...
               curr_stitle = nullstrng ;         NO bottom-plot-panel labels...
               curr_xlabl = nullstrng & curr_xtickn = space_array
               curr_tmtickv = pm[0].tmtickv[0:pm[0].tmticks]
               curr_posn = subpanel_list[*,1] ;  Use MIDDLE sub-panel position.

               if fp_p2 then begin subpanel_first_pass = 1 & fp_p2 = 0
                  endif else       subpanel_first_pass = 0 ;     Sub-p2 update.
            endcase
         endcase
      endcase ; --------------------------------------------------------------

       beam_width: begin ; ------------------------------------ "beam width"...
         ; Override 'swe_newstrl_struct' settings (intended for core-counts)...
         yrange = [0.0,90.0] & ynozero = 0 ;       Preset range for beam width.
         curr_ticks = 3 & curr_minor = 3 & curr_horizlin = fill ;    No h-line.

         case number_of_subpanels of ;     Number of sub-panels, this energy...
            1: begin ;                              ONE (and ONLY) sub-panel...
               curr_stitle = stitle[i] ;        Set plot chars. in usual way...
               curr_xlabl = xlabl[i] & curr_xtickn = xtickn[*,i]
               curr_tmtickv = pm[i].tmtickv[0:pm[0].tmticks]
               curr_posn = subpanel_list[*,0] ;     Use TOP sub-panel position.

               if fp_p1 then begin subpanel_first_pass = 1 & fp_p1 = 0
                  endif else       subpanel_first_pass = 0 ;     Sub-p1 update.
            endcase

            2: begin ;      TWO sub-panels--of which this must be the MIDDLE...
               curr_stitle = stitle[i] ;        Set plot chars. in usual way...
               curr_xlabl = xlabl[i] & curr_xtickn = xtickn[*,i]
               curr_tmtickv = pm[i].tmtickv[0:pm[0].tmticks]
               curr_posn = subpanel_list[*,1] ;  Use MIDDLE sub-panel position.

               if fp_p2 then begin subpanel_first_pass = 1 & fp_p2 = 0
                  endif else       subpanel_first_pass = 0 ;     Sub-p2 update.
            endcase

            3: begin ;    THREE sub-panels--of which this must be the BOTTOM...
               curr_stitle = stitle[i] ;        Set plot chars. in usual way...
               curr_xlabl = xlabl[i] & curr_xtickn = xtickn[*,i]
               curr_tmtickv = pm[i].tmtickv[0:pm[0].tmticks]
               curr_posn = subpanel_list[*,2] ;  Use BOTTOM sub-panel position.

               if fp_p3 then begin subpanel_first_pass = 1 & fp_p3 = 0
                  endif else       subpanel_first_pass = 0 ;     Sub-p3 update.
            endcase
         endcase
      endcase ; --------------------------------------------------------------
   endcase
endif else begin ;                       Otherwise variable behaves normally...
   if (not wst.minmax) then begin ;   If line plot min-Max is NOT to be used...
      yrange = pm[i].range & ynozero = 0 ;                   Use preset ranges.
   endif else begin ;                     If line plot min-Max IS to be used...
      ymn = min(varbl,Max=ymx,/NaN) ; Find min. and Max. values, and set range.
      yrange = [ymn-(0.05*abs(ymx-ymn)),ymx+(0.05*abs(ymx-ymn))] & ynozero = 1
   endelse

   curr_noerase = noerase[i] & curr_label = pm[i].labl ; Set plot chars. in
   curr_ticks = pm[i].ticks & curr_minor = pm[i].minor ;       the usual way...
   curr_horizlin = pm[i].horizlin & curr_psym = pm[i].psym
   curr_posn = full_panel & subpanel_first_pass = first_pass
   curr_ztitle = ztitle[i] & curr_stitle = stitle[i] & curr_xlabl = xlabl[i]
   curr_xtickn = xtickn[*,i] & curr_tmtickv = pm[i].tmtickv[0:pm[0].tmticks]
   do_special_plot = 0 & use_colors = 0 ;  Set special plotting flags to FALSE.

   if wst.hardcopy then ocolor = !P.color else ocolor = pm[i].ocolor ; Get clr.
endelse

; ***************************** Perform plotting *****************************
if (subpanel_first_pass or do_special_plot) then begin ; First (only) "pass"...
   plot,time,varbl,Title=curr_ztitle,Subtitle=curr_stitle,Xstyle=1,$
        Xrange=pm[i].tmrange,Xticks=pm[i].tmticks,Xminor=pm[i].tminor,$
        Xtitle=curr_xlabl,Xtickname=curr_xtickn,Xtickv=curr_tmtickv,$
        Yrange=yrange,Yticks=curr_ticks,Ystyle=1,Ytitle=curr_label,$
        Ytickname=pm[i].tickname,Ytickv=pm[i].tickv,Yminor=curr_minor,$
        Psym=curr_psym,Symsize=pm[i].symsize,Noerase=curr_noerase,$
        /Normal,Position=curr_posn,Ynozero=ynozero,Xticklen=0.05,$
        Charsize=pm[i].charsize,Xcharsize=xcharsize[i],$
        Charthick=pm[i].charthick,NoData=do_special_plot ; Prevents data displ.
endif else begin ;   Subsequent "passes" (for the same variable) use 'oplot'...
   oplot,time,varbl,Color=ocolor,Psym=curr_psym,Symsize=pm[i].symsize
endelse 

if do_special_plot then begin ; User has opted to do point-by-point plotting...
   n_points = n_elements(time) & special_plot_color = !P.color ;  Basic consts.

   ;      Get time-series of Max.-counts values--similar to 'swe_newstrlvar'...
   id1 = d.ndx[0,idatype] & id2 = d.ndx[1,idatype] ;  Time ser. begin/end inds.
   B_weight_list = d.swe_newstrldat[id1:id2].mx_B_cnts[jth_en] ;  B-dir counts.
   aB_weight_lst = d.swe_newstrldat[id1:id2].mx_aB_cts[jth_en] ; aB-dir counts.
   min_weight = min([aB_weight_lst,B_weight_list],Max=Max_weight,/NaN) ; Range.
   min_l_wght = (min_weight le 1.0)?0.0:alog10(min_weight) ;   Min. LOG-counts.
   Max_l_wght = (Max_weight le 1.0)?0.0:alog10(Max_weight) ;   Max. LOG-counts.

   case strmid(pad_list_curr[ith_descr],10,1) of ;  Descriptor based on B or S?
      'B': if (descr_anti_list[ith_descr] eq anti_BorS_dir) then $ ; "B"/"aB"
              weight_list = aB_weight_lst else weight_list = B_weight_list
      'S': begin ;                 Either "strahl" or "anti-strahl" selected...
              wBxp = where((d.swe_newstrldat[id1:id2].B[0] gt 0.0),wBxp_cnt)
              if (descr_anti_list[ith_descr] eq anti_BorS_dir) then begin ; aS.
                    weight_list       = aB_weight_lst ;  Assume aB-dir. counts.
                 if (wBxp_cnt gt 0l) then $ ; ...then, if B-field is SUNWARD...
                    weight_list[wBxp] = B_weight_list[wBxp] ; overwrite  B-cts.
              endif else begin ;                Otherwise, "strahl" selected...
                    weight_list       = B_weight_list ;   Assume B-dir. counts.
                 if (wBxp_cnt gt 0l) then $ ; ...then, if B-field is SUNWARD...
                    weight_list[wBxp] = aB_weight_lst[wBxp] ; overwrite aB-cts.
              endelse
      endcase
   endcase

   if ((ntimes gt ntlim) and (wst.rebin eq 0)) then $ ;  Reduction necessary...
      weight_list = congrid(temporary(weight_list),ntmx,/Interp)
   help,weight_list ;       Send diagnostic output to screen about time-series.
   l_wght_list = fltarr(n_elements(weight_list)) ;   Initialize LOG-count-list.
   wok = where(finite(weight_list) and (weight_list ge 1.0)) ;     Find counts.
   dummy = check_math() ;  Clear error status often triggered by previous line.
   l_wght_list[wok] = alog10(weight_list[wok]) ;     Use LOG-counts where "OK".

   min_sym_size = pm[i].symsize/10.0 & Max_sym_size = 2.0 ;     Symsize bounds.
   symsize_list = $ ;   Set mapping from count-value (weight) to symbol-size...
             ((weight_list-min_weight)*(Max_sym_size/Max_weight))+min_sym_size

   n_colors = byte(!D.table_size-1l) ;  Set number of color-table-indices used.
   ;    Note: color_list[pth_point] = <color-index for weight_list[pth_point]>.
   color_list = bytscl(l_wght_list,Top=(n_colors-1B),$ ;  Note LOG-counts used.
                       Min=min_l_wght,Max=Max_l_wght)

   ; ***** Perform POINT-BY-POINT plotting--size/color modulated by "weight"...
   if use_colors then for pth_point=0l,(n_points-1l) do $ ;  Color requested...
      plots,time[pth_point],varbl[pth_point],NoClip=0,Psym=2,$ ; **Plot point**
            Symsize=symsize_list[pth_point],Color=color_list[pth_point] $
   else for pth_point=0l,(n_points-1l) do $ ;     NOT requested, use default...
      plots,time[pth_point],varbl[pth_point],NoClip=0,Psym=2,$ ; **Plot point**
            Symsize=symsize_list[pth_point],Color=special_plot_color

   if (use_colors and no_colorbar) then begin ;  Using color--disp. colorbar...
      bang_P_in = !P & bang_X_in = !X & bang_Y_in = !Y ;  Save plot-structures.

      ;       Set up colorbar image (i.e. byte-array of pixel color-indices)...
      yc = findgen(n_colors) & xc = findgen(2) ; Define color-table-index list.
      zc = byte((fltarr(100)+1.0)#yc) ;        Define colorbar (indexed) image.

      ; Set up colorbar labels, indicating "weight"-(count-)to-color mapping...
      ycrange = [min_l_wght,Max_l_wght] ;      Define colorbar LOG-count-range.
      ycticks = 3 ; Colorbar labels--def. in terms of LOG-COUNTS, NOT colors...
      yctickv = ycrange[0]+(findgen(ycticks+1)*$ ; LOG-cnt. val., each label...
                            ((ycrange[1]-ycrange[0])/float(ycticks)))
      ytickname = strcompress(string(round(10.0^yctickv),Format='(I4)')) ; CTS.

      ;     Set posn. of colorbar plot-panel (rel. to full) in device pixels...
      px = [(full_panel[2]+0.04),(full_panel[2]+0.06)]*!D.X_Vsize ;    X-range.
      py = [ full_panel[1]      , full_panel[3]      ]*!D.Y_Vsize ;    Y-range.
      sx = (px[1]-px[0])+1 & sy = (py[1]-py[0])+1 ;   ...and pixel-size of bar.

      z_new = congrid(zc,sx,sy) & sz_new = size(z_new,/Dimensions) ;    Resize.
      pos_dev_clrscl = [px[0],py[0],(px[0]+(sz_new[0]-1.0)),$ ;  Set posn. for
                                    (py[0]+(sz_new[1]-1.0))] ; ACTUAL new size.

      if (not keyword_set(wst.hardcopy)) then begin ;     NOT plotting to PS...
         tv,z_new,px[0],py[0] ;       Display new, resized color scale image...
      endif ;           Note: This must come BEFORE 'plot' and 'axis' commands.
      plot,xc,ycrange,/NoData,Xstyle=4,Ystyle=5,/NoErase,$ ;  No DATA or AXES!!
           Charsize=pm[i].charsize,Charthick=pm[i].charthick,$ ;  affects posn.
           Position=pos_dev_clrscl,/Device ;   Displ. NOTHING!---only sets map.
      axis,Yaxis=1,Yrange=ycrange,Ystyle=1,$ ; THIS actually displays the axes.
           Yticks=ycticks,Ytickv=yctickv,Ytickname=ytickname,Ytitle='cts',$
           Charsize=pm[i].charsize,Charthick=pm[i].charthick
      if keyword_set(wst.hardcopy) then begin ;  If currently plotting to PS...
         tv,zc,!X.window[0],!Y.window[0],/Norm,$ ; Disp. c-bar in norm. coords.
            Xsize=(!X.window[1]-!X.window[0]),Ysize=(!Y.window[1]-!y.window[0])
      endif ;            Note: This must come AFTER 'plot' and 'axis' commands.

      !P = bang_P_in & !X = bang_X_in & !Y = bang_Y_in ;  Restore plot-structs.
      no_colorbar = 0 ;  Set flag to FALSE (colorbar IS drawn) to do this ONCE.
   endif
endif

if (curr_horizlin ne fill) then $ ;                Horizon line overplotting...
   oplot,[hrday[0],hrday[ntimes-1l]],[curr_horizlin,curr_horizlin],$
         Linestyle=1 ;                                 Plot with a dotted line.

if (keyword_set(oplot_sec) and lzwin) then begin ;    LZ window overplotting...
   hrday_oplt = (oplot_sec-d.refsec)/3600.0d0
   oplot,[hrday_oplt,hrday_oplt],yrange,Color=175 ;          Yellow vert. line.
endif

if keyword_set(oplot_sec_slice) then begin ; Strahl application overplotting...
   hrday_oplt_slice = (oplot_sec_slice-d.refsec)/3600.0d0
   oplot,[hrday_oplt_slice,hrday_oplt_slice],yrange,Color=wst.clr_orange
endif

; If this is an energy-variable, broken into sub-panels, after the last pass...
if ((cv_is_en and (number_of_subpanels gt 1)) and last_pass) then begin
   plot,time,varbl,Title=curr_ztitle,Subtitle=curr_stitle,Xstyle=4,$ ; No Axis!
        Xrange=pm[i].tmrange,Xticks=pm[i].tmticks,Xminor=pm[i].tminor,$
        Xtitle=curr_xlabl,Xtickname=curr_xtickn,Xtickv=curr_tmtickv,$
        Yrange=yrange,Yticks=curr_ticks,Ytitle=curr_label,Ystyle=4,$ ; No Axis!
        Ytickname=pm[i].tickname,Ytickv=pm[i].tickv,Yminor=curr_minor,$
        /Noerase,/Normal,Position=full_panel,Ynozero=ynozero,Xticklen=0.05,$
        Charsize=pm[i].charsize,Xcharsize=xcharsize[i],/NoData,$ ;    No Data!!
        Charthick=pm[i].charthick ; This plots NOTHING!--resets full-panel map.
        ;  Note: This is important for other routines over-plotting this panel.
endif ; ************************* END plotting *******************************

ith_descr = ith_descr+1l ;   Increment index after plotting data for this pass.

if (cv_is_en and (not first_pass)) then goto,loopback_point ; Skip after first.

;       Note: This will only contain data and plot-settings for the FIRST pass.
if idlsave then begin ;   To save plot-data for this panel to an idlsav file...
   idlsav[i].datatype = pm[i].dtp & idlsav[i].varname = curr_label
   idlsav[i].log = pm[i].plotio & idlsav[i].psym = curr_psym
   idlsav[i].x = hrday & idlsav[i].y = nstrlvbl & idlsav[i].yrange = yrange
   idlsav[i].yticks = curr_ticks & idlsav[i].yminor = curr_minor
endif

if cv_is_en then begin ;         If current variable-name is an energy-level...
   first_pass = 0 & goto,loopback_point ; "loop back" after updating state var.
endif

end
