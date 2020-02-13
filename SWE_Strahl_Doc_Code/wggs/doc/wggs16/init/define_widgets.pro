pro define_widgets;,WDGT
common sharewidg,WDGT

;define widget structures

WDGT={$
  base_main:0l,win_main_xsize:0l,win_main_ysize:0l,win_main:0l,draw_main:0l,$
  detailed_data_type:0l,$
  path:0l,$
  datefile:0l,$
  input_ymd:0l,$
  maxnumber_days:0l,$
  maxnumber_dayskp:0l,$
  select_data_types:0l,$
  read_opts:0l,$
  select_plot_variables:0l,$
  plot_interval:0l,$
  pitch_angle_scale:0l,$
  spinavg_max_energy:0l,$
  img_array_dim_reduction:0l,$
  y_preset_minmax:0l,$
  time_axis:0l,$
  smoothing:0l,$
  timemark:0l,$
  rm_timemark:0l,$
  loadct_restorect:0l,$
  IDLsave:0l,$
  hardcopy_main:0l,$
  orbit:0l  ,$
  special_appl:0l,$
  plt_yvsx:0l,$
  base_special_appl:0l,$
  special_appl_quit:0l,$
  strahl_appl:0l,$
  Fcntrs_offon:0l,$
  strlfov:0l , $
  create_momsvfl:0l,$
  
  base_select_pltvar:0l,$
  button:lonarr(20),$
  doplot:0l,$
  preselected_pltvars:0l,$
  select_strahl_energy:0l,$
  waves_min_db:0l,$
  waves_max_db:0l,$
  waves_min_fhz:0l,$
  waves_max_fhz:0l,$
  waves_reset:0l,$
  fparaperp_max_vel:0l,$
  fparaperp_cntrs_offon:0l,$
  fparaperp_cntr_dec:0l,$
  fparaperp_smooth:0l,$
  fparaperp_reset:0l, $
  newstrl_pad:0l,          $ ; NEW!! MPH (Last modified: 08/20/03)
  
  base_time_scale:0l, $
  time_scale_begin:0l, $
  time_scale_end:0l, $
  time_scale_plot:0l, $
  time_scale_minutes:0l, $
  time_scale_save_interval:0l, $
  time_scale_save_flnm:0l, $
  time_scale_strahl:0l, $
  
  base_rdplt_seq:0l, $
  current_rdplt_seq:0l, $
  dayincre_rdplt_seq:0l, $
  pltsel_rdplt_seq:0l, $
  fwd_bkwd_rdplt_seq:0l, $
  
  base_setpath:0l,  $
  ok_setpath:0l,    $
  
  base_selprntr:0l,    $
  bw_selprntr:0l,    $
  bwflnm_selprntr:0l,    $
  bwcmd_selprntr:0l,    $
  clr_selprntr:0l,    $
  clrflnm_selprntr:0l,    $
  clrcmd_selprntr:0l,    $
  ok_selprntr:0l,    $

  swelz_base_main:0l, $  
  swelz_pnlsel:0l, $
  swelz_drawsurvey:0l, $
  swelz_hmsfld:0l, $
  swelz_fpltype:0l, $
  swelz_mode:0l, $
  swelz_days:0l, $
  swelz_pbin:0l, $
  swelz_quit:0l, $
  swelz_open:0l,$
  swelz_spectra:0l, $
  swelz_spinincre:0l, $
  swelz_spinfld:0l, $
  swelz_recincre:0l, $
  swelz_recfld:0l, $
  swelz_clrs:0l, $
  swelz_hcpy:0l, $
  swelz_orbit:0l, $
  swelz_sav:0l, $
  swelz_vstps:0l, $
  swelz_surveybase:0l, $
  swelz_fevents:0l, $
  swelz_draw1:0l, $
  swelz_draw2:0l, $
  swelz_draw3:0l, $
  swelz_vdist:0l, $ ;             New
  
  iseef_base_main:0l,$
  iseef_draw0:0l,$
  iseef_draw1:0l,$
  iseef_draw2:0l,$
  iseef_draw3:0l,$
  iseef_fpltype:0l, $
  iseef_timeincre:0l, $
  iseef_quit:0l, $
  
  swelz_special_appl:0l,$
  base_special_appl_swelz:0l,$
  special_appl_swelz_quit:0l,$
  
  swelzdspl_base:0l, $
  swelzdspl_quit:0l, $
  swelzdspl_reset:0l, $
  swelzdspl_opts:0l, $
  swelzdspl_sequence:0l, $
  swelzdspl_contrs:0l, $
  swelzdspl_specie:0l, $
  swelzdspl_speciefld:0l, $
  swelzdspl_mode:0l, $
  swelzdspl_eleion_sweep:0l, $
 
  swelzc_base:0l,$
  swelzc_draw:0l,$
  swelzc_win:0l,$
  swelzc_quit:0l,$
  swelzc_relgain:0l,$
  swelzc_pltype:0l,$
  swelzc_bckgsubtr:0l,$
  swelzc_bckgsubtr_noyes:0l, $  
  swelzc_recn_incr:0l,$
  swelzc_recn:0l,$
  swelzc_spin_incr:0l,$
  swelzc_spin:0l,$
  swelzc_sect_incr:0l,$
  swelzc_sect:0l,$
  swelzc_vstep_incr:0l,$
  swelzc_vstep:0l,$
  swelzc_mode:0l,$
  swelzc_eleion:0l,$
  swelzc_eleionswp:0l,$
  swelzc_glnt:0l,$,
  swelzc_glntnum:0l,$
  swelzc_glntdate:0l,$
  swelzc_mnmx:0l, $ 
  swelzc_rgold:0l, $
  swelzc_rgnew:0l, $
  swelzc_det:0l, $
  swelzc_rgincr:0l, $
  swelzc_vpotincr:0l, $
  swelzc_lchanincr:0l, $
  
  base_yvsx:0l, $ 
  win_yvsx_xsize:0l, $
  win_yvsx_ysize:0l, $
  draw_yvsx:0l, $
  yvsx:0l, $
  yvsx_quit:0l, $
  yvsx_y:0l, $
  yvsx_x:0l, $
  yvsx_ylog:0l, $
  yvsx_xlog:0l, $
  win_yvsx:0l, $ 
  yvsx_plot:0l, $ 
  
  swelzav_base:0l, $
  swelzav_quit:0l, $
  swelzav_doavg:0l, $
  swelzav_detpair:0l, $
  swelzav_nrmldet:0l, $
  swelzav_getrelgn:0l, $
  swelzav_set_d2_d3:0l, $
  swelzav_bgnmjf:0l, $
  swelzav_spn:0l, $
  swelzav_endmjf:0l, $
  swelzav_spinbl:0l, $
  swelzav_spins:0l, $
  swelzav_eleion:0l, $
  swelzav_ebias1:0l, $
  swelzav_ebias2:0l, $
  swelzav_calpwr:0l, $
  swelzav_ptchfitdets:0l, $
  swelzav_vstep:0l $
  
    }
  
  

end