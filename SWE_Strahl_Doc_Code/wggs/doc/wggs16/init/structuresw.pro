pro structuresw

common wstuff,wst
common swestuff,swest
common shareisee,iseest
common shared,d


;define main widget interface control structure, wst  
;if keyword_set(wst) eq 0 then begin

mom_version=['Latest','v06','v05','v04','Special']

ptch_version=['Latest','v05','v4','Special']

indata_types=d.datype 

nplmx=20

wst={$
   mom_version:mom_version,$
   ptch_version:ptch_version,$
   date_file:'Date', $
   indate:'',    $
   lzindate:'',   $
   mfilter:'',   $
   mfltr:'',   $
   pfilter:'',$
   pfltr:'',$
   
   number_days:1, $
   maxnumber_days:31, $
   lzdays:strarr(31),  $
   inxt:0 ,    $

   lzdate:'',     $
   magdate:'',    $
   surveydate:'', $
   atdate:'',     $
   orbdate:'',    $

   indata_types:indata_types,       $
   yesdata:0,                          $
   selectinput:-1+intarr(n_elements(indata_types)),     $
   selectinput_last:-1+intarr(n_elements(indata_types)),$

   nplmx:nplmx,              $
   ypnl_relsiz:fltarr(nplmx),$
   xyrange:fltarr(4,nplmx),  $
   xywindow:fltarr(4,nplmx), $
   xysize:fltarr(4,nplmx),   $
   ylog:intarr(nplmx) ,      $
   
   xyrange_mainnwin:fltarr(4,nplmx),  $
   xywindow_mainnwin:fltarr(4,nplmx), $
   xysize_mainnwin:fltarr(4,nplmx),   $
   ylog_mainnwin:intarr(nplmx) ,      $
   
   xyrange_lzwin:fltarr(4),  $
   xywindow_lzwin:fltarr(4), $
   xysize_lzwin:fltarr(4),   $
   ylog_lzwin:0 ,      $
   
   minmax:0,                 $
   hardcopy:0,               $
   cscale:0,                 $
   cf:0,                     $
   newstrlmax:4032l,             $
   ptch_img_opt:0,           $

   smoothing:['0','3','5','10'],$
   spikesout:0, $
   timavg:0,    $

   timemark_offon:0,$
   timemark:dblarr(100),$
   timemark_offon_last:0,$
   
   sldrmnmx:fltarr(2), $

   timsel:'lz', $

   hrsel:0.,         $
   tmn:0.d, tmx:0.d,  $

   xydata:dblarr(2), $
   hms:'',           $
   lhms:0l,          $
   ymd:0l,$ 
   pb5:lonarr(3),     $
   
   pb5_begin:lonarr(3),$
   pb5_end:lonarr(3),$
   ymd_begin:0l,$
   hms_begin:'',$
   ymd_end:0l,$
   hms_end:'',$
   
   lz_is_read:0,  $

   printer:'',   $
   print_flnm:'idl.ps',  $
   print_cmd:'',   $

   printer_bw:'',   $
   print_flnm_bw:'idl_bw.ps',  $
   print_cmd_bw:'',   $
   printer_name_bw_orig:'',  $
   print_filename_bw_orig:'',  $

   printer_clr:'',   $
   print_flnm_clr:'idl_clr.ps',  $
   print_cmd_clr:'',   $
   printer_name_clr_orig:'',  $
   print_filename_clr_orig:'',  $

   ncolors:0,   $
   
   colortable_ps:0,  $
   colortable:0,  $
   clr_green:0,  $
   clr_orange:0,  $

   fhz1_def:4.,  fhz2_def:120.,  $
   scale1_def:0.,scale2_def:30.,  $
      
   fhz1:4.,  fhz2:120.,  $
   scale1:0.,scale2:30.,  $
   foreshock:0, $
   oplt_event:'Off',  $
   event_type:getenv('FEVENTTYPE'),$
   offon:['Off','On'],  $
   noyes:['No','Yes'],$
   min_intrvl:15,  $
   auto_spin:0,  $
   strlenergies:fix(volt_en_strl(1+indgen(63),/en)), $
   strlen0:154,  $
   strlen1:251,  $
   time_interval_flnm:getenv('IDLSAV')+'time_intervals', $
   strlappl_intrvl:0,  $
   rebin:0,  $
   rebin_size_line:720, $
   rebin_size_img:360,  $
   
   strlfov:0l, $
   creatmsvfl:0l  $
   }



;set default color table specified in startup
if getenv('DISPLAY') ne '' then clrtbl_indx

;restore,getenv('IDLSAV')+'mfilter'
mfilter='Latest'
wst.mfilter=mfilter

restore,getenv('IDLSAV')+'printer_def_bw'
wst.printer_name_bw_orig=printer_name_bw
wst.printer_bw=printer_name_bw
wst.print_filename_bw_orig=print_filename_bw
wst.print_flnm_bw=print_filename_bw
wst.print_cmd_bw='lp '+wst.printer_bw+' '+getenv('IDLSAV')+wst.print_flnm_bw

restore,getenv('IDLSAV')+'printer_def_clr'
wst.printer_name_clr_orig=printer_name_clr
wst.printer_clr=printer_name_clr
wst.print_filename_clr_orig=print_filename_clr
wst.print_flnm_clr=print_filename_clr
wst.print_cmd_clr='lp '+wst.printer_clr+' '+getenv('IDLSAV')+wst.print_flnm_clr
wst.ncolors=!d.table_size

;endif

;define SWE levelzero related control structure
;if keyword_set(swest) eq 0 then begin


swest={   $
  noyes:['No','Yes'],$
  
  lzflnm:'',  $
 
  subtrbkg:'Yes',  $
  subtrbkg01:1,  $

  delete:1 ,     $
  delete_set:0,  $
  ndel:0,        $

  spn:0,  $

  nvmax:0,  $
  nvmaxlist:['16','15','14','13','12','11','10'],$
  
  specie:['elecs','ions'],  $
  specie_selct:0,           $
  swpmd:['all elecs','all ions ','alt sects','alt spins', 'bckg test'],  $


  vplotsel:'',         $
  splotsel:'',         $
  ndets:0,             $
  nvsteps:0,           $
  nsectors:0,          $
  nspins:0,            $
  nspectra:0,          $
  idet:0,              $
  ivstep:0,            $
  isector:0,           $
  ispinbl:0,           $
  ispectra:0,          $
  ensteps_ev:fltarr(16),$
  strlensteps_ev:fltarr(15),$
  nstrldets:0,         $
  nstrlsteps:0,        $
  istrldet:0,          $
  istrlstep:0,         $

  vplotsel_last:'',     $
  splotsel_last:'',     $
  noshow:0,             $
 
  minmax:0,  $

  pbinselect:9,  $ 
  pbins:['3','6','9','12','15'],$
  

  hidepoints:0 , $
  c_labels:0,$
  gap_interpol:1, $
  c_decade:0.75,  $
  savez:0, $
  
  win:lonarr(4),$
  win_xsize:lonarr(4),$
  win_ysize:lonarr(4),$
  
  lzwin:[1,2,3],  $
  win_pltype:lonarr(10,3),  $
  win_npltypes:lonarr(3), $
  win_delete:1+lonarr(3),  $
  win_nvmax:fltarr(3),  $
  win_hidepoints:lonarr(3),  $

  spndt:'',  $
  pb5:lonarr(3),$
  
  mode_this_file:0,  $

  strlf_veisf:0,  $

  strlstep_slct:13, $

  strlstep_slct_lst:13, $
  
  ilzplt:-1,    $
  lzsurvey:0,   $
  
  pltype_index:5, $
  autoseq:1,  $
  
  
  vmin_redf:-18.,  $
  vmax_redf:18.,  $
  c_decade_redf:0.25,  $
  vmax_redf_def:18.,  $
  c_decade_redf_def:0.25,  $
  nsmooth_pts_redf:5, $
  nsmooth_pts_redf_def:5,  $
  cntrs_redf:0,  $
  F_integ:0, $
  indxF:0,  $
  Poisson_save:0,  $
  univgmask:0 ,  $
  mag_3s_kp:0,   $
  wchmagdat:['nomag','3sec mag','kp mag'],  $
  detslct_list:['012345','x1x345','01x345','012x45','01xx45','x12x45',$
    'x1xx45','x12x4x','x1xx4x'],  $
  detslct_list_indx:0,   $
  plotf:0,  $
  nplotf:0, $
  iplotf:0,  $
  strlsunmask:0, $
  strlphsun:[174.0,186.0],  $
  
  relgain_nmldet:0, $
  patch_include:1+intarr(6,16,8), $
  lchan:0l,  $
  lchan_old:0l,$
  
  relgain:fltarr(6), $
  relgain_old:fltarr(6), $
  chng_gain_det:0. ,$
  rel_gain_incre:0.5, $
  
  vpot:0. ,$
  vpot_old:0. , $
  vpot_incre:0.5e8, $
  
  veis_on:1,  $
  lzwfrmt:0  $
  
  }

;endif

iseest={tpoint1:0.d}


end

