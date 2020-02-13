;============================= select_pltvar =================================

pro select_pltvar,event

common sharewidg,WDGT
common shared,d
common wstuff,wst
common swestuff,swest
;common strahl2stuff,strahlflnm,strahldat
common share_strlslct,availsteps,availenrgy
common wavestnrstuff,wavestnrflnm,wavestnrspctrm
common newstrl_stuff,pad_list_curr ; NEW!! MPH (Last Modified: 08/20/03)

if keyword_set(wst.yesdata) eq 0 then return

;creates widget to select plot variables

w=where(d.pnlsel) ne -1 & if w(0) eq -1 then return
d.pnlsel=intarr(n_elements(d.pnlist.list))-1
WDGT.base_select_pltvar=WIDGET_BASE(TITLE = 'Available data', /column)
rbase=widget_base(WDGT.base_select_pltvar,/row)

cbase=widget_base(rbase,/row)
WDGT.doplot=cw_bgroup(cbase,label_top='Current selection',$
  column=1,/return_name,$
  ['Do plot','Cancel'])

cbase=widget_base(rbase,/row)
WDGT.preselected_pltvars=cw_bgroup(cbase,column=5,/return_name,$
  label_top='Pre-selected sets of data variables',$
  ['Save 0','Plot 0',$
   'Save 1','Plot 1',$
   'Save 2','Plot 2',$
   'Save 3','Plot 3',$
   'Show'])
  
rbase=widget_base(WDGT.base_select_pltvar,/row)
wst.yesdata=0
for idatyp=0,n_elements(d.flnm)-1 do begin         
  if d.flnm(idatyp) ne '' then begin
     case d.witype(idatyp) of
     0 : begin
         cbase1=widget_base(rbase,/column,space=25)
         WDGT.button(d.bttn(idatyp))=cw_bgroup(cbase1,$
           d.pnlist.list(where(d.pnlist.dtp eq d.datype(idatyp))),$
           /column,/nonexclusive,/scroll,y_scroll_size=d.yscroll(idatyp),$
           x_scroll_size=d.xscroll(idatyp),$
           label_top=d.wilabl(idatyp),uvalue=d.datype(idatyp))
         ;widget_control,WDGT.button(d.bttn(idatyp)),$
         ;  set_value=[1,1,1,1,1,1,0,0,1,1,1,0,1,0,0,1,1]  
         ;stop  
         wst.yesdata=1
         
         endcase
     
     1 : begin
         cbase1=widget_base(rbase,/column,space=25)
         WDGT.button(d.bttn(idatyp))=cw_bgroup(cbase1,$
         d.pnlist.list(where(d.pnlist.dtp eq d.datype(idatyp))),$
            label_top=d.wilabl(idatyp),uvalue=d.datype(idatyp))
         wst.yesdata=1
         endcase
         
     2 : begin
         cbase1=widget_base(rbase,/column,space=25)
         WDGT.button(d.bttn(idatyp))=cw_bgroup(cbase1,$
           d.pnlist.list(where(d.pnlist.dtp eq d.datype(idatyp))),$
           /column,/nonexclusive,/scroll,y_scroll_size=d.yscroll(idatyp),$
           x_scroll_size=d.xscroll(idatyp),$
           label_top=d.wilabl(idatyp),uvalue=d.datype(idatyp))
         wst.yesdata=1          
         endcase
     endcase             
           
     if d.datype(idatyp) eq 'swe_strahl' then begin
       ;display the available strahl energy steps
       mn=min(d.swe_strahldat.enstep,max=mx)
       wmn=where(d.swe_strahldat.enstep eq mn)
       wmx=where(d.swe_strahldat.enstep eq mx)
       wh=wmn(0) > wmx(0:1)
       availsteps=d.swe_strahldat(wh(0):wh(1)).enstep
       availenrgy=long(volt_en_strl(availsteps,/en))             
       WDGT.select_strahl_energy=cw_bgroup(cbase1,$
         string(availenrgy,format='(i5)'),row=8,/return_name,$
         label_top='Select strahl energy (eV)')
         swest.strlstep_slct=swest.strlstep_slct_lst  ;initialize
     endif
           
     if d.datype(idatyp) eq 'wavestnr' or $
     d.datype(idatyp) eq 'waveshrtnr' then begin             
        cbase=widget_base(cbase1,/column,/frame)
         WDGT.waves_min_db=cw_field(cbase,title='min dB',$
           /return_events,/integer,xsize=2)
         WDGT.waves_max_db=cw_field(cbase,title='max dB',$
           /return_events,/integer,xsize=2)

         cbase=widget_base(cbase1,/column,/frame)
         WDGT.waves_min_fhz=cw_field(cbase,title='min fHz',$
            /return_events,/integer,xsize=3)
         WDGT.waves_max_fhz=cw_field(cbase,title='max fHz',$
            /return_events,/integer,xsize=3)

         cbase=widget_base(cbase1,/column)
         WDGT.waves_reset=cw_bgroup(cbase,label_top=' ',' Reset ',/return_name)

         widget_control,WDGT.waves_min_db,set_value=long(wst.scale1)
         widget_control,WDGT.waves_max_db,set_value=long(wst.scale2)
         widget_control,WDGT.waves_min_fhz,set_value=long(wst.fhz1)
         widget_control,WDGT.waves_max_fhz,set_value=long(wst.fhz2)
      endif

     if d.datype(idatyp) eq 'fparaperp' then begin
        cbase=widget_base(cbase1,/column,/frame)
        WDGT.fparaperp_max_vel=cw_field(cbase,title='max vel',$
           /return_events,/string,xsize=2)
        WDGT.fparaperp_cntrs_offon= widget_droplist(cbase,title='Contours?',$
           value=wst.offon)
        widget_control,WDGT.fparaperp_cntrs_offon,$
          set_droplist_select=swest.cntrs_redf  
        WDGT.fparaperp_cntr_dec=cw_field(cbase,title='cntr_decade',$
           /return_events,/string,xsize=4) 
        WDGT.fparaperp_smooth=cw_field(cbase,title='median smooth # pts',$
           /return_events,/string,xsize=2) 
        widget_control,WDGT.fparaperp_max_vel,$
           set_value=string(swest.vmax_redf,format='(i2)')
        widget_control,WDGT.fparaperp_cntr_dec,$
           set_value=string(swest.c_decade_redf,format='(f4.2)') 
        widget_control,WDGT.fparaperp_smooth,$
           set_value=string(swest.nsmooth_pts_redf,format='(i2)')   
           cbase=widget_base(cbase1,/column)
        WDGT.fparaperp_reset=cw_bgroup(cbase,label_top=' ',' Reset ',$
           /return_name)  
      endif

      ; NEW!! MPH (Last Modified: 08/20/03) ------------------------------
      if (d.datype[idatyp] eq 'swe_newstrl') then begin
         cbase = Widget_BASE(cbase1,Column=1,Frame=1) ;       Main sub-base.
         pad = pitch_angle_descriptors() ; Get list of pitch-angle descript.
         empty_val = 'empty' & pad_list_curr = empty_val ;   Init. to empty.
         WDGT.newstrl_pad = CW_BGROUP(cbase,pad,Column=1,/NonExclusive,$
                                      /Scroll,Y_Scroll_Size=175,$
                                              X_Scroll_Size=150,$
                                      /Return_Name,Label_Top=$
                                      'Pitch-angle Dist. Descriptors')
      endif
      ; end of: NEW!! MPH--------------------------------------------------

   endif
endfor
        
WIDGET_CONTROL, WDGT.base_select_pltvar, /REALIZE
XMANAGER, 'select_pltvar', WDGT.base_select_pltvar

end


