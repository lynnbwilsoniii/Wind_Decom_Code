;==================== preselections ==========================================

pro preselections,preselflnm,$
  plot=plotpresel,save=savepresel,show=showpresel,widgt=widgt

common sharewidg,WDGT
common shared,d

if keyword_set(plotpresel) ne 0 then begin
            restore,getenv('IDLSAV')+preselflnm
            wtyp=where(d.flnm ne '')
            for i=0,n_elements(dtp_last)-1 do begin
              w=where(dtp_last(i) eq d.datype(wtyp))
              if w(0) eq -1 then begin
                 print,'Selected data type ',dtp_last,' not available'
                 return
              endif
            endfor
            if keyword_set(pnlsel_last) eq 0 then return
            d.pnlsel=pnlsel_last
            d.pnlist.ev_val=ev_val_last
            wv=where(d.pnlsel ge 0,nwv)                ;nwv=first non-zero elem
            if wv(0) eq -1 then return
            d.pnl(d.pnlsel(wv)).varname=d.pnlist.list(d.pnlsel(wv))
            d.pnl(d.pnlsel(wv)).dtp=d.pnlist.dtp(d.pnlsel(wv))
            d.pnl(d.pnlsel(wv)).ev_val=d.pnlist.ev_val(d.pnlsel(wv))
            print,'d.pnlist.list(d.pnlsel(wv)) ',$
                   d.pnlist.list(d.pnlsel(wv))
            w=where(d.pnlsel ge 0,nw)   ;set of selected indices of pnlist 
             
            if nw eq 0 then return
            print,'Do plot: pnlsel ',d.pnlsel(w)
            mstruct             ;puts selected data variables into structure pm
            if keyword_set(widgt) ne 0 then WIDGET_CONTROL, widgt, /DESTROY
            print,'Selected plot variables:' 
            print,'d.pnl(d.pnlsel).labl ',d.pnl(d.pnlsel(w)).labl    
            wset,WDGT.win_main
            erase
            plt
endif 

if keyword_set(showpresel) ne 0 then begin
         restore,getenv('IDLSAV')+preselflnm
         if keyword_set(labl_last) ne 0 then $
           print,'set ',preselflnm,'  ',labl_last        
endif

if keyword_set(savepresel) ne 0 then begin
       mstruct             ;puts selected data variables into structure pm
       pnlsel_last=d.pnlsel
       ev_val_last=d.pnlist.ev_val
       w=where(d.pnlsel ge 0,nw)
       if w(0) eq -1 then return
       labl_last=d.pnl(d.pnlsel(w)).labl
       dtp_last=d.pnlist.dtp(d.pnlsel(w))
       save,pnlsel_last,ev_val_last,labl_last,dtp_last,$
         filename=getenv('IDLSAV')+preselflnm
         ;filename=getenv('SWEDATLIB')+preselflnm
         
endif


end




;======================select_pltvar_event ===================================

pro select_pltvar_event,event

common sharewidg,WDGT
common shared,d
common wstuff,wst
common swestuff,swest
common share_strlslct,availsteps,availenrgy
common wavestnrstuff,wavestnrflnm,wavestnrspctrm


;------- begin section to store pointers to selected data variable names ------

help,event,/str

case event.id of 

  WDGT.doplot: case (event.value) of
    'Do plot': begin                     ;plot the selected data
       w=where(d.pnlsel ge 0,nw)   ;set of selected indices of pnlist  
       if nw eq 0 then return
       print,'Do plot: pnlsel ',d.pnlsel(w)
       mstruct                ;puts selected data variables into structure pm
       WIDGET_CONTROL, event.top, /DESTROY
       print,'Selected plot variables:' 
       print,'d.pnl(d.pnlsel).labl ',d.pnl(d.pnlsel(w)).labl    
       wset,WDGT.win_main
       erase
       plt
    endcase

    'Cancel': WIDGET_CONTROL, WDGT.base_select_pltvar, /DESTROY

  endcase

  WDGT.preselected_pltvars: case (event.value) of

   'Save 0' : preselections,'selection0',/save
   'Save 1' : preselections,'selection1',/save  
   'Save 2' : preselections,'selection2',/save 
   'Save 3' : preselections,'selection3',/save

   'Plot 0': preselections,'selection0',/plot,widgt=event.top
   'Plot 1': preselections,'selection1',/plot,widgt=event.top
   'Plot 2': preselections,'selection2',/plot,widgt=event.top
   'Plot 3': preselections,'selection3',/plot,widgt=event.top

   'Show': begin
             preselections,'selection0',/show
             preselections,'selection1',/show
             preselections,'selection2',/show 
             preselections,'selection3',/show
           endcase

    else: 
  endcase

  WDGT.waves_min_db: wst.scale1=$
  wavestnrspctrm.dbmin > float(event.value) < wavestnrspctrm.dbmax
  
  WDGT.waves_max_db: wst.scale2=$
  wavestnrspctrm.dbmin > float(event.value) < wavestnrspctrm.dbmax
  
  WDGT.waves_min_fhz: wst.fhz1=$
  wavestnrspctrm.fhzmin > float(event.value) < wavestnrspctrm.fhzmax
  
  WDGT.waves_max_fhz: wst.fhz2=$
  wavestnrspctrm.fhzmin > float(event.value) < wavestnrspctrm.fhzmax 
  
  WDGT.waves_reset: begin
     case event.value of
     ' Reset ': begin
        widget_control,WDGT.waves_min_db,set_value=long(wst.scale1_def)
        widget_control,WDGT.waves_max_db,set_value=long(wst.scale2_def)
        widget_control,WDGT.waves_min_fhz,set_value=long(wst.fhz1_def)
        widget_control,WDGT.waves_max_fhz,set_value=long(wst.fhz2_def)
        wst.scale1=wst.scale1_def
        wst.scale2=wst.scale2_def
        wst.fhz1=wst.fhz1_def 
        wst.fhz2=wst.fhz2_def
              endcase
     endcase
                 endcase
  
  WDGT.fparaperp_max_vel :swest.vmax_redf=float(event.value)
  
  WDGT.fparaperp_cntr_dec :swest.c_decade_redf=float(event.value)
  
  WDGT.fparaperp_smooth :begin
    swest.nsmooth_pts_redf=long(event.value)
    print,'swest.nsmooth_pts_redf ',swest.nsmooth_pts_redf
                endcase
  
  WDGT.fparaperp_reset: begin
     case event.value of
     ' Reset ': begin
        widget_control,WDGT.fparaperp_max_vel,$
          set_value=string(swest.vmax_redf_def,format='(i2)')
        widget_control,WDGT.fparaperp_cntr_dec,$
          set_value=string(swest.c_decade_redf_def,format='(f4.2)')
        widget_control,WDGT.fparaperp_smooth,$
          set_value=string(swest.nsmooth_pts_redf,format='(i2)')  
        swest.vmax_redf=swest.vmax_redf1_def
        swest.c_decade_redf=swest.c_decade_redf_def
        swest.nsmooth_pts_redf=swest.nsmooth_pts_redf_def
              endcase
     endcase
                 endcase
                         
  WDGT.fparaperp_cntrs_offon : if wst.offon(event.index) eq 'On' then $
                  swest.cntrs_redf=1 else swest.cntrs_redf=0
                                          
  WDGT.select_strahl_energy: begin
     strlstep_slct=availsteps(where(availenrgy eq event.value))
     swest.strlstep_slct=strlstep_slct(0)
     print,'strahl step, energy ',$
      swest.strlstep_slct,volt_en_strl(swest.strlstep_slct,/en)
     swest.strlstep_slct_lst=swest.strlstep_slct
                endcase
  else : begin
            print,'event.value ',event.value
            wv=where(d.pnlsel ge 0,nwv)                ;nwv=first non-zero elem
            wi=where(WDGT.button(d.bttn) eq event.id)    ;selected data type
            d.pnlsel(nwv)=d.pnlist.offs(wi)+event.value ;selected variable
            d.pnlist.ev_val(d.pnlsel(nwv))=event.value
            d.pnl(d.pnlsel(nwv)).varname=d.pnlist.list(d.pnlsel(nwv))
            d.pnl(d.pnlsel(nwv)).dtp=d.pnlist.dtp(d.pnlsel(nwv))
            d.pnl(d.pnlsel(nwv)).ev_val=d.pnlist.ev_val(d.pnlsel(nwv))
            print,'wi,d.pnlist.list(d.pnlsel(nwv)) ',$
                   wi,d.pnlist.list(d.pnlsel(nwv)) 
                              
         endcase                       
endcase
 


end
