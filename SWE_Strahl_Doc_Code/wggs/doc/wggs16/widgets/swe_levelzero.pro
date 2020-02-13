
;========================== swe_levelzero_event,event =========================

pro swe_levelzero_event,event

common sharewidg,WDGT
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common drawf,pltype
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common shared,d
common sharefevents,fevents
common mouse_ev,xrange_redf,xsize_redf

;print,'swe_levelzero_event :',event.id,event.value

case getenv('SWE_LZPATH') eq '' of
0 : 
1 : begin
      if event.id ne WDGT.swelz_open and event.id ne WDGT.swelz_spectra $
        then return
      if (event.id eq WDGT.swelz_open or event.id eq WDGT.swelz_spectra) $
          and event.value ne 'Quit' and $
          event.value ne 'Parent' then return
      if event.id eq WDGT.swelz_draw1 and keyword_set(fh) eq 0 then return
    endcase
endcase


CASE event.id of

  WDGT.swelz_draw1 : begin   ;plot in first window
    if keyword_set(recn) eq 0 or event.type eq 1 then return 
    case event.press of
    1: begin
         if swest.strlf_veisf eq 1 then strlf_veisf,wid=swest.lzwin(0) $
         else begin
           if swest.veis_on then $
             proc_fw,wid=swest.lzwin(0),pltype=pltype, err=err,$
             F_integ=swest.F_integ $
           else strl_newmode,wid=swest.lzwin(0)  
           swest.win_pltype(0:n_elements(pltype)-1,0)=pltype
           swest.win_npltypes(0)=n_elements(pltype)
           swest.win_delete(0)=swest.delete
           swest.win_hidepoints(0)=swest.hidepoints
         endelse
       endcase
    2: begin
         print,'Middle mouse button'
         print,'event.x,event.y ',event.x,event.y
         x_redf=xrange_redf(0)+(float(event.x)-xsize_redf(0))*$
           (xrange_redf(1)-xrange_redf(0)) /$
           (xsize_redf(1)-xsize_redf(0))  
         proc_fw,wid=swest.lzwin(0),pltype=pltype,lsscn=1,vparc=x_redf
         print,'x_redf ',x_redf         
       endcase   
    else:
    endcase      
               endcase

  WDGT.swelz_draw2 : begin        ;plot in second window
    if keyword_set(recn) eq 0 then return
    case event.press of
    1: begin
         if swest.strlf_veisf eq 1 then strlf_veisf,wid=swest.lzwin(1) $
         else begin
             proc_fw,wid=swest.lzwin(1),pltype=pltype, err=err,$
             F_integ=swest.F_integ 
           swest.win_pltype(0:n_elements(pltype)-1,1)=pltype
           swest.win_npltypes(1)=n_elements(pltype)
           swest.win_delete(1)=swest.delete
           swest.win_hidepoints(1)=swest.hidepoints
         endelse
       endcase
    2: begin
         print,'Middle mouse button'
         print,'event.x,event.y ',event.x,event.y
         x_redf=xrange_redf(0)+(float(event.x)-xsize_redf(0))*$
           (xrange_redf(1)-xrange_redf(0)) /$
           (xsize_redf(1)-xsize_redf(0))      
         proc_fw,wid=swest.lzwin(1),pltype=pltype,lsscn=1,vparc=x_redf         
       endcase        
    else:
    endcase 
               endcase


  WDGT.swelz_draw3 : begin      ;plot in third window
    if keyword_set(recn) eq 0 then return
    case event.press of
    1: begin
         if swest.strlf_veisf eq 1 then strlf_veisf,wid=swest.lzwin(2) $
         else begin
             proc_fw,wid=swest.lzwin(2),pltype=pltype, err=err,$
             F_integ=swest.F_integ 
           swest.win_pltype(0:n_elements(pltype)-1,2)=pltype
           swest.win_npltypes(2)=n_elements(pltype)
           swest.win_delete(2)=swest.delete
           swest.win_hidepoints(2)=swest.hidepoints
         endelse
       endcase
    2: begin
         print,'Middle mouse button'
         print,'event.x,event.y ',event.x,event.y
         x_redf=xrange_redf(0)+(float(event.x)-xsize_redf(0))*$
           (xrange_redf(1)-xrange_redf(0)) /$
           (xsize_redf(1)-xsize_redf(0))    
         proc_fw,wid=swest.lzwin(2),pltype=pltype,lsscn=1,vparc=x_redf         
       endcase        
    else:
    endcase 
               endcase

  WDGT.swelz_fpltype : begin
    widget_control,WDGT.swelz_fpltype,get_uvalue=list
    case list(event.index) of
      'fcntrs': pltype=[0]
      'f(en,pa) img' : pltype=[10]
      'fcuts' : pltype=[2]
      'redF' : pltype=[1]
      'f fcuts' : pltype=[0,2]
      'f F' :  pltype=[0,1]
      'F fcuts' : pltype=[1,2]
      'fsurface' : pltype=[3]
      'triangles': pltype=[4,1]
      'fpolar':pltype=[12]       
      'f F fcuts' : pltype=[0,1,2]
      'f(en,pa) ctr' : pltype=[11]

   ;    Reacts to the 'F,f time series' selection by gen. MODAL dialog window.
  'F,f time series' : begin ; *********************************************New
     ; This block of code is exactly what happens when the 'hhmmss' field
     ;  generates an event (user presses return while field is selected).
     Widget_CONTROL,WDGT.swelz_hmsfld,Get_Value=val
     Widget_CONTROL,WDGT.swelz_hmsfld,Set_Value=val ; Improves interface.
     ;convert string hhmmss to long hhmmss
     hhmmss=long(val) & wst.xydata(0)=hms_hour(hhmmss)
     hour_hms,wst.xydata(0),hms,lhms=lhms & wst.timsel='lztm'
     wst.hms=hms(0) & wst.pb5=ymd_pb5(long(wst.lzdate))
     wst.pb5(2)=long(wst.xydata(0)*3600000.d)
     print,'WDGT.swelz_hmsfld: selected time ',$
       wst.xydata(0),'  ',wst.hms,'  ',wst.pb5 & wst.lz_is_read=0
     ; End of block of code for emulating 'hhmmss' field event.
     
     Widget_CONTROL,WDGT.swelz_hmsfld,Get_Value=time_begin ; Get begin time.

     if (time_begin(0) ne '') then begin ; If some time has been selected...
        minutes = 5 ; Default num. mins. to displ. (starting at beg. time).
        vdist_tser,string(time_begin),fix(minutes) ; Call MODAL dialog window.
     endif ;                               ...Otherwise do nothing.

     pltype = [0,2] & Widget_CONTROL,WDGT.swelz_fpltype,$ ; Reset f plot type.
                            Set_Droplist_Select=(where(list eq 'f fcuts'))[0]
  endcase

      else : pltype=[0,2] ; Default
    endcase
    print,'pltype ',pltype
    swest.pltype_index=event.index
    if (swest.veis_on eq 0) then strl_newmode,wid=swest.lzwin(0)
                endcase

  WDGT.swelz_recincre : begin  ;increment f data rec number
    case event.value of
      ' + ' : recn=recn+1
      ' - ' : recn=recn-1
      else:
    endcase
    widget_control,WDGT.swelz_spinfld,set_value=swest.ispinbl
    widget_control,WDGT.swelz_recfld,set_value=recn    
    wst.timsel='lz'
    wst.lz_is_read=0
          
    if swest.autoseq gt 0 then begin
                      swest.ilzplt=swest.ilzplt+1
                      case swest.autoseq of
                      1: begin
                        pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                        if swest.veis_on then $
                          proc_fw,wid=pltwin,pltype=pltype $
                        else strl_newmode,wid=1
                         endcase
                      2: begin
                           pltwin=swest.ilzplt-2*fix(swest.ilzplt/2) + 2
                           proc_fw,wid=pltwin,pltype=pltype 
                         endcase
                      3: begin
                          proc_fw,wid=2,pltype=pltype
                          proc_fw,wid=3,pltype=pltype
                         endcase
                      endcase                       
    endif                    
  endcase

  WDGT.swelz_spinincre : begin     ;increment f data spin number
    ;print,'event.value ',event.value

    ;NOTE!! The following test compares swest.ispinbl with
    ;the CURRENT record's vsmjf.n_spins which, when record is
    ;also incremented, may cause the test to skip the last spin
    ;in a record which happens to have a number of spins
    ;greater than the current vsmjf.n_spins, 
    ;for example when there are 8 spins in mode2
    
    case event.value of
      'auto': if swest.veis_on then wst.auto_spin=1 else wst.auto_spin=0 
      else:
    endcase 
    if wst.auto_spin then event.value=' + '
      
    case event.value of
      ' + ' : begin
                  if wst.auto_spin then nauto=3 else nauto=1
                  for iauto=0,nauto-1 do begin
                    swest.ispinbl=swest.ispinbl+1
                    if (swest.veis_on eq 0) then spins = vsmjf.n_strspects $
                       else spins = vsmjf.n_spins ;     Set # spins per record.
                    if swest.ispinbl gt spins-1 then begin
                       swest.ispinbl=0 
                       if recn lt fh.nmf-1 then recn=recn+1
                       wst.lz_is_read=0
                    endif
                    widget_control,WDGT.swelz_spinfld,set_value=swest.ispinbl
                    widget_control,WDGT.swelz_recfld,set_value=recn
                    wst.timsel='lz'
                    print,'WDGT.swelz_spinincre ',$
                         event.value, swest.ispinbl,wst.timsel
                    
                    if swest.autoseq gt 0 then begin
                      swest.ilzplt=swest.ilzplt+1
                      case swest.autoseq of
                      1: begin
                        pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                        if swest.veis_on then $
                          proc_fw,wid=pltwin,pltype=pltype $
                        else strl_newmode,wid=1
                         endcase
                      2: begin
                         pltwin=swest.ilzplt-2*fix(swest.ilzplt/2) + 2
                          proc_fw,wid=pltwin,pltype=pltype  
                         endcase
                      3: begin
                          proc_fw,wid=2,pltype=pltype
                          proc_fw,wid=3,pltype=pltype
                         endcase
                      endcase                       
                    endif
                  endfor
                  wst.auto_spin=0  
                endcase

      ' - ' : begin
                    swest.ispinbl=swest.ispinbl-1
                    if swest.ispinbl lt 0 then begin
                       if (swest.veis_on eq 0) then $ ;          If VEIS off...
                          spins = vsmjf.n_strspects $ ;   # spectra per record.
                       else spins = vsmjf.n_spins ;     Set # spins per record.

                       swest.ispinbl=spins-1 
                       if recn gt 0 then recn=recn-1
                       wst.lz_is_read=0
                    endif
                    widget_control,WDGT.swelz_spinfld,set_value=swest.ispinbl
                    widget_control,WDGT.swelz_recfld,set_value=recn
                    wst.timsel='lz'
                    
                    if swest.autoseq gt 0 then begin
                      swest.ilzplt=swest.ilzplt+1
                      case swest.autoseq of
                      1: begin
                        pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                        if swest.veis_on then $
                          proc_fw,wid=pltwin,pltype=pltype  $
                        else  strl_newmode,wid=1
                         endcase
                      2: begin
                         pltwin=swest.ilzplt-2*fix(swest.ilzplt/2) + 2
                         proc_fw,wid=pltwin,pltype=pltype 
                         endcase
                      3: begin
                          proc_fw,wid=2,pltype=pltype
                          proc_fw,wid=3,pltype=pltype
                         endcase
                      endcase
                       
                    endif
                    
                 endcase
    else:
    endcase
                endcase
  
  WDGT.swelz_days: wst.lzindate=wst.lzdays(event.index) 
  
  WDGT.swelz_spectra : begin
    if keyword_set(recn) eq 0 then begin
           recn=1 & swest.ispinbl=0 & endif
    case event.value of
       'Veis data' : begin
                     get_recnspin            ;get recn and spin number
                     plotcounts
                   endcase
                   
      'Strahl data' : plcnts_strl
       
      else :
    endcase
                endcase
 
  WDGT.swelz_open : begin
    if keyword_set(recn) eq 0 then begin
           recn=1 & swest.ispinbl=0 & endif
    case event.value of
      'Open LZ' : begin           
          swest.spn=0 & swest.ispinbl=0
          lzinput
          if swest.lzwfrmt eq 0 and swest.veis_on eq 0 then begin
            WIDGET_CONTROL, event.top, /DESTROY
            swest.lzwfrmt = 1 & swest.pltype_index = 0 ; Default style
            swe_levelzero
          endif  
          if swest.lzwfrmt eq 1 and swest.veis_on eq 1 then begin
            WIDGET_CONTROL, event.top, /DESTROY
            swest.lzwfrmt = 0 & pltype = 0 ; Default type
            swe_levelzero
          endif  
          widget_control,WDGT.swelz_spectra,sensitive=swest.veis_on
          widget_control,WDGT.swelz_sav,sensitive=swest.veis_on
          widget_control,WDGT.swelz_fevents,sensitive=swest.veis_on
          widget_control,WDGT.swelz_vstps,sensitive=swest.veis_on
          widget_control,WDGT.swelz_pbin,sensitive=swest.veis_on
          
          if keyword_set(fh) eq 0 then return

          wst.xydata(0)=double(fh.fst.ms)/3600000.d
          hour_hms,wst.xydata(0),hms,lhms=lhms
          wst.hms=hms(0)
          wst.pb5=[fh.fst.yr,fh.fst.dy,fh.fst.ms]
          wst.timsel='lztm'
          print,'initial selected time ',$
            wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
          widget_control,WDGT.swelz_hmsfld,set_value=lhms

          wst.indate=wst.lzdate
          WIDGET_CONTROL, WDGT.input_ymd, set_value = wst.indate
          widget_control,WDGT.swelz_mode,$
            set_value=string(vsmjf.scimode,format='(i1)')
                   endcase       
      else :
    endcase
                endcase
 
  WDGT.swelz_quit : begin
    case event.value of
      'Parent' : begin
                   wset,WDGT.win_main
                   wst.xyrange=wst.xyrange_mainnwin
                   wst.xywindow=wst.xywindow_mainnwin
                   wst.xysize=wst.xysize_mainnwin
                   wst.ylog=wst.ylog_mainnwin
                   WIDGET_CONTROL, WDGT.base_main, /show
                 endcase
      'Quit' :  WIDGET_CONTROL, event.top, /DESTROY
      'HELP' : $
        xdisplayfile,getenv('WGGSBASE')+$
        'swelz/help_swe_lz.txt',width=100
    endcase
                    endcase

  WDGT.swelz_clrs : begin
    widget_control,WDGT.swelz_clrs,get_uvalue=list
    case list(event.index) of
      'Restore colors' : begin
                            clrtbl_indx
                            if (swest.veis_on eq 0) then $
                               strl_newmode,wid=swest.lzwin(0)
                         endcase
      'Color tbl' : xloadct
    endcase
                   endcase

 WDGT.swelz_hcpy : begin
    widget_control,WDGT.swelz_hcpy,get_uvalue=list
    case list(event.index) of
      'Hrdcpy_bw' : begin
                 wst.hardcopy=1
                 wst.printer=wst.printer_bw
                 wst.print_flnm=wst.print_flnm_bw
                 wst.print_cmd=wst.print_cmd_bw 
                 clrtbl_indx,/hardcopy
                    endcase
      'Hrdcpy_clr' : begin
                 wst.hardcopy=1
                 wst.printer=wst.printer_clr
                 wst.print_flnm=wst.print_flnm_clr
                 wst.print_cmd=wst.print_cmd_clr
                 clrtbl_indx,/hardcopy
                     endcase
      'Save TIFF'  : begin
                 wst.hardcopy=1
                 strl_newmode,wid=swest.lzwin(0)
                     endcase
    endcase
                  endcase 
 
  WDGT.swelz_recfld : begin
     WIDGET_CONTROL, WDGT.swelz_recfld, GET_VALUE = val
     recn=val(0)
     wst.timsel='lz'
     print,'WDGT.swelz_recfld: selected recn ',recn
     wst.lz_is_read=0
                 endcase

 WDGT.swelz_spinfld : begin
     WIDGET_CONTROL, WDGT.swelz_spinfld, GET_VALUE = val
     swest.ispinbl=val(0)
     wst.timsel='lz'
     print,'WDGT.swelz_spinfld: selected spinbl ',swest.ispinbl
     wst.lz_is_read=0
                 endcase

 WDGT.swelz_hmsfld : begin
     WIDGET_CONTROL, WDGT.swelz_hmsfld, GET_VALUE = val
     Widget_CONTROL,WDGT.swelz_hmsfld,Set_Value=val ; Improves interface.
     ;convert string hhmmss to long hhmmss
     hhmmss=long(val)
     wst.xydata(0)=hms_hour(hhmmss)
     hour_hms,wst.xydata(0),hms,lhms=lhms
     wst.timsel='lztm'
     wst.hms=hms(0)
     wst.pb5=ymd_pb5(long(wst.lzdate))
     wst.pb5(2)=long(wst.xydata(0)*3600000.d)
     print,'WDGT.swelz_hmsfld: selected time ',$
       wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
     wst.lz_is_read=0
                 endcase

 WDGT.swelz_pnlsel : begin
                      swest.lzsurvey=event.index
                      wset,swest.win(0) & erase
                      plt,/lzwin
                  endcase     
                 
  WDGT.swelz_orbit : begin
    widget_control,WDGT.swelz_orbit,get_uvalue=list
    case list(event.index) of
      'Orbit' : orbit_plot,event
      'Display' : lzdspl_opt
      'Fshck geom' : $
        if wst.foreshock eq 0 then wst.foreshock=1 else wst.foreshock=0
      'Scan LZ file' : swelzw 
    endcase                 
                   endcase  

  WDGT.swelz_pbin : swest.pbinselect=fix(swest.pbins(event.index)) 


  WDGT.swelz_vstps : swest.nvmax=fix(swest.nvmaxlist(event.index))
  
  WDGT.swelz_sav : begin
    widget_control,WDGT.swelz_sav,get_uvalue=list
    case list(event.index) of
      'contours & cuts' : swest.savez=1
      'measured GSE seq' : save_f 
    endcase
   
    ;case event.value of
    ;'Save f' : begin
    ;  ;save_f         ;an old application that can be brought back
    ;  swest.savez=1
    ;           endcase
    ;endcase
    
  endcase            

  WDGT.swelz_fevents : begin
  widget_control,WDGT.swelz_fevents,get_uvalue=list
    case list(event.index) of
      'Event' : f_events  
      'Seq' : plotf_survey_seq
    endcase  
  endcase
  
  WDGT.swelz_special_appl : call_procedure,getenv('SPECIAL_APPL_SWELZ')
  
   ; Reacts to the 'F,f time series' button by generating MODAL dialog window.
  WDGT.swelz_vdist : begin ; *********************************************New
     ; This block of code is exactly what happens when the 'hhmmss' field
     ;  generates an event (user presses return while field is selected).
     Widget_CONTROL,WDGT.swelz_hmsfld,Get_Value=val
     Widget_CONTROL,WDGT.swelz_hmsfld,Set_Value=val ; Improves interface.
     ;convert string hhmmss to long hhmmss
     hhmmss=long(val) & wst.xydata(0)=hms_hour(hhmmss)
     hour_hms,wst.xydata(0),hms,lhms=lhms & wst.timsel='lztm'
     wst.hms=hms(0) & wst.pb5=ymd_pb5(long(wst.lzdate))
     wst.pb5(2)=long(wst.xydata(0)*3600000.d)
     print,'WDGT.swelz_hmsfld: selected time ',$
       wst.xydata(0),'  ',wst.hms,'  ',wst.pb5 & wst.lz_is_read=0
     ; End of block of code for emulating 'hhmmss' field event.
     
     Widget_CONTROL,WDGT.swelz_hmsfld,Get_Value=time_begin ; Get begin time.

     if (time_begin(0) ne '') then begin ; If some time has been selected...
        minutes = 5 ; Default num. mins. to displ. (starting at beg. time).
        vdist_tser,string(time_begin),fix(minutes) ; Call MODAL dialog window.
     endif ;                               ...Otherwise do nothing.
  endcase

          
  WDGT.swelz_drawsurvey : begin
    case event.press of
    1: begin
         draw_main_ev,event
           oplot_sec=pb5_sec(wst.pb5)
           wst.timsel='lztm'    
           if swest.autoseq gt 0 then begin
                      swest.ilzplt=swest.ilzplt+1
                      case swest.autoseq of
                      1: begin
                        pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                        if swest.veis_on then $
                          proc_fw,wid=pltwin,pltype=pltype $
                        else strl_newmode,wid=1;pltwin  
                         endcase
                      2: begin
                        pltwin=swest.ilzplt-2*fix(swest.ilzplt/2) + 2
                          proc_fw,wid=pltwin,pltype=pltype 
                         endcase
                      3: begin
                          proc_fw,wid=2,pltype=pltype
                          proc_fw,wid=3,pltype=pltype
                         endcase
                      endcase                       
           if wst.foreshock then foreshock_geometry                   
         endif
    endcase
    else:
    endcase
                endcase
    
  else:
endcase

end




;============================= swe_levelzero ====================================

pro swe_levelzero,group=group

common sharewidg,WDGT
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shared,d
common log_delog,comp_tbl,dcomp_tbl
common drawf,pltype
common wstuff,wst
common swestuff,swest


if keyword_set(pltype) eq 0 then begin
  pltype=[0,2] ;& swest.pltype_index=4
endif


if xregistered('swe_levelzero') then begin

   
     ;selected indices from set of indices d.pnlist.list
     ;   pnlsel=d.pnlsel(where(d.pnlsel ne -1))
     ;structure of plotting parameters for selected plot variables 
     ;   pm=d.pnl(pnlsel)
     ;   list=pm(*).labl 
     widget_control,WDGT.swelz_pnlsel,get_uvalue=list     
     widget_control,WDGT.swelz_pnlsel,set_value=list
     if total(d.pnlsel-d.pnlsel_last) ne 0 then swest.lzsurvey=0
     widget_control,WDGT.swelz_pnlsel,set_droplist_select=swest.lzsurvey
     WIDGET_CONTROL, WDGT.swelz_drawsurvey, GET_VALUE=windw
     wset,windw
     plt,/lzwin
   
   if keyword_set(fh) eq 0 then begin
     WIDGET_CONTROL, WDGT.swelz_base_main,iconify=0
     return
   endif
   
   if wst.xydata(0) eq 0 then begin
     wst.xydata(0)=double(fh.fst.ms)/3600000.d
     hour_hms,wst.xydata(0),hms,lhms=lhms
     wst.hms=hms(0)
     wst.pb5=[fh.fst.yr,fh.fst.dy,fh.fst.ms]
     wst.timsel='lztm'
     print,'initial selected time ',$
      wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
     widget_control,WDGT.swelz_hmsfld,set_value=lhms
   endif   

   widget_control,WDGT.swelz_fpltype,set_droplist_select=swest.pltype_index
   widget_control,WDGT.swelz_mode,set_value=string(vsmjf.scimode,format='(i1)')
   widget_control,WDGT.swelz_days,set_value=wst.lzdays(where(wst.lzdays ne ''))
   wlzdy=where(wst.lzdays eq wst.lzindate)  ;wst.surveydate
   widget_control,WDGT.swelz_days,set_droplist_select=wlzdy(0)
   wpbin=where(swest.pbins eq swest.pbinselect)
   widget_control,WDGT.swelz_pbin,set_droplist_select=wpbin(0)
     
  WIDGET_CONTROL, WDGT.swelz_base_main,/show
  return
endif
  
;get indices of instrument housekeeping into mjf array, lz.mf   
  if keyword_set(ihk) eq 0 then ihkmap,ihk 

;get tm map of science and genl hk data offsets into lz 
  mode1map 
  mode6map 
  mode2map

;get mode1 and mode2 sun phase angles of detectors, unit vectors, glint masks
   phasem2

print,'swe_levelzero: timsel ',wst.timsel


;------------------- set up main base widgets --------------------------------

WDGT.swelz_base_main = WIDGET_BASE(TITLE = 'SWE LZ Data Display', /COLUMN)  

rbase1=widget_base(WDGT.swelz_base_main,/row)

rbase=widget_base(rbase1,/row)
WDGT.swelz_quit=cw_bgroup(rbase,['Parent',$
                                'HELP',$
                                'Quit'],column=3,/return_name)
                           
WDGT.swelz_days=widget_droplist(rbase,$
  title=' ',value=wst.lzdays(where(wst.lzdays ne '')))
  
WDGT.swelz_open=cw_bgroup(rbase,[ 'Open LZ'],column=1,/return_name) 
                                  
WDGT.swelz_spectra=cw_bgroup(rbase,['Veis data','Strahl data'],$
   column=2,/return_name)    


          
list=['Color tbl',$
      'Restore colors']
WDGT.swelz_clrs=widget_droplist(rbase,title=' ',value=list,uvalue=list)

list=['Hrdcpy_bw',$
      'Hrdcpy_clr']
if (swest.veis_on eq 0) then list = ['Save TIFF'] ;      Redefine for new mode.
WDGT.swelz_hcpy=widget_droplist(rbase,title=' ',value=list,uvalue=list)

list=['Orbit',$
      'Display',$
      'Fshck geom',$
      'Scan LZ file']
if (swest.veis_on eq 0) then list = ['Orbit'] ;          Redefine for new mode.
WDGT.swelz_orbit=widget_droplist(rbase,title=' ',value=list,uvalue=list)


;WDGT.swelz_sav=cw_bgroup(rbase,['Save f'],row=1,label_left=' ',/return_name)

list=['contours & cuts','measured GSE seq']
WDGT.swelz_sav=widget_droplist(rbase,title='Save f',$
  value=list,uvalue=list)
      

;if getenv('SPECIAL_APPL_SWELZ') ne '' then begin
;  WDGT.swelz_special_appl=cw_bgroup(rbase,/row,/return_name,label_left=' ',$
;    ['Special Appl'])
;endif    
  
rbase2=widget_base(WDGT.swelz_base_main,/row)
           
rbase=widget_base(rbase2,/row) 
;selected indices from set of indices d.pnlist.list
   pnlsel=d.pnlsel(where(d.pnlsel ne -1))
;structure of plotting parameters for selected plot variables 
   pm=d.pnl(pnlsel)
   list=pm(*).labl
   WDGT.swelz_pnlsel=widget_droplist(rbase,title=' ',value=list,uvalue=list)
widget_control,WDGT.swelz_pnlsel,set_droplist_select=swest.lzsurvey

WDGT.swelz_mode=cw_field(rbase,title='mode',/noedit,/string,xsize=1,ysize=1,/row)

if swest.veis_on then $ ; Button group is defined differently if VEIS is off...
   WDGT.swelz_spinincre = cw_bgroup(rbase,[' + ',' - ','auto'],Row=1,$
                                    Label_Left=' Spin',/Return_Name) $ ;  VEIS.
else $ ;                   Note: Auto button is not appropriate if VEIS is off.
   WDGT.swelz_spinincre = cw_bgroup(rbase,[' + ',' - '],Row=1,$ ;      NO VEIS.
                                    Label_Left=' Spectrum',/Return_Name)

WDGT.swelz_spinfld=cw_field(rbase,title=' ',/return_events,/long,xsize=1,$
  ysize=1,/row)

WDGT.swelz_recincre=cw_bgroup(rbase,[' + ',' - '],row=1,$
  label_left=' Mfrm',/return_name)
WDGT.swelz_recfld=cw_field(rbase,title=' ',/return_events,/long,xsize=4,$
  ysize=1,/row)

WDGT.swelz_hmsfld=cw_field(rbase,title='hhmmss',/return_events,/string,xsize=6,$
  ysize=1,/row)

list=[ 'fcntrs',$
       'f(en,pa) ctr',$
       'f(en,pa) img', $
       'fcuts',$
       'redF',$
       'f fcuts',$
       'f F',$
       'F fcuts',$
       'fsurface',$
       'triangles',$
       'fpolar',$
       'F,f time series']

default_pltype_index = (where(list eq 'f fcuts'))[0] ;       Set default index.

if (swest.veis_on eq 0) then list = ['style 1','style 2'] ; Redef for new mode.

WDGT.swelz_fpltype=widget_droplist(rbase,title=' ',value=list,uvalue=list)


WDGT.swelz_vstps=widget_droplist(rbase,title='vstps',value=swest.nvmaxlist)
wnvmx=where(swest.nvmaxlist eq swest.nvmax)  
widget_control,WDGT.swelz_vstps,set_droplist_select=wnvmx(0)

WDGT.swelz_pbin=widget_droplist(rbase,title='pbin',value=swest.pbins)
wpbin=where(swest.pbins eq swest.pbinselect)
widget_control,WDGT.swelz_pbin,set_droplist_select=wpbin(0)

list=['Event','Seq']
WDGT.swelz_fevents=widget_droplist(rbase,title=' ',value=list,uvalue=list)
         
; Defines button which generates the modal dialog window for 'F,f time series'.
;WDGT.swelz_vdist = CW_BGROUP(rbase,Row=1,/Return_Name,Label_Left=' ',$
;                             ['F,f time series']) ; ***********************New
         

     WDGT.swelz_surveybase=widget_base(WDGT.swelz_base_main,/row,/FRAME)
     rbase0=WDGT.swelz_surveybase
     x_size = fix(getenv('XSIZE_LZSRVY'))  ;825  ;850  ;950            
     y_size = fix(getenv('YSIZE_LZSRVY'))  ;165  ;180  ;150 
     WDGT.swelz_drawsurvey = WIDGET_DRAW(rbase0,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size, YSIZE = y_size)

  x_size =fix(getenv('XSIZE_LZ'))   ;1075  ;1100;1000
  y_size= fix(getenv('YSIZE_LZ'))   ;565  ;580
  swest.win_xsize(0)=x_size
  swest.win_ysize(0)=y_size
  
  rbase0=widget_base(WDGT.swelz_base_main,/row)
  cbase1=widget_base(rbase0,/column)
  if swest.lzwfrmt eq 0 then $
  WDGT.swelz_draw1 = WIDGET_DRAW(cbase1,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size) $
  else $
  WDGT.swelz_draw1 = WIDGET_DRAW(cbase1,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size, YSIZE = y_size)      

  cbase2=widget_base(rbase0,/column)
  if swest.lzwfrmt eq 0 then $
  WDGT.swelz_draw2 = WIDGET_DRAW(cbase2,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size)

  cbase3=widget_base(rbase0,/column)
  if swest.lzwfrmt eq 0 then $
  WDGT.swelz_draw3 = WIDGET_DRAW(cbase3,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size)


WIDGET_CONTROL, WDGT.swelz_base_main, /REALIZE

  WIDGET_CONTROL, WDGT.swelz_drawsurvey, GET_VALUE=windw
  swest.win(0)=windw
  wset,swest.win(0) & erase
  plt,/lzwin

WIDGET_CONTROL, WDGT.swelz_draw1, GET_VALUE=windw
swest.win(1)=windw

if swest.lzwfrmt eq 0 then begin
  WIDGET_CONTROL, WDGT.swelz_draw2, GET_VALUE=windw
  swest.win(2)=windw

  WIDGET_CONTROL, WDGT.swelz_draw3, GET_VALUE=windw
  swest.win(3)=windw

  swest.win_xsize(1:3)=x_size/3
  swest.win_ysize(1:3)=y_size

endif else begin
  swest.win_xsize(1)=x_size
  swest.win_ysize(1)=y_size
endelse

if keyword_set(wst.lzdate) ne 0 then begin
   if wst.xydata(0) eq 0 then begin
     wst.xydata(0)=double(fh.fst.ms)/3600000.d
     hour_hms,wst.xydata(0),hms,lhms=lhms
     wst.hms=hms(0)
     wst.pb5=[fh.fst.yr,fh.fst.dy,fh.fst.ms]
     wst.timsel='lztm'
     print,'initial selected time ',$
      wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
     widget_control,WDGT.swelz_hmsfld,set_value=lhms
  endif  
endif

if (swest.veis_on eq 1) then $ ;              If VEIS is on then set plot type
   swest.pltype_index = default_pltype_index $ ;               to default type.
else swest.pltype_index = 0 ;         Otherwise, (VEIS off) set to top of list.
widget_control,WDGT.swelz_fpltype,set_droplist_select=swest.pltype_index

wlzdy=where(wst.lzdays eq wst.lzindate)
widget_control,WDGT.swelz_days,set_droplist_select=wlzdy(0)

widget_control,WDGT.swelz_spectra,sensitive=swest.veis_on
widget_control,WDGT.swelz_sav,sensitive=swest.veis_on
widget_control,WDGT.swelz_fevents,sensitive=swest.veis_on
widget_control,WDGT.swelz_vstps,sensitive=swest.veis_on
widget_control,WDGT.swelz_pbin,sensitive=swest.veis_on
     
XMANAGER, "swe_levelzero", WDGT.swelz_base_main, GROUP_LEADER = GROUP  


END

