;================== wanal_event =============================================

pro wanal_event,event

common sharewidg,WDGT
common shared,d
common wstuff,wst
common swestuff,swest
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

;-----------------------------------------------------------------------------
;                   begin event response sections 
;-----------------------------------------------------------------------------


help,event,/str

CASE event.id OF
    
  WDGT.detailed_data_type: begin
    case event.value of
      'SWE level zero' : begin
          widget_control,WDGT.timemark,set_droplist_select=wst.timemark_offon
          if xregistered('swe_levelzero') eq 0 then $
          swe_levelzero   else $
          if xregistered('swe_levelzero') ne 0 then $
          swe_levelzero else $
          WIDGET_CONTROL, WDGT.swelz_base_main,iconify=0  
                         endcase  
      'ISEE f':iseef                                          
    endcase
                          endcase
                          
  WDGT.path: begin
    case event.value of
      'Path' : setpaths
      'Quit' : WIDGET_CONTROL, event.top, /DESTROY
      'HELP' : xdisplayfile,getenv('WGGSBASE')+$
        'survey/help_survey.txt',width=100
    endcase
                       endcase       

  WDGT.datefile : begin
     list=['Date','File']
     wst.date_file=list(event.index)
                 endcase 

  WDGT.input_ymd : begin
     WIDGET_CONTROL, WDGT.input_ymd, GET_VALUE = val
     print,val
     wst.indate=string(val(0),format='(i8.8)')
                 endcase 
  
  WDGT.maxnumber_days: begin
    list=string(1+indgen(wst.maxnumber_days),format='(i2)')
    wst.number_days=long(list(event.index))
    
                 endcase
                                                                              
  WDGT.select_data_types: begin
    print,'event.value ',event.value
    inputselst=lonarr(n_elements(d.datype))
    resetflg=where(wst.selectinput eq -1,nresetflg)
    if nresetflg eq n_elements(wst.selectinput) then begin
      inputselst(event.value)=1
      widget_control,WDGT.select_data_types,set_value=inputselst
    endif  
    widget_control,WDGT.select_data_types,get_uvalue=uval
    if uval(event.value) eq 'isee_moments' then wst.date_file='File'            
    wst.selectinput(event.value)=event.value
    ;if swe_moments are selected then also select ionkp
    if uval(event.value) eq 'swe_moments' then $
      wst.selectinput(where(uval eq 'swe_ionkp'))=where(uval eq 'swe_ionkp')
    wst.selectinput_last=wst.selectinput
                 endcase
                 
  WDGT.read_opts: begin    ;selection of input data
    print,'event.value ',event.value 
    WIDGET_CONTROL, WDGT.input_ymd, GET_VALUE = val
    wst.indate=string(val(0),format='(i8.8)')
    print,'wst.indate ',wst.indate
    n_elem=n_elements(d.datype(where(d.datype ne '')))
    case event.value of

      'READ' : begin   ;'READ' has been selected
        wlast=where(wst.selectinput_last ne -1)
        if wlast(0) eq -1 then goto,endpro
        wst.selectinput=wst.selectinput_last
        panelist                                ;initialize
        print,'selectinput ',wst.selectinput
        wsurvey=where(wst.selectinput ne -1)
        if wst.number_days gt d.maxnumberdays then begin
          for i=0,n_elements(wsurvey)-1 do begin
            wmulti=where(d.datype_multiday eq d.datype(wsurvey(i)))
            if wmulti(0) eq -1 then wst.number_days=d.maxnumberdays
          endfor
        endif  
        WIDGET_CONTROL,WDGT.maxnumber_days,set_droplist_select=wst.number_days-1
        wst.selectinput=-1+intarr(n_elem)
        print,'wsurvey ',wsurvey
        if wsurvey(0) ne -1 then begin ;select moments etc data to read 
            print,'indata_types(wsurvey) ',d.datype(wsurvey)
            input,wsurvey
            wst.yesdata=1
            WIDGET_CONTROL,WDGT.input_ymd,set_value=wst.surveydate
            select_pltvar
          endif
                endcase
      
      'READ/PLOT sequence' : readplot_sequence 
      
      else :
      endcase
  endcase
                 
            
  

;------------ begin section to list survey data available to select ------------

  WDGT.plot_interval: begin   
    if keyword_set(wst.yesdata) eq 0 then goto,endpro
    case event.value of

      'Current ' : begin 
           wset,WDGT.win_main
           plt
      endcase

      'Previous' : begin
         d.ndx_buff2=d.ndx
         d.ndx=d.ndx_last
         mstruct_timeaxis
         wset,WDGT.win_main & erase
         plt 
         d.ndx=d.ndx_buff2
         mstruct_timeaxis           
      endcase

          
      'Original' : begin
         ;print,'Restore original'
         d.ndx_last=d.ndx
         d.ndx=d.ndx_orig
         mstruct_timeaxis
         wset,WDGT.win_main & erase
         plt                  
      endcase 
   
      'Stored' : begin
         d.ndx_buff2=d.ndx
         d.ndx=d.ndx_stored
         mstruct_timeaxis
         wset,WDGT.win_main & erase
         plt 
         d.ndx=d.ndx_buff2
         mstruct_timeaxis            
      endcase
     
      else:
    endcase
  endcase
 
  
  WDGT.img_array_dim_reduction: begin
    wst.rebin=event.index
    plt
                                endcase   
  
  WDGT.y_preset_minmax: begin 
    if keyword_set(wst.yesdata) eq 0 then goto,endpro
    wst.minmax=event.index     ;if 0, preset else if 1, minmax
    wset,WDGT.win_main & erase 
    plt    
  endcase
  
  WDGT.time_axis: begin 
    if keyword_set(wst.yesdata) eq 0 then goto,endpro
    case event.value of
      'Time' : time_scale               ;change time scale     
       else:
    endcase
  endcase
  
  WDGT.smoothing: begin
                     wst.spikesout=wst.smoothing(event.index)
                     plt
                  endcase
                  
  WDGT.timemark: wst.timemark_offon=event.index
  
  WDGT.rm_timemark: begin
    wst.timemark_offon=0
    widget_control,WDGT.timemark,set_droplist_select=wst.timemark_offon
    wst.timemark(*)=0.d
    plt 
  endcase
  
  WDGT.strlfov : begin  
    wst.strlfov=event.index
    wset,WDGT.win_main
    plt
  endcase           
  
  WDGT.create_momsvfl: wst.creatmsvfl=event.index
  
  WDGT.yvsx: yvsx
                  
  WDGT.loadct_restorect: begin                         ;change color scale
    if keyword_set(wst.yesdata) eq 0 then goto,endpro
    wset,WDGT.win_main
    list=['Color tbl','Restore clrs']
    case list(event.index) of    
      'Color tbl' : begin
          xloadct
          multi,256/float(!d.table_size) 
          r_orig(!d.table_size-1)=255     ;white plot axes, etc
          g_orig(!d.table_size-1)=255
          b_orig(!d.table_size-1)=255
          tvlct,r_orig,g_orig,b_orig
          !p.background=0                      ;black background
                  endcase

      'Restore clrs' : clrtbl_indx  
    endcase
  endcase
                                                      
  WDGT.IDLsave: begin
     list=['IDLsave ']  
     case list(event.index) of 
        'IDLsave ' : plt,idlsave=1 
     endcase
                 endcase


  
  WDGT.special_appl : call_procedure,getenv('SPECIAL_APPL')
                  
  WDGT.hardcopy_main: begin                         ;
    list=['Hrdcpy bw','Hrdcpy clr','Printer/file']
    case list(event.index) of
      
      'Hrdcpy bw' : begin
        wst.hardcopy=1 & wset,WDGT.win_main
        wst.printer=wst.printer_bw
        wst.print_flnm=wst.print_flnm_bw
        wst.print_cmd=wst.print_cmd_bw
        wst.rebin=0
        WIDGET_CONTROL,WDGT.img_array_dim_reduction,$
          set_droplist_select=wst.rebin 
        plt
      endcase

      'Hrdcpy clr' : begin
        wst.hardcopy=1 & wset,WDGT.win_main
        wst.printer=wst.printer_clr
        wst.print_flnm=wst.print_flnm_clr
        wst.print_cmd=wst.print_cmd_clr
        wst.rebin=0
        WIDGET_CONTROL,WDGT.img_array_dim_reduction,$
          set_droplist_select=wst.rebin 
        plt
      endcase
      
      'Printer/file' :select_printer
            
    endcase     
  endcase

  WDGT.orbit : begin
    list=['WIND orbit','Local Appl']
    case list(event.index) of
      'WIND orbit': orbit_plot,event
      'Local Appl' : if getenv('LOCALUSR') ne '' then local_appl 
    endcase
                  endcase
endcase

endpro:

end




;=================== CALLING PROCEDURE: wanal =================================

;   WGGS is a tool to access, display and analyze WIND and ISEE-1 data.
;   Written by R. J. Fitzenreiter GSFC/LEP, 1994.
;   Last revision: April, 2000.
 
pro wanal

common sharewidg,WDGT
common shared,d
common wstuff,wst
common swestuff,swest

set_plot,'x'


if xregistered('wanal') then return

;define widgets and initialize control structures
  define_widgets
  if keyword_set(d) eq 0 then panelist
  if keyword_set(wst) eq 0 then structuresw
  clrtbl_indx
  decompress_tbl

;------------------- set up main base widgets --------------------------------

WDGT.base_main = WIDGET_BASE(/COLUMN,group_leader=WDGT.base_main,$
 TITLE='Survey Data Display  (version '+getenv('WGGSVERSION')+')')

if getenv('DEFAULT_FONT') ne '' then $
  widget_control,default_font=getenv('DEFAULT_FONT')   

rbase1=widget_base(WDGT.base_main,/row)
cbase2=widget_base(rbase1,/column)

WDGT.win_main_xsize= fix(getenv('XSIZE_MAIN'))
WDGT.win_main_ysize= fix(getenv('YSIZE_MAIN'))     
WDGT.draw_main = WIDGET_DRAW(cbase2,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2,event_pro='draw_main_ev',$
  XSIZE = WDGT.win_main_xsize,YSIZE=WDGT.win_main_ysize)

cbase1=widget_base(rbase1,/column)

rbase=widget_base(cbase1,/row)
WDGT.detailed_data_type=cw_bgroup(rbase,/return_name, row=1,$
   ['SWE level zero','ISEE f'])
   
WDGT.path=cw_bgroup(rbase, /return_name,row=1,$
    ['Path','HELP','Quit'])
     
rbase=widget_base(cbase1,/row)
WDGT.datefile=widget_droplist(rbase,title=' ',$
    value=['Date','File'])

WDGT.input_ymd=cw_field(rbase,title='yyyymmdd',$
   /return_events,/string,xsize=8, ysize=1,/row)

WDGT.maxnumber_days=widget_droplist(rbase,title='Nr days',$
   value=string(1+indgen(wst.maxnumber_days),format='(i2)'))

rbase=widget_base(cbase1,/row,event_pro='xstrahlen_ev')
wlabl=widget_label(rbase,value='swe_strahlen')
strahlen0=widget_droplist(rbase,title='',$
   value=string(wst.strlenergies,format='(i4)'),uvalue='strahlen0')
strahlen1=widget_droplist(rbase,title='',$
   value=string(wst.strlenergies,format='(i4)'),uvalue='strahlen1')
wen=where(wst.strlen0 eq wst.strlenergies)  
widget_control,strahlen0,set_droplist_select=wen(0)
wen=where(wst.strlen1 eq wst.strlenergies)    
widget_control,strahlen1,set_droplist_select=wen(0)

         
rbase=widget_base(cbase1,/row) 
inputlist=d.datype($
  where(d.datype ne 'SWE enrgy spctrm'));  
wst.selectinput=-1+intarr(n_elements(d.datype))
selectinput_val=intarr(n_elements(inputlist))
wsel=$
where(wst.selectinput_last(where(d.datype ne 'SWE enrgy spctrm')) ne -1)
if wsel(0) ne -1 then selectinput_val(wsel)=1
    ;NOTE:'SWE enrgy spctrm' is not incl in the displayed list but is included
    ;whenever pitch angle distributions are selected
WDGT.select_data_types=cw_bgroup(rbase,/nonexclusive,/frame,$
   column=3,/return_index,label_top='SELECT input data types, then READ',$
   set_value=selectinput_val,$
   inputlist,uvalue=inputlist)

if getenv('CREATSVFL') ne '' then begin
  rbase=widget_base(cbase1,/row)
  WDGT.create_momsvfl=widget_droplist(rbase,$
    title='Create swe_moments save file',value=wst.noyes)
endif
  
rbase2=widget_base(cbase1,/row)  
rbase=widget_base(rbase2,/row)
WDGT.read_opts=cw_bgroup(rbase,/frame,$  
   row=1,/return_name,label_left='',['READ','READ/PLOT sequence'])

rbase=widget_base(rbase2,/row,event_pro='xdata_versions_ev')

desc=replicate({flags:0,name:''},$
  1+n_elements(wst.mom_version)+n_elements(wst.ptch_version))
desc(0).flags=1
desc(n_elements(desc)-1).flags=2
desc(0).name='Data version'
desc(1:n_elements(wst.mom_version)).name='swe_moments: '+wst.mom_version
desc(1+n_elements(wst.mom_version):$
     n_elements(wst.mom_version)+n_elements(wst.ptch_version)).name=$
     'swe_pitch:   '+wst.ptch_version
data_versions=cw_pdmenu(rbase,desc,/return_name)

;plot survey data
rbase2=widget_base(cbase1,/row)
rbase=widget_base(rbase2,/row,event_pro='select_pltvar')
WDGT.select_plot_variables=cw_bgroup(rbase,/return_name,$
    ['Select plot variables'])

rbase=widget_base(rbase2,/row,event_pro='xplt_params')
plot_ctrl=widget_button(rbase,value='Change plot parameters')

WDGT.plot_interval=cw_bgroup(cbase1,column=4,/return_name,/frame,$
    label_left='Time interval',$
    ['Current ','Previous','Original','Stored'])
  
  
rbase=widget_base(cbase1,/row)
WDGT.img_array_dim_reduction=widget_droplist(rbase,title='Array reduction',$
    value=['congrid','off'])

WDGT.smoothing=widget_droplist(rbase,title='Smooth',value=wst.smoothing)
          
rbase=widget_base(cbase1,/row)                 
WDGT.y_preset_minmax=widget_droplist(rbase,title='Y-axis',$
    value=['Preset','Minmax'])
  
WDGT.time_axis=cw_bgroup(rbase,/return_name,label_left='Time axis',$
    ['Time'])

WDGT.yvsx=cw_bgroup(rbase,/return_name,label_left=' ',$
    ['Y vs X'])
    
rbase=widget_base(cbase1,/row,/frame)
WDGT.timemark=widget_droplist(rbase,title='Time mark',value=wst.offon)

WDGT.rm_timemark=cw_bgroup(rbase,/return_name,label_left=' ','Remove')

rbase=widget_base(cbase1,/row)
WDGT.strlfov=widget_droplist(rbase,title='Strahl FOV',value=wst.offon)
    
rbase=widget_base(cbase1,/row) 
WDGT.hardcopy_main=widget_droplist(rbase,title='',$
    value=['Hrdcpy bw','Hrdcpy clr','Printer/file'])
                    
WDGT.loadct_restorect=widget_droplist(rbase,title='',$
    value=['Color tbl','Restore clrs'] )
                      
rbase=widget_base(cbase1,/row)                                                 
WDGT.orbit=widget_droplist(rbase,title='',$
  value=['WIND orbit','Local Appl'])

WDGT.IDLsave=widget_droplist(rbase,title='',$
  value=['IDLsave'])    


if getenv('SPECIAL_APPL') ne '' then begin
  rbase=widget_base(cbase1,/row)
  WDGT.special_appl=cw_bgroup(rbase,/row,/return_name,label_left=' ',$
    ['Special Applications'])
endif    
    
widget_control, WDGT.base_main, /REALIZE

widget_control, WDGT.draw_main, GET_VALUE=windw
WDGT.win_main=windw

restore,getenv('IDLSAV')+'mfilter'
wst.mfilter=mfilter

restore,getenv('IDLSAV')+'pfilter'
wst.pfilter=pfilter

nmbrdays= wst.number_days < wst.maxnumber_days
widget_control,WDGT.maxnumber_days,set_droplist_select=nmbrdays-1

widget_control,WDGT.y_preset_minmax,set_droplist_select=wst.minmax 

widget_control,WDGT.img_array_dim_reduction,set_droplist_select=wst.rebin

;widget_control,WDGT.Fcntrs_offon,set_droplist_select=swest.cntrs_redf
  
if wst.date_file eq 'Date' then $
  widget_control,WDGT.datefile,set_droplist_select=0 else $
  widget_control,WDGT.datefile,set_droplist_select=1

if keyword_set(wst.indate) ne 0 then $
  widget_control, WDGT.input_ymd, set_value = string(wst.indate,format='(i8.8)')


wsmth=where(wst.smoothing eq wst.spikesout)
widget_control,WDGT.smoothing,set_droplist_select=wsmth(0)

widget_control,WDGT.timemark,set_droplist_select=wst.timemark_offon

widget_control,WDGT.strlfov,set_droplist_select=wst.strlfov
if getenv('CREATSVFL') ne '' then $
  widget_control,WDGT.create_momsvfl,set_droplist_select=wst.creatmsvfl


wset,WDGT.win_main
plt

XMANAGER, "wanal", WDGT.base_main, GROUP_LEADER = GROUP  ;hand off to manager

           
END

