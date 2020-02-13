

;============================= time_scale_event ==============================

pro time_scale_event,event

;finds new set of data indices for selected time intervals

common sharewidg,WDGT
common shared,d
common wstuff,wst

;help,event,/str

case event.id of

WDGT.time_scale_begin: begin
  widget_control,WDGT.time_scale_begin,get_value=val
  ;convert string hhmmss to long hhmmss
  hhmmss=long(strmid(val(0),10,6))
  wst.tmn=hms_hour(hhmmss)
  indexbegin,wst.tmn  ;begin index in each selected data set
  ;wst.newticks_set=0
           endcase


WDGT.time_scale_end: begin
  widget_control,WDGT.time_scale_end,get_value=val
  ;convert string hhmmss to long hhmmss
  hhmmss=long(strmid(val(0),10,6))
  wst.tmx=hms_hour(hhmmss)
  indexend,wst.tmx  ;end index in each selected data set
  ;wst.newticks_set=0
           endcase

WDGT.time_scale_plot: begin
  case event.value of

    'Plot' : begin    
             ;print,'plot new time interval'
             d.ndx_last=d.ndx
             d.ndx=d.ndx_buff
             mstruct_timeaxis
             widget_control,event.top,/destroy
             wset,WDGT.win_main & erase
             plt
                 endcase
    
    'Store interval and plot' : begin    
             ;print,'plot stored time interval'
             d.ndx=d.ndx_buff
             d.ndx_stored=d.ndx
             mstruct_timeaxis
             widget_control,event.top,/destroy
             wset,WDGT.win_main & erase
             plt
                 endcase
                 
    'Original' : begin
        print,'Restore original'
        d.ndx_last=d.ndx
        d.ndx=d.ndx_orig        
        mstruct_timeaxis
        widget_control,event.top,/destroy
        wset,WDGT.win_main & erase
        plt
                        endcase
     else:
               endcase
              
  endcase

WDGT.time_scale_minutes : begin
    WIDGET_CONTROL, WDGT.time_scale_minutes, GET_VALUE = val
    wst.min_intrvl=fix(val(0))
              endcase
              
WDGT.time_scale_strahl : begin 
  case event.value of
    'Top panel vs next to top panel' : begin
              print,'OK plot variable 1 vs variable 2'
              if total(d.ndx_buff(1,*)) gt total(d.ndx_buff(0,*)) then begin 
                d.ndx=d.ndx_buff 
              endif else begin
                print,'Select start and stop times' & return
              endelse 
              mstruct_timeaxis
              widget_control,event.top,/destroy
              plt_var1_var2 
                                       endcase
     'Strahl appl' : begin
         if wst.strlappl_intrvl then begin
           strlappl_intrvl
         endif
                     endcase                                  
     'Cancel' : widget_control,event.top,/destroy                             
  endcase   
endcase

WDGT.time_scale_save_interval : begin
  openw,lun,wst.time_interval_flnm,/get_lun,/append
  printf,lun,wst.ymd_begin,wst.hms_begin,wst.ymd_end,wst.hms_end,$
    wst.pb5_begin,wst.pb5_end,$
    format='(i8,1x,a8,2x,i8,1x,a8,4x,i4,1x,i3,1x,i8,2x,i4,1x,i3,1x,i8)'
  
  print,''
  print,wst.ymd_begin,wst.hms_begin,wst.ymd_end,wst.hms_end,$
    wst.pb5_begin,wst.pb5_end,$
    format='(i8,1x,a8,2x,i8,1x,a8,4x,i4,1x,i3,1x,i8,2x,i4,1x,i3,1x,i8)'
  free_lun,lun
  print,'time interval saved to file ',wst.time_interval_flnm
  widget_control,event.top,/destroy
endcase

WDGT.time_scale_save_flnm : begin
    WIDGET_CONTROL, WDGT.time_scale_minutes, GET_VALUE = val       
    wst.time_interval_flnm=val(0)      
endcase      
             
endcase   

end     



;============== time_scale ==================================================

pro time_scale

common sharewidg,WDGT
common shared,d
common wstuff,wst

if xregistered('time_scale') then return 
 
WDGT.base_time_scale=widget_base(title='Selected time interval',$
  /column,space=25)
cbase=widget_base(WDGT.base_time_scale,/column,/frame)

rbase=widget_base(cbase,/row)
WDGT.time_scale_begin=cw_field(rbase,title='Begin:  yyyymmdd  hhmmss',$
  /return_events,/string,xsize=16)

rbase=widget_base(cbase,/row)
WDGT.time_scale_end=cw_field(rbase,title='End:  yyyymmdd  hhmmss  ',$
  /return_events,/string,xsize=16)

rbase=widget_base(cbase,/row)
WDGT.time_scale_plot=cw_bgroup(rbase,label_left=' ',row=2,/return_name,$
  ['Plot','Store interval and plot','Original'])
    
WDGT.time_scale_minutes=cw_field(rbase,title='Minutes',/return_events,$
  /integer,xsize=3,ysize=1,/row)

rbase=widget_base(cbase,/column)
WDGT.time_scale_save_interval=cw_bgroup(rbase,row=1,/return_name,$
  ['Save time interval to ascii file'])

WDGT.time_scale_save_flnm=cw_field(rbase,title='Enter file name',/string,$
  xsize=strlen(wst.time_interval_flnm)+15,ysize=1,/row)  
  
cbase=widget_base(WDGT.base_time_scale,/row)
WDGT.time_scale_strahl=cw_bgroup(cbase,row=1,/return_name,$
  ['Strahl appl','Top panel vs next to top panel','Cancel'])

WIDGET_CONTROL, WDGT.base_time_scale, /REALIZE

WIDGET_CONTROL, WDGT.time_scale_minutes, set_value = fix(wst.min_intrvl)

WIDGET_CONTROL, WDGT.time_scale_save_flnm, set_value = wst.time_interval_flnm

XMANAGER, "time_scale", WDGT.base_time_scale, GROUP_LEADER = GROUP


end


