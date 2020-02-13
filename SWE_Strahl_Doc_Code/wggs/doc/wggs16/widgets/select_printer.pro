;================== select_printer_event ====================================

pro select_printer_event,event

common sharewidg,WDGT
common wstuff,wst

help,event,/str

CASE event.id OF  

WDGT.bw_selprntr : begin
  widget_control,WDGT.bw_selprntr,get_value=val
  wst.printer_bw=strcompress(val(0),/remove_all)
  widget_control,WDGT.bw_selprntr,set_value=wst.printer_bw
  wst.print_cmd_bw='lp '+wst.printer_bw+' '+getenv('IDLSAV')+wst.print_flnm_bw
  widget_control,WDGT.bwcmd_selprntr,set_value=wst.print_cmd_bw
          endcase

WDGT.bwflnm_selprntr : begin
  widget_control,WDGT.bwflnm_selprntr,get_value=val
  wst.print_flnm_bw=strcompress(val(0),/remove_all)
  widget_control,WDGT.bwflnm_selprntr,set_value=wst.print_flnm_bw
  wst.print_cmd_bw='lp '+wst.printer_bw+' '+getenv('IDLSAV')+wst.print_flnm_bw
  widget_control,WDGT.bwcmd_selprntr,set_value=wst.print_cmd_bw
          endcase

WDGT.clr_selprntr : begin
  widget_control,WDGT.clr_selprntr,get_value=val
  wst.printer_clr=strcompress(val(0),/remove_all)
  widget_control,WDGT.clr_selprntr,set_value=wst.printer_clr
  wst.print_cmd_clr=$
   'lp '+wst.printer_clr+' '+getenv('IDLSAV')+wst.print_flnm_clr
  widget_control,WDGT.clrcmd_selprntr,set_value=wst.print_cmd_clr
          endcase

WDGT.clrflnm_selprntr : begin
  widget_control,WDGT.clrflnm_selprntr,get_value=val
  wst.print_flnm_clr=strcompress(val(0),/remove_all)
  widget_control,WDGT.clrflnm_selprntr,set_value=wst.print_flnm_clr
  wst.print_cmd_clr=$
   'lp '+wst.printer_clr+' '+getenv('IDLSAV')+wst.print_flnm_clr
  widget_control,WDGT.clrcmd_selprntr,set_value=wst.print_cmd_clr
          endcase

WDGT.ok_selprntr: begin
   case event.value of
   'OK': begin
       print,'wst.print_cmd_bw = ',wst.print_cmd_bw
       print,'wst.print_cmd_clr = ',wst.print_cmd_clr
       widget_control,event.top,/destroy
         endcase
   'Restore original': begin
       wst.printer_bw=wst.printer_name_bw_orig
       wst.print_flnm_bw=wst.print_filename_bw_orig
       wst.print_cmd_bw=$
       'lp '+wst.printer_bw+' '+getenv('IDLSAV')+wst.print_flnm_bw
       widget_control,WDGT.bw_selprntr,set_value=wst.printer_bw
       widget_control,WDGT.bwflnm_selprntr,set_value=wst.print_flnm_bw
       widget_control,WDGT.bwcmd_selprntr,set_value=wst.print_cmd_bw

       wst.printer_clr=wst.printer_name_clr_orig
       wst.print_flnm_clr=wst.print_filename_clr_orig
       wst.print_cmd_clr=$
       'lp '+wst.printer_clr+' '+getenv('IDLSAV')+wst.print_flnm_clr
       widget_control,WDGT.clr_selprntr,set_value=wst.printer_clr
       widget_control,WDGT.clrflnm_selprntr,set_value=wst.print_flnm_clr
       widget_control,WDGT.clrcmd_selprntr,set_value=wst.print_cmd_clr
                              endcase
    endcase
         endcase

endcase

end



;========================= select_printer ===================================

pro select_printer
common sharewidg,WDGT
common wstuff,wst

WDGT.base_selprntr = WIDGET_BASE(TITLE = 'Printer and print file',/COLUMN)
cbase=widget_base(WDGT.base_selprntr,/column,/frame)

WDGT.bw_selprntr=cw_field(cbase,$
   title='Select BLACK & WHITE printer: -Pname or null string (default)',$
   /return_events,/string,xsize=15, ysize=1,/row)
widget_control,WDGT.bw_selprntr,set_value=wst.printer_bw

WDGT.bwflnm_selprntr=cw_field(cbase,title='Select print file name',$
   /return_events,/string,xsize=20, ysize=1,/row)
widget_control,WDGT.bwflnm_selprntr,set_value=wst.print_flnm_bw

rbase=widget_base(cbase,/row)
WDGT.bwcmd_selprntr=cw_field(rbase,title='Print command',$
   /string,xsize=50, ysize=1,/row,/noedit)
wst.print_cmd_bw='lp '+wst.printer_bw+' '+getenv('IDLSAV')+wst.print_flnm_bw
widget_control,WDGT.bwcmd_selprntr,set_value=wst.print_cmd_bw

cbase=widget_base(WDGT.base_selprntr,/column,/frame)
WDGT.clr_selprntr=cw_field(cbase,$
   title='Select COLOR printer: -Pname or null string (default)',$
   /return_events,/string,xsize=15, ysize=1,/row)
widget_control,WDGT.clr_selprntr,set_value=wst.printer_clr

WDGT.clrflnm_selprntr=cw_field(cbase,title='Select print file name',$
   /return_events,/string,xsize=20, ysize=1,/row)
widget_control,WDGT.clrflnm_selprntr,set_value=wst.print_flnm_clr

rbase=widget_base(cbase,/row)
WDGT.clrcmd_selprntr=cw_field(rbase,title='Print command',$
   /string,xsize=50, ysize=1,/row,/noedit)
wst.print_cmd_clr='lp '+wst.printer_clr+' '+getenv('IDLSAV')+wst.print_flnm_clr
widget_control,WDGT.clrcmd_selprntr,set_value=wst.print_cmd_clr

WDGT.ok_selprntr=cw_bgroup(rbase,label_left=' ',['OK','Restore original'],$
     /return_name,row=1)

WIDGET_CONTROL, WDGT.base_selprntr, /REALIZE

XMANAGER, "select_printer", WDGT.base_selprntr , GROUP_LEADER = GROUP ;hand off to manager

end
