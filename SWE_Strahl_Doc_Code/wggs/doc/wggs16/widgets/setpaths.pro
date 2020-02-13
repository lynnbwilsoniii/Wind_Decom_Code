;================== setpaths_event ====================================

pro setpaths_event,event
common sharewidg,WDGT
common sharepaths,datapaths
common shared,d

CASE event.id OF  

WDGT.ok_setpath: widget_control,event.top,/destroy

else: begin
  widget_control,event.id,get_uvalue=uval
  print,uval
  setenv,datapaths(uval).envar+'='+datapaths(uval).list(event.index)
  print,datapaths(uval).envar+' '+getenv(datapaths(uval).envar)
  widget_control,event.id,set_droplist_select=event.index
      endcase

endcase      
      

end



;========================= setpaths ===================================

pro setpaths,init=init
common sharewidg,WDGT
common sharepaths,datapaths

restore,getenv('IDLSAV')+'datapaths'
help,datapaths
help,datapaths,/str
if keyword_set(init) ne 0 then begin
  for i=0,n_elements(datapaths)-1 do begin
    setenv,datapaths(i).envar+'='+datapaths(i).list(0)
    print,datapaths(i).envar+' '+getenv(datapaths(i).envar) 
  endfor
  return
endif


WDGT.base_setpath = WIDGET_BASE(TITLE = 'Set data paths',/COLUMN);main base
WDGT.ok_setpath=cw_bgroup(WDGT.base_setpath, '   OK   ')
    
cbase=widget_base(WDGT.base_setpath,/column,/frame)


for i=0,n_elements(datapaths)-1 do begin
  rbase=widget_base(cbase,/row)
  wp=where(datapaths(i).list ne '')
  if wp(0) ne -1 then begin
    path=datapaths(i).list(wp)
    widg=widget_droplist(rbase,title=datapaths(i).name,value=path,uvalue=i)
    w=where(path eq getenv(datapaths(i).envar))
    widget_control,widg,set_droplist_select=w(0)
  endif  
endfor

    
WIDGET_CONTROL, WDGT.base_setpath, /REALIZE

XMANAGER, "setpaths", WDGT.base_setpath , GROUP_LEADER = GROUP 


end
