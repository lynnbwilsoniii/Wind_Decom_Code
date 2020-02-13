;========================== yvsx_plot ======================================
pro yvsx_plot,hardcopy=hardcopy

common sharewidg,WDGT
common shared,d
common wstuff,wst
common shareyvsx,yvar_indx,xvar_indx,yvar_log,xvar_log

if keyword_set(hardcopy) ne 0 then begin
  print,' ' & print,'making hardcopy... '
  set_plot,'ps',/interpolate
  clrtbl_indx,/hardcopy 
  pltfil=getenv('IDLSAV')+wst.print_flnm 
  print,'pltfil',pltfil
  device,/inches,xoffset=0.75,yoffset=1.,xsize=7.,ysize=9.5,/color,$
     filename=pltfil,bits=8
endif
 
;selected indices from set of indices d.pnlist.list
  wpnl=where(d.pnlsel ne -1)
  if wpnl(0) eq -1 then begin
    print,'no plot selection has been made'
    return
  endif
  pnlsel=d.pnlsel(wpnl)

;structure of plotting parameters for selected plot variables 
   pm=d.pnl(pnlsel)

;get reference time in pb5time
  refpb5=sec_pb5(d.refsec)


print,'Selected:  ',pm(yvar_indx).varname,'   ',pm(xvar_indx).varname

iydatype=where(d.datype eq pm(yvar_indx).dtp)
;yhrday=(d.mfi_magkpdat(d.ndx(0,iydatype):d.ndx(1,iydatype)).ta-d.refsec)/3600.d
yhrday=call_function(pm(yvar_indx).dtp+'_datatime')
yvbl=call_function(pm(yvar_indx).dtp+'var',pm(yvar_indx).varname)

ixdatype=where(d.datype eq pm(xvar_indx).dtp)
;xhrday=(d.mfi_magkpdat(d.ndx(0,ixdatype):d.ndx(1,ixdatype)).ta-d.refsec)/3600.d
xhrday=call_function(pm(xvar_indx).dtp+'_datatime')
xvbl=call_function(pm(xvar_indx).dtp+'var',pm(xvar_indx).varname)


if keyword_set(hardcopy) eq 0 then wset,WDGT.win_yvsx

plot,xvbl(1:n_elements(xvbl)-1),yvbl(1:n_elements(yvbl)-1),$
  psym=1,symsize=0.4,xlog=xvar_log,ylog=yvar_log,/ynozero,$
  ytitle=pm(yvar_indx).varname,xtitle=pm(xvar_indx).varname,$
  title=wst.surveydate
  ;xstyle=1,xrange=pm(xvar_indx).range,xticks=pm(xvar_indx).ticks,$
  ;ystyle=1,yrange=pm(yvar_indx).range,yticks=pm(yvar_indx).ticks


if keyword_set(hardcopy) ne 0 then begin
  device,/close
  print,' ' & print,'printing hardcopy: ',wst.print_cmd
  spawn,wst.print_cmd
  set_plot,'x'
  clrtbl_indx
endif

end



;========================== yvsx_event ===================================

pro yvsx_event,event

common sharewidg,WDGT
common shared,d
common wstuff,wst
common shareyvsx,yvar_indx,xvar_indx,yvar_log,xvar_log

help,event,/str

CASE event.id of

WDGT.yvsx_quit : begin
    case event.value of
      'Parent' : begin
                   wset,WDGT.win_main
                   WIDGET_CONTROL, WDGT.base_main, /show
                 endcase
      'Quit' :  WIDGET_CONTROL, event.top, /DESTROY
      'HELP' :
      'Hardcopy' : yvsx_plot,/hardcopy 
    endcase
                    endcase
                    
WDGT.yvsx_y : yvar_indx=event.index

WDGT.yvsx_x : xvar_indx=event.index

WDGT.yvsx_ylog : yvar_log=event.index

WDGT.yvsx_xlog : xvar_log=event.index

WDGT.yvsx_plot : yvsx_plot
                   
endcase                    
end



;============================== yvsx =====================================

pro yvsx

common sharewidg,WDGT
common shared,d
common wstuff,wst
common shareyvsx,yvar_indx,xvar_indx,yvar_log,xvar_log

set_plot,'x'

if xregistered('yvsx') then return


WDGT.base_yvsx = WIDGET_BASE(/COLUMN,TITLE='Panel Y  vs  Panel X')

if getenv('DEFAULT_FONT') ne '' then $
  widget_control,default_font=getenv('DEFAULT_FONT')   

rbase1=widget_base(WDGT.base_yvsx,/row)
cbase2=widget_base(rbase1,/column)

WDGT.win_yvsx_xsize= fix(getenv('XSIZE_MAIN'))/2
WDGT.win_yvsx_ysize= fix(getenv('YSIZE_MAIN')) /2    
WDGT.draw_yvsx = WIDGET_DRAW(cbase2,/BUTTON_EVENTS,/FRAME, $
  RETAIN = 2,XSIZE = WDGT.win_yvsx_xsize,YSIZE=WDGT.win_yvsx_ysize)

cbase1=widget_base(rbase1,/column)

rbase=widget_base(cbase1,/row)
WDGT.yvsx_quit=cw_bgroup(rbase,['Parent','HELP','Hardcopy','Quit'],$
  column=4,/return_name)


;selected indices from set of indices d.pnlist.list
  wpnl=where(d.pnlsel ne -1)
  if wpnl(0) eq -1 then begin
    print,'no plot selection has been made'
    return
  endif
  pnlsel=d.pnlsel(wpnl)

;structure of plotting parameters for selected plot variables 
   pm=d.pnl(pnlsel)


rbase=widget_base(cbase1,/row)    
WDGT.yvsx_y=widget_droplist(rbase,title='Y-axis',$
   value=pm.varname)
WDGT.yvsx_ylog=widget_droplist(rbase,title=' ',value=['0','1'])
   
rbase=widget_base(cbase1,/row)
WDGT.yvsx_x=widget_droplist(rbase,title='X-axis',$
   value=pm.varname)
WDGT.yvsx_xlog=widget_droplist(rbase,title=' ',value=['0','1'])

rbase=widget_base(cbase1,/row)
WDGT.yvsx_plot=cw_bgroup(rbase,[' Plot '],column=1,/return_name)

widget_control, WDGT.base_yvsx, /REALIZE

widget_control, WDGT.draw_yvsx, GET_VALUE=windw
WDGT.win_yvsx=windw

if keyword_set(yvar_indx) eq 0 then yvar_indx=0
if keyword_set(xvar_indx) eq 0 then xvar_indx=0
if keyword_set(yvar_log) eq 0 then yvar_log=0
if keyword_set(xvar_log) eq 0 then xvar_log=0

widget_control,WDGT.yvsx_y,set_droplist_select=yvar_indx
widget_control,WDGT.yvsx_x,set_droplist_select=xvar_indx
widget_control,WDGT.yvsx_ylog,set_droplist_select=yvar_log
widget_control,WDGT.yvsx_xlog,set_droplist_select=xvar_log

XMANAGER, "yvsx", WDGT.base_yvsx, GROUP_LEADER = GROUP  ;hand off to manager

end