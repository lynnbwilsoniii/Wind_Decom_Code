;----------------- xplt_params_event ------------------------------------------

pro xplt_params_event,event
common shared,d
common wstuff,wst

widget_control,event.id,get_uvalue=uvalue
help,event,/str
print,'uvalue ',uvalue

widget_control,event.top,get_uvalue=info,/no_copy
help,info
help,info,/str

case event.id of
  info.quit: begin
     widget_control,event.top,set_uvalue=info,/no_copy
     widget_control,event.top,/destroy
     return
             endcase
  info.plotp: begin
    case event.value of
      'Plot': 
      'Restore': begin
         mstruct ;restores the default plot parameters
         widget_control,info.ypnl_proprtn,set_value=d.pnl(info.pnlindx).ypnlp
         widget_control,info.ztitle,set_value=d.pnl(info.pnlindx).ztitle 
         widget_control,info.dtp,set_value=d.pnl(info.pnlindx).dtp
         widget_control,info.varname,set_value=d.pnl(info.pnlindx).varname
         widget_control,info.labl,set_value=d.pnl(info.pnlindx).labl
         widget_control,info.range(0),set_value=d.pnl(info.pnlindx).range(0)
         widget_control,info.range(1),set_value=d.pnl(info.pnlindx).range(1) 
         widget_control,info.ticks,set_value=d.pnl(info.pnlindx).ticks
         widget_control,info.minor,set_value=d.pnl(info.pnlindx).minor
         widget_control,info.plotio,set_value=d.pnl(info.pnlindx).plotio
         widget_control,info.psym,set_value=d.pnl(info.pnlindx).psym
         widget_control,info.symsize,set_value=d.pnl(info.pnlindx).symsize
         widget_control,info.charthick,set_value=d.pnl(info.pnlindx).charthick
         widget_control,info.charsize,set_value=d.pnl(info.pnlindx).charsize
         
       endcase
    endcase 
      
    plt                           
             endcase  
  
  info.drplst: begin
    w=where(d.pnl.varname eq info.vname(event.index))
    info.pnlindx=w(0)
    widget_control,info.ypnl_proprtn,set_value=d.pnl(info.pnlindx).ypnlp
    widget_control,info.ztitle,set_value=d.pnl(info.pnlindx).ztitle 
    widget_control,info.dtp,set_value=d.pnl(info.pnlindx).dtp
    widget_control,info.varname,set_value=d.pnl(info.pnlindx).varname
    widget_control,info.labl,set_value=d.pnl(info.pnlindx).labl
    widget_control,info.range(0),set_value=d.pnl(info.pnlindx).range(0)
    widget_control,info.range(1),set_value=d.pnl(info.pnlindx).range(1) 
    widget_control,info.ticks,set_value=d.pnl(info.pnlindx).ticks
    widget_control,info.minor,set_value=d.pnl(info.pnlindx).minor
    widget_control,info.plotio,set_value=d.pnl(info.pnlindx).plotio
    widget_control,info.psym,set_value=d.pnl(info.pnlindx).psym
    widget_control,info.symsize,set_value=d.pnl(info.pnlindx).symsize
    widget_control,info.charthick,set_value=d.pnl(info.pnlindx).charthick
    widget_control,info.charsize,set_value=d.pnl(info.pnlindx).charsize
               endcase

  info.ypnl_proprtn : begin
    widget_control,info.ypnl_proprtn,get_value=value
    d.pnl(info.pnlindx).ypnlp=value(0)
    widget_control,info.ypnl_proprtn,set_value=d.pnl(info.pnlindx).ypnlp
    
    wpnl=where(d.pnlsel ne -1)
    if wpnl(0) eq -1 then return
    pnlsel=d.pnlsel(wpnl)
    wrem=where(pnlsel ne info.pnlindx)
    if wrem(0) eq -1 then return
    d.pnl(pnlsel(wrem)).ypnlp=(1.0-d.pnl(info.pnlindx).ypnlp)/n_elements(wrem)
    
  endcase
     
  info.ztitle: begin
    widget_control,info.ztitle,get_value=value
    d.pnl(info.pnlindx).ztitle=value(0)
    widget_control,info.ztitle,set_value=d.pnl(info.pnlindx).ztitle    
               endcase  
               
  info.dtp: begin
    widget_control,info.dtp,get_value=value
    d.pnl(info.pnlindx).dtp=value(0)
    widget_control,info.dtp,set_value=d.pnl(info.pnlindx).dtp    
               endcase           
        
  info.varname: begin
    widget_control,info.varname,get_value=value
    d.pnl(info.pnlindx).varname=value(0)
    widget_control,info.varname,set_value=d.pnl(info.pnlindx).varname    
               endcase           
   
  info.labl: begin
    widget_control,info.labl,get_value=value
    d.pnl(info.pnlindx).labl=value(0)
    widget_control,info.labl,set_value=d.pnl(info.pnlindx).labl    
               endcase           
  
  info.range(0): begin
    widget_control,info.range(0),get_value=value
    d.pnl(info.pnlindx).range(0)=value(0)
    widget_control,info.range(0),set_value=d.pnl(info.pnlindx).range(0)    
               endcase           
  
  info.range(1): begin
    widget_control,info.range(1),get_value=value
    d.pnl(info.pnlindx).range(1)=value(0)
    widget_control,info.range(1),set_value=d.pnl(info.pnlindx).range(1)    
               endcase           
 
  info.ticks: begin
    widget_control,info.ticks,get_value=value
    d.pnl(info.pnlindx).ticks=value(0)
    widget_control,info.ticks,set_value=d.pnl(info.pnlindx).ticks    
               endcase       
                                                                           
 
  info.minor: begin
    widget_control,info.minor,get_value=value
    d.pnl(info.pnlindx).minor=value(0)
    widget_control,info.minor,set_value=d.pnl(info.pnlindx).minor    
               endcase 
               
                
  info.plotio: begin
    widget_control,info.plotio,get_value=value
    d.pnl(info.pnlindx).plotio=value(0)
    widget_control,info.plotio,set_value=d.pnl(info.pnlindx).plotio    
               endcase       
    
  info.psym: begin
    widget_control,info.psym,get_value=value
    d.pnl(info.pnlindx).psym=value(0)
    widget_control,info.psym,set_value=d.pnl(info.pnlindx).psym    
               endcase       
    
  info.symsize: begin
    widget_control,info.symsize,get_value=value
    d.pnl(info.pnlindx).symsize=value(0)
    widget_control,info.symsize,set_value=d.pnl(info.pnlindx).symsize    
               endcase       
    
  info.charthick: begin
    widget_control,info.charthick,get_value=value
    ;d.pnl(info.pnlindx).charthick=value(0)
    d.pnl(*).charthick=value(0)
    widget_control,info.charthick,set_value=d.pnl(info.pnlindx).charthick    
               endcase       
    
  info.charsize: begin
    widget_control,info.charsize,get_value=value
    ;d.pnl(info.pnlindx).charsize=value(0)
    d.pnl(*).charsize=value(0)
    widget_control,info.charsize,set_value=d.pnl(info.pnlindx).charsize    
               endcase       
                     
   endcase
   
   widget_control,event.top,set_uvalue=info,/no_copy


;stop 
end



;---------------- xplt_params -----------------------------------------------

pro xplt_params,event
common shared,d
common wstuff,wst

if xregistered('xplt_params') then return

base_main = WIDGET_BASE(/COLUMN,TITLE='Change Preset Plot Parameters')
cbase1=widget_base(base_main,/column)

rbase=widget_base(cbase1,/row)
quit=cw_bgroup(rbase, uvalue='Quit',row=1,['Quit'],/return_name)
plotp=cw_bgroup(rbase, uvalue='Plot',row=1,['Plot','Restore'],/return_name)
   
;plot parameters
cbase=widget_base(cbase1,/column)

rbase=widget_base(cbase,/row)
labl=widget_label(rbase,value='Make changes, hit RETURN, and Plot')

rbase=widget_base(cbase,/row)
;selected indices from set of indices d.pnlist.list
   pnlsel=d.pnlsel(where(d.pnlsel ne -1))
;structure of plotting parameters for selected plot variables 
   pm=d.pnl(pnlsel)
   list=pm(*).varname
   w=where(d.pnl.varname eq list(0))
   pnlindx=w(0)
   drplst=widget_droplist(rbase,title=' ',$
     uvalue='chng_pltparm',value=list,uname='chng_pltparm')
   ;labl=widget_label(rbase,value='Panel changes:')

rbase=widget_base(cbase,/row)
ypnl_proprtn=cw_field(rbase,title='Ypnl proportion',/string,value=pm(0).ypnlp,$
  /return_events)
       
rbase=widget_base(cbase1,/row)  
ztitle=cw_field(rbase,title='Plot tile',/string,value=pm(0).ztitle,$
  /return_events)
dtp=cw_field(rbase,title='Data type',/string,value=pm(0).dtp)

rbase=widget_base(cbase1,/row)
varname=cw_field(rbase,title='Plot variable',/string,value=pm(0).varname)
labl=cw_field(rbase,title='Y-axis label',/string,value=pm(0).labl,$
  /return_events)
  
rbase=widget_base(cbase1,/row)
range=lonarr(2)
range(0)=cw_field(rbase,title='Y min',/floating,value=pm(0).range(0),$
  /return_events)
range(1)=cw_field(rbase,title='Y max',/floating,value=pm(0).range(1),$
  /return_events)

rbase=widget_base(cbase1,/row)
ticks=cw_field(rbase,title='Y ticks',/integer,value=pm(0).ticks,$
  /return_events)

minor=cw_field(rbase,title='Y minor ticks',/integer,value=pm(0).minor,$
  /return_events)

rbase=widget_base(cbase1,/row)
plotio=cw_field(rbase,title='Log (1), Linear (0)',/integer,value=pm(0).plotio,$
  /return_events)

rbase=widget_base(cbase1,/row)
psym=cw_field(rbase,title='Plot symbol',/integer,value=pm(0).psym,$
  /return_events)

symsize=cw_field(rbase,title='Plot symsize',/floating,$
  value=pm(0).symsize,/return_events)

;rbase=widget_base(cbase1,/column)
;labl=widget_label(rbase,value=' ')
;labl=widget_label(rbase,value=' ')
;labl=widget_label(rbase,value='All panel changes:')

rbase=widget_base(cbase1,/row)
charthick=cw_field(rbase,title='Character thickness',/integer,$
  value=pm(0).charthick,/return_events)
  
charsize=cw_field(rbase,title='Character size',/floating,$
  value=pm(0).charsize,/return_events)
                  
widget_control,base_main,/realize

info=$
 {top:base_main,quit:quit,plotp:plotp,drplst:drplst,ypnl_proprtn:ypnl_proprtn,$
  ztitle:ztitle,dtp:dtp,varname:varname,labl:labl,range:range,$
  ticks:ticks,minor:minor,plotio:plotio,psym:psym,symsize:symsize,$
  charthick:charthick,charsize:charsize,$
  vname:list,pnlindx:pnlindx}

widget_control,base_main,set_uvalue=info,/no_copy

xmanager,'xplt_params',base_main
end