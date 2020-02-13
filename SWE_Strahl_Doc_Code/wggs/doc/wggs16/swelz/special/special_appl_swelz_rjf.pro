;----------- special_appl_swelz_rjf_event -------------------------------------

pro special_appl_swelz_rjf_event,event

common sharewidg,WDGT
common swestuff,swest
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shared,d

help,event,/str

CASE event.id of

  WDGT.special_appl_swelz_quit: begin
    case event.value of
      'Parent' : WIDGET_CONTROL, WDGT.swelz_base_main, /show
      'Quit' : WIDGET_CONTROL, event.top, /DESTROY
    endcase
  endcase
endcase
end


;------------------- special_appl_swelz_rjf -----------------------------------

pro special_appl_swelz_rjf

common sharewidg,WDGT
common swestuff,swest
common wstuff,wst
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shared,d
common share_example1,wdgt_example1
common share_example2,wdgt_example2a,list,wdgt_example2b


if xregistered('special_appl_swelz_rjf') then begin
  widget_control,WDGT.base_special_appl_swelz,/show
  return
endif


WDGT.base_special_appl_swelz = WIDGET_BASE(/COLUMN,TITLE=getenv('SPECIAL_APPL'))

cbase=widget_base(WDGT.base_special_appl_swelz,/column)

rbase=widget_base(cbase,/row)
WDGT.special_appl_swelz_quit=cw_bgroup(rbase,/row,/return_name,label_left=' ',$
  ['Parent','Quit'])


rbase=widget_base(cbase,/row)
rbase1=widget_base(rbase,/row,event_pro='example1')
wdgt_example1=cw_bgroup(rbase1,/row,/return_name,label_left='example1',$
    ['appla','applb','applc'])

rbase1=widget_base(rbase,/row,event_pro='example2') 
list=['a','b','c']   
wdgt_example2a=widget_droplist(rbase1,title='example2',value=list)
wdgt_example2b=cw_field(rbase1,title='date',/return_events,/string,xsize=8,$
  ysize=1,/row)
       
widget_control, WDGT.base_special_appl_swelz, /REALIZE

widget_control,wdgt_example2b,set_value=wst.lzdate


XMANAGER, "special_appl_swelz_rjf", WDGT.base_special_appl_swelz, $
  GROUP_LEADER = GROUP  ;hand off to manager
    
end