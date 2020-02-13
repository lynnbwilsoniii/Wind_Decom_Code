
;================================ lzdspl_opt_event ============================

pro lzdspl_opt_event,event

common sharewidg,WDGT
common swestuff,swest
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common sharelzdspl,opts_initval

specie=swest.specie

widget_control,event.top,get_uvalue=info,/no_copy
help,info,/str

case event.id of
   WDGT.swelzdspl_quit : begin
     widget_control,event.top,/destroy
     return
   endcase
   
   WDGT.swelzdspl_reset : begin
     swest.strlf_veisf   = opts_initval(0)
     swest.hidepoints    = opts_initval(1)
     swest.c_labels      = opts_initval(2)
     swest.gap_interpol  = opts_initval(3)
     swest.subtrbkg01    = opts_initval(4)
     swest.delete        = opts_initval(5)
     swest.subtrbkg01=where(swest.noyes eq swest.subtrbkg)
     widget_control,WDGT.swelzdspl_opts,set_value=opts_initval
     
     swest.autoseq=info.autoseq_init
     widget_control,WDGT.swelzdspl_sequence,set_value=swest.autoseq
     
     swest.c_decade=info.c_decade_init
     widget_control,WDGT.swelzdspl_contrs,set_value=swest.c_decade
     
     swest.specie_selct=info.specie_selct_init
     widget_control,WDGT.swelzdspl_speciefld,set_value=specie(swest.specie_selct) 
   endcase         
    
   WDGT.swelzdspl_opts : begin
      case event.value of
        'Strlf+veisf' : $
          if swest.strlf_veisf eq 0 then swest.strlf_veisf=1 $
          else swest.strlf_veisf=0 
                                                 
        'Hide data points' : $
          if swest.hidepoints eq 1 then swest.hidepoints=0 $
          else swest.hidepoints=1
              
        'Contour labels' : $
          if swest.c_labels eq 1 then swest.c_labels=0 $
          else swest.c_labels=1
              
        'Interp 0, 180' : $
          if swest.gap_interpol eq 1 then $
          swest.gap_interpol=0 else swest.gap_interpol=1
          
        'Subtract bckgrd': begin
          if swest.subtrbkg eq 'No' then swest.subtrbkg='Yes'$
          else swest.subtrbkg='No'
          swest.subtrbkg01=where(swest.noyes eq swest.subtrbkg)
        endcase  
          
        'Sun glint points excluded' : $
          if swest.delete eq 0 then swest.delete=1 $
          else swest.delete=0
      endcase
   endcase
            
   WDGT.swelzdspl_sequence : begin
      case event.value of
        'l-m-r': swest.autoseq=1
        'reference-m-r': swest.autoseq=2
        'reference-m-difference': swest.autoseq=3
        'off': swest.autoseq=0
     endcase
   endcase 
                    
   WDGT.swelzdspl_contrs :  begin
      WIDGET_CONTROL, event.id, GET_VALUE = val
      swest.c_decade=val(0)
      WIDGET_CONTROL, event.id, SET_VALUE = val
   endcase 
   
   ;WDGT.swelzdspl_specie : begin
   ;   if swest.specie_selct eq 0 then swest.specie_selct=1 $
   ;      else swest.specie_selct=0      
   ;   specie=swest.specie
   ;   widget_control, WDGT.swelzdspl_speciefld,$
   ;     set_value=specie(swest.specie_selct)
   ;endcase 
   
   else:  
endcase

widget_control,WDGT.swelzdspl_opts,get_value=val
info.opts_currval=val
widget_control,event.top,set_uvalue=info,/no_copy
end



;=============================== lzdspl_opt ==================================

pro lzdspl_opt

common sharewidg,WDGT
common swestuff,swest
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common sharelzdspl,opts_initval

specie=swest.specie

if xregistered('lzdspl_opt') then return 

if keyword_set(opts_initval) eq 0 then begin  ;store initial widget states
  opts_initval=[$
    swest.strlf_veisf,$
    swest.hidepoints,$
    swest.c_labels,$
    swest.gap_interpol,$
    swest.subtrbkg01,$
    swest.delete  ]  
endif
        
info={$
    opts_currval:intarr(n_elements(opts_initval)),$             
    autoseq_init:swest.autoseq,$
    c_decade_init:swest.c_decade,$
    specie_selct_init:swest.specie_selct}  

WDGT.swelzdspl_base=$
  WIDGET_BASE(TITLE = 'Distribution Function Display Options',/COLUMN)

base=widget_base(WDGT.swelzdspl_base,/column,/frame)
rbase=widget_base(base,/row)
WDGT.swelzdspl_quit=widget_button(rbase,value='Quit')

WDGT.swelzdspl_reset=widget_button(rbase,value='Reset')

rbase=widget_base(base,/row) 
info.opts_currval=[$
   swest.strlf_veisf,$
   swest.hidepoints,$
   swest.c_labels,$
   swest.gap_interpol,$
   swest.subtrbkg01,$
   swest.delete  ]   
WDGT.swelzdspl_opts=cw_bgroup(rbase,label_top='Options',/return_name,row=2,$
  /nonexclusive,set_value=info.opts_currval,$
  ['Strlf+veisf','Hide data points','Contour labels','Interp 0, 180',$
  'Subtract bckgrd','Sun glint points excluded'])

rbase=widget_base(base,/row)
WDGT.swelzdspl_sequence=$
  cw_bgroup(rbase,label_top='Plot window sequence',$
  ['off','l-m-r','reference-m-r','reference-m-difference'],$
  /exclusive,row=1,/return_name,set_value=swest.autoseq) 

rbase=widget_base(base,/row)
WDGT.swelzdspl_contrs=$
  cw_field(rbase,title='Contours: d[log10(f)]',/return_events,$
  /floating,xsize=5,ysize=1,/row,value=swest.c_decade)

cbase=widget_base(base,/column)
rbase=widget_base(cbase,/row)
WDGT.swelzdspl_specie=$
  cw_bgroup(rbase,label_left='',['Select species'],row=1,/return_name)
WDGT.swelzdspl_speciefld=$
  cw_field(rbase,/string,xsize=5,ysize=1,title=' ',/row,$
  value=specie(swest.specie_selct))

rbase=widget_base(cbase,/row)
WDGT.swelzdspl_mode=$
  cw_field(rbase,/integer,xsize=1,ysize=1,title='Mode',/row,/noedit)
WDGT.swelzdspl_eleion_sweep=$
  cw_field(rbase,/string,xsize=9,ysize=1,title=' ',/row,/noedit)

widget_control, WDGT.swelzdspl_speciefld, set_value = specie(swest.specie_selct)
widget_control, WDGT.swelzdspl_mode, set_value = vsmjf.scimode
widget_control, WDGT.swelzdspl_eleion_sweep, $
  set_value = swest.swpmd(vsmjf.eleion_sweep)
widget_control, WDGT.swelzdspl_contrs,set_value=swest.c_decade
            
WIDGET_CONTROL, WDGT.swelzdspl_base, /REALIZE

widget_control,WDGT.swelzdspl_base,set_uvalue=info,/no_copy

XMANAGER, "lzdspl_opt", WDGT.swelzdspl_base , GROUP_LEADER = GROUP 

end


