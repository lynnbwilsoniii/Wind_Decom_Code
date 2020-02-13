;============================= iseef_event,event ============================

pro iseef_event,event

common plt,y,xyrange,ylog,xywindow,xysize,ev_pnl,minmax,hardcopy,newticks,cscale
common tsel,xydata,hms
common sharefhdr,fflnm,lunf,tmsec,frec1,frec2,pltype
common sharewidg,WDGT
common shared,d
common wstuff,wst
common swestuff,swest
common shareisee,iseest
common sharelevelzero,pltwin_frmt,oplot_sec

print,'iseef_event :'
help,event,/str

CASE event.id of

  WDGT.iseef_draw0 : begin
     case event.press of
        1: begin
             ;wa_draw0_ev,event
             draw_main_ev,event
             oplot_sec=wst.xydata(0)*3600.d +d.refsec
             wset,swest.win(0) & erase
             plt,/lzwin 
             swest.ilzplt=swest.ilzplt+1
             pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
             if wst.lz_is_read eq 0 then readisee_f
             fredf_isee
             wset,swest.win(pltwin) 
             if keyword_set(pltype) eq 0 then pltype=[0,2]
             fplot_isee,swest.win_xsize(1),swest.win_ysize(1),$
             pltype=pltype,hardcopy=hardcopy 
           endcase
         else:
     endcase      
             
  endcase
  
  WDGT.iseef_draw1 : begin
    case event.press of
    1: begin
         if wst.lz_is_read eq 0 then readisee_f
         fredf_isee
         wset,swest.win(1) & if keyword_set(pltype) eq 0 then pltype=[0,2]
         fplot_isee,swest.win_xsize(1),swest.win_ysize(1),pltype=pltype,$
           hardcopy=hardcopy
         oplot_sec=iseest.tpoint1*3600.d +d.refsec
         wset,swest.win(0) & erase
         plt,/lzwin
       endcase
    else:
    endcase  
               endcase

  WDGT.iseef_draw2 : begin
    case event.press of
    1: begin
         if wst.lz_is_read eq 0 then readisee_f
         
         fredf_isee
         wset,swest.win(2) & if keyword_set(pltype) eq 0 then pltype=[0,2]
         fplot_isee,swest.win_xsize(1),swest.win_ysize(1),pltype=pltype,$
           hardcopy=hardcopy
         oplot_sec=iseest.tpoint1*3600.d +d.refsec
         wset,swest.win(0) & erase
         plt,/lzwin
       endcase
    else:
    endcase 
               endcase


  WDGT.iseef_draw3 : begin
    case event.press of
    1: begin
         if wst.lz_is_read eq 0 then readisee_f
         fredf_isee
         wset,swest.win(3) & if keyword_set(pltype) eq 0 then pltype=[0,2]
         fplot_isee,swest.win_xsize(1),swest.win_ysize(1),pltype=pltype,$
           hardcopy=hardcopy
         oplot_sec=iseest.tpoint1*3600.d +d.refsec
         wset,swest.win(0) & erase
         plt,/lzwin  
       endcase
    else:
    endcase 
               endcase

  WDGT.iseef_fpltype : begin
    print,'event.value ',event.value
    case event.value of
      'f-contours' : pltype=[0]
      'Reduced F' : pltype=[1]
      'f-cuts' : pltype=[2]
      'f, F' : pltype=[0,1]
      'f, f-cuts (default)' :  pltype=[0,2]
      'f, F, f-cuts' : pltype=[0,1,2]
      else :
    endcase
                endcase

  WDGT.iseef_timeincre : begin
    print,'event.value ',event.value
    case event.value of
      '  +  ' : readisee_f,increment=+1
      '  -  ' : readisee_f,increment=-1
      else :
    endcase
    swest.ilzplt=swest.ilzplt+1
    pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
    fredf_isee
    wset,swest.win(pltwin) 
    if keyword_set(pltype) eq 0 then pltype=[0,2]
    fplot_isee,swest.win_xsize(1),swest.win_ysize(1),$
    pltype=pltype,hardcopy=hardcopy
    oplot_sec=iseest.tpoint1*3600.d +d.refsec
    wset,swest.win(0) & erase
    plt,/lzwin 
                endcase

  WDGT.iseef_quit : begin
    print,'event.value ',event.value
    case event.value of
      'HELP distr func' : $
        xdisplayfile,getenv('WGGSBASE')+'isee/help_isee_f_data_display.txt',$
        width=100
      'HELP moments' : $ 
        xdisplayfile,$
        getenv('WGGSBASE')+'isee/help_isee_survey_data_display.txt',width=100
      'Hardcopy' : wst.hardcopy=1
      'Parent' :   current_appl=xregistered('wanal')
      'Quit' :  WIDGET_CONTROL, event.top, /DESTROY
      else :
    endcase
                endcase
  else:

endcase

end


;============================= iseef ====================================

pro iseef,group=group

common sharewidg,WDGT
common sharefhdr,fflnm,lunf,tmsec,frec1,frec2,pltype
common shared,d
common wstuff,wst
common swestuff,swest

idatype=where(d.datype eq 'isee_moments')
fflnm=d.flnm(idatype)

if fflnm eq '' then return

if xregistered('iseef') then begin
  wset,swest.win(0) & erase
  plt,/lzwin    
  return
endif

print,'iseef :'

;------------------- set up main base widgets --------------------------------

WDGT.iseef_base_main = WIDGET_BASE(TITLE = 'ISEE f_Data Display', /COLUMN)  ;main base

;wtext=widget_text(WDGT.iseef_base_main,$
;  value='Select type of plot and click inside desired window.')

x_size = 1100;965       
y_size = 170  

rbase0=widget_base(WDGT.iseef_base_main,/row)
WDGT.iseef_draw0 = WIDGET_DRAW(rbase0,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size, YSIZE = y_size)

x_size = 940         
y_size = 560 

rbase0=widget_base(WDGT.iseef_base_main,/row)
cbase1=widget_base(rbase0,/column)
WDGT.iseef_draw1 = WIDGET_DRAW(cbase1,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size)

cbase2=widget_base(rbase0,/column)
WDGT.iseef_draw2 = WIDGET_DRAW(cbase2,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size)

cbase3=widget_base(rbase0,/column)
WDGT.iseef_draw3 = WIDGET_DRAW(cbase3,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size)

cbase4=widget_base(rbase0,/column)
WDGT.iseef_fpltype=cw_bgroup(cbase4,label_top='Select plot',$
      ['f-contours',$
       'Reduced F',$
       'f-cuts',$
       'f, F',$
       'f, f-cuts (default)',$
       'f, F, f-cuts'], row=6,/return_name)

WDGT.iseef_timeincre=cw_bgroup(cbase4,label_top='Increment time index',$
      ['  +  ','  -  '],row=1,/return_name)

WDGT.iseef_quit=cw_bgroup(cbase4,$
  ['HELP distr func','HELP moments','Hardcopy','Parent','Quit'], $
  row=5,/return_name)


WIDGET_CONTROL, WDGT.iseef_base_main, /REALIZE

WIDGET_CONTROL, WDGT.iseef_draw0, GET_VALUE=windw
swest.win(0)=windw
wset,swest.win(0) & erase
plt,/lzwin  
  
WIDGET_CONTROL, WDGT.iseef_draw1, GET_VALUE=windw
swest.win(1)=windw

WIDGET_CONTROL, WDGT.iseef_draw2, GET_VALUE=windw
swest.win(2)=windw

WIDGET_CONTROL, WDGT.iseef_draw3, GET_VALUE=windw
swest.win(3)=windw

swest.win_xsize(1:3)=x_size/3
swest.win_ysize(1:3)=y_size

XMANAGER, "iseef", WDGT.iseef_base_main, GROUP_LEADER = GROUP  ;hand off to manager



END

