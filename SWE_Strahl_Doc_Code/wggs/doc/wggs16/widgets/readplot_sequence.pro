
;========================= readplot ==========================================

pro readplot,noseq=noseq

common sharewidglz,wlz
common wstuff,wst
common sharereadplotseq,mdir,mfltr,wbegin,result,$
  iseq,istart,tjd_start,plotsel
common sharewidg,WDGT
common shared,d

  
if keyword_set(noseq) eq 0 then begin
  tjd_seq=[tjd_start(0)+iseq,0l]
  wst.indate=string(long(pb5_ymd(tjd_pb5(tjd_seq(0),tjd_seq(1)))),format='(i8)')
  
endif

wst.lzdays=[wst.indate]
wst.lzindate=wst.lzdays(0)
  
whichdata=where(d.datype_input ne -1)

panelist
input,whichdata,err=err

if err ne '' then begin
print,err & print,'Check on data file availability.' & return
endif

WIDGET_CONTROL,WDGT.input_ymd,set_value=wst.surveydate
widget_control,WDGT.current_rdplt_seq,set_value=wst.indate

preselections,'selection'+plotsel,/plot
;stop
end


;================== readplot_sequence_event ==================================

pro readplot_sequence_event,event

common sharewidg,WDGT
common wstuff,wst
common sharewidglz,wlz
common sharereadplotseq,mdir,mfltr,wbegin,result,$
  iseq,istart,tjd_start,plotsel
common sharemlog6,mlbase,mlogwidg,$
  date,flnm,regime_name,plasma_regime,qwidg,q,qname,qval
common shared,d

offon=['Off','On']
  
CASE event.id OF

  WDGT.current_rdplt_seq:  begin
     WIDGET_CONTROL, WDGT.current_rdplt_seq, GET_VALUE = val
     print,val
     wst.indate=string(val(0),format='(i8.8)')     
     readplot,/noseq
           endcase
  WDGT.dayincre_rdplt_seq:
          
  WDGT.pltsel_rdplt_seq: begin
             plotsel=string(event.index,format='(i1)')
             preselections,'selection'+plotsel,/plot
          endcase   
  
  WDGT.fwd_bkwd_rdplt_seq: begin
    case event.value of
      'Forward':  begin
                     result=widget_info(WDGT.dayincre_rdplt_seq,/droplist_select)
                     iseq=iseq+result+1
                     readplot
                   endcase
                   
      'Backward': begin
                    result=widget_info(WDGT.dayincre_rdplt_seq,/droplist_select)
                    iseq=iseq-(result+1)
                    readplot
                  endcase                  
      'free_lun' : for ilun=100,128 do free_lun,ilun 
      'Quit': widget_control,event.top,/destroy
    endcase
  endcase  
  
  else: begin
    case event.value of
      'End of day' : begin
         print,event.value
         d.ndx(0,*)=d.ndx_orig(1,*)-100
         d.ndx(1,*)=d.ndx_orig(1,*)
         wst.minmax=0;1
         preselections,'selection'+plotsel,/plot
         wst.minmax=0
      endcase
    endcase  
  endcase
   
endcase

end    
  

;======================= readplot_sequence =================================

pro readplot_sequence

common sharewidg,WDGT
common wstuff,wst
common shared,d
common sharewidglz,wlz
common sharereadplotseq,mdir,mfltr,wbegin,result,$
  iseq,istart,tjd_start,plotsel
common sharemlog6,mlbase,mlogwidg,$
  date,flnm,regime_name,plasma_regime,qwidg,q,qname,qval

  
if xregistered('readplot_sequence') then begin
  WIDGET_CONTROL, WDGT.base_rdplt_seq,iconify=0   
  return
endif

WDGT.base_rdplt_seq = $
  WIDGET_BASE(TITLE = 'Automatic date increment/read/plot',/COLUMN)
rbase=widget_base(WDGT.base_rdplt_seq,/row)


WDGT.current_rdplt_seq=cw_field(rbase,title='Current',/return_events,$
   /string,xsize=8, ysize=1,/row)

WDGT.dayincre_rdplt_seq=widget_droplist(rbase,title='Day increment',$
  value=string(1+indgen(10),format='(i2)') )
  
rbase=widget_base(WDGT.base_rdplt_seq,/row)
WDGT.pltsel_rdplt_seq=widget_droplist(rbase,title='Plot selection',$
  value=string(indgen(4),format='(i2)') )

special_button=$
  cw_bgroup(rbase,label_left=' ',/return_name,row=1,$
  ['End of day'])
  
rbase=widget_base(WDGT.base_rdplt_seq,/row)
WDGT.fwd_bkwd_rdplt_seq=$
  cw_bgroup(rbase,label_left=' ',/return_name,row=1,$
  ['Forward','Backward','free_lun','Quit'])


               
WIDGET_CONTROL, WDGT.base_rdplt_seq, /REALIZE

XMANAGER, "readplot_sequence", WDGT.base_rdplt_seq , GROUP_LEADER = GROUP 

widget_control,WDGT.current_rdplt_seq,set_value=wst.surveydate
  
plotsel='0'
WIDGET_CONTROL,WDGT.pltsel_rdplt_seq,set_droplist_select=plotsel

iseq=0
istart=0
tjd_start=pb5_tjd(ymd_pb5(long(wst.surveydate)))

end


