
;procedure to select time axis interval (or point) with mouse button
;  and cause time value to register in field and slider widgets 

pro draw_main_ev,event

common sharewidg,WDGT
common left,leftpressed,xydata0_left
common shared,d
common wstuff,wst
;common mpstuff,mflnm,pflnm,mdat,pdat,d.refsec


case event.press of
1: begin                                         ;left button pressed
     wst.xydata=dev_to_data(event.x,event.y)         ;convert to data coords
     if total(wst.xydata) eq 0 then return
     
     indexbegin,wst.xydata(0)  ;find beginning index in each selected data set
     hour_hms,wst.xydata(0),hms,/mod24 
     wst.hms=hms(0)   ;convert to hh:mm:ss
     wst.pb5=sec_pb5(d.refsec+wst.xydata(0)*double(3600))
     wst.ymd=pb5_ymd(wst.pb5)
     wst.pb5_begin=wst.pb5
     wst.ymd_begin=wst.ymd
     wst.hms_begin=wst.hms
     
     leftpressed=1
     xydata0_left=wst.xydata(0)
     refpb5=sec_pb5(d.refsec)
     print,'selected begin time ',$
       'wst.xydata(0) ',wst.xydata(0),'  ',$
       'refpb5 ',refpb5,'   ',$
       'wst.hms ',wst.hms,'  ','wst.ymd ',wst.ymd,'   ',$
       'wst.pb5_begin ',wst.pb5

     wst.timsel='survey'
     wst.lz_is_read = 0
     ;print,'wst.timsel ',wst.timsel
     if wst.timemark_offon then begin
       w1=where(wst.timemark ne 0,nw1)
       if nw1 ne 0 ne -1 or nw1 ne n_elements(wst.timemark) $
         then wst.timemark(nw1)=wst.xydata(0)
       plt
     endif
       
   endcase

2: begin                                          ;middle button pressed
     time_scale                                  ;activate timescale widget
     hour_hms,wst.xydata(0),hms,lhms=lhms,/mod24
     wst.hms=hms(0)
     ymd0=string(pb5_ymd(wst.pb5_begin),format='(i8)')
     widget_control,WDGT.time_scale_begin,set_value=ymd0+'  '+lhms

     wst.xydata=dev_to_data(event.x,event.y)         ;convert to data coords
     if total(wst.xydata) eq 0 then return
     
     indexend,wst.xydata(0)  ;find ending index in each selected data set

     hour_hms,wst.xydata(0),hms,lhms=lhms,/mod24
     wst.hms=hms(0)           ;convert to hh:mm:ss
     wst.pb5=sec_pb5(d.refsec+wst.xydata(0)*double(3600))
     wst.ymd=pb5_ymd(wst.pb5)
     wst.pb5_end=wst.pb5
     wst.ymd_end=wst.ymd
     wst.hms_end=wst.hms
     
     refpb5=sec_pb5(d.refsec)
     print,'selected end time ',$
       'wst.xydata(0) ',wst.xydata(0),'  ',$
       'refpb5 ',refpb5,'   ',$
       'wst.hms ',wst.hms,'  ','wst.ymd ',wst.ymd,'   ',$
       'wst.pb5_end ',wst.pb5  

     ymd1=string(pb5_ymd(wst.pb5_end),format='(i8)')
     widget_control,WDGT.time_scale_end,set_value=ymd1+'  '+lhms       
   endcase
else:

endcase
end


