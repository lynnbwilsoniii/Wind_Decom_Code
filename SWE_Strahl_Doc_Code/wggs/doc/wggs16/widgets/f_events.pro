
;===================== f_events =============================================

pro f_events_event,event 

common sharewidglz,wlz
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common drawf,pltype
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common shared,d
common sharefevents,fevents


case event.id of

  wlz.button(27) : begin
     case event.value of
     'Init':  begin
          fevents=replicate({type:wst.event_type,event:'',$
            spndt:'',pb5:lonarr(3),recn:0l,ispinbl:0l},14400)
          widget_control,wlz.field(22),set_value=' '
          print,'structure array fevents is initialized'
                    endcase
                    
     'Save':begin
      if keyword_set(fevents) eq 0 then return
      wev=where(fevents.spndt ne '')
      if wev(0) eq -1 then return
      filename=getenv('IDLSAV')+$
      strmid(swest.spndt,6,4)+strmid(swest.spndt,0,2)+strmid(swest.spndt,3,2)+$
      '.'+strmid(fevents(wev(0)).spndt,15,8)+'-'+$
      strmid(fevents(wev(n_elements(wev)-1)).spndt,15,8)+$
      '.fev'+wst.event_type
      fevents=fevents(where(fevents.spndt ne ''))
      save,filename=filename,fevents
      fevents=replicate({type:wst.event_type,event:'',$
            spndt:'',pb5:lonarr(3)},14400)
      widget_control,wlz.field(22),set_value=' '
      print,'fevents file saved: ',filename
          endcase
          
      'Restore' : begin
         result=pickfile(/read,get_path=getenv('IDLSAV'),path=getenv('IDLSAV'),$
               filter='*'+getenv('FEVENTTYPE')+'*',title='fevents Time Files',$
               /must_exist)
         print,result
         restore,result
         help,fevents & help,fevents,/str
         swest.indxF=-1
                  endcase 
                   
      'Show' : begin
         wset,wlz.win(0) & erase
         plt,/lzwin
         wev=where(fevents.spndt ne '')
         if wev(0) ne -1 then begin
           for i=0,n_elements(fevents)-1 do begin
             oplot_sec=pb5_sec(fevents(i).pb5)
             hrday_oplt=(oplot_sec-refsec)/3600.d
             if wst.ylog_lzwin then $
             yrange= [10.^wst.xyrange_lzwin(1),10.^wst.xyrange_lzwin(3)] else $
             yrange= [wst.xyrange_lzwin(1),wst.xyrange_lzwin(3)]              
             oplot,[hrday_oplt,hrday_oplt],yrange,color=175 
           endfor
         endif 
       endcase
       
       ' + ': begin  
         sz=size(fevents)
         if swest.indxF+1 ge sz(1)-1 then return
         swest.indxF=swest.indxF+1
         recn=fevents(swest.indxF).recn
         swest.ispinbl=fevents(swest.indxF).ispinbl
         wst.timsel='lz'
         print,'Show: selected recn ',recn
         print,'Show: selected spinbl ',swest.ispinbl
         wst.lz_is_read=0
                 
        proc_fw,wid=swest.lzwin(0),pltype=pltype, err=err,$
             F_integ=swest.F_integ                
             endcase
                                                
      ' - ': begin  
         sz=size(fevents)
         if swest.indxF-1 lt 0 then return
         swest.indxF=swest.indxF-1
         recn=fevents(swest.indxF).recn
         swest.ispinbl=fevents(swest.indxF).ispinbl
         wst.timsel='lz'
         print,'Show: selected recn ',recn
         print,'Show: selected spinbl ',swest.ispinbl
         wst.lz_is_read=0
                 
        proc_fw,wid=swest.lzwin(0),pltype=pltype, err=err,$
             F_integ=swest.F_integ                
               endcase 
      'Reset' : swest.indxF=-1
      
      'Parent' : widget_control,WDGT.lz_base_main,iconify=0
      
      'Quit' : widget_control,event.top,/destroy
                                                        
      endcase
      
       
    endcase 
    
      
   wlz.button(28) : begin
     if keyword_set(fevents) eq 0 then return
     wnxt=where(fevents.spndt eq '')
     fevents(wnxt(0)).spndt=swest.spndt
     fevents(wnxt(0)).pb5=swest.pb5
     fevents(wnxt(0)).recn=recn
     fevents(wnxt(0)).ispinbl=swest.ispinbl         
     widget_control,wlz.field(22),$
       set_value=strmid(swest.spndt,strlen(swest.spndt)-8,8)
     ;fevents(wnxt(0)).event=event.value(0)
     event_value=['positive slope','zero slope','negative slope']
     fevents(wnxt(0)).event=event_value(event.index)
                            endcase
   
   wlz.button(29): swest.F_integ=event.index
     
      
   wlz.button(30) : begin
    swest.plotf=event.index
    swest.iplotf=-1
                   endcase
                                             
   ;'F_integ':if swest.F_integ eq 0 then swest.F_integ=1 else swest.F_integ=0
 
endcase
     
end     


pro f_events 
common sharewidglz,wlz

if xregistered('f_events') then begin
  widget_control,wlz.base(5),iconify=0
  return
endif
     
wlz.base(5) = WIDGET_BASE(TITLE = 'SWE LZ Data Display', /COLUMN) 
     
cbase=widget_base(wlz.base(5),/column,/FRAME)
rbase=widget_base(cbase,/column)
wlz.button(27)=cw_bgroup(rbase,$
   ['Init','Save','Restore','Show',' + ',' - ','Reset','Parent','Quit'],$
   row=2,label_top='Select f-events',/return_name)
    
rbase=widget_base(cbase,/row)
     
case getenv('FEVENTTYPE') of
  '_fshck' : wlz.button(28)=widget_droplist(rbase,title='Event value',$
    value=['positive slope','zero slope','negative slope'])
  ;wlz.button(28)=cw_bgroup(rbase,['  OK  '],row=1,$
             ;     label_left='',/return_name)
  '' :  wlz.button(28)=cw_bgroup(rbase,['Event'],row=1,$
                  label_top='',/return_name) 
  else:                   
endcase
       
wlz.field(22)=cw_field(rbase,title=' ',/string,xsize=8,ysize=1,/row)
rbase=widget_base(cbase,/row)     
wlz.button(29)=$
  widget_droplist(rbase,title='',value=['F_integ off','F_integ on'])
     
wlz.button(30)=widget_droplist(rbase,title='Plot f',value=['Off','On'])
widget_control,wlz.base(5),/realize
XMANAGER, "f_events", wlz.base(5), GROUP_LEADER = GROUP 

end

     