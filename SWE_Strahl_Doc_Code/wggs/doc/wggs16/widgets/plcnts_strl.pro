
pro plotcts_strl

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest
common sharewidgd,wd

case swest.splotsel of
    'strlsteps'      :strl_steps
    'strlphi'        :strl_phi
    'fstrlphi'       :strl_phi,/ff
    'strlpit'        :strl_pit
    'fstrlpit'       :strl_pit,/ff
    'fstrlveis'      :strahl_veis
    'fstrlphth'      :strl_phth,/phth_image
    'fstrlpitch'     :strl_phth,/pitch
endcase
swest.splotsel_last=swest.splotsel
swest.istrlstep=vsmjf.strlstep(swest.ispinbl)  ;current strahl step

if vsmjf.scimode eq 6 then begin  
  widget_control,wd.field(2),$
    set_value=string(vsmjf.bxyz_status(swest.ispinbl),format='(i1)') 
  widget_control,wd.field(1),$
    set_value=string(vsmjf.bxyz_ind(swest.ispinbl),format='(i3)') 
  widget_control,wd.field(6),set_value=$
    string(360.-float(vsmjf.bxyz_phase(swest.ispinbl))*(360./4096.),$
    format='(i3)')

  print,'vsmjf.bxyz_phase(swest.ispinbl) ',vsmjf.bxyz_phase(swest.ispinbl)
  print,'Bxyz ',vsmjf.bxyz_bx(swest.ispinbl),vsmjf.bxyz_by(swest.ispinbl),$
    vsmjf.bxyz_bz(swest.ispinbl)
  phi=atan(float(vsmjf.bxyz_by(swest.ispinbl))/$
    float(vsmjf.bxyz_bx(swest.ispinbl)))/!dtor
  print,'phi of Bxyz at By=0 crossing (deg) ',phi 
endif
  
  widget_control,wd.field(7),set_value=swest.wchmagdat(swest.mag_3s_kp)
  
  widget_control,wd.field(0),set_value=recn
  widget_control,wd.field(3),set_value=swest.ispinbl
  widget_control,wd.field(4),set_value=$
  string(volt_en_strl(vsmjf.strlstep(swest.ispinbl),/en),format='(i4)')+'ev'
  

end


;=========================== plcnts_event =================================

pro plcnts_strl_event,event

common sharewidg,WDGT
common sharewidgd,wd
;common sharewidg,wa
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common detset,detseta,detsetb,detset
common strlstuff, strlflnm,strldat
common swestuff,swest
common wstuff,wst

print,'pro plcnts_event'
help,event,/str
help,wd,/str


CASE event.id OF

   wd.field(0) : begin
     widget_control,wd.field(0),get_value=val
     recn=val(0)
     read_rec,date_time   ;read record and display time from header
     proc_rec           ;process lz record
     plotcts_strl  
                 endcase
     

   wd.button(0): begin
    case event.value of
      'Parent' : swe_levelzero  ;  WIDGET_CONTROL, WDGT.swelz_base_main, /show
      'HELP' : $
        xdisplayfile,getenv('WGGSBASE')+$
        'swe/lz/help_swe_strahl_lz.txt',width=100
      'Quit' : WIDGET_CONTROL, event.top, /DESTROY
    endcase
                 endcase

  wd.button(3): begin
      case event.value of
      'Counts vs phi index': begin 
          swest.splotsel='strlsteps'       
          strl_steps
                          endcase
      'Counts vs phi' : begin
          swest.splotsel='strlphi'
          strl_phi
                            endcase

      'f vs phi' : begin
          swest.splotsel='fstrlphi'
          strl_phi,/ff
                               endcase
   
      'Counts vs pa ' : begin
          swest.splotsel='strlpit'
          strl_pit
                            endcase 

      'f vs pa ' : begin
          swest.splotsel='fstrlpit'
          strl_pit,/ff
                              endcase 
      'f(phi,theta) (def)' : begin
          swest.splotsel='fstrlphth'
          strl_phth,/phth_image
                              endcase 

      'f vs pa (all det''s)' : begin
          swest.splotsel='fstrlpitch'
          strl_phth,/pitch
                              endcase 

      'Compare strahl & veis' : begin
          swest.splotsel='fstrlveis'
          strahl_veis
                               endcase 

   
      'Xloadct' : xloadct

      'Restore clrs' : clrtbl_indx


      'Hardcopy_bw' : begin
         wst.hardcopy=1
         wst.printer=wst.printer_bw
         wst.print_flnm=wst.print_flnm_bw
         wst.print_cmd=wst.print_cmd_bw 
         plotcts_strl
       endcase
   

      'Hardcopy_clr' : begin
         wst.hardcopy=1
         wst.printer=wst.printer_clr
         wst.print_flnm=wst.print_flnm_clr
         wst.print_cmd=wst.print_cmd_clr 

         plotcts_strl
       endcase

    endcase
                endcase
  
wd.button(1): begin
   case event.value of
     0: detset=fix(detseta)
     1: detset=fix(detsetb)
   endcase
   plotcts_strl
   print,detset
endcase
  

wd.button(2): begin
    case event.value of
       '  +  ': begin
          swest.ispinbl=(swest.ispinbl + 1) ;< (swest.nspins-1)
          if swest.ispinbl gt swest.nspins-1 then begin
              swest.ispinbl=0
              recn_new=recn+1
              if recn_new ge 1 and recn_new le fh.nmf then begin
                recn=recn_new
                ;widget_control,wd.field(0),set_value=recn
                read_rec,date_time   ;read record and display time from header
                proc_rec           ;process lz record
              endif 
            endif
          print,'selected ispin ',swest.ispinbl

          plotcts_strl
                endcase

       '  -  ': begin
          swest.ispinbl=(swest.ispinbl - 1) ;> 0
          if swest.ispinbl lt 0 then begin
              swest.ispinbl=swest.nspins-1
              recn_new=recn-1
              if recn_new ge 1 and recn_new le fh.nmf then begin
                recn=recn_new
                ;widget_control,wd.field(0),set_value=recn
                read_rec,date_time   ;read record and display time from header
                proc_rec           ;process lz record
              endif 
          endif
          print,'selected ispin ',swest.ispinbl
          plotcts_strl         
                endcase

       'reset' : begin
          swest.ispinbl=0
          plotcts_strl         
                endcase
    endcase
                endcase 

wd.button(4): begin
    case event.value of
             0 : recn_new=recn+1                            

             1 : recn_new=recn-1                            
          endcase
          if recn_new ge 1 and recn_new le fh.nmf then begin
            recn=recn_new
            ;widget_control,wd.field(0),set_value=recn
            read_rec,date_time   ;read record and display time from header
            proc_rec           ;process lz record
            plotcts_strl           
          endif 
        endcase

wd.button(5): begin
    point1:
    case event.value of
             0 : recn_new=recn+1
                  
             1 : recn_new=recn-1                  
          endcase
    if recn_new ge 1 and recn_new le fh.nmf then begin
       recn=recn_new
       ;print,'reading recn ',recn
       read_rec,date_time
       proc_rec
       w=where(vsmjf.strlstep eq swest.istrlstep)
       if w(0) eq -1 then goto,point1
       swest.ispinbl=w(0)
       plotcts_strl
    endif      
             endcase

wd.button(6): begin
  if swest.strlsunmask eq 0 then swest.strlsunmask=1 else swest.strlsunmask=0
  widget_control,wd.field(8),set_value=wst.offon(swest.strlsunmask)
  plotcts_strl
              endcase        
endcase

end

;=========================== plcnts_strl ==================================

pro plcnts_strl

common sharewidgd,wd
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common detset,detseta,detsetb,detset
common swestuff,swest
common wstuff,wst

if swest.splotsel eq '' then swest.splotsel='fstrlphth'
if xregistered('plcnts_strl') then begin
  wset,wd.win(0) & wshow
  read_rec,date_time   ;read record and display time from header
  proc_rec           ;process lz record
  plotcts_strl
  return
endif

;define widget structure
  wd={wd_widgets,base:lonarr(10),slider:lonarr(10),button:lonarr(10),$
    field:lonarr(10),menu:lonarr(10),draw:lonarr(10),win:lonarr(10),$
     win_xsize:lonarr(10),win_ysize:lonarr(10)}


;help,vsmjf.strl
s=size(vsmjf.strl)
swest.nstrldets=s(1) & swest.nstrlsteps=s(2) & swest.nspins=s(3)
swest.istrldet=0 & swest.istrlstep=0   ;initialize

if keyword_set(swest.ispinbl) eq 0 then swest.ispinbl=0

wd.base(0) = WIDGET_BASE(TITLE = 'SWE Strahl Counts Data  ', /COLUMN) ;main base

rbase1=widget_base(wd.base(0),/row)

cbase1=widget_base(rbase1,/column)
 
if (NOT keyword_set(x_size)) THEN x_size = 700        
if (NOT keyword_set(y_size)) THEN y_size = 750

wd.draw(0) = WIDGET_DRAW(cbase1,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size, YSIZE = y_size)

cbase2=widget_base(rbase1,/column)

wd.button(0)=cw_bgroup(cbase2,['Parent','HELP','Quit'],row=1,/return_name)

wd.button(3)=cw_bgroup(cbase2,$
  ['Counts vs phi','f vs phi',$
   'Counts vs pa ','f vs pa ',$
   'f(phi,theta) (def)','f vs pa (all det''s)',$
   'Counts vs phi index',$   
   'Compare strahl & veis',$
   'Xloadct','Restore clrs',$
   'Hardcopy_bw',$
   'Hardcopy_clr'],row=7,/return_name)

if vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or $
   vsmjf.scimode eq 4 or vsmjf.scimode eq 6 then begin
  detseta=['0','2','4','6','8','10']
  detsetb=['1','3','5','7','9','11']
  detlist=['0 2 4 6 8 10',  '1 3 5 7 9 11']
  wd.button(1)= cw_bgroup(cbase2,label_top='Strahl detectors (mode'+$
    string(vsmjf.scimode,format='(i1)')+')',$
    detlist,row=1)
endif else if vsmjf.scimode eq 2 then begin
  detseta=['0','1','2','3']   ;corresponds to ['1','4','7','10']
  detsetb=''
  detlist=['0 1 2 3']
  wd.button(1)= cw_bgroup(cbase2,label_top='Strahl detectors (mode2)',$
    detlist,row=1)
endif 
detset=fix(detseta)

if vsmjf.scimode eq 6 then begin
  rbase=widget_base(cbase2,/row)
  wd.field(2)=cw_field(rbase,title='Bxyz tracking',/string,xsize=1,/row,$
    /noedit)
  wd.field(1)=cw_field(rbase,title='Bxyz index',/string,xsize=3,/row,/noedit)
  rbase=widget_base(cbase2,/row)
  wd.field(6)=cw_field(rbase,title='360 - Bxyz_phase',$
    /string,xsize=3,/row,/noedit)
endif    

rbase=widget_base(cbase2,/row)
wd.field(5)=cw_field(rbase,title='B phi,theta',/string,/row,xsize=8,/noedit)
wd.field(7)=cw_field(rbase,title=' ',/string,/row,xsize=8,/noedit)

rbase=widget_base(cbase2,/row)
wd.button(2)=cw_bgroup(rbase,$
  label_top='Increment spin',['  +  ','  -  ','reset'],row=1,/return_name)
wd.field(3)=cw_field(rbase,title='spin',/long,xsize=1,/column,/noedit)

rbase=widget_base(cbase2,/row)
wd.button(4)=cw_bgroup(rbase,$
  label_top='Increment recn',['  +  ','  -  '],row=1)
wd.field(0)=cw_field(rbase,title='recn',/long,xsize=8,/column,/return_events)

wd.field(4)=cw_field(cbase2,title='strahl step',/string,/row,xsize=6,/noedit)
wd.button(5)=cw_bgroup(cbase2,$
  label_left='Search for current strahl step',['  +  ','  -  '],row=1)

rbase=widget_base(cbase2,/row)
wd.button(6)=cw_bgroup(rbase,label_left=' ','Sun mask',row=1)
wd.field(8)=cw_field(rbase,title=' ',/string,/row,xsize=3,/noedit)
    
WIDGET_CONTROL, wd.base(0), /REALIZE
WIDGET_CONTROL, wd.draw(0), GET_VALUE=windw
wd.win(0)=windw
wset,wd.win(0)

wd.win_xsize(0)=x_size
wd.win_ysize(0)=y_size

widget_control,wd.field(0),set_value=recn
widget_control,wd.field(3),set_value=swest.ispinbl

if vsmjf.scimode eq 6 then begin
  widget_control,wd.field(2),$
    set_value=string(vsmjf.bxyz_status(swest.ispinbl),format='(i1)')  
  widget_control,wd.field(1),$
    set_value=string(vsmjf.bxyz_ind(swest.ispinbl),format='(i3)')  
  widget_control,wd.field(6),set_value=$
    string(360.-float(vsmjf.bxyz_phase(swest.ispinbl))*(360./4096.),$
    format='(i3)')
  widget_control,wd.field(8),set_value=wst.offon(swest.strlsunmask)
    
endif


widget_control,wd.field(7),set_value=swest.wchmagdat(swest.mag_3s_kp)
      
widget_control,wd.field(4),set_value=$
  string(volt_en_strl(vsmjf.strlstep(swest.ispinbl),/en),format='(i4)')+'ev'

read_rec,date_time   ;read record and display time from header
proc_rec           ;process lz record

if keyword_set(swest.splotsel_last) eq 0 then $
  swest.splotsel='fstrlphth' else swest.splotsel=swest.splotsel_last

swest.splotsel_last=swest.splotsel
if keyword_set(detset) eq 0 and vsmjf.scimode eq 2 then detset=fix(detseta)

;if vsmjf.scimode ne 2 then $
;    widget_control,wd.button(1),set_value=string(vsmjf.scimode,format='(i3)')
    
plotcts_strl

XMANAGER, "plcnts_strl", wd.base(0), GROUP_LEADER = GROUP  ;hand off to manager

END
