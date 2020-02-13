;====================== crvfit functions =====================================

pro funct,x,c,f,pder
f=c(0)+c(1)*x
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
return
end


pro funct2,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end

;=========================== timeslice_plt ==================================
pro timeslice_plt,hardcopy=hardcopy,plot_reference=plot_reference,$
  select_reference=select_reference

common sharestrlappl,strlappl
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common ionkpstuff,ionkpflnm,ionkpdat
common magkpstuff,magkpflnm,magkpdat
common strahl2stuff,strahlflnm,strahldat
common shared,d
common wstuff,wst
common sharewidg,wa
common sharetimeslice,oplot_sec_slice
common sharestrlspctrm,strlbase,enstrlbase,pb5base
  
if xregistered('strahl_appl') then $
  WIDGET_CONTROL, strlappl.widgbase(0),iconify=0 else return

print,'timeslice_plt: ',strlappl.list(strlappl.sel)

if keyword_set(hardcopy) eq 0 then hardcopy=0
if hardcopy then begin 
  print,' ' & print,'making hardcopy... '
  set_plot,'ps',/interpolate
  clrtbl_indx,/hardcopy 
  pltfil=getenv('IDLSAV')+wst.print_flnm 
  print,'pltfil',pltfil
  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5,/color,$
     filename=pltfil;,bits_per_pixel=8
  ;device,/inches,/landscape,filename=pltfil
endif

npl=2
pos,npl,posit,xoff=0.2,xtop=0.84,yoff=0.1


if strlappl.sel eq 1 or strlappl.sel eq 3 then counts=1  else counts=0
if strlappl.sel eq 2 or strlappl.sel eq 3 then astrahl=1  else astrahl=0

strlind=d.ndx_buff(0,6) +lindgen(32) ;index range of one complete strahl spectrm
pb5ref=ymd_pb5(19941130l)
elapsec_strl0=pb5_elapsec(strahldat(strlind(0)).pb5,pb5ref)
elapsec_strl1=pb5_elapsec(strahldat(strlind(n_elements(strlind)-1)).pb5,pb5ref)
elapsec_ions=dblarr(n_elements(ionkpdat))
for i=0,n_elements(ionkpdat)-1 do $
  elapsec_ions(i)=pb5_elapsec(ionkpdat(i).tpb5,pb5ref)
wions=where(elapsec_ions ge elapsec_strl0 and  elapsec_ions le elapsec_strl1)
denslims=fltarr(2)
densavg=0.
if wions(0) ne -1 then begin
  mn=min(ionkpdat(wions).n,max=mx)
  denslims=[mn,mx]
  densavg=total(ionkpdat(wions).n)/n_elements(wions)
endif 

en=strahldat(strlind).en
enstep=strahldat(strlind).enstep
strlmaxdet=strahldat(strlind).strlmaxdet
strahl=strahldat(strlind).strl
antistrahl=strahldat(strlind).astrl
strahlwidth=strahldat(strlind).wstrl
antistrahlwidth=strahldat(strlind).strl
scimod=strahldat(strlind(0)).scimod
pb5=strahldat(strlind(0)).pb5
cts_f_strl_det,strlmaxdet,pb5,enstep,scimod,strl_cts_factor
fstrahl=strahl*strl_cts_factor
fantistrahl=antistrahl*strl_cts_factor
pb5_slice=strahldat(strlind(0)).pb5

minstrl=2
wstrlok=where(strahl gt minstrl)
en=en(wstrlok)
enstep=enstep(wstrlok)
strahl=strahl(wstrlok)
antistrahl=antistrahl(wstrlok)
strahlwidth=strahlwidth(wstrlok)
fstrahl=fstrahl(wstrlok)
fantistrahl=fantistrahl(wstrlok)


;------------ select either strahl or anti-strahl --------------------------
case astrahl of
0: begin
     wstrl=strahlwidth
     strl=strahl
     fstrl=fstrahl   
   endcase
1: begin
     wstrl=antistrahlwidth
     strl=antistrahl
     fstrl=fantistrahl
   endcase   
endcase


xrange=[0,1200]
xticks=6
xminor=2
charsize=1.25
title=pb5_ymdhms(strahldat(strlind(0)).pb5)
strlappl.current_datetime=title 
widget_control,strlappl.widg(7),set_value=strlappl.current_datetime


;-------- select and strore reference spectrum -----------------------------
if keyword_set(select_reference) ne 0 then begin   ;select_reference
  strlappl.ref_datetime=title
  strlappl.ref_pb5=strahldat(strlind(0)).pb5
  
  widget_control,strlappl.widg(6),set_value=strlappl.ref_datetime
  sdatref={$
  datetime:strlappl.ref_datetime,$
  pb5:strlappl.ref_pb5,$
  en:en,$
  fstrl:fstrl,$
  strl:strl,$
  wstrl:wstrl}
    
  strlappl.ref_flnm=$
    getenv('IDLSAV')+strcompress(strlappl.ref_datetime,/remove_all)
  save,sdatref,filename=strlappl.ref_flnm
  print,'file saved: '+ strlappl.ref_flnm 
endif  


;------------------ plot the stored reference spectrum ----------------------
if keyword_set(plot_reference) ne 0 then begin
  restore,strlappl.ref_flnm
  print,'file restored: '+ strlappl.ref_flnm
  title=sdatref.datetime
  pb5_slice=sdatref.pb5
  en=sdatref.en
  fstrl=sdatref.fstrl
  strl=sdatref.strl
  wstrl=sdatref.wstrl
endif


;------------- plot current spectrum - reference spectrum --------------------
if strlappl.curr_ref_diff and keyword_set(plot_reference) eq 0 then begin

  if strlappl.antistrl_as_ref eq 0 then begin
    ;use stored reference   
    restore,strlappl.ref_flnm
    print,'file restored: '+ strlappl.ref_flnm
    enref=sdatref.en
    strlref=sdatref.strl
    fstrlref=sdatref.fstrl
    
  endif else if strlappl.antistrl_as_ref eq 2 then begin 
    ;use current anti-strahl as ref  
    enref=en
    strl=strahl
    strlref=antistrahl
    fstrl=fstrahl
    fstrlref=fantistrahl
    widget_control,strlappl.widg(6),set_value=strlappl.current_datetime
  endif
  
  ;find difference between current and reference spectrum  
  enr=fltarr(n_elements(en))
  strlr=fltarr(n_elements(en))
  fstrlr=fltarr(n_elements(en))
  wstrlr=fltarr(n_elements(en))
  k=-1
  for i=0,n_elements(en)-1 do begin
    wref=where(enref eq en(i))
    if wref(0) ne -1 then begin
      k=k+1
      enr(k)=en(i)
      strlr(k)=strl(i) - strlref(wref(0))
      fstrlr(k)=fstrl(i) - fstrlref(wref(0))
      wstrlr(k)=wstrl(i)
    endif
  endfor
  en=enr(0:k)
  strl=strlr(0:k)
  fstrl=fstrlr(0:k)
  wstrl=wstrlr(0:k)
 
endif
;---------------- end current - reference spectrum -------------------------
    
stitl='Density = '+string(denslims(0),format='(f4.1)')+' : '+$
  string(denslims(1),format='(f4.1)')+ '   avg = '+$
  string(densavg,format='(f4.1)')
 
WIDGET_CONTROL, strlappl.widg(0), GET_VALUE=windw 
if hardcopy eq 0 then wset,windw
yrange=[0,40]

plot,$
     en,wstrl,$
     title=title(0),subtitle='',$
     xrange=xrange,  xticks=xticks,  xstyle=1,xtitle='energy (ev)',$
     xminor=xminor,xcharsize=0.001,$
     yrange=yrange,yticks=4,ystyle=1,$
     ytitle='width (deg, fwhm)',$
     yminor=2,$
     psym=2,symsize=0.6,/normal,position=posit(*,0),$
     ynozero=ynozero,xticklen=0.05,$
     charsize=charsize

;stop
case counts of
1: begin    ;plot counts
  yrange=[1.,500.]
  ytitle='strl cts'
  if astrahl then ytitle='anti-'+ytitle
  plot_io,$
     en,strl,$
     title='',subtitle=stitl,$
     xrange=xrange,  xticks=xticks,  xstyle=1,xtitle='energy (ev)',$
     xminor=xminor,$
     yrange=yrange,ystyle=1,$
     ytitle=ytitle,$
     psym=2,symsize=0.6,/normal,position=posit(*,1),/noerase,$
     ynozero=ynozero,xticklen=0.05,$
     charsize=charsize 
endcase

0: begin    ;plot f
  yrange=[1e-32,1e-24]
  ytitle='strl f'
  if astrahl then ytitle='anti-'+ytitle
  if strlappl.antistrl_as_ref eq 2 and strlappl.curr_ref_diff $
    then ytitle='strl f  - antistrl f'
  plot_io,$
     en,fstrl,$
     title='',subtitle=stitl,$
     xrange=xrange,  xticks=xticks,  xstyle=1,xtitle='energy (ev)',$
     xminor=xminor,$
     yrange=yrange,ystyle=1,$
     ytitle=ytitle,$
     psym=2,symsize=0.6,/normal,position=posit(*,1),/noerase,$
     ynozero=ynozero,xticklen=0.05,$
     charsize=charsize 
    oplot,en(sort(en)),strl_cts_factor(sort(en)),linestyle=1
    
   
  ;linear/parabolic fit to logf vs en
  x=en(sort(en))
  y=alog10(fstrl(sort(en)))
  z=strl(sort(en))
  
  WIDGET_CONTROL,strlappl.widg(12),set_droplist_select=strlappl.typefit
  if strlappl.typefit eq 1 then begin   ;parabolic fit
    ncoeff=3
    function_name='funct2'
    strlappl.enfit_lim=[200,800]  ;[0,1200]
    enslope=500.  ;600.
    atenergy=' @ '+string(enslope,format='(i3)') + ': '
  endif else if strlappl.typefit eq 0 then begin   ;linear fit
    ncoeff=2
    function_name='funct'
    strlappl.enfit_lim=[200,700] ;[0,75]  ;[200,800] ;[45,200] 
    atenergy=''
  endif 
     
  ;widget_control,strlappl.widg(10),get_value=val
  ;  strlappl.enfit_lim(0)=string(val(0),format='(i4)')
  widget_control,strlappl.widg(10),set_value=string(strlappl.enfit_lim(0),$
    format='(i4)') 
 ; widget_control,strlappl.widg(11),get_value=val
 ;   strlappl.enfit_lim(1)=string(val(0),format='(i4)')
  widget_control,strlappl.widg(11),set_value=string(strlappl.enfit_lim(1),$
    format='(i4)') 
  enlim=fix(strlappl.enfit_lim)
  welim=where(x ge enlim(0) and x le enlim(1))
  x=x(welim)
  y=y(welim)
  z=z(welim)
  strlappl.weight=1
  if strlappl.weight eq 1 then wt=1.+fltarr(n_elements(y)) else wt=z
  
  c=fltarr(ncoeff)
  c(0)=y(0)
  ycrvfit=curvefit(x,y,wt,c,sigc,function_name=function_name,chisq=chisq)
  if strlappl.typefit eq 0 then enfit=-alog10(exp(1.))/c(1) else $
  if strlappl.typefit eq 1 then enfit=-alog10(exp(1.))/(c(1)+c(2)*enslope) 
     
  if hardcopy eq 0 then begin
    oplot,x,10.^ycrvfit,color=wst.clr_orange
    xyouts,posit(0,1)+0.6*(posit(2,1)-posit(0,1)),$
      posit(1,1)+0.8*(posit(3,1)-posit(1,1)),/normal,$
      atenergy+string(enfit,format='(i4)')+'ev',color=wst.clr_orange
     
   xyouts,posit(0,1)+0.6*(posit(2,1)-posit(0,1)),$
      posit(1,1)+0.7*(posit(3,1)-posit(1,1)),/normal,$
      string(chisq,format='(f6.4)'),color=wst.clr_orange
       
      xyouts,posit(0,1)+0.6*(posit(2,1)-posit(0,1)),$
      posit(1,1)+0.9*(posit(3,1)-posit(1,1)),/normal,$
      'strahl mincts'+string(minstrl,format='(i2)'),color=wst.clr_orange
  endif else begin
    oplot,x,10.^ycrvfit
    xyouts,posit(0,1)+0.6*(posit(2,1)-posit(0,1)),$
      posit(1,1)+0.8*(posit(3,1)-posit(1,1)),/normal,$
      atenergy+string(enfit,format='(i4)')+'ev'
    
    xyouts,posit(0,1)+0.6*(posit(2,1)-posit(0,1)),$
      posit(1,1)+0.9*(posit(3,1)-posit(1,1)),/normal,$
      'strahl mincts'+string(minstrl,format='(i4)')   
  endelse   
endcase
endcase
     
if hardcopy then begin
  device,/close
  print,' ' & print,'printing hardcopy: ',wst.print_cmd
  spawn,wst.print_cmd
  set_plot,'x'
  clrtbl_indx
endif

if hardcopy eq 0 then begin
  wset,wa.win(0) & erase
  oplot_sec_slice=pb5_sec(pb5_slice)
  plt
  oplot_sec_slice=0
  wset,windw
endif
                    
end


;============================= strahl_appl_ev ================================

pro strahl_appl_event,event

common sharestrlappl,strlappl
common sharewidg,wa
common wstuff,wst
common shared,d

case event.id of
  strlappl.widg(1) : begin
    case event.value of
      'Quit' : begin
        wst.timeslice=0
        WIDGET_CONTROL, event.top, /DESTROY
               endcase
      
      'Print' : begin
         wst.hardcopy=1 
         wst.printer=wst.printer_bw
         wst.print_flnm=wst.print_flnm_bw
         wst.print_cmd=wst.print_cmd_bw 
         if wst.strlappl_intrvl then strlappl_intrvl,/hardcopy else $
         timeslice_plt,/hardcopy
         wst.hardcopy=0
                   endcase
                   
      'Parent' : WIDGET_CONTROL, wa.base(0),iconify=0             
   
      'HELP' :
    endcase   
                     endcase
  
  strlappl.widg(4) : begin
    case event.value of
    ' + ' : d.ndx_buff(0,6)=d.ndx_buff(0,6)+32
    ' - ' : d.ndx_buff(0,6)=d.ndx_buff(0,6)-32
    endcase
    timeslice_plt
                     endcase
     
  strlappl.widg(3) : begin
    strlappl.sel=event.index
    timeslice_plt
                     endcase
    
  strlappl.widg(5) : begin
    strlappl.antistrl_as_ref=event.index
    if strlappl.antistrl_as_ref eq 0 then timeslice_plt,/select_reference
    if strlappl.antistrl_as_ref eq 1 then timeslice_plt,/plot_reference
                     endcase
  
  strlappl.widg(8) : begin
    case event.value of
      'Current' : timeslice_plt
    endcase
                     endcase
  
  strlappl.widg(13) : wst.strlappl_intrvl=event.index
                                        
  strlappl.widg(9) : strlappl.curr_ref_diff=event.index
  
  strlappl.widg(10) : begin
    widget_control,strlappl.widg(10),get_value=val
    strlappl.enfit_lim(0)=string(val(0),format='(i4)')
                      endcase 
                      
  strlappl.widg(11) : begin
    widget_control,strlappl.widg(11),get_value=val
    strlappl.enfit_lim(1)=string(val(0),format='(i4)')
                      endcase
                      
  strlappl.widg(12) : begin
    strlappl.typefit=event.index
    timeslice_plt
                      endcase
                                                                             
endcase
end


;============================= strahl_appl ================================


pro strahl_appl

common sharestrlappl,strlappl
common wstuff,wst


wst.timeslice=1
if xregistered('strahl_appl') then begin
  WIDGET_CONTROL, strlappl.widgbase(0),iconify=0
  WIDGET_CONTROL,strlappl.widg(3),set_droplist_select=strlappl.sel
  widget_control,strlappl.widg(7),set_value=strlappl.current_datetime 
  widget_control,strlappl.widg(6),set_value=strlappl.ref_datetime
  WIDGET_CONTROL,strlappl.widg(9),set_droplist_select=strlappl.curr_ref_diff 
  WIDGET_CONTROL,strlappl.widg(5),set_droplist_select=strlappl.antistrl_as_ref
  widget_control,strlappl.widg(10),set_value=string(strlappl.enfit_lim(0),$
    format='(i4)') 
  widget_control,strlappl.widg(11),set_value=string(strlappl.enfit_lim(1),$
    format='(i4)')
  WIDGET_CONTROL,strlappl.widg(12),set_droplist_select=strlappl.typefit
  WIDGET_CONTROL,strlappl.widg(13),set_droplist_select=wst.strlappl_intrvl   
  return
endif

if keyword_set(strlappl) eq 0 then $
  strlappl={offon:['Off','On'],$
    list:['strahl f','strahl counts',$
    'anti-strahl f','anti-strahl counts'],$
    sel:0,current_datetime:'',ref_datetime:'',ref_pb5:lonarr(3),ref_flnm:'',$
    current:1,curr_ref_diff:0,antistrl_as_ref:2,enfit_lim:[200,800],$
    weight:1,typefit:0,$
    widgbase:lonarr(10),widg:lonarr(20)}
  

strlappl.widgbase(0) = WIDGET_BASE(TITLE = 'SWE strahl spectrum',/column)

cbase2=widget_base(strlappl.widgbase(0),/column)
rbase=widget_base(cbase2,/row)
strlappl.widg(1)=cw_bgroup(rbase,['Parent','HELP','Quit','Print'],$
  row=1,/return_name)

strlappl.widg(4)=cw_bgroup(rbase,label_left='Increment',$
  [' + ',' - '],row=1,/return_name)
  
rbase=widget_base(cbase2,/row)

strlappl.widg(3)=widget_droplist(rbase,title=' ',value=strlappl.list)

strlappl.widg(9)=widget_droplist(rbase,title='Subtract reference',$
  value=strlappl.offon)
  
rbase=widget_base(cbase2,/row)
strlappl.widg(8)=cw_bgroup(rbase,['Current'],row=1,/return_name)
  
strlappl.widg(7)=cw_field(rbase,title=' ',/string,xsize=17, ysize=1,/row)

strlappl.widg(13)=widget_droplist(rbase,title='Do interval',$
  value=strlappl.offon)
        
rbase=widget_base(cbase2,/row)
strlappl.widg(5)=widget_droplist(rbase,title='Reference',$
  value=['Select and store','Plot stored reference','Use current anti-strahl'])

strlappl.widg(6)=cw_field(rbase,title=' ',/string,xsize=17, ysize=1,/row)

  
rbase=widget_base(cbase2,/row)

strlappl.widg(10)=cw_field(rbase,title='Energy fit range (ev):  min',/string,$
  xsize=4, ysize=1,/row,/return_events)

strlappl.widg(11)=cw_field(rbase,title='max',/string,$
  xsize=4, ysize=1,/row,/return_events)

strlappl.widg(12)=widget_droplist(rbase,title='Fit',$
  value=['Linear','Parabola'])
           
cbase1=widget_base(strlappl.widgbase(0),/column)
x_size = 400         
y_size = 600  
strlappl.widg(0) = WIDGET_DRAW(cbase1,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size, YSIZE = y_size) 

  
WIDGET_CONTROL, strlappl.widgbase(0), /REALIZE

WIDGET_CONTROL,strlappl.widg(3),set_droplist_select=strlappl.sel
widget_control,strlappl.widg(7),set_value=strlappl.current_datetime 
widget_control,strlappl.widg(6),set_value=strlappl.ref_datetime 
WIDGET_CONTROL,strlappl.widg(9),set_droplist_select=strlappl.curr_ref_diff
WIDGET_CONTROL,strlappl.widg(5),set_droplist_select=strlappl.antistrl_as_ref 
widget_control,strlappl.widg(10),set_value=string(strlappl.enfit_lim(0),$
  format='(i4)')
widget_control,strlappl.widg(11),set_value=string(strlappl.enfit_lim(1),$
    format='(i4)')    
WIDGET_CONTROL,strlappl.widg(12),set_droplist_select=strlappl.typefit
WIDGET_CONTROL,strlappl.widg(13),set_droplist_select=wst.strlappl_intrvl

XMANAGER, "strahl_appl", strlappl.widgbase(0), GROUP_LEADER = GROUP
    
end