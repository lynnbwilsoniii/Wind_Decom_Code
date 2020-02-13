pro plotf_survey_seq_event,event

common sharepltfsurv3,fbase,draw,windw,button1,button2,hardcopy,pos0,$
  pltindx,nplts,pos
common sharefplt,fpltc,fpltF
common sharewidglz,wlz
common wstuff,wst
common swestuff,swest
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common drawf,pltype
common sharelevelzero,pltwin_frmt,oplot_sec

CASE event.id of

  button1 : begin
  
  case event.value of
  
  'LZ display': WIDGET_CONTROL, wlz.base(3), /show
                
  'Hardcopy' : begin
                 wst.hardcopy=1
                 hardcopy=1
                 wst.printer=wst.printer_clr
                 wst.print_flnm=wst.print_flnm_clr
                 wst.print_cmd=wst.print_cmd_clr 
                 set_plot,'ps',/interpolate
                 clrtbl_indx,/hardcopy 
                 pltfil=getenv('IDLSAV')+wst.print_flnm 
                 print,'pltfil',pltfil
                 device,/inches,/landscape,filename=pltfil                  
               endcase
  
  'Quit': WIDGET_CONTROL, event.top, /DESTROY
  
  'Plot survey' : begin
      erase
      if hardcopy eq 0 then wset,windw         
      pos0=[0.10,0.10,0.90,0.32]
      oplot_sec=0.
      plt,/lzwin,pos_overide=pos0
      pltindx=-1
      print,'survey plotted'
                  endcase
  
  'Plot distrb' : begin
      set_plot,'x'
      swest.ispinbl=swest.ispinbl+1
      if swest.ispinbl gt vsmjf.n_spins-1 then begin
        swest.ispinbl=0 
        if recn lt fh.nmf-1 then recn=recn+1
        wst.lz_is_read=0
      endif
      widget_control,wlz.field(11),set_value=swest.ispinbl
      widget_control,wlz.field(10),set_value=recn
      wst.timsel='lz'
      print,'wlz.button(15) ',$
        event.value, swest.ispinbl,wst.timsel
      swest.ilzplt=swest.ilzplt+1
      pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
      proc_fw,wid=pltwin,pltype=pltype
      if hardcopy then set_plot,'ps'
          
      if hardcopy eq 0 then begin
        wset,windw
        dev=0
      endif else dev=1  
      pltindx=pltindx+1
      if pltindx eq nplts and hardcopy then begin
         device,/close
         print,' ' & print,'printing hardcopy: ',wst.print_cmd
         spawn,wst.print_cmd
         set_plot,'x'
         clrtbl_indx
         hardcopy=0 
         wst.hardcopy=0
         stop
      endif  else if pltindx eq nplts and hardcopy eq 0 then stop
      k=nplts/2
      r=0.6
      pltsiz=(0.94-pos0(3))/k
      pos=fltarr(4)
      px0=0.05
      py0=pos0(3)+0.05
      
      if pltindx lt k then $
      pos=[px0+pltindx*pltsiz*r,py0+pltindx*pltsiz,$
           px0+pltindx*pltsiz*r+pltsiz,py0+pltindx*pltsiz+pltsiz] $
      else $
      pos=[px0+(pltindx+1)*pltsiz*r,py0+(2*k-1-pltindx)*pltsiz,$ 
           px0+(pltindx+1)*pltsiz*r+pltsiz,py0+(2*k-1-pltindx)*pltsiz+pltsiz]

      if pltindx eq 0 then $     
         plot_io,fpltF.x,fpltF.y,xrange=fpltF.xrange,xstyle=1,$
         xticks=fpltF.xticks,$
         xtitle=' ',xtickv=fpltF.xtickv,$
         ytitle=' ',$
         position=pos,device=dev,yrange=fpltF.yrange,$
         ystyle=1,/noerase,xminor=fpltF.xminor $
      else plot_io,fpltF.x,fpltF.y,xrange=fpltF.xrange,xstyle=1,$
         xticks=fpltF.xticks,$
         xtitle=' ',xtickv=fpltF.xtickv,$
         ytitle=' ',$
         position=pos,device=dev,yrange=fpltF.yrange,$
         ystyle=1,/noerase,xminor=fpltF.xminor,$
         xcharsize=0.001,ycharsize=0.001    
     print,'distrb plotted'
           endcase  
         
  
  endcase
  endcase 
  
endcase  
end





pro plotf_survey_seq

common sharepltfsurv3,fbase,draw,windw,button1,button2,hardcopy,pos0,$
  pltindx,nplts,pos
common wstuff,wst

if xregistered('plotf_survey_seq')  then begin
  widget_control,button2,set_droplist_select=nplts
  widget_control,fbase,iconify=0
endif

fbase = WIDGET_BASE(TITLE = 'Plot f survey sequence',/COLUMN)

rbase=widget_base(fbase,/row)
button1=cw_bgroup(rbase,$
 ['LZ display','Hardcopy','Quit','Plot survey','Plot distrb'],row=1,$
 /return_name) 

rbase=widget_base(fbase,/row)
xsize=0.90*1100
ysize=0.90*850
draw = WIDGET_DRAW(rbase,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = xsize, YSIZE = ysize)

WIDGET_CONTROL, fbase, /REALIZE
WIDGET_CONTROL, draw, GET_VALUE=windw

hardcopy=0

nplts=8

XMANAGER, "plotf_survey_seq", fbase, GROUP_LEADER = GROUP
          
end