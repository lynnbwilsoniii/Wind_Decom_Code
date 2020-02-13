pro redf_integ_event,event

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shareredf,sbase,button1,button2,draw1,draw2,field1,field2,$
  ndistribs,xc1,xc2,sgnslop,x1,F1,x2,F2
common sharefevents,fevents
common sharewidg,wa
common sharewidglz,wlz
common wstuff,wst
common swestuff,swest
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common shareredfintg,xwinrc,xrangec,ywinrc,yrangec,lbl1,lbl2,xsize,ysize,$
                     xwinrF,xrangeF,ywinrF,yrangeF,xwin,ywin,vxl,vxr,ivxl,ivxr
common sharefparr,Fvprp,vygrid
common sharelabl,labl


!p.charsize=1.25

CASE event.id of

  button1 : begin
  
  case event.value of
  
  'LZ display': begin
                  WIDGET_CONTROL, wlz.base(3), /show
                endcase
                
  'Hardcopy' : begin
                 wst.hardcopy=1
                 wst.printer=wst.printer_bw
                 wst.print_flnm=wst.print_flnm_bw
                 wst.print_cmd=wst.print_cmd_bw 
                 clrtbl_indx,/hardcopy 
                 
               endcase
  
  'Quit': WIDGET_CONTROL, event.top, /DESTROY
  
                    
  'Do Poisson': begin
         fplot,xsize,ysize,pltype=[0,1],labl1=labl1,labl2=labl(0),$
            xoplt=[xc1,xc2]
         nstats=25
         forshk=replicate({type:wst.event_type,$
            spndt:'',pb5:lonarr(3),recn:0l,spinbl:0l,xc1:0.,xc2:0.,q:'',$
            ndistribs:0l,sgnslop:0l,x1:0.,F1:0.,x2:0.,F2:0.},nstats)
         for ndistribs=0,nstats-1 do begin
         if ndistribs eq 0 then proc_fw,pltype=[0,1],err=err,F_integ=1 else $ 
           proc_fw,pltype=[0,1],err=err,F_integ=1,statistcs=1
         WIDGET_CONTROL, field1,set_value=ndistribs
                     
         forshk(ndistribs).spndt=swest.spndt
         forshk(ndistribs).pb5=swest.pb5         
         forshk(ndistribs).recn=recn
         forshk(ndistribs).spinbl=swest.ispinbl
         forshk(ndistribs).ndistribs=ndistribs
         forshk(ndistribs).sgnslop=sgnslop
         forshk(ndistribs).x1=x1
         forshk(ndistribs).F1=F1
         forshk(ndistribs).x2=x2
         forshk(ndistribs).F2=F2
         forshk(ndistribs).xc1=xc1
         forshk(ndistribs).xc2=xc2
         help,forshk(ndistribs),/str
         print,'ndistribs,sgnslop,x1,F1,x2,F2 ',ndistribs,sgnslop,x1,F1,x2,F2
         answ='' & read,answ & if answ ne '' then stop 
         
         
       endfor
       
       if swest.Poisson_save then begin
         flnm=getenv('LOCALUSR')+'foreshock/data/'+pb5_yeartime(swest.pb5)+$
           '.stat'+getenv('FEVENTTYPE')
         save,filename=flnm(0),forshk
         print,'file saved: ',flnm
       endif  
                 endcase            
              
  else:
  endcase

        endcase
 
   button2: swest.Poisson_save=event.index
   
   draw1: begin
     case event.press of
     1: begin
          print,'left button pressed'
          print,xwinrc,xwin,xrangec
          print,event.x,event.y
          vx=xrangec(0)+(event.x-xwin(0))*$
            ((xrangec(1)-xrangec(0))/(xwin(1)-xwin(0)))
          vy=yrangec(0)+(event.y-ywin(0))*$
            ((yrangec(1)-yrangec(0))/(ywin(1)-ywin(0)))
          print,'vx ',vx
          print,'vy ',vy
          vxl=vx
          
          ;WIDGET_CONTROL, draw1, GET_VALUE=windw
          ;wset,windw
          ;fplot,xsize,ysize,pltype=[0,1],labl1=labl1,labl2=labl2,xoplt=[vxl]
          ;xc1=vxl
     
          sz=size(Fvprp)
          x=xrangec(0)+indgen(sz(1))*(xrangec(1)-xrangec(0))/(sz(1)-1)
          
          if vx lt 0 then begin
            w1=where(x gt vx)
            ivxl=w1(0)
          endif else begin   
            w1=where(x lt vx)
            ivxl=w1(n_elements(w1)-1)
          endelse      
      endcase
       
     2: begin
          print,'middle button pressed'
          print,xwinrc,xwin,xrangec
          print,event.x,event.y
          vx=xrangec(0)+(event.x-xwin(0))*$
            ((xrangec(1)-xrangec(0))/(xwin(1)-xwin(0)))
          vy=yrangec(0)+(event.y-ywin(0))*$
            ((yrangec(1)-yrangec(0))/(ywin(1)-ywin(0)))
          print,'vx ',vx
          print,'vy ',vy          
          vxr=vx
         
          sz=size(Fvprp)
          x=xrangec(0)+indgen(sz(1))*(xrangec(1)-xrangec(0))/(sz(1)-1)
          
          if vx lt 0 then begin
            w1=where(x gt vx)
            ivxr=w1(0)
          endif else begin   
            w1=where(x lt vx)
            ivxr=w1(n_elements(w1)-1)
          endelse              
         
          ;NOTE: vxl and vxr can be either increasing or decreasing in value;
          ;however, vx1 and vx2 are always increasing in absolute magnitude
           
          if vxr lt 0 then begin
            ivx1=max([ivxl,ivxr]) 
            ivx2=min([ivxl,ivxr])  
            vx1=max([vxl,vxr]) 
            vx2=min([vxl,vxr])   
          endif else if vxr gt 0 then begin 
            ivx1=min([ivxl,ivxr]) 
            ivx2=max([ivxl,ivxr]) 
            vx1=min([vxl,vxr]) 
            vx2=max([vxl,vxr]) 
          endif 

          WIDGET_CONTROL, draw1, GET_VALUE=windw1
          wset,windw1 
          hcpy=wst.hardcopy
          xc1=vx1
          xc2=vx2 
                  
          fplot,xsize,ysize,pltype=[0,1],labl1=labl1,labl2=labl(0),$
            xoplt=[vx1,vx2],oplotvx=1
          wst.hardcopy=hcpy
           
          start:        
          WIDGET_CONTROL, draw2, GET_VALUE=windw2
          if wst.hardcopy eq 0 then wset,windw2                 
          ;wne0=where([Fvprp(ivx1,*),Fvprp(ivx2,*)] ne 0)
          wne0=where(Fvprp(ivx1,*) ne 0 and Fvprp(ivx2,*) ne 0)
          mx=max([Fvprp(ivx1,wne0),Fvprp(ivx2,wne0)],min=mn)
          if wne0(0) ne -1 then begin
            if wst.hardcopy then begin               
              orange=0
              green=0
            endif else begin
              orange=wst.clr_orange
              green=wst.clr_green
            endelse
 
 
      if wst.hardcopy then begin 
        pltfil=getenv('IDLSAV')+'idl_redf_integ.ps' ;wst.print_flnm
        print,' ' & print,'making hardcopy..... ' 
        set_plot,'ps'
        device,/inches,$
        xoffset=1.0,yoffset=1.0,xsize=5.,ysize=7.,filename=pltfil,/color 
      endif  
 
            plot,vygrid(wne0)/1e8,Fvprp(ivx2,wne0),yrange=[mn,mx],$
              xrange=[0,xrangec(1)],xstyle=1,xticks=2,xminor=3,/nodata,$
              xtitle='v perpendicular',ytitle='vperp * f',$
              position=[0.2,0.1,0.9,0.9],subtitle=labl(0),charsize=1.25,$
              title='vpara='+string(vx1,format='(f5.1)')+$
                '  !cpos slope when solid > dashed (integrated)'
            oplot,vygrid(wne0)/1e8,Fvprp(ivx1,wne0),$
              color=green,linestyle=1
            oplot,vygrid(wne0)/1e8,Fvprp(ivx2,wne0)
            
           
            absdev=0   ;abs(vx1-vx2)
            if vx1 gt 0 then $
            wslice=$
            where(vxm ge (vx1-absdev)*1e8 and vxm le (vx2+absdev)*1e8,nwslice) $
            else $
            wslice=$
            where(vxm le (vx1+absdev)*1e8 and vxm ge (vx2-absdev)*1e8,nwslice)
            if nwslice(0) gt 1 then $
              oplot,vym(wslice)/1e8,replicate(0.1*mx,nwslice),psym=1,$
              color=orange
              
        
   if wst.hardcopy then begin   
     device,/close 
     set_plot,'x'
     print,' ' & print,'printing hardcopy: ',wst.print_cmd
     spawn, 'lpr '+pltfil
     wst.hardcopy=0
     clrtbl_indx
     goto,start
   endif        
            
          endif
          
         
          
        
          
        endcase
     
     else:
     endcase
          endcase 
endcase

!p.charsize=1.00

end

;======================= main ================================================

pro redf_integ,xsize=xsiz,$
       ysize=ysiz,$
       labl1=labl1,labl2=labl2

common shareredf,sbase,button1,button2,draw1,draw2,field1,field2,$
  ndistribs,xc1,xc2,sgnslop,x1,F1,x2,F2
common wstuff,wst
common shareredfintg,xwinrc,xrangec,ywinrc,yrangec,lbl1,lbl2,xsize,ysize,$
                     xwinrF,xrangeF,ywinrF,yrangeF,xwin,ywin,vxl,vxr,ivxl,ivxr
common sharelabl,labl

labl=labl2
  
xsize=1.5*xsiz
ysize=1.5*ysiz

if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0

if xregistered('redf_integ') and wst.hardcopy(0) eq 0 then begin
     WIDGET_CONTROL, field1,set_value=ndistribs     
     
     WIDGET_CONTROL, draw1, GET_VALUE=windw1
     if wst.hardcopy eq 0 then wset,windw1
     fplot,xsize,ysize,pltype=[0,1],labl1=labl1,labl2=labl2
     xwin=xsize*xwinrc
     ywin=ysize*ywinrc
     
     WIDGET_CONTROL, sbase,iconify=0
     return
endif


;---------------- set up draw widget ---------------------------------------

sbase = WIDGET_BASE($
  TITLE = 'Electron Foreshock Analysis',/COLUMN)

rbase=widget_base(sbase,/row)
button1=cw_bgroup(rbase,$
 ['LZ display','Hardcopy','Quit','Do Poisson'],$
   row=1,/return_name) 

button2=$
       widget_droplist(rbase,title='',$
       value=['Save Poisson off','Save Poisson on'])
       
field1=cw_field(rbase,title='ndistribs',/return_events,/long,xsize=3,ysize=1,/row)

rbase=widget_base(sbase,/row)
draw1 = WIDGET_DRAW(rbase,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = xsize, YSIZE = ysize)

draw2 = WIDGET_DRAW(rbase,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = xsize, YSIZE = ysize)

ndistribs=0
WIDGET_CONTROL, field1,set_value=ndistribs
  
WIDGET_CONTROL, sbase, /REALIZE


;------------------ do plot -----------------------------------------------

WIDGET_CONTROL, draw1, GET_VALUE=windw1
if wst.hardcopy eq 0 then wset,windw1

     fplot,xsize,ysize,pltype=[0,1],labl1=labl1,labl2=labl2

xwin=xsize*xwinrc
ywin=ysize*ywinrc


if wst.hardcopy(0) eq 0 then XMANAGER, "redf_integ", sbase, GROUP_LEADER = GROUP
print,'xsize,ysize ',xsize,ysize




end
