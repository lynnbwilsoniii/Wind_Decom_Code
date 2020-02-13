pro plt_glintcts,savefile

common phasemod2,phiveis_m2,theveis_m2,phistrl_m2,thestrl_m2,vunit_m2
common phasemod1,phiveis_m1,theveis_m1,phistrl_m1,thestrl_m1,vunit_m1
common plotstuff,minmax

charsize=1.0

color=[175,125,225]
oppdet=[5,4,3,2,1,0]

nfiles=size(savefile)
scimode=intarr(nfiles(1))
date=strarr(nfiles(1))
for m=0,nfiles(1)-1 do begin

  print,savefile(m)
  restore,savefile(m)
  date(m)=strmid(savefile(m),strlen(getenv('BACKGPATH')),8)

  s=size(sumcts)
  ndets=s(1) & nsteps=s(2) & nsectors=s(3) & numspins=s(4)
  if nsectors eq 6 then scimode(m)=1 else if nsectors eq 8 then scimode(m)=2 $
    else stop

  print,' ' & print,'mode ',scimode 
  ;'instrhk={datim:'',elec_ion:0,tmmode:0,scimode:0,ebias1:0,ibias1:0,ebias2:0,
  ;ibias2:0,calpwr:0}'
  for i=0,numspins-1 do print,i,instrhk(i),format='(i4,a30,8i4)'

  case m of
  0: begin
       avgcts0=fltarr(ndets,nsteps*nsectors)
       avgcts0(*,*)=avgcts
     endcase
  1: begin
       avgcts1=fltarr(ndets,nsteps*nsectors)
       avgcts1(*,*)=avgcts
     endcase
  endcase

endfor

  pos,ndets,posn
  noerase=[0,1,1,1,1,1]
  detmx=fltarr(6)
   
  for idet=0,ndets-1 do begin
    xrange=[0,360]
    xticks=6
    if idet eq ndets-1 then xcharsize=1.0 else xcharsize=0.001
    if idet eq 0 then title='SWE VEIS Background Counts' else title=' '
    xtitle='spin phase angle phi'
    if nfiles(1) gt 1 then begin
      mx0=max(avgcts0(idet,*)) & mx1=max(avgcts1(idet,*))
      detmx(idet)=max([mx0,mx1])
      mx0=max(avgcts0) & mx1=max(avgcts1)
      mx=max([mx0,mx1])
    endif else begin
      detmx(idet)=max([avgcts0(idet,*)])
      mx=max([avgcts0])
    endelse
    if minmax then yrange=[0,1.1*detmx(idet)] else yrange=[0,1.1*mx]
    yticks=4
    ytitle='avg cts'
    plot,xrange,yrange,/nodata,xstyle=1,xrange=xrange,xticks=xticks,$
        ystyle=1,yrange=yrange,yticks=yticks,position=posn(*,idet),$
        noerase=noerase(idet),xcharsize=xcharsize,charsize=1.25*charsize,$
        title=title,ytitle='det '+string(idet,format='(i1)'),$
        xtitle='spin phase'       

    for m=0,nfiles(1)-1 do begin
  
      if scimode(m) eq 1 then phi=reform(phiveis_m1(idet,*,*)) else $
      if scimode(m) eq 2 then phi=reform(phiveis_m2(idet,*,*))

      if m eq 0 then bcts=reform(avgcts0(idet,*))
      if m eq 1 then bcts=reform(avgcts1(idet,*))

      oplot,phi,bcts,psym=4,symsize=0.4,color=color(m)

      if m eq 0 then begin      
        sz=size(phi)
        ns=sz(2)
        for is=0,ns-1 do begin
          oplot,[phi(0,is),phi(0,is)],!y.crange,linestyle=1,color=color(m) 
          xyouts,phi(7,is),!y.crange(0)+0.8*total(!y.crange),/data,$
            string(is,format='(i1)'),color=color(m)
        endfor

      endif

      print,'idet,  m ',idet,m 
      ;stop
    endfor
    
  endfor

  for m=0,nfiles(1)-1 do begin
        if m eq 0 then xyouts,0.2,0.05,/norm,charsize=1.25*charsize,$
          date(m)+'  scimode '+string(scimode(m),format='(i1)'),color=color(m)
        if m eq 1 then xyouts,0.5,0.05,/norm,charsize=1.25*charsize,$
          date(m)+'  scimode '+string(scimode(m),format='(i1)'),color=color(m)
  endfor

end






;---------------- backg_plot_event -------------------------------------

pro backg_plot_event,event

common sharewidgb,wbp
common stuff,ndets,nsteps,nsects,nspins,files,selectfiles,iselect
common phasemod2,phiveis_m2,theveis_m2,phistrl_m2,thestrl_m2,vunit_m2
common phasemod1,phiveis_m1,theveis_m1,phistrl_m1,thestrl_m1,vunit_m1
common plotstuff,minmax

;-----------------------------------------------------------------------------
;                   begin event response sections 
;-----------------------------------------------------------------------------
CASE event.id OF

wbp.button(0): begin
  case event.value of
     'Quit' : WIDGET_CONTROL, event.top, /DESTROY
  endcase
endcase

wbp.button(1): begin
   iselect=iselect+1
   selectfiles(iselect)=event.value
               endcase
  
wbp.button(2): begin
  print,event.value
  case event.value of
   
    'Counts spectrum by det vs spin phase' : begin
      wfiles=where(selectfiles ne -1)
      if wfiles(0) eq -1 then begin
        print,'Select one or two files' & return
      endif
      print,selectfiles(wfiles(*)),files(selectfiles(wfiles(*)))
      plt_glintcts,files(selectfiles(wfiles(*)))
      selectfiles=-1+intarr(n_elements(files))
    endcase

    'Min-max all detectors' : if minmax eq 0 then minmax=1 else minmax=0
      

    else : 
  endcase
endcase

endcase


end


pro backg_plot

common sharewidgb,wbp
common stuff,ndets,nsteps,nsects,nspins,files,selectfiles,iselect
common phasemod2,phiveis_m2,theveis_m2,phistrl_m2,thestrl_m2,vunit_m2
common phasemod1,phiveis_m1,theveis_m1,phistrl_m1,thestrl_m1,vunit_m1
common plotstuff,minmax

;define widget structure
  wbp={wbp_widgets,base:lonarr(100),slider:lonarr(100),button:lonarr(100),$
    field:lonarr(100),menu:lonarr(100),draw:lonarr(100),win:lonarr(100),$
     win_xsize:lonarr(100),win_ysize:lonarr(100)}

loadct,18

minmax=1

phasem1       
phasem2   ;spin phase angle in fixed payload coords

files=findfile(getenv('BACKGPATH')+'*backg.idlsav')
print,files

selectfiles=-1+intarr(n_elements(files))
iselect=-1

n=n_elements(files)

wbp.base(0) = WIDGET_BASE($
  TITLE = 'VEIS background files',$
  /column);main base

rbase1=widget_base(wbp.base(0),/row)
cbase1=widget_base(rbase1,/column)
wbp.draw(0) = WIDGET_DRAW(cbase1,/BUTTON_EVENTS,/FRAME, $
  UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = 650, YSIZE = 700)

cbase=widget_base(rbase1,/column)
rbase=widget_base(cbase,/row)
wbp.button(2)=cw_bgroup(rbase,/return_name,$
  ['Counts spectrum by det vs spin phase',$
   'Min-max all detectors'],row=2)
wbp.button(0)=cw_bgroup(rbase, ['Quit'],row=1,/return_name)

wbp.button(1)=cw_bgroup(cbase,/nonexclusive,/frame,files,$
  row=n_elements(files),/return_index,$
  label_top='Select one or two files',$
  /scroll,y_scroll_size=450,x_scroll_size=300)


WIDGET_CONTROL, wbp.base(0), /REALIZE
WIDGET_CONTROL, wbp.draw(0), GET_VALUE=windw
wbp.win(0)=windw

XMANAGER, "backg_plot", wbp.base(0), GROUP_LEADER = GROUP  ;hand off to manager

end
