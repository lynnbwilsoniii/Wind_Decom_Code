


;======================== plt ==================================================
pro plt,group=group,spikesout=spikesout,idlsave=idlsv,$
lzwin=lzwn,pos_overide=pos_overide

common shared,d
common wstuff,wst
common swestuff,swest
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
common sharetimeslice,oplot_sec_slice

if keyword_set(spikesout) eq 0 then spikesout=0   ;smoothing parameter
if keyword_set(wst.minmax) eq 0 then wst.minmax=0
if keyword_set(idlsv) eq 0 then idlsv=0  & idlsave=idlsv
if keyword_set(lzwn) eq 0 then lzwn=0  & lzwin=lzwn
if keyword_set(pos_overide) eq 0 then pos_overide=-1

n_colors=wst.ncolors

start: print,'plt'
if wst.hardcopy(0) eq 0 then erase

;set default color scale (preset rscale each panel)
  if keyword_set(wst.cscale) eq 0 then wst.cscale=0  

!p.charsize=1.0;35

;selected indices from set of indices d.pnlist.list
  wpnl=where(d.pnlsel ne -1)
  if wpnl(0) eq -1 then begin
    print,'no plot selection has been made'
    return
  endif
  
  if lzwin then pnlsel=d.pnlsel(wpnl(swest.lzsurvey)) else $
    pnlsel=d.pnlsel(wpnl)

;structure of plotting parameters for selected plot variables 
   pm=d.pnl(pnlsel)

;number of plotvariables selected
   npl=n_elements(pm)

;get reference time in pb5time
  refpb5=sec_pb5(d.refsec)

;some initialization
   wst.xyrange=fltarr(4,wst.nplmx) & wst.xywindow=fltarr(4,wst.nplmx) 
   wst.xysize=fltarr(4,wst.nplmx) & wst.ylog=intarr(wst.nplmx)

;find normal coordinates of plot panel corners 
   pos=fltarr(4,npl)
   ypnl_relsiz=reverse(pm.ypnlp)  ;1.0/float(npl)+fltarr(npl)
   ;yp=1./18
   ;ypnl_relsiz=reverse([2*yp,2*yp,2*yp,2*yp,yp,2*yp,yp,yp,yp,yp,2*yp,yp])
   
   if npl eq 1 and lzwin eq 0 then $
     pos,npl,pos,xoff=0.16,xtop=0.84,yoff=0.2,ytop=0.7,ypnl_relsiz=ypnl_relsiz $
   else if npl gt 1 and lzwin eq 0 then pos,npl,pos,xoff=0.15,xtop=0.84,$
     yoff=0.075,ypnl_relsiz=ypnl_relsiz $
   else if lzwin then pos,npl,pos,yoff=0.2,ytop=0.9,xoff=0.1,xtop=0.88
   if lzwin and pos_overide(0) gt 0 then pos=pos_overide
   print,'pos',pos 

;define labels
  xlabl=strarr(npl) & if lzwin eq 0 then xlabl(npl-1)=pm(0).tmlabl
  ztitle=strarr(npl) 
  if lzwin eq 0 then ztitle(0)=pm(0).ztitle else ztitle(0)=pm(0).tmlabl
  stitle=strarr(npl) & if lzwin eq 0 then stitle(npl-1)=pm(0).subtitle  
  if lzwin and pos_overide(0) gt 0 then begin
     stitle(npl-1)=pm(0).tmlabl
     ztitle(0)=''
  endif
  
;define some plotting parameters
   xtickn=strarr(pm(0).tmticks+1,npl)
   for i=0,npl-1 do xtickn(*,i)=replicate(string(32b),pm(0).tmticks+1)
   xtickn(*,npl-1)=pm(0).tmtickname(0:pm(0).tmticks)
   noerase=1+intarr(npl) & noerase(0)=0
   xcharsize=0.001+fltarr(npl)
   xcharsize(npl-1)=1.0
   
;--------------- hardcopy ----------------------------------------------------
if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0
if wst.hardcopy(0) and lzwin eq 0 then begin 
  print,' ' & print,'making hardcopy... '
  set_plot,'ps',/interpolate
  clrtbl_indx,/hardcopy 
  pltfil=getenv('IDLSAV')+wst.print_flnm 
  print,'pltfil',pltfil

  if npl eq 1 then $
    device,/inches,xoffset=0.75,yoffset=1.,xsize=7.,ysize=5.0,/color,bits=8,$
     filename=pltfil $
  else $
    device,/inches,xoffset=0.75,yoffset=1.,xsize=7.,ysize=9.5,/color,$
     filename=pltfil,bits=8;,bits_per_pixel=8
     
  ;device,/inches,/landscape,filename=pltfil
endif

;stop
;iinitialize structure to save plotted quantities in a file
  ;itype=where(d.datype_input ne -1)
  
  idatype_swe_moments=where(d.datype eq 'swe_moments')
  idatype_swe_ionkp=where(d.datype eq 'swe_ionkp')
  idatype_mfi_magkp=where(d.datype eq 'mfi_magkp')
  idatype_mfi_mag3s=where(d.datype eq 'mfi_mag3s')
  idatype_swe_newstrl=where(d.datype eq 'swe_newstrl') ; NEW!! MPH
  idatype_swe_newmomf=where(d.datype eq 'swe_newmomf') ; NEW!! MPH
  if d.wdatype(0,idatype_swe_moments) ne -1 or $
     d.wdatype(0,idatype_swe_ionkp) ne -1 or $
     d.wdatype(0,idatype_mfi_magkp) ne -1 or $
     d.wdatype(0,idatype_mfi_mag3s) ne -1 or $
     d.wdatype(0,idatype_swe_newstrl) ne -1 or $ ;       NEW!! MPH
     d.wdatype(0,idatype_swe_newmomf) ne -1 $ ;          NEW!! MPH
  then begin
    nsav=0
    nelem=[0]
    if d.wdatype(0,idatype_swe_moments) ne -1 then begin
      nsav=nsav+$
      n_elements(d.wdatype(where(d.wdatype(*,idatype_swe_moments(0)) ne -1),$
        idatype_swe_moments(0)))
      nelem=[nelem,$
        d.ndx(1,idatype_swe_moments(0))-d.ndx(0,idatype_swe_moments(0))+1] 
    endif
    if d.wdatype(0,idatype_swe_ionkp) ne -1 then begin
      nsav=nsav+$
      n_elements(d.wdatype(where(d.wdatype(*,idatype_swe_ionkp(0)) ne -1),$
          idatype_swe_ionkp(0)))
      nelem=[nelem,$
        d.ndx(1,idatype_swe_ionkp(0))-d.ndx(0,idatype_swe_ionkp(0))+1]    
    endif
    if d.wdatype(0,idatype_mfi_magkp) ne -1 then begin
      nsav=nsav+$
      n_elements(d.wdatype(where(d.wdatype(*,idatype_mfi_magkp(0)) ne -1),$
          idatype_mfi_magkp(0)))
      nelem=[nelem,$
        d.ndx(1,idatype_mfi_magkp(0))-d.ndx(0,idatype_mfi_magkp(0))+1]    
    endif
    if d.wdatype(0,idatype_mfi_mag3s) ne -1 then begin
      nsav=nsav+$
      n_elements(d.wdatype(where(d.wdatype(*,idatype_mfi_mag3s(0)) ne -1),$
          idatype_mfi_mag3s(0)))
      nelem=[nelem,$
        d.ndx(1,idatype_mfi_mag3s(0))-d.ndx(0,idatype_mfi_mag3s(0))+1]    
    endif
    if d.wdatype(0,idatype_swe_newstrl) ne -1 then begin ; -------- NEW!! MPH
      nsav=nsav+$
      n_elements(d.wdatype(where(d.wdatype(*,idatype_swe_newstrl(0)) ne -1),$
          idatype_swe_newstrl(0)))
      nelem=[nelem,$
        d.ndx(1,idatype_swe_newstrl(0))-d.ndx(0,idatype_swe_newstrl(0))+1]    
    endif ; --------------------------------------------------- End NEW!! MPH
    if d.wdatype(0,idatype_swe_newmomf) ne -1 then begin ; -------- NEW!! MPH
      nsav=nsav+$
      n_elements(d.wdatype(where(d.wdatype(*,idatype_swe_newmomf(0)) ne -1),$
          idatype_swe_newmomf(0)))
      nelem=[nelem,$
        d.ndx(1,idatype_swe_newmomf(0))-d.ndx(0,idatype_swe_newmomf(0))+1]    
    endif ; --------------------------------------------------- End NEW!! MPH
    nelem=max(nelem)      
    idlsav=replicate({datatype:'',$
                      varname:'',$
                      log:0,$
                      psym:0,$
                      x:dblarr(nelem),$
                      y:fltarr(nelem),$
                      yrange:fltarr(2),$
                      yticks:0,$
                      yminor:0,$
                      refpb5:refpb5,  $
                      xrange:pm(0).tmrange,$
                      xtickname:pm(0).tmtickname(0:pm(0).tmticks),$
                      xticks:pm(0).tmticks,$
                      xtickv:pm(0).tmtickv(0:pm(0).tmticks),$
                      xminor:pm(0).tminor,$
                      xtitle:pm(0).tmlabl,$
                      subtitle:pm(0).subtitle,$
                      title:pm(0).ztitle $
                      },nsav)
  endif


;if isee_moments are selected, then form structure to
;contain plotted isee_moments parameters to save in a file
  idatype_isee_moments=where(d.datype eq 'isee_moments')
  if d.wdatype(0,idatype_isee_moments(0)) ne -1 $
  then begin
    nsav=0
    ;if d.wdatype(0,idatype_isee_moments(0)) ne -1 then $
      nsav=nsav+$
      n_elements(d.wdatype(where(d.wdatype(*,idatype_isee_moments(0)) ne -1),$
            idatype_isee_moments(0)))
    idlsav=replicate({datatype:'',$
                      varname:'',$
                      log:0,$
                      psym:0,$
                      x:dblarr(d.ndx(1,idatype_isee_moments(0))-$
                        d.ndx(0,idatype_isee_moments(0))+1),$
                      y:fltarr(d.ndx(1,idatype_isee_moments(0))-$
                        d.ndx(0,idatype_isee_moments(0))+1),$
                      yrange:fltarr(2),$
                      yticks:0,$
                      yminor:0,$
                      refpb5:refpb5,  $
                      xrange:pm(0).tmrange,$
                      xtickname:pm(0).tmtickname(0:pm(0).tmticks),$
                      xticks:pm(0).tmticks,$
                      xtickv:pm(0).tmtickv(0:pm(0).tmticks),$
                      xminor:pm(0).tminor,$
                      xtitle:pm(0).tmlabl,$
                      subtitle:pm(0).subtitle,$
                      title:pm(0).ztitle $
                      },nsav)
  endif


;------------------ begin plot loop over data selected variables ---------- 
;------------------ testing each variable for data type        ------------

for i=0,npl-1 do begin
  
  print,pm(i).dtp,pnlsel(i),pm(i).dtp,pm(i).labl,pm(i).ev_val  
  call_procedure,pm(i).dtp+'_plt',i,npl

;store 1) axis limits in data coords, 
;window corners in 2) normal and 3) device coords, 
;and 4) log-linear type plot for this panel
  wst.xyrange(*,i) =[!x.crange(0),!y.crange(0),!x.crange(1),!y.crange(1)]
  wst.xywindow(*,i)=[!x.window(0),!y.window(0),!x.window(1),!y.window(1)]
  wst.xysize(*,i)=[!d.x_size*!x.window(0),!d.y_size*!y.window(0),$
               !d.x_size*!x.window(1),!d.y_size*!y.window(1)]
  wst.ylog(i)=pm(i).plotio

if wst.timemark_offon then begin
    if wst.hardcopy(0) eq 0 then clr=wst.clr_orange else clr=wst.clr_green
    
    w1=where(wst.timemark ne 0,nw1)
    if nw1 ge 0 then for imrk=0,nw1-1 do begin
      if wst.ylog(i) and wst.minmax eq 0 then $
        oplot,[wst.timemark(imrk),wst.timemark(imrk)],$
        [10.^wst.xyrange(1,i),10.^wst.xyrange(3,i)],color=clr $
      else  oplot,[wst.timemark(imrk),wst.timemark(imrk)],$
        [wst.xyrange(1,i),wst.xyrange(3,i)],color=clr 
    endfor
    
endif    
    

if keyword_set(oplot_sec) ne 0 and lzwin then begin
  wst.xyrange_lzwin(*)=wst.xyrange(*,i)
  wst.xywindow_lzwin(*)=wst.xywindow(*,i)
  wst.xysize_lzwin(*)=wst.xysize(*,i)
  wst.ylog_lzwin=wst.ylog(i)
endif else begin  
  wst.xyrange_mainnwin(*,i)=wst.xyrange(*,i)
  wst.xywindow_mainnwin(*,i)=wst.xywindow(*,i)
  wst.xysize_mainnwin(*,i)=wst.xysize(*,i)
  wst.ylog_mainnwin(i)=wst.ylog(i)
endelse  

endfor

;------------------ end plot loop over data selected variables ------------

;stop
;giffl=getenv('IDLSAV')+'test.gif'
;tvlct,rr,gg,bb,/get 
;write_gif,giffl,tvrd();,rr,gg,bb
;stop

;save plotted data in each panel
  if idlsave then begin 
    dir=getenv('IDLSAV')  
    save,filename=dir+wst.surveydate+'_idlsave.dat',idlsav
    print,'plots saved in file ',dir+wst.surveydate+'_idlsave.dat'
    help,idlsav & help,idlsav,/str
  endif


;------------ printing hardcopy -------------------------------------------
if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0
if wst.hardcopy(0) then begin
  device,/close
  print,' ' & print,'printing hardcopy: ',wst.print_cmd

  spawn,wst.print_cmd
  set_plot,'x'
  clrtbl_indx
  wst.hardcopy=0 ;& goto,start
endif

!p.charsize=1.0

end
     

