pro swe_moments_plt,indx,npl

common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns

fill =-1.0e31

idatype=where(d.datype eq 'swe_moments')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

i=indx 
 
hrday=(d.swe_mdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d
 
;swe_momentsvar returns the data (mvbl) correspndng to 
;  selctd variable (pm(i).varname)   
  mvbl=swe_momentsvar(pm(i).varname)

;remove fill
wnefill=where(mvbl ne fill)
if wnefill(0) ne -1 then begin
  mvbl=mvbl(wnefill)
  hrday=hrday(wnefill)
endif
   
;return data for selected overplot variable
  mvbl_tperp=-1
  if pm(i).varname eq 'T parallel' then $
    mvbl_tperp=swe_momentsvar('T perpendicular')
  if pm(i).varname eq 'T para eV' then $
    mvbl_tperp=swe_momentsvar('T perp eV')
      
if wst.strlfov then begin ;accept mag data only when in strahl field of view
  if ( (pm(i).varname eq 'B magnetic field') or (pm(i).varname eq 'th_b') or $
    (pm(i).varname eq 'ph_b') ) then begin     
    th=[-28.,28.]
    pht=[292.,338.]
    pha=[112.,158.]
    theta=swe_momentsvar('th_b')
    phi=swe_momentsvar('ph_b')
  
    ;Before 19990126, strahl det had fixed fileid of view in both theta and phi.
    ;Beginning 19990126, strahl det tracked mag field in phi, theta still fixed.
    if wst.indate lt '19990126' then $
      wfv=where( ((phi gt pht(0) and phi lt pht(1)) or $
                  (phi gt pha(0) and phi lt pha(1))) and $
                  (theta gt th(0) and theta lt th(1)) )  $
                  
    else wfv=where( theta gt th(0) and theta lt th(1) )  
                              
    if wfv(0) ne -1 then begin
      mvbl=mvbl(wfv)
      if mvbl_tperp(0) ne -1 then mvbl_tperp=mvbl_tperp(wfv)
      hrday=hrday(wfv)
    endif 
  endif             
endif
  
if wst.spikesout gt 0 then begin
  mvbl=median(temporary(mvbl),wst.spikesout)
  if mvbl_tperp(0) ne -1 then $
    mvbl_tperp=median(temporary(mvbl_tperp),wst.spikesout)
endif  

;if necessary, reduce array sizes
  ntlim=wst.rebin_size_line                                   
  if n_elements(hrday) gt ntlim then begin          
    sclfctr=fix(float(n_elements(hrday))/float(ntlim)+0.5)
    ntmx=n_elements(hrday)/sclfctr
    case wst.rebin of
    0: begin
         time=congrid(hrday,ntmx,/interp)
         varbl=congrid(mvbl,ntmx,/interp)
         if mvbl_tperp(0) ne -1 then $
           varbl_tperp=congrid(mvbl_tperp,ntmx,/interp)
       endcase
    else : begin
         time=hrday
         varbl=mvbl
         if mvbl_tperp(0) ne -1 then varbl_tperp=mvbl_tperp
           endcase  
    endcase
  endif  else begin
    time=hrday
    varbl=mvbl
    if mvbl_tperp(0) ne -1 then varbl_tperp=mvbl_tperp
  endelse
help,hrday,time

    
idatype_ionkp=where(d.datype eq 'swe_ionkp')

if d.datype_input(idatype_ionkp) eq 1 then ionkp=1 else ionkp=0
 
if pm(i).oplot and ionkp and $
d.ndx(0,idatype_ionkp) lt d.ndx(1,idatype_ionkp) $
then begin      ;overplotting ionkp data
  
  ohrday=$
    (d.swe_ionkpdat(d.ndx(0,idatype_ionkp):d.ndx(1,idatype_ionkp)).ta-$
    d.refsec)/3600.d
  omvbl=swe_ionkpvar(pm(i).oplotvar)
  
  if wst.spikesout gt 0 then omvbl=median(temporary(omvbl),wst.spikesout)
  
  ;if necessary, reduce array sizes
  ntlim=wst.rebin_size_line                                   
  if n_elements(ohrday) gt ntlim then begin          
    sclfctr=fix(float(n_elements(ohrday))/float(ntlim)+0.5)
    ntmx=n_elements(ohrday)/sclfctr
    case wst.rebin of
    0: begin
         ohrday=congrid(ohrday,ntmx,/interp)
         omvbl=congrid(omvbl,ntmx,/interp)
       endcase
    else :  
    endcase
  endif  
  help,ohrday

  
  if wst.hardcopy then ocolor=!p.color else ocolor=pm(i).ocolor
endif

;determine if line plot min-max is to be used   
  if wst.minmax eq 0 then begin
    yrange=pm(i).range & ynozero=0
  endif else begin
    if pm(i).oplot and ionkp then $
      ymn=min([mvbl,omvbl],max=ymx)  else $
      ymn=min(mvbl,max=ymx) 
    yrange=[ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero=1 
    if yrange(0) eq yrange(1) then begin
      yrange(0)=0 & ynozero=0
    endif     
  endelse

 
             
if pm(i).plotio eq 0 or wst.minmax eq 1 then begin ;not log plot
   plot,$
      time,varbl,$
      title=ztitle(i),subtitle=stitle(i),$
      xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,$
      xtitle=xlabl(i),$
      xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
      xminor=pm(i).tminor,$
      yrange=yrange,yticks=pm(i).ticks,ystyle=1,ytitle=pm(i).labl,$
      ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
      psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
      position=pos(*,i),ynozero=ynozero,xticklen=0.05,$
      charsize=pm(i).charsize,xcharsize=xcharsize(i),$
      charthick=pm(i).charthick
   
   if mvbl_tperp(0) ne -1 then $
     oplot,time,varbl_tperp,color=wst.clr_green
        
   if pm(i).oplot and ionkp and $
   d.ndx(0,idatype_ionkp) lt d.ndx(1,idatype_ionkp) then $
      oplot,ohrday,omvbl,$
      color=ocolor,psym=1,symsize=0.4 ;linestyle=pm(i).olinestyle,
   
   if pm(i).labl eq 'X fshck' then $
      oplot,$
        [d.swe_mdat(d.ndx(0,idatype)).ta-d.refsec,$
         d.swe_mdat(d.ndx(1,idatype)).ta-d.refsec]/3600.d,[0,0],$
        linestyle=1,color=wst.clr_green
   if keyword_set(err) ne 0 then $
      oplot,hrday,varerr(0,*),linestyle=1,color=wst.clr_orange
     
endif else begin   ;yes log plot
   plot,/ylog,$
      time,varbl,$
      title=ztitle(i),subtitle=stitle(i),$
      xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,$
      xtitle=xlabl(i),$
      xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(i).tmticks),$
      xminor=pm(i).tminor,$
      yrange=yrange,ystyle=1,ytitle=pm(i).labl,$
      ytickname=pm(i).tickname,ytickv=pm(i).tickv,$
      psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
      position=pos(*,i),xticklen=0.05,$
      charsize=pm(i).charsize,xcharsize=xcharsize(i),$
      charthick=pm(i).charthick
   
   if mvbl_tperp(0) ne -1 then $
     oplot,time,varbl_tperp,color=wst.clr_green
             
   if pm(i).oplot and ionkp and $
   d.ndx(0,idatype_ionkp) lt d.ndx(1,idatype_ionkp) then $
      oplot,ohrday,omvbl,$
         color=ocolor,psym=1,symsize=0.4 ;linestyle=pm(i).olinestyle
endelse
  
if keyword_set(oplot_sec) ne 0 and lzwin then begin
    hrday_oplt=(oplot_sec-d.refsec)/3600.d
    oplot,[hrday_oplt,hrday_oplt],yrange,color=175
endif 
  
if pm(i).horizlin ne fill then oplot,$
    [hrday(0),hrday(n_elements(hrday)-1)],[pm(i).horizlin,pm(i).horizlin],$
    linestyle=1


if idlsave then begin  ;saving plotted data this panel to save to a file
   idlsav(i).datatype=pm(i).dtp
   idlsav(i).varname=pm(i).labl
   idlsav(i).log=pm(i).plotio
   idlsav(i).psym=pm(i).psym
   idlsav(i).x=$
     (d.swe_mdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d
   idlsav(i).y=mvbl
   idlsav(i).yrange=yrange
   idlsav(i).yticks=pm(i).ticks
   idlsav(i).yminor=pm(i).minor    
endif

end 