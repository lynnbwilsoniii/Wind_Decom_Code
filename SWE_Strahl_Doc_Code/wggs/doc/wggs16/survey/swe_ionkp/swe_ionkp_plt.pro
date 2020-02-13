pro swe_ionkp_plt,indx,npl

common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
common sharetimeslice,oplot_sec_slice

fill =-1.0e31

idatype=where(d.datype eq 'swe_ionkp')
idatype_swe_moments=where(d.datype eq 'swe_moments')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

i=indx 

hrday=(d.swe_ionkpdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d
  
;ionkpvar returns data (ikpvbl) correspndng to selctd variable (pm(i).varname)
   ikpvbl=swe_ionkpvar(pm(i).varname)

if wst.spikesout gt 0 then ikpvbl=median(temporary(ikpvbl),wst.spikesout)

;if necessary, reduce array sizes
  ntlim=wst.rebin_size_line                                   
  if n_elements(hrday) gt ntlim then begin          
    sclfctr=fix(float(n_elements(hrday))/float(ntlim)+0.5)
    ntmx=n_elements(hrday)/sclfctr
    case wst.rebin of
    0: begin
         time=congrid(hrday,ntmx,/interp)
         varbl=congrid(ikpvbl,ntmx,/interp)
       endcase
    else : begin
         time=hrday
         varbl=ikpvbl
           endcase  
    endcase
  endif  else begin
    time=hrday
    varbl=ikpvbl
  endelse
help,hrday,time


if d.datype_input(idatype_swe_moments) eq 1 then moments=1 else moments=0

if moments then ohrday=(d.swe_mdat(d.ndx(0,idatype_swe_moments):$
                             d.ndx(1,idatype_swe_moments)).ta-d.refsec)/3600.d
if pm(i).oplot and moments ne -1 then begin      ;overplotting elec data
   ;omvbl=mvar(pm(i).oplotvar)
   omvbl=swe_momentsvar(pm(i).oplotvar)
   if wst.spikesout gt 0 then omvbl=median(temporary(omvbl),wst.spikesout)
   if wst.hardcopy then ocolor=!p.color else ocolor=pm(i).ocolor
endif
   
;determine if line plot min-max is to be used 
  if wst.minmax eq 0 then begin
    yrange=pm(i).range & ynozero=0
  endif else begin
    ymn=min(ikpvbl(where(ikpvbl ne fill and finite(ikpvbl) eq 1)),max=ymx) 
    yrange=[ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero=1
  endelse
    
if pm(i).plotio eq 0 or wst.minmax eq 1 then begin    ;not log plot           
   plot,$
     time,varbl,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
     xminor=pm(i).tminor,$
     yrange=yrange,yticks=pm(i).ticks,ystyle=1,ytitle=pm(i).labl,$
     ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
     psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
     position=pos(*,i),ynozero=ynozero,xticklen=0.05,$
     charsize=pm(i).charsize,xcharsize=xcharsize(i),charthick=pm(i).charthick
            
   if pm(i).oplot and moments ne -1 then $
      oplot,ohrday,omvbl,$
        color=ocolor,linestyle=pm(i).olinestyle
                     
endif else begin   ;yes log plot
   plot_io,$
     time,varbl,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
     xminor=pm(i).tminor,$
     yrange=yrange,ystyle=1,ytitle=pm(i).labl,$
     ytickname=pm(i).tickname,ytickv=pm(i).tickv,$
     psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
     position=pos(*,i),xticklen=0.05,$
     charsize=pm(i).charsize,xcharsize=xcharsize(i),charthick=pm(i).charthick
           
   if pm(i).oplot and moments ne -1 then $
       oplot,ohrday,omvbl,$
         color=ocolor,linestyle=pm(i).olinestyle
                
endelse
   
if keyword_set(oplot_sec) ne 0 and lzwin then begin
   hrday_oplt=(oplot_sec-d.refsec)/3600.d
   oplot,[hrday_oplt,hrday_oplt],yrange,color=175
endif 


if keyword_set(oplot_sec_slice) ne 0 then begin
   hrday_oplt_slice=(oplot_sec_slice-d.refsec)/3600.d
   oplot,[hrday_oplt_slice,hrday_oplt_slice],yrange,color=wst.clr_orange
endif
 
if pm(i).horizlin ne fill then oplot,$
  [hrday(0),hrday(n_elements(hrday)-1)],[pm(i).horizlin,pm(i).horizlin],$
    linestyle=1
             
if idlsave then begin   ;saving plotted data this panel to save to a file

   idlsav(i).datatype=pm(i).dtp
   idlsav(i).varname=pm(i).labl
   idlsav(i).log=pm(i).plotio
   idlsav(i).psym=pm(i).psym
   idlsav(i).x=hrday
   idlsav(i).y=ikpvbl
   idlsav(i).yrange=yrange
   idlsav(i).yticks=pm(i).ticks
   idlsav(i).yminor=pm(i).minor 
endif


end