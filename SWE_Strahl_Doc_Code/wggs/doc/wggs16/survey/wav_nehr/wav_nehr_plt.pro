pro wav_nehr_plt,indx,npl

common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
common sharetimeslice,oplot_sec_slice

fill =-1.0e31

idatype=where(d.datype eq 'wav_nehr')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

idatype_swe_moments=where(d.datype eq 'swe_moments')

i=indx 

hrday=(d.wav_nehrdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d
  
;wav_nehrvar returns data (varbl_orig) for selctd variable (pm(i).varname)
   varbl_orig=wav_nehrvar(pm(i).varname)

if wst.spikesout gt 0 then varbl_orig=median(temporary(varbl_orig),wst.spikesout)

;if necessary, reduce array sizes
  ntlim=wst.rebin_size_line                                   
  if n_elements(hrday) gt ntlim then begin          
    sclfctr=fix(float(n_elements(hrday))/float(ntlim)+0.5)
    ntmx=n_elements(hrday)/sclfctr
    case wst.rebin of
    0: begin
         time=congrid(hrday,ntmx,/interp)
         varbl=congrid(varbl_orig,ntmx,/interp)
       endcase
    else : begin
         time=hrday
         varbl=varbl_orig
           endcase  
    endcase
  endif  else begin
    time=hrday
    varbl=varbl_orig
  endelse
help,hrday,time

    
idatype_ionkp=where(d.datype eq 'swe_ionkp')

if d.datype_input(idatype_ionkp) eq 1 then ionkp=1 else ionkp=0
 
if pm(i).oplot and ionkp then begin      ;overplotting ionkp data
  ohrday=$
    (d.swe_ionkpdat(d.ndx(0,idatype_ionkp):d.ndx(1,idatype_ionkp)).ta-$
    d.refsec)/3600.d
  ovbl=swe_ionkpvar(pm(i).oplotvar)
  
  if wst.spikesout gt 0 then ovbl=median(temporary(ovbl),wst.spikesout)
  
  ;if necessary, reduce array sizes
  ntlim=wst.rebin_size_line                                   
  if n_elements(ohrday) gt ntlim then begin          
    sclfctr=fix(float(n_elements(ohrday))/float(ntlim)+0.5)
    ntmx=n_elements(ohrday)/sclfctr
    case wst.rebin of
    0: begin
         ohrday=congrid(ohrday,ntmx,/interp)
         ovbl=congrid(ovbl,ntmx,/interp)
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
      ymn=min([varbl,ovbl],max=ymx)  else $
      ymn=min(varbl,max=ymx) 
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
            
   if pm(i).oplot and ionkp then $
      oplot,ohrday,ovbl,$
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
           
   if pm(i).oplot and ionkp then $
      oplot,ohrday,ovbl,$
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
   idlsav(i).y=varbl_orig
   idlsav(i).yrange=yrange
   idlsav(i).yticks=pm(i).ticks
   idlsav(i).yminor=pm(i).minor 
endif


end