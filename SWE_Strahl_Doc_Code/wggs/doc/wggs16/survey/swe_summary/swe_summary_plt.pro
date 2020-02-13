;=============================== pro swe_summary_plt ==========================
; For the panel in a series of 'npl' stackplot panels identified by 'indx',
;  locate the desired absissa and ordinate vectors for the current time series.
; Perform any final array reduction and smoothing, set any remaining plot
;  parameters and perform plotting (and saving).     Last modified: (02/25/02)

PRO swe_summary_plt,indx,npl

common shared,d ;  Provides access to:                 the main data structure.
common wstuff,wst ;                the main widget interface control structure.
common sharelevelzero,pltwin_frmt,oplot_sec ;       For LZ window overplotting.
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
                idlsave,lzwin,elaps_spns ;  For sharing global plot parameters.
common sharetimeslice,oplot_sec_slice ;    For strahl application overplotting.

;     Identify survey data type, define time series absissa (hour of day), etc.
idatype = where(d.datype eq 'swe_summary') & i = indx & fill = -1.0e31
hrday = (d.swe_summarydat.ta[d.ndx[0,idatype]:d.ndx[1,idatype]]-$
                                                              d.refsec)/3600.d

;     swe_summaryvar returns data corresponding to the selected variable given
ssummvbl = swe_summaryvar(pm[i].varname) ;        by the string, pm[i].varname.

;             Find and retain only "good" (not infinity, NaN or 'fill') data...
; Note: To remove corrupted fill values, keep only interval: (-1.0e6,infinity).
wgood = where((finite(ssummvbl) and (ssummvbl gt -1.0e6)),wg_cnt)
if (wg_cnt ne 0) then begin ;                      If some "good" data found...
   ssummvbl = ssummvbl[wgood] & hrday = hrday[wgood] ;          Keep good data.
endif else begin ;                                                 Otherwise...
   print,'No data to be plotted.' & return ;                    Do no plotting.
endelse

ntlim = 2*wst.rebin_size_line ;    Define limit on the length of a time series.
ntimes = n_elements(hrday) ;        Find the length of the current time series.

if (ntimes gt ntlim) then begin ;            Reduce array sizes if necessary...
   sclfctr = fix(float(ntimes)/float(ntlim)+0.5) ;                Scale factor.
   ntmx = ntimes/sclfctr ;               Desired length of reduced time series.

   case wst.rebin of ;                      Has user opted for array reduction?
      0: begin & time = congrid(hrday,ntmx,/Interp) ; Array reduction SELECTED.
                 varbl = congrid(ssummvbl,ntmx,/Interp) & endcase
      else: begin & time = hrday & varbl = ssummvbl & endcase ;  Reduction OFF.
   endcase
endif else begin & time = hrday & varbl = ssummvbl & endelse ;  Short time ser.

help,ssummvbl,varbl,hrday,time ;       Send output to screen about time series.

;          If user has opted for data smoothing, then perform median filtering.
if (wst.spikesout gt 0) then varbl = median(temporary(varbl),wst.spikesout)
  
if (not wst.minmax) then begin ;      If line plot min-max is NOT to be used...
   yrange = pm[i].range & ynozero = 0 ;                      Use preset ranges.
endif else begin ;                        If line plot min-max IS to be used...
   ymn = min(varbl,Max=ymx) ;         Find min. and max. values, and set range.
   yrange = [ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero = 1
endelse

; ***************************** Perform plotting ******************************
;     Note: If min-max is selected by user, linear plotting is the only option.
if ((not pm[i].plotio) or wst.minmax) then begin ;      LINEAR plot selected...
   plot,time,varbl,Title=ztitle[i],Subtitle=stitle[i],$
        Xrange=pm[i].tmrange,Xticks=pm[i].tmticks,Xstyle=1,$
        Xtitle=xlabl[i],Xtickname=xtickn[*,i],$
        Xtickv=pm[i].tmtickv[0:pm[0].tmticks],Xminor=pm[i].tminor,$
        Yrange=yrange,Yticks=pm[i].ticks,Ystyle=1,Ytitle=pm[i].labl,$
        Ytickname=pm[i].tickname,Ytickv=pm[i].tickv,Yminor=pm[i].minor,$
        Psym=pm[i].psym,Symsize=pm[i].symsize,Noerase=noerase[i],/Normal,$
        Position=pos[*,i],Ynozero=ynozero,Xticklen=0.05,$
        Charsize=pm[i].charsize,Xcharsize=xcharsize[i],$
        Charthick=pm[i].charthick
endif else begin ;                                 LOGARITHMIC plot selected...
   plot_io,time,varbl,Title=ztitle[i],Subtitle=stitle[i],$
           Xrange=pm[i].tmrange,Xticks=pm[i].tmticks,Xstyle=1,$
           Xtitle=xlabl[i],Xtickname=xtickn[*,i],$
           Xtickv=pm[i].tmtickv[0:pm[0].tmticks],Xminor=pm[i].tminor,$
           Yrange=yrange,Ystyle=1,Ytitle=pm[i].labl,$
           Ytickname=pm[i].tickname,Ytickv=pm[i].tickv,$
           Psym=pm[i].psym,Symsize=pm[i].symsize,Noerase=noerase[i],/Normal,$
           Position=pos[*,i],xticklen=0.05,$
           Charsize=pm[i].charsize,Xcharsize=xcharsize[i],$
           Charthick=pm[i].charthick
endelse 

if (keyword_set(oplot_sec) and lzwin) then begin ;    LZ window overplotting...
   hrday_oplt = (oplot_sec-d.refsec)/3600.d
   oplot,[hrday_oplt,hrday_oplt],yrange,Color=175
endif  

if keyword_set(oplot_sec_slice) then begin ; Strahl application overplotting...
   hrday_oplt_slice = (oplot_sec_slice-d.refsec)/3600.d
   oplot,[hrday_oplt_slice,hrday_oplt_slice],yrange,Color=wst.clr_orange
endif

if (pm[i].horizlin ne fill) then $ ;               Horizon line overplotting...
   oplot,[hrday[0],hrday[ntimes-1l]],[pm[i].horizlin,pm[i].horizlin],$
         Linestyle=1 ;                                 Plot with a dotted line.

if idlsave then begin ; Saving plotted data for this panel to save to a file...
  idlsav[i].datatype = pm[i].dtp & idlsav[i].varname = pm[i].labl
  idlsav[i].log = pm[i].plotio & idlsav[i].psym = pm[i].psym
  idlsav[i].x = hrday & idlsav[i].y = ssummvbl & idlsav[i].yrange = yrange
  idlsav[i].yticks = pm[i].ticks & idlsav[i].yminor = pm[i].minor 
endif

end