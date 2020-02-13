;============================ SWE_fParaPerp_Plt ==============================
; For the panel in a series of 'npl' stackplot panels identified by 'indx',
;  locate the desired absissa and ordinate vectors for the current time series.
; Perform any final array reduction and smoothing, set any remaining plot
;  parameters and perform plotting (and saving).     (Last modified: 07/26/04).
PRO swe_fparaperp_plt,indx,npl

common shared,d ;  Provides access to:                 the main data structure.
common wstuff,wst ;                the main widget interface control structure.
common sharelevelzero,pltwin_frmt,oplot_sec ;       For LZ window overplotting.
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
                idlsave,lzwin,elaps_spns ;  For sharing global plot parameters.
common sharetimeslice,oplot_sec_slice ;    For strahl application overplotting.

idatype = where(d.datype eq 'swe_fparaperp') & i = indx ;     Survey data-type.
beg_ind = d.ndx[0,idatype] & end_ind = d.ndx[1,idatype] ;     Selection bounds.

if ((end_ind-beg_ind) lt 2) then begin ;             Issue error (and exit) if
  print,'array size must be ge 2' & return ;             too few data-points...
endif

clrs = long((float([60,80,160,180])/183.0)*float(!D.n_colors)) ;    Color-list.
x = (d.swe_fparaperpdat[beg_ind:end_ind].ta-d.refsec)/3600.0d0 ;   Hour-of-day.

case strmid(pm[i].varname,7,16) of ;  Select the SET of ENERGY STEPS to plot...
   ; Note: The lowest energy-channel (index=0) is not used, and the repeated
   ;         energy-channels (index=11,12 and index=13,14) are always combined.

   '[38-1005 eV, x2]': endx = [ 1,    3,    5,    7,    9,   11,12     ]
   '[58-1237 eV, x2]': endx = [    2,    4,    6,    8,   10,     13,14]
   '[38-1005 eV, x3]': endx = [ 1,       4,       7,    9,   11,12     ]
   '[58-1237 eV, x3]': endx = [    2,       5,       8,   10,     13,14]
   '[77-1237 eV, x3]': endx = [       3,       6,       9,        13,14]

endcase

case strmid(pm[i].varname,0,6) of ;   Select TYPE of phase-density averaging...

   'f_omni': f = reform(d.swe_fparaperpdat[beg_ind:end_ind].fspa[endx])
  
   'f_para': f = (reform(d.swe_fparaperpdat[beg_ind:end_ind].f[0,endx]) + $
                  reform(d.swe_fparaperpdat[beg_ind:end_ind].f[1,endx])) $
                  /(2.0*!pi) ; Divide integral by TOT. SOLID ANGLE integration.

   'f_perp': f = (reform(d.swe_fparaperpdat[beg_ind:end_ind].f[1,endx]) + $
                  reform(d.swe_fparaperpdat[beg_ind:end_ind].f[2,endx])) $
                  /(2.0*!pi) ; Divide integral by TOT. SOLID ANGLE integration.
     
   'f_anti': f = (reform(d.swe_fparaperpdat[beg_ind:end_ind].f[2,endx]) + $
                  reform(d.swe_fparaperpdat[beg_ind:end_ind].f[3,endx])) $
                  /(2.0*!pi) ; Divide integral by TOT. SOLID ANGLE integration.

endcase

max_en_indx = n_elements(endx)-1l ; Average over REPEATED en. steps (last two).
f[max_en_indx-1l,*] = total(f[[max_en_indx-1l,max_en_indx],*],1)/2.0

;  First, display the plot-panel frame (axes, NO data) for the current panel...
plot,x,f,/Ylog,/NoData,NoErase=noerase[i],/Normal,Position=pos[*,i],$
     Title=ztitle[i],Subtitle=stitle[i],Xtitle=xlabl[i],Ytitle=pm[i].labl,$
     Xrange=pm[i].tmrange,Xstyle=1,Xticks=pm[i].tmticks,Xminor=pm[i].tminor,$
     Xtickname=xtickn[*,i],Xtickv=pm[i].tmtickv[0:pm[0].tmticks],$
     Yrange=pm[i].range,Ystyle=1,Yticks=pm[i].ticks,Yminor=pm[i].minor,$
     Ytickname=pm[i].tickname,Ytickv=pm[i].tickv,Xticklen=0.05,$
     Charsize=pm[i].charsize,Xcharsize=xcharsize[i],Charthick=pm[i].charthick

for j=0,(max_en_indx-1) do begin ; Now, for each UNIQUE selected energy step...

   y = reform(f[j,*]) & ohrday = x ;  Extract jth time-series for this panel...
   wok = where(((x ge pm[i].tmrange[0]) and (x le pm[i].tmrange[1])),wok_cnt)
   if (wok_cnt ne 0) then begin x = x[wok] & y = y[wok] & endif

   nx = n_elements(x) & ntlim = wst.rebin_size_line ;  Size (length) of series.
   if (nx gt ntlim) then begin ; If necessary, reduce array sizes by sclfctr...
      sclfctr = fix((float(nx)/float(ntlim))+0.5) & ntmx = nx/sclfctr

      case wst.rebin of ;                   Has user opted for array reduction?
         0: begin & ohrday = congrid(x,ntmx,/Interp) ; Arr. reduction SELECTED.
                         y = congrid(y,ntmx,/Interp) & endcase  
         else : ohrday = x ;        Otherwise, array reduction is NOT selected.
      endcase
   endif

   if (wst.spikesout gt 0) then y = median(reform(y),wst.spikesout) ;   Filter.

   jth_color = clrs[j mod n_elements(clrs)] ;   Select current color from list.
   oplot,ohrday,y,Psym=0,Color=jth_color ; !!!!! Plot current time-series HERE.

   y_last = y[n_elements(y)-1l] ;      Last y-value from current time-series...
   jth_energy = d.swe_fparaperpdat[beg_ind].energy[endx[j]] ; Get curr. energy.
   if ((y_last ge (10.0^!Y.cRange[0])) and $ ;    Display energy lable if last
       (y_last le (10.0^!Y.cRange[1]))) then $ ;         y-value is in range...
      xyouts,!X.cRange[1],y_last,/Data,$ ;      Label to right of last y-value.
             string(jth_energy,Format='(I4)')+' eV',Color=jth_color
     
   print,'color: ',jth_color,'   energy: ',jth_energy,'  eV' ;      Diagnostic.

   if (keyword_set(oplot_sec) and lzwin) then begin ; LZ window overplotting...
      hrday_oplt = (oplot_sec-d.refsec)/3600.d ;                 Do overplot...
      oplot,[hrday_oplt,hrday_oplt],pm[i].range,Color=175
   endif

   if keyword_set(oplot_sec_slice) then begin ;     Strahl app. overplotting...
      hrday_oplt_slice = (oplot_sec_slice-d.refsec)/3600.d ;     Do overplot...
      oplot,[hrday_oplt_slice,hrday_oplt_slice],$
            pm[i].range,Color=wst.clr_orange
   endif
endfor

end
