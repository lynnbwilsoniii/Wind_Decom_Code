;============================= SWE_NewMOMf_Plt ===============================
; For the panel in a series of 'npl' stackplot panels identified by 'indx',
;  locate the desired absissa and ordinate vectors for the current time series.
; Perform any final array reduction and smoothing, set any remaining plot
;  parameters and perform plotting (and saving).     (Last modified: 08/18/03).
PRO swe_newmomf_plt,indx,npl

common shared,d ;  Provides access to:                 the main data structure.
common wstuff,wst ;                the main widget interface control structure.
common sharelevelzero,pltwin_frmt,oplot_sec ;       For LZ window overplotting.
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
                idlsave,lzwin,elaps_spns ;  For sharing global plot parameters.
common sharetimeslice,oplot_sec_slice ;    For strahl application overplotting.

;     Identify survey data type, define time series absissa (hour of day), etc.
idatype = where(d.datype eq 'swe_newmomf') & i = indx & fill = -1.0e31
hrday = (d.swe_newmomfdat[d.ndx[0,idatype]:d.ndx[1,idatype]].ta-$
                                                            d.refsec)/3600.0d0

;   'swe_newmomfvar' returns data corresponding to the selected variable given
nmomfvbl = swe_newmomfvar(pm[i].varname) ;        by the string, pm[i].varname.

ntlim = 2*wst.rebin_size_line ;    Define limit on the length of a time series.
ntimes = n_elements(hrday) ;        Find the length of the current time series.

if (ntimes gt ntlim) then begin ;            Reduce array sizes if necessary...
   sclfctr = fix(float(ntimes)/float(ntlim)+0.5) ;                Scale factor.
   ntmx = ntimes/sclfctr ;               Desired length of reduced time series.

   case wst.rebin of ;                      Has user opted for array reduction?
      0: begin & time = congrid(hrday,ntmx,/Interp) ; Array reduction SELECTED.
                 varbl = congrid(nmomfvbl,ntmx,/Interp) & endcase
      else: begin & time = hrday & varbl = nmomfvbl & endcase ;  Reduction OFF.
   endcase
endif else begin & time = hrday & varbl = nmomfvbl & endelse ;  Short time ser.

help,nmomfvbl,varbl,hrday,time ;       Send output to screen about time series.

;          If user has opted for data smoothing, then perform median filtering.
if (wst.spikesout gt 0) then varbl = median(temporary(varbl),wst.spikesout)

idatype_ionkp = where(d.datype eq 'swe_ionkp') ;  Pos.: IONS in data-type list.

if (d.datype_input[idatype_ionkp] eq 1) then ionkp = 1 else ionkp = 0 ;   Ions?
 
if ((pm[i].oplot and ionkp) and $ ;  ION-MOMENTS data are to be over-plotted...
    (d.ndx[0,idatype_ionkp] lt d.ndx[1,idatype_ionkp])) then begin
   plot_ionmom = 1 ;           There EXISTS ion-moment data to be over-plotted.
   ohrday = (d.swe_ionkpdat[d.ndx[0,idatype_ionkp]:$ ;   OVERPLOT-DATA times...
                            d.ndx[1,idatype_ionkp]].ta-d.refsec)/3600.0d0
   omvbl = swe_ionkpvar(pm[i].oplotvar) & ontimes = n_elements(ohrday) ;  DATA.

   ;      If reduction of array sizes is necessary and user has opted for it...
   if ((n_elements(ohrday) gt ntlim) and (wst.rebin eq 0)) then begin          
      osclfctr = fix(float(ontimes)/float(ntlim)+0.5) ;           Scale factor.
      ontmx = ontimes/osclfctr ;         Desired length of reduced time series.
      ohrday = congrid(temporary(ohrday),ontmx,/Interp) ; Reduce overplt-TIMES.
      omvbl  = congrid(temporary(omvbl ),ontmx,/Interp) ; Reduce overplot-DATA.
   endif  

   help,ohrday,omvbl ;   Send output to screen about OVERPLOT-DATA time series.

   ;       If user has opted for data smoothing, then perform median filtering.
   if (wst.spikesout gt 0) then omvbl = median(temporary(omvbl),wst.spikesout)

   if wst.hardcopy then ocolor = !P.color else ocolor = pm[i].ocolor ; Get clr.
endif else plot_ionmom = 0 ;  Otherwise, NO ion-moment data to be over-plotted.

;                  Obtain data for over-plotting of minimization constraints...
case pm[i].varname of ;                     For a specific list of variables...
   'A_Kappa fit result': begin ;                         A_Kappa fit-parameter.
      plot_bounds = 1 ;                Parameter-bounds SHOULD be over-plotted.
      l_bnd_varname = 'A_Kappa min. bound' ;                       A_Kappa min.
      u_bnd_varname = 'A_Kappa Max. bound' ;                       A_Kappa Max.
   endcase

   's_Kappa fit result': begin ;                         s_Kappa fit-parameter.
      plot_bounds = 1 ;                Parameter-bounds SHOULD be over-plotted.
      l_bnd_varname = 's_Kappa min. bound' ;                       s_Kappa min.
      u_bnd_varname = 's_Kappa Max. bound' ;                       s_Kappa Max.
   endcase

   'Kappa fit result': begin ;                             Kappa fit-parameter.
      plot_bounds = 1 ;                Parameter-bounds SHOULD be over-plotted.
      l_bnd_varname = 'Kappa min. bound' ;                           Kappa min.
      u_bnd_varname = 'Kappa Max. bound' ;                           Kappa Max.
   endcase

   'E_bar fit result': begin ;                             E_bar fit-parameter.
      plot_bounds = 1 ;                Parameter-bounds SHOULD be over-plotted.
      l_bnd_varname = 'E_bar min. bound' ;                           E_bar min.
      u_bnd_varname = 'E_bar Max. bound' ;                           E_bar Max.
   endcase

   else: plot_bounds = 0 ;   Otherwise, NO parameter-bounds to be over-plotted.
endcase

if plot_bounds then begin ; If there are parameter-bounds to be over-plotted...
   l_bnd = swe_newmomfvar(l_bnd_varname) ;             Obtain LOWER-bound data.
   u_bnd = swe_newmomfvar(u_bnd_varname) ;             Obtain UPPER-bound data.

   ;      If reduction of array sizes is necessary and user has opted for it...
   if ((ntimes gt ntlim) and (wst.rebin eq 0)) then begin
      l_bnd = congrid(temporary(l_bnd),ntmx,/Interp) ; Reduce LOWER-bound data.
      u_bnd = congrid(temporary(u_bnd),ntmx,/Interp) ; Reduce UPPER-bound data.
   endif

   help,l_bnd,u_bnd ;       Send output to screen about BOUND-DATA time series.
   
   if (wst.spikesout gt 0) then begin ;    User has opted for data smoothing...
      l_bnd = median(temporary(l_bnd),wst.spikesout) ; Median-filt. LOWER data.
      u_bnd = median(temporary(u_bnd),wst.spikesout) ; Median-filt. UPPER data.
   endif

   if wst.hardcopy then ocolor = !P.color else ocolor = pm[i].ocolor ; Get clr.
endif

if (pm[i].varname eq 'Energy range used') then begin ;   User sel. En.-range...
   fitEn_range = 1 ;                     Energy-range used for fit IS selected.
   E_bnd = swe_newmomfvar('Energy range Max.') ;       Obtain UPPER-bound data.

   ;      If reduction of array sizes is necessary and user has opted for it...
   if ((ntimes gt ntlim) and (wst.rebin eq 0)) then $
      E_bnd = congrid(temporary(E_bnd),ntmx,/Interp) ; Reduce UPPER-bound data.

   help,E_bnd ;       Send output to screen about UPPER-bound data time series.
   
   if (wst.spikesout gt 0) then $ ;        User has opted for data smoothing...
      E_bnd = median(temporary(E_bnd),wst.spikesout) ; Median-filt. UPPER data.

   if wst.hardcopy then ocolor = !P.color else ocolor = pm[i].ocolor ; Get clr.
endif else fitEn_range = 0 ; Otherwise, en.-range used for fit is NOT selected.

if (not wst.minmax) then begin ;      If line plot min-max is NOT to be used...
   yrange = pm[i].range & ynozero = 0 ;                      Use preset ranges.
endif else begin ;                        If line plot min-max IS to be used...
   ; Note: This logic is defined assuming that the existence of OVERPLOT-ION-
   ;        MOM.-DATA and the need for PARAMETER-BOUNDS are MUTUALLY EXCLUSIVE.
   if plot_ionmom then begin ;              If OVERPLOT-ION-MOM.-DATA exists...
      ymn = min([varbl,omvbl],Max=ymx) ;            Find min./Max. of ALL data.
   endif else if plot_bounds then begin ;   If PARAM.-BNDS to be over-pltted...
      ymn = min([l_bnd,varbl,u_bnd],Max=ymx) ;      Find min./Max. of ALL data.
   endif else if fitEn_range then begin ;     If EN.-MAX. to be over-plotted...
      ymn = min([varbl,E_bnd],Max=ymx) ;            Find min./Max. of ALL data.
   endif else begin ;                 Otherwise (nothing to be over-plotted)...
      ymn = min(varbl,Max=ymx) ;    Find min. and Max. values of VARIABLE data.
   endelse
   ;                          ...now set range (extended top-and-bottom by 5%).
   yrange = [ymn-(0.05*abs(ymx-ymn)),ymx+(0.05*abs(ymx-ymn))] & ynozero = 1

   if (yrange[0] eq yrange[1]) then begin ;  Range degenerated to zero-width...
      yrange[0] = 0.0 & ynozero = 0 ;                  Set lower bound to zero.
   endif
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

if plot_ionmom then begin ;  If there are ION-MOMENT DATA to be over-plotted...
   oplot,ohrday,omvbl,Color=ocolor,Psym=1,Symsize=0.4 ;            Do overplot.
endif

if plot_bounds then begin ; If there are PARAMETER-BOUNDS to be over-plotted...
   oplot,time,l_bnd,Color=ocolor & oplot,time,u_bnd,Color=ocolor ; Do overplot.
endif

if fitEn_range then begin ;  If the Max.-en.-val.-used is to be over-plotted...
   oplot,time,E_bnd,Color=ocolor,Psym=10 ;                         Do overplot.
endif

if (keyword_set(oplot_sec) and lzwin) then begin ;    LZ window overplotting...
   hrday_oplt = (oplot_sec-d.refsec)/3600.d ;                    Do overplot...
   oplot,[hrday_oplt,hrday_oplt],yrange,Color=175
endif  

if keyword_set(oplot_sec_slice) then begin ; Strahl application overplotting...
   hrday_oplt_slice = (oplot_sec_slice-d.refsec)/3600.d ;        Do overplot...
   oplot,[hrday_oplt_slice,hrday_oplt_slice],yrange,Color=wst.clr_orange
endif

if (pm[i].horizlin ne fill) then $ ;               Horizon line overplotting...
   oplot,[hrday[0],hrday[ntimes-1l]],[pm[i].horizlin,pm[i].horizlin],$
         Linestyle=1 ;                                 Plot with a dotted line.

if idlsave then begin ; Saving plotted data for this panel to save to a file...
  idlsav[i].datatype = pm[i].dtp & idlsav[i].varname = pm[i].labl
  idlsav[i].log = pm[i].plotio & idlsav[i].psym = pm[i].psym
  idlsav[i].x = hrday & idlsav[i].y = nmomfvbl & idlsav[i].yrange = yrange
  idlsav[i].yticks = pm[i].ticks & idlsav[i].yminor = pm[i].minor 
endif

end
