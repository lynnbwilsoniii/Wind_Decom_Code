;============================= VDist_Tser (v1.1) ==============================
;           M. Holland, R. Fitzenreiter (last modified: 07/26/01)
;                     Interplanetary Physics Branch
;                Laboratory for Extraterrestrial Physics
;            Goddard Space Flight Center: Greenbelt, MD 20771
;
; **** Time-series plot of solar wind velocity distributions (v1.0):
; The widget defined in the routines in this file is intended to be used as a
;  part of the SWEDAT analysis tool originally developed by R. Fitzenreiter.
; This widget locates all the level zero (LZ) data starting from the requested
;  begin time and proceeding through the LZ data set, forming distributions
;  from each spinblock it finds, until the selected number of minutes
;  (starting from the begin time) has been processed.
; "Processing" consists of certain initialization and calculation procedures,
;  as well as the storage of the acquired distributions in a dynamically
;  growing distribution table.  Finally, this distribution table is passed to
;  a display routine which plots the analysis results in the display window.
;
; **** Version 1.1 (initiated 07/25/01):
; This version contains a simple addition of code to the display routine,
;  allowing for "live" viewing of the parallel and perpendicular distribution
;  time series.  See IDL documentation concerning "live" visualization--in
;  particular, see: LIVE_STYLE procedure (adjusts "live" style settings) and
;                 LIVE_SURFACE procedure (generates 3D visualization widget).
; Note: These "live" sessions are generated any time a LZ time series is, and
;        one result is always a modification of the current color table.  Use
;        the 'restore colors' selection in the SWE_levelzero SWEDAT widget (or
;        somehow generate an event that has the same effect) to fix this.
;==============================================================================

;============================= fmat_Insert ==================================
; Given the table of solar wind velocity distributions, 'fparaperpF', and the
;  "current" parallel, perpendicular and reduced (to a function of parallel
;  velocity only) solar wind velocity distributions: 'fpara','fperp' and 'F',
;  respectively; this function inserts these into 'fparaperpF'.  Note that,
;  if no data is contained in the given distributions, blank rows are
;  inserted into the distribution table for the "current" record and spin.
function fmat_insert,fparaperpF

; The common block, 'vdtserst', containing the logical constants and return
common vdtserst,true,false,error_value,success_val ; values, is defined here.
common fplt ;  In this reference to the common block, 'fplt', we access
; 'fpara', 'fperp' and 'F' (the "current" parallel, perpendicular and
; reduced--to a function of parallel velocity only--vel. dist. functions).
; Also from this common block; we may use 'nx', the number of velocity-steps
; (see fparr.pro), and 'vmax', the maximum velocity-step value.

; Note: In IDL, matrices are referenced as in: F[col,row,level,...], and
;        concatenation proceeds along the dimension given by the number of
;        levels of nesting of the '[...]' concatenation operator.  So one
;        level of nesting (e.g. [A,B]) takes all the columns of each level
;        of B and concatenates them onto the corresponding levels of A, such
;        that the total number of columns in each level of the result of
;        concatenation is the sum of the number of columns in each level of
;        A with that of B.  The preceeding concatenation has no effect on the
;        resultant number of rows or levels, (i.e. A, B and the result all
;        the same number of rows and levels).
;       These remarks apply in the obvious way to two and three levels of
;        nesting of the concatenation operator (i.e. concatenation along the
;        row and level dimensions when nesting progresses to two and three
;        levels, respectively).

; If there are no velocity distributions for this spin then insert blanks...
if ((fpara(0) eq error_value) and (fperp(0) eq error_value) and $
                                      (F(0) eq error_value)) then begin
   ; Insert row of zero (double) values into growing tables of distributions.
   if (fparaperpF(0) ne error_value) then begin ; Not first dists. in table.
      fpara = [ [reform(fparaperpF[*,*,0])],[dblarr(nx)] ]
      ; 'fpara' should be: 'nx'-cols x <num_dists>-rows x 1-level.

      fperp = [ [reform(fparaperpF[*,*,1])],[dblarr(nx)] ]
      ; 'fperp' should be: 'nx'-cols x <num_dists>-rows x 1-level.

          F = [ [reform(fparaperpF[*,*,2])],[dblarr(nx)] ]
      ;     'F' should be: 'nx'-cols x <num_dists>-rows x 1-level.
   endif else begin ; There are no dists. in the table; these are the first.
      if (not keyword_set(nx)) then nx = 129 ; Default value, if not defined.
      ; Note: ^-This is the best solution to a very rarely occurring problem.
      fpara = dblarr(nx) & fperp = dblarr(nx) & F = dblarr(nx)
      ; each of the above should be: 'nx'-cols x 1-row x 1-level.
   endelse
endif else begin ; Otherwise insert distributions into distribution table...
   ; Insert acquired velocity distributions into growing tables of dists...
   if (fparaperpF(0) ne error_value) then begin ; Not first dists. in table.  
      fpara = [ [reform(fparaperpF[*,*,0])],[fpara] ] ; PARALLEL dists.
      ; 'fpara' should be: 'nx'-cols x <num_dists>-rows x 1-level.

      fperp = [ [reform(fparaperpF[*,*,1])],[fperp] ] ; PERPEND. dists.
      ; 'fperp' should be: 'nx'-cols x <num_dists>-rows x 1-level.

          F = [ [reform(fparaperpF[*,*,2])],[F] ]     ; REDUCED  dists.
      ;     'F' should be: 'nx'-cols x <num_dists>-rows x 1-level.
   endif ; Otherwise, concat. in return statement forms first table entries.
endelse

return,[[[fpara]],[[fperp]],[[F]]] ; Return combined distribution table.
; Output table should be: 'nx'-cols x <num_dists>-rows x 3-levels.

end


;========================== Get_VDtser_LZdata ================================
; This routine establishes the main (nested) loop for gathering LZ data.  The
;  outer loop proceeds until the "current" elapsed time (since the begin time)
;  exceeds the selected number of minutes and while the last record in the
;  active LZ file has yet to be processed.  The inner loop iterates through
;  the <nspins> spinblocks in the "current" record.
; There is some error correction, which involves the passing of error values
;  if no data is found.  A list of any missing requested spinblocks is
;  maintained and output to the screen after all data is gathered.  Also, the
;  screen output generated by existing tool routines called by this routine
;  is encapsulated in matching headers and footers generated by this routine.
function get_vdtser_LZdata,time_begin,minutes

common vdtserst ; Provides access to the logical constants and return vals.
common wstuff & common swestuff ; Provides access to 'wst', 'swest' structs.
common lzstuff ; Provides access to: 'recn': the "current" record number,
;                                    'fh': the 'file header' structure, and
;                                    'vsmjf': the 'VEIS major frame' struct.
common sharewidg ; The common-block, 'sharewidg', contains the 'WDGT'
;            structure of widget state variables for main widgets in SWEDAT.
common fplt ; In this reference to the common block, 'fplt', we access
; 'fpara', 'fperp' and 'F' (the "current" parallel, perpendicular and
; reduced--to a function of parallel velocity only--vel. dist. functions).

; Initialize all loop variables before entering nested loops...
fparaperpF = error_value ; Initialize distribution table contents with
;                 default error value, for the case where no LZ data is found.
min_spinbl = 0 & max_spinbl = vsmjf.n_spins-1 ; Set bounds on spinblock loops.
; Initialize list of missing spins (for screen output) to empty, and set
spins_not_found = strarr(1) & some_spins_missing = false ;    "missing" flag.
; Initialize the number of elapsed seconds to zero, set end time.
sec_el = 0. & sec_el_end = minutes*60. ; 60 secs. for each requested min.

; Process records until the last requested record or end-of-file is reached...
; Note: 'fh.nmf': <file header>.<number of major frames (records) this file>.
while ((sec_el le sec_el_end) and (recn le fh.nmf)) do begin

   ; Loop through the <nspins> spinblocks of the current major frame...
   while ((sec_el le sec_el_end) and (swest.ispinbl le max_spinbl)) do begin
      ; Add the dists. for this record-and-spin to the dist. table...
      print,'' & print,'*****************' ; Display record-and-spin header.
      print,'VDist_Tser: BEGIN record-and-spin: ',recn,'-and-',swest.ispinbl
      ; Initialize to the error value for the case of spinblock proc. error.
      F = error_value & fpara = error_value & fperp = error_value
      proc_fw,/NoPltf,Pltype=error_value ; **************Get "current" dists.
      ; Note: /NoPltf==>NO "automatic" plotting of "current" distributions.
      ;       Pltype=error_value because bogus value selects NO plot-type.
      if ((fpara(0) eq error_value) and (fperp(0) eq error_value) and $
          (F(0) eq error_value)) then err = true else err = false ; Set flag.
      fparaperpF = fmat_insert(fparaperpF) ; **Put "current" dists. in table.
      ; Note: If the "current" dists. are still set to error values when they
      ;        are to be inserted into the dist. table, blanks are inserted.

      if (err) then begin ; If there was an error in proc. spinbl ...
         print,'Error in processing spinblock detected by PRO: VDist_Tser.'
         print,'' ; Print error message if no data was found, then skip line.
         ; Store record-and-spin information in a string for screen output.
         spin = string(recn,Format='(i4)')+':'+$ ; ~10^3 (4 dig.) recs/day,
                string(swest.ispinbl,Format='(i1)') ; < 10 (1 dig.) spins/rec.
         
         ; Add this record-and-spin to list of missing requested spins...
         if (some_spins_missing) then begin ; At least one spin in the list.
            spins_not_found = [spins_not_found,spin] ; Add new spin to list.
         endif else begin ;         Enter first spin into list and set flag.
            spins_not_found(0) = spin & some_spins_missing = true
         endelse
      endif

      print,'VDist_Tser: END record-and-spin: ',recn,'-and-',swest.ispinbl
      print,'*****************' & print,'' ; Display record-and-spin footer.   
      swest.ispinbl = swest.ispinbl+1 ; Incr. "current" spin number by one.
      Widget_CONTROL,WDGT.swelz_hmsfld,Get_value=hms_now ; Get "curr." time.
      sec_el = (hms_msecday(long(  hms_now(0)   ))-$ ; Find elapsed number of
                hms_msecday(long( time_begin(0) )))/1000. ; secs. this spin.
   endwhile
   
   recn = recn+1 ; Increment "current" record number by one record.
   wst.lz_is_read = false ; Indicate that new "current" record not been read.
   swest.ispinbl = min_spinbl ; Set "current" spinblock to first in record.
endwhile

if (some_spins_missing) then begin ; Final output to screen for this routine.
   print,'VDist_Tser: List of missing requested spinblocks (recn:spinbl):'
   print,spins_not_found & print,'' ; Skip line after final screen output.
endif

return,fparaperpF ; Return distribution table containing all located data.

end


;============================ Display_Dists ===================================
; Given a table of velocity distributions called 'fparaperpF', this procedure
;  displays these distributions three-dimensionally as a time-series.  The
;  F, fpara and fperp distribution plots are respectively sent to the three
;  'swe_levelzero'-widget plot windows (on the botton of the widget).
; The string, 'time_begin' (hhmmss) provides plot annotation information about
;  the begin time selected by the user.  The elapsed seconds parameter is
;  defined with respect to this begin time.
PRO display_dists,fparaperpF,time_begin,drawid1,drawid2,drawid3

common vdtserst ; Provides access to the logical constants and return vals.
common fplt ;  In this reference to the common block, 'fplt', we access
; 'fpara', 'fperp' and 'F' (the "current" parallel, perpendicular and
; reduced--to a function of parallel velocity only--vel. dist. functions).
; Also from this common block; we may use 'nx', the number of velocity-steps
; (see fparr.pro), and 'vmax', the maximum velocity-step value.
common lzstuff ; Provides access to: 'fh': the 'file header' structure, and
;                                    'vsmjf': the 'VEIS major frame' struct.
common sharewidg ; The common-block, 'sharewidg', contains the 'WDGT'
;            structure of widget state variables for main widgets in SWEDAT.

; "Strip off" levels of distribution table, yielding the individual tables.
fpara = reform(fparaperpF[*,*,0]) & fperp = reform(fparaperpF[*,*,1])
    F = reform(fparaperpF[*,*,2]) ; Reduced distributions (across perp. dir.).

; Using the information from calls to 'proc_fw', find the range of x values.
if (not keyword_set(vmax)) then vmax = 17.5E8 ; Default value, if not defined.
; Note: If this value is not defined here, we are in a pathological situation
;       where no distributions were found, so this value matters very little.
half_nx = floor(nx/2.) & X = ((findgen(nx)-float(half_nx))/half_nx)*vmax*1.E-8
xrange = [X(0),X(n_elements(X)-1)] ; Define the range of velocities for plot.
xstyle = 1 & ystyle = 1 & zstyle = 1 ; Set plot-style vals.->"tight" plotting.

; Using the content of the current LZ file header, find num. secs. per rec.
print,'Information about the current LZ file:'
	print,'*) first PB5 time: ',[fh.fst.yr,fh.fst.dy,fh.fst.ms]
	print,'*) last PB5 time:  ',[fh.lst.yr,fh.lst.dy,fh.lst.ms]
	print,'*) number of records: ',fh.nmf
timpb5fst=[fh.fst.yr,fh.fst.dy,fh.fst.ms] ; First PB5 time in current LZ file.
timpb5lst=[fh.lst.yr,fh.lst.dy,fh.lst.ms] ;  Last PB5 time in current LZ file.
secfst = pb5_sec(timpb5fst) & seclst = pb5_sec(timpb5lst); Secs. from tjd epoch.
print,'Averaged over an entire day...' ; Preface next telemetry rate value.
sec_rec = (seclst-secfst)/float(fh.nmf) & print,'*) num. secs./rec.: ',sec_rec
Widget_CONTROL,WDGT.swelz_hmsfld,Get_value=hms_now ; Get "curr." time.
sec_el = (hms_msecday(long(  hms_now(0)   ))-$ ; Find elapsed number of
          hms_msecday(long( time_begin(0) )))/1000. ; secs. in sel. interval.
print,'Averaged over selected interval...' ; Preface next telem. rate value.
sec_spin = sec_el/(size(fparaperpF))[2] & print,'*) num. secs./spin: ',sec_spin
; Y = indgen((size(fparaperpF))[2])*(sec_rec/vsmjf.n_spins) ; Elapsed seconds.
  Y = indgen((size(fparaperpF))[2])*sec_spin ; (<-replaces-^) Elapsed seconds.
; Note: (size(fparaperpF))[2] is the number of distributions in the table.

; Possibly perfom formatting on the 'time_begin' label provided by the tool...
num_char_time_begin = strlen(time_begin(0)) ; 'time_begin' format: 'hhmmss'.
missing_char_tm_beg = 6-num_char_time_begin ; There should be exactly 6 chars.
;        We do nothing if there are too many characters (or the right amount).
if (missing_char_tm_beg gt 0) then begin ; If there are too few characters...
   for curr_char=1,missing_char_tm_beg do begin ; For each missing char...
      time_begin(0) = '0'+time_begin(0) ; Add a zero to "left" side of time.
   endfor ; Add to the left side because this is the only side from which the
endif ;      zeros should be missing.  E.g. 00:00:00--midnight--becomes:
;            '0' when stored in 'time_begin', and 00:01:00--1:00 AM--becomes:
;            '100' when stored in 'time_begin').  We fix this truncation here.

wFz = where((F eq 0.d),wFzcnt) & wset,drawid1 ; =================> REDUCED.
if (wFzcnt ne 0) then F(wFz) = !Values.D_NaN
surface,alog10(F),X,Y,Charsize=1.5,Ycharsize=1.2,Zcharsize=1.5,$
        Xrange=xrange,Xstyle=xstyle,Xtitle='reduced para. velocity (Mm/s)',$
        Ystyle=ystyle,Ytitle='seconds since (hhmmss): '+time_begin(0),$
        Zstyle=zstyle,Ztitle='!18l o g !II ()!N ( f r e d )!3',$
        /Horizontal,/Upper_only,Az=0,Ax=70

wfparaz = where((fpara eq 0.d),wfparazcnt) & wset,drawid2 ; =====> PARALLEL.
if (wfparazcnt ne 0) then fpara(wfparaz) = !Values.D_NaN
surface,alog10(fpara),X,Y,Charsize=1.5,Ycharsize=1.2,Zcharsize=1.5,$
        Xrange=xrange,Xstyle=xstyle,Xtitle='parallel velocity cut (Mm/s)',$
        Ystyle=ystyle,Ytitle='seconds since (hhmmss): '+time_begin(0),$
        Zstyle=zstyle,Ztitle='!18l o g !II ()!N ( f p a r a )!3',$
        /Horizontal,/Upper_only,Az=0,Ax=70

wfperpz = where((fperp eq 0.d),wfperpzcnt) & wset,drawid3 ; =====> PERPEND.
if (wfperpzcnt ne 0) then fperp(wfperpz) = !Values.D_NaN
surface,alog10(fperp),X,Y,Charsize=1.5,Ycharsize=1.2,Zcharsize=1.5,$
        Xrange=xrange,Xstyle=xstyle,Xtitle='perpend. velocity cut (Mm/s)',$
        Ystyle=ystyle,Ytitle='seconds since (hhmmss): '+time_begin(0),$
        Zstyle=zstyle,Ztitle='!18l o g !II ()!N ( f p e r p )!3',$
        /Horizontal,/Upper_only,Az=0,Ax=70

; This block of code activates a "live" session where the distribution time
;  series can be manipulated in three dimensions, annotated and printed.
; Note: A side-effect of this code is the modification of the curr. color tbl.
fcuts_style = live_style('surface',Base_Style='Basic Surface',$ ; Surf. style.
                         Graphic_Properties={style:3},$ ; Same style as above.
                         Legend_Properties={hide:false}) ; DO show legend.
                         ; Set 'hide' to 'true' to not display legend on plot.
live_surface,alog10(fpara),Style=fcuts_style,Title=$ ; "Live" window title.
            'f_para--starting at (hhmmss): '+time_begin(0),$ ; (start time).
             Xindependent=X,Yindependent=Y,Name={$ ; Structure of data labels.
             data:'log_10 f_para',$ ; Dependent variable label (for legend).
             ix:'parallel velocity cut in Mm per sec',$ ; (ind.) X-axis label.
             iy:'sec since hhmmss '+time_begin(0)} ; (independ.) Y-axis label.
live_surface,alog10(fperp),Style=fcuts_style,Title=$ ; "Live" window title.
            'f_perp--starting at (hhmmss): '+time_begin(0),$ ; (start time).
             Xindependent=X,Yindependent=Y,Name={$ ; Structure of data labels.
             data:'log_10 f_perp',$; Dependent variable label (for legend).
             ix:'perpend velocity cut in Mm per sec',$ ; (ind.) X-axis label.
             iy:'sec since hhmmss '+time_begin(0)} ; (independ.) Y-axis label.

end


;=========================== VDist_Tser_Event =================================
; This is the event-handler for the widget which displays velocity-distribution
;  time-series plots ('vdist_tser').  The common block, 'vdst'--which contains
;  the widget's state variables--is defined here.
PRO vdist_tser_event,EVENT

; The reference to the common-block, 'vdtserst', provides access to the logical
common vdtserst ;                       constants and the error/success values.
common vdst,vdst_tlb,vdst_rb1,vdst_rb2,vdst_quit,vdst_tbeg,vdst_mins,vdst_plt
common sharewidg ; The common-block, 'sharewidg', contains the 'WDGT' structure
;                         of widget state variables for main widgets in SWEDAT.

; This logical variable is always initially assumed to be false.  It will
;  be set to true--triggering a plot of the dists.--at the user's request.
lplt = false ;                                     Initialize plotting flag.

; Determine which event occurred, the nature of the event, and the proper
;  sequence of actions to perform in response...
case EVENT.id of ;                  Who (which widget) initiated the event?
	vdst_quit: Widget_Control,EVENT.top,/Destroy ; The quit button did.

	vdst_mins: begin ; The minutes field did, so perform error control.
	   Widget_CONTROL,vdst_mins,Get_Value=minutes ; Get number of minutes.
	      
	   if (minutes le 0) then begin ; If minutes is neg., set to default.
	      minutes = 5 & Widget_CONTROL,vdst_mins,Set_Value=minutes
	   endif ; We cannot allow a non-positive number of requested minutes.
	endcase

	vdst_plt: lplt = true ; The plot button did, so set plot flag.

	else: ; Fall through (don't do anything, nothing important happened).
endcase

; The quit button might have been pressed, and the widget killed, so we need
;                                       to guard against using a dead widget...
if xregistered('vdist_tser') then begin 
   ; The get/set sequence improves the input-field interface (over just 'get').
   Widget_CONTROL,vdst_tbeg,Get_Value=time_begin ; Get/set the 'Begin:' value.
   Widget_CONTROL,vdst_tbeg,Set_Value=time_begin
   Widget_CONTROL,vdst_mins,Get_Value=minutes ;  Get/set the 'Minutes' value...
   if (minutes le 0) then minutes = 5 ; Error correction for nonsense input.
   Widget_CONTROL,vdst_mins,Set_Value=minutes & print,'' ;          Skip line.
   print,"*VDist_Tser: Begin(hhmmss): ",time_begin & print,"Minutes: ",minutes
   
   if (lplt eq true) then begin ; If plotting was selected by user...
      ; Gather together data from the requested days, store in 'fparaperpF'.
      print,'Begin gathering data and plotting distributions...'
      fparaperpF = get_vdtser_LZdata(time_begin,minutes) ; ****New dist. tbl.
      print,'All located LZ data has been compiled into a distribution table.'
      help,fparaperpF ; Output info. about final contents of compiled data set.

      if (fparaperpF(0) ne error_value) then begin ; If dist. tbl. has data...
         ; Locate plotting windows and display contents of dist. tables...
         Widget_CONTROL,WDGT.swelz_draw1,Get_Value=drawid1 ; Find window IDs
         Widget_CONTROL,WDGT.swelz_draw2,Get_Value=drawid2 ;  for desired
         Widget_CONTROL,WDGT.swelz_draw3,Get_Value=drawid3 ;  plot locations.
         display_dists,fparaperpF,time_begin,drawid1,drawid2,drawid3
         print,'VDist_Tser: Completed plotting.' & print,'' ; Final output.
      endif else begin ; Otherwise, the dist. table contains no data...
         print,'VDist_Tser error: No data found for selected time interval.'
         print,'' ; Print error message if no data was found, then skip line.
      endelse
      
      Widget_Control,EVENT.top,/Destroy ; Always destroy widget after plot.
   endif
endif

end


;============================ VDist_Tser ===================================
; This is the top-level procedure in the set of routines in this file.  The
;  user has provided input to this routine in the form of input parameters:
;  'time_begin': A string containing display information about the approx.
;                timestamp of the first user-requested LZ record.
;  'minutes': An integer giving the number of minutes elapsed between the
;             timestamp on the first user-requested LZ record and the last.
; Input is also provided in the form of the common block references:
;  'vdst': Defined in the event handler for this widget and containing the
;          widget state variables like the widgetID numbers, etc.
;  'sharewidg': Defined in 'define_widgets.pro', contains 'WDGT' structure
;               of widget state variables for main widgets in SWEDAT.
;  'vdtserst': Defined in the first routine in this file, provides access to
;              the logical constants and the error/success values.
; This widget definition routine, together with the event handling routine
;  above, create a dialog window interface for displaying velocity
;  distribution time series to the "swe_levelzero" widget.
PRO vdist_tser,time_begin,minutes

common vdst & common sharewidg & common vdtserst ; Common block references.

; Limit this widget to one instantiation.  This is important because,
;  otherwise, every instance of this widget would be sharing the same
;  common-block containing the widget's state variables.
if xregistered('vdist_tser') then return

error_value = -1 ; Define function return value indicating FAILURE.
success_val = 1 ; Define function return value indicating SUCCESS.
true = 1 & false = 0 ; Define logical constant values.

; The top-level-base needs no parentID value parameter.
;  All of the children of this widget are to be arranged in a single column.
; Note: This widget is being defined as a MODAL dialog window, which suspends
;        event occurrances in all widgets of its group while it is active.
vdst_tlb = Widget_BASE(Title='F,f time series',Column=1,$
                       Group_Leader=WDGT.swelz_base_main,/Modal,/Floating,$
                       TLB_Frame_Attr=(1+8)) ; 1: No resize, min. or max.
                       ;                       8: No closing of this dialog.

; There are two sub-bases whose children will be arranged in single rows.
vdst_rb1 = Widget_BASE(vdst_tlb,Row=1) ; Contains cancel (quit) button.
vdst_rb2 = Widget_BASE(vdst_tlb,Row=1) ; Contains input fields and plot button.

; The first row sub-base contains the cancel (quit) button.
vdst_quit = Widget_BUTTON(vdst_rb1,Value='Cancel') ; This destroys the widget.

; The second row sub-base contains the time selectors and plot button.
;    Note: Frame=1 => display compound widget with a 1-pixel frame around it.
; Note: The <time_begin> field may not be modified, time is selected on the
;       survey plot window of the "swe_levelzero" widget.
vdst_tbeg = CW_FIELD(vdst_rb2,Title='Begin: hhmmss',Frame=1,/NoEdit,$ ;
                     /Return_Events,/String,Xsize=6,Ysize=1,/Row) ; STRING!!!
vdst_mins = CW_FIELD(vdst_rb2,Title='Minutes',Frame=1,$ ; Num. minutes field.
                     /Return_Events,/Integer,Xsize=3,Ysize=1,/Row) ; INTEGER!
vdst_plt = Widget_BUTTON(vdst_rb2,Value='Plot') ; This plots the distributions.

Widget_CONTROL,vdst_tlb,/Realize ; Instantiate the widget defined above.

; Initialize input-field data using user-selected time and interval length.
Widget_CONTROL,vdst_tbeg,Set_Value=time_begin ;   Set the 'Begin:' value.
Widget_CONTROL,vdst_mins,Set_Value=minutes ;      Set the 'Minutes' value.

; Hand this widget (name and top lev. base ID num.) off to the widget manager.
Xmanager,'vdist_tser',vdst_tlb ; Widget is now registered (one copy only).

end
