;+
; PROJECT:
;       SOHO - CDS/SUMER
;       THEMIS
;
; NAME:
;       DPRINT
;
; PURPOSE:
;       Diagnostic PRINT (activated only when DEBUG reaches DLEVEL)
;
; EXPLANATION:
;       This routine acts similarly to the PRINT command, except that
;       it is activated only when the common block variable DEBUG is
;       set to be equal to or greater than the debugging level set by
;       DLEVEL (default to 0).  It is useful for debugging.
;       If DLEVEL is not provided it uses a persistent (common block) value set with the
;       keyword SETDEBUG.
;
; CALLING SEQUENCE (typically written into code):
;       DPRINT, v1 [,v2 [,v3...]]] [,format=format] [,dlevel=dlevel] [,verbose=verbose]
;             The values of v1,v2,v3 will only be printed if verbose >= dlevel
;
; CALLING SEQUENCE to change options (typically typed from IDL command line)
;       DPRINT, setdebug=d   ; define persistent debug level
;       DPRINT, print_trace=[0,1,2, or 3]  ; Display program trace info in subsequent calls to DPRINT
;       DPRINT, /print_dtime       ; Display delta time between DPRINT statements.
;
; INPUTS:
;       V1, V2, ... - List of variables to be printed out (20 max).
;
; OPTIONAL INPUTS:
;       None.
;
; OUTPUTS:
;       All input variables are printed out on the screen (or the
;       given unit)
;
; OPTIONAL Keywords:
;       FORMAT - Output format to be used
;       UNIT   - Output unit through which the variables are printed. If
;                missing, the standard output (i.e., your terminal) is used.
;
; KEYWORD PARAMETERS:
;       DLEVEL = DLEVEL - An integer indicating the debugging level; defaults to 0
;       VERBOSE = VERBOSE - An integer indicating current verbosity level, If verbose is set
;       it will override the current value of debug, for the specific call of dprint in which
;       it is set.
;       SETDEBUG=value            - Set debug level to value
;       GETDEBUG=named variable   - Get current debug level
;       DWAIT = NSECONDS  ; provides an additional constraint on printing.
;              It will only print if more than NSECONDS has elapsed since last dprint.
;
; CALLS:
;       PTRACE()
;
; COMMON BLOCKS:
;       DPRINT_COM.
;
; RESTRICTIONS:
;     - Changed see SETDEBUG above
;       Can print out a maximum of 12 variables (depending on how many
;          is listed in the code)
;
; SIDE EFFECTS:
;       None.
;
; CATEGORY:
;       Utility, miscellaneous
;
; PREVIOUS HISTORY:
;       Written March 18, 1995, Liyun Wang, GSFC/ARC
;
; MODIFICATION HISTORY:
;       Version 1, Liyun Wang, GSFC/ARC, March 18, 1995
;       Version 2, Zarro, SM&A, 30 November 1998 - added error checking
;       Version 3, Zarro, (EIT/GSFC), 23 Aug 2000 - removed DATATYPE calls
;       Version 4, Larson  (2007) stripped out calls to "execute" so that it can be called from IDL VM
;                          Fixed bug that allows format keyword to be used.
;                          Added PTRACE() call
;                          Added SETDEBUG keyword and GETDEBUG keyword
;                          Added DWAIT keyword
;                          Added PRINT_TRACE,PRINT_DTIME,PRINT_DLEVEL
;                          Added Widget options
;
;-



function dprint_header,sublevel=sublevel  ,delta_time=delta_time
  common dprint_com, dprint_struct

  delta_time=2.1345
  prefix = ''
  if dprint_struct.print_dlevel then  prefix=[prefix, string(dlevel,dbg,format='(i0.0,"/",i0.0)') ]
  if dprint_struct.print_time   then  prefix=[prefix, time_string(tformat=dprint_struct.tformat,newtime,/local)]
  if dprint_struct.print_dtime  then  prefix=[prefix, string(format='(f6.3)',delta_time) ]
  if dprint_struct.print_trace  then  begin
    stack = scope_traceback(/structure,system=1)
    level = n_elements(stack) -1
;    if level gt 200 then begin
;       Message,"Stack is too large! Runaway recursion?"
;    endif
    if keyword_set(sublevel) then level -= sublevel
    level = level > 1
    stack = stack[0:level-1]
;    levels = indgen(level)
;    stacknames=strtrim(levels,2)+'  '+stack.routine + string(stack.line,format='(" (",i0,")")')
    stacknames=stack.routine + string(stack.line,format='(" (",i0,")")')
    prefix = [prefix,stacknames]
  endif
  return,prefix
end


PRO DPRINT,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,  $
           v11,v12,v13,v14,v15,v16,v17,v18,v19,v20, $
           format=format, $            ;  Like the format string in print
           dlevel=dlevel,  $           ;  Overides the debug level
           verbose=verbose,$
           setdebug=setdebug, $
           getdebug=getdebug,$
           filename=filename, $
           print_dlevel=print_dlevel,  $
           print_time = print_time   , $
           print_dtime =print_dtime,   $
           print_trace= print_trace,  $
           break_requested = break_requested,  $
    ;       names=names,  $                            ; ?????? don't use it!
           dwait=dwait,  $
           reset=reset,  $
           sublevel=sublevel, $
           phelp=phelp,  $
           unit=unit

   on_error,2

   common dprint_com, dprint_struct
;   common dprint_com, debug, lasttime, lastflushtime, print_dlevel_c,  $
;      print_dtime_c, print_time_c, print_trace_c, file_unit_c, file_name_c, check_events, tplottool_id, break_flag
   newtime = systime(1)

   if not keyword_set(dprint_struct) or keyword_set(reset) then dprint_struct={  $
       debug:FIX(getenv('DEBUG')), $
       lasttime:newtime, $
       lastflushtime:0d, $
       print_dlevel:0, $
       print_time:0,  $
       tformat:'',   $
       print_dtime:0,  $
       print_trace:0,  $
       file_unit:-1,    $
       file_name:'',   $
       check_events:0, $
       widget_id:0l,   $
       widget_lasttime:0d, $
       widget_dwait:0d,  $
       break_flag:0,   $
       ireturn:0  }

  ; if dprint_struct.ireturn then return        ; do nothing (used to avoid unlimited recursion)

;   if not keyword_set(dprint_struct.lasttime) then dprint_struct.lasttime = newtime
;   if not keyword_set(dprint_struct.lastflushtime) then dprint_struct.lastflushtime = newtime
   getdebug = dprint_struct.debug
;   if n_elements(file_unit_c) eq 0 then file_unit_c = -1  ; standard output
   np = N_PARAMS()
;   if np eq 0 then begin
      if n_elements(print_dlevel) ne 0 then dprint_struct.print_dlevel=print_dlevel
      if n_elements(print_dtime)  ne 0 then begin
          dprint_struct.print_dtime =print_dtime
          dprint_struct.lasttime = newtime
      endif
      if n_elements(print_time)   ne 0 then dprint_struct.print_time  = print_time
      if n_elements(print_trace)  ne 0 then begin
          dprint_struct.print_trace = print_trace
  ;        dummy= ptrace(option=dprint_struct.print_trace)
      endif
      if n_elements(filename) ne 0 then begin
          if dprint_struct.file_unit gt 0  then free_lun,dprint_struct.file_unit
          dprint_struct.file_unit = -1
          if keyword_set(filename)  then begin
              openw,unit,filename,/get_lun
              dprint_struct.file_unit = unit
              fs = fstat(unit)
              dprint_struct.file_name_c = fs.name
          endif
      endif
      if n_elements(setdebug) ne 0 then begin
          dprint_struct.debug = setdebug
         ; return
      endif
;      return
;   endif

   IF N_ELEMENTS(dlevel) EQ 0 THEN dlevel = 0
   delta_time = newtime-dprint_struct.lasttime

   if keyword_set(dprint_struct.check_events) then begin
      event= widget_event(/nowait)
   endif

   if keyword_set(dwait) and not keyword_set(dprint_struct.break_flag) then begin
      if dwait ge delta_time  then return
   endif

   if newtime-dprint_struct.lastflushtime gt 10. then begin
      dprint_struct.lastflushtime = newtime
      wait,.01    ; This wait statement is the only way I know of to flush the print buffer. This is a low overhead.
   endif

   dbg = n_elements(verbose) ne 0 ? verbose : dprint_struct.debug
   IF dlevel GT dbg and not keyword_set(dprint_struct.break_flag) THEN RETURN

   dprint_struct.ireturn = 1

   prefix = ''
;   if keyword_set(dprint_struct.print_dlevel) then prefix = prefix + string(dlevel,dbg,format='(i0.0,"/",i0.0,": ")')
;   if keyword_set(dprint_struct.print_time)   then prefix = prefix + time_string(tformat=dprint_struct.tformat,newtime,/local)+' '
;   if keyword_set(dprint_struct.print_dtime)  then prefix = prefix + string(format='(f6.3,": ")',delta_time)
;   if keyword_set(dprint_struct.print_trace)  then prefix = prefix + ptrace(/sublevel)

   if dprint_struct.print_dlevel then  prefix=[prefix, string(dlevel,dbg,format='(i0.0,"/",i0.0)') ]
   if dprint_struct.print_time   then  prefix=[prefix, time_string(tformat=dprint_struct.tformat,newtime,/local)]
   if dprint_struct.print_dtime  then  prefix=[prefix, string(format='(f6.3)',delta_time) ]
   if dprint_struct.print_trace ne 0  then  begin
     stack = scope_traceback(/structure,system=1)
     level = n_elements(stack) -1
;     if level gt 200 then begin
;        Message,"Stack is too large! Runaway recursion?"
;     endif
     if keyword_set(sublevel) then level -= sublevel
     level = level > 1
     stack = stack[0:level-1]
;     levels = indgen(level)
;     stacknames=strtrim(levels,2)+'  '+stack.routine + string(stack.line,format='(" (",i0,")")')
     stacknames=stack.routine + string(stack.line,format='("(",i0,")")')
     case dprint_struct.print_trace of
        1: if level ge 2 then stacknames = stacknames[level-1]
        2: if level ge 2 then stacknames[0:level-2] = '  '
        else:   ; do nothing
     endcase
     prefix = [prefix,stacknames]
  endif
  if keyword_set(prefix) then prefix = prefix[1:*]

   dprint_struct.lasttime = newtime

   if dprint_struct.file_unit gt 0 then begin   ; perform safety check
       fs = fstat(dprint_struct.file_unit)
       if fs.open eq 0 or fs.name ne dprint_struct.file_name then begin
           dprint_struct.file_unit = -1
           dprint_struct.file_name = ''
       endif
   endif
   u = n_elements(unit) ? unit : dprint_struct.file_unit
 ;  printdat,prefix
   if keyword_set(prefix) then printf,u,strjoin(prefix+': '),format='(a,$)'

   if keyword_set(phelp) then begin
        vnames0=scope_varname(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
        vnames1=scope_varname(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,level=-1)
        for i=0,np-1 do begin
           printdat,unit=u,scope_varfetch(vnames0[i]),varname=vnames1[i],recursemax=phelp ;,pgmtrace=2,width=300
        endfor
        dprint_struct.ireturn=0
        return
   endif


;   if keyword_set(names) then begin  ; Huh????
;        vnames=scope_varname(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,level=-1,count=count)
;        printf,u,ptrace(/sublevel),'<'+vnames[0:n_params()-1]+'>',format='(a,20a8)'
;   endif

   case np of
   0:  text = ''  ;string(/print,format=format)
   1:  text = string(/print,format=format,v1)
   2:  text = string(/print,format=format,v1,v2)
   3:  text = string(/print,format=format,v1,v2,v3)
   4:  text = string(/print,format=format,v1,v2,v3,v4)
   5:  text = string(/print,format=format,v1,v2,v3,v4,v5)
   6:  text = string(/print,format=format,v1,v2,v3,v4,v5,v6)
   7:  text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7)
   8:  text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8)
   9:  text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9)
   10: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
   11: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11)
   12: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
   13: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13)
   14: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14)
   15: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15)
   16: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16)
   17: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17)
   18: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18)
   19: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19)
   20: text = string(/print,format=format,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20)
   else: text = 'Get real! 20 variables is enough!'
   endcase


   printf,u,text

   if keyword_set(dwait) then wait, .01      ; This  line is used to flush the print buffer (update the display)

   if keyword_set(dprint_struct.widget_id) and newtime-dprint_struct.widget_lasttime ge dprint_struct.widget_dwait then begin
        dprint_struct.widget_lasttime = newtime
        dprinttool,/update,text,prefix=prefix ,/sublevel
   endif

   dprint_struct.ireturn=0

   if keyword_set(dprint_struct.break_flag) then begin
       dprint_struct.break_flag = 0
       message,'Break Detected'
   endif
   return

END

;---------------------------------------------------------------------------
; End of 'dprint.pro'.
;---------------------------------------------------------------------------
