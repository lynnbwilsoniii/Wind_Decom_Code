;-------------------------------------------------------------
;+
; NAME:
;       YMD2JD
; PURPOSE:
;       From Year, Month, and Day compute Julian Day number.
; CATEGORY:
; CALLING SEQUENCE:
;       jd = ymd2jd(y,m,d)
; INPUTS:
;       y = Year (like 1987).                    in
;       m = month (like 7 for July).             in
;       d = month day (like 23).                 in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       jd = Julian Day number (like 2447000).   out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner,  23 June, 1985 --- converted from FORTRAN.
;       Johns Hopkins University Applied Physics Laboratory.
;       RES 18 Sep, 1989 --- converted to SUN
;
; Copyright (C) 1985, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------

        function ymd2jd, iy, im, id, help=hlp

        if (n_params(0) LT 3) or keyword_set(hlp) then begin
          print,' From Year, Month, and Day compute Julian Day number.'
          print,' jd = ymd2jd(y,m,d)'
          print,'   y = Year (like 1987).                    in'
          print,'   m = month (like 7 for July).             in'
          print,'   d = month day (like 23).                 in'
          print,'   jd = Julian Day number (like 2447000).   out'
          return, -1
        endif

        y = long(iy)
        m = long(im)
        d = long(id)
        jd = 367*y-7*(y+(m+9)/12)/4-3*((y+(m-9)/7)/100+1)/4 $
             +275*m/9+d+1721029

        return, jd

        end

;-------------------------------------------------------------
;+
; NAME:
;       YMDS2JS
; PURPOSE:
;       Convert to year, month, day, second to "Julian Second".
; CATEGORY:
; CALLING SEQUENCE:
;       js = ymds2js(y,m,d,s)
; INPUTS:
;       y,m,d = year, month, day numbers.   in
;       s = second into day.                in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       js = "Julian Second".               out
; COMMON BLOCKS:
; NOTES:
;       Notes: Julian seconds (not an official unit) serve the
;         same purpose as Julian Days, interval computations.
;         The zero point is 0:00 1 Jan 2000, so js < 0 before then.
;         Julian Seconds are double precision and have a precision
;         better than 1 millisecond over a span of +/- 1000 years.
;       
;       See also js2ymds, dt_tm_fromjs, dt_tm_tojs, jscheck.
; MODIFICATION HISTORY:
;       R. Sterner, 2 Sep, 1992
;
; Copyright (C) 1992, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------

        function ymds2js, y, m, d, s, help=hlp

        if (n_params(0) lt 4) or keyword_set(hlp) then begin
          print,' Convert to year, month, day, second to "Julian Second".'
          print,' js = ymds2js(y,m,d,s)'
          print,'   y,m,d = year, month, day numbers.   in'
          print,'   s = second into day.                in'
          print,'   js = "Julian Second".               out'
          print,' Notes: Julian seconds (not an official unit) serve the'
          print,'   same purpose as Julian Days, interval computations.'
          print,'   The zero point is 0:00 1 Jan 2000, so js < 0 before then.'
          print,'   Julian Seconds are double precision and have a precision'
          print,'   better than 1 millisecond over a span of +/- 1000 years.'
          print,' '
          print,' See also js2ymds, dt_tm_fromjs, dt_tm_tojs, jscheck.'
          return, -1
        endif

        return, s + (ymd2jd(y,m,d)-2451545)*86400d0

        end

;-------------------------------------------------------------
;+
; NAME:
;       JD2YMD
; PURPOSE:
;       Find year, month, day from julian day number.
; CATEGORY:
; CALLING SEQUENCE:
;       jd2ymd, jd, y, m, d
; INPUTS:
;       jd = Julian day number (like 2447000).     in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       y = year (like 1987).                      out
;       m = month number (like 7).                 out
;       d = day of month (like 23).                out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner.  21 Aug, 1986.
;       Johns Hopkins Applied Physics Lab.
;       RES 18 Sep, 1989 --- converted to SUN
;       R. Sterner, 30 Apr, 1993 --- cleaned up and allowed arrays.
;       Theo Brauers, 21 Sep, 1997 long loop index i
;
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------

    pro jd2ymd, jdd, y, m, d, help=hlp

    if (n_params(0) lt 4) or keyword_set(hlp) then begin
      print,' Find year, month, day from julian day number.'
      print,' jd2ymd, jd, y, m, d'
      print,'   jd = Julian day number (like 2447000).     in'
      print,'   y = year (like 1987).                      out'
      print,'   m = month number (like 7).                 out'
      print,'   d = day of month (like 23).                out'
      return
    endif

    jd = long(jdd)              ; Force long.
    y = fix((jd - 1721029)/365.25)      ; Estimated year.
    jd0 = ymd2jd(y, 1, 0)           ; JD for day 0.
    days = jd - jd0             ; Day of year.
    w = where(days le 0, cnt)       ; Find where year is wrong.
    if cnt gt 0 then begin
      y(w) = y(w) - 1           ; Year was off by 1.
      jd0(w) = ymd2jd( y(w), 1, 0)      ; New JD for day 0.
      days(w) = jd(w) - jd0(w)      ; New day of year.
    endif

    ;---  Correct for leap-years.  -----
    ly = (((y mod 4) eq 0) and ((y mod 100) ne 0)) $
            or ((y mod 400) eq 0)

    ;---  Days before start of each month.  -----
    ydays = [0,0,31,59,90,120,151,181,212,243,273,304,334,366]
    off   = [0,0, 0, 1, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1]

    ;----------------  Find which month.  --------------------------
    ;     Algorithm: ydays has cumulative # days up to start of each month
    ;     (13 elements so month number may be used as an index).
    ;     This number needs 1 added for Mar to Dec if it is a leap year.
    ;     This is done by adding the offset, off, times the leap year flag.
    ;     The larger the day of year (days) the fewer elements of this
    ;     corrected ydays array will be greater than days.  The
    ;     entries in the corrected ydays gt days are located by where and
    ;     counted by n_elements.  Ydays has 13 elements so subtract result
    ;     from 13 to get month number.
    ;---------------------------------------------------------------
    njd = n_elements(jd)    ; # of JDs to convert.
    m = intarr(njd)     ; Set up storage for months.
    d = intarr(njd)     ; Set up storage for day of month.
    ; T.B.  i=0L  (was i=0)
    for i = 0L, njd-1 do begin   ; Loop through each JD.
      ydays2 = ydays+ly(i)*off  ; Correct cumulative days for year.
      dy = days(i)          ; Days into year for i'th JD.
      mn = 13-n_elements(where(dy le ydays2))  ; i'th month number.
      m(i) = mn         ; Store month.
      d(i) = fix(dy - ydays2(mn))   ; Find and store i'th day of month.
    endfor

    ;---------  Make sure scalars are returned as scalars  -------
    if n_elements(m) eq 1 then begin
      m = m(0)
      d = d(0)
    endif

    return
    end

;-------------------------------------------------------------
;+
; NAME:
;       JS2YMDS
; PURPOSE:
;       Convert from "Julian Second" to year, month, day, second.
; CATEGORY:
; CALLING SEQUENCE:
;       js2ymds, js, y, m, d, s or js2ymds, js, y, m, d, h, mn, s
; INPUTS:
;       js = "Julian Second".                       in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       y,m,d = year, month, day numbers.           out
;       h, mn, s = hour, minute, second into day.   out
; COMMON BLOCKS:
; NOTES:
;       Notes: Julian seconds (not an official unit) serve the
;         same purpose as Julian Days, interval computations.
;         The zero point is 0:00 1 Jan 2000, so js < 0 before then.
;         Julian Seconds are double precision and have a precision
;         better than 1 millisecond over a span of +/- 1000 years.
;         A precision warning may point to a call to dt_tm_fromjs.
;       
;       See also ymds2js, dt_tm_tojs, dt_tm_fromjs, jscheck.
; MODIFICATION HISTORY:
;       R. Sterner, 2 Sep, 1992
;       R. Sterner, 13 Dec, 1992 --- added data type check.
;       R. Sterner, 2001 Oct 15 --- Now allows h, m, s.
;
; Copyright (C) 1992, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------

        pro js2ymds, js, y, m, d, s, t2, t3, help=hlp

        if (n_params(0) lt 4) or keyword_set(hlp) then begin
          print,' Convert from "Julian Second" to year, month, day, second.'
          print,' js2ymds, js, y, m, d, s or js2ymds, js, y, m, d, h, mn, s'
          print,'   js = "Julian Second".                       in'
          print,'   y,m,d = year, month, day numbers.           out'
          print,'   h, mn, s = hour, minute, second into day.   out'
          print,' Notes: Julian seconds (not an official unit) serve the'
          print,'   same purpose as Julian Days, interval computations.'
          print,'   The zero point is 0:00 1 Jan 2000, so js < 0 before then.'
          print,'   Julian Seconds are double precision and have a precision'
          print,'   better than 1 millisecond over a span of +/- 1000 years.'
          print,'   A precision warning may point to a call to dt_tm_fromjs.'
          print,' '
          print,' See also ymds2js, dt_tm_tojs, dt_tm_fromjs, jscheck.'
          return
        endif

        sz = size(js)
        if sz(sz(0)+1) ne 5 then begin
          print,' Warning in js2ymds: Julian Seconds should be passed in'
          print,'   as double precision.  Precision degraded.'
        endif
        days = floor(js/86400)
        t = js - 86400D0*days
        jd2ymd, days+2451545, y, m, d

        if n_params(0) eq 7 then begin
          sechms, t, s, t2, t3          ; Resolve seconds into day into h,m,s.
        endif else begin
          s = t                         ; Seconds into day.
        endelse

        return
        end

;-------------------------------------------------------------
;+
; NAME:
;       STRSEC
; PURPOSE:
;       Convert seconds after midnight to a time string.
; CATEGORY:
; CALLING SEQUENCE:
;       tstr = strsec(sec, [d])
; INPUTS:
;       sec = seconds after midnight.             in
;         Scalar or array.
;       d = optional denominator for a fraction.  in
; KEYWORD PARAMETERS:
;       Keywords:
;          /HOURS forces largest time unit to be hours instead of days.
; OUTPUTS:
;       tstr = resulting text string.             out
; COMMON BLOCKS:
; NOTES:
;       Notes: Output is of the form: [DDD/]hh:mm:ss[:nnn/ddd]
;         where DDD=days, hh=hours, mm=minutes, ss=seconds,
;         nnn/ddd=fraction of a sec given denominator ddd in call.
;         If sec is double precision then 1/10 second can be
;         resolved in more than 10,000 days.  Use double precision when
;         possible. Time is truncated, so to round to nearest second,
;         when not using fractions, add .5 to sec.
; MODIFICATION HISTORY:
;       Written by R. Sterner, 8 Jan, 1985.
;       Johns Hopkins University Applied Physics Laboratory.
;       RES --- Added day: 21 Feb, 1985.
;       RES 19 Sep, 1989 --- converted to SUN
;       RES 18 Mar, 1990 --- allowed arrays.
;       R. Sterner, 27 Jan, 1993 --- dropped reference to array.
;       R. Sterner, 12 Feb, 1993 --- returned 1 element array as a scalar.
;       also cleaned up.
;
; Copyright (C) 1985, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------

        function strsec,sec0,d, help=hlp, hours=hrs

        if (n_params(0) lt 1) or keyword_set(hlp) then begin
          print,' Convert seconds after midnight to a time string.'
          print,' tstr = strsec(sec, [d])'
          print,'    sec = seconds after midnight.             in'
          print,'      Scalar or array.
          print,'    d = optional denominator for a fraction.  in'
          print,'    tstr = resulting text string.             out'
          print,' Keywords:'
          print,'    /HOURS forces largest time unit to be hours '+$
            'instead of days.'
          print,' Notes: Output is of the form: [DDD/]hh:mm:ss[:nnn/ddd]'
          print,'   where DDD=days, hh=hours, mm=minutes, ss=seconds,'
          print,'   nnn/ddd=fraction of a sec given denominator ddd in call.'
          print,'   If sec is double precision then 1/10 second can be'
          print,'   resolved in more than 10,000 days.  Use double precision'+$
            ' when'
          print,'   possible. Time is truncated, so to round to nearest '+$
            'second,'
          print,'   when not using fractions, add .5 to sec.'
          return, -1
        endif

        seca = sec0
        nn = n_elements(seca)           ; Number of elements in input array.
        out = strarr(nn)                ; Output string array.

        ;-------------------------------------------------------------------
        for ii = 0, nn-1 do begin       ; Loop through all input elements.
        sec = seca(ii)                  ; Pull out the ii'th element.
        t = double(sec)                 ; Convert to double.
        dy = long(t/86400)              ; # days.
        t = t - 86400*dy                ; Time without days.
        h = long(t/3600)                ; # hours.
        t = t - 3600*h                  ; Time without hours.
        if keyword_set(hrs) then begin  ; If /HOURS then convert days to hours.
          h = h + 24*dy
          dy = 0
        endif
        m = long(t/60)                  ; # minutes.
        t = t - 60*m                    ; Time without minutes.
        s = long(t)                     ; Seconds.
        f = t - s                       ; Time without seconds (=fraction).

        ;------  Make days string  --------
        sdy = ''                                ; Day part of string, def=null.
        if dy gt 0 then begin
          if dy lt 1000 then begin              ; 3 digit day.
            sdy = string(dy,form='(I3.3,"/")')  ; Convert days to string.
          endif else begin
            sdy = strtrim(dy,2)+'/'             ; GE 3 digit day.
          endelse
        endif
        ;------  Make hours string (may be big)  -----
        if h lt 10 then begin
          hh = string(h,form='(I2.2)')          ; 2 digit hour.
        endif else begin
          hh = strtrim(h,2)                     ; GE 2 digit hour.
        endelse
        ;------  Make mm:ss string  ----------
        fmt = '(I2.2,":",I2.2)'         ; hh:mm:ss format like 12:34:32
        shms = sdy + hh + ':' + string(m,s,form=fmt)
        ;------  Make fraction of second string  ----------
        if n_params(0) ge 2 then begin          ; Also want fraction of second.
          sd = strtrim(string(d),2)             ; Convert denom. to string.
          ln = strtrim(strlen(strtrim(d-1,2)),2)  ; Length of numerator as str.
          n = long(d*f+.5)                      ; Find numerator.
          fmt = '(I'+ln+'.'+ln+')'              ; Numerator format.
          sn = string(n,form=fmt)               ; Convert numerator to string.
          shms = shms+':'+sn+'/'+sd             ; Tack on fraction as a string.
        endif

        out(ii) = shms

        endfor
        ;-------------------------------------------------------------------

        if n_elements(out) eq 1 then out=out(0) ; 1 elem: return as a scalar.

        return, out

        end


;-------------------------------------------------------------
;+
; NAME:
;       DATA_PLOT 
; PURPOSE:
;       This program is designed to make a plot for THEMIS data.
; CATEGORY:
; CALLING SEQUENCE:
;       DATA_PLOT, filename, variable, time_range, data_title, xticks
; INPUTS:
;       filename = the filename of an IDL SAVE file      
;       variable = a variable in the IDL SAVE file     
;       time_range = time range, format: 'start_time-end_time', 'hh:mm:ss-hh:mm:ss'  
;       data_title = the title of the variable, format: 'data_title' 
;       xticks = the number of intervals in time axis  
; MODIFICATION HISTORY:
;       Space Environment Labboratory , 19 Nov, 2007
;
; Copyright (C) 2007, Space Environment Labboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made. This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
pro DATA_PLOT,filename,variable,time,data_title,xticks

variable=variable
restore,filename
ini_hour=fix(strmid(time,0,2))
ini_minute=fix(strmid(time,3,2))
ini_second=fix(strmid(time,6,2))
end_hour=fix(strmid(time,9,2))
end_minute=fix(strmid(time,12,2))
end_second=fix(strmid(time,15,2))

sod=hour*3600.+minute*60.+second
js=ymds2js(year,month,day,sod)	

ini_sod=ini_hour*3600.+ini_minute*60.+ini_second
end_sod=end_hour*3600.+end_minute*60.+end_second
xmin=ymds2js(year,month,day,ini_sod)
xmax=ymds2js(year,month,day,end_sod)
xticks=xticks
delta=findgen(xticks+1)
delta=delta*(xmax-xmin)/xticks+xmin
timlbl=strarr(xticks+1)
blanklbl=replicate(' ',xticks+1)

for i=0,xticks do begin
  js2ymds,delta(i),y,m,d,s
  strsecond=strsec(s)
  timlbl(i)=strmid(strsecond,0,8)
endfor

device,decomposed=0
loadct,5

title=string(y,'(i4.4)')+'-'+string(m,'(i2.2)')+'-'+string(d,'(i2.2)')
window,xsize=500,ysize=300
ytitle=data_title
plot,js,variable,xstyle=1,ystyle=1,xrange=[xmin,xmax],xticks=xticks,$
                 xtickname=timlbl,xmargin=[10,10],color=0,background=255,$
                 xtitle='Time',ytitle=ytitle,title=title
end

