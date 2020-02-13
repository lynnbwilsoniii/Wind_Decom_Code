;+
;FUNCTION: time_struct(time)
;NAME:
;  time_struct
;PURPOSE:
; A fast, vectorized routine that returns a time structure.
;INPUT:  input can be any of the following types:
;  double(s)      seconds since 1970
;  string(s)      format:  YYYY-MM-DD/hh:mm:ss
;  structure(s)   similar to format below.
;
;OUTPUT:
;  structure with the following format:
;** Structure TIME_STRUCT, 11 tags, length=40:
;   YEAR            INT           1970            ; year    (0-14699)
;   MONTH           INT              1            ; month   (1-12)
;   DATE            INT              1            ; date    (1-31)
;   HOUR            INT              0            ; hours   (0-23)
;   MIN             INT              0            ; minutes (0-59)
;   SEC             INT              0            ; seconds (0-59)
;   FSEC            DOUBLE           0.0000000    ; fractional seconds (0-.999999)
;   DAYNUM          LONG            719162        ; days since 0 AD  (subject to change)
;   DOY             INT              0            ; day of year (1-366)
;   DOW             INT              3            ; day of week  (subject to change)
;   SOD             DOUBLE           0.0000000    ; seconds of day
;   DST        =    INT      = 0                  ; Daylight saving time flag
;   TZONE      =    INT      = 0                  ; Timezone  (Pacific time is -8)
;   TDIFF      =    INT      = 0                  ; Hours from UTC
;
;See Also:  "time_double", "time_string", "time_epoch", "time_pb5"
;
;NOTE:
;  This routine works on vectors and is designed to be fast.
;  Output will have the same dimensions as the input
;
;CREATED BY:    Davin Larson  Oct 1996
;FILE:  time_struct.pro
;VERSION:  1.15
;LAST MODIFICATION:  02/11/01
;-
function time_struct,time,epoch=epoch,no_clean=no_clean,MMDDYYYY=MMDDYYYY, $
   timezone=timezone,  $
   local_time=local_time, $
   informat = informat,  $
   is_local_time=is_local_time     ; this keyword is not yet working correctly

dprint,dlevel=9,time[0]
dt = size(/type,time)
if keyword_set(timezone) then begin
   local_time=1
   tzone = timezone
endif
;tzone = keyword_set(timezone) ? timezone : 0

tst0 = {time_structr,year:0,month:0,date:0,hour:0,min:0,sec:0, $
        fsec:!values.d_nan, daynum:0l,doy:0,dow:0,sod:!values.d_nan, $
        dst:0,tzone:0,tdiff:0}
dim =   size(/dimension,time)
ndim =  size(/n_dimen,time)
if ndim eq 0 then tsts =tst0 else tsts = make_array(value=tst0,dim=dim)

if dt eq 7 then begin         ; input is a string
  bt = bindgen(256)
  bt[byte(':_-/,')]= 32
  year=0l & month=0 & date=0 & hour=0 & min=0 & fsec=0.d
  for i=0l,n_elements(time)-1l do begin
    tst = tst0
    str = string(bt[byte(time[i])])+' 0 0 0 0 0 0'    ; remove separators
    if keyword_set(MMDDYYYY) then reads,str,month,date,year,hour,min,fsec  $
    else  reads,str,year,month,date,hour,min,fsec

    if year gt 10000000l then begin
       hour = month
       date = year mod 100
       year = year/100
       month = year mod 100
       year = year/100
;       if hour gt 99999 then begin
;          fsec = hour mod 100
;          hour = hour / 100
 ;      endif
  ;     if hour gt 999 then begin
          min = hour mod 100
          hour = hour / 100
   ;    endif
    endif
    if year lt 70  then year = year+2000
    if year lt 200 then year = year+1900
;    if not (year eq 0 and month eq 0 and date eq 0)
    month = month > 1
    date = date > 1
    tst.year=year
    tst.month=month
    tst.date=date
    tst.hour=hour
    tst.min=min
    tst.fsec=fsec
    tsts[i]=tst
  endfor
  if keyword_set(no_clean) then return,tsts $
  else begin
     t = time_double(tsts)
     if keyword_set(is_local_time) then begin
dprint,dlevel=9,'is_local_time'
        dst = isdaylightsavingtime(t,tzone)
        t -= (dst+tzone)*3600
     endif
     return,time_struct(t,timezone=timezone,local_time=local_time)
  endelse
endif

if keyword_set(epoch) then return,time_struct(time_double(time,epoch=epoch),timezone=timezone)


if dt eq 5 or dt eq 4 or dt eq 3 or dt eq 14 then begin         ; input is a double or integer
    good = where(finite(time),ngood)
    if  ngood gt 0 then begin
        ltime = time[good]
        if keyword_set(local_time)  then begin
           dst = isdaylightsavingtime(time[good],tzone)
           ltime += (tzone+dst) * 3600l
           tsts[good].dst = dst
           tsts[good].tzone = tzone
           tsts[good].tdiff = tzone+dst
        endif
        dn1970 = 719162l        ; day number of 1970-1-1
        dn = floor(ltime/3600.d/24.d)
        sod = ltime - dn*3600.d*24.d
        daynum = dn + dn1970
        hour = floor(sod/3600.d)
        fsec = sod - hour*3600.d
        min  = floor(fsec/60.d)
        fsec  = fsec - min*60
        sec  = floor(fsec)
        fsec = fsec - sec
        day_to_year_doy,daynum,year,doy
        doy_to_month_date,year,doy,month,date
        tsts[good].month= month
        tsts[good].date = date
        tsts[good].year = year
        tsts[good].doy  = doy
        tsts[good].hour = hour
        tsts[good].min  = min
        tsts[good].sec  = sec
        tsts[good].fsec = round(fsec*1d6)/1d6
        tsts[good].sod  = sod
        tsts[good].daynum = daynum
        tsts[good].dow = daynum mod 7
    endif
    return,tsts
endif

if dt eq 8 then  $             ;input is a structure
   return,time_struct(time_double(time),timezone=timezone)

if dt eq 0 then  $             ;input is undefined
   return,time_struct(time_string(time,prec=6))

message,/info,'Improper time input'

end



