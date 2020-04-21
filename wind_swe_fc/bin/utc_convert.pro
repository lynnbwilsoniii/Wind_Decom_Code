; utc_convert.pro - 
; Procedure to convert the 6 byte UTC time code to 
; year, day of year, seconds of day.
; JTS 6/98
;
; See the sibling file utc_day_to_cal.pro

;cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
; NOTES FROM JERRY NEEDELL.
; The UTC consists of 2 values:
;  utc= 32 bit unsigned integer containing the GGS TIME code
;   ms= 16 bit unsigned integer containing the ones complement if the
;              millisecond counter reset by the UTC signal.
;
;   Every second, the spacecraft sends a signal (UTC interrupt) to SWE
;   indicating that a new time code word is available. SWE hardware 
;   resets  a millisecond counter upon receipt of this interrupt. SWE then
;   reads the UTC value and saves it, 
;      The UTC code contains 32 bits:
;      the 15 most significant bits indicate the day and the 17 least sig. 
;      bits indicate the seconds of that day. At this time, I do not know
;      what the reference time will be (launch? , Jan 1? your birthday?)
;      During I&T it has been set to real time or left to the time the 
;      spacecraft was turned on.
;         

;      The millisecond value must be complemented. The counter is a
;      "down-counter" so it is initialized to ffff thus a count of ffff
;       represents 0 . after 1 millisecond it will read fffe which complements       to 1. ....

;      The data values are stored low to high byte in the telemetry, that is
;      the first byte read is the low byt of the UTC.

;      UTC CODE BYTE      0   1   2   3    4    5  
;                    utc  0   1   2   3
;                                      ms  0    1
;cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


;***************************************************
;***************************************************
;**************** MAIN ROUTINE *********************
;***************************************************
;***************************************************

pro utc_convert, utc_6byte, year, day, sec


;***Variables passed to this routine:
;
; utc_6byte     - a 6 byte array from the FCFF block
;                 in most modes this is the 3rd-8th byte of
;                 the FCFF block 
;
;***Variables returned from this routine:
;
; year          - calendar year
; day           - day of year
; sec           - decimal seconds: isec + imsec/1000.
;
;
;***Other internal variables:
;
; utc           - long(utc_6byte)
; utc_day       - a number from 0 - 9999
;                 This counter rolled over from 9999 to 0000 on
;                 Oct 10, 1995.  This code should be OK until 2117.
; isec          - seconds at UTC interrupt
; imsec         - milliseconds after UTC interrupt


;***Initilizations

year = 0L
day = 0L
sec = 0.0D
;*****************


;***Begin calculations

utc = long(utc_6byte)

isec =  utc(0) + 256*utc(1) + 65536*(utc(2) and 1)

utc_day = (utc(2) + 256*utc(3) )/2

imsec = 65535 - utc(4) - 256*utc(5) 
if(imsec gt 999) then imsec = 0

sec = isec*1.0D + imsec/1000.0D

;print, 'isec', isec,'  imsec',imsec,'  iday',utc_day

utc_day_to_cal, utc_day, year, day 

end

;*******************************************
;*******************************************
;*******************************************
;*******************************************

; pro utc_day_to_cal, utc_day, year, day

; day = utc_day - 8987  ; 8987 complete days through 1992
; if(day lt 0) then day = day + 10000
 
; daysums=[365,730,1095,1461,1826,2191,2556,2922,3287,3652,$
;          4017,4383,4748,5113,5478,5844,6209,6574,6939,7305,$
;          7670,8035,8400,8766,9131,9496,9861,10227]
 
; dummy = where( (day-daysums) le 0 )
; year = 1993 + dummy(0)
; if( (dummy(0)-1) ge 0 ) then day = day - daysums(dummy(0)-1)
; print, year, day, '  ',date(year,day)

; end

