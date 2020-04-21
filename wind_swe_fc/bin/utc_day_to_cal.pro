; utc_day_to_cal.pro - 
; Procedure to convert the UTC day to a calendar day.
; JTS 6/98
;
; For more infor see utc_convert.pro
;

;***************************************************
;***************************************************
;**************** MAIN ROUTINE *********************
;***************************************************
;***************************************************
;*******************************************

pro utc_day_to_cal, utc_day, year, day

; utc_day  -  day from UTC = 0 - 9999
; day      -  calendar day
; year     -  calendar year

day = utc_day - 8987  ; 8987 complete days through 1992
if(day lt 0) then day = day + 10000
 
daysums=[365,730,1095,1461,1826,2191,2556,2922,3287,3652,$
         4017,4383,4748,5113,5478,5844,6209,6574,6939,7305,$
         7670,8035,8400,8766,9131,9496,9861,10227]
 
dummy = where( (day-daysums) le 0 )
year = 1993 + dummy(0)
if( (dummy(0)-1) ge 0 ) then day = day - daysums(dummy(0)-1)
;print, year, day, '  ',date(year,day)

end

