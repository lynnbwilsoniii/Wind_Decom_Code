pro time_conv,fraction=fraction,hhmm=hhmm,hours=hours

;fraction and hhmm should be integers or floats, not strings
;ex:   time_conv,fraction=0.53 
;ex:   time_conv,hhmm=1523.35     (3:23 pm plus 35/100ths of a minute)
;ex:   time_conv,hours=9.5643     (9.5643 hours into the day)

print,'Time conversion program.'
print,''

if keyword_set(fraction) then begin 
    hour=fix(fraction*24)
    min=(fraction*24-hour)*60

    print,'fraction of day           = ', strcompress(fraction,/remove_all)
    print,'hour:min.fraction of min  = ', strcompress(string(hour,':',min),$
                                                     /remove_all)
    print,'hour.fraction of hour     = ', strcompress(fraction*24,/remove_all)

endif
if keyword_set(hhmm) then begin
    hour=fix(hhmm/100)
    min=hhmm-hour
    
    print,'hour:min.fraction of min  = ', strcompress(string(hour,':',min),$
                                                     /remove_all)
    print,'fraction of day           = ', strcompress((hour+min/60)/24,/remove)
    print,'hour.fraction of hour     = ', strcompress(hour+min/60,/remove_all)

endif

if keyword_set(hours) then begin 
    hour=fix(hours)
    min=(hours-hour)*60

    print,'hour.fraction of hour     = ', strcompress(hours,/remove_all)
    print,'fraction of day           = ', strcompress(hours/24,/remove_all)
    print,'hour:min.fraction of min  = ', strcompress(string(hour,':',min),$
                                                     /remove_all)
endif
end
