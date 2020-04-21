pro get_current, spinmax, specmax, counts, current_lookup, currents

; this procedure takes the array containing the count information and 
; looks up each value of the count

currents = fltarr(20, spinmax, 4, specmax+1)
for i = 0, 3 do currents[*, *, i, *] = current_lookup[i, counts[*, *, i, *]]

end
