
; Convert calendar dates MDY to ordinal day-of-year (DOY)

FUNCTION mdy2doy, MM, DD, YYYY
RETURN, JULDAY(MM, DD, YYYY) - JULDAY(1, 0, yyyy)
END
