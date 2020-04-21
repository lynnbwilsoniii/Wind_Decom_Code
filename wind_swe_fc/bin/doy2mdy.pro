
; CONVERT year, doy to calendar dates

PRO doy2mdy, y, doy, mdy
CALDAT, JULDAY(1, 0, y, 0, 0, 0)+DOY, month, day, year, hour, minute, second
mdy = [year, month, day, hour, minute, second]
END
