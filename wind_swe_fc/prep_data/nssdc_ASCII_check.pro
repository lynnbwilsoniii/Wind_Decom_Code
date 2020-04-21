
; quick algorithm to check for *'s and NaN's in the ascii data

pro nssdc_ASCII_check, yy=yy

if keyword_set(yy) then spawn, 'ls /crater/observatories/wind/swe/nssdc/*'+yy+'*.txt', files else $
  spawn, 'ls /crater/observatories/wind/swe/nssdc/*.txt', files

for i = 0, n_elements(files)-1 do begin
  print, 'Checking: ' + files[i]
  temp = read_ascii(files[i])
  dt = temp.field01[58, *] - temp.field01[1, *]
  badout = where(strpos(temp.field01, '*') gt 0, nbad)
  nans = where(strpos(temp.field01, 'NaN') gt 0, nnan)
  badtiming = where(dt lt 0 or dt gt 92./86400., nbadtiming)

  if nbad gt 0 or nnan gt 0 or nbadtiming gt 0 then begin
      print, 'Bad output found in this file. nssdc_ascii_check HALTED'
      stop
  endif

endfor

end



; error log: 
;
; checked OK: 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003,
; 2002, 2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994
