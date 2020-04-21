
; remove obsolete versions of the mag data

pro mfi_catalogue

dir = '/crater/observatories/wind/mfi/mfi_h0/2013/*'
fl = file_search(dir)

fl05 = file_search('/crater/observatories/wind/mfi/mfi_h0/2013/*v05*')

for i = 0, n_elements(fl05)-1 do begin &$
  temp = strmid(fl05[i], 0, 62) &$
  fltemp04 = file_search(temp + '*v04*') &$
  fltemp03 = file_search(temp + '*v03*') &$
  if fltemp04 ne '' then spawn, 'rm ' + fltemp04 &$
  if fltemp03 ne '' then spawn, 'rm ' + fltemp03 &$
endfor

fl04 = file_search('/crater/observatories/wind/mfi/mfi_h0/2013/*v04*')

for i = 0, n_elements(fl04)-1 do begin &$
  temp = strmid(fl04[i], 0, 62) &$
  fltemp03 = file_search(temp + '*v03*') &$
  if fltemp03 ne '' then spawn, 'rm ' + fltemp03 &$
  endfor

dir = '/crater/observatories/wind/mfi/mfi_h0/2013/*'
fl = file_search(dir)


