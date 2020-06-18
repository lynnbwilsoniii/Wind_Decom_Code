; ttnr6.pro
;@/home/wind/src/wind_idl.pro	! sunos
;@wdir:wind_idl.pro		! vms

ok = 0l
ch = 0l
i4buf = lonarr(512)
r4buf = fltarr(512)
r8buf = dblarr(512)
bufsz =        512L
EOF = 82L
ver = '12345678'
file = '                                               '
item = '    '
retsz = 0L
cbufsz = 0L
t1 = 0.d0

ok = w_channel_open(ch,'offline')
if (ok ne 1) then stop

ok = w_version(ver)
print, 'Using wind_lib version ', ver

ok = w_channel_position(ch, t1)
print, '...got the first position...ok=', ok
;ok = w_channel_position(ch, t1)
;print, '...got the second position...ok=', ok


ok = w_channel_filename(ch,file)
print, 'File: ', file
print, ' '

while ok ne EOF do begin
   ok = w_event(ch,'TNR')
   if ok ne 1 then print, 'cannot get event, ok=', ok

   if ok eq 1 then begin
      item = 'SPECTRA_1'
      ok = w_item_i4(ch,item,i4buf,bufsz,retsz)
      if ok eq 1 then begin
         print, item, ': ', i4buf(0:4)
      endif else begin
         print, 'cannot get item ', item, ', ok=', ok
      endelse

      item = 'SPECTRA_1_MICROVOLTS_R4'
      ok = w_item_r4(ch,item,r4buf,bufsz,retsz)
      if ok eq 1 then begin
         print, item, ': ', r4buf(0:4)
      endif else begin
         print, 'cannot get item ', item, ', ok=', ok
      endelse

      item = 'SPECTRA_1_SCET_R8'
      ok = w_item_r8(ch,item,r8buf,bufsz,retsz)
      if ok eq 1 then begin
         print, item, ': ', r8buf(0:4)
      endif else begin
         print, 'cannot get item ', item, ', ok=', ok
      endelse

      item = 'EVENT_MODE'
      cbuf = 'abcdefghijklmnop'
      ok = w_item_char(ch,item,cbuf,1L,retsz)
      if ok eq 1 then begin
         print, item, ': ', cbuf
      endif else begin
         print, 'cannot get item ', item, ', ok=', ok
      endelse

      item = 'ANTENNA'
      cbuf = 'abcdefghijklmnop'
      ok = w_item_char(ch,item,cbuf,1L,retsz)
      if ok eq 1 then begin
         print, item, ': ', cbuf
      endif else begin
         print, 'cannot get item ', item, ', ok=', ok
      endelse
   endif
endwhile

stop
end
