; tnr6.pro

ok = 0l
ch = 0l
i4buf = lonarr(512)
r4buf = fltarr(512)
r8buf = dblarr(512)
bufsz =        512L
EOF = 82L
ver = '12345678'
item = '    '
retsz = 0L
cbufsz = 0L
t1 = 0.d0
file='WIND_DATA:wi_lz_wav_19950214_v01.dat'
file='/home/wind/data_test/wi_lz_wav_19950214_v01.dat'
get_items = 1L
n_events = 0L
n_err = 0L
a = 0.d0
b = 0.d0

ok = w_channel_open(ch,file)
if (ok ne 1) then stop

ok = w_version(ver)
print, 'Using wind_lib version ', ver
print, 'get_items = ', get_items

print, 'File: ', file
print, ' '

while ok ne EOF do begin
   ok = w_event(ch,'TNR')
   if ok ne 1 then n_err = n_err + 1
   if ok eq 1 then n_events = n_events + 1

   if ok eq 1 and get_items eq 1 then begin
      item = 'SPECTRA_1'
      ok = w_item_i4(ch,item,i4buf,bufsz,retsz)

      item = 'SPECTRA_1_MICROVOLTS_R4'
      ok = w_item_r4(ch,item,r4buf,bufsz,retsz)

      item = 'SPECTRA_1_SCET_R8'
      ok = w_item_r8(ch,item,r8buf,bufsz,retsz)

      item = 'EVENT_MODE'
      cbuf = 'abcdefghijklmnop'
      ok = w_item_char(ch,item,cbuf,1L,retsz)

      item = 'ANTENNA'
      cbuf = 'abcdefghijklmnop'
      ok = w_item_char(ch,item,cbuf,1L,retsz)
   endif
endwhile

print, 'Processed ', n_events, ' events.'
print, 'Events with errs: ', n_err
stop
end
