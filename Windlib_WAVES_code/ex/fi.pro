; fi.pro - tests accessing successive files in idl

ch         = long(0)
ok         = 0L
ver        = 'abcdefghijklmnopqrstuvwxyz'
file       = 'abcdefghijklmnopqrstuvwxyz--------------------------------------'

print, '...getting the version number...'
;ok = w_version(ver)
print, 'Using wind_lib version: ', ver

first=19960401L
f0=first
   print, '...file: ', file

next_file:
   print, '----------------------------------------------------------'
   print,'...opening the channel...f0=', f0
   fs = string(f0)
;removes leading and trailing blanks-->   fs = '*' + strtrim(fs,2) + '*'
   fs = '*' + fs + '*'
   fs = strcompress(fs, /remove_all)
   print,'...opening the channel...fs=', fs
   ok = w_channel_open(ch, fs)
   print,'...channel open status:  `,ok=',ok
   if ok ne 1L then begin
      print, '...channel not opened... ok=', ok
      goto, bottom
   endif
   ok = w_channel_filename(ch,file)
   print, ' '
   print, '...file: ', file
   print, ' '
   ok = w_channel_close(ch)

bottom:
   f0 = f0 + 1
   if f0 lt (first + 4) then goto, next_file 

stop
end

