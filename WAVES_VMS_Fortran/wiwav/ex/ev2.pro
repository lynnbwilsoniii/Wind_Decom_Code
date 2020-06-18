; ev.pro - sample wind_lib event gathering program.  This program gets a
; rad2 event at half hour intervals throughout a file.  For each event the
; S values are gathered, the SCET of each S value gathered, and the 
; EVENT_SCET is printed.
;
; sun_wind_idl.pro must be run prior to running this program.
;

;@/home/wind/src/sun_wind_idl.pro

;
; procedure PRINT_VERSIONS
;
pro print_versions, ch

item  = '0123456789012345678901'
buf   = '012345678'
buf_sz = long(8)
ret_sz = long(0)
ok = 0L

item = 'ITEM_DB_VERSION'
ok = w_item_char(ch,item,buf,buf_sz,ret_sz)
if ok ne 1 then print, 'cannot get item ', item $
else print, strtrim(item), ' is ', buf, format = '(a, a, a8)'

item = 'WIND_LIB_VERSION'
ok = w_item_char(ch,item,buf,buf_sz,ret_sz)
if ok ne 1 then print, 'cannot get item ', item $
else print, strtrim(item), ' is ', buf, format = '(a, a, a8)'

return
end

;
; procedure PRINT_ITEMS
;
pro print_items, ch

item  = '0123456789012345678901'
buf_sz = long(1)
ret_sz = long(0)
scet = double(0)
s_scet = '0123456789012345678901234567890'
ok = 0L
i  = 0L
obvs_time = dblarr(512)
obvs = dblarr(512)

item = 'EVENT_SCET_R8'
ok = w_item_r8(ch,item,scet,1L,ret_sz)

; reformat and print the event time
ok = w_ur8_to_string(scet, s_scet)
if ok ne 1 then stop, 'cannot convert ur8 time to string' $
else print, 'SCET of beginning of event: ', scet, ' ', strtrim(s_scet)

; get the s values
item = 'S'
ok = w_item_r8(ch, item, obvs, 512L, ret_sz)
if ok ne 1 then stop, 'cannot get item ', strtrim(item)
print, 'got item ', strtrim(item)
print, ret_sz, 'S values: ', obvs(1:10)

; get the scet of each s observation
item = 'S_UR8'
ok = w_item_r8(ch, item, obvs_time, 512L, ret_sz)
if ok ne 1 then stop, 'cannot get item ', strtrim(item) 
print, ret_sz, 'S_UR8 values: ', obvs_time(1:10)

return
end

;
; main program
;

ok = long(0)
ch = long(0)
file = '                                                       '
ur8_half_hour = double(0)
time  = double(0)
stime = '0123456789012345678901'
i = long(0)
j = long(0)
tm_lib = 'wind_tm_lib'			; contains wind_tm_* routines
time_lib = 'wind_time2_lib'		; contains time conversion routines

ur8_half_hour = 0.0;  1.0/24.0/2.0

; open a channel to the telemetry
ok = w_channel_open(ch, 'offline')
if ok ne 1 then stop, 'cannot open channel, ok=', ok $
else print, 'TM stream opened on channel ', ch

; get and print the name of the selected file
ok = w_channel_filename(ch, file)
print, 'File: ', strtrim(file)

; get the initial position in Ulysses real*8 format time
ok = w_channel_position(ch, time)
if ok ne 1 then stop, 'cannot get initial time position'

; reformat and print the time
ok = w_ur8_to_string(time, stime)
if ok ne 1 then stop, 'cannot convert ur8 time to string' $
else print, 'Initial stream position is: ', time, ' ', strtrim(stime)

; get the first event
ok = w_event(ch, 'RAD2') 
if ok ne 1 then stop, 'cannot get first event'
print_versions, ch

while ok eq 1 do begin
   print_items, ch
;   time = time + ur8_half_hour
;   ok = w_channel_position(ch, time)
;   if ok ne 1 then begin
;      print, 'cannot move to stream position by time, ok=', ok
;      return
;   endif else begin
;      ok = w_ur8_to_string(time, stime)
;      print, 'moved to new stream position ', strtrim(stime)
      ok = w_event(ch, 'RAD2') 
;   endelse
endwhile

end
