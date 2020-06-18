; sun_wind_idl.pro - provides a nice, transparent interface to wind_lib routines
; for IDL 
;
; Note:  The first argument to the IDL CALL_EXTERNAL function is the
; complete path and file specification for the shared library libIDL_WAVES.so.
; If this is a new installation make sure this path is correct, otherwise
; IDL will not be able to call wind_lib routines.
;
; Jan-1995

function w_channel_open, ch, context
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_open', long(ch), context)
return, ok
end

function w_channel_select, ch, context, t1, t2
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_select', long(ch), context, double(t1), double(t2))
return, ok
end

function w_channel_close, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_close', long(ch))
return, ok
end

function w_channel_position, ch, ur8
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_position', long(ch), double(ur8))
return, ok
end

function w_channel_filename, ch, filename
ok = 0L
big = '                                                  '
if strlen(filename) eq 0 then filename = big+big+big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_channel_filename', long(ch), filename)
filename = strtrim(filename,2)
return, ok
end

;
; messages, version
;


function w_messages_off, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_messages_off', long(ch))
return, ok
end

function w_messages_on, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_messages_on', long(ch))
return, ok
end

function w_version, ver
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(ver) eq 0 then ver = big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_version', ver)
ver = strtrim(ver,2)
return, ok
end

;
; event/item
;

function w_event, ch, event
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_event', long(ch), event)
return, ok
end

function w_status, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_status', long(ch))
return, ok
end

function w_item_char, ch, item, buf, buf_sz, ret_sz
ok = 0L
i = 0L
j = 0L
k = 0L
n = 0L

v = size(buf)
if v(0) eq 0 then begin		; scalar string
   ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
       'w_item_char_idl', long(ch), item, buf, 1L, long(ret_sz))
;   ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), ok
endif

; for an idl array of strings emulate a fortran array of strings
n = n_elements(buf)			; # of array elements
if buf_sz lt n then n = buf_sz

i = strlen(buf(0))		; assume length of first element is ok for all

if i eq 0 then i = 16		; supply a default for uninitialized data
j = i*n				; total number of bytes
fmt = '(a'+strtrim(string(j),2)+')'
c = string(format=fmt,' ')	; create an empty scaler, blank filled

ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
        'w_item_char_idl_ary', $
        ch, item, c, n, ret_sz, i)

; copy scaler into array in fixed size chunks
for k=0,ret_sz-1 do begin
   buf(k) = strmid(c, k*i, i)
endfor
return, ok
end

function w_item_i4, ch, item, buf, buf_sz, ret_sz
ok = 0L

ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_item_i4_idl', long(ch), item, long(buf), long(buf_sz), $
     long(ret_sz))

return, ok
end

function w_item_r4, ch, item, buf, buf_sz, ret_sz
ok = 0L

ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_item_r4_idl', long(ch), item, float(buf), long(buf_sz), $
     long(ret_sz))

return, ok
end

function w_item_r8, ch, item, buf, buf_sz, ret_sz
ok = 0L

ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_item_r8_idl', long(ch), item, double(buf), long(buf_sz), $
     long(ret_sz))

return, ok
end

function w_item_xlate, ch, event, item, item_val, item_str
ok = 0L
big = '                                                  '
if strlen(item_str) eq 0 then item_str = big+big+big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_item_xlate', $
     long(ch), event, item, long(item_val), item_str)
item_str = strtrim(item_str,2)
return, ok
end

;
; physical units, old
;

function w_phys_fft_r8, ch, cal, start_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_phys_fft_r8', ch, cal, start_sz, ret_sz)
return, ok
end

function w_phys_rad1_r8, ch, cal, start_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_phys_rad1_r8', ch, cal, start_sz, ret_sz)
return, ok
end

function w_phys_rad2_r8, ch, cal, start_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_phys_rad2_r8', ch, cal, start_sz, ret_sz)
return, ok
end

function w_phys_tds_r8, ch, cal, start_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_phys_tds_r8', ch, cal, start_sz, ret_sz)
return, ok
end

function w_phys_tnr_r8, ch, cal, start_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_phys_tnr_r8', ch, cal, start_sz, ret_sz)
return, ok
end

;
; time conversions
;


function w_ur8_from_ydoy, ur8, year, doy, msec
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_ur8_from_ydoy', double(ur8), long(year), long(doy), $
     long(msec))
return, ok
end

function w_ur8_from_ymd, ur8, year, month, day, hour, minute, second, msec
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_ur8_from_ymd', $
     double(ur8), long(year), long(month), long(day), long(hour), $
     long(minute), long(second), long(msec))
return, ok
end

function w_ur8_to_string, ur8, str
ok = 0L
if strlen(str) eq 0 then begin
   str = 'dd-mmm-yyyy, hh:mm:ss.xxx....'
endif
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_ur8_to_string', double(ur8), str)
str = strtrim(str,2)
return, ok
end

function w_ur8_to_string_fr, ur8, str
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(str) eq 0 then str = big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_ur8_to_string_fr', double(ur8), str)
str = strtrim(str,2)
return, ok
end

function w_ur8_to_ydoy, ur8, year, doy, msec
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_ur8_to_ydoy', double(ur8), long(year), long(doy), $
     long(msec))
return, ok
end

function w_ur8_to_ymd, ur8, year, month, day, hour, minute, second, msec
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'w_ur8_to_ymd', $
      double(ur8), long(year), long(month), long(day), long(hour), $
      long(minute), long(second), long(msec))
return, ok
end

function w_ur8_to_ymd_i, ur8, ymd, hms
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'w_ur8_to_ymd_i', double(ur8), long(ymd), long(hms))
return, ok
end

function w_ur8_from_ymd_i, ur8, ymd, hms
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'w_ur8_from_ymd_i', double(ur8), long(ymd), long(hms))
return, ok
end

function w_ur8_to_epoch, ur8, epoch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'w_ur8_to_epoch', double(ur8), double(epoch))
return, ok
end

function w_ur8_from_epoch, ur8, epoch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'w_ur8_from_epoch', double(ur8), double(epoch))
return, ok
end
;
; Old wind_tm routines
;

function wind_tm_open_channel, ch, context
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_open_channel', long(ch), context)
return, ok
end

function wind_tm_close_channel, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_close_channel', long(ch))
return, ok
end

function wind_tm_get_filename, ch, filename
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(filename) eq 0 then filename = big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_get_filename', long(ch), filename)
filename = strtrim(filename,2)
return, ok
end

function wind_tm_set_messages_off, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_set_messages_off', long(ch))
return, ok
end

function wind_tm_set_messages_on, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_set_messages_on', long(ch))
return, ok
end

function wind_tm_version, ch, ver
ok = 0L
big = '                                                  '
big = big+big+big
if strlen(ver) eq 0 then ver = big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_version', long(ch), ver)
ver = strtrim(ver,2)
return, ok
end

function wind_tm_eof, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_eof', $
     long(ch), long(major), long(minor))
return, ok
end

function wind_tm_get_event, ch, mjr, mnr, event
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_get_event', long(ch), long(mjr), long(mnr), event)
return, ok
end

function wind_tm_get_next_event, ch, mjr, mnr, event
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_get_next_event', long(ch), long(mjr), long(mnr), event)
return, ok
end

function wind_tm_get_previous_event, ch, mjr, mnr, event
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'wind_tm_get_previous_event', long(ch), long(mjr), long(mnr), event)
return, ok
end

function wind_tm_get_item, ch, item, buf, buf_sz, ret_sz
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
     'wind_tm_get_item', long(ch), item, long(buf), long(buf_sz), $
      long(ret_sz))
return, ok
end

function wind_tm_xlate_item, ch, event, item, item_val, item_str
ok = 0L
big = '                                                  '
if strlen(item_str) eq 0 then item_str = big+big+big
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_xlate_item', $
     long(ch), event, item, long(item_val), item_str)
item_str = strtrim(item_str,2)
return, ok
end

function wind_tm_set_wait, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_set_wait', long(ch))
return, ok
end

function wind_tm_set_nowait, ch
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_set_nowait', long(ch))
return, ok
end

function wind_tm_get_mfmf, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_get_mfmf', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_get_stream_mfmf, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_get_stream_mfmf', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_get_next_mfmf, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_get_next_mfmf', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_get_earliest_mfmf, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_get_earliest_mfmf', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_get_latest_mfmf, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_get_latest_mfmf', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_increment_packet, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_increment_packet', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_decrement_packet, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_decrement_packet', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_increment_mfmf, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_increment_mfmf', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_decrement_mfmf, ch, major, minor
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
    'wind_tm_decrement_mfmf', long(ch), long(major), long(minor))
return, ok
end

function wind_tm_bit_rate, ch, major, minor, bit_rate
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_bit_rate', long(ch), long(major), long(minor), $
     double(bit_rate))
return, ok
end

function wind_tm_delta_mfmf, ch, major1, minor1, major2, minor2, diff
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_delta_mfmf', long(ch), long(major1), long(minor1), $
    long(major2), long(minor2), long(diff))
return, ok
end

function wind_tm_get_word, ch, major, minor, word
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_get_word', long(ch), long(major), long(minor), long(word))
return, ok
end

function wind_tm_get_minor_frame, ch, major, minor, buf
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_get_minor_frame', long(ch), long(major), long(minor), $
    long(buf))
return, ok
end

function wind_tm_get_major_frame, ch, major, minor, buf
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_get_major_frame', long(ch), long(major), long(minor), $
    long(buf))
return, ok
end

function wind_tm_get_packet, ch, major, minor, buf
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_get_packet', long(ch), long(major), long(minor), $
    long(buf))
return, ok
end

function wind_tm_get_hk, ch, major, hkindex, buf
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_get_hk', long(ch), long(major), long(hkindex), $
    long(buf))
return, ok
end

function wind_tm_get_test, ch, major, minor, buf
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_get_test', long(ch), long(major), long(minor), $
    long(buf))
return, ok
end

function wind_tm_get_step, ch, major, minor, buf
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_get_step', long(ch), long(major), long(minor), $
    long(buf))
return, ok
end

function wind_tm_scet_to_mfmf, ch, major, minor, ur8scet
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_scet_to_mfmf', long(ch), long(major), long(minor), $
    double(ur8scet))
return, ok
end

function wind_tm_mfmf_to_scet, ch, major, minor, ur8scet
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_mfmf_to_scet', long(ch), long(major), long(minor), $
    double(ur8scet))
return, ok
end

function wind_tm_ert_to_mfmf, ch, major, minor, ur8ert
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_ert_to_mfmf', long(ch), long(major), long(minor), $
    double(ur8ert))
return, ok
end

function wind_tm_mfmf_to_ert, ch, major, minor, ur8ert
ok = 0L
ok = call_external(STRING(getenv('WIWAV_IDL_LIB')), $
   'wind_tm_mfmf_to_ert', long(ch), long(major), long(minor), $
    double(ur8ert))
return, ok
end

