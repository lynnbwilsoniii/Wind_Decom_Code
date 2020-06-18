; pkt.idl - idl version of packet.for
;

tm_lib   = '/home/wind/lib/libWAVES.so'	; contains wind_tm_* routines
open_ch  = '_wind_tm_open_channel'
get_mfmf = '_wind_tm_get_mfmf'
get_pkt  = '_wind_tm_get_packet'
inc_pkt  = '_wind_tm_increment_packet'
is_eof   = '_wind_tm_eof'

pkt = lonarr(512)
ok  = 8l
ch  = 0l
mjr = 0l
mnr = 0l

; open a channel to the telemetry
print, '...attempting to open channel...'
ok = call_external(tm_lib, open_ch, ch,'offline')
if ok ne 1 then begin
   print, 'cannot open channel, ok=', ok
   stop
endif
print, 'TM stream opened on channel ', ch

; position to the beginning of the stream with frame numbers
ok = call_external(tm_lib, get_mfmf, ch,mjr,mnr)
if ok ne 1 then begin
   print, 'cannot position to beginning of stream'
   stop
endif

; advance to next packet
ok = call_external(tm_lib, inc_pkt, mjr,mnr)

; loop until end-of-file
eof = 0
while (eof eq 0) do begin
   ok = call_external(tm_lib, get_pkt, ch,mjr,mnr,pkt)
   if ok ne 1 then begin
      print, 'cannot get packet at MF.mf ', mjr, '.',mnr,format="(a,i6,a,i3.3)"
   endif

   print, 'MF.mf: ', mjr,mnr, 'Pkt:', pkt(0:11),'...',pkt(429),pkt(430),$
   format="(a,i6,'.',i3.3,a,12z3.2,a,2z3.2)"

   ok = call_external(tm_lib, inc_pkt, mjr,mnr)
   eof = call_external(tm_lib, is_eof, ch,mjr,mnr)
endwhile

stop
end
