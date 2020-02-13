ssh -X wilson@waves1.space.umn.edu

wind_login
idl
@wind

stream = 'offline'
tdsch = 0

ok = w_channel_open(tdsch,stream)
print, tdsch
; -tdsch = 1

v

scettds = 0.
ok = w_channel_position(tdsch,scettds)    
print, tdsch
print, scettds
; -tdsch = 1
; -scettds = 6670.00

dds = scettds
hh = 15
mm = 30
scettds = float(dds) + hh/24. + mm/1440.
print, scettds
; - scettds = 6670.65

ok = w_channel_position(tdsch,scettds)
print, tdsch
print, scettds
; - tdsch   = 1
; - scettds = 6670.65

fillch = 0
ok = w_channel_open(fillch,stream)           
print,fillch
; - fillch = 2

scetfill = 0.
ok = w_channel_position(fillch,scetfill)
print,scetfill
print,fillch
; - fillch = 2
; - scetfill = 6670.00

dds = scetfill
scetfill = float(dds) + hh/24. + mm/1440.
print,scetfill
; -scetfill = 6670.65

ok = w_channel_position(fillch,scetfill)
print,fillch
print,scetfill
; - fillch = 2
; -scetfill = 6670.65


yyyy  = 0
mon   = 0
dd    = 0
ss    = 0
ms    = 0
doy   = 0
msday = 0

ok = w_ur8_to_ymd(scetfill,yyyy,mon,dd,hh,mm,ss,ms)
print,scetfill
print,yyyy, mon, dd
print, hh, mm, ss, ms
; - scetfill = 6670.65
; - yyyy = 2000, mon = 4, dd = 6
; - hh = 15, mm = 30, ss = 56, ms = 384


ok = w_ur8_to_ydoy(scetfill,yyyy,doy,msday)
print,scetfill
print,yyyy, doy, msday
; - scetfill = 6670.65
; - yyyy = 2000, doy = 97, msday = 19712


ok = w_event(tdsch,'TDSF')
print, tdsch
; - tdsch = 1

s_scet = INTARR(2)
return_size = 0
item = 'EVENT_SCET'
ok = w_item_i4(tdsch, item, s_scet, 2, return_size)
print, tdsch, item
print, s_scet, return_size
; - s_scet = 11926     305


; -I added this, but it is NOT in tdspro.for before this point
scett = 0d0                           
item = 'EVENT_SCET_R8'
ok = w_item_r8(tdsch,item,scett,1,return_size)
print, tdsch, ' '+item
print, scett, return_size
; - scett = 6670.6424


ok = w_event(fillch,'FILL')
print, fillch 
; - fillch = 2

scetfill = 0d0
item = 'EVENT_SCET_R8'
ok = w_item_r8(fillch, item, scetfill, 1, return_size)
print,fillch, ' '+item
print,scetfill, return_size
; -scetfill = 6670.6408

file = ''
ok = w_channel_filename(tdsch,file)
print, file
; -E.G. wind_data:wi_lz_wav_20000406_v01.dat
print, tdsch

; -Depends on 
event = 'FILL'
event = 'TDSF'
ch = fillch
ch = tdsch
ok = w_event(ch,event)
print,ch,' '+event

item = 'EVENT_SCET_R8'
ok = w_item_R8(ch, item, scett, 1, return_size)
print,ch, ' '+item
print,scett, return_size
; - scett = 6670.6424  => didn't change scett


;if (ch eq tdsch) then begin
;  scettds = scett
;endif else begin
;  scetfill = scett
;endif


item = 'EVENT_SCET'
ok = w_item_i4(ch, item, s_scet, 2, return_size)
print, ch, ' '+item
print, s_scet, return_size
; - s_scet = 11926     305  => didn't change s_scet

; C ss = mod(s_scet(2),100)
ss = s_scet[1] mod 100
print, ss

mm = s_scet[1]/100
print, mm
; C mm = s_scet(2)/100
mm = mm mod 100
print, mm
; C mm = mod(mm,100)
; C hh = s_scet(2)/10000
hh = s_scet[1]/10000
print, hh
scett2 = float(dds) + hh/24. + mm/1440. + ss/86400.
print, scett2
; - scett2 = 6670.00 => NOT the same event time as scett

itemp = 0L
item  = 'EVENT_NUMBER'
ok = w_item_i4(ch, item, itemp, 1, return_size)
print, ch, ' '+item
print, itemp, return_size
; - itemp = 27117931

item = 'DPU_CLOCK'
DPUCLK = 0d0
ok = w_item_R4(ch, item, DPUCLK, 1, return_size)
print, ch, ' '+item
print, DPUCLK, return_size
; - DPUCLK = 10962228.

item = 'SUN_ANGLE'
sunclock = 0d0
ok = w_item_R4(ch, item, sunclock, 1, return_size)
print, ch, ' '+item
print, sunclock, return_size
; - sunclock = 3390.0000


item = 'SUN_ANGLE_R4'
sunANG = 0d0
ok = w_item_R4(ch, item, sunANG, 1, return_size)
print, ch, ' '+item
print, sunANG, return_size
; - sunANG = 297.94922


item = 'source'
isource = 0L
ok = w_item_i4(ch, item,isource, 1, return_size)
print, ch, ' '+item
print, isource, return_size
; - isource = 2

item = 'event_boe_r8'
beginevt = 0d0
ok = w_item_R8(ch, item, beginevt, 1, return_size)
print, ch, ' '+item
print,beginevt, return_size
; - beginevt = 6670.6762

item = 'event_eoe_r8'
endevt = 0d0
ok = w_item_R8(ch, item, endevt, 1, return_size)
print, ch, ' '+item
print,endevt, return_size
; - endevt = 6670.7016

yyyy2  = 0
mon2   = 0
dd2    = 0
hh2    = 0
mm2    = 0
ss2    = 0
ms2    = 0
scett3 = scett
doy2   = 0L
msday2 = 0L

ok = w_ur8_to_ymd(scett3,yyyy2,mon2,dd2,hh2,mm2,ss2,ms2)
print, scett3
print, yyyy2, mon2, dd2
print, hh2, mm2, ss2, ms2
; - scett3 = 6670.6424
; - yyyy2 = 2000, mon2 = 4, dd2 = 6
; - hh2 = 15, mm2 = 25, ss2 = 3, ms2 = 472


ok = w_ur8_to_ydoy(scett3,yyyy2,doy2,msday2)
print,scett3
print,yyyy2, doy2, msday2
; - scett3 = 6670.6424
; - yyyy2 = 2000, doy2 = 97, msday2 = 55503472



;IDL> print, scett, format='(f20.10)'
;     6670.6424012963
;IDL> print, scett3, format='(f20.10)'
;     6670.6424012963

item = 'EVENT_START_SCET_R8'
truest = 0d0
ok = w_item_R8(ch, item, truest, 1, return_size)


item = 'EVENT_END_SCET_R8'
trueen = 0d0
ok = w_item_R8(ch, item, trueen, 1, return_size)


yy=0
mn=0
dy=0
hr=0
mi=0
se=0
ml=0
ok = w_ur8_to_ymd(6670.00,yy,mn,dy,hr,mi,se,ml)
print, yy, mn, dy
; 2000       4       6
print, hr, mi, se, ml
; 0       0       0       0
item = 'EVENT_START_SCET_R8'
truest = 0d0
ok = w_item_R8(ch, item, truest, 1, return_size)
print, truest,format='(f15.9)'
; 6670.642400884
item = 'EVENT_END_SCET_R8'
trueen = 0d0
ok = w_item_R8(ch, item, trueen, 1, return_size)
; print, trueen, format='(f15.9)'
 6670.642401082
print, trueen - scett
; -2.1450614e-07
help, scett
; SCETT           DOUBLE    =        6670.6424
help, trueen
;TRUEEN          DOUBLE    =        6670.6424
print, truest - scett
;  -4.1203702e-07
