; trad2ai.pro
ok = 0L
ch = 0L
size = 512L
s = lonarr(512)
z = lonarr(512)
sp = lonarr(512)
f = lonarr(512)
smv = fltarr(512)
s_scet = dblarr(512)
event = 'RAD2'
rs1 = 0L
rs2 = 0L
rs3 = 0L
rs4 = 0L
rs5 = 0L
rs6 = 0L
i = 0L
j = 0L
k = 0L
event_number = 0L
ver = 'abcdefghigjlmnopqrstuvwxyz'
file ='abcdefghigjlmnopqrstuvwxyz_abcdefghigjlmnopqrstuvwxyz_abc'
keep_going = 1L
c1 = ' '
c_s = 'S'
c_z = 'Z'
c_sp= 'S_PRIME'
c_f = 'FREQUENCIES'
c_smv = 'S_MICROVOLTS_R4'
c_s_scet = 'S_SCET_R8'

ok = w_channel_open(ch,'offline')
if (ok ne 1) then begin
   print, 'cannot open channel'
   stop
endif

ok = w_version(ver)
ok = w_ch_filename(ch,file)
print, ' '
print, 'Using Wind_Lib version ', ver
print, 'File: ', file

while (keep_going) do begin
   event_number = event_number + 1
   print, ' '
   print, event, ' event number ', event_number
   ok = w_event(ch,event)
   if (ok eq 1) then begin
      ok = w_item_i4(ch,c_s,s,size,rs1)
      if (ok ne 1) then print, 'Cannot get item ', c_s
      ok = w_item_i4(ch,c_z,z,size,rs2)
      if (ok ne 1)then  print, 'Cannot get item ', c_z
      ok = w_item_i4(ch,c_sp,sp,size,rs3)
      if (ok ne 1) then print, 'Cannot get item ', c_sp
      ok = w_item_i4(ch,c_f,f,size,rs4)
      if (ok ne 1) then print, 'Cannot get item ', c_f
;      ok = w_item_r4(ch,c_smv,smv,size,rs5)
      if (ok ne 1) then print, 'Cannot get item ', c_smv
      ok = w_item_r8(ch,c_s_scet,s_scet,size,rs6)
      if (ok ne 1) then print, 'Cannot get item ', c_s_scet

      print,format='(1x,4x,     4(1x,a3), 1x,a8,   1x,a12)',$
         c_s, c_z, c_sp, c_f, c_smv, c_s_scet
      print,format='(1x,4x,     4(1x,i3), 1x,i8,   1x,i12)',$
         rs1, rs2, rs3, rs4, rs5, rs6
      print,format='(1x,4x,     4(1x,a3), 1x,a8,   1x,a12)',$
         '---', '---', '---', '---', '--------', '--------------'
      for j=0,16,1 do begin
         print,format='(1x,i3,".", 4(1x,i3), 1x,f8.4, 1x,f12.7)',$
           j+1, s(j), z(j), sp(j), f(j), smv(j), s_scet(j)
      endfor
   endif

   print, ' '
   print,format='(1x,a,$)', 'Get another event [Y/N]? '
   read, c1
   keep_going = not (c1 eq 'N' or c1 eq 'n')
endwhile

end

