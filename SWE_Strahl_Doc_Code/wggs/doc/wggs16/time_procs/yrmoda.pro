function yrmoda,tpb5

;returns string of year, month, day for 
;  given pb5 time (year, day of year, msec of day)

;if tpb5(0) ne 1996 then begin
if tpb5(0) ne 4*fix(tpb5(0)/4) then begin
;non leap year : j   f   m   a   m    j    j    a    s    o    n    d
caldayfrst=     [1,  32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 ]
endif

;if tpb5(0) eq 1996 then begin
if tpb5(0) eq 4*fix(tpb5(0)/4) then begin
;    leap year : j   f   m   a   m    j    j    a    s    o    n    d
caldayfrst=     [1,  32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336 ]
endif

w=where(tpb5(1)/caldayfrst ne 0,nw)
mo=w(nw-1)+1
da=tpb5(1)-caldayfrst(w(nw-1)) + 1
yr=tpb5(0)

if mo lt 10 then $
  smo='0'+string(mo,format='(i1)') else $
  smo=string(mo,format='(i2)')

if da lt 10 then $
  sda='0'+string(da,format='(i1)') else $
  sda=string(da,format='(i2)')

return,  smo+'-'+sda+'-'+string(yr,format='(i4)')

end
