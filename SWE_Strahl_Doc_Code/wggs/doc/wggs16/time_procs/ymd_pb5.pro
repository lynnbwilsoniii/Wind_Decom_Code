function ymd_pb5,ymd

;returns  pb5 time (year, day of year, msec of day)  as long array for 
;  given year, month, day as long integer

y=long(fix(ymd/10000))
m=long(fix(ymd-10000*y)/100)
d=long(ymd-10000*y-100*m)

;if y ne 1996 then begin
if y ne 4*fix(y/4) then begin
  ;non leap year : j   f   m   a   m    j    j    a    s    o    n    d
  caldayfrst=     [1,  32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 ]
endif
 
;if y eq 1996 then begin
if y eq 4*fix(y/4) then begin
  ;leap year :     j   f   m   a   m    j    j    a    s    o    n    d
  caldayfrst=     [1,  32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336 ]
endif

calday=long(caldayfrst(m-1)+d-1)
msec=0l

return,  [y,calday,msec]

end
