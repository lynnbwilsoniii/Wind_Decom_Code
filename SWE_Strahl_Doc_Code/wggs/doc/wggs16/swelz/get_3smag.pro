pro get_3smag,timpb5,magtpb5,b,phi,theta

common magstuff,magfile,tpb5,bgse


;get mfi 3sec mag field
    ; timpb5=given pb5 time (year, day of year, millisec of day)  lonarr(3)
    ; tpb5 = pb5 time at center of minute for given record recn   lonarr(1440,3)
    ; bgse = mag vector at 3 sec intervals in minute tpb5     fltarr(1440,3,20)
    mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
    sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
    b=reform(bgse(mindx,0:2,sindx))
    magtpb5=[tpb5(mindx,0),tpb5(mindx,1),$
      tpb5(mindx,2) - (30000l - 1500l - long(sindx)*3000)]
    print,' '
    print,'given time ',timpb5
    print,'  mag time ',magtpb5
    print,'mag fld (gse) ',b

;get phi and theta for mag fld
  phi=atan(b(1),b(0))/!dtor
  if phi lt 0 then phi=phi + 360.
  theta=90.-acos(b(2)/sqrt(total(b*b)))/!dtor

end