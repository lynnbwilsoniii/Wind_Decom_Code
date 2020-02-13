;================= eclipse_special =========================================

pro eclipse_special0

common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common special1,lunar
;used by input.pro

restore,'lunar_27dec.idlsav'
help,lunar & help,lunar,/str

;=========== lunar eclipse interval ===========================================
  tpb5_lun27dec=lonarr(3,2)
  tpb5_lun27dec(*,0) = [1994,        361,    52862580 ]
  tpb5_lun27dec(*,1) = [1994,        361,    56213067 ]

;convert pb5 time to trunc jl day and msec of day
  tjd1=pb5_tjd(tpb5_lun27dec(*,0))    
  tjd2=pb5_tjd(tpb5_lun27dec(*,1))

;convert jul day and msec of day to seconds from tjd epoch
  sec1=sec_tjdepoch(tjd1(0),tjd1(1))
  sec2=sec_tjdepoch(tjd2(0),tjd2(1))

wecl=where(mdat.ta ge sec1 and mdat.ta le sec2)
if wecl(0) ne -1 then begin
  print,'eclipse_special: sec1, sec2, wecl(0),mdat(wecl(0)).ta, ',$
    'n_elements(wecl)-1,mdat(wecl(n_elements(wecl)-1)).ta'
  print,sec1, sec2, wecl(0),mdat(wecl(0)).ta,$
    n_elements(wecl)-1,mdat(wecl(n_elements(wecl)-1)).ta
  for i=0,n_elements(lunar)-1 do begin
    w=where(lunar(i).recn eq mdat(wecl).mfrec and $
            lunar(i).spinbl eq mdat(wecl).mfspinbl)
    if w(0) ne -1 then begin
      k=wecl(w(0))
      cs=cos(-lunar(i).dphi)
      sn=sin(-lunar(i).dphi)
      bxnew=mdat(k).b(0) * cs - mdat(k).b(1) * sn
      bynew=mdat(k).b(0) * sn + mdat(k).b(1) * sn
      ;print,'eclipse_special: mag_orig, mag_rot ',mdat(k).b(0:1),bxnew,bynew
      mdat(k).b(0) = bxnew
      mdat(k).b(1) = bynew
      mdat(k).phib=atan(mdat(k).b(1),mdat(k).b(0))/!dtor
      if mdat(k).b(1) lt 0 then mdat(k).phib=mdat(k).phib+360.
    endif
  endfor
endif

end

