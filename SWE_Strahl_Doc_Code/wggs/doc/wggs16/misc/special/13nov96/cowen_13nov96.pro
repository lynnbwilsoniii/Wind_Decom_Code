;================= eclipse_special =========================================

pro cowen_13nov96,msec,errangle,checkplot=checkplot

restore,getenv('WGGSBASE')+'swe/13nov96/data/19961113_ecl_ang.idl'
help,t_sec,error_ang


wbad=[245,246,247]

wrollover=where(t_sec gt 9345.2633)
errangle=error_ang
errangle(wrollover)=errangle(wrollover)+2*!pi
wok=where(t_sec ne t_sec(wbad))
wok=indgen(n_elements(t_sec))
wok(wbad)=-1
wok=wok(where(wok ge 0))
msec=long(t_sec(wok)*1000)
errangle=errangle(wok)

if keyword_set(checkplot) ne 0 then begin
  window,0
  !p.multi=[0,0,2,0,0]
  plot,t_sec,error_ang,psym=3,title='13nov96 Chris Owen original',$
    xtitle='seconds of day',ytitle='angle err (radians)'
  loadct,18
  for i=0,n_elements(t_sec)-1 do print,i,t_sec(i),error_ang(i)

  oplot,t_sec(wbad),error_ang(wbad),psym=1,color=225
  print,' ' & print,'wbad:'
  for i=0,n_elements(wbad)-1 do print,wbad(i),t_sec(wbad(i)),error_ang(wbad(i))

  print,' ' & print,'wbad ',wbad & print,'wok ',wok
  plot,msec,errangle/!dtor,psym=4,symsize=0.3,title='13nov96 angle correction',$
    xtitle='milliseconds of day',ytitle='angle err (degreess)',$
    subtitle='pb5 time [1996, 318, milliseconds]'
  
  for i=0,n_elements(msec)-1 do print,i,msec(i),errangle(i),errangle(i)/!dtor
endif

end

