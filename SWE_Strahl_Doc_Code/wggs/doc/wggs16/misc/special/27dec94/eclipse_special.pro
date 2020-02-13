;================= eclipse_special =========================================

pro eclipse_special

common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common special1,lunar

;used by input.pro

restore,getenv('WGGSBASE')+'swe/27dec94/data/lunar_27dec.idlsav'
help,lunar & help,lunar,/str

;read mag field
  openr,lunb,getenv('WGGSBASE')+'swe/27dec94/data/moon_1min_gse.dat',/get_lun
  s=''
  for i=0,7 do readf,lunb,s
  timehrs=0.d & bx=0. & by=0. & bz=0. & bm=0.
  bhrs=dblarr(1000) & bsec=dblarr(1000) & bxnew=fltarr(1000) 
  bynew=fltarr(1000) & bznew=fltarr(1000) & bmnew=fltarr(1000)
  phnew=fltarr(1000) & thnew=fltarr(1000)
  pb5=lonarr(3)
  pb5(0)=1994l
  pb5(1)=361l
  i=-1
  while not eof(lunb) do begin
    readf,lunb,timehrs,bx,by,bz,bm
    i=i+1
    bhrs(i)=timehrs
    pb5(2)=timehrs*3600000
    tjd=pb5_tjd(pb5)
    td=tjd(0)*86400.d & sc=tjd(1)/1000.d
    bsec(i)=td+sc 
    bxnew(i)=bx
    bynew(i)=by
    bznew(i)=bz
    bmnew(i)=bm
    th=asin(bz/bm)/!dtor
    ph=atan(by,bx)/!dtor
    if bx lt 0 then ph=ph+360.
    thnew(i)=th
    phnew(i)=ph
  endwhile
  bhrs=bhrs(0:i)
  bsec=bsec(0:i)
  bxnew=bxnew(0:i)
  bynew=bynew(0:i)
  bznew=bznew(0:i)
  bmnew=bmnew(0:i)
  phnew=phnew(0:i)
  thnew=thnew(0:i)
  nb=i+1
  window,0,XSIZE = 650,YSIZE = 700
  ;!p.multi=[0,0,3,0,0]
  plot,bhrs,bmnew,charsize=2.0,yrange=[5,10],ystyle=1,yticks=5,yminor=1
  ;plot,bhrs,thnew,charsize=2.0,yrange=[-90,90],ystyle=1,yticks=2,yminor=4
  ;plot,bhrs,phnew,charsize=2.0,yrange=[0,360],ystyle=1,yticks=2,yminor=4
  !p.multi=0 
;=========== lunar eclipse interval ===========================================
  tpb5_lun27dec=lonarr(3,2)
  tpb5_lun27dec(*,0) = [1994,        361,    43200000]   ;52862580 ]
  tpb5_lun27dec(*,1) = [1994,        361,    64800000]   ;56213067 ]

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
  mdat(wecl).bmag=interpol(bmnew,bsec,mdat(wecl).ta)
  mdat(wecl).b(0)=interpol(bxnew,bsec,mdat(wecl).ta)
  mdat(wecl).b(1)=interpol(bynew,bsec,mdat(wecl).ta)
  mdat(wecl).b(2)=interpol(bznew,bsec,mdat(wecl).ta)
  mdat(wecl).phib=interpol(phnew,bsec,mdat(wecl).ta)
  mdat(wecl).theb=interpol(thnew,bsec,mdat(wecl).ta)
 
  
endif
end

