;pro getrelgains_tblgen_19941130_19971027

;This procedure uses getrelgains.pro to create a table of daily relative gains.
;Both an ascii file (getenv('WGGSBASE')+'swelz/swecal/gains/getrelgains_tbl.ascii')
;and an idlsave file 
; (getenv('WGGSBASE')+'swelz/swecal/gains/getrelgains_tbl.dat'
;of the gains table is made


begindate=19941130l  
pb5begin=ymd_pb5(begindate)
elapsecbegin=pb5_elapsec(pb5begin,pb5begin)

enddate=19971027l
pb5end=ymd_pb5(enddate)
elapsecend=pb5_elapsec(pb5end,pb5begin)

ndays=long((elapsecend-elapsecbegin)/double(86400.) + 1)

datetbl=lonarr(ndays)
rgtbl=fltarr(6,ndays)
ascii_file='swelz/swecal/gains/gainsrev/getrelgains_tbl_'+$
  string(begindate,format='(i8)')+'_'+string(enddate,format='(i8)')+'.ascii'
openw,1,getenv('WGGSBASE')+ascii_file
for i=0,ndays-1 do begin
  elapsec=elapsecbegin+i*86400l
  pb5tm=elapsec_pb5(elapsec,pb5begin) 
  sectm=pb5_sec(pb5tm)          
  getrelgains,rg,sectm
  rgtbl(*,i)=rg 
  datetbl(i)=pb5_ymd(pb5tm)
  printf,1,datetbl(i),rgtbl(*,i),format='(i8,5x,6f7.3)'
endfor
close,1
sav_file='swelz/swecal/gains/gainsrev/getrelgains_tbl_'+$
  string(begindate,format='(i8)')+'_'+string(enddate,format='(i8)')+'.dat'
save,file=getenv('WGGSBASE')+sav_file,datetbl,rgtbl
  
print,'ascii file created ',$
  getenv('WGGSBASE')+ascii_file
print,'idlsav file created ',$
  getenv('WGGSBASE')+sav_file    
end