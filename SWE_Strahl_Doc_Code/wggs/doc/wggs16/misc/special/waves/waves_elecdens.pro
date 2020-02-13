file='waves_elecdens.dat'
openr,lun,file,/get_lun
s=''
readf,lun,s
while not eof(lun) do begin
  readf,lun,s
  date=''
  readf,lun,date
  print,' ' & print,'date ',date
  n=0
  readf,lun,'n ',n
  ;print,'n ',n
  readf,lun,s
  sec=fltarr(n) & chan=intarr(n) & densw=fltarr(n)
  se=0.d & ch=0 & de=0.
  for i=0,n-1 do begin
    readf,lun,se,ch,de
    sec(i)=se & chan(i)=ch & densw(i)=de
    ;print,sec(i),chan(i),densw(i)
  endfor 
  hour_hms,sec(0)/3600.d,hms0
  hour_hms,sec(n-1)/3600.d,hms1
  print,sec(0)/3600.d,'  ',hms0
  print,sec(n-1)/3600.d,'  ',hms1
;stop
endwhile

end
