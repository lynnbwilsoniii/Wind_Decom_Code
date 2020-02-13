;pro read_lzscan_for_mode

close,1
openr,1,getenv('WGGSBASE')+'utilities/lzscan_for_mode.dat'
s=''
readf,1,s

i=-1
while eof(1) ne 1 do begin
s=''
readf,1,s
i=i+1
date=strmid(s,0,8)
print,i,' ',date
if date eq '19970107' then goto,point1
endwhile
close,1

point1:
close,1
openr,1,getenv('WGGSBASE')+'utilities/lzscan_for_mode.dat'
s=''
readf,1,s

date=strarr(764)
mode=intarr(764)
for i=0,763 do begin
  s=''
  readf,1,s
  ;print,s
  s=strcompress(s,/remove_all)
  print,s
  date(i)=strmid(s,0,8)
  mode(i)=strmid(s,25,1)
  print,i,'                                 ',date(i),mode(i)
  ;stop
endfor

date=[date,$
  '19970108','19970109','19970110','19970111','19970112','19970113','19970114']
mode=[mode,2,2,2,2,2,2,2]

w=where(mode eq 2)
date_mode2=date(w)
mode2=mode(w)
print,' ' & print,' '
for i=0,n_elements(mode2)-1 do print,i,'  ',date_mode2(i),'  ',mode2(i)


print,' ' & print,' '
for i=0,n_elements(mode2)-1 do print,date2(i),mode2(i)

save,filename=getenv('SWEDATLIB')+'mode2_dates.txt',date_mode2

end