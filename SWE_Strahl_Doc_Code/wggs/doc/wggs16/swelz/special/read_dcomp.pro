
; given the de-logcompression table and given an 8-bit log compressed value, 
; compval(byte), the 12-bit value(short integer) is returned

openr,lun,getenv('WGGSBASE')+'swe/swedatlib/decompress.tbl',/get_lun
dcomp_tbl=intarr(256)

line='  '
for i=0,3 do readf,lun,line

readf,lun,dcomp_tbl,format='(5x,16z4)'

free_lun,lun



col=indgen(16) & row=indgen(16)
compval=intarr(256)
for i=0,15 do compval(i*16 : (i+1)*16-1)=i*16+indgen(16)

print,col,format='(6x,16z4)'
print,' '
for i=0,15 do print,row(i),dcomp_tbl(i*16 : (i+1)*16-1),format='(z3,3x,16z4)'
print,dcomp_tbl,format='(16i5)'


end


