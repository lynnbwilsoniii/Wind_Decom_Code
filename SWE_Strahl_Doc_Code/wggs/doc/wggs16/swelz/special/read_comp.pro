
; given the logcompression table and given a 12-bit value(short integer), 
;  the  8-bit log compressed value, val(byte),is returned

openr,lun,getenv('WGGSBASE')+'swe/swedatlib/compress.tbl',/get_lun
comp_tbl=intarr(4096)

line='  '
for i=0,3 do readf,lun,line

readf,lun,comp_tbl,format='(6x,16z3)'

free_lun,lun

col=indgen(16) & row=indgen(256)
dcompval=intarr(4096)
for i=0,255 do dcompval(i*16 : (i+1)*16-1)=i*16+indgen(16)

print,col,format='(6x,16z3)'
print,' '
for i=0,255 do print,row(i),comp_tbl(i*16 : (i+1)*16-1),format='(z4,2x,16z3)'
;print,dcomp_tbl,format='(16i5)'


end


