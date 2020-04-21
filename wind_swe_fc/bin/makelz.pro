PRO makelz,infile,outfile,lpr=lpr
if not keyword_set(lpr) then lpr = 0

count=0
print,'infile:  ',infile
openr,inlun,infile,/get_lun
print,'outfile: ',outfile
openw,outlun,outfile,/get_lun

record=bytarr(11552)
readu,inlun,record			;read file header
writeu,outlun,record

recqy=252l 
recl=11552
recmf = recl - 48l - recqy
lz = {lzrcdr, intsn:0l, recn:0l, mfc:0l, spcclk:bytarr(8), yr:0l, dy:0l, $
      ms:0l, us:0l, nfill:0l, nsync:0l, telmod:0l, qlty:bytarr(recqy), $
      mf:bytarr(recmf)} 

lz2 = {lzrcdr}

while count lt 109 do begin
	count=count+1
	if lpr then print,count
	readu,inlun,lz
;	if count eq 57 then lz2=lz
;	if (count eq 59) or (count eq 60) then lz.mf=lz2.mf
	writeu,outlun,lz 
endwhile

print,'Number of records: ',long(count) 

free_lun,inlun
free_lun,outlun
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

