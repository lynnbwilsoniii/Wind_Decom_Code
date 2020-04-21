pro prt_flhdr,lun,infile,fh

if lun eq 0 then begin
openw,lun2,'fc_data.prt',/get_lun
print,' '
printf,lun2,' '
print,'infile ',infile
printf,lun2,'infile ',infile
print,'file header'
printf,lun2,'file header'
help,fh,/str
print,'first'
printf,lun2,'first'
help,fh.fst,/str
printf,lun2,'last'
help,fh.lst,/str
print,'# phys rec ',fh.nphyrc
printf,lun2,'# phys rec ',fh.nphyrc
	free_lun,lun2
endif else begin

endelse
end
