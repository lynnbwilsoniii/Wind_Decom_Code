pro prt_flhdr,lun,infile,fh

if lun eq 0 then begin
print,' '
print,'infile ',infile
print,'file header'
;help,fh,/str
print,'first'
help,fh.fst,/str
print,'last'
help,fh.lst,/str
print,'# phys rec ',fh.nphyrc

ms_hms,fh.fst.ms,h,m,s
print,fh.fst & print,h,m,s & print,' '

ms_hms,fh.lst.ms,h,m,s
print,fh.lst & print,h,m,s & print,' '

endif else begin

endelse
end
