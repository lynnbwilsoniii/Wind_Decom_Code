;pro readstrahlmap.pro

;reads strahlmap.prt to get array of strahl offsets into lz record

;strahlmap.prt is the output of Jerry's makestrahlmap.c 

lpr=1

openr,lun,'strahlmap.prt',/get_lun

n_spins=7
n_sdat=336
n_sectors=6
n_strdets=12
n_strphis=28
sdatc=replicate({strlbl,ind:intarr(n_sdat)},n_spins)
k=-1
for ispin=0,n_spins-1 do begin
  spnbl=0 & strvar='block'  
  readf,lun,spnbl,strvar
  if lpr then print,spnbl,strvar
  for istep=0,27 do begin
    step=0 & offs=lonarr(36)
    readf,lun,step,offs
      if lpr then print,step,offs,format='(i2,12(i6,i4,i3))'
      ;vdatc(spnbl).ind(sector*96+istep*6+indgen(6))=offs(3*indgen(6))
      sdatc(spnbl).ind(istep*12+indgen(12))=offs(3*indgen(12))
  endfor
endfor
if lpr then print,'sdatc.ind'
if lpr then print,sdatc,format='(12i6)'
free_lun,lun

end 

