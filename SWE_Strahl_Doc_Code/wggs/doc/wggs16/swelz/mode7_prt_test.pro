;pro mode7_prt_test

;mode7 strahl map : offsets directly into lz array
n_spectra=7
n_sdat=720
sdatc7=replicate({strlbl7,ind:intarr(n_sdat),spect_spncnt_offs:0l},n_spectra)

lpr=1
dir=getenv('SWEDATLIB')
close,1
lun=1
openr,lun,dir+'swemode7.prt';,/get_lun
null=''
readf,lun,null
for ispect=0,n_spectra-1 do begin
  spect=0 & spect_spncnt_offs=0  
  readf,lun,spect,spect_spncnt_offs
  if lpr then print,spect,spect_spncnt_offs
  sdatc7(spect).spect_spncnt_offs=spect_spncnt_offs
  for ivstep=0,14 do begin
    vstep=0 & vstepvar='step'
    readf,lun,vstep,vstepvar
    if lpr then print,vstep,vstepvar
    for isect=0,7 do begin
      sect=0 & offs=lonarr(18)
      readf,lun,sect,offs
      if lpr then print,sect,offs,format='(i2,6(i6,i4,i3))'
      sdatc7(spect).ind(vstep*48+sect*6+indgen(6))=offs(3*indgen(6))
    endfor
    ;if lpr then stop
  endfor
  if lpr then print,sdatc7(spect).ind
  if lpr then print,sdatc7(spect).spect_spncnt_offs
  if lpr then stop
endfor
close,1

end