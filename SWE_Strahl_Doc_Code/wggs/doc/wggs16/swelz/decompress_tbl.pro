pro decompress_tbl

;reads decompression and compression tables

common log_delog,comp_tbl,dcomp_tbl


;read compress/decompress tables
  dir=getenv('SWEDATLIB')
  openr,lun,dir+'compress.tbl',/get_lun
  comp_tbl=bytarr(4096)
  line='  '
  for i=0,3 do readf,lun,line
  readf,lun,comp_tbl,format='(6x,16z3)'
  free_lun,lun

  openr,lun,dir+'decompress.tbl',/get_lun
  dcomp_tbl=intarr(256)
  line='  '
  for i=0,3 do readf,lun,line
  readf,lun,dcomp_tbl,format='(5x,16z4)'
  free_lun,lun

end
