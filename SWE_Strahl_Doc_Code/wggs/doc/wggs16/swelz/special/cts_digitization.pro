;pro cts_digitization

;reads decompression and compression tables and computes digitization level

common log_delog,comp_tbl,dcomp_tbl

;read compress/decompress tables
  openr,lun,'compress.tbl',/get_lun
  comp_tbl=bytarr(4096)
  line='  '
  for i=0,3 do readf,lun,line
  readf,lun,comp_tbl,format='(6x,16z3)'
  free_lun,lun

  openr,lun,'decompress.tbl',/get_lun
  dcomp_tbl=intarr(256)
  line='  '
  for i=0,3 do readf,lun,line
  readf,lun,dcomp_tbl,format='(5x,16z4)'
  free_lun,lun

  compc=indgen(256)
  dcompc=dcomp_tbl(compc)
  
  data_comp=indgen(256)
  data_dcomp=dcomp_tbl(data_comp)
  digitz_tbl=fltarr(256) + 0.5
  digitz_tbl(1:254)=float(( 0.5* (dcomp_tbl(data_comp(1:254) + 1) - $
                                  dcomp_tbl(data_comp(1:254) - 1)) ) ) / 2  
  digitz_tbl(255)=float( 0.5*(dcomp_tbl(data_comp(255)) - $
                           dcomp_tbl(data_comp(254)) ) )   
  
 print, 'data_comp  data_dcomp  digitz_tbl'
 for i=0,255 do print,data_comp(i),data_dcomp(i),digitz_tbl(i)

end
