FUNCTION MFI_READ_LZ_HK, FILE_PATH=PATH, FILE_PREFIX=PREFIX, HK=HK, N_MAFR=N_MAFR, $
                         DATA_RECS=DATA_RECS, FILE_HDR=FILE_HDR, FILENAME=FILENAME

;- find data file with latest version, return 1 if no files
filename = ''
files    = file_search(path, prefix + '*.dat', count=n_all_files)
if (n_all_files eq 0) then return, 1
filename = (files[sort(files)])[n_all_files-1]

;- read data from file as byte sequence, return 2 if error or size=0
openr, lun, filename, error=error, /get_lun
   if (error ne 0) then return, 2
filesize = (fstat(lun)).size
   if (filesize eq 0) then return, 2
byte_seq = bytarr(filesize)
readu, lun, byte_seq
free_lun, lun

;- find number of major frames, physical record length, and minor frame length
n_mafr       = mfi_bytearr_to_long(byte_seq[84:87])
phys_rec_len = mfi_bytearr_to_long(byte_seq[176:179])
mifr_len_max = floor((phys_rec_len - 300)/250)

;- read file header record and data records, and return 0
;- file header record
file_hdr  = byte_seq[0:phys_rec_len-1]
data_recs = replicate({phys_rec_n:0L, mafr_count:0L, epoch:0d, msec_day:0d, tel_mode:0L, $
                       q_mifr:bytarr(250), mafr:bytarr(mifr_len_max, 250)},  n_mafr)
;- data records
for i=1L, n_mafr do begin
   data_recs[i-1].phys_rec_n = mfi_bytearr_to_long( byte_seq[i*phys_rec_len +  4 : i*phys_rec_len +  7] )
   data_recs[i-1].mafr_count = mfi_bytearr_to_long( byte_seq[i*phys_rec_len +  8 : i*phys_rec_len + 11] )
   data_recs[i-1].tel_mode   = mfi_bytearr_to_long( byte_seq[i*phys_rec_len + 44 : i*phys_rec_len + 77] )
   data_recs[i-1].q_mifr     = byte_seq[i*phys_rec_len + 48 : i*phys_rec_len + 297]

   year      = mfi_bytearr_to_long( byte_seq[i*phys_rec_len + 20 : i*phys_rec_len + 23] )
   day_year  = mfi_bytearr_to_long( byte_seq[i*phys_rec_len + 24 : i*phys_rec_len + 27] )
   msec_day  = mfi_bytearr_to_long( byte_seq[i*phys_rec_len + 28 : i*phys_rec_len + 31] )
   ;mksec     = mfi_bytearr_to_long( byte_seq[i*phys_rec_len + 32 : i*phys_rec_len + 35] )
   cdf_epoch, epoch, year, 0, day_year, 0, 0, 0, msec_day, /compute_epoch
   data_recs[i-1].epoch     = epoch     ;+ mksec/1d3
   data_recs[i-1].msec_day  = msec_day  ;+ mksec/1d3

   case data_recs[i-1].tel_mode of
      1: if keyword_set(hk) then mifr_len_current=15 else mifr_len_current=25
      5: if keyword_set(hk) then mifr_len_current=15 else mifr_len_current=25
      4: if keyword_set(hk) then mifr_len_current=15 else mifr_len_current=25
      8: if keyword_set(hk) then mifr_len_current=15 else mifr_len_current=25
      3: if keyword_set(hk) then mifr_len_current=66 else mifr_len_current=23
      7: if keyword_set(hk) then mifr_len_current=66 else mifr_len_current=23
   else: begin & data_recs[i-1].q_mifr = 1 & mifr_len_current = 1 & end
   endcase

   data_recs[i-1].mafr[0:mifr_len_current-1,*] = $
      reform(byte_seq[i*phys_rec_len +300 : i*phys_rec_len +300 +mifr_len_current*250L -1], mifr_len_current, 250)
endfor

;- return 0
return, 0

END