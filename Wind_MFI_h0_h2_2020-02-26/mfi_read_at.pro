FUNCTION MFI_READ_AT, AT_PATH=AT_PATH, AT_PREFIX=AT_PREFIX, OR_PATH=OR_PATH, OR_PREFIX=OR_PREFIX, $
                      ATTITUDE=ATTIT, AT_FILE=AT_FILE, OR_FILE=OR_FILE
; corrects gse coordinates of z payload axis using z payload axis in gci (attitude file) and 
; sun vector in gci (orbit file). return 0 if gse is corrected, 2 if original gse

;- reset at_file and or_file
at_file  = ''
or_file  = ''

;--- attitude
;- find attitude file with latest version, return 1 if no files
at_files = file_search(at_path, at_prefix + '*.cdf', count=n_at_files)
   if (n_at_files eq 0) then return, 1
at_file  = (at_files[sort(at_files)])[n_at_files-1]

;- initialize attitude variable names and open file
varnames  = ['Epoch', 'GCI_R_ASCENSION', 'GCI_DECLINATION', $
             'GSE_R_ASCENSION', 'GSE_DECLINATION', 'GSM_R_ASCENSION', 'GSM_DECLINATION']
uvarnames = ['Epoch', 'GCI_R_ASCN', 'GCI_DECL', $
             'GSE_R_ASCN', 'GSE_DECL', 'GSM_R_ASCN', 'GSM_DECL']
n_dimens  = [1,1,1,1,1,1,1]
nvarnames = n_elements(varnames)
cdfid     = cdf_open(at_file, /readonly)

;- read attitude variable values, plus fillval, validmin, and validmax values for quality
for i=(nvarnames-1), 0, -1 do begin
   cdf_control, cdfid, get_var_info=var_info, variable=varnames[i]
   cdf_varget,  cdfid, varnames[i], varvalue, rec_count=var_info.maxrec+1
   if (n_dimens[i] eq 1) then varvalue = varvalue[0,*]
   if i eq 0 then begin
      q = byte(varvalue*0d)
   endif else begin
      cdf_attget,  cdfid, 'VALIDMIN', varnames[i], validmin
      cdf_attget,  cdfid, 'VALIDMAX', varnames[i], validmax
      q = (varvalue lt validmin) or (varvalue gt validmax)
   endelse
   if i eq (nvarnames-1) then attit = create_struct(uvarnames[i], {value:double(varvalue), q:q}) $
                         else attit = create_struct(uvarnames[i], {value:double(varvalue), q:q}, attit)
endfor

;- close cdf
cdf_close, cdfid

;- set gse q with 0 to 2 (means no gse correction for that points yet); leave bad quality (1) points as are
sub_temp = where(attit.gse_r_ascn.q eq 0, n_temp)
   if (n_temp gt 0) then attit.gse_r_ascn.q[sub_temp] = 2
sub_temp = where(attit.gse_decl.q eq 0, n_temp)
   if (n_temp gt 0) then attit.gse_decl.q[sub_temp] = 2

;- subscripts of good epoch and gci attitude records
sub_q_at = where((attit.epoch.q eq 0) and (attit.gci_r_ascn.q eq 0) and (attit.gci_decl.q eq 0), n_q_at)
   if (n_q_at eq 0) then return, 2 ;means no gse correction for any point
;--- end: attitude

;--- orbit
;- find orbit file with latest version, return 2 if no files (2 means no gse correction)
or_files = file_search(or_path, or_prefix + '*.cdf', count=n_or_files)
   if (n_or_files eq 0) then return, 2
or_file = (or_files[sort(or_files)])[n_or_files-1]

;- initialize orbit variable names and open file
varnames  = ['Epoch', 'SUN_VECTOR']
uvarnames = ['Epoch', 'GCI_SVECT']
n_dimens  = [1,3]
nvarnames = n_elements(varnames)
cdfid     = cdf_open(or_file, /readonly)

;- read orbit variable values, plus fillval, validmin, and validmax values for quality
for i=(nvarnames-1), 0, -1 do begin
   cdf_control, cdfid, get_var_info=var_info, variable=varnames[i]
   cdf_varget,  cdfid, varnames[i], varvalue, rec_count=var_info.maxrec+1
   if (n_dimens[i] eq 1) then varvalue = varvalue[0,*]
   if i eq 0 then begin
      q = byte(varvalue*0d)
   endif else begin
      cdf_attget,  cdfid, 'VALIDMIN', varnames[i], validmin
      cdf_attget,  cdfid, 'VALIDMAX', varnames[i], validmax
      q = (varvalue lt validmin) or (varvalue gt validmax)
   endelse
   if i eq (nvarnames-1) then orbit = create_struct(uvarnames[i], {value:double(varvalue), q:q}) $
                         else orbit = create_struct(uvarnames[i], {value:double(varvalue), q:q}, orbit)
endfor

;- close cdf
cdf_close, cdfid

;- subscripts of good epoch and gci_svect orbit records
sub_q_or = where((orbit.epoch.q eq 0) and (orbit.gci_svect.q[0,*] eq 0) and $
                 (orbit.gci_svect.q[1,*] eq 0) and (orbit.gci_svect.q[2,*] eq 0), n_q_or)
if (n_q_or eq 0) then return, 2 ;means no gse correction for any point
;--- end: orbit

;--- gse replacement with the correct one
;- gse axes in gci
zgse_gci_r_ascn = 1.5d*!dpi
zgse_gci_decl   = (90d - 23.439281d)*!dtor
zgse_gci      = dblarr(3, n_q_or)
zgse_gci[0,*] = cos(zgse_gci_decl)*cos(zgse_gci_r_ascn)
zgse_gci[1,*] = cos(zgse_gci_decl)*sin(zgse_gci_r_ascn)
zgse_gci[2,*] = sin(zgse_gci_decl)
xgse_gci      = dblarr(3, n_q_or)
gci_svect_tot = reform(sqrt(total(orbit.gci_svect.value[*, sub_q_or]^2d, 1)), 1, n_q_or)
xgse_gci[0,*] = orbit.gci_svect.value[0, sub_q_or]/gci_svect_tot
xgse_gci[1,*] = orbit.gci_svect.value[1, sub_q_or]/gci_svect_tot
xgse_gci[2,*] = orbit.gci_svect.value[2, sub_q_or]/gci_svect_tot
ygse_gci      = dblarr(3, n_q_or)
ygse_gci[0,*] = zgse_gci[1,*]*xgse_gci[2,*] - zgse_gci[2,*]*xgse_gci[1,*]
ygse_gci[1,*] = zgse_gci[2,*]*xgse_gci[0,*] - zgse_gci[0,*]*xgse_gci[2,*]
ygse_gci[2,*] = zgse_gci[0,*]*xgse_gci[1,*] - zgse_gci[1,*]*xgse_gci[0,*]

;- z payload in cartesian gci
zpay_xgci = cos(attit.gci_decl.value[sub_q_at])*cos(attit.gci_r_ascn.value[sub_q_at])
zpay_ygci = cos(attit.gci_decl.value[sub_q_at])*sin(attit.gci_r_ascn.value[sub_q_at])
zpay_zgci = sin(attit.gci_decl.value[sub_q_at])

;- for each good orbit point find corresponding gse attitude and correct it
n_corrected = 0L
for i=0, n_q_or-1 do begin
   depoch_temp = min(abs(attit.epoch.value[sub_q_at] - orbit.epoch.value[sub_q_or[i]]), sub_temp)
      if (depoch_temp gt 6d*60d3) then continue else n_corrected += 1
   
   ;- z payload in cartesian gse
   gci_to_gse_matrix = [ [xgse_gci[*,sub_q_or[i]]], [ygse_gci[*,sub_q_or[i]]], [zgse_gci[*,sub_q_or[i]]] ]
   zpay_gse          = reform(gci_to_gse_matrix ## $
                       [ [zpay_xgci[sub_temp]], [zpay_ygci[sub_temp]], [zpay_zgci[sub_temp]] ])

   ;- z payload in angular gse; update q with 0
   attit.gse_decl.value[sub_q_at[sub_temp]]   = asin(zpay_gse[2])
   attit.gse_r_ascn.value[sub_q_at[sub_temp]] = acos(zpay_gse[0]/sqrt(zpay_gse[0]^2d +zpay_gse[1]^2d))
   if (zpay_gse[1] lt 0) then $
      attit.gse_r_ascn.value[sub_q_at[sub_temp]] = (2d*!dpi - attit.gse_r_ascn.value[sub_q_at[sub_temp]])
   attit.gse_decl.q[sub_q_at[sub_temp]]   = 0
   attit.gse_r_ascn.q[sub_q_at[sub_temp]] = 0
endfor
;--- end: gse replacement with the correct one

;- return either 0 or 2
if (n_corrected gt 0) then return, 0 else return, 2

END
