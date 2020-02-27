FUNCTION MFI_READ_OR, OR_PATH=OR_PATH, OR_PREFIX=OR_PREFIX, ORBIT=ORBIT

;- find orbit file with latest version, return 1 if no files
files = file_search(or_path, or_prefix + '*.cdf', count=n_all_files)
   if (n_all_files eq 0) then return, 1
filename = (files[sort(files)])[n_all_files-1]

;- initialize orbit variable names and open file
varnames  = ['Epoch', 'GSE_POS', 'GSE_VEL', 'GSM_POS', 'GSM_VEL']
uvarnames = ['Epoch', 'GSE_POS', 'GSE_VEL', 'GSM_POS', 'GSM_VEL']
n_dimens  = [1, 3, 3, 3, 3]
nvarnames = n_elements(varnames)
cdfid     = cdf_open(filename, /readonly)

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
      q = (varvalue lt 2d*validmin) or (varvalue gt 2d*validmax)
   endelse
   if i eq (nvarnames-1) then orbit = create_struct(uvarnames[i], {value:double(varvalue), q:q}) $
                         else orbit = create_struct(uvarnames[i], {value:double(varvalue), q:q}, orbit)
endfor

;- close cdf
cdf_close, cdfid

;- check that position change and velocity are in agreement
n_data = n_elements(orbit.epoch.value)
for i=0, n_data-2 do begin
   d_second  = (orbit.epoch.value[0,i+1]  - orbit.epoch.value[0,i])/1d3
   d_pos_gse = orbit.gse_pos.value[*,i+1] - orbit.gse_pos.value[*,i]
   d_pos_gsm = orbit.gsm_pos.value[*,i+1] - orbit.gsm_pos.value[*,i]
   if (sqrt(total((d_pos_gse - (orbit.gse_vel.value[*,i]+orbit.gse_vel.value[*,i+1])*d_second/2d)^2d)) gt 5d) $
      then begin
      orbit.gse_pos.q[*,i]   = 1
      orbit.gse_pos.q[*,i+1] = 1
      orbit.gse_vel.q[*,i]   = 1
      orbit.gse_vel.q[*,i+1] = 1
   endif
   if (sqrt(total((d_pos_gsm - (orbit.gsm_vel.value[*,i]+orbit.gsm_vel.value[*,i+1])*d_second/2d)^2d)) gt 5.5d) $
      then begin
      orbit.gsm_pos.q[*,i]   = 1
      orbit.gsm_pos.q[*,i+1] = 1
      orbit.gsm_vel.q[*,i]   = 1
      orbit.gsm_vel.q[*,i+1] = 1
   endif
endfor

;- return 1 if no good both gse and gsm orbit data
sub_gse_ok = where((orbit.epoch.q eq 0) and (total(orbit.gse_pos.q, 1) eq 0), n_gse_ok)
sub_gsm_ok = where((orbit.epoch.q eq 0) and (total(orbit.gsm_pos.q, 1) eq 0), n_gsm_ok)
if (n_gse_ok + n_gsm_ok) eq 0 then return, 1 else return, 0

END
