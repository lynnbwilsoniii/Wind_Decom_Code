FUNCTION MFI_READ_SPHA, SPHA_RECS=SPHA_RECS, FILE_PATH=FILE_PATH, FILE_PREFIX=FILE_PREFIX

;- find spha file with latest version, return 1 if no files
files = file_search(file_path, file_prefix + '*.cdf', count=n_all_files)
if (n_all_files eq 0) then return, 1
filename = (files[sort(files)])[n_all_files-1]

;- initialize spha variable names and open file
varnames  = ['Epoch', 'SPIN_PHASE', 'AVG_SPIN_RATE', 'STNDEV_SPIN_RATE', 'FAULT']
uvarnames = ['Epoch', 'SPIN_PHASE', 'AVG_SPIN_RATE', 'STNDEV_SPIN_RATE', 'FAULT']
n_dimens  = [1,1,1,1,1]
nvarnames = n_elements(varnames)
cdfid     = cdf_open(filename, /readonly)

;- read spha variable values, plus fillval, validmin, and validmax values for quality
for i=(nvarnames-1), 0, -1 do begin
   cdf_control, cdfid, get_var_info=var_info, variable=varnames[i]
   cdf_varget,  cdfid, varnames[i], varvalue, rec_count=var_info.maxrec+1
   if (n_dimens[i] eq 1) then varvalue = varvalue[0,*]
   if i gt 0 then begin
      cdf_attget,  cdfid, 'VALIDMIN', varnames[i], validmin
      cdf_attget,  cdfid, 'VALIDMAX', varnames[i], validmax
      q = (varvalue lt validmin) or (varvalue gt validmax)
   endif else begin
      q = (varvalue eq 0)
   endelse
   if i eq (nvarnames-1) then spha_recs = create_struct(uvarnames[i], {value:double(varvalue), q:q}) $
                         else spha_recs = create_struct(uvarnames[i], {value:double(varvalue), q:q}, spha_recs)
endfor

;- check that real and predicted phase changes between two nearby points are within 1 deg
n_rec = n_elements(spha_recs.epoch.value)
d_phase_pred0 = ((spha_recs.epoch.value[1L:n_rec-1L] - spha_recs.epoch.value[0L:n_rec-2L])* $
                 spha_recs.avg_spin_rate.value[0L:n_rec-2L]/1d3) mod (2d*!dpi)
d_phase_pred1 = ((spha_recs.epoch.value[1L:n_rec-1L] - spha_recs.epoch.value[0L:n_rec-2L])* $
                 spha_recs.avg_spin_rate.value[1L:n_rec-1L]/1d3) mod (2d*!dpi)
d_phase_real  = (spha_recs.spin_phase.value[1L:n_rec-1L] - spha_recs.spin_phase.value[0L:n_rec-2L] + 2d*!dpi) mod $
                (2d*!dpi)
sub_temp0 = where(acos(cos(d_phase_pred0 - d_phase_real)) gt 1d*!dtor, n_sub_temp0)
sub_temp1 = where(acos(cos(d_phase_pred1 - d_phase_real)) gt 1d*!dtor, n_sub_temp1)
if n_sub_temp0 gt 0 then begin
   spha_recs.avg_spin_rate.q[sub_temp0] or= 1
   spha_recs.spin_phase.q[sub_temp0]    or= 1
endif
if n_sub_temp1 gt 0 then begin
   spha_recs.avg_spin_rate.q[sub_temp1+1L] or= 1
   spha_recs.spin_phase.q[sub_temp1+1L]    or= 1
endif

;- close cdf
cdf_close, cdfid

END
