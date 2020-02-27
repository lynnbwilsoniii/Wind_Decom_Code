PRO MFI_STATUS_WORD12, LZ_RECS=RECS, SW12=SW12, QSW12=QSW12

;- define and initialize common block
@mfi_common_definition
if (n_elements(cal) eq 0) then mfi_common_initialization

;- status word 1,2 arrays
n_mafr = (size(recs))[1]
qsw12  = bytarr(5, n_mafr)
sw12   = replicate({sw12_struc}, 5, n_mafr)

;- reform recs
records = reform(recs, 1, n_mafr)

;- status word 12 and quality
for i=0, 4 do begin
   sw12[i,*].tel_rate    =  (records.mafr[0, i*50] and 128b) eq 128b
   sw12[i,*].swap        =  (records.mafr[0, i*50] and  64b) eq  64b
   sw12[i,*].mode        = ((records.mafr[1, i*50] and 128b) eq 128b)*2b + $
                           ((records.mafr[1, i*50] and  64b) eq  64b)
   ;- inboard mag
   sw12[i,*].auto_man[0] =  (records.mafr[1, i*50] and  32b) eq  32b
   sw12[i,*].range[0]    = ((records.mafr[1, i*50] and  16b) eq  16b)*4b + $
                           ((records.mafr[1, i*50] and   8b) eq   8b)*2b + $
                           ((records.mafr[1, i*50] and   4b) eq   4b)
   sw12[i,*].calib[0]    =  (records.mafr[1, i*50] and   2b) eq   2b
   sw12[i,*].el_flip[0]  =  (records.mafr[1, i*50] and   1b) eq   1b

   ;- outboard mag
   sw12[i,*].auto_man[1] =  (records.mafr[0, i*50] and  32b) eq  32b
   sw12[i,*].range[1]    = ((records.mafr[0, i*50] and  16b) eq  16b)*4b + $
                           ((records.mafr[0, i*50] and   8b) eq   8b)*2b + $
                           ((records.mafr[0, i*50] and   4b) eq   4b)
   sw12[i,*].calib[1]    =  (records.mafr[0, i*50] and   2b) eq   2b
   sw12[i,*].el_flip[1]  =  (records.mafr[0, i*50] and   1b) eq   1b

   ;- quality of corrsponding minor frame
   qsw12[i,*] = records.q_mifr[i*50]
endfor

;- update quality of sw12 based on values
for i=0, n_mafr-1 do begin
   ;- check sw12 for unexpected values
   qsw12[*,i] += (sw12[*,i].calib[0]    ne 0) or $
                 (sw12[*,i].calib[1]    ne 0) or $
                 (sw12[*,i].el_flip[0]  ne 0) or $
                 (sw12[*,i].el_flip[1]  ne 0) or $
                 (sw12[*,i].auto_man[0] ne 0) or $
                 (sw12[*,i].auto_man[1] ne 0) or $
                 (sw12[*,i].tel_rate ne (records[i].tel_mode ge 5)) or $
                 (sw12[*,i].mode gt 2) or $
                 (sw12[*,i].swap ne (total(sw12[*,i].swap) ge 3))

   ;- additional check of sw12.mode
   sub_temp = where(qsw12[*,i] eq 0, n_temp)
   if (n_temp gt 1) then begin
      temp      = where(sw12[sub_temp,i].mode eq 0, n_mode0)
      temp      = where(sw12[sub_temp,i].mode eq 1, n_mode1)
      temp      = where(sw12[sub_temp,i].mode eq 2, n_mode2)
      n_mode012 = [n_mode0, n_mode1, n_mode2]
      n_modes   = total(n_mode012 ne 0)
      if (n_modes eq 2) then begin
         temp = max(n_mode012, major_mode)
         sub_minor_mode = where(sw12[sub_temp,i].mode ne major_mode)
         qsw12[sub_temp[sub_minor_mode],i] = 1
      endif
      if (n_modes eq 3) then qsw12[*,i] = 1
   endif
endfor

;- update quality so that qsw12=0 -ok, qsw12=1 -bad
qsw12 = (qsw12 ne 0)

END