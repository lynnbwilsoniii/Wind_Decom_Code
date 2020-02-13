function ctspbin_f_strl,cts,wstep,wt ;convert image compressed counts to logf's

common log_delog,comp_tbl,dcomp_tbl
common test1, weight

print,'cts_favgpitch_strl:'
help,cts,wstep

;wstep = energy step read from telem data (offset into voltage table)

;given the bin average of compressed counts weighted by geometrical 
;and efficiency factors for each detector (cts), use the bin average of the
;weights (wt) to compute logf

;the weighting factor for each sample = 1./cf_strl, 
;where cf_strl = geometry * efficiency * deltat * 0.5 * 1.e-5 for each detector

;the bin average of the weifghts (wt) has been computed from cf_strl when the 
;bin averages were created by program lzstrl
;the weights are are stored in byte form and converted when used in this proc

;strahl counts to f factor: cf_strl;  (efficiencies for hv step2)
  efficiency = [0.75926, 0.75926, 0.75926, 0.75926, 0.75926, 0.75926, $
               0.82945,  0.82945, 0.82945, 0.82945, 0.82945, 0.82945]

  geometry = [ 4.4349, 5.4222, 6.1882,  6.1182, 6.4422, 6.5440,$
               6.5149, 6.2687, 5.9394, 6.2334, 5.5779, 4.5819]

  deltat = 0.030

  cf_strl= geometry * efficiency * deltat * 0.5 * 1.e-5
  
  weight=1./cf_strl
  minwt=min(weight,max=maxwt)

;----do de-compression from 8-bit to 12-bit count data ----------------------- 
;---- also do conversion from counts to logf (phase space densiy) ------------

;decompress (dcomp_tbl) and convert decompressed counts to phase density (f)
   logf=$
     (1./(volt_en_strl(wstep,/vel)^4)) * $
     (minwt+wt*(maxwt-minwt)/255) * dcomp_tbl(cts)

   w=where(logf ne 0)
   if w(0) ne -1 then logf(w) = alog10(logf(w))


return,logf 

end
