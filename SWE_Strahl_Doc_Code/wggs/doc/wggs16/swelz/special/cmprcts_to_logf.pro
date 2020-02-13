function cmprcts_to_logf,cts,wsteps  ;convert image compressed counts to logf's

common log_delog,comp_tbl,dcomp_tbl

print,'cmprcts_to_logf:'
help,cts,wsteps

;------------------------- get steps in energy sweep ------------------------

;en_tbl = the table of possible swe energy steps, assumed in ascending order
velec_tbl=[1.05,  1.18,    1.35,   1.53,   1.75,   1.98,   2.25,   2.55,  $
        2.86,  3.26,    3.73,   4.23,   4.82,   5.45,   6.22,   7.08,  $
        7.90,  9.04,   10.33,  11.74,  13.37,  15.20,  17.32,  19.72,  $
       22.03,  25.21,  28.91,  32.86,  37.37,  42.35,  48.54,  55.22,  $
       62.79,  71.86,  82.27,  93.58, 106.49, 120.93, 138.12, 157.54,  $
      176.73, 202.28, 231.59, 263.38, 299.91, 340.09, 388.45, 443.26,  $
    495.79, 567.7,  649.90, 739.1,  841.56, 954.25, 1.089e3, 1.2435e3, $
    1.3873e3, 1.5886e3, 1.818e3, 2.067e3, 2.3528e3, 2.666e3, 3.043e3, 3.470e3]

vtoev=7.05 ;volts to ev conversion
en_tbl=vtoev * velec_tbl

;wsteps = actual energy step levels (offsets into energy table)
;print,'which energy steps ',wsteps


en_steps=en_tbl(wsteps)         ;actual energy (ev)
;print,'en_steps ', en_steps

vel=sqrt(en_steps/2.85e-16)    ;velocity steps (1e8 cm/s)
 
;----do de-compression from 8-bit to 12-bit count data ----------------------- 
;---- also do conversion from counts to f (phase space densiy) ---------------

;swe conversion from counts to f's : cf (J. Keller)
cf=7.2e-7 

;decompress (dcomp_tbl) and convert decompressed counts to phase density (f)
;print,'ctof: decompressing counts cts ;',cts 
print,vel 
s=size(vel)
case s(0) of
1: begin 
    ss=size(cts)
    print,s
    print,ss
    if ss(2) ne s(1) then stop,'img_fp: cts and vel have wrong dimensions' 
    logf=( 1./(cf * vel^4) ) # replicate(1,s(1)) * dcomp_tbl( cts )   
   endcase
0: logf=( 1./(cf * vel^4) ) * dcomp_tbl( cts )
else: stop,'img_fp: vel has wrong dimensions'
endcase

w=where(logf ne 0)
if w(0) ne -1 then logf(w) = alog10(logf(w))

print,'cmprcts_to_logf: vel',vel

return,logf 

end
