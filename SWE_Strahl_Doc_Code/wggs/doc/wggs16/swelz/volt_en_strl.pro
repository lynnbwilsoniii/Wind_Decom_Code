function volt_en_strl,volt_step,vel=vel,en=en     ;vel_lvls,en_lvls

;function to convert strahl voltage steps obtained from housekeeping 
;to electron volts or electron velocities

;volt_step = offset into STRAHL voltage table obtained from hk

vltsperstep=987./255.   ;the voltage table

vtoev=4.9971 ;volts to ev conversion

if keyword_set(vel) ne 0 then $
  return,sqrt(vtoev * volt_step * vltsperstep /2.85e-16)
   
if keyword_set(en) ne 0 then return,vtoev * volt_step * vltsperstep

stop
;--------------------------- 19941227 example -----------------------------

;IDL> steps=lz.mf(hkm1(25).offs+indgen(32)) ;strahl steps from housekeeping data
;IDL> print,steps   ;the voltage table
;   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  18  20  22
;  24  26  28  30  32  36  40  44  48  52   0   0  56

;IDL> vltsperstep=987./255. ;the step to volts algorithm            

;second byte in veis-strahl data block is the voltage table index this spinbl
; and ranges from 0 to 31

;IDL> print,steps * vltsperstep         ;voltages
;      3.87059      7.74118      11.6118      15.4824      19.3529      23.2235
;      27.0941      30.9647      34.8353      38.7059      42.5765      46.4471
;      50.3176      54.1882      58.0588      61.9294      69.6706      77.4118
;      85.1529      92.8941      100.635      108.376      116.118      123.859
;      139.341      154.824      170.306      185.788      201.271      0.00000
;      0.00000      216.753


;IDL> print,volt_en_strl(steps,/en)    ;energy in electron volts
;      19.3417      38.6834      58.0252      77.3669      96.7086      116.050
;      135.392      154.734      174.075      193.417      212.759      232.101
;      251.442      270.784      290.126      309.467      348.151      386.834
;      425.518      464.201      502.885      541.568      580.251      618.935
;      696.302      773.669      851.036      928.402      1005.77      0.00000
;      0.00000      1083.14


;IDL> print,volt_en_strl(steps,/vel)    ;velocities in cm/sec
;  2.60510e+08  3.68417e+08  4.51217e+08  5.21021e+08  5.82519e+08  6.38118e+08
;  6.89246e+08  7.36835e+08  7.81531e+08  8.23806e+08  8.64015e+08  9.02434e+08
;  9.39284e+08  9.74741e+08  1.00895e+09  1.04204e+09  1.10525e+09  1.16504e+09
;  1.22190e+09  1.27624e+09  1.32835e+09  1.37849e+09  1.42687e+09  1.47367e+09
;  1.56306e+09  1.64761e+09  1.72803e+09  1.80487e+09  1.87857e+09      0.00000
;      0.00000  1.94948e+09


end
