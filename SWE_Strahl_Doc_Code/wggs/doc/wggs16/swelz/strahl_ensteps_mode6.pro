;program strahl_ensteps_mode6.pro

;============ convert STRAHL voltage steps to eV ============================

;---mode 6 offsets into STRAHL voltage table
strlstep= $
 [1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,$
18, 20, 22, 24, 26, 28, 30, 32, 36, 40, 44, 48, 52, 56, 60, 64] 

;---the STRAHL voltage table
vltsperstep=987./255.

;---STRAHL volts to ev conversion   
vtoev=4.9971 

strlstep_energy = vtoev * strlstep * vltsperstep

print,'STRAHL: step   energy(eV)'
for i=0,n_elements(strlstep_energy)-1 do print,strlstep(i),strlstep_energy(i)
print,'maximum:',' 127',vtoev * 127 * vltsperstep

;============ convert VEIS voltage steps to eV ============================

;---mode 6 offsets into VEIS voltage table
veistep= $
[1,   3,   5,   7,   9,  11,  13,  15,  17,  20,  23,  26,  29,  32,  35,  38]

;---the VEIS voltage table
veis_voltbl= $
       [1.05,  1.18,    1.35,   1.53,   1.75,   1.98,   2.25,   2.55,  $
        2.86,  3.26,    3.73,   4.23,   4.82,   5.45,   6.22,   7.08,  $
        7.90,  9.04,   10.33,  11.74,  13.37,  15.20,  17.32,  19.72,  $
       22.03,  25.21,  28.91,  32.86,  37.37,  42.35,  48.54,  55.22,  $
       62.79,  71.86,  82.27,  93.58, 106.49, 120.93, 138.12, 157.54,  $
      176.73, 202.28, 231.59, 263.38, 299.91, 340.09, 388.45, 443.26,  $
      495.79,  567.7, 649.90, 739.1,  841.56, 954.25, 1.089e3, 1.2435e3, $
    1.3873e3, 1.5886e3, 1.818e3, 2.067e3, 2.3528e3, 2.666e3, 3.043e3, 3.470e3]
    
;---VEIS volts to ev conversion
vtoev=7.05

veistep_energy=vtoev * veis_voltbl(veistep) 

print,' ' & print,' '
print,'VEIS: step   energy(eV)'
for i=0,n_elements(veistep)-1 do print,veistep(i),veistep_energy(i)


end