pro coil



freq   = DBLARR(300)
trans  = DBLARR(300)
panois = DBLARR(300)
;
;  => CALCULATES AND PLOTS WIND SEARCH COIL TRANSFER F AND NOISE THRESH
;
;   freq  trans(V/nT)  noise(nT/rtHz)  noise(v**2/rtHz)  noise(V/rtHz)
;     40.     .012       1.17E-04       1.98E-12            1.41E-6
;    100.     .03         4.9E-05       2.16E-12            1.47E-6
;    300.     .09         2.9E-05       6.81E-12            2.61E-6
;   1000.     .290       2.18E-05       4.00E-11            6.32E-6
;   2000.     .6         2.15E-05       1.66E-10            1.29E-5
;   3000.     .750       2.12E-05       2.53E-10            1.59E-5
;   4000.     .7         2.10E-05       2.16E-10            1.47E-5
;   5000.     .57        2.07E-05
;   6000.     .500       2.04E-05       1.04E-10            1.02E-5

gfreq = [1000.,2000.,3000.,4000.,5000.,6000.]
gtr   = [.290,.6,.750,.7,.57,.500]
gpan  = [2.12,2.09,2.07,2.05,2.03,2.01]

r1    = 49.9d3
c1    = 1d-6
r2    = 1000d3
c2    = 1d-9
twopi = 2d0*!DPI
iterm = -5

;
;  => CALCULATE FREQUENCY LIST
;
frat    = SQRT(1d1)
freq[0] = 0.1
FOR n=1L, 79L DO BEGIN
  nf = n
  k  = n - 1L
  freq[n] = freq[k]*frat
ENDFOR

;
;  => CALCULATE ARRAY 'TRANS' OF TRANSFER FUNCTION IN V/nT
;
FOR n=0L, 79L DO BEGIN
  trans[n] = 0.29*(freq[n]/1d3)
ENDFOR

;
;  => MULTIPLY BY EFFECT OF NEW IOWA INPUT CAPS .1 UF FEEDING 2.04 MEG
;
FOR n=0L, 79L DO BEGIN
  trans[n] = trans[n]/SQRT(1. + (0.78/freq[n])^2)
ENDFOR
;
;  => CALCULATE NOISE LEVEL IN nT/SQRT(HZ)
;
FOR n=0L, 79L DO BEGIN
;  => APPROXIMATE NOISE BY A/FREQ + B.  THIS IS TOO HIGH FOR CROSSOVER FREQS
  panois[n] = 1.8d-5 + 1d-4*(4d1/freq[n])
ENDFOR

FOR n=0L, 5L DO BEGIN
  nf        += 1L
  freq[nf]   = gfreq[n]
  trans[nf]  = gtr[n]
  panois[nf] = gpan[n]*1d-5
ENDFOR
;
;  => CALCULATE TRANSFER FUNCTION (VOLTS/NT) WITH FIRST AMPLIFIER GAIN
;
f1 = 1d0/(twopi*r1*c1)
f2 = 1d0/(twopi*r2*c2)
n  = nf
FOR n=0L, nf - 1L DO BEGIN
  w        = twopi*freq[n]
  z1       = COMPLEX(r1,-1d0/(w*c1))
  y2       = COMPLEX(1d0/r2,w*c2)
  z2       = 1d0/y2
  cgain    = z2/z1
  gain     = ABS(cgain)
  trans[n] = trans[n]*gain
ENDFOR
;
;  => CALCULATE NOISE LEVEL IN V/SQRT(HZ)
;
pant   = 0d0
panlow = 0d0
n      = 0
panois[n] = panois[n]*trans[n]
FOR n=1L, nf - 1L DO BEGIN
  k         = n - 1L
  panois[n] = panois[n]*trans[n]
  dnoise    = 5d-1*((freq[n] - freq[k])*(panois[n] + panois[k]))^2
  pant     += dnoise
  IF (freq[n] LT 170.) THEN panlow += dnoise
ENDFOR
panrms = SQRT(pant)
str    = 'Total Preamp noise (V^2/Hz):  '+STRTRIM(STRING(FORMAT='(f15.3)',pant),2)
str    = str[0]+'  RMS = '+STRTRIM(STRING(FORMAT='(f15.3)',panrms),2)
PRINT,str
panrms = SQRT(panlow)
str    = 'Preamp Noise Below 170 Hz (V^2/Hz):  '
str    = str[0]+STRTRIM(STRING(FORMAT='(f15.3)',panlow),2)+'  RMS = '
str    = str[0]+STRTRIM(STRING(FORMAT='(f15.3)',panrms),2)
PRINT,str

END


FUNCTION parint,x1,x0,x2,y1,y0,y2,x,y

;
;  => PARABOLIC INTERPOLATION ON UNIFORMLY SPACED X
;
	aa = x0
	bb = (y2 - y1)/(x2 - x1)
	cc = 2.*(x1 + x2 - 2.*x0)/(x2 - x1)**2
	y  = aa + bb*(x-x0) + cc*(x-x0)**2

RETURN
END










