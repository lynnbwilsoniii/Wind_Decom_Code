FUNCTION MFI_TILTANG, EPOCH

n_epoch = n_elements(epoch)
iyr   = dblarr(n_epoch)
imth  = dblarr(n_epoch)
iday  = dblarr(n_epoch)
ihr   = dblarr(n_epoch)
imn   = dblarr(n_epoch)
isc   = dblarr(n_epoch)
imsc  = dblarr(n_epoch)
idy   = dblarr(n_epoch)

for i_e=0L, n_epoch-1L do begin
   cdf_epoch, epoch[i_e], yr_temp, mth_temp, day_temp, hr_temp, mn_temp, sc_temp, msc_temp, /b
   iyr[i_e]   = yr_temp
   imth[i_e]  = mth_temp
   iday[i_e]  = day_temp
   ihr[i_e]   = hr_temp
   imn[i_e]   = mn_temp
   isc[i_e]   = sc_temp
   imsc[i_e]  = msc_temp
   cdf_epoch, epoch_yr_temp, yr_temp, 1, 1, /c
   idy = floor((epoch[i_e] - epoch_yr_temp)/86400000d) + 1L
endfor
   
fday  = (ihr*3600d +imn*60d +isc)/86400d
dj    = 365d*(iyr-1900d) + ((iyr-1901d)/4d)+idy-0.5d +fday
t     = dj/36525d
vl    = (279.696678d + 0.9856473354d*dj mod 360.0d)
gst   = (279.690983d + 0.9856473354d*dj + 360.0d*fday + 180.0d mod 360.0d)/!radeg
g     = (358.475845d + 0.985600267d*dj mod 360.0d)/!radeg
slong = (vl + (1.91946d - 0.004789d*t)*sin(g) + 0.020094d*sin(2.0d*g))/!radeg
sub_gt2 = where(slong gt 2.0d*!dpi, n_gt2)
if n_gt2 gt 0 then slong[sub_gt2]=slong[sub_gt2]-2.0d*!dpi
sub_lt0 = where(slong lt 0.0d, n_lt0)
if n_lt0 gt 0 then slong[sub_lt0]=slong[sub_lt0]+2.0d*!dpi
obliq = (23.45229d - 0.0130125d*t)/!radeg
sob   = sin(obliq)
slp   = slong-9.924d-5
sind  = sob*sin(slp)
cosd  = sqrt(1.0d -sind^2d)
sc    = sind/cosd
sdec  = atan(sc)
srasn = !dpi - atan(cos(obliq)/sob*sc,-cos(slp)/cosd)

dt  = iyr + idy/365.0d - 1990.0d
g10 = 29775.0d - 18.0d*dt
g11 = -1851.0d + 10.6d*dt
h11 = 5411.0d - 16.1d*dt

sq   = g11^2d + h11^2d
sqq  = sqrt(sq)
sqr  = sqrt(g10^2d + sq)
sl0  = -h11/sqq
cl0  = -g11/sqq
st0  = sqq/sqr
ct0  = g10/sqr
stcl = st0*cl0
stsl = st0*sl0
ctsl = ct0*sl0
ctcl = ct0*cl0

s1 = cos(srasn)*cos(sdec)
s2 = sin(srasn)*cos(sdec)
s3 = sin(sdec)

cgst = cos(gst)
sgst = sin(gst)

dip1 = stcl*cgst - stsl*sgst
dip2 = stcl*sgst + stsl*cgst
dip3 = ct0

sps = dip1*s1 + dip2*s2 + dip3*s3
cps = sqrt(1.0d - sps^2d)
tiltang = asin(sps)*!radeg 
return, tiltang

END