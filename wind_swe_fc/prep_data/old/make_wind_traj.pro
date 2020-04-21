

; Convert trajectory info from 
; http://sscweb.gsfc.nasa.gov/cgi-bin/sscweb/Locator.cgi
; into .idl save files with the following elements:

; INSTRUCTIONS:
; download trajectory file from the above, request:
; time in dd hhh.hh format
; x, y, z GSE in RE units
; Y_SM, Z_SM in RE units
; bowshock, neutral sheet, and magnetopause distances in RE units
; save the output file (with full header) as 'wind.traj.YYYY.txt'
; to the temp directory at /nfs/plas7/d7/wind/temp/
;
; then run this converter

; take care not to choose any invalid ranges; the warning
; message will change the header length!


;TRAJ_BS         FLOAT     = Array[43800] 
;TRAJ_FDOY       DOUBLE    = Array[43800]
;TRAJ_MP         FLOAT     = Array[43800]
;TRAJ_NS         FLOAT     = Array[43800]
;TRAJ_X          FLOAT     = Array[43800]
;TRAJ_Y          FLOAT     = Array[43800]
;TRAJ_YSM        FLOAT     = Array[43800]
;TRAJ_Z          FLOAT     = Array[43800]
;TRAJ_ZSM        FLOAT     = Array[43800]

; all units are RE

pro make_wind_traj

readpath = '/nfs/plas7/d7/wind/temp/'
savepath = '/nfs/plas7/d7/wind/input/traj/'
spawn, 'ls ' + readpath+'*traj*', files

for i = 0 , n_elements(files)-1 do begin

year = strmid(files[i], 7, 4, /reverse)
data = read_ascii(files[i], data_start = 56)

;n=n_elements(data.field01[0, *])

q = where(finite(data.field01[1, *]), n)


traj_fdoy = dblarr(n)
traj_x = fltarr(n)
traj_y = fltarr(n)
traj_z = fltarr(n)
traj_ysm = fltarr(n)
traj_zsm = fltarr(n)
traj_ns = fltarr(n)
traj_bs = fltarr(n)
traj_mp = fltarr(n)

q = where(finite(data.field01[1, *]))

traj_fdoy[*] = data.field01[1, q] + data.field01[2, q]/24.
traj_x[*] = data.field01[3, q]
traj_y[*] = data.field01[4, q]
traj_z[*] = data.field01[5, q]
traj_ysm[*] = data.field01[6, q]
traj_zsm[*] = data.field01[7, q]
traj_ns[*] = data.field01[8, q]
traj_bs[*] = data.field01[9, q]
traj_mp[*] = data.field01[10, q]

savefile = savepath + 'wind.traj.'+year+'.idl'
save, traj_fdoy, traj_x, traj_y, traj_z, traj_ysm, traj_zsm, traj_bs, $
traj_ns, traj_mp, filename = savefile

endfor

end
