pro read_calfile, reset_angle, num_meas, wintype, numwin, cups, integrate_num, rmid_time 

File  = '/crater/observatories/wind/code/cal/calfile_mode_params'

temp = fltarr(3,40)
cups = intarr(40)
integrate_num = intarr (40)
rmid_time = fltarr (40)

openr, 1, File
readf, 1,format = '(/)'
readf,1, reset_angle, num_meas, wintype, numwin, temp

cups = long(temp[0, *])
integrate_num = long(temp[1, *])
rmid_time = temp[2, *]

close, 1
end
