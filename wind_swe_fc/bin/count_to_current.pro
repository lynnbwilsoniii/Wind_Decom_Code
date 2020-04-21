pro count_to_current, current_lookup

n = indgen (3072) ;a "reordered count" (0-3071, higher count is higher current)

;arrays containing the calibration parameter, a and b, for cup1,
;cup2, collector a, and collector b, for ranges 0, 1 and 2
board_1a = fltarr (2, 3)
board_1b = fltarr (2, 3)
board_2a = fltarr (2, 3)
board_2b = fltarr (2, 3)
trash = strarr (4)
current_lookup = fltarr (4, 3072) ;this will contain the table that 
                                  ;converts from counts to currents
File = '/crater/observatories/wind/code/cal/calfile_brds'

;*****************************************************
;read the data in from file into 8 one by three arrays
;each containing the values for a, or b, in cup 1 or 2, 
;collector a or b

openr, 1, File
readf, 1, format = '(/)'
readf, 1, format = '(/)'
readf, 1, board_1a
readf, 1, format = '(/)'
readf, 1, board_1b
readf, 1, format = '(/)'
readf, 1, board_2a
readf, 1, format = '(/)'
readf, 1, board_2b
close, 1

;*****************************
;b_1a=(7774.13, 7645.81, 7814.57)
;b_1b=(8039.99, 7645.81, 7826.18) 
;b_2a=(7748.48, 7636.33, 7793.92) 
;b_2b=(7796.59, 7600.59, 7776.34)

;a_1a=(269.923, 264.742, 272.789)
;a_1b=(280.538, 266.083, 273.593)
;a_2a=(269.516, 264.474, 272.252)
;a_2b=(272.149, 263.937, 271.984)
;*****************************

;*****************************************************
;For each range, "re-ordered count" = a(i)*log(I) + b(i)
;A different "a" and "b" are determined for each amplifier 
;range of each collector or each cup.

;Calibrated current in amps is evaluated as follows:
;      current=exp((n - b(range))/a(range))

range = n/1024

current_1a = exp((n - board_1a[1, range])/board_1a[0, range])
current_1b = exp((n - board_1b[1, range])/board_1b[0, range])
current_2a = exp((n - board_2a[1, range])/board_2a[0, range])
current_2b = exp((n - board_2b[1, range])/board_2b[0, range])

;*******************************************************
;the next few lines of code use the parameters just read 
;in from the file to create a "lookup table" which converts
;from counts to current, given the cup and collector 
;information, as well as the value of the count.  This is a 
;(4, 3072) array, where the first 
;dimension is the cup and collector data:  0 = cup 1, coll a
;1 = cup 1, coll b, 2 = cup 2, coll a, 3 = cup 2, coll b.
;The second dimension is simply the count

current_lookup[0, *] = current_1a
current_lookup[1, *] = current_2a
current_lookup[2, *] = current_1b
current_lookup[3, *] = current_2b

end
