;     @(#)get_count.pro   VERSION 1.1    06/30/98  11:01:10
;
;  June 1998, susama, jts

pro get_count, specmax, spinmax, fcspectra, cups, counts,$
     w_type_arr, windows_arr, tracking_arr, scan_arr, year_arr,$
     day_arr, sec_arr, spin_cnt_arr, win_index_arr, cup1_vel, $
     cup2_vel, cup1_EperQ, cup2_EperQ, cup1_vdel, cup2_vdel, $
     cup1_EQdel, cup2_EQdel , verbose=verbose

; angle by energy step array
counts = intarr (20, spinmax, 4, specmax + 1)

;declarations for the arrays of length 40
;A12 = intarr (20)
;n = indgen(40) 
;n = n*3

;declarations for sorting the collectors into cups

cup1 = where (cups eq 1)
cup2 = where (cups eq 2)

;store the information for each spectra
   ;decarations
    w_type_arr  = intarr(specmax + 1)
    windows_arr  = intarr(specmax + 1)
    tracking_arr = intarr(specmax + 1)
    scan_arr  = intarr(specmax + 1)
    year_arr  = lonarr(specmax + 1)
    day_arr  = lonarr(specmax + 1)
    sec_arr  = dblarr(specmax + 1)
    spin_cnt_arr = intarr(spinmax, specmax +1)
    win_index_arr = intarr(specmax +1, spinmax)
    cup1_EperQ = fltarr (spinmax, specmax+1)
    cup2_EperQ = fltarr (spinmax, specmax+1)
    cup1_vel = fltarr (spinmax, specmax+1)
    cup2_vel = fltarr (spinmax, specmax+1)
    cup1_EQdel = fltarr (spinmax, specmax+1)
    cup2_EQdel = fltarr (spinmax, specmax+1)
    cup1_vdel = fltarr (spinmax, specmax+1)
    cup2_vdel = fltarr (spinmax, specmax+1)

;***********************************************
;organization of the data in each  spectra

;***********************************************
;read in modulator levels

read_mod_levels, velocity1, velocity2, EperQ1, EperQ2
;***********************************************

for spec = 0, specmax do begin
 ;declaring spin_count
   spin_cnt = intarr(31)

 ;initializing the spin, and selecting the starting data
 spin = 1
 raw = FCspectra[spec].fcblocks[spin, *] ;a single fcblock
 
 ;test for when the relevant data will stop
 while not (((raw[0] eq 252) and (raw[1] eq 255)) or (spin eq 32)) do begin
  
  ;declarations for the blocks
 
  CC = raw (2:121) ;array fcblock minus the status bytes
  n = indgen(40) 
  n = n*3

 ;converting into 12 bit words with the information from A and B collectors
      A12 = 256*('0c'XB and CC[n+1])$
             + '3FF'X - ( 256*('03'XB and CC[n+1]) +CC[n] )
      B12 = 16*('c0'XB and CC[n+2])$
             + '3FF'X - (16*('3f'XB and CC[n+2]) + ('f0'XB and CC[n+1])/16 )

   ;sorting out the cups from the collectors and storing the data in the matrix

   ;counts[angles, spin, cup and collector, spec count]
   ;angles = 20, spin = 31, spec count is the large number in the block
   ;cup and collector 0 = coll A, cup1 data, 1 = coll A, cup2 data, 
   ;2 = coll B, cup 1 data, 3 = coll B cup 2 data
 
      counts[*,spin-1, 0, spec] = A12[cup1]
      counts[*,spin-1, 1, spec] = A12[cup2]
      counts[*,spin-1, 2, spec] = B12[cup1]
      counts[*,spin-1, 3, spec] = B12[cup2]

   ;spin_cnt is the firstbyte in each fcblock
      spin_cnt(spin-1) = (raw(0) and 31B)

   ;go to next spin and data set
      spin = spin +1
      raw = FCspectra[spec].fcblocks[spin, *] ;a single fcblock
  endwhile

;call a procedure that takes information out of the fcblock and

fcff, raw, fcspectra[spec].swemode, w_type=w_type, windows=windows, $
      lowest_ML=lowest_ML, tracking=tracking, scan=scan,  year=year, $
      day=day, sec=sec


; THE FOLLOWING WERE COMMENTED OUT BY STEVENS ON 
; DEC 12.  I DON'T KNOW THE EFFECT OF AVERTING THE STOPS!
if keyword_set(verbose) then print, w_type, windows, lowest_ML, tracking, scan
;if( scan gt 1 ) then stop
;if( w_type gt 2) then stop
;if(lowest_ML gt 63) then stop
  win_index = spin_cnt - 2 + (lowest_ML +1)/w_type

  test = where(spin_cnt eq 0)
  if ((n_elements(test) eq 1) and (test(0) eq -1))then noreason = 0$
     else win_index(test)=0

 w_type_arr [spec]  = w_type
 windows_arr [spec]  = windows
 tracking_arr [spec] = tracking
 scan_arr[spec]  = scan
 year_arr[spec]  = year
 day_arr[spec]  = day
 sec_arr[spec]  = sec
 spin_cnt_arr[*, spec] = spin_cnt
 win_index_arr[spec, *] = win_index

;cup1_vel(*, spec) = velocity1(w_type_arr(spec)-1,0,win_index_arr(spec,*))
;cup2_vel(*, spec) = velocity2(w_type_arr(spec)-1,0,win_index_arr(spec,*))

;cup1_EperQ(*, spec) = EperQ1(w_type_arr(spec)-1,0,win_index_arr(spec,*))
;cup2_EperQ(*, spec) = EperQ2(w_type_arr(spec)-1,0,win_index_arr(spec,*))

endfor

for spec = 0, specmax do cup1_vel(*, spec) = velocity1(w_type_arr(spec)-1,0,win_index_arr(spec,*))
for spec = 0, specmax do cup2_vel(*, spec) = velocity2(w_type_arr(spec)-1,0,win_index_arr(spec,*))

for spec = 0, specmax do cup1_EperQ(*, spec) = EperQ1(w_type_arr(spec)-1,0,win_index_arr(spec,*))
for spec = 0, specmax do cup2_EperQ(*, spec) = EperQ2(w_type_arr(spec)-1,0,win_index_arr(spec,*))

for spec = 0, specmax do cup1_vdel(*, spec) = velocity1(w_type_arr(spec)-1,1,win_index_arr(spec,*))
for spec = 0, specmax do cup2_vdel(*, spec) = velocity2(w_type_arr(spec)-1,1,win_index_arr(spec,*)) 

for spec = 0, specmax do cup1_EQdel(*, spec) = EperQ1(w_type_arr(spec)-1,1,win_index_arr(spec,*))
for spec = 0, specmax do cup2_EQdel(*, spec) = EperQ2(w_type_arr(spec)-1,1,win_index_arr(spec,*))

end



