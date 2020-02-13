pro get_3slzmag,timpb5,magfld

common wstuff,wst
common swestuff,swest
common magstuff,magfile,tpb5,bgse

;returns 3sec mag field to proc_fw


if swest.mag_3s_kp eq 1 then begin       ;get mfi 3sec mag field
  ; timpb5=given pb5 time (year, day of year, millisec of day)  lonarr(3)
  ; tpb5 = pb5 time at center of minute for given record recn   lonarr(1440,3)
  ; bgse = mag vector at 3 sec intervals in minute tpb5     fltarr(1440,3,20)

  ;test whether day of the given time is the same as "lzdate" 
  ;or is just before midnight on previous day
  ;if given time is the same as lzdate, then the correct magfile has already
  ;been read;  
  ;if given time is NOT the same as lzdate, then the appropriate
  ;magfile should be read to get the magnetic vector and then read again the
  ;magfile that agrees with lzdate..
  ;TEMPORARILY, we will not read the appropriate (previous day's) magfile
  ;but will use the first mag vector on the day corresponding to lzdate..
  ;the probable error will be to use mag data corresponding in time to 
  ;one or two spins later than the given time.

  if ymd(timpb5) eq wst.lzdate then begin 
    mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
      sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
  endif else begin
    wmag=where(wst.magdate eq wst.lzdate)
    if wmag(0) ne -1 then begin
      mindx=0 & sindx=0
      print,'given time and magdate do not agree'
      print,'ymd(given time), lzdate, magdate ',$
          ymd(timpb5), wst.lzdate, wst.magdate(wmag)
      answ='' & print,'Hit return to continue' &read,answ
      if answ ne '' then stop  
      ;print,$
      ; 'first 3sec interval on lzdate will be used: mindx,sindx',mindx,sindx
    endif
  endelse

  magfld=bgse(mindx,0:2,sindx)
  magtpb5=[tpb5(mindx,0),tpb5(mindx,1),$
        tpb5(mindx,2) - (30000l - 1500l - long(sindx)*3000)]
  print,' '
  print,'given time ',timpb5
  print,'mindx, sindx ',mindx, sindx
  print,'  mag time ',magtpb5
  print,'mag fld (gse) ',magfld
  if magfld(0) eq -1.e31 then begin
    magfld=[-1.,0.,0.]
    print,'fill mag data, substitute ',magfld
  endif

endif else if swest.mag_3s_kp eq 2 then begin       ;get mfi kp mag field 
         
  get_kpmag,timpb5,magfld,magkp_ph,magkp_th
    
endif else begin                                                     
      
  magfld=[-1.,0.,0.]
  print,'No mag data; using anti-solar direction instead'
  ;if getenv('LZNOMAG') eq '' then begin
  ;  print,'Hit return if you want to continue.'
  ;  answ='' & read,answ & if answ ne '' then stop
  ;endif  
endelse
   
   
end