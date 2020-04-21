
; MAJOR REVISIONS:
; 
; 02/12/2014, MLS
;     I revised this to include h0 data as far as possible, and then
;     to temporarily extend using kp data. I have also accounted for
;     the possibility of multiple file versions, using only the
;     highest version available.
;
; note-- there is a new-year bug in this code. If there are no H0
;        (CDF) files available for the present year, we'll
;        crash at line 54 and fail to run the key parameter contingency
; 



FUNCTION mdy2doy, mm, dd, yyyy
return, JULDAY(mm, dd, yyyy) - JULDAY(1, 0, yyyy) 
END

FUNCTION si, str, i, j
form = '(I' + strtrim(string(i), 2) + '.' + $
  strtrim(string(j), 2) + ')'
return, string(str, format = form)
END


PRO update_wind3secmag_cdf2idl, yyyy=yyyy


; Location of files
CDFPATH = '/crater/observatories/wind/mfi/mfi_h0/'
KPPATH =  '/crater/observatories/wind/mfi/mfi_k0/'
SAVEPATH= '/crater/observatories/wind/mfi/hires_idl/'

; choose present year for file update

if not keyword_set(yyyy) then begin
    yyyy = strmid(systime(), 3, 4, /reverse)
    i_year = float(yyyy)
endif else begin
  yyyy=string(yyyy, format = '(I4)')
  i_year = fix(yyyy)
endelse

spawn, 'ls ' + CDFPATH + '/'+yyyy+'/wi_h0_mfi_**_v0*.cdf', fl_cdf

if (fl_cdf[0] eq '' and n_elements(fl_cdf) eq 1) then noH0 = 1 else noH0 = 0

if noH0 eq 0 then begin 
   n_files = n_elements( fl_cdf )
   print, 'Found ', n_files, ' CDF files.'
   yr_cdf = double(strmid(fl_cdf,39,4))
   month_cdf = double(strmid(fl_cdf,58,2))
   day_cdf = double(strmid(fl_cdf,60,2))

   spawn, 'ls -al ' + CDFPATH + '/'+yyyy+'/wi_h0_mfi_**_v0*.cdf', listing
   length_cdf = double(strmid(listing,32,7))

   julday_cdf = julday(month_cdf, day_cdf, yr_cdf)
   doy_cdf = julday_cdf - julday(1, 0, yr_cdf)

; get rid of obsolete versions
   tk = uniq(doy_cdf)           ; the uniq function is sneaky useful here 
; because it returns the index of the LAST element when an entry is
; repeated in an array. Since the file list is sorted, that means it
; returns the index corresponding to the latest version
   fl_cdf = fl_cdf[tk]
   yr_cdf = yr_cdf[tk]
   month_cdf = month_cdf[tk]
   day_cdf = day_cdf[tk]
   doy_cdf = doy_cdf[tk]
   length_cdf = length_cdf[tk]
endif

; also check for key parameter files that extend our time range:
spawn, 'ls ' + KPPATH + '/'+yyyy+'/wi_k0_mfi_**_v0*.cdf', fl_KP
if fl_kp[0] ne '' then begin
    yr_kp = double(strmid(fl_kp,39,4))
    month_kp = double(strmid(fl_kp,58,2))
    day_kp = double(strmid(fl_kp,60,2))

    spawn, 'ls -al ' + KPPATH + '/'+yyyy+'/wi_k0_mfi_**_v0*.cdf', listing
    length_kp = double(strmid(listing,32,7))

; again, get rid of the obsolete versions
    julday_kp = julday(month_kp, day_kp, yr_kp)
    doy_kp = julday_kp - julday(1, 0, yr_kp)

    tk = uniq(doy_kp)       ; the uniq function is sneaky useful here 
; because it returns the index of the LAST element when an entry is
; repeated in an array. Since the file list is sorted, that means it
; returns the index corresponding to the latest version
    fl_kp = fl_kp[tk]
    yr_kp = yr_kp[tk]
    month_kp = month_kp[tk]
    day_kp = day_kp[tk]
    doy_kp = doy_kp[tk]
    length_kp = length_kp[tk]

; merge the h0 and kp file lists (just use the KP list if no h0)
if noH0 eq 0 then begin
    maxdoy_h0 = max(doy_cdf)
    kp_indices = where(doy_kp gt maxdoy_h0, nkp)
    if nkp gt 0 then begin
        fl_cdf = [fl_cdf, fl_kp[kp_indices]]
        yr_cdf = [yr_cdf, yr_kp[kp_indices]]
        month_cdf = [month_cdf, month_kp[kp_indices]]
        day_cdf = [day_cdf, day_kp[kp_indices]]
        doy_cdf = [doy_cdf, doy_kp[kp_indices]]
        length_cdf = [length_cdf, length_kp[kp_indices]]
    endif
 endif else begin
    kp_indices = where(doy_kp gt 0, nkp)
    if nkp gt 0 then begin
        fl_cdf = fl_kp[kp_indices]
        yr_cdf = yr_kp[kp_indices]
        month_cdf = month_kp[kp_indices]
        day_cdf = day_kp[kp_indices]
        doy_cdf = doy_kp[kp_indices]
        length_cdf = length_kp[kp_indices]
     endif
 endelse
endif

n_files = n_elements(fl_cdf)
if n_files eq 1 and fl_cdf[0] eq '' then return


FOR i=0,n_files-1 DO doy_cdf[i] = mdy2doy(month_cdf[i], day_cdf[i], $
	yr_cdf[i] )

    ; Loop through 20 day intervals
    FOR i_doy = 0,18 DO BEGIN 

        doy_start = i_doy*20
        doy_end   = (i_doy + 1)*20

        print, '   Scanning for files from ' + si(i_year,4,4) + $
               ' DOY [' + si(doy_start,3,3) + ' - ' + $
               si(doy_end,3,3) + ']'

        tk_files = WHERE( (yr_cdf EQ i_year) AND (doy_cdf GE doy_start) AND $
                          (doy_cdf LT doy_end) AND (length_cdf GT 1d5), ntk )

        IF ntk GT 0 THEN BEGIN

            print, '   Found ' + si(ntk,2,2) + ' files in this interval'

            FIRST = 1

            FOR i_file = 0,ntk-1 DO BEGIN 

                                ; Open a CDF
                print, '      ' + fl_cdf[tk_files[i_file]]
                is_k0file = strpos(fl_cdf[tk_files[i_file]], '_k0_') gt 0

                id = CDF_OPEN( fl_cdf[tk_files[i_file]] )
                
                                ; make a check here to read the k0 and
                                ; the h0 files differently. h0 files
                                ; contain several different time
                                ; resolutions, marked 3 seconds with
                                ; the 3 appended, whereas k0 files
                                ; have only one resolution
                if is_k0file then cdf_control,id,var='Epoch',get_var_info=info,get_filename=fn $
                else cdf_control,id,var='Epoch3',get_var_info=info,get_filename=fn
                nrec=info.maxrec + 1L

                IF nrec GT 0 THEN BEGIN

                    if is_k0file then cdf_varget, id, 0, Epoch, REC_COUNT=nrec, /ZVAR $
                      else cdf_varget, id, 20, Epoch, REC_COUNT=nrec, /ZVAR
                    if is_k0file then cdf_varget, id, 8, B3GSE, REC_COUNT=nrec, /ZVAR $
                      else cdf_varget, id, 27, B3GSE, REC_COUNT=nrec, /ZVAR
                    
                    newDOYMAG = DINDGEN(NREC)
                    
                    FOR i=0,NREC-1 DO BEGIN
                        
                        CDF_EPOCH, epoch[0,i], year, month, day, hour, $
                          minute, second, milli, /BREAKDOWN_EPOCH

                        newDOYMAG[i] = DOUBLE(mdy2doy(month,day,year)) + $
                          DOUBLE(hour)/24.d + DOUBLE(minute)/1440.0d + $
                          DOUBLE(second)/86400.0d + DOUBLE(milli)/86400000.d

                        
                    ENDFOR
                                ; Close the CDF
                    CDF_CLOSE,  id
                    
                    IF FIRST THEN BEGIN 
                        FIRST = 0 
                        
                        DOYMAG = newDOYMAG
                        
                        BXMAG = FLOAT(REFORM(B3GSE[0,*]))
                        BYMAG = FLOAT(REFORM(B3GSE[1,*]))
                        BZMAG = FLOAT(REFORM(B3GSE[2,*]))
                        
                    ENDIF ELSE BEGIN
                        
                        DOYMAG = [DOYMAG, newDOYMAG]
                        
                        BXMAG = [BXMAG, FLOAT(REFORM(B3GSE[0,*]))]
                        BYMAG = [BYMAG, FLOAT(REFORM(B3GSE[1,*]))]
                        BZMAG = [BZMAG, FLOAT(REFORM(B3GSE[2,*]))]
                        
                    ENDELSE
                    
                ENDIF
                
            ENDFOR
            
            npts = N_ELEMENTS(DOYMAG)
            print, '   Loaded in ' + si(npts,8,8) + ' measurements'
            
            tk_good = WHERE( (BXMAG GT -3000.0) AND (BYMAG GT -3000.0) $
                             AND (BZMAG GT -3000.0), ntk_good )
            
            IF ntk_good GT 0 THEN BEGIN
                
                print, '   There are '+si(ntk_good,8,8) + ' good measurements'
                
                DOYMAG  = DOYMAG[tk_good]
                BXMAG   = BXMAG[tk_good]
                BYMAG   = BYMAG[tk_good]
                BZMAG   = BZMAG[tk_good]
                
                fsave = 'wind_mag.' + si(i_year,4,4) + '.' + $
                  si(doy_start,3,3) + '.' + si(doy_end,3,3) + '.idl'
                
                print, '   Writing data to ' + fsave
                
                
                save, DOYMAG, BXMAG, BYMAG, BZMAG, FILE=SAVEPATH+fsave
                
            ENDIF ELSE BEGIN
                
                print, '   No good measurements in this interval'

            ENDELSE

        ENDIF ELSE BEGIN
            
            print, '   No files in this interval'
            
        ENDELSE
        
        
                                ; End loop through days
    ENDFOR




END

