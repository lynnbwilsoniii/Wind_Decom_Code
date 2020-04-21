

FUNCTION mdy2doy, mm, dd, yyyy
return, JULDAY(mm, dd, yyyy) - JULDAY(1, 0, yyyy) 
END

FUNCTION si, str, i, j
form = '(I' + strtrim(string(i), 2) + '.' + $
  strtrim(string(j), 2) + ')'
return, string(str, format = form)
END

PRO convert_wind3secmag_cdf2idl


; Location of files
CDFPATH = '/crater/observatories/wind/mfi/mfi_h0/'
SAVEPATH= '/crater/observatories/wind/mfi/hires_idl/'

; Get listing of the files
spawn, 'ls ' + CDFPATH + '/*/wi_h0_mfi_**_v05.cdf', fl_cdf

n_files = n_elements( fl_cdf )
print, 'Found ', n_files, ' CDF files.'
yr_cdf = double(strmid(fl_cdf,39,4))
month_cdf = double(strmid(fl_cdf,58,2))
day_cdf = double(strmid(fl_cdf,60,2))

doy_cdf = DINDGEN( n_files )
FOR i=0,n_files-1 DO doy_cdf[i] = mdy2doy(month_cdf[i], day_cdf[i], $
	yr_cdf[i] )

; Get minimum and maximum years
yr_start = MIN(yr_cdf)
;yr_start = 2003
yr_end   = MAX(yr_cdf)
print, 'CDF files cover '+si(yr_start,4,4)+' to '+ $
       si(yr_end,4,4)

; length of each file
spawn, 'ls -al ' + CDFPATH + '/*/*.cdf', fl_length
length = double(strmid(fl_length,31,8))

; Loop through years
FOR i_year = yr_start, yr_end DO BEGIN 

    print, 'Starting year: ' + si(i_year,4,4)

    ; Loop through 20 day intervals
    FOR i_doy = 0,18 DO BEGIN 

        doy_start = i_doy*20
        doy_end   = (i_doy + 1)*20

        print, '   Scanning for files from ' + si(i_year,4,4) + $
               ' DOY [' + si(doy_start,3,3) + ' - ' + $
               si(doy_end,3,3) + ']'

        tk_files = WHERE( (yr_cdf EQ i_year) AND (doy_cdf GE doy_start) AND $
                          (doy_cdf LT doy_end) AND (length GT 5d5), ntk )

        IF ntk GT 0 THEN BEGIN

            print, '   Found ' + si(ntk,2,2) + ' files in this interval'

            FIRST = 1

            FOR i_file = 0,ntk-1 DO BEGIN 

                                ; Open a CDF
                print, '      ' + fl_cdf[tk_files[i_file]]

                id = CDF_OPEN( fl_cdf[tk_files[i_file]] )
                
                cdf_control,id,var='Epoch3',get_var_info=info,get_filename=fn
                nrec=info.maxrec + 1L

                IF nrec GT 0 THEN BEGIN

                    cdf_varget, id, 20, Epoch, REC_COUNT=nrec, /ZVAR
                    cdf_varget, id, 27, B3GSE, REC_COUNT=nrec, /ZVAR
                    
                    
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

                                ; End loop through years
ENDFOR


END

