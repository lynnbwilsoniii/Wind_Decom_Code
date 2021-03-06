; JTS 4/15/99
;
; Routine to generate spec files from LZ files.
;
; This routine calls swelzfc.pro to read in data from LZ file.
;
;  To use pickfile to choose a level zero file:
;  IDL> make_spec_file
;
;  To identify the level zero file in the command line: 
;  IDL> make_spec_file, lzfile = '/nfs/oersted/d3/wind/lz_files/WI_LZ_SWE_19990413_V01.DAT'
;
;  An idl restore file is automatically saved to $windDATA/spec_files 
;

pro make_spec_file, lzfile = lzfile

if(not keyword_set(lzfile)) then begin
    lzpath =  '/crater/observatories/wind/swe/lz/'
    lzfile=pickfile(/read,path=lzpath(0),$
                    filter='*',$
                    title='Level Zero Data Files')
endif

; get fc block data from lz file
;
swelzfc, fcspectra, infile = lzfile 
;

;** Make up a filename for saving the spec_file
nn   = n_elements(fcspectra)
time = fcspectra(nn/2).majfrmtim
year = long(time/1000.)
day  = long(time - year*1000.)
;** cal ~windd/idl/doy2cal.pro routine ***
doy2cal,year,day,month_string,cday,cmonth
monthnum = strcompress( string(cmonth),/remove )
if( cmonth le 9) then monthnum = '0'+strcompress(string(cmonth),/remove)
daynum = strcompress( string(cday) , /remove)
if( cday le 9) then daynum = '0'+strcompress(string(cday),/remove)
specfilename =  '/crater/observatories/wind/ionspec/'+$
   'ionspec'+ $ 
    strcompress(string(year),/remove) + monthnum + daynum +'.idl'

;*** file is saved to $windDATA/spec_files
save, fcspectra, filename = specfilename

end
