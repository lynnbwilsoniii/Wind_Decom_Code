; @(#)prt_hdr.pro  VERSION 1.2    7/28/94   16:13:30
pro prt_hdr,lun,lz,h,m,s,infile

if lun eq 0 then begin
    openw,lun2,'hdr_data.prt',/get_lun
    print,' '
    if n_params() eq 5 then infile = ' '
    printf,lun2,infile
    print,'mjf header: lz.recn ',lz.recn,format='(a20,i4)'
    printf,lun2,'mjf header: lz.recn ',lz.recn,format='(a20,i4)'
    print, 'yr,dy,msdy,hhmmss.ms ',lz.yr,lz.dy,lz.ms,h,':',m,':',s,$
       format='(a21,i5,i4,i9,2x,i2,a1,i2,a1,f6.3)'
    printf,lun2, 'yr,dy,msdy,hhmmss.ms ',lz.yr,lz.dy,lz.ms,h,':',m,':',s,$
       format='(a21,i5,i4,i9,2x,i2,a1,i2,a1,f6.3)'
    help,lz,/str  
	free_lun,lun2
endif else begin
endelse
end
