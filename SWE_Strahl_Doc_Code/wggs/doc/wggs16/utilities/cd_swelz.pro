pro cd_swelz

start:
print,'Enter date as long integer yymmdd'
indate='' & read,indate  
if strmid(indate,0,1) ne '9' or strlen(indate) ne 6 then stop,'bad date format' 
print,'indate ',indate

openr,lun,getenv('WILZCDINDEX')+'/index.txt',/get_lun

s1=''
s2=''

while not eof(lun) do begin
  readf,lun,s1
  readf,lun,s2
  s=strcompress(s1,/remove_all)
  sc_inv_dtyp=strmid(s,0,9)
  if sc_inv_dtyp eq 'WI_LZ_SWE' then begin
    date=strmid(s,12,6)
    if long(date) eq long(indate) then begin
      seqno=long(strmid(s,50,4))
      free_lun,lun
      print,'CD-ROM sequence number ',seqno
      print,' '
      goto,endproc
    endif
  endif
endwhile

print,'Given date not found in index'
free_lun,lun

endproc:

goto,start

end

