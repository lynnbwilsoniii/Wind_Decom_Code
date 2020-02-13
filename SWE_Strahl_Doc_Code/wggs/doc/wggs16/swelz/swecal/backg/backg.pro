;----- procedure to write a file associating each date with a
;------glint background counts file

pro bckdates

;creates and updates a table associating an existing background counts file
;  with each day from nov 30 1994 to present

;the association is done by finding the date of background data 
;  which is closest to the date of lz data

datfiles_m1=findfile(getenv('BACKGPATH')+'*_m1_backg.dat')
datfiles_m2=findfile(getenv('BACKGPATH')+'*_m2_backg.dat')

datdates_m1=strmid(datfiles_m1,strlen(getenv('BACKGPATH')),8)
datdates_m2=strmid(datfiles_m2,strlen(getenv('BACKGPATH')),8)

dlast=long('20051231')
;from launch to daylast:
 
day=[334+indgen(365-333),    1+indgen(365),    1+indgen(366),$
           1+indgen(365),    1+indgen(365),    1+indgen(365),$
           1+indgen(366),    1+indgen(365),    1+indgen(365),$
           1+indgen(365),    1+indgen(366),    1+indgen(365)]
year=[  replicate(1994,32), replicate(1995,365),replicate(1996,366),$
        replicate(1997,365),replicate(1998,365),replicate(1999,365),$
        replicate(2000,366),replicate(2001,365),replicate(2002,365),$
        replicate(2003,365),replicate(2004,366),replicate(2005,365)]

ymdy=strarr(n_elements(day))
bfile_m1=strarr(n_elements(day))
bfile_m2=strarr(n_elements(day))

for i=0,n_elements(day)-1 do begin
  mdy=yrmoda([year(i),day(i),0])

  ymdy(i)=strmid(mdy,strlen(mdy)-4,4)+strmid(mdy,0,2)+strmid(mdy,3,2)

  ;print,year(i),day(i),mdy,ymdy(i),format='(i2,i5,a12,a12)'

  d1=long('19950127') 
  d2=long('19950510')  ;ebias2 level changed from hex 52 to 55 on 950511
  d3=long('19950531')
  if long(ymdy(i)) le d1 then $
    bfile_m1(i)='19941214' $
  else if long(ymdy(i)) gt d1 and long(ymdy(i)) le d2 then $
    bfile_m1(i)='19950517a' $
  else if long(ymdy(i)) gt d2 and long(ymdy(i)) le d3 then $
    bfile_m1(i)='19950517b' $
  else if long(ymdy(i)) gt d3 and long(ymdy(i)) le dlast then begin
    mind=min(abs(long(ymdy(i))-long(datdates_m1)),imin)
    bfile_m1(i)=datdates_m1(imin(0))

    mind=min(abs(long(ymdy(i))-long(datdates_m2)),imin)
    bfile_m2(i)=datdates_m2(imin(0))
  endif

  
  ;print,'date,bdate ',ymdy(i),'   ',bfile_m1(i)
  
endfor

print,' '
for i=0,n_elements(day)-1 do print,ymdy(i),'  ',bfile_m1(i),'  ',bfile_m2(i)

;openw,lun,getenv('BACKGPATH')+'bdates_m1',/get_lun
;for i=0,n_elements(day)-1 do printf,lun,ymdy(i),'  ',bfile_m1(i) 
;free_lun,lun

;openw,lun,getenv('BACKGPATH')+'bdates_m2',/get_lun
;for i=0,n_elements(day)-1 do printf,lun,ymdy(i),'  ',bfile_m2(i) 
;free_lun,lun

end


;------------ main program ---------------------------------------------

;-----restores an idlsave file and writes a new binary file containing just the 
;-----background counts array avgcts(ndets,nsteps,nsects)

;-----computes ".dat" files only for those ".idlsav" files that do not already 
;-----have ".dat" file with the same date identifier



idlsavfiles=findfile(getenv('BACKGPATH')+'*.idlsav')
if idlsavfiles eq '' then goto,point1

for i=0,n_elements(idlsavfiles)-1 do print,'idlsavfiles ',idlsavfiles(i)
datfiles=findfile(getenv('BACKGPATH')+'*.dat')
for i=0,n_elements(datfiles) -1 do $
  print,'datfiles ',datfiles(i),$
    '   ',strmid(datfiles(i),strlen(getenv('BACKGPATH')),8)


newfiles=strarr(n_elements(idlsavfiles))
inew=-1
for i=0,n_elements(idlsavfiles)-1 do begin
  w=where(strmid(idlsavfiles(i),strlen(getenv('BACKGPATH')),8) eq $
          strmid(datfiles,strlen(getenv('BACKGPATH')),8))
  if w(0) eq -1 then begin
    print,' ' & print,'idlsavfiles(i) ',$
       idlsavfiles(i),strmid(idlsavfiles(i),strlen(getenv('BACKGPATH')),8)
    
    if strmid(idlsavfiles(i),strlen(getenv('BACKGPATH'))+8,3) eq '_m2' $
    then modid='_m2' else modid='_m1'

    restore,idlsavfiles(i)
    help,avgcts,stdcts
    datfn=getenv('BACKGPATH')+$
      strmid(idlsavfiles(i),strlen(getenv('BACKGPATH')),8)+modid+'_backg.dat'
    print,'Do you want to create background .dat file ',datfn,' ? (y/n)'
    answ='' & read,answ 
    if answ eq 'y' then begin
      inew=inew+1
      newfiles(inew)=datfn
      openw,lun,datfn,/get_lun
      writeu,lun,avgcts
      writeu,lun,stdcts
      free_lun,lun
      print,'data file created: ',datfn 
    endif
  endif else print,strmid(idlsavfiles(i),strlen(getenv('BACKGPATH')),8),$
    '    ".dat" file exists....no new file created'
endfor
point1:
print,'update bdates file containing dates and background files'
print,'hit return to continue'
answ='' & read,answ & if answ ne '' then stop
print,'running pro bckdates'
bckdates

print,' '
print,' ' & print,'newly created files are: '
for i=0,inew do print,newfiles(i)


 
end    


