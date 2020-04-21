PRO recdnum,file=file,num=num,fc=fc,lpr=lpr
;temp lines marked ;temp#
;uncomment temp# lines to preform operation #
;operations:
;1	will print fcblocks from fcfast format file, 
;	choose appropriate print statement
;	55 = # of fcblocks needed to preform analysis (27 data, 27 fill, 1 fcff)
;	currently set to copy only the first 55 blocks.

;2	copy selected lz major frames (records) to another file

common counter,count

if not keyword_set(lpr) then lpr=0

if keyword_set(file) then countfile=file $
else countfile='lz/lzsimholder.dat'
if not keyword_set(num) then num=11522
if keyword_set(fc) then num=130

count=0
print,'File: ',countfile
openr,luncount,countfile,/get_lun
record=bytarr(num)
;readu,luncount,record

;openw,templun,'block1',/get_lun					    ;temp1
;openw,templun,'lzsimholder2.dat',/get_lun				    ;temp2
while not eof(luncount) do begin
	count=count+1
	if lpr then print,count
	readu,luncount,record
;	if count lt  56 then printf,templun,record(4:125),format='(2z3.2)'  ;temp1
;	if count lt  56 then writeu,templun,record			    ;temp1
;	if count lt 110 then writeu,templun,record			    ;temp2
;	if count eq 110 then free_lun,templun				    ;temp2
endwhile
;free_lun,templun							    ;temp1,2

print,'Number of records: ',long(count) 
free_lun,luncount
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO fixrecdnum,file=file,cnt=cnt

common counter,count
if not keyword_set(count) then recdnum,file=file else count=cnt

openr,luncount,file,/get_lun
record=bytarr(11552)

fixfile=string(file,'.fix')
print,'Repaired file: ',fixfile
openw,lunfix,fixfile,/get_lun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;define file header
ef = {editfile, nm:'01234567890123456789012345678901234567890123', $
      ek:'EDITXXYYYYDDDHHMMSSVV   ', ern:'RERN', epvn:'EPVNEPVN', $
      erdt:'YYYYDDDHHMMSSUUU', dt:'DTDT', mk:'0123456789012345678901234567'}
dt = {time, yr:0l, dy:0l, ms:0l, us:0l}
fh = {filehd, spcid:0l, intsn:0l, instrnm:'NAME', phyrc:0l, phyrcmf:0l, $
      nphyrc:0l, mfcfst:0l, mfclst:0l, sccfst:bytarr(8), scclst:bytarr(8), $
      fst:replicate(dt,1), lst:replicate(dt,1), $
      nmfe:0l, nmf:0l, ngap:0l, dct:'TYPE', drn:0l, $
      dpvn:'DPVNDPVN', ddvn:'DDVNDDVN', drdt:'YYYYDDDHHMMSSUUU', $
      ifn:'01234567890123456789012345678901234567890123',rln:0l, $
      spares:lonarr(5), $
      mrn:0l, mpvn:'MPVNMPVN', mpdt:'YYYYDDDHHMMSSUUU',nef:0l, $
      efs:replicate(ef,20)}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end file header definition
readu,luncount,fh
;help,count,fh.nphyrc,(conv_vax_unix(fh.nphyrc)),(conv_vax_unix(long(count)))
print,'Old header count:  ',(conv_vax_unix(fh.nphyrc))
fh.nphyrc=(conv_vax_unix(long(count)))
print,'New header count:  ',(conv_vax_unix(fh.nphyrc))
writeu,lunfix,fh

for i=1,(count-1) do begin
	if not eof(luncount) then begin
		readu,luncount,record
		writeu,lunfix,record
	endif
endfor

free_lun,luncount
free_lun,lunfix
spawn,'"rm" '+file
spawn,'mv '+fixfile+' '+file
end
