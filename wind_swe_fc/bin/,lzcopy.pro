From u3rjf@leprjf.gsfc.nasa.gov Thu Apr 28 11:02:00 1994
Date: Thu, 28 Apr 94 11:05:41 EDT
From: u3rjf@leprjf.gsfc.nasa.gov (Dick Fitzenreiter)
To: ajl
Subject: Re: writing an lz file from idl
Content-Length: 3252
Status: RO
X-Lines: 99


Al,

After a number of false starts, I now have a simple idl program to copy
new data bytes back into the faraday cup blocks and write a new lz file.
It's a simple binary read, insert, and write. There is no vax to unix conversion
done as in the display procedure, so there is no need to do the inverse. 
This only works with byte data words.

The program is lzcopy.pro and uses some of the same procedures as the display
procedure, swelz.pro. Just put lzcopy.pro in the same directory as swelz.pro.
Edit the infile and outfile directories and filenames. Right now the
directory used is for my machine. Put the new lz file in the same directory as 
the others and use swelz to look at it. There is a print option, lpr=1 for 
printout. The number of records to be read and copied is nrec. 
New fc data should be  inserted as byte data type. Here is the program. It's
set up right now for a test with nrec=10 and the new fc data byte =171b =ab(hex)
for all fc data values.
Good luck!

Regards,

Dick

;
;============================================================================
;
;  A program to read and copy faraday cup data into lz record
;
;  R. J. Fitzenreiter April, 1994
;
;============================================================================

;define data record structure
mjf={junk,rechdr:bytarr(300),data:bytarr(11552-300)}

;get indices, scindx, of science data, 
;i.e., the indices of the mjf array, scidat,  without the instr hk
  ind_scidat,scindx 

;get mode1 tm map of science and genl hk data offsets into scindx
  mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  ;hkm1=genl hk offsets for mode1
                                         ;fcblm1=faraday cup offsets
                                         ;vsm1=veis/strl offsets
                                         ;vdatc=veis data index
                                         ;sdatc=strl data index
                    
;open level zero file
infile='/data1/swe/lz/19930917_v02.dat'
openr,lundat,infile,/get_lun
print, ' ' & print,'input data file name',infile

;open outfile
outfile='/data1/swe/lz/lzoutfile.dat'
openw,lunw,outfile,/get_lun

;read and copy file header
hdr=bytarr(11552)
readu,lundat,hdr
writeu,lunw,hdr

lpr=0 ;print option

;read and copy nrec data records
nrec=10
for i=0,nrec-1 do begin
  readu,lundat,mjf  ;read
  
  if lpr then prt_fc,0,fcblm1,mjf.data(scindx)  ;print original faraday cup data

  ;put new values into fc bytes
    s=size(fcblm1)
    for j=0,s(1)-1 do begin
      if lpr then print,'faraday cup block # ',j
      if lpr then print,'mjf.data byte offset of fc data in block # j'
      fcoff=scindx(fcblm1(j).offs+indgen(fcblm1(j).ln))
      if lpr then print,fcoff,format='(10i6)'
      if lpr then print,'original fc data'
      if lpr then print,mjf.data(fcoff),format='(20z3)'
      ;insert new fc data
      for k=0,fcblm1(j).ln-1 do begin
        offset=scindx(fcblm1(j).offs+k)
        mjf.data(offset)=171b  ;a test, 171=ab(hex)
        ;print,offset,mjf.data(offset),format='(i6,z3)'
      endfor
    endfor

    if lpr then print,'new fc data'
    if lpr then prt_fc,0,fcblm1,mjf.data(scindx)  ;print new faraday cup data
    writeu,lunw,mjf ;copy back into lz data output file   
  
endfor

free_lun,lundat
free_lun,lunw

end



