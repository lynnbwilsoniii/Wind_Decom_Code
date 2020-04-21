; @(#)lzcopy.pro  VERSION 1.3    7/29/94   19:29:23
;The program is lzcopy.pro and uses some of the same procedures as the display
;procedure, swelz.pro. Just put lzcopy.pro in the same directory as swelz.pro.
;Edit the infile and outfile directories and filenames. Right now the
;directory used is for my machine. Put the new lz file in the same directory as 
;the others and use swelz to look at it. There is a print option, lpr=1 for 
;printout. The number of records to be read and copied is nrec. 
;New fc data should be  inserted as byte data type. Here is the program. It's
;set up right now for a test with nrec=10 and the new fc data byte =171b =ab(hex)
;for all fc data values.
;Good luck!
;
;Regards,
;
;Dick
;
;
;============================================================================
;
;  A program to read and copy faraday cup data into lz record
;  R. J. Fitzenreiter April, 1994
;
;============================================================================
;  Modification history: (FVM)
;	Modified to read a file of unformatted fortran records of fc blocks
;	  (a simfile) to insert into a lz file to test key param software.
;	  File is truncated after all simulated fc blocks are inserted (once).
;	Modified to approximate the number of records in the simfile and to
;	  read in all of the records at once
;	Modified to output (some) major frames in hex if flag hex=1.
;	Modified to repeat stuffing of simulated fc blocks into the lz file
;	  until entire lz file is full.
;	Modified to look at the telemetry mode, and skip records with obvious
;	  telemetry dropouts.
;	Modified to skip stuffing of f0ff blocks if tmrate_ihk = slow
;	Record number may be off by one

;define data record structure
mjf={junk,rechdr:bytarr(300),data:bytarr(11552-300)}

;get indices, scindx, of science data, 
;i.e., the indices of the mjf array, scidat, without the instr hk
  ind_scidat,scindx 

;get mode1 tm map of science and genl hk data offsets into scindx
  mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  ;hkm1=genl hk offsets for mode1
                                         ;fcblm1=faraday cup offsets
                                         ;vsm1=veis/strl offsets
                                         ;vdatc=veis data index
                                         ;sdatc=strl data index


;open level zero file.  MAY WISH TO CHANGE FILE NAME HERE
;infile='/plasma/d3/wind/lz_files/pre_launch/wi_lz_swe_19930902_v02.dat'
infile='/plasma/d3/wind/lz_files/pre_launch/wi_lz_swe_19930902_v02.dat'
openr,lundat,infile,/get_lun
print, ' ' & print,'input data file name ',infile

;open outfile.  MAY WISH TO CHANGE FILE NAME HERE 
;outfile='~wind/source/analysis/fvm/idl_shell/fcfast2lz.lz'
outfile='lzsimholder.dat'
openw,lunout,outfile,/get_lun

;read and copy file header
hdr=bytarr(11552)
readu,lundat,hdr
writeu,lunout,hdr

;for i=1,40 do readu, lundat, mjf

while not eof(lundat) do begin
	readu,lundat,mjf
	writeu,lunout,mjf
endwhile 

free_lun,lundat
free_lun,lunout
end
