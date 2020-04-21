; @(#)lzcopy.pro  VERSION 1.3    7/29/94   19:29:23

PRO lzgetfc,infile,outfile,fortran=fortran
;will produce ascii file of fc blocks unless fortran keyword is set:
;IDL> lzgetfc,'lzinputfile','outputfile'	(text output)
;IDL> lzgetfc,'lzinputfile','outputfile',/for	(fortran unformatted output)

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


openr,lundat,infile,/get_lun
print, ' ' & print,'Input data file: ',infile

;open outfile.  MAY WISH TO CHANGE FILE NAME HERE 
if not keyword_set(fortran) then BEGIN
  print,'Text output file: ',outfile
  fortran = 0
  openw,lunout,outfile,/get_lun
  printf,lunout,'Faraday cup data file:  ',outfile
  printf,lunout,'Source file:            ',infile
  spawn,'date',result
  printf,lunout,'Created by lzgetfc.pro:',result
  printf,lunout,' '
endif else BEGIN
  print,'Unformatted output file: ',outfile
  openw,lunout,outfile,/get_lun,/F77_UNFORMATTED
endelse

;read file header
hdr=bytarr(11552)
readu,lundat,hdr

output=bytarr(122)

repeat BEGIN
  readu,lundat,mjf

  if 1 then BEGIN  
    s=size(fcblm1)
    for j=0,s(1)-1 do BEGIN

      fcoff=scindx(fcblm1(j).offs+indgen(fcblm1(j).ln))

      ;write fc data
      for k=0,fcblm1(j).ln-1 do BEGIN
        offset=scindx(fcblm1(j).offs+k)
        output(k)=mjf.data(offset)
      ENDFOR

      IF (fortran) then writeu,lunout,output(0:121) $
      ELSE BEGIN
	printf,lunout,'faraday cup  block # ',j
	printf,lunout,output(0:121),format='(6(20z3.2,/),2z3.2)'
      ENDELSE

    ENDFOR 
  ENDIF 
ENDREP until eof(lundat)
 
free_lun,lundat
free_lun,lunout
end
