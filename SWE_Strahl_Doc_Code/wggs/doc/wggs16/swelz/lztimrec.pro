function lztimrec,timpb5

;given time (pb5), find lz record number with closest header time

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common swestuff

;get begin and end times and number of records from file header

print,lzfile
;print,fh.fst,fh.lst,fh.nmf
timpb5fst=[fh.fst.yr,fh.fst.dy,fh.fst.ms]
timpb5lst=[fh.lst.yr,fh.lst.dy,fh.lst.ms]

;get seconds from tjd epoch
secfst=pb5_sec(timpb5fst)
seclst=pb5_sec(timpb5lst)

;get records per second
;drec=float(fh.nmf-1)/(seclst-secfst)
drec=float(fh.nmf)/(seclst-secfst)

;get seconds corresponding to input timpb5
seconds=pb5_sec(timpb5)

if (swest.veis_on eq 1) then spins = vsmjf.n_spins $ ;            VEIS is ON.
else spins = vsmjf.n_strspects ;      If VEIS is off--use spectra, not spins.

lzrec = 1 + long(fix(drec*(seconds-secfst)))
swest.ispinbl = long(fix((drec*(seconds-secfst)-float(lzrec-1))*(spins)))

lcheck=0
if lcheck then begin
  print,'check'
  print,timpb5fst,secfst,timpb5lst,seclst
  print,' ' 
  print,secfst,sec_pb5(secfst),seclst,sec_pb5(seclst)
  print,drec
  print,timpb5,seconds
  print,lzrec
endif

return,lzrec

end


