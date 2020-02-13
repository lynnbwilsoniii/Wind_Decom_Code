pro test_all_elecs,scimode,ispin,elemode
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

elemode=0
if scimode eq 1 or scimode eq 4 or scimode eq 6 then begin
  if vsmjf.eleion(0) eq 0 then elemode=1
endif else if scimode eq 2 then begin
  if total(vsmjf.eleion(*,ispin)) eq 0 then elemode=1
endif 

;if elemode eq 1 then print,'all electron mode' $
;else  print,'NOT all electron mode'

if elemode eq 0 then print,'NOT all electron mode' 
   
end