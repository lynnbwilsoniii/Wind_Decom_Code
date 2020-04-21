FUNCTION count_rn,c_file,prt=prt,exact=exact
;get number of exact number of records 
IF NOT keyword_set(prt) then prt=0
if (prt) then $                        
  print,'Counting records in file: ',c_file ;say hello
IF keyword_set(exact) THEN BEGIN
    if not keyword_set(prt) then prt=0
    openr,c_unit,c_file,/get_lun ;open the file
    rec=bytarr(11552)           ;define file record
    readu,c_unit,rec            ;read in the header
    rn=0                        ;file header = record 0
    
    REPEAT BEGIN                ;repeat
        rn=rn+1                 ;increment record counter
        readu,c_unit,rec        ;read in the rn-th record
    ENDREP UNTIL (eof(c_unit))  ;until the end of the file
    
    free_lun,c_unit             ;close the file
ENDIF ELSE BEGIN
    spawn,'ls -s '+c_file,ls_ans,/sh
    reads,ls_ans,kbytes             
    rn=fix(kbytes*1024./11552.)-4
ENDELSE
if (prt) then print,strtrim(rn,1),' records.' ;say goodbye
return,rn                                      ;return the number of records
end
