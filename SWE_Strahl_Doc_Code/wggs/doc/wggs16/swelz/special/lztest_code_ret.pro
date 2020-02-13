pro lztest_code_ret,tmmode_ihk=tmmode_ihk,ntest2,retcode,repcode

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest

;test various lz quantities and sets flags:
;   retcode =1 means return to calling procedure for proc_fw
;   repcode =1 means to increment recn and do proc_rec again 

;initialize flags
  retcode=0
  repcode=0

;test tm mode
   if keyword_set(tmmode_ihk) eq 0 then begin
     retcode=1
     return
   endif  
   if tmmode_ihk ne 2 then begin
     print,' ' & print,'not in tm science mode'
     retcode=1
     return
   endif

;test whether in background test mode
  if vsmjf.background_test then begin
    print,' ' & print, 'BACKGROUND TEST mode'
    retcode=1
    return
  endif


;test current spinbl number against max number spins in current mjf 
   if swest.ispinbl gt vsmjf.n_spins-1 then begin
     swest.ispinbl=0   
     recn=recn+1
     repcode=1
     return
     ;goto,point1     ;read the next record
   endif

;test whether current spinbl contains selected specie
;test first step in each sector of spin
   if ntest2 lt 3 then begin
     ntest2=ntest2+1
     if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then $
       wspecie=where(vsmjf.eleion(*,swest.ispinbl) eq swest.specie_selct) $   
     else if vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 $
     or vsmjf.scimode eq 6 then $
       wspecie=where(vsmjf.eleion eq swest.specie_selct) $ 
     else if wspecie(0) eq -1 then begin
       swest.ispinbl=swest.ispinbl+1
       if swest.ispinbl gt vsmjf.n_spins-1 then begin
         swest.ispinbl=0 & if recn lt fh.nmf-1 then recn=recn+1 $
         else stop,'end of file'
       endif
       repcode=1
       return 
       ;goto,point1     ;read the next record
     endif
   endif else begin
      retcode=1
      print,$
      'selected species not available on 3 successive spinwst....increment recn'
      return
   endelse



  ;test for data quality
  if vsmjf.vqlty(swest.ispinbl) ne 0 then begin
     retcode=1
     print,' '
     print,'quality flag ne 0 for this spinblock',swest.ispinbl,$
        '; data set to 0'
     return
  endif
 
 ;  (for now, we assume that all sectors of a given spin are the same specie
;   and have the same steps......see comment in mode2.pro  )
if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then begin
   chcksect=$
     where(vsmjf.eleion(*,swest.ispinbl) ne vsmjf.eleion(0,swest.ispinbl))
   if chcksect(0) ne -1 then begin
     retcode=1
     print,'all sectors of given spin are not of  same specie'
     return
   endif

;  Now test species this spin with select species to plot
   if vsmjf.eleion(0,swest.ispinbl) ne swest.specie_selct then begin
     retcode=1
     print,'species this spin is not the same as the selected species'
     return
   endif
endif 
end