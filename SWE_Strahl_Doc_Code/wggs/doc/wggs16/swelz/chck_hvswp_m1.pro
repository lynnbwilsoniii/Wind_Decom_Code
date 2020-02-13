function chck_hvswp_m1,wsteps,weleion,specie_selct

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

flag1=0
    ;check for consistency in volt_steps and selected specie mode2
      welec=where(wsteps lt 64)
      wion=where(wsteps ge 64 and wsteps lt 128)
      if (welec(0) ne -1 and wion(0) ne -1) or $
         (wion(0) eq -1 and specie_selct eq 1) or $
         (welec(0) eq -1 and specie_selct eq 0) then begin
        print,'volt_en: inconsistency in voltage steps and specie flag'
        print,'select another record,  current record = ',recn
        flag1=1
        return,flag1
      endif

return,flag1

end
