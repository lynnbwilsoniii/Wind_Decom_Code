function chck_hvswp_m2,wsteps,weleion,specie_selct,ispin

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

nweleion=n_elements(weleion)

flag2=0
    ;check for consistency in volt_steps and selected specie mode2
      welec=where(wsteps lt 64)
      wion=where(wsteps ge 64 and wsteps lt 128)
      if (welec(0) ne -1 and wion(0) ne -1) or $
         (wion(0) eq -1 and specie_selct eq 1) or $
         (welec(0) eq -1 and specie_selct eq 0) then begin
        print,'volt_en: inconsistency in voltage steps and specie flag'
        print,'select another record,  current record = ',recn
        flag2=1
        return,flag2
      endif

    ;make sure voltage sweeps in all selected sectors of spin are the same
      for i=0,vsmjf.n_vesteps-1 do $
      if wsteps(i) ne total(vsmjf.veistep(i,weleion,ispin))/nweleion $
      then begin        
        print,'voltage sweeps in this spin are different'
        print,'recn ',recn,'  mjf cnt  ',lz.mf(ihk(1).offs),$
         '   vsmjf.scimode ',vsmjf.scimode
        print,'veis hv tbl: ',lz.mf(hkind(ghk(31).offs))
        print,lz.mf(hkind(ghk(27).offs+indgen(64))),format='(16i4)'
        print,'wsteps ',wsteps
        print,'vsmjf.eleion_sweep, swpmd(vsmjf.eleion_sweep) ',$
          vsmjf.eleion_sweep,'  ',swpmd(vsmjf.eleion_sweep)
        flag2=1
        return,flag2
      endif

return,flag2

end
