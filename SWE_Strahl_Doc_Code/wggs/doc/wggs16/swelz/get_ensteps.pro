pro get_ensteps,wsteps,vel,en

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest

  if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then $
     wsteps=vsmjf.veistep(*,0,swest.ispinbl) $
   else if vsmjf.scimode eq 0 or vsmjf.scimode eq 1  or vsmjf.scimode eq 4 then $
     wsteps=vsmjf.veistep $
   else if vsmjf.scimode eq 6 then $
     wsteps=vsmjf.veistep(*,swest.ispinbl) 
      
   vel=volt_en(wsteps,/vel,ion=swest.specie_selct)
   en=volt_en(wsteps,/en,ion=swest.specie_selct)
   
end