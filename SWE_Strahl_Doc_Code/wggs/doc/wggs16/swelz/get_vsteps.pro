function get_vsteps,scimode,ispin

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

if scimode eq 1 or scimode eq 4 then vsteps=vsmjf.veistep $
else if scimode eq 6 then vsteps=reform(vsmjf.veistep(*,ispin))
    ;assumes all sectors electrons
    if scimode eq 2 then vsteps=reform(vsmjf.veistep(*,0,ispin))  

return, vsteps
    
end