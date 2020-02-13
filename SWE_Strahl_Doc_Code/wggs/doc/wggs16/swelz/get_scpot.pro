pro get_scpot,vpot

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shared,d
common wstuff,wst
common swestuff,swest

idatype=where(d.datype eq 'swe_moments')

if d.datype_input(idatype(0)) ne -1 then begin    ;moments data has also been selected
  if wst.timsel eq 'survey' then mspn=swest.spn
  if wst.timsel eq 'lz' or wst.timsel eq 'lztm' then  $
       mspn=long(indx_begin(d.swe_mdat.ta,vsmjf.suntim_vsbl(swest.ispinbl) ))
  vpot=d.swe_mdat(mspn).vpot      ;cm/s
endif else begin                  ;moments data has not been selected
  vpot=0.
endelse


end