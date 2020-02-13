pro get_interpltd_swekp,date,ispin,iondx,helium,dens_cal,iondens,ionvel,iontemp

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shared,d

refsec=pb5_sec(ymd_pb5(long(date)))
      
      interpol_ion,$
      d.swe_ionkpdat.ta,d.swe_ionkpdat.n,iondx,vsmjf.suntim_vsbl(ispin),$
        refsec,iondens
      ;interpol_ion,$
      ;d.swe_ionkpdat.ta,d.swe_ionkpdat.v,iondx,vsmjf.suntim_vsbl(ispin),$
      ;  refsec,ionvel
      ;interpol_ion,$
      ;d.swe_ionkpdat.ta,d.swe_ionkpdat.ti,iondx,vsmjf.suntim_vsbl(ispin),$
      ;  refsec,iontemp
            
      ;ion densities are increased by 10% to estimate 5% doubly charged 
      ;helium contribution
        dens_cal=(1.0+2*helium)*iondens
    
end