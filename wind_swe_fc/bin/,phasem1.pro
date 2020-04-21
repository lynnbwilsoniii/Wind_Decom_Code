
;================================= phasem1 =====================================

;pro phasem1

;determine spin phase at each bci (each data sample) for mode 1
;at beginning of mjf and whenever there is a mode change

common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf

print,'phasem1'

;set parameters
  iclicks_sunpulse=4096 & iclicks_bci=40
  spin_bci=float(iclicks_bci)/float(iclicks_sunpulse) 
  phi_bci=iclicks_bci*360.d0/iclicks_sunpulse 
  bcis_spin=360.d0/phi_bci & nbcis_spin= fix(bcis_spin) & phidly_sun=42.5
  phi1_bci0=181.1 & phi2_bci0=1.1 & phisc_bci0=46.1 & phistrl_bci0=200.6 
  n_hkvars=32 & n_hkmode=3 
  n_spins=7 & n_vs=924  & n_fcbl=31 & n_fc=122
  n_vdat=576  & n_sdat=336   
  n_vdets=6 & n_vesteps=16 & n_sectors=6  & n_strdets=12 & n_strphis=28


;define data structure for veis/strahl data samples 
  vsmjf={  vsdata,$
  descr:'   ',tjd:0,sec:0d,hour:0l,min:0l,isec:0l,ms:0,mjfcnt:0,scimode:0,$
  n_vdets:0l,n_vesteps:0l,n_sectors:0l,n_spins:0l,n_strdets:0l,n_strphis:0l,$
  veis:bytarr(n_vdets,n_vesteps,n_sectors,n_spins),veistep:bytarr(n_vesteps),$
  secveis:dblarr(n_vesteps,n_sectors,n_spins),$
  phiveis:dblarr(n_vdets,n_vesteps,n_sectors),theveis:dblarr(n_vdets),$
  strl:bytarr(n_strdets,n_strphis,n_spins),strlstep:bytarr(n_spins),$
  secstrl:dblarr(n_strphis,n_spins),$
  phistrl:dblarr(n_strphis),thestrl:dblarr(n_strdets)  }
 


;------------------------spin phase of each data sample -----------------------
	
;phivdet = azimuthal angle of each veis detector in spin plane relative to its 
;respective sensor normal, measured in the spin direction
  phivdet=[-13.78,53.00,-47.25,47.25,-53.00,13.78]

;thevdet = polar angle of each veis detector measured from spin axis
  thevdet=[143.49,73.79,58.36,121.64,106.21,36.51]

;thesdet = polar angle of each strahl detector measured from spin axis
  thesdet=[61,66,71,76,81,86,94,99,104,109,114,119]

;set bci indices corrsponding to strahl samples (mode1)
  ibci_strl=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,    $
             52,53,54,55,56,57,58,59,60,61,62,63,64,65 ]

 
;spin phase angle of veis sensor normals, strahl sensor direction,
;and spacecraft x-axis relative to sun at each bci per spin 

phiv1=indgen(nbcis_spin)*phi_bci + phi1_bci0   ;sensor #1
phiv2=indgen(nbcis_spin)*phi_bci + phi2_bci0   ;sensor #2
phisc=indgen(nbcis_spin)*phi_bci + phisc_bci0  ;sc x-axis
phistrl=indgen(nbcis_spin)*phi_bci + phistrl_bci0 ;strahl sensor
;w=where( phiv1 ge 360.,nw ) & if nw gt 0 then phiv1(w)=phiv1(w)-360.
;w=where( phiv2 ge 360.,nw ) & if nw gt 0 then phiv2(w)=phiv2(w)-360.
;w=where( phisc ge 360.,nw ) & if nw gt 0 then phisc(w)=phisc(w)-360.
;w=where( phistrl ge 360.,nw ) & if nw gt 0 then phistrl(w)=phistrl(w)-360.

;spin phase angle of veis data samples relative to direction of sun;
    ibci=-1
    for isector=0,n_sectors-1 do begin
      ibci=ibci+1                  ;skipping 1 bci each sweep
      for ivestep=0,n_vesteps-1 do begin
         ibci=ibci+1
         vsmjf.phiveis(0:2,ivestep,isector)=phiv1(ibci)-phivdet(0:2)
         vsmjf.phiveis(3:5,ivestep,isector)=phiv2(ibci)+phivdet(3:5)
      endfor
    endfor

;make range of phi 0:360
  w=where(vsmjf.phiveis ge 360.d,nw)
  if nw gt 0 then vsmjf.phiveis(w)=vsmjf.phiveis(w)-360.d

  w=where(vsmjf.phiveis lt 0.d,nw)
  if nw gt 0 then vsmjf.phiveis(w)=vsmjf.phiveis(w)+360.d

;polar angle of veis data samples relative to spin axis
  vsmjf.theveis=thevdet


;spin phase angle of strahl data samples relative to direction of sun
  vsmjf.phistrl=phistrl(ibci_strl)

;polar angle of strahl data samples relative to spin axis
  vsmjf.thestrl=thesdet


end

