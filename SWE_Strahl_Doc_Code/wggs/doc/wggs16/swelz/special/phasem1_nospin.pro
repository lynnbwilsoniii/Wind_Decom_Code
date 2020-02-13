
;================================= phasem1 ==============================

pro phasem1_nospin,lpr=lpr

;<<<< !!!! modified to simulate non-spinning s/c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;determine spin phase at each bci (each data sample) for mode 1

common phasemod1,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl

if keyword_set(lpr) eq 0 then lpr=0
if lpr then print,'phasem1'

;set parameters
  iclicks_sunpulse=4096 & iclicks_bci=40
  spin_bci=float(iclicks_bci)/float(iclicks_sunpulse) 
  phi_bci=iclicks_bci*360.d0/iclicks_sunpulse 
  bcis_spin=360.d0/phi_bci & nbcis_spin= fix(bcis_spin) & phidly_sun=42.5d
  phi1_bci0=181.1d & phi2_bci0=1.1d & phisc_bci0=46.1d & phistrl_bci0=202.4d 
  ;phistrl_bci0=200.6  (old)

  n_vdets=6           ;number of veis detectors
  n_vesteps=16        ;number of veis energy steps per scan
  n_sectors =6        ;number of sectors
  n_spins=7           ;number of spins with data per mjf
  n_strdets=12        ;number strahl detectors
  n_strphis=28        ;number of strahl samples (azimuth) per mjf

 
phiveis=dblarr(n_vdets,n_vesteps,n_sectors)
theveis=dblarr(n_vdets)
phistrl=dblarr(n_strphis)
thestrl=dblarr(n_strdets)

;------------------------spin phase of each data sample -----------------------
	
;phivdet = azimuthal angle of each veis detector in spin plane relative to its 
;respective sensor normal, measured in the spin direction
  ;phivdet=[-13.78d, 53.00d, -47.25d, 47.25d, -53.00d, 13.78d] ;opposed det's
  phivdet=[-17.340d, 53.480d, -46.070d, 48.380d, -52.510d, 10.110d] ;actual elec

;thevdet = polar angle of each veis detector measured from spin axis
  ;thevdet=[143.49d, 73.79d, 58.36d, 121.64d, 106.21d, 36.51d] ;opposed det's
   thevdet=[142.78d, 75.99d, 56.34d, 119.61d, 108.41d, 35.91d] ;actual elec

;thesdet = polar angle of each strahl detector measured from spin axis
  thesdet=[61.30d, 65.65d, 70.46d,   75.35d,  80.35d,  84.39d, $
           94.89d, 99.79d, 104.67d, 109.52d, 114.35d, 118.75d]

;set bci indices corrsponding to strahl samples (mode1)
  ibci_strl=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14, $
             52,53,54,55,56,57,58,59,60,61,62,63,64,65 ]

 
;spin phase angle of veis sensor normals, strahl sensor direction,
;and spacecraft x-axis relative to sun at each bci per spin 
;NOTE: The term, phi_bci/2, causes the angles to refer to the bci midpoint

;phiv1=indgen(nbcis_spin)*phi_bci + phi1_bci0 - phi_bci/2  ;sensor #1
;phiv2=indgen(nbcis_spin)*phi_bci + phi2_bci0  - phi_bci/2 ;sensor #2
;phisc=indgen(nbcis_spin)*phi_bci + phisc_bci0 - phi_bci/2 ;sc x-axis

phiv1=dblarr(nbcis_spin)+51.d0+phi_bci + phi1_bci0 - phi_bci/2  ;sensor #1
phiv2=dblarr(nbcis_spin)+51.d0+phi_bci + phi2_bci0  - phi_bci/2 ;sensor #2
phisc=dblarr(nbcis_spin)+51.d0+phi_bci + phisc_bci0 - phi_bci/2 ;sc x-axis

phistrl=indgen(nbcis_spin)*phi_bci + phistrl_bci0 - phi_bci/2 ;strahl sensor

;spin phase angle of veis data samples relative to direction of sun;
    ibci=-1
    for isector=0,n_sectors-1 do begin
      ibci=ibci+1                  ;skipping 1 bci each sweep
      for ivestep=0,n_vesteps-1 do begin
         ibci=ibci+1
         phiveis(0:2,ivestep,isector)=phiv1(ibci)-phivdet(0:2)
         phiveis(3:5,ivestep,isector)=phiv2(ibci)+phivdet(3:5)
      endfor
    endfor

;make range of phi 0:360
  w=where(phiveis ge 360.,nw)
  if nw gt 0 then phiveis(w)=phiveis(w)-360.

  w=where(phiveis lt 0.,nw)
  if nw gt 0 then phiveis(w)=phiveis(w)+360.

;polar angle of veis data samples relative to spin axis
  theveis=thevdet 

;spin phase angle of strahl data samples relative to direction of sun,
;positive counterclockwise about spin axis (z-axis)
  phistrl=phistrl(ibci_strl)
  w=where(phistrl ge 360.,nw)
  if nw gt 0 then phistrl(w)=phistrl(w)-360.

  w=where(phistrl lt 0.,nw)
  if nw gt 0 then phistrl(w)=phistrl(w)+360.

;polar angle of strahl data samples relative to spin axis
  thestrl=thesdet

;get unitvector of particle velocity (opposite look direction)
  unitvector,mode=1,phiveis,theveis,vunit,check=0
  unitvectorstrl,mode=1,phistrl,thestrl,vunitstrl,check=0

;stop
end


