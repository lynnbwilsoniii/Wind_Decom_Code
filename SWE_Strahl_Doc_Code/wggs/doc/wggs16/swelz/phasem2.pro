
;================================= phasem2 ==============================

pro phasem2,lpr=lpr

;determine spin phase at each bci (each data sample) for mode 2


common phasemod2,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl

if keyword_set(lpr) eq 0 then lpr=0
;print,'into phasem2'

;set parameters
  iclicks_sunpulse=4096 & iclicks_bci=32
  spin_bci=float(iclicks_bci)/float(iclicks_sunpulse) 
  phi_bci=iclicks_bci*360.d0/iclicks_sunpulse 
  bcis_spin=360.d0/phi_bci & nbcis_spin= fix(bcis_spin) & phidly_sun=42.5d
  phi1_bci0=181.1d & phi2_bci0=1.1d & phisc_bci0=46.1d & phistrl_bci0=202.4d 

  n_vdets=6           ;number of veis detectors
  n_vesteps=16        ;number of veis energy steps per scan
  n_sect_m2 =4        ;number of sectors per half-spin
  n_hspns=17          ;number of half-spins with data per mjf
  n_strdets=4         ;number strahl detectors
  n_strphis=16        ;number of strahl samples (azimuth) per half-spin

 
phiveis=dblarr(n_vdets,n_vesteps,n_sect_m2,2)  ;two half spin
theveis=dblarr(n_vdets)
phistrl=dblarr(n_strphis,2)   ;two half spins
thestrl=dblarr(n_strdets)

;------------------------spin phase of each data sample -----------------------
	
;phivdet = azimuthal angle of each veis detector in spin plane relative to its 
;respective sensor normal, measured in the spin direction
  ;phivdet=[-13.78d, 53.00d, -47.25d, 47.25d, -53.00d, 13.78d] ;opposed det's
  phivdet=[-17.3400, 53.4800, -46.0700, 48.3800, -52.5100, 10.1100] ;actual elec

;thevdet = polar angle of each veis detector measured from spin axis
  ;thevdet=[143.49d, 73.79d, 58.36d, 121.64d, 106.21d, 36.51d] ;opposed det's
   thevdet=[142.78, 75.99, 56.34, 119.61, 108.41, 35.91] ;actual elec

;thesdet = polar angle of each strahl detector measured from spin axis
  ;thesdet=[61.30d, 65.65d, 70.46d,   75.35d,  80.35d,  84.39d, $
  ;         94.89d, 99.79d, 104.67d, 109.52d, 114.35d, 118.75d]
  thesdet=[65.65d, 80.35d, 99.79d, 114.35d]

;set bci indices corrsponding to strahl samples (mode2)
   ibci_strl=[[1 + indgen(n_strphis)],[65 + indgen(n_strphis)]]

;spin phase angle of veis sensor normals, strahl sensor direction,
;and spacecraft x-axis relative to sun at each bci per spin 
;NOTE: The term, phi_bci/2, causes the angles to refer to the bci midpoint

phiv1=indgen(nbcis_spin)*phi_bci + phi1_bci0 - phi_bci/2  ;sensor #1
phiv2=indgen(nbcis_spin)*phi_bci + phi2_bci0  - phi_bci/2 ;sensor #2
phisc=indgen(nbcis_spin)*phi_bci + phisc_bci0 - phi_bci/2 ;sc x-axis
phistrahl=indgen(nbcis_spin)*phi_bci + phistrl_bci0 - phi_bci/2 ;strahl sensor

;spin phase angle of veis data samples relative to direction of sun;
    ibci=-1
    for isector=0,n_sect_m2-1 do for ivestep=0,n_vesteps-1 do begin ;1st hspn
      ibci=ibci+1
      phiveis(0:2,ivestep,isector,0)=phiv1(ibci)-phivdet(0:2)
      phiveis(3:5,ivestep,isector,0)=phiv2(ibci)+phivdet(3:5)
    endfor
    for isector=0,n_sect_m2-1 do for ivestep=0,n_vesteps-1 do begin ;2nd hspn
      ibci=ibci+1
      phiveis(0:2,ivestep,isector,1)=phiv1(ibci)-phivdet(0:2)
      phiveis(3:5,ivestep,isector,1)=phiv2(ibci)+phivdet(3:5)
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
  phistrl=phistrahl(ibci_strl)
  w=where(phistrl ge 360.,nw)
  if nw gt 0 then phistrl(w)=phistrl(w)-360.

  w=where(phistrl lt 0.,nw)
  if nw gt 0 then phistrl(w)=phistrl(w)+360.

;polar angle of strahl data samples relative to spin axis
  thestrl=thesdet

;redimension phiveis from half spins to whole spins
  n_sectors=n_sect_m2*2
  phiveis1=dblarr(n_vdets,n_vesteps,n_sect_m2*2)
  phiveis1(*,*,*)=phiveis
  phiveis=phiveis1

;get unitvector of particle velocity (opposite look direction)
  unitvector,mode=2,phiveis,theveis,vunit,check=0
  unitvectorstrl,mode=2,phistrl,thestrl,vunitstrl,check=0
  
end


