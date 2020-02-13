pro nosunpuls_get_spnphx,tjd,ibci_strl,event_date

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common phasemod1,phiveis,theveis,phistrl,thestrl,vunit,vunitstrl

;---If the sun sensor becomes saturated by a solar proton event, 
;   shutting off the sunpulse, then this procedure is called by mode6.pro
;   to compute the necessary spin phase information to process strahl 
;   and VEIS data. This procedure restores the save file containing the correct
;   spin period and sc spin phase.

iclicks_sunpulse=4096 & iclicks_bci=40
spin_bci=float(iclicks_bci)/float(iclicks_sunpulse) 
phi_bci=iclicks_bci*360.d0/iclicks_sunpulse 
bcis_spin=360.d0/phi_bci & nbcis_spin= fix(bcis_spin) & phidly_sun=42.5
phixsc=46.1
phistrl_bci0=202.4d
phi1_bci0=181.1d
phi2_bci0=1.1d 
n_strphis=28
n_spins=7
n_vdets=6
n_vesteps=16
n_sectors=6
phivdet=[-17.340d, 53.480d, -46.070d, 48.380d, -52.510d, 10.110d] ;actual elec

;---original spin phase
phiveis_orig=phiveis

;---restore the save file containing correct spin phase during nosunpuls period
restore,getenv('SWEDATLIB')+event_date+'_spnphxdat'
wrecn=where(spnphx.tjd_tag eq tjd and spnphx.mjf eq recn)
if wrecn(0) eq -1 then return
i=wrecn(0)

;---begin to replace original spin phase data with nosun pulse spin phase data
phxsc=spnphx(i).phxsc
wle0=where(phxsc lt 0.d0)
if wle0(0) ne -1 then phxsc(wle0)=360.d0+phxsc(wle0)
 
phistrl=intarr(n_strphis,n_spins) 
vunit=dblarr(n_vdets,n_vesteps,n_sectors,3,n_spins) 
for k=0,n_spins-1 do begin
  phistrl(0:n_strphis/2-1,k)=phxsc(k)+(phistrl_bci0-phixsc)+$
     (ibci_strl(0,k)+indgen(n_strphis/2))*$
     phi_bci*(spnphx(i).spinp_apparent/spnphx(i).spinp_real)
  phistrl(n_strphis/2:n_strphis-1,k)=phxsc(k)+(phistrl_bci0-phixsc)+$
     (ibci_strl(n_strphis/2,k)+indgen(n_strphis/2))*$
     phi_bci*(spnphx(i).spinp_apparent/spnphx(i).spinp_real)
  phistrl(*,k)= phistrl(*,k) mod 360.d 

  phiv1=indgen(nbcis_spin)*$
    phi_bci*(spnphx(i).spinp_apparent/spnphx(i).spinp_real) + $
    (phi1_bci0-phixsc) +phxsc(k)- phi_bci/2  ;sensor #1
  phiv2=indgen(nbcis_spin)*$
    phi_bci*(spnphx(i).spinp_apparent/spnphx(i).spinp_real) + $
    (phi2_bci0-phixsc)+phxsc(k)  - phi_bci/2 ;sensor #2
  
  ;---spin phase angle of veis data samples relative to direction of sun;
    ibci=-1
    for isector=0,n_sectors-1 do begin
      ibci=ibci+1                  ;skipping 1 bci each sweep
      for ivestep=0,n_vesteps-1 do begin
         ibci=ibci+1
         phiveis(0:2,ivestep,isector,k)=phiv1(ibci)-phivdet(0:2)
         phiveis(3:5,ivestep,isector,k)=phiv2(ibci)+phivdet(3:5)
      endfor
    endfor
  
  phiveis(*,*,*,k)=phiveis(*,*,*,k) mod 360.d0
  wle0=where(phiveis(*,*,*,k) lt 0.d0)
  if wle0(0) ne -1 then phiveis(wle0,k)=360.d0+phiveis(wle0,k)
  unitvector,mode=1,phiveis(*,*,*,k),theveis,vunit_sp,check=0
  vunit(*,*,*,*,k)=vunit_sp
endfor

vsmjf.phiveis=phiveis
vsmjf.vunit=vunit
vsmjf.phistrl=phistrl
sp.spinp=spnphx(i).spinp_apparent    
vsmjf.spinp=spnphx(i).spinp_apparent

;---glint treatment
testplt=0;1
jk=indgen(n_vesteps*n_sectors)
nspglnt=lonarr(n_vdets,n_vesteps,n_sectors,n_spins)+1
for ispin=0,n_spins-1 do begin
  for ldet=0,n_vdets-1 do begin
    w=where(vsmjf.xveis(ldet,*,*,ispin) eq -1)
    if w(0) ne -1 then begin
      phi_glint=dblarr(n_elements(w))
      for iw=0,n_elements(w)-1 do begin
        isect=fix(w(iw)/n_vesteps)
        ivstep=w(iw)-isect*n_vesteps
        phi_glint(iw)=phiveis_orig(ldet,ivstep,isect,ispin)
        wmin=min(abs(phi_glint(iw)-phiveis(ldet,*,*,ispin)),wndx)
        ;print,phi_glint(iw),wmin
        ;print,phiveis(ldet,*,*,ispin) & print,' '
        ;print,phi_glint(iw) & print,' '
        ;print,abs(phi_glint(iw)-phiveis(ldet,*,*,ispin))
        deltaphi=7.0  ;3.5
        if wmin le deltaphi then begin
          isect_new=fix(wndx/n_vesteps)
          ivstep_new=wndx-isect_new*n_vesteps
          nspglnt(ldet,ivstep_new,isect_new,ispin)=-1
        endif
         
        if testplt then print,w(iw),ivstep,isect,ivstep_new,isect_new,$
          vsmjf.xveis(ldet,ivstep,isect,ispin),$
          phiveis_orig(ldet,ivstep,isect,ispin),$
          phiveis(ldet,ivstep_new,isect_new,ispin),$
          format='(6i10,2f12.3)
      endfor 
      
      ;---determine if a run of consecutive original glint samples has in the
      ;   corresponding run of new glint samples a point not selected as glint
      w=where(nspglnt(ldet,*,*,ispin) eq -1,nw)
      ;if ldet eq 2 and ispin eq 2 then stop
      if nw gt 1 then begin
        for iw=1,n_elements(w)-1 do begin
          if (w(iw)-w(iw-1)) eq 2 then begin
            wndx=w(iw-1)+1
            isect_new=fix(wndx/n_vesteps)
            ivstep_new=wndx-isect_new*n_vesteps
            nspglnt(ldet,ivstep_new,isect_new,ispin)=-1
          endif   
        endfor
      endif
      ;w=where(nspglnt(ldet,*,*,ispin) eq -1,nw)  
      ;if ldet eq 2 and ispin eq 2 then stop
       
      if testplt then begin
        window,0
        plot,jk,phiveis_orig(ldet,*,*,ispin),yrange=[0,360],ystyle=1
        oplot,jk,phiveis(ldet,*,*,ispin),psym=4,symsize=0.3,color=100 
        oplot,jk(w),phi_glint,psym=4,color=75
        
        for isect=0,n_sectors-1 do for ivstep=0,n_vesteps-1 do $
          print,ivstep,isect,n_vesteps*isect+ivstep,$
          phiveis(ldet,ivstep,isect,ispin)
        print,ispin,ldet    
        stop  
      endif
      
    endif
  endfor
endfor

vsmjf.xveis=nspglnt

end