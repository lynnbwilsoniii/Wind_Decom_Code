pro unitvector,mode=mode,phiveis,theveis,vunit,check=check

;unit vectors particle velocity, i.e., opposite look directions, mode1 & mode2

ndets=6 & nvsteps=16 
if mode eq 1 then nsectors=6
if mode eq 2 then nsectors=8

vunit=dblarr(ndets,nvsteps,nsectors,3)

vhatm=1.+fltarr(nvsteps)    ;unit magnitude for velocity unit vectors

snhat=sin(theveis*!dtor)#vhatm
cnhat=cos(theveis*!dtor)#vhatm
vunit(*,*,*,0)=$
  -(snhat(*)#replicate(1,nsectors))*cos(phiveis(*,*,*)*!dtor)
vunit(*,*,*,1)=$
  -(snhat(*)#replicate(1,nsectors))*sin(phiveis(*,*,*)*!dtor)
vunit(*,*,*,2)=-cnhat(*)#replicate(1,nsectors)

if keyword_set(check) eq 0 then check=0
if check then begin
  print,'opposing detectors'
  oppdet=[5,4,3,2,1,0]
  for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
    for i=0,5 do print,$
      j,k,i,oppdet(i),total(vunit(i,j,k,*)*vunit(i,j,k,*)),$
      total(vunit(i,j,k,*)*vunit(oppdet(i),j,k,*))
  endfor
  print,'adjacent detectors'
  adjdet1=[1,2,0,4,5,3]
  adjdet2=[2,0,1,5,3,4]
  for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
    for i=0,5 do print,$
      j,k,i,adjdet1(i),total(vunit(i,j,k,*)*vunit(adjdet1(i),j,k,*))
    for i=0,5 do print,$
      j,k,i,adjdet2(i),total(vunit(i,j,k,*)*vunit(adjdet2(i),j,k,*))
  endfor
endif


end 
  

