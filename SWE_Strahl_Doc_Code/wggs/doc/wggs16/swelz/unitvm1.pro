pro unitvm1

;unit vectors mode1

common phase,phiveis,theveis,phistrl,thestrl
common vunitm1,vhat,ndets,nvsteps,nsectors,nspins

;get detector angles (in common phase)
  phasem1  

ndets=6 & nvsteps=16 & nsectors=6 & nspins=7
vhatm=1.+fltarr(nvsteps)    ;unit magnitude for velocity unit vectors

snhat=sin(theveis*!dtor)#vhatm
cnhat=cos(theveis*!dtor)#vhatm
vhat=fltarr(ndets,nvsteps,nsectors,3)  ;vel unit vector
vhat(*,*,*,0)=-(snhat(*)#replicate(1,nsectors))*cos(phiveis(*,*,*)*!dtor)
vhat(*,*,*,1)=-(snhat(*)#replicate(1,nsectors))*sin(phiveis(*,*,*)*!dtor)
vhat(*,*,*,2)=-cnhat(*)#replicate(1,nsectors)

check=0
if check then begin
  oppdet=[5,4,3,2,1,0]
  for j=0,nvsteps-1 do for k=0,nsectors-1 do begin
    for i=0,5 do print,j,k,i,oppdet(i),total(vhat(i,j,k,*)*vhat(i,j,k,*)),$
    total(vhat(i,j,k,*)*vhat(oppdet(i),j,k,*))
  endfor
endif


end 
  

