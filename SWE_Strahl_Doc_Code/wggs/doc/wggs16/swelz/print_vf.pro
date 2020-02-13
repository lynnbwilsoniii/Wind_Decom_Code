pro print_vf,velocity,vpot,ue,w,fe,npatch,nvmin,ndets,nvsteps,nsectors

if keyword_set(ue) eq 0 then ue=fltarr(3)

;--- w(0:ndets-1, 0:npatch+nvsteps-nvmin-1, 0:nsectors-1)
;---fe(0:ndets-1, 0:npatch+nvsteps-nvmin-1, 0:nsectors-1)
wavg=fltarr(npatch+nvsteps-nvmin)
favg=fltarr(npatch+nvsteps-nvmin)
for i=0,npatch+nvsteps-nvmin-1 do begin
  wavg(i)=total(w(*,i,*))/(ndets*nsectors)
  f=fe(*,i,*)
  wgt0=where(f gt 0,nwgt0)
  favg(i)=total(f(wgt0))/nwgt0
endfor

print,' ' 
print,'scpot (cm/s), (eV), bulk vel, nvmin : ',$
  vpot,(vpot^2)*2.85e-16,sqrt(total(ue^2)),nvmin,$
  format='(a35,e12.3,f6.1,e12.3,i5)'
print,' ' & print,'                vel - vpot        f     velocity    energy'
print,'patch:  '
for j=0,npatch-1 do print,j,wavg(j),favg(j),format='(10x,i3,2e12.3)'
print,'measured:'

for j=npatch,npatch+nvsteps-nvmin-1 do $
  print,j,wavg(j),favg(j),velocity(j-npatch+nvmin),$
    (velocity(j-npatch+nvmin)^2)*2.85e-16,format='(10x,i3,3e12.3,f8.1)'
  
end