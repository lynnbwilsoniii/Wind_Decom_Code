

restore,getenv('WGGSBASE')+'wggs/lz_swe/mode2_patchangles.dat'  ;951216

help,veistep,phiveis

indx=[0,1,2,3]
vel=volt_en(veistep(indx,0,0),/vel)
print,vel
en=volt_en(veistep(indx,0,0),/en)
print,en


siz=size(phiveis)
n_vdets=siz(1)
n_sectors=siz(3)

print,'phiveis(n_vdets,0,n_sectors) '
for isect=0,n_sectors-1 do print,phiveis(*,0,isect),format='(6f11.4)'

for idet=0,n_vdets-1 do for isect=0,n_sectors-1 do begin
  ph=phiveis(idet,indx,isect)-phiveis(idet,0,isect)
  w=where(ph lt 0)
  if w(0) ne -1 then ph(w) = ph(w) + 360.
  plot,alog10(vel),ph,psym=1,/ynozero,xrange=[8.0,9.6],xstyle=1,$
    yrange=[-20,60],ystyle=1,xtitle='log velocity',ytitle='phi - phi(0)',$
    title='det '+string(idet,format='(i2)')+$
    '   sect '+string(isect,format='(i2)')
  y0=alog10(vel(0))
  a0=(alog10(vel(n_elements(vel)-1))-alog10(vel(0)))/ph(n_elements(vel)-1)
  vex=[8.0,(8.0+alog10(vel(0)))/2,alog10(vel)]
  phfit=(vex-y0)/a0 + phiveis(idet,0,isect)
  oplot,vex,phfit - phiveis(idet,0,isect)

  print,'y0,a0 ',y0,a0
  answ='' & print,'hit return to continue' & read,answ & if answ ne '' then stop
endfor



end


