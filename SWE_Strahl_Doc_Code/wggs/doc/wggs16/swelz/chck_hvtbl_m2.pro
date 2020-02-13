function chck_hvtbl_m2,veis_hvtbl


;check the stored mode2 hv table 
    ;first test for a full mode2 hv table   
    w0=where(veis_hvtbl eq 0)
    ;now test for consistency with the two allowable sweep modes, 
    ;elecs/ions alternating sectors or alternating spins
    hv=bytarr(16,16)
    hv(*,*)=veis_hvtbl
    hvodd=hv(*,2*indgen(16/2)+1)
    hvevn=hv(*,2*indgen(16/2))
    wsecteq=where(hvodd-hvevn eq 0,nwsecteq)
    hvspn0=hv(*,0:7)
    hvspn1=hv(*,8:15)
    wspineq=where(hvspn0-hvspn1 eq 0,nwspineq)
    ;if w0(0) eq -1 then no zeroes
    ;if nwspineq eq 0 and nwsecteq eq 128 then alt spins
    ;if nwsecteq eq 0 and nwspineq eq 128 then alt sects
    wokspns=nwspineq eq 0 and nwsecteq eq 128
    wokscts=nwsecteq eq 0 and nwspineq eq 128
    wok=(wokspns eq 0 and wokscts eq 1) or (wokspns eq 1 and wokscts eq 0) $
         and w0(0) eq -1

return,wok

end
