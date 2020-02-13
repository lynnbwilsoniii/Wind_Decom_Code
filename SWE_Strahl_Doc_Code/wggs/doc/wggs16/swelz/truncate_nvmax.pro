pro truncate_nvmax,fe,wx,wy,wz,w,nvmax

;---Truncates arrays above nvmax-1 energy steps

sz=size(fe)
ndet=sz(1)
nv=sz(2)
nsect=sz(3)

fe_trunc=fltarr(ndet,nvmax,nsect)
wx_trunc=dblarr(ndet,nvmax,nsect)
wy_trunc=dblarr(ndet,nvmax,nsect)
wz_trunc=dblarr(ndet,nvmax,nsect)
for i=0,ndet-1 do for j=0,nvmax-1 do for k=0,nsect-1 do begin 
  fe_trunc(i,j,k)=fe(i,j,k)
  wx_trunc(i,j,k)=wx(i,j,k) 
  wy_trunc(i,j,k)=wy(i,j,k)
  wz_trunc(i,j,k)=wz(i,j,k)
endfor

fe=fe_trunc
wx=wx_trunc
wy=wy_trunc
wz=wz_trunc
w=sqrt(wx^2+wy^2+wz^2)

fe_trunc=0
wx_trunc=0
wy_trunc=0
wz_trunc=0

end