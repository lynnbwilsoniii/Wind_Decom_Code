pro patch_vel0,bnfit,ndets,npatch,nsectors,velocity,nvmin,vpot,$
  vpatch,wxpatch,wypatch,wzpatch,fpatch 

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

if npatch eq 0 then return

vpatch=(findgen(npatch)/npatch)*(velocity(nvmin)-vpot)
fpatch=fltarr(ndets,npatch,nsectors)
wxpatch=dblarr(ndets,npatch,nsectors)
wypatch=dblarr(ndets,npatch,nsectors)
wzpatch=dblarr(ndets,npatch,nsectors)

theveis=vsmjf.theveis
phiveis=vsmjf.phiveis

;---coefficients to fit spin phase angle for steps below measured range of vel
y0=8.25787 & a0=0.238804

for j=0,npatch-1 do begin
  if vpatch(j) eq 0 then $
    fpatch(*,j,*)=fltarr(ndets,nsectors)+fintt(0.,0.,0.,bnfit) $
  else begin
    for i=0,ndets-1 do for k=0,nsectors-1 do begin
      ;---put velocities in SPACECRAFT frame to evaluate patch
      phipatch=phiveis(i,nvmin,k)+(alog10(vpatch(j))-y0)/a0
      wxpatch(i,j,k)=$
        -sin(theveis(i)*!dtor)*cos(phipatch*!dtor)*vpatch(j)
      wypatch(i,j,k)=$
        -sin(theveis(i)*!dtor)*sin(phipatch*!dtor)*vpatch(j)
      wzpatch(i,j,k)=-cos(theveis(i)*!dtor)*vpatch(j)

      ;---get fitted f
      fpatch(i,j,k)=$
        fintt(wxpatch(i,j,k)*1e-8,wypatch(i,j,k)*1e-8,wzpatch(i,j,k)*1e-8,bnfit)
    endfor
  endelse
endfor
    
end