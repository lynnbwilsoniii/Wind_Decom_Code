pro mulmat, a, b, nrowa, ncola, ncolb, c

;     THIS ROUTINE MULTIPLIES 2 GENERAL MATRICES TO FORM A RESULTANT
;     MATRIX. THE ONLY RESTRICTION IS THAT THE NUMBER OF COLUMNS OF
;     MATRIX A MUST BE THE SAME AS THE NUMBER OF ROWS OF MATRIX B --
;     WHICH IS NOT A REAL LIMITATION, AS IF THAT IS NOT TRUE, THE
;     MATRIX MULTIPLICATION OPERATION IS NOT DEFINED IN THE FIRST
;     PLACE.

c=dblarr(nrowa,ncolb)
for irow=0,nrowa-1 do for icol=0,ncolb-1 do begin
  sum=0.d
  for inner=0,ncola-1 do sum = sum + a(irow,inner)*b(inner,icol)
  c(irow,icol) = sum
endfor

end
