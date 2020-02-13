function sweepmode,hv,mode=mode

;determine (science mode 1,2) sweep mode from veis hv table, i.e.,
;  eleion_sweep = 0   all electrons,   mode 1,2
;               = 1   all ions,        mode 1,2
;               = 2   electrons, ions alternating sectors (or sweeps), mode 2
;               = 3   electrons, ions alternating spins, mode 2
;               =-1   undetermined, mode 1,2

;hv table contains levels for two complete spins of 8 sectors each (mode2)


;test for undetermined sweep
  eleion_sweep=-1
  w=where(hv ge 128,nw)
  if nw gt 0 then return,eleion_sweep

case mode of   

1 : begin   ;test mode1 sweep

;test for electrons or ions
  wele=where(hv lt 64,nwele)
  wion=where(hv gt 63 and hv lt 128,nwion)
  
  if nwele eq 16 then eleion_sweep=0  else $
  if nwion eq 16 then eleion_sweep=1  else eleion_sweep=-1
  return,eleion_sweep

  endcase
        
2 : begin   ;test mode2 sweep

;test for electrons
  wele=where(hv lt 64,nwele)

  ;test for all electrons
    if nwele eq 256 then begin

      ;test for 16 identical sweeps
         eleion_sweep=-1
         for isweep=1,15 do begin
           diff=hv(isweep*16+indgen(16))-hv((isweep-1)*16+indgen(16)) 
           w=where(diff ne 0)
           if w(0) ne -1 then return,eleion_sweep  ;undetermined sweep
         endfor

      ;ok, all electron sweeps are equal
      eleion_sweep=0
      return,eleion_sweep
    endif

;test for ions
  wion=where(hv gt 63 and hv lt 128,nwion)

  ;test for all ions
    if nwion eq 256 then begin

      ;test for 16 identical sweeps
         eleion_sweep=-1
        for isweep=1,15 do begin
           diff=hv(isweep*16+indgen(16))-hv((isweep-1)*16+indgen(16)) 
           w=where(diff ne 0)
           if w(0) ne -1 then return,eleion_sweep  ;undetermined sweep
         endfor

      ;ok, all ion sweeps are equal      
      eleion_sweep=1
      return,eleion_sweep
    endif

;test for electrons and ions in alternating sweeps or spins
  if nwele eq 128 and nwion eq 128 then begin
    
    ;test for 8 identical sweeps in alternating spins
      eleion_sweep=-1
      ;test for 8 identical sweeps in each spin
         eleion_sweep=-1
         for isweep=1,7 do begin
           diff=hv(isweep*16+indgen(16))-hv((isweep-1)*16+indgen(16))
           w=where(diff ne 0)
           if w(0) ne -1 then goto,check_altsect
           diff=hv((8+isweep)*16+indgen(16))-hv((8+isweep-1)*16+indgen(16))
           w=where(diff ne 0)
           if w(0) ne -1 then goto,check_altsect
         endfor               
         ;ok, alternating spins' sweeps are equal
         eleion_sweep=3
         return,eleion_sweep

    check_altsect:
    ;test for 8 identical sweeps in alternating sectors
       eleion_sweep=-1
       evensectors=[0,2,4,6,8,10,12,14]
       oddsectors=[1,3,5,7,9,11,13,15]
       for i=1,7 do begin

         isweep=evensectors(i)
         diff=hv(isweep*16+indgen(16))-hv((isweep-2)*16+indgen(16))
         w=where(diff ne 0)
         if w(0) ne -1 then return,eleion_sweep  ;undetermined sweep
         
         isweep=oddsectors(i)
         diff=hv(isweep*16+indgen(16))-hv((isweep-2)*16+indgen(16))
         w=where(diff ne 0)
         if w(0) ne -1 then return,eleion_sweep  ;undetermined sweep
       endfor 
       ;ok, alternating sectors' sweeps are equal
       eleion_sweep=2
       return,eleion_sweep

      
    endif
  endcase

else : stop

endcase

return,eleion_sweep
end

