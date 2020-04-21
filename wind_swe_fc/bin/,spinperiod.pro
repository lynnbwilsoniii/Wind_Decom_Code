
pro spinperiod,sp


case sp.newmjf of

0: begin

     ispin_dis=sp.spincnt-sp.old_spincnt
     if ispin_dis lt 0 then ispin_dis=ispin_dis+256
     sec_dis=(sp.tjd-sp.old_tjd)*86400.d0 + sp.sec-sp.old_sec
     if sec_dis gt 0.d0 then begin
       sp.spinp=sec_dis/ispin_dis
       if sp.spinp gt 0.9*3 and sp.spinp lt 1.1*3 then begin ;good spin period
         sp.lst_spinp=sp.spinp        ;last known spin period
         sp.lst_tjd=sp.tjd            ;tjd of last known spin period
         sp.lst_sec=sp.sec            ;sec of last known spin period
       endif else sp.spinp=0.d0    ;spin period unknown
     endif else sp.spinp=0.d0    ;spin period unknown
  endcase

1: sp.spinp=0.d0  ;spin period unknown

endcase

sp.old_tjd=sp.tjd
sp.old_sec=sp.sec
sp.old_spincnt=sp.spincnt
sp.old_mjfcnt=sp.mjfcnt
sp.newmjf=0
end


