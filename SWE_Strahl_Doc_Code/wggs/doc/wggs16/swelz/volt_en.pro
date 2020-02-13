function volt_en,volt_steps,vel=vel,en=en,ion=ion    ;vel_lvls,en_lvls

;volt_steps = offset into voltage table

if keyword_set(ion) eq 0 then ions=0 else ions=ion

vtoev=7.05 ;volts to ev conversion

const=2.85e-16

case ions of
0: begin    ;electrons
  if keyword_set(vel) ne 0 then $
   return,sqrt(vtoev * veis_eletbl(volt_steps)/const)

  if keyword_set(en) ne 0 then return,vtoev * veis_eletbl(volt_steps)

  endcase

1: begin    ;ions 

  if keyword_set(vel) ne 0 then $
   return,sqrt(vtoev * veis_iontbl(volt_steps-64)/(const*1836.))

  if keyword_set(en) ne 0 then return,vtoev * veis_iontbl(volt_steps-64)

  endcase
endcase

end
