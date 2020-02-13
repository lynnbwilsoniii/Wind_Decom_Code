

pro dglint_m1_19950723,glnt,review=review

print,'enter dglint_m1_950723'

;============= array to identify glint data ===============================

restore,getenv('GLINTPATH')+'19950723_std2.0_m1.glntmap'

;fine-tuning (based on inspection of counts spectra and background spectra).....

glnt(1,   [9,10,11,12,13],   4)=-1
glnt(2,   [9,10,11,12],      2)=-1
glnt(3,   [6,7,11],          5)=-1
glnt(4,   [9,10,11],         0)=-1

;fine tune chabges made 25.2.99
glnt(       4,        9,         0 ) = +1
glnt(       0,        9:10,      2 ) = -1
glnt(       2,        9:10,      2 ) = +1
glnt(1,   [9,10,11,12,13],   4)=+1

help,glnt  
print,'number of glint points ',n_elements(where(glnt eq -1))

if keyword_set(review) ne 0 then begin
  for isect=0,5 do for idet=0,5 do for istep=0,15 do $
  if glnt(idet,istep,isect) eq -1 then $
  print,'glnt(', idet, ',  ', istep, ',  ',  isect, ' ) = -1'
endif



end
