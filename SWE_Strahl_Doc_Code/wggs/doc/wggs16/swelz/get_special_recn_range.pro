pro get_special_recn_range,date,eclipse,sheath,recn_range

;Redefines record range for lzmom.pro on spaecial dates

if date eq '19941212' then recn_range=[1,1225]
if eclipse and date eq '19941227' then recn_range=[1141,1229]   ;27dec94 eclipse
if sheath then begin
  if date eq '19941130' then recn_range=[1259,1777]  ;sheath
  if date eq '19941201' then recn_range=[518,1023]   ;sheath
  if date eq '19941212' then recn_range=[667,1222]   ;sheath
  if date eq '19941213' then recn_range=[1,626]      ;sheath
  if date eq '19941224' then recn_range=[626,1045]   ;sheath
  if date eq '19941225' then recn_range=[1,663]      ;sheath
endif
if date eq '19961223' then recn_range=[1100,fh.nmf]
if date eq '20000127' then recn_range(1)=recn_range(1) < 860

end