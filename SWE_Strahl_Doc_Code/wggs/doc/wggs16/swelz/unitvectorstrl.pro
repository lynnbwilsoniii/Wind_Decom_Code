pro unitvectorstrl,mode=mode,phistrl,thestrl,vunitstrl,check=check

;unit vectors particle velocity, i.e., opposite look directions, mode1 & mode2

if mode eq 1 or mode eq 4 or mode eq 6 then begin
  nstrdets=12
  nstrphis=28
endif else if mode eq 2 then begin
  nstrdets=4
  nstrphis=32
endif

vunitstrl=dblarr(nstrdets,nstrphis,3)


snth=sin(thestrl*!dtor)
csth=cos(thestrl*!dtor)
snph=sin(phistrl*!dtor)
csph=cos(phistrl*!dtor)
for i=0,nstrdets-1 do for j=0,nstrphis-1 do begin
  vunitstrl(i,j,0)=-snth(i)*csph(j)
  vunitstrl(i,j,1)=-snth(i)*snph(j)
  vunitstrl(i,j,2)=-csth(i)
endfor

if keyword_set(check) eq 0 then check=0
if check then begin
  for i=0,nstrdets-1 do for j=0,nstrphis-1 do begin
    print,i,j,sqrt(total(vunitstrl(i,j,*)*vunitstrl(i,j,*))),$
      vunitstrl(i,j,*),format='(2i4,4f12.4)'
  endfor
  
endif



end 
  

