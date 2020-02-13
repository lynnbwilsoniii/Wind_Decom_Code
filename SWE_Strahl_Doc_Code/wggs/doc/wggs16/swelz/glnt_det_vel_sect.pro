pro glnt_det_vel_sect,xveis,glnt

;print,'glnt_det_vel_sect.pro'

;help,xveis(*,*,*,0)
glnt=-1+lonarr(3,64)
iglnt=-1
ndets=6
nsteps=16
sz=size(xveis)

for k=0,sz(3)-1 do for i=0,5 do for j=0,15 do begin
 if xveis(i,j,k) eq -1 and iglnt lt 63 then begin
   iglnt=iglnt+1
   glnt(0,iglnt)=i
   glnt(1,iglnt)=j
   glnt(2,iglnt)=k
   ;print,iglnt,glnt(0,iglnt),glnt(1,iglnt),glnt(2,iglnt)
   if iglnt eq 99 then goto,endp
 endif
endfor
endp:
end