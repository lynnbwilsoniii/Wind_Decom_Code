;pro backg_m1_summary

dir=getenv('SWEDATLIB')+'backg/'
result=findfile(dir+'*m1_backg.dat',count=count)

n_vdets=6 & n_vesteps=16 & n_sectors=6

;sum over all background spectra
sumcts=fltarr(n_vdets,n_vesteps,n_sectors,count)
for i=0,count-1 do begin
  bkgcts_m1=fltarr(n_vdets,n_vesteps,n_sectors)
  openr,1,result(i)
  readu,1,bkgcts_m1
  close,1
  sumcts(*,*,*,i)=float(bkgcts_m1)
endfor

;find average background spectra
avgcts=fltarr(n_vdets,n_vesteps,n_sectors)
stdcts=fltarr(n_vdets,n_vesteps,n_sectors)
for jsect=0,n_sectors-1 do for jdet=0,n_vdets-1 do $
for jstep =0,n_vesteps-1 do begin
   std=stdev(float(sumcts(jdet,jstep,jsect,*)),mean)
   avgcts(jdet,jstep,jsect)=mean
   stdcts(jdet,jstep,jsect)=std
   ;print,jdet,jstep,jsect,avgcts(jdet,jstep,jsect),stdcts(jdet,jstep,jsect) 
endfor

m1backg_m1glint,glnt
for jsect=0,n_sectors-1 do for jdet=0,n_vdets-1 do $
for jstep =0,n_vesteps-1 do begin

if glnt(jdet,jstep,jsect) eq -1 then $
print,jdet,jstep,jsect,avgcts(jdet,jstep,jsect),stdcts(jdet,jstep,jsect),'  G' $
else print,jdet,jstep,jsect,avgcts(jdet,jstep,jsect),stdcts(jdet,jstep,jsect)
  
endfor







end