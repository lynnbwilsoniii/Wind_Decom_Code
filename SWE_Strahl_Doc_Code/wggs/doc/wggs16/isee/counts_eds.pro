pro counts_eds,mode,format,inset,cc,counts,ff,spinp,b,countm,fm,Em,wx,wy,wz

ndet=6
nsect=6
cblok=fltarr(16,ndet,nsect)
cblok(*,*,*)=counts
fblok=fltarr(18,ndet,nsect)
fblok(*,*,*)=ff
nv=inset
v=cc(indgen(nv))

theta=[2.504943d,1.2877747d,1.0182763d,2.1233161d,1.8538178d,0.63664945d]
sn=sin(theta)
cn=cos(theta)
dphi=[13.79740892d,-53.03900295d,47.291061d,47.291061d,-53.03900295d,13.79740892d]
cyclt=0.48223222d
rad=57.29578d
spind=(6.2831853d/spinp)*rad
phiz=dblarr(ndet,nsect)
phiz(*,*)=((-9.d1+1.8d2*((indgen(ndet)+1)/4)+dphi)#replicate(1,nsect)+replicate(1,ndet)#(spind*cyclt*indgen(nsect)))/rad

tim=[ [0.0144048d,0.0427714d,0.0711380d,0.0995046d,0.1278712d,0.1562378d,0.1846044d,$
       0.2129710d,0.2413376d,0.2697042d,0.2980708d,0.3264374d,0.3548040d,0.3831706d,$
       0.4115372d,0.4399038d,0.4399038d,0.4399038d],$
      [0.0285881d,0.0853213d,0.1278712d,0.1562378d,0.17751275d,0.19169605d,0.20587935d,$
       0.22006265d,0.2413376d,0.2697042d,0.2980708d,0.3264374d,0.3548040d,0.3831706d,$
       0.4115372d,0.4399038d,0.4399038d,0.4399038d]  ]

phh=dblarr(nv,ndet,nsect)

wx=dblarr(nv,ndet,nsect)
wy=dblarr(nv,ndet,nsect)
wz=dblarr(nv,ndet,nsect)

phh(*,*,*)=((spind/rad)*tim(0:nv-1,format-1))#replicate(1,ndet*nsect)+replicate(1,nv)#phiz(*)

;vsn=v#sn
;vcn=v#cn
vsn=replicate(1.d,n_elements(v))#sn
vcn=replicate(1.d,n_elements(v))#cn
;direction cosines of detector viewing direction
wx(*,*,*)=(vsn(*)#replicate(1,nsect))*cos(phh)
wy(*,*,*)=(vsn(*)#replicate(1,nsect))*sin(phh)
wz(*,*,*)=vcn(*)#replicate(1,nsect)
w=sqrt(wx^2+wy^2+wz^2)
;wpara=(wx*b(0)+wy*b(1)+wz*b(2))/sqrt(total(b*b))
;wperp=sqrt(w^2-wpara^2)
;pa=acos(wpara/w)*rad

countm=fltarr(nv,ndet,nsect)
countm(*,*,*)=cblok(0:nv-1,*,*)
fm=fltarr(nv,ndet,nsect)
fm(*,*,*)=fblok(0:nv-1,*,*)
Em=2.85e-16*v^2

end
