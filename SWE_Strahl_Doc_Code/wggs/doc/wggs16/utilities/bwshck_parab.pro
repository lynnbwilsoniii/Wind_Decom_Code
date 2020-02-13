pro bwshck_parab,xbs
common wstuff,wst
common mpstuff,mflnm,pflnm,mdat,pdat,refsec

dens=total(mdat.fnout)/n_elements(mdat.fnout)
umag=total(mdat.umag)/n_elements(mdat.umag)

rad=!pi/180.

;shock parameters for 77309-310 are adynamically adjusted for current ram press
a10=15.576
a20=0.0375399
dens0=12.5
u0=280.    ;e5
c=((dens/dens0)*(umag/u0)^2)^0.16667
;c=((dens/dens0)*(sqrt(ux^2+uy^2+uz^2)/u0)^2)^0.16667
a1=a10/c
a2=a20*c

x=xbs(where(xbs le a1))
r=sqrt((a1-x)/a2)

oplot,x,r,color=wst.clr_orange,linestyle=2 
oplot,x,-r,color=wst.clr_orange,linestyle=2

end