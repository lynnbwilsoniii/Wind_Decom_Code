pro swe_moments_structavg,ver,indat,data,nrec,nred

if ver eq '09' or ver eq '13' then nmisc=20 else $
if ver eq '07' or ver eq '08' then nmisc=10

  ;indat={$
  ;  mfrec:0l,$
  ;  mfspinbl:0l,$
  ;  ta:0.d,$
  ;  iflgs:lonarr(8),$
  ;  gains:fltarr(6),$
  ;  misc:fltarr(nmisc),$
  ;  v:fltarr(16),$
  ;  vpot:0.,$
  ;  fnout:0.,$
  ;  uout:fltarr(3),$
  ;  hout:fltarr(3),$
  ;  trout:0.,$
  ;  ettrt:0.,$
  ;  gyrtrpy:0.,$
  ;  eavg:0.,$
  ;  paxis:fltarr(3),$
  ;  pout:fltarr(3,3),$
  ;  spcpot:0.,$
  ;  b:fltarr(3),$
  ;  bnfit:fltarr(10),$
  ;  timpb5:lonarr(3),$
  ;  misc2:bytarr(28),$
  ;  rgse:fltarr(3)}


n=long(float(nrec)/float(nred))
    
mfrec=congrid(data.mfrec,n)
mfspinbl=congrid(data.mfspinbl,n)
ta=congrid(data.ta,n)
iflgs=congrid(data.iflgs,8,n)
gains=congrid(data.gains,6,n)
misc=congrid(data.misc,nmisc,n)
v=congrid(data.v,16,n)
vpot=congrid(data.vpot,n)
fnout=congrid(data.fnout,n,/interp)
uout=congrid(data.uout,3,n,/interp)
hout=congrid(data.hout,3,n,/interp)
trout=congrid(data.trout,n,/interp)
ettrt=congrid(data.ettrt,n,/interp)
gyrtrpy=congrid(data.gyrtrpy,n)
eavg=congrid(data.eavg,n)
paxis=congrid(data.paxis,3,n)
pout=congrid(data.pout,3,3,n)
spcpot=congrid(data.spcpot,n)
b=congrid(data.b,3,n,/interp)
bnfit=congrid(data.bnfit,10,n)
timpb5=congrid(data.timpb5,3,n)
misc2=congrid(data.misc2,28,n)
rgse=congrid(data.rgse,3,n)

newdat=replicate(indat,n)
for i=0,n-1 do begin
  newdat(i).mfrec=mfrec(i)
  newdat(i).mfspinbl=mfspinbl(i)
  newdat(i).ta=ta(i)
  newdat(i).iflgs(*)=iflgs(*,i)
  newdat(i).gains(*)=gains(*,i)
  newdat(i).misc(*)=misc(*,i)
  newdat(i).v(*)=v(*,i)
  newdat(i).vpot=vpot(i)
  newdat(i).fnout=fnout(i)
  newdat(i).uout(*)=uout(*,i)
  newdat(i).hout(*)=hout(*,i)
  newdat(i).trout=trout(i)
  newdat(i).ettrt=ettrt(i)
  newdat(i).gyrtrpy=gyrtrpy(i)
  newdat(i).eavg=eavg(i)
  newdat(i).paxis(*)=paxis(*,i)
  newdat(i).pout(*,*)=pout(*,*,i)
  newdat(i).spcpot=spcpot(i)
  newdat(i).b(*)=b(*,i)
  newdat(i).bnfit(*)=bnfit(*,i)
  newdat(i).timpb5(*)=timpb5(*,i)
  newdat(i).misc2(*)=misc2(*,i)
  newdat(i).rgse(*)=rgse(*,i)
  
endfor 


data=newdat
newdat=0
nrec=n


print,'Note: Input swe_moments array time dimension reduced by factor '+$
  string(nred,format='(i2)')+' using congrid.'

end