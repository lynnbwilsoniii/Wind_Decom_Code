pro swe_moments_read_cdf,tjd0_thisfile=tjd0

common shared,d
common wstuff,wst

lpr=1   ;help and print flag

idatype=where(d.datype eq 'swe_moments')
CDF_file=d.flnm(idatype)


CDFid= -1
CDFid = cdf_open(CDF_file)

;vnames=get_allvarnames(CDFid=CDFid)

print,' ' & print,'reading CDF moments file ',CDF_file,' ......'

!quiet=1

vname='major_fr_rec' 
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
mfrec=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='major_fr_spin_number'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
mfspinbl=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='Te'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
trout=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='Te_anisotropy'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
anis=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='average_energy'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
eavg=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='pa_press_tensor'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
paxis=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='pa_dot_B'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
padotb=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

;vname='heat_flux_vector'
;out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
;hmag=out(0,*)
;theh=out(1,*)
;phih=out(2,*)
;if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='heat_flux_magn'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
hmag=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='heat_flux_el'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
theh=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='heat_flux_az'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
phih=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='Q_dot_B'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
qdotb=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='sc_position'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
rgse=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

;vname='el_bulk_vel'
;out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
;umag=out(0,*) 
;theu=out(1,*)
;phiu=out(2,*)
;help,out,vname, CDFid, vary, dtype, recs & print,''

vname='el_bulk_vel_magn'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
umag=out
help,out,vname, CDFid, vary, dtype, recs & print,''

vname='el_bulk_vel_el'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
theu=out
help,out,vname, CDFid, vary, dtype, recs & print,''

vname='el_bulk_vel_az'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
phiu=out
help,out,vname, CDFid, vary, dtype, recs & print,''

vname='el_density'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
fnout=out
help,out,vname, CDFid, vary, dtype, recs & print,''

vname='sc_pot'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
spcpot=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='flag'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
flag=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

;vname='Alt_heat_flux'
;out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
;if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='Epoch'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
epoch=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

vname='time_PB5'
out = read_myVARIABLE( vname, CDFid, vary, dtype, recs)
tpb5=out
if lpr then help,out,vname, CDFid, vary, dtype, recs & print,''

nrec=n_elements(mfrec)

d.ndx(0,idatype)=0 
d.ndx(1,idatype)=nrec-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)       
       
fill=-1.e31
;put input data into structure mdat
mdat=replicate($
  {mfrec:0l,mfspinbl:0l,trout:0.,anis:0.,eavg:0.,paxis:fltarr(3),$
  padotb:0.,hmag:0.,theh:0.,phih:0.,qdotb:0.,rgse:fltarr(3),$
  umag:0.,theu:0.,phiu:0.,fnout:0.,spcpot:0.,flag:0,tpb5:lonarr(3),ta:0.d,$
  prod_date:string('',format='(i8)'),gains:fltarr(6)},$
  nrec)
  
mdat.mfrec=mfrec
mdat.mfspinbl=mfspinbl
mdat.trout=trout
mdat.anis=anis
mdat.eavg=eavg
mdat.paxis=paxis
mdat.padotb=padotb
mdat.hmag=hmag
mdat.theh=theh
mdat.phih=phih
mdat.qdotb=qdotb
mdat.rgse=rgse
mdat.umag=umag
mdat.theu=theu
mdat.phiu=phiu
mdat.fnout=fnout
mdat.spcpot=spcpot
mdat.flag=flag
mdat.tpb5=tpb5
for i=0l,nrec-1 do mdat(i).ta=pb5_sec(mdat(i).tpb5)

;get start time for this file 
k0=0
tjd0=long(fix(mdat(k0).ta/86400.d)) 

print,'swe_moments_read_cdf: start/stop time'
print,mdat(0).tpb5,mdat(nrec-1).tpb5

d=create_struct(d,'swe_mdat',mdat)
      

;cdf_vname------------------plot_vname                
;major_fr_rec               mfrec
;major_fr_spin_number       mfspinbl
;Te                         trout
;Te_anisotropy              anis
;average_energy             eavg
;pa_press_tensor
;pa_dot_B
;heat_flux_vector
;Q_dot_B
;sc_position
;el_bulk_vel
;el_density
;sc_pot
;flag
;Alt_heat_flux
;Epoch
;time_PB5



end
