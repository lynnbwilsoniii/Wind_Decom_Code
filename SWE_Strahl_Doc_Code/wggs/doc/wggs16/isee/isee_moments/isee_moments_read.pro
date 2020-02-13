
pro isee_moments_read

common shared,d

print,'isee_moments_read :'

;---define isee data structure
indat={datstruct,flnm:'',recn:0l,year:0,dday:0,msec:0l,ta:0.d,tpb5:lonarr(3),$
	harvey:0,mozer:0,mode:0,$
	format:0,spin:0.d,$
	gsex:0.,gsey:0.,gsez:0.,$
	uout:fltarr(3),fnout:0.,fn:fltarr(6),fpout:fltarr(9),fhout:fltarr(3),$
	trout:0.,ettrt:0.,deig:0.,$
	cosb:0.,eigval:fltarr(3),$
	potnew:0.,vpott:fltarr(6,6),$
        b:fltarr(3),$
        inset:0,iloflg:0,$
        
        umag:0.,theu:0.,phiu:0.,$
        hmag:0.,theh:0.,phih:0.,$
        bmag:0.,theb:0.,phib:0.,$
        qdotb:0.}

idatype=where(d.datype eq 'isee_moments')
filename=d.flnm(idatype)
        
CDF_file=filename

cdf_var='YEAR'
loadcdf,cdf_file,cdf_var,year

wne0=where(year ne 0)

ndat=n_elements(year(wne0))
iseemdat=replicate(indat,ndat)

d.ndx(0,idatype)=0 & d.ndx(1,idatype)=ndat-1  ;begin and end time indices
d.ndx_orig(*,idatype)=d.ndx(*,idatype)

;help,iseemdat
;help,iseemdat,/str
iseemdat.flnm=strmid(filename,strlen(getenv('ISEEPATH')),$
   strlen(filename)-strlen(getenv('ISEEPATH')) )
iseemdat.year=year(wne0)

cdf_var='DECIMAL DAY'
loadcdf,cdf_file,cdf_var,dday
iseemdat.dday=dday(wne0)

cdf_var='MILLISEC'
loadcdf,cdf_file,cdf_var,msec
iseemdat.msec=msec(wne0)

cdf_var='HARVEY STATUS'
loadcdf,cdf_file,cdf_var,harvey
iseemdat.harvey=harvey(wne0)

cdf_var='MOZER STATUS'
loadcdf,cdf_file,cdf_var,mozer
iseemdat.mozer=mozer(wne0)

cdf_var='MODE'
loadcdf,cdf_file,cdf_var,mode
iseemdat.mode=mode(wne0)

cdf_var='FORMAT'
loadcdf,cdf_file,cdf_var,iformat
iseemdat.format=iformat(wne0)

cdf_var='SPIN PERIOD'
loadcdf,cdf_file,cdf_var,spin
iseemdat.spin=spin(wne0)

cdf_var='THEO_GSE_X'
loadcdf,cdf_file,cdf_var,gsex
iseemdat.gsex=gsex(wne0)

cdf_var='THEO_GSE_Y'
loadcdf,cdf_file,cdf_var,gsey
iseemdat.gsey=gsey(wne0)

cdf_var='THEO_GSE_Z'
;print,cdf_var
loadcdf,cdf_file,cdf_var,gsez
iseemdat.gsez=gsez(wne0)

cdf_var='UOUT'
loadcdf,cdf_file,cdf_var,uout
iseemdat.uout=transpose(uout(wne0,*))*1e-5

cdf_var='FNOUT'
loadcdf,cdf_file,cdf_var,fnout
iseemdat.fnout=fnout(wne0)

cdf_var='FN'
loadcdf,cdf_file,cdf_var,fn
iseemdat.fn=transpose(fn(wne0,*))

cdf_var='FPOUT'
loadcdf,cdf_file,cdf_var,fpout
iseemdat.fpout=transpose(fpout(wne0,*))

cdf_var='FHOUT'
loadcdf,cdf_file,cdf_var,fhout
iseemdat.fhout=transpose(fhout(wne0,*))

cdf_var='TROUT'
loadcdf,cdf_file,cdf_var,trout
iseemdat.trout=trout(wne0)

cdf_var='ETTRT'
loadcdf,cdf_file,cdf_var,ettrt
iseemdat.ettrt=ettrt(wne0)

cdf_var='DEIG'
loadcdf,cdf_file,cdf_var,deig
iseemdat.deig=deig(wne0)

cdf_var='COSB'
loadcdf,cdf_file,cdf_var,cosb
iseemdat.cosb=cosb(wne0)

cdf_var='EIGVAL'
loadcdf,cdf_file,cdf_var,eigval
iseemdat.eigval=transpose(eigval(wne0,*))

cdf_var='POTNEW'
loadcdf,cdf_file,cdf_var,potnew
iseemdat.potnew=potnew(wne0)

cdf_var='VPOTT'
loadcdf,cdf_file,cdf_var,vpott

cdf_var='BX'
loadcdf,cdf_file,cdf_var,bx
iseemdat.b(0)=bx(wne0)

cdf_var='BY'
loadcdf,cdf_file,cdf_var,by
iseemdat.b(1)=by(wne0)

cdf_var='BZ'
loadcdf,cdf_file,cdf_var,bz
iseemdat.b(2)=bz(wne0)

cdf_var='INSET'
loadcdf,cdf_file,cdf_var,inset
iseemdat.inset=inset(wne0)

cdf_var='ILOFLG'
loadcdf,cdf_file,cdf_var,iloflg
iseemdat.iloflg=iloflg(wne0)


print,'end reading file ',filename

;computed from input data

  iseemdat.recn=lindgen(ndat)

  iseemdat.tpb5(0)=1900l+iseemdat.year
  iseemdat.tpb5(1)=iseemdat.dday+1
  iseemdat.tpb5(2)=iseemdat.msec

  ;NOTE! The isee epoch time will be chosen as pb5 [1977, 0, 0].
  ;The available isee data is within the period 1977-79.
  ;iseemdat.ta=seconds from epoch

  for i=0,ndat-1 do iseemdat(i).ta=isee_pb5_sec(iseemdat(i).tpb5)

  iseemdat.umag=sqrt(iseemdat.uout(0)^2+iseemdat.uout(1)^2+iseemdat.uout(2)^2)
  iseemdat.theu=asin(iseemdat.uout(2)/iseemdat.umag)/!dtor
  iseemdat.phiu=atan(iseemdat.uout(1),iseemdat.uout(0))/!dtor
     wphlt0=where(iseemdat.phiu lt 0,nwphlt0)
     if nwphlt0 gt 0 then iseemdat(wphlt0).phiu=iseemdat(wphlt0).phiu+360.

  iseemdat.hmag=$
    sqrt(iseemdat.fhout(0)^2+iseemdat.fhout(1)^2+iseemdat.fhout(2)^2)
  iseemdat.theh=asin(iseemdat.fhout(2)/iseemdat.hmag)/!dtor
  iseemdat.phih=atan(iseemdat.fhout(1),iseemdat.fhout(0))/!dtor
     wphlt0=where(iseemdat.phih lt 0,nwphlt0)
     if nwphlt0 gt 0 then iseemdat(wphlt0).phih=iseemdat(wphlt0).phih+360.

  fill=-1e8
  wbok=where(iseemdat.b(0) gt fill,nwbok)
  if wbok(0) ne -1 then begin
    iseemdat(wbok).bmag=$
      sqrt(iseemdat(wbok).b(0)^2+iseemdat(wbok).b(1)^2+iseemdat(wbok).b(2)^2)
    iseemdat(wbok).theb=asin(iseemdat(wbok).b(2)/iseemdat(wbok).bmag)/!dtor
    iseemdat(wbok).phib=atan(iseemdat(wbok).b(1),iseemdat(wbok).b(0))/!dtor
    wbylt0=where(iseemdat.b(0) gt fill and iseemdat.b(1) lt 0,nwbylt0)
    if nwbylt0 gt 0 then iseemdat(wbylt0).phib=iseemdat(wbylt0).phib+360.
  endif

  iseemdat.qdotb= (iseemdat.fhout(0)*iseemdat.b(0)+ $
            iseemdat.fhout(1)*iseemdat.b(1)+ $
            iseemdat.fhout(2)*iseemdat.b(2))/$
            (iseemdat.bmag * iseemdat.hmag)


d=create_struct(d,'iseemdat',iseemdat)

end


