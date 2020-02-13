
pro readisee_f,increment=increment

;common iseestuff,iseemdat,iseefdat,iseerefsec
common iseestuff,iseefdat
common shared,d
common wstuff,wst
common shareisee,iseest

print,'readisee_f :'

idatype=where(d.datype eq 'isee_moments')
filename=d.flnm(idatype)

if filename eq '' then begin
  print,'You must first read a moments file'
  print,'Hit return to continue'
  answ='' & read,answ
  return
endif

if keyword_set(increment) eq 0 then begin  ;secs from isee epoch
  tpoint1=d.refsec+wst.xydata(0)*3600.d 
   
  iseerecn=(indx_begin(d.iseemdat.ta,tpoint1) < n_elements(d.iseemdat)-1) > 0

endif else begin   ;incrementing record

  iseerecn=(iseefdat.recn+increment < (n_elements(d.iseemdat)-1)) > 0
  wst.pb5=d.iseemdat(iseerecn).tpb5
  wst.xydata(0)=double(d.iseemdat(iseerecn).tpb5(2))/3600000.d
  tpoint1=wst.xydata(0)
  hour_hms,wst.xydata(0),hms & wst.hms=hms(0)
  
endelse

print,' ' & print,'selected point in time ',$
    wst.hms,'  ',tpoint1,'  ',wst.pb5
iseest.tpoint1=tpoint1

print,'selected isee record ',iseerecn
  
print,'reading iseedat file ',filename,' ......'

CDF_file=filename

;define isee data structure

iseefdat={datstructf,recn:0l,year:0,dday:0,msec:0l,ta:0.d,tpb5:lonarr(3),$
	mode:0,format:0,spin:0.d,$
	uout:fltarr(3),$
	potnew:0.,$
        b:fltarr(3),$
        inset:0,ifloflg:0,icor:intarr(6),nsect:0,ispeed:0,ipatch:intarr(6),$
        unc:intarr(16,6,6),f:fltarr(18,6,6),cc:dblarr(19),vunit:dblarr(6,18,3)}


cdf_var='YEAR'
loadcdf,cdf_file,cdf_var,year,rec_count=1,rec_start=iseerecn
iseefdat.year=year(0)

cdf_var='DECIMAL DAY'
loadcdf,cdf_file,cdf_var,dday,rec_count=1,rec_start=iseerecn
iseefdat.dday=dday(0)

cdf_var='MILLISEC'
loadcdf,cdf_file,cdf_var,msec,rec_count=1,rec_start=iseerecn
iseefdat.msec=msec(0)

cdf_var='MODE'
loadcdf,cdf_file,cdf_var,mode,rec_count=1,rec_start=iseerecn
iseefdat.mode=mode(0)

cdf_var='FORMAT'
loadcdf,cdf_file,cdf_var,iformat,rec_count=1,rec_start=iseerecn
iseefdat.format=iformat(0)

cdf_var='SPIN PERIOD'
loadcdf,cdf_file,cdf_var,spin,rec_count=1,rec_start=iseerecn
iseefdat.spin=spin(0)

cdf_var='UNCOMPACTED OGM'
loadcdf,cdf_file,cdf_var,unc,rec_count=1,rec_start=iseerecn
iseefdat.unc=unc

cdf_var='F'
loadcdf,cdf_file,cdf_var,f,rec_count=1,rec_start=iseerecn
iseefdat.f(*,*,*)=f

cdf_var='UOUT'
loadcdf,cdf_file,cdf_var,uout,rec_count=1,rec_start=iseerecn
iseefdat.uout=uout*1e-5

cdf_var='CENTER'
loadcdf,cdf_file,cdf_var,center,rec_count=1,rec_start=iseerecn

cdf_var='BN'
loadcdf,cdf_file,cdf_var,bn,rec_count=1,rec_start=iseerecn

cdf_var='CC'
loadcdf,cdf_file,cdf_var,cc,rec_count=1,rec_start=iseerecn
iseefdat.cc=cc

cdf_var='VUNIT'
loadcdf,cdf_file,cdf_var,vunit,rec_count=1,rec_start=iseerecn
iseefdat.vunit=vunit

cdf_var='BX'
loadcdf,cdf_file,cdf_var,bx,rec_count=1,rec_start=iseerecn
iseefdat.b(0)=bx(0)

cdf_var='BY'
loadcdf,cdf_file,cdf_var,by,rec_count=1,rec_start=iseerecn
iseefdat.b(1)=by(0)

cdf_var='BZ'
loadcdf,cdf_file,cdf_var,bz,rec_count=1,rec_start=iseerecn
iseefdat.b(2)=bz(0)

cdf_var='INSET'
loadcdf,cdf_file,cdf_var,inset,rec_count=1,rec_start=iseerecn
iseefdat.inset=inset(0)

cdf_var='ILOFLG'
loadcdf,cdf_file,cdf_var,iloflg,rec_count=1,rec_start=iseerecn
iseefdat.ifloflg=iloflg(0)

cdf_var='ICOR'
loadcdf,cdf_file,cdf_var,icor,rec_count=1,rec_start=iseerecn
iseefdat.icor=icor

cdf_var='NSECT'
loadcdf,cdf_file,cdf_var,nsect,rec_count=1,rec_start=iseerecn
iseefdat.nsect=nsect(0)

cdf_var='ISPEED'
loadcdf,cdf_file,cdf_var,ispeed,rec_count=1,rec_start=iseerecn
iseefdat.ispeed=ispeed(0)

cdf_var='IPATCH'
loadcdf,cdf_file,cdf_var,ipatch,rec_count=1,rec_start=iseerecn
iseefdat.ipatch=ipatch

iseefdat.recn=iseerecn

iseefdat.tpb5(0)=1900l+year
iseefdat.tpb5(1)=dday+1
iseefdat.tpb5(2)=msec

iseefdat.ta=isee_pb5_sec(iseefdat.tpb5)

wst.lz_is_read=1

end


