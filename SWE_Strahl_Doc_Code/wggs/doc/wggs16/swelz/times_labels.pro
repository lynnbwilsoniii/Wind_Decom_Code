pro times_labels

common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common wstuff,wst
common swestuff,swest

tjd=long(fix(vsmjf.suntim_vsbl(swest.ispinbl)/86400.d))
sec=vsmjf.suntim_vsbl(swest.ispinbl) - tjd*86400.d
wst.pb5=vsmjf.pb5tim_vsbl(*,swest.ispinbl)
wst.xydata(0)=sec/3600.d
hour_hms,wst.xydata(0),hms,lhms=lhms
wst.hms=hms(0) 
wst.lhms=lhms
spndt= $
   yrmoda(wst.pb5) + ' ' +string(wst.pb5(1),format='(i3)') + ' ' + wst.hms
swest.spndt=spndt(0)
swest.pb5=wst.pb5   
          
print,' ' & print,'proc_fw: timsel',wst.timsel
print,'time selected from lz file: '
print,'lz.recn, swest.ispinbl, vsmjf.suntim_vsbl(swest.ispinbl) (seconds) ',$
      lz.recn,swest.ispinbl,vsmjf.suntim_vsbl(swest.ispinbl)
print,'tjd, sec of day ',tjd,sec
print,'pb5 time ',wst.pb5
print,ymd(wst.pb5),'  ',wst.hms


end