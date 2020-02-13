pro wav_hrtnr_plt,indx,npl

common shared,d
common sharelevelzero,pltwin_frmt,oplot_sec
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common wstuff,wst
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
  
i=indx

idatype=where(d.datype eq 'wav_hrtnr')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

flnm=d.flnm(idatype)

date_flnm=long(strmid(flnm,strlen(getenv(d.pathenv(idatype))),8))
date_data=pb5_ymd(d.wav_hrtnr_spctrm.tpb5(*,d.ndx(0,idatype)))
if date_flnm ne date_data then stop,'date inconsistency'

wav_hrtnr_img,d.wav_hrtnr_spctrm.z(d.ndx(0,idatype):d.ndx(1,idatype),*),$
  (d.wav_hrtnr_spctrm.ta(d.ndx(0,idatype):d.ndx(1,idatype))-d.refsec)/3600.d,$
  date_flnm,pos=pos(*,i),xrange=pm(i).tmrange,$
  xtickv=pm(i).tmtickv(0:pm(i).tmticks),xtickn=xtickn(*,i),$
  subtitle=stitle(i),$
  xticks=pm(i).tmticks, xtitle=xlabl(i) ,title=ztitle(i),$
  charsize=pm(i).charsize,charthick=pm(i).charthick,xcharsize=xcharsize(i)  

if idlsave then begin  ;saving plotted data this panel to save to a file

idlsav_wav_hrtnr=$
{datatype:'wav_hrtnr',$
 z:d.wav_hrtnr_spctrm.z(d.ndx(0,idatype):d.ndx(1,idatype),*),$
 t:(d.wav_hrtnr_spctrm.ta(d.ndx(0,idatype):d.ndx(1,idatype))-d.refsec)/3600.d,$
 date_flnm:date_flnm,$
 pos:reform(pos(*,i)),$
 xrange:reform(pm(i).tmrange),$
 xtickv:reform(pm(i).tmtickv(0:pm(i).tmticks)),$
 xtickn:reform(xtickn(*,i)),$
 stitle:stitle(i),$
 tmticks:pm(i).tmticks,$
 xlabl:xlabl(i),$
 ztitle:ztitle(i),$
 charsize:pm(i).charsize,$
 charthick:pm(i).charthick,$
 xcharsize:xcharsize(i),$
 fhz1:wst.fhz1,$
 fhz2:wst.fhz2,$
 scale1:wst.scale1,$
 scale2:wst.scale2,$
 fhzmin:d.wav_hrtnr_spctrm.fhzmin,$
 fhzmax:d.wav_hrtnr_spctrm.fhzmax,$
 refsec:d.refsec}
                     
dir=getenv('IDLSAV')  
save,filename=dir+wst.surveydate+'_idlsave_wav_hrtnr.dat',idlsav_wav_hrtnr
print,'plots saved in file ',dir+wst.surveydate+'_idlsave_wav_hrtnr.dat'
;help,idlsav_wav_hrtnr 
;help,idlsav_wav_hrtnr,/str

endif
                          
end
