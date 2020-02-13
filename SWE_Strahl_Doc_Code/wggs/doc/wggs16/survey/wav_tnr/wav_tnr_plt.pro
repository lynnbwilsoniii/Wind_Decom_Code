pro wav_tnr_plt,indx,npl

common shared,d
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
  
i=indx

idatype=where(d.datype eq 'wav_tnr')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

flnm=d.flnm(idatype)

date_flnm=long(strmid(flnm,strlen(flnm)-12,8))
date_data=pb5_ymd(d.wav_tnr_spctrm.tpb5(*,d.ndx(0,idatype)))
;if date_flnm ne date_data then stop,'date inconsistency'

wav_tnr_img,d.wav_tnr_spctrm.z(d.ndx(0,idatype):d.ndx(1,idatype),*),$
  (d.wav_tnr_spctrm.ta(d.ndx(0,idatype):d.ndx(1,idatype))-d.refsec)/3600.d,$
  date_flnm,pos=pos(*,i),xrange=pm(i).tmrange,$
  xtickv=pm(i).tmtickv(0:pm(i).tmticks),xtickn=xtickn(*,i),$
  subtitle=stitle(i),$
  xticks=pm(i).tmticks, xtitle=xlabl(i) ,title=ztitle(i),$
  charsize=pm(i).charsize,charthick=pm(i).charthick,xcharsize=xcharsize(i)  
    
end