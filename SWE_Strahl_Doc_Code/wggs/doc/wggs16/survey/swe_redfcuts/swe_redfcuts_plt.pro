pro swe_redfcuts_plt,indx,npl

common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns

i=indx

swe_redfcuts_img,pos=pos(*,i),ytitle=pm(i).labl,$
   rlbl='log_F',$
   xrange=pm(i).tmrange, xtickv=pm(i).tmtickv(0:pm(i).tmticks),$
   xtickn=xtickn(*,i),subtitle=stitle(i),$
   xticks=pm(i).tmticks, xtitle=xlabl(i) ,xminor=pm(i).tminor, title=ztitle(i),$
   charsize=pm(i).charsize,charthick=pm(i).charthick,xcharsize=xcharsize(i)

    
end