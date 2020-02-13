pro isee_moments_plt,indx,npl

common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
  
idatype=where(d.datype eq 'isee_moments')

i=indx  
  
hrday=(d.iseemdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d

;mvar returns the data (mvbl) correspndng to selctd variable (pm(i).varname) 
  mvbl=isee_momentsvar(pm(i).varname)
  
if wst.spikesout gt 0 then mvbl=median(temporary(mvbl),wst.spikesout);smoothing
  
;determine if line plot min-max is to be used   
  if wst.minmax eq 0 then begin
    yrange=pm(i).range & ynozero=0
  endif else begin
    ymn=min(mvbl,max=ymx) 
    yrange=[ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero=1
  endelse

if pm(i).plotio eq 0 or wst.minmax eq 1 then begin ;not log plot
   plot,$
     hrday,mvbl,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
     xminor=pm(i).tminor,charsize=pm(i).charsize,$
     yrange=yrange,yticks=pm(i).ticks,ystyle=1,ytitle=pm(i).labl,$
     ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
     psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
     position=pos(*,i),ynozero=ynozero,xticklen=0.05,xcharsize=xcharsize(i)

  if pm(i).labl eq 'X fshck' then oplot,$
     [d.iseemdat(d.ndx(0,idatype)).ta-d.refsec,$
     d.iseemdat(d.ndx(1,idatype)).ta-d.refsec]/3600.d,[0,0],$
     linestyle=1,color=wst.clr_green
             
endif else begin   ;yes log plot
   plot_io,$
     hrday,mvbl,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(i).tmticks),$
     xminor=pm(i).tminor,$
     yrange=yrange,ystyle=1,ytitle=pm(i).labl,$
     ytickname=pm(i).tickname,ytickv=pm(i).tickv,$
     psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
     position=pos(*,i),xticklen=0.05,$
     charsize=pm(i).charsize,xcharsize=xcharsize(i),charthick=pm(i).charthick
endelse 
  
  
if keyword_set(oplot_sec) ne 0 and lzwin then begin
  hrday_oplt=(oplot_sec-d.refsec)/3600.d
  oplot,[hrday_oplt,hrday_oplt],yrange,color=175
endif
  
if idlsave then begin  ;saving plotted data this panel to save to a file
  idlsav(i).datatype=pm(i).dtp
  idlsav(i).varname=pm(i).labl
  idlsav(i).log=pm(i).plotio
  idlsav(i).psym=pm(i).psym
  idlsav(i).x=(d.iseemdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d
  idlsav(i).y=mvbl
  idlsav(i).yrange=yrange
  idlsav(i).yticks=pm(i).ticks
  idlsav(i).yminor=pm(i).minor    
endif

end