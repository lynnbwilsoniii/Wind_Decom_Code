pro swe_strahlen_plt,indx,npl

common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns

fill =-1.0e31

idatype=where(d.datype eq 'swe_strahlen')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

i=indx 

sdat=d.swe_strahlendat(d.ndx(0,idatype):d.ndx(1,idatype))

;do line plots at given energy

;strahlenvar returns data correspndng to selctd variable (pm(i).varname)

  strahlenvbl=swe_strahlenvar(pm(i).varname)
  if n_elements(strahlenvbl.f) le 1 then begin
    print,'no data to be plotted'
    return
  endif

if pm(i).varname eq 'strlen' or pm(i).varname eq 'anti_strlen' then begin
  ymx=max(median(strahlenvbl.f,10))
  pm(i).range=[0,1.3*ymx]
endif    
          
if wst.spikesout gt 0 then begin    ;smoothing
  ysmoothed=strahlenvbl.f
  ysmoothed=median(temporary(ysmoothed),wst.spikesout)
  strahlenvbl.f=ysmoothed
endif

;determine if line plot min-max is to be used     
  if wst.minmax eq 0 and pm(i).range(0) ne pm(i).range(1) then begin
    yrange=pm(i).range & ynozero=0
  endif else begin
    ymn=min(strahlenvbl.f,max=ymx) 
    ;yrange=[ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero=1
    yrange=[0,ymx+0.05*abs(ymx-ymn)]
  endelse

enlbl=strcompress(string(strahlenvbl.energy,format='(i4)'),/remove_all)

if pm(i).plotio eq 0 or wst.minmax eq 1 then begin  ;not log plot
   plot,$
     (strahlenvbl.ta-d.refsec)/3600.d,strahlenvbl.f,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
     xminor=pm(i).tminor,$
     yrange=yrange,yticks=pm(i).ticks,ystyle=1,$
     ytitle=pm(i).labl+enlbl,$
     ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
     psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
     position=pos(*,i),ynozero=ynozero,xticklen=0.05,$
     charsize=pm(i).charsize,xcharsize=xcharsize(i),charthick=pm(i).charthick
                
endif else begin     ;yes log plot
   plot_io,$
     (strahlenvbl.ta-d.refsec)/3600.d,strahlenvbl.f,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
     xminor=pm(i).tminor,$
     yrange=yrange,ystyle=1,$
     ytitle=pm(i).labl+enlbl,$
     ytickname=pm(i).tickname,ytickv=pm(i).tickv,$
     psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
     position=pos(*,i),xticklen=0.05,$
     charsize=pm(i).charsize,xcharsize=xcharsize(i),charthick=pm(i).charthick
endelse
          
if idlsave then begin    ;saving plotted data this panel to save to a file
  idlsav(i).datatype=pm(i).dtp
  idlsav(i).varname=pm(i).labl
  idlsav(i).log=pm(i).plotio
  idlsav(i).psym=pm(i).psym
  idlsav(i).x=(d.swe_strahlendat($
    d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d
  idlsav(i).y=strahlenvbl.f
  idlsav(i).yrange=yrange
  idlsav(i).yticks=pm(i).ticks
  idlsav(i).yminor=pm(i).minor 
endif


if keyword_set(oplot_sec) ne 0 and lzwin then begin
   hrday_oplt=(oplot_sec-d.refsec)/3600.d
   oplot,[hrday_oplt,hrday_oplt],yrange,color=175
endif 

end  