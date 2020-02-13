pro swe_strahl_plt,indx,npl
common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
common sharetimeslice,oplot_sec_slice

fill =-1.0e31

idatype=where(d.datype eq 'swe_strahl')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

i=indx 

sdat=d.swe_strahldat(d.ndx(0,idatype):d.ndx(1,idatype))

if pm(i).varname eq 'strahl energy spectrum' then begin
  ;find the available strahl energy steps
   mn=min(sdat.enstep,max=mx)
   wmn=where(sdat.enstep eq mn)
   wmx=where(sdat.enstep eq mx)
   wh=wmn(0:1)   
   availsteps=sdat(wh(0):wh(1)-1).enstep
   strlenstepsev=long(volt_en_strl(availsteps,/en))
   if strlenstepsev(n_elements(strlenstepsev)-1) ne long(volt_en_strl(mx,/en)) $
     then stop,'bad energy'
   mnstrlstep=pm(i).range(0)
   mxstrlstep=pm(i).range(1) < n_elements(strlenstepsev)-1
   ;stop  
   ;en=strlenstepsev;       2:n_elements(strlenstepsev)-1
   en=strlenstepsev(mnstrlstep:mxstrlstep)  
   enmx=en(n_elements(en)-1)
   enmn=en(0)
   nen=n_elements(en)
   
   thour=(sdat.ta-d.refsec)/3600.d
   ntmx=n_elements(thour)
   thourmx=max(thour,min=thourmn)
   ndmx=ntmx*nen
   xd=dblarr(ndmx)
   yd=dblarr(ndmx)
   zd=fltarr(ndmx)
   
   id=0l
   for k=0,nen-1 do begin
     wen=where(sdat.en eq en(k),nwen)
     xd(id+indgen(nwen))=thour(wen)
     yd(id+indgen(nwen))=sdat(wen).en
     zd(id+indgen(nwen))=sdat(wen).strl
     id=id+nwen
   endfor
   nd=id
   xd=xd(0:nd-1) 
   yd=yd(0:nd-1)
   zd=zd(0:nd-1)
   
   triangulate,xd,yd,tr
   zd(where(zd ne 0))=alog10(zd(where(zd ne 0)))
   nx=120
   ny=60
   xgridsize=(thourmx-thourmn)/(nx-1)
   ygridsize=(enmx-enmn)/(ny-1)
   z=trigrid(xd,yd,zd,tr,[xgridsize,ygridsize],[thourmn,enmn,thourmx,enmx])
   xd=0 & yd=0 & zd=0
   sz=size(z)
   ;if sz(1) ne nx and sz(2) ne ny then stop,'unexpected zgrd dimensions'
   yrange=[enmn,enmx] 
   y=yrange(0)+indgen(ny)*(yrange(1)-yrange(0))/(ny-1)

   if wst.spikesout gt 0 then  $   
     z=median(temporary(z),3);wst.spikesout)  ;smoothing
   
   ;scale non-zero elements of (log) image array to byte scale
   w=where(z ne 0)
   mn=min(z(w),max=mx)
   w0=where(z eq 0)
   if wst.hardcopy then set_plot,'ps'
   n_colors=!d.table_size-1
   z(w)=bytscl(temporary(z(w)),min=mn,max=mx,top=n_colors-1);scale to clrs
   if w0(0) ne -1 then z(w0)=0
   
   ;set plot parameters  
   ytck=4 
   ytickv=yrange(0)+indgen(ytck+1)*(yrange(1)-yrange(0))/ytck ;[100,400,800,1200]
   ytickn=string(ytickv,format='(i4)')
   yminor=4
   
   swe_strahl_spectrum_img, z, y, thour, zmn=mn, zmx=mx,  pos=pos(*,i),  $
     ytickv=ytickv,ytitle=pm(i).labl, $
     ytickn=ytickn,yminor=yminor,logimg=1,$
     xrange=pm(i).tmrange,xminor=pm(i).tminor, $
     xtickv=pm(i).tmtickv(0:pm(0).tmticks), $
     xtickn=xtickn(*,i),$
     subtitle=stitle(i),rlbl='counts',$
     xticks=pm(i).tmticks, xtitle=xlabl(i), $
     title=ztitle(i),$
     charsize=pm(i).charsize,charthick=pm(i).charthick,xcharsize=xcharsize(i),$
     ximsize=!d.x_vsize,yimsize=!d.y_vsize,n_colors=n_colors 

   
endif else begin  ;do line plots at given energy

;strahlvar returns data correspndng to selctd variable (pm(i).varname)

  strahlvbl=swe_strahlvar(pm(i).varname)
  if n_elements(strahlvbl.f) le 1 then begin
    print,'no data to be plotted'
    return
  endif

if wst.spikesout gt 0 then begin    ;smoothing
  ysmoothed=strahlvbl.f
  ysmoothed=median(temporary(ysmoothed),wst.spikesout)
  strahlvbl.f=ysmoothed
endif


;determine if line plot min-max is to be used     
  if wst.minmax eq 0 and pm(i).range(0) ne pm(i).range(1) then begin
    yrange=pm(i).range & ynozero=0
  endif else begin
    ymn=min(strahlvbl.f,max=ymx) 
    ;yrange=[ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero=1
    yrange=[0,ymx+0.05*abs(ymx-ymn)]
  endelse

if pm(i).plotio eq 0 or wst.minmax eq 1 then begin  ;not log plot
   plot,$
     (strahlvbl.ta-d.refsec)/3600.d,strahlvbl.f,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
     xminor=pm(i).tminor,$
     yrange=yrange,yticks=pm(i).ticks,ystyle=1,$
     ytitle=pm(i).labl+' '+string(strahlvbl.energy,format='(i4)'),$
     ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
     psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
     position=pos(*,i),ynozero=ynozero,xticklen=0.05,$
     charsize=pm(i).charsize,xcharsize=xcharsize(i),charthick=pm(i).charthick
                
endif else begin     ;yes log plot
   plot_io,$
     (strahlvbl.ta-d.refsec)/3600.d,strahlvbl.f,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
     xminor=pm(i).tminor,$
     yrange=yrange,ystyle=1,$
     ytitle=pm(i).labl+' '+string(strahlvbl.energy,format='(i4)'),$
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
  idlsav(i).x=(d.swe_strahldat(d.ndx(0,6):d.ndx(1,6)).ta-d.refsec)/3600.d
  idlsav(i).y=strahlvbl
  idlsav(i).yrange=yrange
  idlsav(i).yticks=pm(i).ticks
  idlsav(i).yminor=pm(i).minor 
endif

endelse ;end line plots at given energy


if keyword_set(oplot_sec) ne 0 and lzwin then begin
   hrday_oplt=(oplot_sec-d.refsec)/3600.d
   oplot,[hrday_oplt,hrday_oplt],yrange,color=175
endif 

if keyword_set(oplot_sec_slice) ne 0 then begin
   hrday_oplt_slice=(oplot_sec_slice-d.refsec)/3600.d
   oplot,[hrday_oplt_slice,hrday_oplt_slice],yrange,color=wst.clr_orange
endif

end  