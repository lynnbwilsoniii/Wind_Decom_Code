pro mfi_mag3s_plt,indx,npl


common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns

idatype=where(d.datype eq 'mfi_mag3s')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif
  
fill =-1.0e31

i=indx

hrday=(d.mfi_mag3sdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d

;mag3svar returns data (mag3svbl) crrspndng to selctd variable (pm(i).varname)
  mag3svbl=mfi_mag3svar(pm(i).varname)

;if necessary, reduce array sizes
  ntlim=wst.rebin_size_line
  if n_elements(hrday) gt ntlim then begin          
    sclfctr=fix(float(n_elements(hrday))/float(ntlim)+0.5)
    ntmx=n_elements(hrday)/sclfctr
    case wst.rebin of
    0: begin
         time=congrid(hrday,ntmx,/interp)
         varbl=congrid(mag3svbl,ntmx,/interp)
       endcase
    else : begin
         time=hrday
         varbl=mag3svbl
           endcase  
    endcase
  endif else begin
    time=hrday
    varbl=mag3svbl
  endelse 
help,hrday,time

if wst.spikesout eq 1 then $
  mag3svbl=median(temporary(mag3svbl),wst.smoothwindow)  ;smoothing

;determine if line plot min-max is to be used     
  if wst.minmax eq 0 then begin
    yrange=pm(i).range & ynozero=0
  endif else begin
    ymn=min(mag3svbl,max=ymx) 
    yrange=[ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero=1
  endelse

if pm(i).plotio eq 0 or wst.minmax eq 1 then begin  ;not log plot
   plot,$
     time,varbl,$
     title=ztitle(i),subtitle=stitle(i),$
     xrange=pm(i).tmrange,  xticks=pm(i).tmticks,  xstyle=1,xtitle=xlabl(i),$
     xtickname=xtickn(*,i),xtickv=pm(i).tmtickv(0:pm(i).tmticks),$
     xminor=pm(i).tminor,$
     yrange=yrange,yticks=pm(i).ticks,ystyle=1,ytitle=pm(i).labl,$
     ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
     psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
     position=pos(*,i),ynozero=ynozero,xticklen=0.05,$
     charsize=pm(i).charsize,xcharsize=xcharsize(i),charthick=pm(i).charthick
    
if pm(i).labl eq 'X fshck (i+3sm)' then oplot,$
   [d.mfi_mag3sdat(d.ndx(0,idatype)).ta-d.refsec,$
   d.mfi_mag3sdat(d.ndx(1,idatype)).ta-d.refsec]/3600.d,[0,0],$
   linestyle=1,color=wst.clr_green
                
endif else begin     ;yes log plot
   plot_io,$
     time,varbl,$
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
          
if idlsave then begin    ;saving plotted data this panel to save to a file
  idlsav(i).datatype=pm(i).dtp
  idlsav(i).varname=pm(i).labl
  idlsav(i).log=pm(i).plotio
  idlsav(i).psym=pm(i).psym
  idlsav(i).x=hrday
  idlsav(i).y=mag3svbl
  idlsav(i).yrange=yrange
  idlsav(i).yticks=pm(i).ticks
  idlsav(i).yminor=pm(i).minor 
endif
  
end    