
;======================= triang ===============================================

function triang,x,y,z,nx,ny

;makes irregular data regular for contouring or imaging

xd=x#replicate(1,n_elements(y))
yd=replicate(1,n_elements(x))#y
triangulate,xd,yd,tr
xrange=[x(0),x(n_elements(x)-1)]
yrange=[y(0),y(n_elements(y)-1)]
zgrd=trigrid(xd,yd,z,tr,$
    [(xrange(1)-xrange(0))/(nx-1),(yrange(1)-yrange(0))/(ny-1)],$
    [xrange(0),yrange(0),xrange(1),yrange(1)])

return,zgrd
end



;======================== plt ==================================================
pro plt_lzwindow,group=group,spikesout=spikesout,idlsave=idlsave,timavg=timavg

common sharewidg,wa
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common strlstuff, strlflnm,strldat
common ionkpstuff,ionkpflnm,ionkpdat
common magkpstuff,magkpflnm,magkpdat
common hydspctrmstuff,hydspctrm
common wavestnrstuff,wavestnrflnm,wavestnrspctrm
common iseestuff,iseemdat,iseefdat,iseerefsec
common log_delog,comp_tbl,dcomp_tbl
common shared,d
common test5,strlstep
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec

;temporarily save structure d :
  dsave=d
 
if keyword_set(spikesout) eq 0 then spikesout=0   ;smoothing parameter
if keyword_set(timavg) eq 0 then timavg=0   ;moments time average parameter
if keyword_set(wst.minmax) eq 0 then wst.minmax=0
if keyword_set(idlsave) eq 0 then idlsave=0

start: ;print,'plt_lzwindow'
;if wst.hardcopy(0) eq 0 then erase

;set default color scale (preset rscale each panel)
  if keyword_set(wst.cscale) eq 0 then wst.cscale=0  

!p.charsize=1.35
n_spins=7
fill =-1.0e31

;possible data types are: 
;     d.datype=['moments', 'fspnavg', 'fpitch', 'strahl', 'ionkp', 'magkp']
;selected data types are where 
;     d.wdatype(0,n_elements(d.datype)) ne -1

;selected indices from set of indices d.pnlist.list
  wpnl=where(d.pnlsel ne -1)
  if wpnl(0) eq -1 then begin
    print,'no plot selection has been made'
    return
  endif

  ;plot only PANEL # wst.lzsurvey
 
    pnlsel=d.pnlsel(wpnl(swest.lzsurvey))
    
  ;print,pnlsel

;structure of plotting parameters for selected plot variables 
   pm=d.pnl(pnlsel)
   ;help,pm,/str

;number of plotvariables selected
   npl=n_elements(pm)

;get reference time in pb5time
  refpb5=sec_pb5(refsec)


;find timegaps of spectrum and pitch angle image arrays 
  if d.wdatype(0,1) ne -1 or d.wdatype(0,2) ne -1 then begin ;if image selctd
    ;the set of indices, elaps_spns, corresponds to elapsed spins, incl gaps
      elaps_spns=pdat(d.ndx(0,1):d.ndx(1,1)).elapspn
  endif else elaps_spns=0

;cf=0 if images in compressed (12 to 8 bit) counts
;cf=1 if images to be converted to phase density
   if wst.cf eq 0 then rlbl='cmpr_cts' else rlbl='log_f'

;some initialization
   wst.xyrange=fltarr(4,wst.nplmx) & wst.xywindow=fltarr(4,wst.nplmx) 
   wst.xysize=fltarr(4,wst.nplmx) & wst.ylog=intarr(wst.nplmx)

;find normal coordinates of plot panel corners 
   pos=fltarr(4,npl)
   pos,npl,pos,yoff=0.2,ytop=0.9,xoff=0.1,xtop=0.88
   ;print,'pos',pos

;define labels
  xlabl=strarr(npl) ;& xlabl(npl-1)=pm(0).tmlabl
  ztitle=strarr(npl) 
  ztitle(0)=pm(0).tmlabl  ;pm(0).ztitle
  stitle=strarr(npl) ;& stitle(npl-1)=pm(0).subtitle

;define some plotting parameters
   xtickn=strarr(pm(0).tmticks+1,npl)
   for i=0,npl-1 do xtickn(*,i)=replicate(string(32b),pm(0).tmticks+1)
   xtickn(*,npl-1)=pm(0).tmtickname(0:pm(0).tmticks)
   xtickv=pm(0).tmtickv(0:pm(0).tmticks)
   xrange=pm(0).tmrange
   xticks=pm(0).tmticks
   xminor=pm(0).tminor
   noerase=1+intarr(npl) & noerase(0)=0

if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0
;if wst.hardcopy(0) then begin 
;  print,' ' & print,'making hardcopy... ' 
  ;dirhc=getenv('SWEDATLIB')
  ;restore,filename=dirhc+'hardcopycolors.dat'
  ;tvlct,rnew,gnew,bnew
;  set_plot,'ps',/interpolate
;  pltfil=getenv('IDLSAV')+wst.print_flnm 
;  print,'pltfil',pltfil

;  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5,/color,$
;     filename=pltfil;,bits_per_pixel=8
  ;device,/inches,/landscape,filename=pltfil
;endif

;if electron moments or ionkp's are selected then form structure to
;contain plotted moments and ionkp parameters to save in a file
  if d.wdatype(0,0) ne -1 or d.wdatype(0,4) ne -1 then begin

    nsav=0
    if d.wdatype(0,0) ne -1 then $
      nsav=nsav+n_elements(d.wdatype(where(d.wdatype(*,0) ne -1),0))
    if d.wdatype(0,4) ne -1 then $
      nsav=nsav+n_elements(d.wdatype(where(d.wdatype(*,4) ne -1),4))
    idlsav=replicate({datatype:'',$
                      varname:'',$
                      log:0,$
                      psym:0,$
                      x:dblarr(d.ndx(1,0)-d.ndx(0,0)+1),$
                      y:fltarr(d.ndx(1,0)-d.ndx(0,0)+1),$
                      yrange:fltarr(2),$
                      yticks:0,$
                      yminor:0,$
                      refpb5:refpb5,  $
                      xrange:pm(0).tmrange,$
                      xtickname:pm(0).tmtickname(0:pm(0).tmticks),$
                      xticks:pm(0).tmticks,$
                      xtickv:pm(0).tmtickv(0:pm(0).tmticks),$
                      xminor:pm(0).tminor,$
                      xtitle:pm(0).tmlabl,$
                      subtitle:pm(0).subtitle,$
                      title:pm(0).ztitle $
                      },nsav)
  endif

;----------------- end preliminaries ----------------------------------------



;------------------ begin plot loop over data selected variables, ------------                ;------------------ testing each variable for data type           ------------
firstmom=1 
for i=0,npl-1 do begin

if i eq npl-1 then xcharsize=1.0 else xcharsize=0.001

case pm(i).dtp of

d.datype(0): begin   ;moments plots
  ;print,d.datype(0),pnlsel(i),pm(i).dtp,pm(i).labl,pm(i).ev_val

  hrday=(mdat(d.ndx(0,0):d.ndx(1,0)).ta-refsec)/3600.d

  ;mvar returns the data (mvbl) correspndng to selctd variable (pm(i).varname) 
    mvbl=mvar(pm(i).varname)
  if spikesout eq 1 then mvbl=median(temporary(mvbl),wst.smoothwindow);smoothing
  if timavg then begin    ;time average
    minavg=1.d/60.d       ;do 1min average
    if firstmom then timavg,hrday,mvbl,minavg,xav,yav,first=1 $
    else timavg,hrday,mvbl,minavg,xav,yav
    hrday=xav & mvbl=yav
    if i eq 0 then ztitle(0)=ztitle(0)+' (1min avg)' 
  endif
  firstmom=0

  ;if pm(i).oplot then begin      ;overplotting
   ; ;omvbl=mvar(pm(i).oplotvar)
  ;  omvbl=ionkpvar(pm(i).oplotvar)
  ;  if spikesout eq 1 then omvbl=median(temporary(omvbl),wst.smoothwindow)
 ; endif

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
           xrange=xrange,  xticks=xticks,  xstyle=1,xtitle=xlabl(i),$
           xtickname=xtickn(*,i),xtickv=xtickv,xminor=xminor,$
           yrange=yrange,yticks=pm(i).ticks,ystyle=1,ytitle=pm(i).labl,$
           ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
           psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
           position=pos(*,i),ynozero=ynozero,xticklen=0.05,$
           charsize=pm(i).charsize,xcharsize=xcharsize,charthick=pm(i).charthick
         ;if pm(i).oplot then $
         ;    oplot,(mdat(d.ndx(0,0):d.ndx(1,0)).ta-refsec)/3600.d,omvbl,$
         ;    linestyle=pm(i).olinestyle,color=pm(i).ocolor
            
  endif else begin   ;yes log plot
         plot_io,$
           hrday,mvbl,$
           title=ztitle(i),subtitle=stitle(i),$
           xrange=xrange,  xticks=xticks,  xstyle=1,xtitle=xlabl(i),$
           xtickname=xtickn(*,i),xtickv=xtickv,xminor=xminor,$
           yrange=yrange,ystyle=1,ytitle=pm(i).labl,$
           ytickname=pm(i).tickname,ytickv=pm(i).tickv,$
           psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
           position=pos(*,i),xticklen=0.05,$
           charsize=pm(i).charsize,xcharsize=xcharsize,charthick=pm(i).charthick
         ;if pm(i).oplot then $
         ;    oplot,(mdat(d.ndx(0,0):d.ndx(1,0)).ta-refsec)/3600.d,omvbl,$
         ;    linestyle=pm(i).olinestyle,color=pm(i).ocolor
  endelse 

  if keyword_set(oplot_sec) ne 0 then begin
    hrday_oplt=(oplot_sec-refsec)/3600.d
    oplot,[hrday_oplt,hrday_oplt],yrange,color=175
  endif
  if idlsave then begin  ;saving plotted data this panel to save to a file
    idlsav(i).datatype=pm(i).dtp
    idlsav(i).varname=pm(i).labl
    idlsav(i).log=pm(i).plotio
    idlsav(i).psym=pm(i).psym
    idlsav(i).x=(mdat(d.ndx(0,0):d.ndx(1,0)).ta-refsec)/3600.d
    idlsav(i).y=mvbl
    idlsav(i).yrange=yrange
    idlsav(i).yticks=pm(i).ticks
    idlsav(i).yminor=pm(i).minor    
  endif

endcase  ;end moments


;============== WAVES TNR dynamic spectrum ================================
d.datype(9): begin 
  print,'WAVES TNR spectrum' 
  yymmdd=long(strmid(d.flnm(9),strlen(d.flnm(9))-6,6))
  yymmddc=pb5_ymd(wavestnrspctrm.tpb5(*,d.ndx(0,9)))
  
  if 19000000l+yymmdd ne yymmddc then stop,'date inconsistency'
  img_wavestnr,wavestnrspctrm.z(d.ndx(0,9):d.ndx(1,9),*),$
  (wavestnrspctrm.ta(d.ndx(0,9):d.ndx(1,9))-refsec)/3600.d,$
    yymmdd,pos=pos(*,i),xrange=xrange,xtickv=xtickv,xtickn=xtickn(*,i),subtitle=stitle(i),$
    xticks=xticks, xtitle=xlabl(i) ,title=ztitle(i),$
    charsize=pm(i).charsize,charthick=pm(i).charthick,xcharsize=xcharsize
    
    
endcase

;============== end WAVES TNR dynamic spectrum ================================

d.datype(1): begin;  fspinavg image (energy spectrum) 

   ;set of energy steps of spectum plotted = indgen(wst.nvmax_spnavg)
   ;wst.nvmax_spnavg may be set to a value less than total number of steps
   if keyword_set(wst.enmax_spnavg) eq 0 then wst.enmax_spnavg=$
     volt_en(pdat(d.ndx(0,1)).vsteps(n_elements(pdat(d.ndx(0,1)).vsteps)-1),/en)
   wst.enmax_spnavg = wst.enmax_spnavg < $
     volt_en(pdat(d.ndx(0,1)).vsteps(n_elements(pdat(d.ndx(0,1)).vsteps)-1),/en)
   w=where(long(volt_en(pdat(d.ndx(0,1)).vsteps,/en)) le wst.enmax_spnavg,nw)
   if nw gt 0 then wst.nvmax_spnavg=nw
   
   widget_control,wa.field(13),set_value=wst.enmax_spnavg

   ;print,'d.datype(1),pm(i).dtp,pm(i).labl',d.datype(1),pm(i).dtp,pm(i).labl

   ;pdat.fspa=input spectrum array, dim(en steps,time indices)
   ;g=plotted image array
     g=transpose(pdat(d.ndx(0,1):d.ndx(1,1)).fspa(0:wst.nvmax_spnavg-1))

   ;if spikesout eq 1 then g=median(temporary(g),wst.smoothwindow)   ;smoothing

   ;if converting compressed counts to phase density
     if wst.cf eq 1 then $
       g=cts_fspnavg( temporary(g), $
        pdat(d.ndx(0,1)).vsteps(0:wst.nvmax_spnavg-1))

   ;determine whether image min-max or preselected scale to be used
   case wst.cscale of          
     0: begin  ;preset
           mn=pm(i).lzrange(0,wst.cf)
           mx=pm(i).lzrange(1,wst.cf)        
        endcase
     1: begin     ;min-max of selected panel
          mn=min(g(where(g ne 0)),max=mx)
          print,'img: mn,mx ',mn,mx
        endcase 
     2: begin
          mn=pm(i).lzrange(0,wst.cf)
          mx=wst.newclrmax < pm(i).lzrange(1,wst.cf)
          widget_control,wa.field(14),$
            set_value=string(wst.newclrmax,format='(i3)')
        endcase
     else:
   endcase

   ;scale non-zero elements of image array to byte scale
     w0=where(g eq 0)
     g=bytscl(temporary(g),min=mn,max=mx,top=!d.table_size-2);scale to  colors
     if w0(0) ne -1 then g(w0)=0

   ;get y (energy) scale
     y=alog10(volt_en(pdat(d.ndx(0,1)).vsteps(0:wst.nvmax_spnavg-1),/en))
     ymn=min(y,max=ymx) & ytck=4


   ;image plot procedure
   img_fp, g, y, (pdat(d.ndx(0,1):d.ndx(1,1)).ta-refsec)/3600.d, $
      pos=pos(*,i),zmn=mn, zmx=mx, rlbl=rlbl,$
      ytickn=string(10.^(ymn+indgen(ytck+1)*(ymx-ymn)/ytck),format='(i4)'),$
      ytitle=pm(i).labl, ytickv=ymn+indgen(ytck+1)*(ymx-ymn)/ytck,$
      xrange=xrange, xtickv=xtickv,xtickn=xtickn(*,i),subtitle=stitle(i),$
      xticks=xticks, xtitle=xlabl(i) ,xminor=1, title=ztitle(i),$
      elaps_spns=elaps_spns,charsize=pm(i).charsize,charthick=pm(i).charthick,$
      xcharsize=xcharsize

   endcase   ;end energy spectrum


d.datype(6): begin     ;hydra spectrum

    print,'HYDRA spectrum'
    ;d.ndx(0,6):d.ndx(1,6) = selected time index range
    g=hydspctrm.spectrum(d.ndx(0,6):d.ndx(1,6),*)    ;image array
    x=(hydspctrm.ta(d.ndx(0,6):d.ndx(1,6))-refsec)/3600.d   ;selected time

   ;set zero elements to the minimum
   w0=where(g eq 0)
   if w0(0) ne -1 then g(w0)=hydspctrm.min_e  ;!! assumes min_e = min_i
    
   n_colors=!d.table_size-1    ;127
   
   ;scale image array to color scale, electrons and ions separately
     g(*,0:54)=index_scale(g(*,0:54),hydspctrm.min_e,hydspctrm.max_e,$
         n_colors-1,inverse_ele)
     g(*,55:109)=index_scale(g(*,55:109),hydspctrm.min_i,hydspctrm.max_i,$
         n_colors-1,inverse_ion)

  ;get color scale axis 
    elescal=10^(inverse_ele)
    ionscal=10^(inverse_ion)

   ;get y (energy) scale
     ytitle = 'Electron Energy (eV)               Ion Energy (eV)'
     y=findgen(hydspctrm.nen)
     ytickv = [0,20,40,60,80,100]  ;[0,10,20,35,45,55,65]
     ytickname = [strtrim(hydspctrm.energy(0),2),$
                  strtrim(hydspctrm.energy(20),2), $
                  strtrim(hydspctrm.energy(40),2),$
                  strtrim(hydspctrm.energy(60),2), $
                  strtrim(hydspctrm.energy(80),2),$
                  strtrim(hydspctrm.energy(100),2)]
     yticklen=-0.02

   mn=[hydspctrm.min_e,hydspctrm.min_i]
   mx=[hydspctrm.max_e,hydspctrm.max_i]

   ;image plot procedure
     img_fp_hyd, g, y, x, $
       pos=pos(*,i),zmn=mn, zmx=mx, rlbl='',$
       elescal=elescal,ionscal=ionscal,$
       ytickn=ytickname,$
       ytitle=ytitle, ytickv=ytickv,$
       xrange=xrange, xtickv=xtickv,xtickn=xtickn(*,i),subtitle='',$
       xticks=xticks, xtitle=xlabl(i) ,xminor=1, title=hydspctrm.title,$
       ispin_gaps=0,charsize=pm(i).charsize,charthick=pm(i).charthick,$
       xcharsize=xcharsize,n_colors=n_colors

             endcase   ;end 


d.datype(2): begin  ;   fpitch image
   print,'d.datype(2),pm(i).dtp,pm(i).labl,pm(i).ev_val',$
     'volt_en(pdat(d.ndx(0,2)).vsteps(pm(i).ev_val),/en)',$
     d.datype(2),pm(i).dtp,pm(i).labl,pm(i).ev_val,$
     volt_en(pdat(d.ndx(0,2)).vsteps(pm(i).ev_val),/en)

   ;pdat.f=pitch angle array, dim(pitch angle indices,en steps,time indices)
      sz=size(pdat.f)

   ;get array of pitch angle bins 
      dp=(180./sz(1))
      pabin=byte(fix(float(dp)/2)+indgen(sz(1))*dp)

   ;g=pitch angle plot array for selected en step=pm(i).ev_val  
      g=transpose(pdat(d.ndx(0,2):d.ndx(1,2)).f(*,pm(i).ev_val))

   ;if spikesout eq 1 then g=median(temporary(g),wst.smoothwindow)  ;smoothing

   ;if converting compressed counts to phase density
      if wst.cf eq 1 then g=cts_fpitch(temporary(g),pm(i).ev_val)

   ;determine whether image min-max or preselected scale to be used
      case wst.cscale of
        0: begin   ;preset 
             mn=pm(i).lzrange(0,wst.cf) 
             mx=pm(i).lzrange(1,wst.cf) 
           endcase
        1: begin     ;min-max of selected panel
             mn=min(g(where(g ne 0)),max=mx)
             print,'plt: mn,mx ',mn,mx
           endcase
        2: begin
          mn=pm(i).lzrange(0,wst.cf) 
          mx=wst.newclrmax < pm(i).lzrange(1,wst.cf)
          widget_control,wa.field(14),$
            set_value=string(wst.newclrmax,format='(i3)')
        endcase 
        else:
      endcase

   ;scale non-zero elements of image array to byte scale  
      w0=where(g eq 0)
      g=bytscl(temporary(g),min=mn,max=mx,top=!d.table_size-2);scale to  colors
      if w0(0) ne -1 then g(w0)=0


   ;image plot procedure
   img_fp, g,pabin,(pdat(d.ndx(0,2):d.ndx(1,2)).ta-refsec)/3600.d, $
      pos=pos(*,i),   $
      ytitle=pm(i).labl, rlbl=rlbl,ytickv=[0,90,180],$
      ytickn=string([0,90,180],format='(i3)'),$
      xrange=xrange, xtickv=xtickv,xtickn=xtickn(*,i),zmn=mn, zmx=mx,$
      xticks=xticks, xtitle=xlabl(i) ,xminor=xminor, title=ztitle(i),$
      subtitle=stitle(i),elaps_spns=elaps_spns,$
      charsize=pm(i).charsize,charthick=pm(i).charthick,xcharsize=xcharsize
   
endcase  ;end pitch angle image

d.datype(3): begin  ;   strahl fpitch image
     print,'d.datype(3),pm(i).dtp,pm(i).labl,pm(i).ev_val',$
     d.datype(3),pm(i).dtp,pm(i).labl,pm(i).ev_val
     
   sz=size(strldat.f)
   dp=(180./sz(1))
   strlpabin=byte( dp/2 + indgen(sz(1)) * dp )
   strl_times=d.ndx(0,3) + indgen(d.ndx(1,3)-d.ndx(0,3)+1)
   wv=$
   where(volt_en_strl(strldat(strl_times).vstep,/en) eq strlstep(pm(i).ev_val))
   if wv(0) ne -1 then begin
     g= transpose(strldat(strl_times(wv)).f)
     ;if spikesout eq 1 then g=median(temporary(g),wst.smoothwindow)
     if wst.cf eq 1 then begin
       wt=transpose(strldat(strl_times(wv)).wt)
       g=ctspbin_f_strl(temporary(g),pm(i).ev_val,wt)
     endif
     help,'g,strlpabin,strldat(strl_times(wv)).ta',$
           g,strlpabin,strldat(strl_times(wv)).ta

   case wst.cscale of
      0: begin   ;preset 
           mn=pm(i).lzrange(0,wst.cf) 
           mx=pm(i).lzrange(1,wst.cf) 
         endcase
      1: begin     ;min-max of selected panel
           mn=min(g(where(g ne 0)),max=mx)
           print,'plt: mn,mx ',mn,mx
         endcase 
      2: begin
          mn=pm(i).lzrange(0,wst.cf)
          mx=wst.newclrmax < pm(i).lzrange(1,wst.cf)
          widget_control,wa.field(14),$
            set_value=string(wst.newclrmax,format='(i3)')
        endcase
     else:
   endcase

     strlmn=min(g(where(g ne 0)),max=strlmx)  ;triangulate creates color spikes
     tm=(strldat(strl_times(wv)).ta-refsec)/3600.d
     g=triang(tm,strlpabin,temporary(g),$
         (tm(n_elements(tm)-1)-tm(0))*3600./6.25,n_elements(strlpabin) )
     wout=where(g gt strlmx or g lt strlmn)
     if wout(0) ne -1 then g(wout)=0
     w0=where(g eq 0)
     g=bytscl(temporary(g),min=mn,max=mx,top=!d.table_size-2)
     if w0(0) ne -1 then g(w0)=0


     img_fp,g,strlpabin,tm, $
       pos=pos(*,i),   $
       ytitle=pm(i).labl, rlbl=rlbl,ytickv=[0,90,180],$
       ytickn=string([0,90,180],format='(i3)'),$
       xrange=xrange, xtickv=xtickv,xtickn=xtickn(*,i),zmn=mn, zmx=mx,$
       xticks=xticks, xtitle=xlabl(i) ,xminor=xminor, title=ztitle(i),$
       subtitle=stitle(i),ispin_gaps=0,$
       charsize=pm(i).charsize,xcharsize=xcharsize,charthick=pm(i).charthick
   endif
endcase

d.datype(4):  begin   ;ion kp plots
  ;print,d.datype(4),pnlsel(i),pm(i).dtp,pm(i).labl,pm(i).ev_val
  
  ;ionkpvar returns data (ikpvbl) correspndng to selctd variable (pm(i).varname)
    ikpvbl=ionkpvar(pm(i).varname)

  ;determine if line plot min-max is to be used 
    if wst.minmax eq 0 then begin
      yrange=pm(i).range & ynozero=0
    endif else begin
      ymn=min(ikpvbl(where(ikpvbl ne fill)),max=ymx) 
      yrange=[ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero=1
    endelse


    if pm(i).plotio eq 0 or wst.minmax eq 1 then begin    ;not log plot           
         plot,$
           ionkpdat(d.ndx(0,4):d.ndx(1,4)).hrday,ikpvbl,$
           title=ztitle(i),subtitle=stitle(i),$
           xrange=xrange,  xticks=xticks,  xstyle=1,xtitle=xlabl(i),$
           xtickname=xtickn(*,i),xtickv=xtickv,xminor=xminor,$
           yrange=yrange,yticks=pm(i).ticks,ystyle=1,ytitle=pm(i).labl,$
           ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
           psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
           position=pos(*,i),ynozero=ynozero,xticklen=0.05,$
           charsize=pm(i).charsize,xcharsize=xcharsize,charthick=pm(i).charthick
                     
    endif else begin   ;yes log plot
         plot_io,$
           ionkpdat(d.ndx(0,4):d.ndx(1,4)).hrday,ikpvbl,$
           title=ztitle(i),subtitle=stitle(i),$
           xrange=xrange,  xticks=xticks,  xstyle=1,xtitle=xlabl(i),$
           xtickname=xtickn(*,i),xtickv=xtickv,xminor=xminor,$
           yrange=yrange,ystyle=1,ytitle=pm(i).labl,$
           ytickname=pm(i).tickname,ytickv=pm(i).tickv,$
           psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
           position=pos(*,i),xticklen=0.05,$
           charsize=pm(i).charsize,xcharsize=xcharsize,charthick=pm(i).charthick
         if pm(i).oplot then $
             oplot,(mdat(d.ndx(0,0):d.ndx(1,0)).ta-refsec)/3600.d,omvbl,$
             linestyle=pm(i).olinestyle,color=pm(i).ocolor
   endelse
   if idlsave then begin   ;saving plotted data this panel to save to a file
     idlsav(i).datatype=pm(i).dtp
     idlsav(i).varname=pm(i).labl
     idlsav(i).log=pm(i).plotio
     idlsav(i).psym=pm(i).psym
     idlsav(i).x=ionkpdat(d.ndx(0,4):d.ndx(1,4)).hrday
     idlsav(i).y=ikpvbl
     idlsav(i).yrange=yrange
     idlsav(i).yticks=pm(i).ticks
     idlsav(i).yminor=pm(i).minor 
   endif

endcase  ;end ionkp


d.datype(5): begin   ;mag kp plots
  ;print,d.datype(5),pnlsel(i),pm(i).dtp,pm(i).labl,pm(i).ev_val

  ;magkpvar returns data (magkpvbl) crrspndng to selctd variable (pm(i).varname)
    magkpvbl=magkpvar(pm(i).varname)

  if spikesout eq 1 then magkpvbl=median(temporary(magkpvbl),wst.smoothwindow)  ;smoothing

  ;determine if line plot min-max is to be used     
    if wst.minmax eq 0 then begin
      yrange=pm(i).range & ynozero=0
    endif else begin
      ymn=min(magkpvbl,max=ymx) 
      yrange=[ymn-0.05*abs(ymx-ymn),ymx+0.05*abs(ymx-ymn)] & ynozero=1
    endelse


  if pm(i).plotio eq 0 or wst.minmax eq 1 then begin  ;not log plot
         plot,$
           (magkpdat(d.ndx(0,5):d.ndx(1,5)).ta-refsec)/3600.d,magkpvbl,$
           title=ztitle(i),subtitle=stitle(i),$
           xrange=xrange,  xticks=xticks,  xstyle=1,xtitle=xlabl(i),$
           xtickname=xtickn(*,i),xtickv=xtickv,xminor=xminor,$
           yrange=yrange,yticks=pm(i).ticks,ystyle=1,ytitle=pm(i).labl,$
           ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
           psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
           position=pos(*,i),ynozero=ynozero,xticklen=0.05,$
           charsize=pm(i).charsize,xcharsize=xcharsize,charthick=pm(i).charthick
          
         
   if keyword_set(oplot_sec) ne 0 then begin
     hrday_oplt=(oplot_sec-refsec)/3600.d
     oplot,[hrday_oplt,hrday_oplt],yrange,color=175
   endif
       
  endif else begin     ;yes log plot
         plot_io,$
           (magkpdat(d.ndx(0,5):d.ndx(1,5)).ta-refsec)/3600.d,magkpvbl,$
           title=ztitle(i),subtitle=stitle(i),$
           xrange=xrange,  xticks=xticks,  xstyle=1,xtitle=xlabl(i),$
           xtickname=xtickn(*,i),xtickv=xtickv,xminor=xminor,$
           yrange=yrange,ystyle=1,ytitle=pm(i).labl,$
           ytickname=pm(i).tickname,ytickv=pm(i).tickv,$
           psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
           position=pos(*,i),xticklen=0.05,$
           charsize=pm(i).charsize,xcharsize=xcharsize,charthick=pm(i).charthick
  endelse 


  if idlsave then begin    ;saving plotted data this panel to save to a file
    idlsav(i).datatype=pm(i).dtp
    idlsav(i).varname=pm(i).labl
    idlsav(i).log=pm(i).plotio
    idlsav(i).psym=pm(i).psym
    idlsav(i).x=(magkpdat(d.ndx(0,5):d.ndx(1,5)).ta-refsec)/3600.d
    idlsav(i).y=magkpvbl
    idlsav(i).yrange=yrange
    idlsav(i).yticks=pm(i).ticks
    idlsav(i).yminor=pm(i).minor 
  endif
endcase   ;end magkp

;============= isee moments plots ==========================================
d.datype(8): begin   
  print,d.datype(0),pnlsel(i),pm(i).dtp,pm(i).labl,pm(i).ev_val

  hrday=(iseemdat(d.ndx(0,8):d.ndx(1,8)).ta-iseerefsec)/3600.d

  ;mvar returns the data (mvbl) correspndng to selctd variable (pm(i).varname) 
    mvbl=iseemvar(pm(i).varname)
  if spikesout eq 1 then mvbl=median(temporary(mvbl),wst.smoothwindow);smoothing
  if timavg then begin    ;time average
    minavg=1.d/60.d       ;do 1min average
    if firstmom then timavg,hrday,mvbl,minavg,xav,yav,first=1 $
    else timavg,hrday,mvbl,minavg,xav,yav
    hrday=xav & mvbl=yav
    if i eq 0 then ztitle(0)=ztitle(0)+' (1min avg)' 
  endif
  firstmom=0

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
           xrange=xrange,  xticks=xticks,  xstyle=1,xtitle=xlabl(i),$
           xtickname=xtickn(*,i),xtickv=xtickv,xminor=xminor,$
           yrange=yrange,yticks=pm(i).ticks,ystyle=1,ytitle=pm(i).labl,$
           ytickname=pm(i).tickname,ytickv=pm(i).tickv,yminor=pm(i).minor,$
           psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
           position=pos(*,i),ynozero=ynozero,xticklen=0.05

         if pm(i).labl eq 'X fshck' then oplot,$
         [iseemdat(d.ndx(0,8)).ta-iseerefsec,$
          iseemdat(d.ndx(1,8)).ta-iseerefsec]/3600.d,[0,0],$
           linestyle=1,color=wst.clr_green
            
  endif else begin   ;yes log plot
         plot_io,$
           hrday,mvbl,$
           title=ztitle(i),subtitle=stitle(i),$
           xrange=xrange,  xticks=xticks,  xstyle=1,xtitle=xlabl(i),$
           xtickname=xtickn(*,i),xtickv=xtickv,xminor=xminor,$
           yrange=yrange,ystyle=1,ytitle=pm(i).labl,$
           ytickname=pm(i).tickname,ytickv=pm(i).tickv,$
           psym=pm(i).psym,symsize=pm(i).symsize,noerase=noerase(i),/normal,$
           position=pos(*,i),xticklen=0.05,$
           charsize=pm(i).charsize,xcharsize=xcharsize,charthick=pm(i).charthick
  endelse 
  
  if keyword_set(oplot_sec) ne 0 then begin
    hrday_oplt=(oplot_sec-iseerefsec)/3600.d
    oplot,[hrday_oplt,hrday_oplt],yrange,color=175
  endif
  
endcase  

else:
endcase

;store 1) axis limits in data coords, 
;window corners in 2) normal and 3) device coords, 
;and 4) log-linear type plot for this panel
  wst.xyrange(*,i) =[!x.crange(0),!y.crange(0),!x.crange(1),!y.crange(1)]
  wst.xywindow(*,i)=[!x.window(0),!y.window(0),!x.window(1),!y.window(1)]
  wst.xysize(*,i)=[!d.x_size*!x.window(0),!d.y_size*!y.window(0),$
               !d.x_size*!x.window(1),!d.y_size*!y.window(1)]
  wst.ylog(i)=pm(i).plotio
  
  wst.xyrange_lzwin(*,i)=wst.xyrange(*,i)
  wst.xywindow_lzwin(*,i)=wst.xywindow(*,i)
  wst.xysize_lzwin(*,i)=wst.xysize(*,i)
  wst.ylog_lzwin(i)=wst.ylog(i)

endfor

;------------------ end plot loop over data selected variables ------------


;save plotted data in each pane
  if idlsave then begin 
    dir=getenv('IDLSAV')  
    save,filename=dir+wst.surveydate+'_idlsave.dat',idlsav
    print,'plots saved in file ',dir+wst.surveydate+'_idlsave.dat'
    help,idlsav & help,idlsav,/str
  endif


if keyword_set(wst.hardcopy) eq 0 then wst.hardcopy=0
;if wst.hardcopy(0) then begin
;  device,/close
;  print,' ' & print,'printing hardcopy file ',pltfil
;  spawn,wst.print_cmd
;  set_plot,'x'
;  clrtbl_indx
;  wst.hardcopy=0 ;& goto,start
;endif

!p.charsize=1.0

;restore saved structure d
  d=dsave

end
     

