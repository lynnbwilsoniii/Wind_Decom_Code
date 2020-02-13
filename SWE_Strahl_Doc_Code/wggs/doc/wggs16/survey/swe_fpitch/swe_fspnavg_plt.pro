pro swe_fspnavg_plt,indx,npl

common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
common sharewidg,WDGT
common log_delog,comp_tbl,dcomp_tbl

idatype=where(d.datype eq 'swe_fpitch')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

  
i=indx 

;cf=0 if images in compressed (12 to 8 bit) counts
;cf=1 if images to be converted to phase density


;print,'d.datype(1),pm(i).dtp,pm(i).labl',d.datype(1),pm(i).dtp,pm(i).labl

;d.swe_pdat.fspa=input spectrum array, dim(en steps,time indices)
;g=plotted image array (compressed counts)
   nvmin_spnavg=pm(i).range(0) & nvmax_spnavg=pm(i).range(1)

; MPH (10/02/2003)-----------This modifies the selected range for the NEW mode.
if ((ymd_pb5(long(wst.indate)))[0] ge 2002l) then begin ;     2002-and-after...
   ; Note: Due to VEIS instrument failure in mid-2001, only NEW-mode (mode7)
   ;        electron pitch-angle data exists for 2002-and-after.
   nvmin_spnavg = nvmin_spnavg-2 & nvmax_spnavg = nvmax_spnavg-2 ;  Drop range.
endif

   g=transpose($
   d.swe_pdat(d.ndx(0,idatype):d.ndx(1,idatype)).fspa(nvmin_spnavg:nvmax_spnavg))

;if wst.spikesout eq 1 then g=median(temporary(g),wst.smoothwindow) ;smoothing

szgin=size(g)

;the set of indices, elaps_spns, corresponds to elapsed time, therefore
;the array, fimg, contains the actual data and null elements in time gaps
    elaps_spns=d.swe_pdat($
    d.ndx(0,idatype):d.ndx(1,idatype)).elapspn 
    gtmp=bytarr(max(elaps_spns-elaps_spns(0))+1,szgin(2))
    gtmp(elaps_spns-elaps_spns(0),*)=g
    help,g,gtmp
    g=gtmp
    gtmp=0
    
;determine scale to be used
  case wst.ptch_img_opt of
        0: begin
             g=float(dcomp_tbl(g))    ;decompress
             g(where(g ne 0))=alog10(g(where(g ne 0)))
             logimg=1
             rlbl='cts' 
           endcase   
    endcase

;if necessary, rebin time dimension to the pixel resolution
   ;if wst.hardcopy then ntlim=200l else ntlim=650l
   ntlim=wst.rebin_size_img
   if szgin(1) gt ntlim then begin  
     sclfctr=fix(szgin(1)/ntlim)+1
     ntmx=sclfctr*(szgin(1)/sclfctr)
     case wst.rebin of
     0: g=congrid(g,ntmx/sclfctr,szgin(2))
     ;0: g=rebin(g(0:ntmx-1,*),ntmx/sclfctr,szgin(2))
     else:
     endcase
   endif
   help,g
       
;find min-max    
mn=min(g(where(g ne 0)),max=mx)
print,'plt: mn,mx ',mn,mx    

;scale non-zero elements of image array to byte scale
   w0=where(g eq 0)
   g=bytscl(temporary(g),min=mn,max=mx,top=wst.ncolors-1);scale to  colors
   if w0(0) ne -1 then g(w0)=0

;get y (energy) scale
   y=alog10(volt_en($
     d.swe_pdat(d.ndx(0,idatype)).vsteps(nvmin_spnavg:nvmax_spnavg),/en))

; MPH (10/02/2003)-----------This corrects the range labeling for the NEW mode.
if ((ymd_pb5(long(wst.indate)))[0] ge 2002l) then begin ;     2002-and-after...
   ; Note: Due to VEIS instrument failure in mid-2001, only NEW-mode (mode7)
   ;        electron pitch-angle data exists for 2002-and-after.
   y=alog10(volt_en_strl($
     d.swe_pdat(d.ndx(0,idatype)).vsteps((nvmin_spnavg+1):$
                                         (nvmax_spnavg+1)),/En))
endif

   ymn=min(y,max=ymx) & ytck=4

;image plot procedure
   swe_fspnavg_img, g, y, $
      (d.swe_pdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d, $
      pos=pos(*,i),zmn=mn, zmx=mx, rlbl=rlbl,$
      ytickn=string(10.^(ymn+indgen(ytck+1)*(ymx-ymn)/ytck),format='(i4)'),$
      ytitle=pm(i).labl, ytickv=ymn+indgen(ytck+1)*(ymx-ymn)/ytck,$
      xrange=pm(i).tmrange, xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
      xtickn=xtickn(*,i),subtitle=stitle(i),$
      xticks=pm(i).tmticks, xtitle=xlabl(i) ,xminor=pm(i).tminor, $
      title=ztitle(i),$
      charsize=pm(i).charsize,charthick=pm(i).charthick,$
      xcharsize=xcharsize(i),logimg=logimg

end