pro swe_fpitch_plt,indx,npl

common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common shareplt,pos,pm,idlsav,ztitle,stitle,xlabl,xtickn,xcharsize,noerase,$
  idlsave,lzwin,elaps_spns
common log_delog,comp_tbl,dcomp_tbl

idatype=where(d.datype eq 'swe_fpitch')

if d.ndx(1,idatype) -  d.ndx(0,idatype) lt 2 then begin
  print,'array size must be ge 2' & return
endif

numen=n_elements(d.swe_pdat(d.ndx(0,idatype)).vsteps)

; MPH (10/10/2003)--This avoids code which is not appropriate for the new mode.
if ((ymd_pb5(long(wst.indate)))[0] lt 2002l) then begin ;    2001-and-before...
   ; Note: Due to VEIS instrument failure in mid-2001, any electron pitch-
   ;                          angle data from 2001-and-before must be old-mode.
   print,"Using OLD MODE (VEIS) panel list." ;                      Diagnostic.
   d.pnlist.list(d.pnlist.offs(idatype)+1:$ ;   Do this ONLY for old-mode data.
     d.pnlist.offs(idatype)+1+d.pnlist.len(idatype)-1)=''
   d.pnlist.list(d.pnlist.offs(idatype)+1:d.pnlist.offs(idatype)+1+numen-1)=$
     string(volt_en(d.swe_pdat(d.ndx(0,idatype)).vsteps,/en),format='(f8.1)')
endif
           
i=indx 

if pm(i).varname eq 'Spin avg f(en,t)' then begin
  print,'swe_fpitch_plot: plotting ',pm(i).varname
  swe_fspnavg_plt,indx,npl
  return
endif else enstep=pm(i).ev_val-1
      
;d.swe_pdat.f=pitch angle array, dim(pitch angle indices,en steps,time indices)
   sz=size(d.swe_pdat.f)

;get array of pitch angle bins 
   dp=(180./sz(1))
   pabin=byte(fix(float(dp)/2)+indgen(sz(1))*dp)

print,"plotting (zero-based) swe_fpitch energy-channel: ",enstep ; Diagnostic.
;g=pitch angle plot array for selected en step=pm(i).ev_val  
   g=transpose(d.swe_pdat(d.ndx(0,idatype):d.ndx(1,idatype)).f(*,enstep))
   

                   
;if wst.spikesout eq 1 then g=median(temporary(g),wst.smoothwindow)  ;smoothing

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


;if converting compressed counts to phase density
;  fix this later!  if wst.cf eq 1 then g=cts_fpitch(temporary(g),enstep)

;determine scale to be used   
   case wst.ptch_img_opt of
        0: begin                    ;log counts scale
             g=float(dcomp_tbl(g))    ;decompress
             g(where(g ne 0))=alog10(g(where(g ne 0)))
             logimg=1
             rlbl='cts' 
           endcase 
        else:     
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
 
   
;image plot procedure
   swe_fpitch_img, g,pabin,$
     (d.swe_pdat(d.ndx(0,idatype):d.ndx(1,idatype)).ta-d.refsec)/3600.d, $
      pos=pos(*,i),   $
      ytitle=pm(i).labl, rlbl=rlbl,ytickv=[0,90,180],$
      ytickn=string([0,90,180],format='(i3)'),$
      xrange=pm(i).tmrange, xtickv=pm(i).tmtickv(0:pm(0).tmticks),$
      xtickn=xtickn(*,i),zmn=mn, zmx=mx,$
      xticks=pm(i).tmticks, xtitle=xlabl(i) ,xminor=pm(i).tminor, $
      title=ztitle(i),$
      subtitle=stitle(i),$
      charsize=pm(i).charsize,charthick=pm(i).charthick,xcharsize=xcharsize(i),$
      logimg=logimg
      
end   