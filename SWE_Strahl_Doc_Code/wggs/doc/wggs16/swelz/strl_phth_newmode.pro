; Modified version of strl_phth.pro, for displaying mode7 strahl spectra.
;                                                    Last modified: (09/05/02).

;============================== Img_fp_Strl ===================================
; Display color-scale info., phase-density data, magnetic-field data, contours
;  and other annotation for the ith plot-panel in the current strahl spectrum.
PRO img_fp_strl_newmode,f,u,t,ipl,Zmn=mn,Zmx=mx,Pos=posit,Title=title,$
                        Ytickv=utickv,Ytitle=labl,Ytickn=ytickn,Ymag=ymag,$
                        Xtickv=xtickv,Xtitle=xlabl,Xtickn=xtickn,Xmag=xmag,$
                        Xticks=xticks,Xminor=xminor,Xcharsize=xcharsize,$
                        Charsize=charsize,Charthick=charthick,Style=style,$
                        Ximsize=ximsize,Yimsize=yimsize,N_Colors=n_colors,$
                        fMn=fmn,fMx=fmx,IsNull=isnull,IsFirstEn=isfirsten

;--------------------------- Some preliminaries ------------------------------
isnull = keyword_set(isnull) ;  This flag is set if the curr. spectrum is null.
if (not keyword_set(n_colors)) then n_colors = !D.table_size-1 ;  Default size.

;------------------------ Plot color scale and its axis ----------------------
if ((not isnull) and isfirsten) then begin ; For 1st en. in non-null spectra...
   ;                    Set up color scale (i.e. byte array of pixel values)...
   yc = findgen(n_colors) & xc = findgen(2) & zc = byte((intarr(100)+1)#yc)
   sx = 50 & sy = 400 & z_new = congrid(zc,sx,sy) ; New, resized color scl. im.
   ycrange = [float(fix(mn))-1.0,float(fix(mx))] ;  Data range in posn. coords.

   tv,z_new,850.0,50.0 ;              Display new, resized color scale image...
   plot,xc,ycrange,/NoData,Title='                log!D10!N(f)',$ ; Axis title.
        Position=[780.0,50.0,850.0,450.0],/Device,Ystyle=4,Xstyle=4,$
        /NoErase,Charthick=charthick,Charsize=charsize ;      Set position etc.
   axis,Yaxis=1,Yrange=ycrange,Yticklen=-0.75,Charthick=charthick,$
        Charsize=charsize ;  This is the call which actually displays the axes.
   isfirsten = 0 ;   Will stay false--so that color scale is only plotted once.
endif

;----------------------- Plot strahl phase-density data ----------------------
;    Create new, resized data image, find its size information and display new 
pt = [posit[0],posit[2]]*ximsize & pp = [posit[1],posit[3]]*yimsize ;  image...
st = pt[1]-pt[0]+1 & sp = pp[1]-pp[0]+1 ;  Get desired size of image in pixels.
f_new = congrid(f,st,sp) & corr = [0,1,1,0,0,1,1,0,0,1,1,0,0,0,0] ; Correction.
if (style) then corr = [0,1,0,1,0,1,0,0,1,0,1,0,1,0,0] ; Override, other style.
sf_new = size(f_new) & tv,f_new,pt[0],(pp[0]+corr[ipl-1]),/Order ; Disp. image.
pos_dev_data = [ posit[0]*!D.x_vsize ,pp[0],$ ;  Data axes positions scaled to
                (posit[2]*!D.x_vsize),pp[0]+sf_new[2]-1] ;   size of new image.

;          Define X-axis (phi angles) characteristics for current plot-panel...
;   Note: The following are particle-velocity boundry-angles for each sector...
x = [-11.4,32.9,77.2,121.5,165.8,210.1,254.4,298.7,343.0] & xtickv = round(x)
xtickn = string(xtickv,format='(i3.3)') & xtickn[0] = '343' & xticks = 8
centers = ((x+shift(x,-1))/2.)[0:7] ;     Center-angles of each sector on plot.

;                               Overplot annotated data axes onto data image...
plot,x,u,/NoData,Position=pos_dev_data,/Device,/NoErase,Title=title,$
     Xrange=[-11.4,343.0],Xticks=xticks,Xminor=6,Xtickv=xtickv,$
     Xtitle=xlabl,Xtickname=xtickn,Xticklen=-0.02,Xstyle=1,$
     Yrange=[u[n_elements(u)-1]-4.75,u[0]+4.75],Yticks=2,Yminor=1,$
     Ytickv=utickv,Ytitle=labl,Ytickname=ytickn,Yticklen=-0.02,Ystyle=1,$
     Charthick=charthick,Charsize=charsize,Xcharsize=xcharsize

if (not isnull) then begin ;             If the current spectrum is non-null...
   ;          Overplot vertical lines at the solar and anti-solar directions...
   plots,[0.0,0.0],[u[0]+4.75,u[n_elements(u)-1]-4.75],/Data,Thick=0.5
   plots,[180.0,180.0],[u[0]+4.75,u[n_elements(u)-1]-4.75],/Data,Thick=0.5

   ; Calculate and display the byte/intensity levels for 1/4 and 1/2 of max(f).
   levels = bytscl([(fmx-0.6),(fmx-0.3)],Min=mn,Max=mx,Top=n_colors-1)
   if (levels[1] eq levels[0]) then levels[1] = levels[1]+1 ;  Must be diffrnt.
   ; Overplot onto the current plot-panel contours at the levels where f is at
   contour,f,centers,u,/Overplot,$ ;  one-quarter and one-half of its max. val.
           Levels=levels,C_Annotation=['0.25','0.50'],C_Charsize=1.0
   ;  Note that the base-10 log of f drops by ~0.6 and ~0.3 (from max.) when f
   ;    takes on the 1/4*max(f) and 1/2*max(f) (resp.) values.  These reduced
   ;    log. values must then be mapped onto a byte-scale to be related to the
   ;    rest of the log(f) values, already mapped onto the same byte-scale.

   if (ipl le 5) then begin ;         For the first five plot-panels display...
      print,'' & print,'For energy-steps ',ipl+(indgen(3)*5)-1,'of 14',$
                       Format='(A17,2(I2.2,", "),"and ",I2.2," ",A5)'
      print,'Particle-velocity azimuth angles (GSE) (ascending):' & print,t
   endif

   for isect=0,7 do begin ;        For each sector in the current plot-panel...
      plots,[t[isect],t[isect]],$ ; Mark the angle where counts were collected.
            [u[0],u[n_elements(u)-1]],/Data,Thick=0.25,Color=0,Linestyle=1
      ;   Note: the angle marked is the angle where the count-collection BEGAN.
   endfor

   usersym,[-4,+4],[0,0],Thick=3 ;        Overplot '-' symbol in the direction 
   oplot,[xmag[0],xmag[0]],[ymag[0],ymag[0]],Psym=8 ;   opposite of mag. field.

   ;                        Overplot '+' symbol in direction of magnetic field.
   oplot,[xmag[1],xmag[1]],[ymag[1],ymag[1]],Psym=1,Symsize=3,Thick=3
endif

end


;============================= Strl_PhTh_Newmode =============================
; This routine generates a full-spectrum-display of mode7 strahl data, given
;  the dimensions of the display area ('x_im_sz' by 'y_im_sz', in pixels) and
;  (cmmn-blk) access to the level-zero data read from the current major-frame.
PRO strl_phth_newmode,X_Im_Sz=x_im_sz,Y_Im_Sz=y_im_sz

common swestuff,swest ;  Common-blocks for data sharing among reading/plotting
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl ;     routines.

; ---------------------------- Initialization --------------------------------
print,'' & print,'-------Generating (mode7) strahl spectrum: ',swest.ispinbl
ispct = swest.ispinbl & timpb5 = vsmjf.pb5tim_vsbl[*,ispct] ;  Get time info...
sec = double(timpb5[2]/1000) & hour_hms,(sec/3600.d),hms
subtitle = yrmoda(timpb5)+' '+string(timpb5[1],Format='(I3)')+' '+hms

; --------------------- Gathering Magnetic-Field Data ------------------------
;             Get MFI magnetic field data (source depends upon availability)...
if (swest.mag_3s_kp eq 1) then begin             ;  Use MFI 3s mag. field data.
   get_3smag,timpb5,magtpb5,b,phi_bpos,the_bpos
endif else if (swest.mag_3s_kp eq 2) then begin  ;  Use MFI KP mag. field data.
   get_kpmag,timpb5,b,phi_bpos,the_bpos 
endif else begin                                 ;    No mag. field data avail.
   phi_bpos = 0 & the_bpos = 0 & b = [-1.0,0.0,0.0] ;              Use default.
endelse

;  If magnetic field data STILL not available and this is "ok" with the user...
if ((not keyword_set(phi_bpos)) and (getenv('LZNOMAG') eq 'ok')) then begin
   b = [-1.0,0.0,0.0] ;               Set magfield to default anti-solar value.
   phi_bpos = 0 & the_bpos = 0 ;          Set corresponding orientation angles.
   print,'' & print,'NO B-FIELD: Using default anti-solar direction.'
endif

; Calculate the orientation of the "anti-B-field" from the B-field direction...
phi_bneg = (phi_bpos+180.0) mod 360.0 & the_bneg = -the_bpos ;  "Neg." B-field.
print,'mag fld phi,theta (gse): ',phi_bpos,the_bpos ; "Pos." B-fld. directions.

; ------------------- Determining Strahl-Instrument Angles -------------------
; Azimuth look-angles of strahl detector at each energy of each sector in one
;   full spin--given in s/c (payload spin angle) coords.--for current spectrum.
phistrl_onespin = reform(vsmjf.phistrl[0,*,*,ispct]) mod 360.0 ;       8-by-15.
thestrl_lk = 90.0-vsmjf.thestrl ; Calc. elev. ang. using angles from spin-axis.

; NOTE: Converting from s/c (payload spin angle) coords. to GSE is done here
;        by the approx. transformation of a 180 deg. rotation about the x-axis.
;  Azimuth look-angles of strahl detector in one spin in GSE spin angle coords.
phistrl_onespin_gse = 360.0-phistrl_onespin ;    S/c spin-axis in -Z (GSE) dir.
thestrl_lk_gse = -thestrl_lk ; Elev. ang. conv. to GSE using approx. transform.

thestrl = -thestrl_lk_gse ;   Finally, convert to dir. along vel. of particles.
phistrl_onespin_gse_vel = (phistrl_onespin_gse+180.0) mod 360.0

; --------------------- Unpacking Phase-Density (f) Data ---------------------
n_strsamps = vsmjf.n_strsects*vsmjf.n_strensteps ;    Determine # of samps/det.
fstrl = transpose(reform(vsmjf.fstrl[*,*,*,ispct],$ ;   The f data in one spin.
                         n_elements(thestrl),n_strsamps))
w = where(fstrl ne 0) & if (w[0] eq -1) then return ;    Avoid taking log of 0.
mn = min(alog10(fstrl[w]),Max=mx) ;           Get min and max of log_10(fstrl).
print,'' & print,'Minimum and maximum values of log_10(fstrl): ',mn,mx

; --------------------- Begin Spectrum-Display Setup -------------------------
;               Initialize basic plot characteristics (positions, flags, etc.).
npnl = 16 & pos,npnl,posn,Xoff=0.10,Xsep=0.15,Yoff=0.10,Xtop=1.0,Col=1
erase & charsize = 1.25 & charthick = 1.0 & isnull = 0 & isfirsten = 1
if (swest.pltype_index eq 1) then $ ;     Other style of plot was selected, so
   posn = posn[*,[1,0,2,4,6,8,10,12,14,3,5,7,9,11,13,15]] ;     override posns.

if vsmjf.is_null[ispct] then begin ; Set flags & display labels for null spect.
   fstrl[*,*] = min(fstrl) & isnull = 1 ;         Set flag to supress plotting.
   xyouts,(0.12+(swest.pltype_index*0.45)),0.91,'**Null Spectrum**',/Normal,$
          Charsize=2.0*charsize ;            Label position is style-dependent.
   subtitle = yrmoda(timpb5)+' '+string(timpb5[1],Format='(I3)')+' '+'**:**:**'
endif

y = thestrl ;  Define Y-axis (elevation angles) characteristics for each panel.
ymn = min(y,Max=ymx) & ymx = max([abs(ymx),abs(ymn)]) & ymn = -ymx
yticks = 2 & ytickv = ymn+(indgen(yticks+1)*((ymx-ymn)/yticks))
ytickn = string(ytickv,Format='(f5.1)') ;  Center-angles of combined detectors.
print,'' & print,'Particle-velocity elevation angles (GSE) (ascending):'
print,reverse(thestrl) ;                   Angles initially in decending order.

; ---------------------- Generate Display At Each Energy ---------------------
for ipl=1,15 do begin ;           For each of the plot-panels (energy-steps)...
   ;  Sort both the phi list and the corresponding f vals. by increasing phi...
   x = reform(phistrl_onespin_gse_vel[*,(ipl-1)]) & sort_phi = sort(x)
   if ((ipl mod 5) eq 0) then sort_phi = shift(sort_phi,1) ; Rollover crrction.
   x = x[sort_phi] & g = (fstrl[(indgen(8)+((ipl-1)*8)),*])[sort_phi,*]

   if (ipl le 2) then unit = ' eV' else unit = '' ;     Disp. unit only on top.
   if (swest.pltype_index eq 1) then begin ; Other style of plot was selected,
      if ((ipl mod 8) eq 1) then unit = ' eV' else unit = '' ;  override style.
   endif

   xtitle = 'phi (B-field/particle-velocity direction) GSE' ;      Axis titles.
   ytitle = string(round(volt_en_strl(vsmjf.strlstep[ipl-1],/En)),$
                   Format='(i4.2)')+unit

   ;     Set X-axis charsize and main title according to current plot number...
   if (ipl lt 14) then xcharsize=0.001 else xcharsize=1.0 ;   Bottom two plots.
   if (ipl eq 2) then title='STRAHL electrons '+subtitle(0) else title='' ; Top

   if (swest.pltype_index eq 1) then begin ; Other style of plot was selected,
      if ((ipl mod 7) eq 1) then xcharsize=1.0 else xcharsize=0.001 ; Bttm two.
      if (ipl eq 1) then xcharsize=0.001 ;    Special case of above (override).
      if (ipl eq 9) then title='STRAHL electrons '+subtitle(0)+'    ' $
                    else title='' ;               Modified title for top panel.
   endif

   xmag = [phi_bneg,phi_bpos] & ymag = [the_bneg,the_bpos] ; Mag. field data...

   ;              Scale non-zero elements of (log) image array to byte scale...
   w = where(g ne 0) & if (w[0] ne -1) then begin ;     Avoid finding log of 0.
      g[w] = alog10(g[w]) & fmx = max(g[w],Min=fmn) ; Max/min @ CURRENT ENERGY.
      n_colors = !d.table_size-1 ;  Find max{log(f)}, and scale to # of avail.
      g[w] = bytscl(temporary(g[w]),Min=mn,Max=mx,Top=n_colors-1) ;     colors.
   endif

   ; **************************************************Generate Strahl image...
   img_fp_strl_newmode,g,y,x,ipl,Zmn=mn,Zmx=mx,Pos=posn[*,ipl],Title=title,$
                       Ytickv=ytickv,Ytitle=ytitle,Ytickn=ytickn,Ymag=ymag,$
                       Xtickv=xtickv,Xtitle=xtitle,Xtickn=xtickn,Xmag=xmag,$
                       Xticks=xticks,Xminor=1,Xcharsize=xcharsize,fMx=fmx,$
                       Charsize=charsize,Charthick=charthick,fMn=fmn,$
                       Ximsize=x_im_sz,Yimsize=y_im_sz,N_Colors=n_colors,$
                       IsNull=isnull,IsFirstEn=isfirsten,$
                       Style=swest.pltype_index
endfor

print,'' ;                        Skip line and generate final screen output...
print,'--------Completed (mode7) strahl spectrum: ',swest.ispinbl & print,''

end
