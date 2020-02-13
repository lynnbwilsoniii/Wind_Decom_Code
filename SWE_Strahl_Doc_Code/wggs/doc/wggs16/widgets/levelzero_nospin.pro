

;====================== get_recnspin ====================================
 
pro get_recnspin

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common shared,d
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec

;-------------------------------------------------------------
print,'get_recnspin: wst.lzdate ',wst.lzdate,' wst.timsel ',wst.timsel
case wst.timsel of
 'survey' : begin
          
       tpoint1=refsec+wst.xydata(0)*3600.d ;secs of selctd point from tjd epoch 
       print,' ' & print,'selected point in time ',$
         wst.xydata(0),'  ',wst.hms,'  ',tpoint1,'  ',wst.pb5
       ;spn is the index of mdat.ta or pdat.ta corresponding to tpoint

       case 1 of

       d.wdatype(0,0) ne -1 : begin                 ;moments yes
         swest.spn=long(indx_begin(mdat.ta,tpoint1 ))
         hour_hms,(mdat(swest.spn).ta - refsec)/3600.d,hmsspn
         recn=mdat(swest.spn).mfrec     ;+1
         swest.ispinbl=mdat(swest.spn).mfspinbl
         print,'spn,mdat(spn).ta,hmsspn,recn,swest.ispinbl',$
           swest.spn,mdat(swest.spn).ta,hmsspn,recn,swest.ispinbl

                              endcase

       d.wdatype(0,0) eq -1 and $
       (d.wdatype(0,1) ne -1 or d.wdatype(0,2) ne -1): begin  ;mom no, pitch yes
          swest.spn=long(indx_begin(pdat.ta,tpoint1 ))
          hour_hms,(pdat(swest.spn).ta - refsec)/3600.d,hmsspn
          recn=pdat(swest.spn).mfrec    ;+1
          swest.ispinbl=pdat(swest.spn).mfspinbl
          print,'spn,pdat(spn).ta,hmsspn,recn,swest.ispinbl',$
          swest.spn,pdat(swest.spn).ta,hmsspn,recn,swest.ispinbl
                                                        endcase
       else : begin    ;moments and pitch no
          recn=lztimrec(sec_pb5(tpoint1))
          swest.ispinbl=0
          swest.isector=0
          ;swest.ivstep=0
              endcase
       endcase
             endcase

 'lz' : print,'rec, swest.ispinbl ',recn,swest.ispinbl, 'selected'

 'lztm' : begin
            case pltwin_frmt of
            0: begin
                 pb5=ymd_pb5(long(wst.lzdate))
                 pb5(2)=wst.xydata(0)*3600000
                 recn=lztimrec(pb5)
                 swest.ispinbl=0
                 print,'selected point in time ',$
                 wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
               endcase

            1: begin
                recn=lztimrec(wst.pb5)
                swest.ispinbl=0
                print,'selected point in time ',wst.pb5
               endcase
            endcase
          endcase

 else : begin
   recn=1 & swest.ispinbl=0
        endcase

endcase

wst.timsel='lz'    ;reset

end   



;=========================== proc_fw ==========================================

pro proc_fw,wid=wid,pltype=pltype,nopltf=nopltf,err=err

common sharewidglz,wlz
common sharewidg,wa
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common magstuff,magfile,tpb5,bgse
common log_delog,comp_tbl,dcomp_tbl
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common ftemp,fblok,vel
common shared,d
common wstuff,wst
common swestuff,swest
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common sharelevelzero,pltwin_frmt,oplot_sec

err=0

if keyword_set(infile) eq 0 then return
if infile eq '' then return
if keyword_set(wid) eq 0 then wid=1
if keyword_set(swest.subtrbkg) eq 0 then swest.subtrbkg='Yes'
if keyword_set(idlsavf) eq 0 then idlsavf=0


md=strarr(257)
md(1)='sci92' & md(3)='man92' & md(4)='con92' & md(5)='sci46' & md(7)='man46' 
md(8)='con46' & md(128)='trans' & md(256)='u'

ntest2=0

if wst.lz_is_read then goto,lzisread

flag1=0
flag2=0

get_recnspin            ;get recn and spin number
                 
if keyword_set(recn) eq 0 then begin
     recn=1 & swest.ispinbl=0
endif

;ntest2=0
point1:
 
;process selected lz record
   print,'processinf recn ',recn
   proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,elec_ion_m1=elec_ion_m1,$
    err=err
   print,'err: ',err
   ;widget_control, wlz.field(14), set_value = swest.subtrbkg

;test tm mode
   if keyword_set(tmmode_ihk) eq 0 then return
   if tmmode_ihk ne 2 then begin
     print,' ' & print,'not in tm science mode'
     return
   endif

;test whether in background test mode
  if vsmjf.background_test then begin
    print,' ' & print, 'BACKGROUND TEST mode'
    return
  endif

wst.lz_is_read=1

lzisread:    ;an lz record has been read

;test current spinbl number against max number spins in current mjf 
   if swest.ispinbl gt vsmjf.n_spins-1 then begin
     swest.ispinbl=0   
     recn=recn+1
     goto,point1     ;read the next record
   endif

;test whether current spinbl contains selected specie
;test first step in each sector of spin
   if ntest2 lt 3 then begin
      ntest2=ntest2+1
      if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then $
        wspecie=where(vsmjf.eleion(*,swest.ispinbl) eq swest.specie_selct)      
      if vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4 then $
        wspecie=where(vsmjf.eleion(0) eq swest.specie_selct)
      if wspecie(0) eq -1 then begin
        swest.ispinbl=swest.ispinbl+1
        if swest.ispinbl gt vsmjf.n_spins-1 then begin
          swest.ispinbl=0 & if recn lt fh.nmf-1 then recn=recn+1 $
          else stop,'end of file'
        endif 
        goto,point1     ;read the next record
      endif
   endif else begin
      print,$
      'selected species not available on 3 successive spinwst....increment recn'
      return
   endelse


widget_control,wlz.field(10),set_value=recn
spinbl=swest.ispinbl & widget_control,wlz.field(11),set_value=spinbl
if keyword_set(swest.nvmax) eq 0 then swest.nvmax=vsmjf.n_vesteps
swest.nvmax = swest.nvmax < vsmjf.n_vesteps
nvmax=swest.nvmax ;& widget_control,wlz.field(12),set_value=swest.nvmax
if (vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4) then begin
   if vsmjf.eleion eq 1 then print,'MODE 1 ION MODE!!'
endif

;get tjd and pb5 times
   tjd=long(fix(vsmjf.suntim_vsbl(swest.ispinbl)/86400.d))
   sec=vsmjf.suntim_vsbl(swest.ispinbl) - tjd*86400.d
   ;timpb5=tjd_pb5(long(tjd),long(1000*sec))  ;convert tjd,sec to pb5 time
   timpb5=vsmjf.pb5tim_vsbl(*,swest.ispinbl)
   wst.xydata(0)=sec/3600.d
   hour_hms,wst.xydata(0),hms
   wst.hms=hms(0)
   wst.pb5=timpb5
  
  print,' ' & print,'proc_fw: timsel',wst.timsel
  print,'time selected from lz file: '
  print,'lz.recn, swest.ispinbl, vsmjf.suntim_vsbl(swest.ispinbl) (seconds) ',$
      lz.recn,swest.ispinbl,vsmjf.suntim_vsbl(swest.ispinbl)
  print,'tjd, sec of day ',tjd,sec
  print,'pb5 time ',timpb5
  print,ymd(timpb5),'  ',hms

  ;test for data quality
  if vsmjf.vqlty(swest.ispinbl) ne 0 then begin
     print,' '
     print,'quality flag ne 0 for this spinblock',swest.ispinbl,$
        '; data set to 0'
     return
  endif

;get mfi 3sec mag field
    ; timpb5=given pb5 time (year, day of year, millisec of day)  lonarr(3)
    ; tpb5 = pb5 time at center of minute for given record recn   lonarr(1440,3)
    ; bgse = mag vector at 3 sec intervals in minute tpb5     fltarr(1440,3,20)

    ;test whether day of the given time is the same as "lzdate" 
    ;or is just before midnight on previous day
    ;if given time is the same as lzdate, then the correct magfile has already
    ;been read;  
    ;if given time is NOT the same as lzdate, then the appropriate
    ;magfile should be read to get the magnetic vector and then read again the
    ;magfile that agrees with lzdate..
    ;TEMPORARILY, we will not read the appropriate (previous day's) magfile
    ;but will use the first mag vector on the day corresponding to lzdate..
    ;the probable error will be to use mag data corresponding in time to 
    ;one or two spins later than the given time.

    if ymd(timpb5) eq wst.lzdate then begin 
      mindx=fix((timpb5(2))/60000)  ;magfile record number from minute index
      sindx=fix((timpb5(2)-mindx*60000 )/3000) ;3sec index in minute interval
    endif else begin
      wmag=where(wst.magdate eq wst.lzdate)
      if wmag(0) ne -1 then begin
        mindx=0 & sindx=0
        print,'given time and magdate do not agree'
        print,'ymd(given time), lzdate, magdate ',$
          ymd(timpb5), wst.lzdate, wst.magdate(wmag)
        print,$
         'first 3sec interval on lzdate will be used: mindx,sindx',mindx,sindx
      endif
    endelse

    if magfile ne '' then begin      
      magfld=bgse(mindx,0:2,sindx)
      magtpb5=[tpb5(mindx,0),tpb5(mindx,1),$
        tpb5(mindx,2) - (30000l - 1500l - long(sindx)*3000)]
      print,' '
      print,'given time ',timpb5
      print,'mindx, sindx ',mindx, sindx
      print,'  mag time ',magtpb5
      print,'mag fld (gse) ',magfld
      if magfld(0) eq -1.e31 then begin
        magfld=[-1.,0.,0.]
        print,'fill mag data, substitute ',magfld
      endif
    endif else begin
      magfld=[-1.,0.,0.]
      print,'No mag data; using direction of Sun instead'
      print,'Hit return if you want to continue?  y/n'
      answ='' & read,answ & if answ ne 'y' then stop
    endelse
   
;get time labels
    mjfdt='mjf '+$
       string(lz.yr,format='(i4)')+'_'+string(lz.dy,format='(i3)')+'_'+$
       string(lz.ms/1000,format='(f9.3)')
    hour_hms,sec/3600.d,hms,lhms=lhms
    spndt= yrmoda(timpb5) + ' ' +string(timpb5(1),format='(i3)') + ' ' + hms
    swest.spndt=spndt(0)
    title='SWE ele  '
    widget_control,wlz.field(13),set_value=lhms
   
;get patch fit coeff's if there is a moments file
   npatch=0 &  bnfit=0 & vpot=0. & spcpot=0.
   u=fltarr(3)  ;electron moment bulk flow

   if keyword_set(d) ne 0 then begin
   if d.wdatype(0,0) ne -1 then begin    ;moments data has also been selected   
     if wst.timsel eq 'survey' then mspn=swest.spn
     if wst.timsel eq 'lz' or wst.timsel eq 'lztm' then  $
       mspn=long(indx_begin(mdat.ta,vsmjf.suntim_vsbl(swest.ispinbl) ))
     u=mdat(mspn).uout*1e5
     print,' '
     print,'lchan, n_vesteps_trunc, truncstep, skip_step0, skip_step1, nglint :'
     print,mdat(mspn).iflgs(7),mdat(mspn).misc(0),mdat(mspn).misc(5),$
           mdat(mspn).misc(1),mdat(mspn).misc(2),mdat(mspn).misc(6)
     print,' '
     print,'v : ',mdat(mspn).v
     print,' '
     if total(mdat(mspn).bnfit) ne 0 then begin
       bnfit=mdat(mspn).bnfit
       npatch=4
     endif
     ;w=$
     ;where(volt_en(vsmjf.veistep(0:vsmjf.n_vesteps-1),/vel) gt mdat(mspn).vpot)
     vpot=mdat(mspn).vpot
     ;if vpot ne 0. then nvmin=w(0)


 
    
       if mdat(mspn).misc(1) eq 1 and mdat(mspn).misc(2) eq 1 then nvmin=2
       if mdat(mspn).misc(1) eq 1 and mdat(mspn).misc(2) eq 0 then nvmin=1
       if mdat(mspn).misc(1) eq 0 and mdat(mspn).misc(2) eq 0 then nvmin=0
       if mdat(mspn).misc(1) eq 0 and mdat(mspn).misc(2) eq 1 then $
         stop,'not allowed to skip second and not first vel step'
     

     print,'proc_fw: npatch, nvmin, nvmax ',npatch, nvmin, swest.nvmax
     print,'proc_fw: vpot, spcpot ',mdat(mspn).vpot,mdat(mspn).spcpot
   endif
   endif 


if swest.subtrbkg eq 'Yes' then fblok=vsmjf.fveis_b(*,*,*,swest.ispinbl)  $
else fblok=vsmjf.fveis(*,*,*,swest.ispinbl)

;-------  treatment of glint points --------------------------
  if swest.delete eq 0 then fblok=abs(fblok)
  ;widget_control, wlz.field(15), set_value = swest.delete * swest.ndel


;form plotting arrays
;  (for now, we assume that all sectors of a given spin are the same specie
;   and have the same steps......see comment in mode2.pro  )
   if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then $
     wsteps=vsmjf.veistep(*,0,swest.ispinbl)
   if vsmjf.scimode eq 0 or vsmjf.scimode eq 1  or vsmjf.scimode eq 4 $
     then wsteps=vsmjf.veistep
   vel=volt_en(wsteps,/vel,ion=swest.specie_selct)
   fparr_patch,fblok,vel,wsteps,vpot,spcpot,magfld,u,timpb5,$
      swest.specie_selct,wid=wid,pltype=pltype,$
      npatch=npatch,bnfit=bnfit,nvmin=nvmin,nvmax=swest.nvmax,err=err

 if err then return

;stop
;plot data

if keyword_set(nopltf) eq 0 then nopltf=0 else nopltf=1
if nopltf eq 0 then begin

if swest.delete eq 0 then $
     labl1='SWE '+swest.specie(swest.specie_selct)+' (Mask Off)' else $
     labl1='SWE '+swest.specie(swest.specie_selct)
 
if max(pltype) lt 10 then begin;   display in vpara, vperp coords

   if pltwin_frmt eq 1 and wst.hardcopy ne 1 then begin
     oplot_sec=pb5_sec(timpb5)
     wset,wlz.win(0) & erase
     plt,/lzwin
     ;plt_lzwindow
   endif

   if keyword_set(pltype) eq 0 then pltype=[0,2]
   
   if pltype(0) eq -1 then redf_integ,xsize=wlz.win_xsize(wid),$
       ysize=wlz.win_ysize(wid),ion=swest.specie_selct,$
       labl1=labl1,labl2=spndt  $
      
   else begin   
     
     wset,wlz.win(wid) 
     fplot,wlz.win_xsize(wid),wlz.win_ysize(wid),pltype=pltype,$
       ion=swest.specie_selct,$
       labl1=labl1,$
       labl2=spndt
       
   endelse  

endif else if max(pltype) eq 10 then begin
     y=reform(em(*,0))
     ymin=1.
     wle=where(y le ymin)
     if wle(0) ne -1 then y(wle(n_elements(wle)-1))=ymin
     wge=where(y ge ymin)
     y(wge)=alog10(y(wge))
     x=reverse(reform(pm(0,*)))
     g=fltarr(n_elements(x),n_elements(y(wge))) 
     for i=0,n_elements(x)-1 do g(i,*)=fm(wge,n_elements(x)-1-i) 
   
  ;scale non-zero elements of (log) image array to byte scale
     w=where(g ne 0)
     g(w)=alog10(g(w))
     mn=min(g(w),max=mx)
     w0=where(g eq 0)
     n_colors=!d.table_size-1
     g(w)=bytscl(temporary(g(w)),min=mn,max=mx,top=n_colors-1);scale to clrs
     if w0(0) ne -1 then g(w0)=0
    
  ;set plot parameters  
     ymn=min(y,max=ymx) & ytck=5 
     ytickv=ymn+indgen(ytck+1)*(ymx-ymn)/ytck
     ytickn=string(10.^ytickv,format='(i5)')
     xmn=180. & xmx=0. & xrange=[xmn,xmx] & xtck=6  
     
     wset,wlz.win(wid)
     x_im_sz=wlz.win_xsize(wid)
     y_im_sz=wlz.win_ysize(wid)
     pos,1,posn,xoff=0.21
     img_ep, g, y, x, zmn=mn, zmx=mx,  pos=posn,  $
       ytickv=ytickv,ytitle='log eV', $
       ytickn=ytickn,$
       xrange=xrange, $
       xtickv=xmn+indgen(xtck+1)*(xmx-xmn)/xtck, $
       xtickn=string(xmn+indgen(xtck+1)*(xmx-xmn)/xtck,format='(i5)'),$
       subtitle=spndt(0),$
       xticks=xtck, xtitle='Pitch Angle (deg)' ,xminor=1, $
       title=labl1,$
       charsize=1.25,charthick=1.0,xcharsize=1.0,$
       ximsize=x_im_sz,yimsize=y_im_sz,n_colors=n_colors 

endif 
endif
print,'wst.timsel ',wst.timsel  
  
end


;------------------- end  proc_fw --------------------------------------------

;================================ dspl_opt_event ==============================

pro dspl_opt_event,event
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common sharewidglz,wlz
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

CASE event.id OF

swest.wdsp.button(0) : begin

  case event.value of
    'Strlf+veisf' : if swest.strlf_veisf eq 0 then swest.strlf_veisf=1 $
                                                else swest.strlf_veisf=0
                                                
    'Hide points' : if swest.hidepoints eq 1 then swest.hidepoints=0 else $
          swest.hidepoints=1
    
    'Contour labels'  : if swest.c_labels eq 1 then swest.c_labels=0 else $
          swest.c_labels=1
    
    'Interp 0, 180' :  if swest.gap_interpol eq 1 then $
          swest.gap_interpol=0 else swest.gap_interpol=1
    
        
    'Quit' : widget_control,event.top,/destroy
  endcase
endcase

  
swest.wdsp.button(1): begin
  case event.value of
    'l-m-r': swest.autoseq=1
    'reference-m-r': swest.autoseq=2
    'reference-m-difference': swest.autoseq=3
    'Off': swest.autoseq=0
  endcase
  
endcase


wlz.button(22) : begin
   widget_control, wlz.field(19),get_value=val
   w=where(val(0) eq swest.pbins)
   if w(0) ne -1 then begin
     if w(0)+1 le n_elements(swest.pbins)-1 then $
       swest.pbinselect=swest.pbins(w(0)+1) else $
       swest.pbinselect=swest.pbins(0)
     endif else swest.pbinselect=swest.pbins(1)
   widget_control, wlz.field(19),set_value=swest.pbinselect
                 endcase


 wlz.button(19) : begin
    ;print,'event.value ',event.value
    if swest.subtrbkg eq 'No' then swest.subtrbkg='Yes' else swest.subtrbkg='No'
    ;widget_control, wlz.field(14), set_value = swest.subtrbkg
                 endcase

 wlz.button(21) : begin
    if swest.specie_selct eq 0 then swest.specie_selct=1 $
      else swest.specie_selct=0
    specie=swest.specie
    widget_control, wlz.field(16), set_value = specie(swest.specie_selct)
    widget_control,wlz.field(14),set_value = swest.subtrbkg
   widget_control, wlz.field(15), set_value = swest.delete * swest.ndel
                  endcase
 
 wlz.button(18) : begin
   if swest.delete eq 0 then swest.delete=1 else swest.delete=0
   widget_control, wlz.field(15), set_value = swest.delete * swest.ndel
                   endcase                 

wlz.field(12) : begin
     WIDGET_CONTROL, wlz.field(12), GET_VALUE = val
     swest.nvmax=val(0)
     print,'wlz.field(12): en steps',swest.nvmax
                  endcase
                                                                      
endcase

end


;=============================== dspl_opt ====================================

pro dspl_opt,norealize=norealize

common swestuff,swest
common sharewidglz,wlz
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl

swpmd=['all elecs','all ions ','alt sects','alt spins','bckg test']

if xregistered('dspl_opt') then begin
  WIDGET_CONTROL, swest.wdsp.base(0),iconify=0 
  
  if keyword_set(swest.subtrbkg) eq 0 $
  then swest.subtrbkg='Yes' ;else swest.subtrbkg='No'
  widget_control, wlz.field(14), set_value = swest.subtrbkg

  widget_control, wlz.field(15), set_value = swest.delete * swest.ndel

  if keyword_set(swest.specie_selct) eq 0 then swest.specie_selct=0
  specie=swest.specie 
  widget_control, wlz.field(16), set_value = specie(swest.specie_selct)
 
  widget_control, wlz.field(17), set_value = vsmjf.scimode
  widget_control, wlz.field(18), set_value = swpmd(vsmjf.eleion_sweep)
  widget_control, wlz.field(19),set_value=swest.pbinselect
  
  widget_control,wlz.field(12),set_value=swest.nvmax
  
  return
endif


swest.wdsp.base(0) = WIDGET_BASE(TITLE = 'Distribution Function Display Options',$
   /COLUMN)

base=widget_base(swest.wdsp.base(0),/column,/frame)
rbase=widget_base(base,/row)   
swest.wdsp.button(0)=cw_bgroup(rbase,label_left=' ',$
  ['Strlf+veisf','Hide points','Contour labels','Interp 0, 180',$
   'Quit'],$
     /return_name,row=1)

rbase=widget_base(base,/row)     
swest.wdsp.button(1)=cw_bgroup(rbase,label_left='Sequence:',$
  ['l-m-r','reference-m-r',$
  'reference-m-difference','Off'],/exclusive,$
   row=1,/return_name) 


rbase=widget_base(base,/row)
wlz.button(22)=cw_bgroup(rbase,label_left='',['+/- pitch bin'],row=1)
wlz.field(19)=cw_field(rbase,/long,xsize=2,ysize=1,title=' ',/row)
wlz.field(12)=cw_field(rbase,title='Enstps',/return_events,/long,xsize=2,$
  ysize=1,/row)

rbase=widget_base(base,/row)
wlz.button(19)=cw_bgroup(rbase,$
  label_left='',['- Bckg'],row=1,/return_name)
wlz.field(14)=cw_field(rbase,/string,xsize=3,$
  ysize=1,title=' ',/row)


cbase=widget_base(base,/column)
rbase=widget_base(cbase,/row)
wlz.button(21)=cw_bgroup(rbase,label_left='',['Select'],row=1,/return_name)
wlz.field(16)=cw_field(rbase,/string,xsize=5,ysize=1,title=' ',/row)

rbase=widget_base(cbase,/row)
wlz.field(17)=cw_field(rbase,/integer,xsize=1,ysize=1,title='Mode',/row)
wlz.field(18)=cw_field(rbase,/string,xsize=9,ysize=1,title=' ',/row)


wlz.button(18)=cw_bgroup(cbase,$
  label_left='',['Glint'],row=1,/return_name)
wlz.field(15)=cw_field(cbase,/integer,xsize=3,$
  ysize=1,title=' ',/row)
  

if keyword_set(swest.subtrbkg) eq 0 $
  then swest.subtrbkg='Yes' ;else swest.subtrbkg='No'
widget_control, wlz.field(14), set_value = swest.subtrbkg

widget_control, wlz.field(15), set_value = swest.delete * swest.ndel

if keyword_set(swest.specie_selct) eq 0 then swest.specie_selct=0
specie=swest.specie 
widget_control, wlz.field(16), set_value = specie(swest.specie_selct)

widget_control,wlz.field(12),set_value=swest.nvmax
widget_control, wlz.field(17), set_value = vsmjf.scimode
widget_control, wlz.field(18), set_value = swpmd(vsmjf.eleion_sweep)
widget_control, wlz.field(19),set_value=swest.pbinselect
            
WIDGET_CONTROL, swest.wdsp.base(0), /REALIZE

XMANAGER, "dspl_opt", swest.wdsp.base(0) , GROUP_LEADER = GROUP ;hand off to manager

end

;-------------------------------- end dspl_opt ------------------------------


;========================== levelzero_event,event ============================

pro levelzero_event,event

common sharewidg,wa
common sharewidglz,wlz
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common drawf,pltype
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common shared,d


;print,'levelzero_event :',event.id,event.value

case getenv('LZPATH') eq '' of
0 : 
1 : begin
      if event.id ne wlz.button(14) then return
      if event.id eq wlz.button(14) and event.value ne 'Quit' and $
         event.value ne 'Parent' then return
      if event.id eq wlz.draw(1) and keyword_set(fh) eq 0 then return
    endcase
endcase


CASE event.id of

  wlz.draw(1) : begin   ;plot in first window 
    if keyword_set(recn) eq 0 or event.type eq 1 then return 
    case event.press of
    1: begin
         if swest.strlf_veisf eq 1 then strlf_veisf,wid=swest.lzwin(0) $
         else begin
           proc_fw,wid=swest.lzwin(0),pltype=pltype, err=err
           swest.win_pltype(0:n_elements(pltype)-1,0)=pltype
           swest.win_npltypes(0)=n_elements(pltype)
           swest.win_delete(0)=swest.delete
           swest.win_nvmax(0)=swest.nvmax
           swest.win_pbinselect(0)=swest.pbinselect
           swest.win_hidepoints(0)=swest.hidepoints
         endelse
       endcase    
    else:
    endcase      
               endcase

  wlz.draw(2) : begin        ;plot in second window
    if keyword_set(recn) eq 0 then return
    case event.press of
    1: begin
         if swest.strlf_veisf eq 1 then strlf_veisf,wid=swest.lzwin(1) $
         else begin
           proc_fw,wid=swest.lzwin(1),pltype=pltype, err=err
           swest.win_pltype(0:n_elements(pltype)-1,1)=pltype
           swest.win_npltypes(1)=n_elements(pltype)
           swest.win_delete(1)=swest.delete
           swest.win_nvmax(1)=swest.nvmax
           swest.win_pbinselect(1)=swest.pbinselect
           swest.win_hidepoints(1)=swest.hidepoints
         endelse
       endcase  
    else:
    endcase 
               endcase


  wlz.draw(3) : begin      ;plot in third window
    if keyword_set(recn) eq 0 then return
    case event.press of
    1: begin
         if swest.strlf_veisf eq 1 then strlf_veisf,wid=swest.lzwin(2) $
         else begin
           proc_fw,wid=swest.lzwin(2),pltype=pltype, err=err
           swest.win_pltype(0:n_elements(pltype)-1,2)=pltype
           swest.win_npltypes(2)=n_elements(pltype)
           swest.win_delete(2)=swest.delete
           swest.win_nvmax(2)=swest.nvmax
           swest.win_pbinselect(2)=swest.pbinselect
           swest.win_hidepoints(2)=swest.hidepoints
         endelse
       endcase  
    else:
    endcase 
               endcase

 wlz.button(1) : begin
     print,'NOT YET ACTIVE' & return
     for i=0,2 do begin
        swest.delete=swest.win_delete(i)
        swest.nvmax=swest.win_nvmax(i)
        swest.pbinselect=swest.win_pbinselect(i)
        swest.hidepoints=swest.win_hidepoints(i)
        pltype=swest.win_pltype(0:swest.win_npltypes(i)-1,i)
        wid=swest.lzwin(i)

        if i eq 0 then begin
          proc_fw,wid=wid,pltype=pltype 
        endif else begin
          if swest.delete eq 0 then $
          labl1='SWE '+swest.specie(swest.specie_selct)+' (Mask Off)' else $
          labl1='SWE '+swest.specie(swest.specie_selct)
          wset,wlz.win(wid) 
          if max(pltype) lt 10 then begin;   display in vpara, vperp coords
            fplot,wlz.win_xsize(wid),wlz.win_ysize(wid),pltype=pltype,$
              ion=swest.specie_selct,$
              labl1=labl1,$
              labl2=swest.spndt
          endif else begin            
            x=reverse(reform(pm(0,*)))
            if vsmjf.scimode eq 2 or vsmjf.scimode eq 11 then $
            wsteps=vsmjf.veistep(*,0,swest.ispinbl)
            if vsmjf.scimode eq 0 or vsmjf.scimode eq 1  or vsmjf.scimode eq 4 $
            then $
              wsteps=vsmjf.veistep
            y=alog10(volt_en(wsteps,/en,ion=swest.specie_selct))
            g=fltarr(n_elements(x),n_elements(y)) 
            for j=0,n_elements(x)-1 do g(j,*)=fm(*,n_elements(x)-1-j) 
   
            w=where(g ne 0)
            g(w)=alog10(g(w))
            mn=min(g(w),max=mx)
            w0=where(g eq 0)
            n_colors=!d.table_size-1
            g(w)=bytscl(temporary(g(w)),min=mn,max=mx,top=n_colors-1)
            if w0(0) ne -1 then g(w0)=0
    
            ymn=min(y,max=ymx) & ytck=5 
            ytickv=ymn+indgen(ytck+1)*(ymx-ymn)/ytck
            ytickn=string(10.^ytickv,format='(i5)')
            xmn=180. & xmx=0. & xrange=[xmn,xmx] & xtck=6  
     
            wset,wlz.win(wid)
            x_im_sz=wlz.win_xsize(wid)
            y_im_sz=wlz.win_ysize(wid)
            pos,1,posn,xoff=0.21
     
            img_ep, g, y, x, zmn=mn, zmx=mx,  pos=posn,  $
              ytickv=ytickv,ytitle='log eV', $
              ytickn=ytickn,$
              xrange=xrange, $
              xtickv=xmn+indgen(xtck+1)*(xmx-xmn)/xtck, $
              xtickn=string(xmn+indgen(xtck+1)*(xmx-xmn)/xtck,format='(i5)'),$
              subtitle=swest.spndt,$
              xticks=xtck, xtitle='Pitch Angle (deg)' ,xminor=1, $
              title=labl1,$
              charsize=1.25,charthick=1.0,xcharsize=1.0,$
              ximsize=x_im_sz,yimsize=y_im_sz,n_colors=n_colors 
            
          endelse
        endelse
     endfor 
                      endcase

  wlz.button(13) : begin
    list=wst.options_list(*,3)
    case list(event.index) of
      'fcntrs': pltype=[0]
      'fcuts' : pltype=[2]
      'redF' : pltype=[1]
      'f fcuts' : pltype=[0,2]
      'f F' :  pltype=[0,1]
      'f F fcuts' : pltype=[0,1,2]
      'fsurface' : pltype=[3]
      'triangles': pltype=[4,1]
      'f(en,pa)' : pltype=[10]
      'F_integ'  : pltype=[-1]
      'strahl': ;pltype=[11]
      else :
    endcase
    print,'pltype ',pltype
    swest.pltype_index=event.index
                endcase

  wlz.button(20) : begin  ;increment f data rec number
    case event.value of
      ' + ' : recn=recn+1
      ' - ' : recn=recn-1
      else:
    endcase
    widget_control,wlz.field(11),set_value=swest.ispinbl
    widget_control,wlz.field(10),set_value=recn    
    wst.timsel='lz'
    wst.lz_is_read=0
          
     if pltwin_frmt and swest.autoseq gt 0 then begin
                      swest.ilzplt=swest.ilzplt+1
                      case swest.autoseq of
                      1: begin
                        pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                        proc_fw,wid=pltwin,pltype=pltype
                         endcase
                      2: begin
                        pltwin=swest.ilzplt-2*fix(swest.ilzplt/2) + 2
                        proc_fw,wid=pltwin,pltype=pltype
                         endcase
                      3: begin
                        proc_fw,wid=2,pltype=pltype
                        proc_fw,wid=3,pltype=pltype
                         endcase
                      endcase
                       
    endif
                    
  endcase

  wlz.button(15) : begin     ;increment f data spin number
    ;print,'event.value ',event.value

    ;NOTE!! The following test compares swest.ispinbl with
    ;the CURRENT record's vsmjf.n_spins which, when record is
    ;also incremented, may cause the test to skip the last spin
    ;in a record which happens to have a number of spins
    ;greater than the current vsmjf.n_spins, 
    ;for example when there are 8 spins in mode2 

    case event.value of
      ' + ' : begin
                    swest.ispinbl=swest.ispinbl+1
                    if swest.ispinbl gt vsmjf.n_spins-1 then begin
                       swest.ispinbl=0 
                       if recn lt fh.nmf-1 then recn=recn+1
                       wst.lz_is_read=0
                    endif
                    widget_control,wlz.field(11),set_value=swest.ispinbl
                    widget_control,wlz.field(10),set_value=recn
                    wst.timsel='lz'
                    print,'wlz.button(15) ',$
                         event.value, swest.ispinbl,wst.timsel
                    
                    if pltwin_frmt and swest.autoseq gt 0 then begin
                      swest.ilzplt=swest.ilzplt+1
                      case swest.autoseq of
                      1: begin
                        pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                        proc_fw,wid=pltwin,pltype=pltype
                         endcase
                      2: begin
                        pltwin=swest.ilzplt-2*fix(swest.ilzplt/2) + 2
                        proc_fw,wid=pltwin,pltype=pltype
                         endcase
                      3: begin
                        proc_fw,wid=2,pltype=pltype
                        proc_fw,wid=3,pltype=pltype
                         endcase
                      endcase
                       
                    endif
  
                endcase

      ' - ' : begin
                    swest.ispinbl=swest.ispinbl-1
                    if swest.ispinbl lt 0 then begin
                       swest.ispinbl=vsmjf.n_spins-1 
                       if recn gt 0 then recn=recn-1
                       wst.lz_is_read=0
                    endif
                    widget_control,wlz.field(11),set_value=swest.ispinbl
                    widget_control,wlz.field(10),set_value=recn
                    wst.timsel='lz'
                    
                     if pltwin_frmt and swest.autoseq gt 0 then begin
                      swest.ilzplt=swest.ilzplt+1
                      case swest.autoseq of
                      1: begin
                        pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                        proc_fw,wid=pltwin,pltype=pltype
                         endcase
                      2: begin
                        pltwin=swest.ilzplt-2*fix(swest.ilzplt/2) + 2
                        proc_fw,wid=pltwin,pltype=pltype
                         endcase
                      3: begin
                        proc_fw,wid=2,pltype=pltype
                        proc_fw,wid=3,pltype=pltype
                         endcase
                      endcase
                       
                    endif
                    
                 endcase

      'Reset' : swest.ispinbl=0l         ;reset to zero

      else :
    endcase
                endcase

  wlz.button(14) : begin
    if keyword_set(recn) eq 0 then begin
           recn=1 & swest.ispinbl=0 & endif
    list=wst.options_list(*,0)
    case list(event.index) of
      'Open LZ' : begin     
          swest.spn=0 & swest.ispinbl=0
          lzinput
          if keyword_set(fh) eq 0 then return
          ;start time of file
            widget_control,wlz.field(20),$
             set_value=pb5_ymdhms([fh.fst.yr,fh.fst.dy,fh.fst.ms])

          ;end time of file
            widget_control,wlz.field(21),$
             set_value=pb5_ymdhms([fh.lst.yr,fh.lst.dy,fh.lst.ms])

          ;if keyword_set(wst.xydata) eq 0 then wst.xydata=dblarr(2)         
          wst.xydata(0)=double(fh.fst.ms)/3600000.d
          hour_hms,wst.xydata(0),hms,lhms=lhms
          wst.hms=hms(0)
          wst.pb5=[fh.fst.yr,fh.fst.dy,fh.fst.ms]
          wst.timsel='lztm'
          print,'initial selected time ',$
            wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
          widget_control,wlz.field(13),set_value=lhms

          indate=strmid(wst.lzdate,2,6)
          wst.indate=indate
          WIDGET_CONTROL, wa.field(18), set_value = indate
          ;WIDGET_CONTROL, wa.base(0), /show
                   endcase

      'Scan LZ file' : swelzw
      'Veis data' : begin
                     get_recnspin            ;get recn and spin number
                     plotcounts
                   endcase
      'Strahl data' : plcnts_strl
       
      else :
    endcase
                endcase
 
  wlz.button(26) : begin
    case event.value of
      'Parent' : begin
                   wset,wa.win(0)
                   ;if pltwin_frmt eq 1 then plt
                   wst.xyrange=wst.xyrange_mainnwin
                   wst.xywindow=wst.xywindow_mainnwin
                   wst.xysize=wst.xysize_mainnwin
                   wst.ylog=wst.ylog_mainnwin
                   WIDGET_CONTROL, wa.base(0), /show
                 endcase
      'Quit' :  WIDGET_CONTROL, event.top, /DESTROY
      'HELP' : $
        xdisplayfile,getenv('WGGSBASE')+'help_swe_lz_data_display.txt',$
        width=100
    endcase
                    endcase

  wlz.button(24) : begin
    list=wst.options_list(*,1)
    case list(event.index) of
      'Xloadct' : xloadct
      'Restore clrs' : clrtbl_indx 
    endcase
                   endcase

 wlz.button(25) : begin
    list=wst.options_list(*,2)
    case list(event.index) of
      'Hrdcpy_bw' : begin
                 wst.hardcopy=1
                 wst.printer=wst.printer_bw
                 wst.print_flnm=wst.print_flnm_bw
                 wst.print_cmd=wst.print_cmd_bw 
                 clrtbl_indx,/hardcopy
                    endcase
      'Hrdcpy_clr' : begin
                 wst.hardcopy=1
                 wst.printer=wst.printer_clr
                 wst.print_flnm=wst.print_flnm_clr
                 wst.print_cmd=wst.print_cmd_clr
                 clrtbl_indx,/hardcopy
                     endcase 
    endcase
                  endcase 
 
  wlz.field(10) : begin
     WIDGET_CONTROL, wlz.field(10), GET_VALUE = val
     recn=val(0)
     wst.timsel='lz'
     print,'wlz.field(10): selected recn ',recn
     wst.lz_is_read=0
                 endcase

 wlz.field(11) : begin
     WIDGET_CONTROL, wlz.field(11), GET_VALUE = val
     swest.ispinbl=val(0)
     wst.timsel='lz'
     print,'wlz.field(11): selected spinbl ',swest.ispinbl
     wst.lz_is_read=0
                 endcase

 wlz.field(13) : begin
     WIDGET_CONTROL, wlz.field(13), GET_VALUE = val
     ;convert string hhmmss to long hhmmss
     hhmmss=long(val)
     wst.xydata(0)=hms_hour(hhmmss)
     hour_hms,wst.xydata(0),hms,lhms=lhms
     wst.timsel='lztm'
     wst.hms=hms(0)
     wst.pb5=ymd_pb5(long(wst.lzdate))
     wst.pb5(2)=long(wst.xydata(0)*3600000.d)
     print,'wlz.field(13): selected time ',$
       wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
     wst.lz_is_read=0
                 endcase

 wlz.button(23) : begin
                      pltwin_frmt=1
                      swest.lzsurvey=event.index
                      WIDGET_CONTROL, wlz.draw(0), GET_VALUE=windw
                      wlz.win(0)=windw
                      wset,wlz.win(0) & erase
                      plt,/lzwin
                      ;plt_lzwindow   
                      ;levelzero
                    endcase     
                 
  wlz.button(17) : begin
    list=wst.options_list(*,4)
    case list(event.index) of
      'Orbit' : orbit_plot
      'Display options' : dspl_opt
      'Foreshock geometry' : $
        if wst.foreshock eq 0 then wst.foreshock=1 else wst.foreshock=0
    endcase                 
                   endcase  

  wlz.button(27) : begin
     case event.value of
     'Initialize':
     'Save f_time':
     endcase
                   endcase
  
  wlz.field(22) : begin
                  endcase 
  
  wlz.button(28) : begin
     case event.value of
     'Save to file':
     'Overplot times':
     endcase
                   endcase
  
  wlz.draw(0) : begin
    case event.press of
    1: begin
         wa_draw0_ev,event
         if pltwin_frmt eq 1 then begin
           oplot_sec=pb5_sec(wst.pb5)
           wst.timsel='lztm'    
           if swest.autoseq gt 0 then begin
                      swest.ilzplt=swest.ilzplt+1
                      case swest.autoseq of
                      1: begin
                        pltwin=swest.ilzplt-3*fix(swest.ilzplt/3) + 1
                        if pltype(0) eq 11 then begin
                          wset,wlz.win(pltwin)
                          strl_phth_srvy,/phth_image 
                        endif  $
                        else proc_fw,wid=pltwin,pltype=pltype
                         endcase
                      2: begin
                        pltwin=swest.ilzplt-2*fix(swest.ilzplt/2) + 2
                        proc_fw,wid=pltwin,pltype=pltype
                         endcase
                      3: begin
                        proc_fw,wid=2,pltype=pltype
                        proc_fw,wid=3,pltype=pltype
                         endcase
                      endcase                       
           endif 
           if wst.foreshock then foreshock_geometry                   
         endif
    endcase
    else:
    endcase
                endcase
    
  else:
endcase

end


;============================= levelzero ====================================

pro levelzero,group=group

common sharewidglz,wlz
common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common m2stuff,hkind,ghk,vblhsp,sblhsp
common m1stuff,hkm1,vsm1,vdatc,sdatc
common shared,d
common log_delog,comp_tbl,dcomp_tbl
common drawf,pltype
common wstuff,wst
common swestuff,swest
common sharelevelzero,pltwin_frmt,oplot_sec
common sharedspp,list


if keyword_set(pltwin_frmt) eq 0 then pltwin_frmt=1  ;0

if keyword_set(pltype) eq 0 then begin
  pltype=[0,2] & swest.pltype_index=4
endif

;read compress/decompress tables
  if keyword_set(dcomp_tbl) eq 0 then decompress_tbl
  
;get indices of instrument housekeeping into mjf array, lz.mf   
  if keyword_set(ihk) eq 0 then ihkmap,ihk 

;get tm map of science and genl hk data offsets into lz 
  if keyword_set(hkm1) eq 0 then mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  
  if keyword_set(ghk) eq 0 then mode2map,hkind,ghk,vblhsp,sblhsp

;get mode1 and mode2 sun phase angles of detectors, unit vectors, glint masks
   phasem1_nospin
   phasem2

if keyword_set(swest.specie_selct) eq 0 then swest.specie_selct=0 ;default: ele
  ;specie=['elecs','ions']


;initialize delete flag to delete selected samples: 1= yes delete   0= no delete
  if keyword_set(swest.delete) eq 0 then swest.delete=1 ;nodelete 0, yesdelete 1
  print,'initialization : delete ',swest.delete
;deletion of veis samples, set= 0 (45 May95), 1 (27), 2 (27+18), 3 (27+18+12)
  swest.delete_set=0 

if xregistered('levelzero') then begin

if pltwin_frmt eq 1 then begin
   
     ;selected indices from set of indices d.pnlist.list
        pnlsel=d.pnlsel(where(d.pnlsel ne -1))
     ;structure of plotting parameters for selected plot variables 
        pm=d.pnl(pnlsel)
        list=pm(*).labl   
     widget_control,wlz.button(23),set_value=list
     if total(d.pnlsel-d.pnlsel_last) ne 0 then swest.lzsurvey=0
     widget_control,wlz.button(23),set_droplist_select=swest.lzsurvey
     
     WIDGET_CONTROL, wlz.draw(0), GET_VALUE=windw
     wlz.win(0)=windw
     wset,wlz.win(0) & erase
     plt,/lzwin
   endif
   
   if keyword_set(fh) eq 0 then begin
     WIDGET_CONTROL, wlz.base(3),iconify=0
     return
   endif
   
   widget_control,wlz.field(20),$
             set_value=pb5_ymdhms([fh.fst.yr,fh.fst.dy,fh.fst.ms])

   widget_control,wlz.field(21),$
             set_value=pb5_ymdhms([fh.lst.yr,fh.lst.dy,fh.lst.ms])

   if wst.xydata(0) eq 0 then begin
     wst.xydata(0)=double(fh.fst.ms)/3600000.d
     hour_hms,wst.xydata(0),hms,lhms=lhms
     wst.hms=hms(0)
     wst.pb5=[fh.fst.yr,fh.fst.dy,fh.fst.ms]
     wst.timsel='lztm'
     print,'initial selected time ',$
      wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
     widget_control,wlz.field(13),set_value=lhms
   endif   

   widget_control,wlz.button(13),set_value=wst.options_list(0:8,3)
   widget_control,wlz.button(13),set_droplist_select=swest.pltype_index
     
  WIDGET_CONTROL, wlz.base(3),/show
  return
endif

print,'levelzero: timsel ',wst.timsel


;------------------- set up main base widgets --------------------------------

wlz.base(3) = WIDGET_BASE(TITLE = 'SWE LZ Data Display', /COLUMN)  ;main base

rbase1=widget_base(wlz.base(3),/row)
cbase1=widget_base(rbase1,/column)
cbase2=widget_base(rbase1,/column)

rbase=widget_base(cbase1,/row,space=20)
wlz.button(26)=cw_bgroup(rbase,['Parent',$
                                'HELP',$
                                'Quit'],$
       column=3,/return_name)

;opt_list1
wst.options_list(0:3,0)=[  'Open LZ',$
                           'Veis data',$        
                           'Strahl data',$
                           'Scan LZ file']
list=wst.options_list(0:3,0)                           
wlz.button(14)=widget_droplist(rbase,title='Level zero',$
      value=list)

;opt_list4
wst.options_list(0:8,3)=['fcntrs',$
                         'f(en,pa)', $
                         'fcuts',$
                         'redF',$
                         'f fcuts',$
                         'f F',$
                         'fsurface',$
                         'triangles',$
                         'F_integ']
                         ;'strahl']
list=wst.options_list(0:8,3)
wlz.button(13)=widget_droplist(rbase,title='Plot Type',$
      value=list)

rbase=widget_base(cbase1,/row,space=20)      
;opt_list2
wst.options_list(0:1,1)=['Xloadct',$
                         'Restore clrs']
list=wst.options_list(0:2,1)
wlz.button(24)=widget_droplist(rbase,title='Colors',$
      value=list)

;opt_list3
wst.options_list(0:1,2)=['Hrdcpy_bw',$
                         'Hrdcpy_clr']
list=wst.options_list(0:1,2)
wlz.button(25)=widget_droplist(rbase,title='Printers',$
      value=list)

;opt_list5
wst.options_list(0:2,4)=['Orbit',$
                         'Display options',$
                         'Foreshock geometry']
list=wst.options_list(0:2,4)
wlz.button(17)=widget_droplist(rbase,title='Applications',$
      value=list)
      
rbase=widget_base(cbase2,/row)
wlz.button(15)=cw_bgroup(rbase,[' + ',' - '],row=1,$
  label_left='Spin',/return_name)
wlz.field(11)=cw_field(rbase,title=' ',/return_events,/long,xsize=3,$
  ysize=1,/row)

wlz.button(20)=cw_bgroup(rbase,[' + ',' - '],row=1,$
  label_left='    Mfrm',/return_name)
wlz.field(10)=cw_field(rbase,title=' ',/return_events,/long,xsize=4,$
  ysize=1,/row)
  
rbase=widget_base(cbase2,/row)
wlz.field(13)=cw_field(rbase,title='hhmmss',/return_events,/string,xsize=6,$
  ysize=1,/row)

wlz.field(20)=cw_field(rbase,title=' ',/string,xsize=15,ysize=1,/row)
wlz.field(21)=cw_field(rbase,title='to',/string,xsize=15,ysize=1,/row)


case pltwin_frmt of

0: begin
     rbase0=widget_base(wlz.base(3),/row)
     x_size = 1000               
     y_size = 580      
   endcase

1: begin 
     wlz.base(4)=widget_base(wlz.base(3),/row,/FRAME)
     rbase0=wlz.base(4)
     x_size = 850  ;950               
     y_size = 180  ;150  
     wlz.draw(0) = WIDGET_DRAW(rbase0,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size, YSIZE = y_size)

     ;selected indices from set of indices d.pnlist.list
        pnlsel=d.pnlsel(where(d.pnlsel ne -1))
     ;structure of plotting parameters for selected plot variables 
        pm=d.pnl(pnlsel)
        list=pm(*).labl
     cbase=widget_base(wlz.base(4),/column )
     rbase=widget_base(cbase,/row)            
     wlz.button(23)=widget_droplist(rbase,title=' ',$
      value=list)
 
     rbase=widget_base(wlz.base(4),/column,/FRAME)
     wlz.button(27)=cw_bgroup(rbase,['Initialize','Save f_time'],row=2,$
       label_top='Record Events',/return_name) 
     wlz.field(22)=cw_field(rbase,title=' ',/string,xsize=10,ysize=1,/row)
     wlz.button(28)=cw_bgroup(rbase,['Save to file','Overplot times'],$
       row=2,label_left=' ',/return_name) 
       
     rbase0=widget_base(wlz.base(3),/row)
     x_size =1000
     y_size=  580      
   endcase

endcase

cbase1=widget_base(rbase0,/column)
wlz.draw(1) = WIDGET_DRAW(cbase1,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size)

cbase2=widget_base(rbase0,/column)
wlz.draw(2) = WIDGET_DRAW(cbase2,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size)

cbase3=widget_base(rbase0,/column)
wlz.draw(3) = WIDGET_DRAW(cbase3,/BUTTON_EVENTS,/FRAME, $
       UVALUE ='DRAW_WIN_EVENT',RETAIN = 2, XSIZE = x_size/3, YSIZE = y_size)

WIDGET_CONTROL, wlz.base(3), /REALIZE

if pltwin_frmt eq 1 then begin
  WIDGET_CONTROL, wlz.draw(0), GET_VALUE=windw
  wlz.win(0)=windw
  wset,wlz.win(0) & erase
  plt,/lzwin
endif

WIDGET_CONTROL, wlz.draw(1), GET_VALUE=windw
wlz.win(1)=windw

WIDGET_CONTROL, wlz.draw(2), GET_VALUE=windw
wlz.win(2)=windw

WIDGET_CONTROL, wlz.draw(3), GET_VALUE=windw
wlz.win(3)=windw

wlz.win_xsize(1:3)=x_size/3
wlz.win_ysize(1:3)=y_size


if keyword_set(wst.lzdate) ne 0 then begin
  widget_control,wlz.field(20),$
             set_value=pb5_ymdhms([fh.fst.yr,fh.fst.dy,fh.fst.ms])

  widget_control,wlz.field(21),$
             set_value=pb5_ymdhms([fh.lst.yr,fh.lst.dy,fh.lst.ms])

   if wst.xydata(0) eq 0 then begin
     wst.xydata(0)=double(fh.fst.ms)/3600000.d
     hour_hms,wst.xydata(0),hms,lhms=lhms
     wst.hms=hms(0)
     wst.pb5=[fh.fst.yr,fh.fst.dy,fh.fst.ms]
     wst.timsel='lztm'
     print,'initial selected time ',$
      wst.xydata(0),'  ',wst.hms,'  ',wst.pb5
     widget_control,wlz.field(13),set_value=lhms
  endif
endif

XMANAGER, "levelzero", wlz.base(3), GROUP_LEADER = GROUP  ;hand off to manager


END

