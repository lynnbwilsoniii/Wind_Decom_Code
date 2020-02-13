PRO strl_newmode,Wid=wid

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common swestuff,swest & common wstuff,wst & common sharewidg,WDGT
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
            f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,$
            f90cut,v90cut,fpara,fperp
common sharelevelzero,pltwin_frmt,oplot_sec

ntest2=0

;---check to see if an lz record has been read
if wst.lz_is_read then goto,lzisread

;---get recn and spin number
get_recnspin            
                 
if keyword_set(recn) eq 0 then begin
     recn=1 & swest.ispinbl=0
endif

point1:

;---process selected lz record
print,'processinf recn ',recn
proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,elec_ion_m1=elec_ion_m1,err=err
print,'err: ',err
if err ne '' then return

;---test lz record for:
;   tm mode,
;   whether in background test mode,
;   current spinbl number against max number spins in current mjf,
;   whether current spinbl contains selected specie,
;   for lzdata quality flag
if (swest.veis_on eq 1) then begin ;                Test only when VEIS is ON.
   lztest_code_ret,tmmode_ihk=tmmode_ihk,ntest2,retcode,repcode
   if retcode then return         ;return to calling pro
   if repcode then goto, point1   ;read next record
endif else if (vsmjf.scimode ne 7) then begin ; Test for mode7 if VEIS is OFF.
   print,'Wrong mode!!! Resetting to first record...' & recn = 0
   return ;        Reset record number to first record and return to caller...
endif

widget_control,WDGT.swelz_mode,set_value=string(vsmjf.scimode,format='(i1)')

wst.lz_is_read=1

lzisread:    ; an lz record has been read

widget_control,WDGT.swelz_recfld,set_value=recn
spinbl=swest.ispinbl & widget_control,WDGT.swelz_spinfld,set_value=spinbl

;                             Get and print: tjd and pb5 times and time labels.
times_labels & widget_control,WDGT.swelz_hmsfld,Set_Value=wst.lhms

get_3slzmag,wst.pb5,magfld ;       Get magnetic field (possibly not available).

;     If magnetic field data is not available and this is "ok" with the user...
if ((not keyword_set(magfld)) and (getenv('LZNOMAG') eq 'ok')) then begin
   magfld = [-1.0,0.0,0.0] ;          Set magfield to default anti-solar value.
   print,'' & print,'NO B-FIELD: Using default anti-solar direction.'
endif

print,'magfld ', magfld ;       Output (read or default) magfld data to screen.

s = size(vsmjf.strl) ;               Initialize basic dimensions and indices...
swest.nstrldets = s[1] & swest.nstrlsteps = s[2]
swest.nspins = s[3] & swest.istrldet = 0 & swest.istrlstep = 0
if keyword_set(swest.ispinbl) eq 0 then swest.ispinbl=0

wset,swest.win[wid] ;                     Select window and store window sizes.
x_im_sz = swest.win_xsize[1] & y_im_sz = swest.win_ysize[1]

strl_phth_newmode,X_im_sz=x_im_sz,Y_im_sz=y_im_sz ; ** Display strahl spectrum.

; The strahl spectrum may be sent to a TIFF file if a hardcopy was requested
;  and if the visual depth (color mode) is more than 8-bit (in TrueColor mode).
device,Get_Visual_Depth=depth & criteria = (wst.hardcopy and (depth gt 8))

if (criteria) then begin ;          If the current color mode is 'TrueColor'...
   ;      Temporarily set the display device to use the decomposed color model,
   ;        store the contents of the current display window (the one with the
   ;        strahl spectrum generated above) and then return to decomposed=0...
   device,Decomposed=1 & image = tvrd(True=1) & device,Decomposed=0

   pltfil = getenv('IDLSAV')+'strl_newmode.tif' ; Generate flnm for TIFF image.
   print,'Saving captured image to: ',pltfil ;           Display screen output.

   write_tiff,pltfil,reverse(image,3),1 ;    Write TIFF image to sel. filename.
endif else if wst.hardcopy then $ ;  Otherwise (8-bit mode is not implemntd)...
   dummy = dialog_message(['IDL is currently in PseudoColor (8-bit) mode.',$
                          '          The '+getenv('WGGSVERSION')+$
                                              ' WGGS tool-version       ',$
                          'does not currently support image saving for ',$
                          'this mode.  This feature must be added.'],$
                          Title='Wrong Color Mode',/Error,$ ;         Generate
                          Dialog_Parent=WDGT.swelz_hcpy) ; error dialog-widget.

if wst.hardcopy then wst.hardcopy = 0 ;                    Reset hardcopy flag.

oplot_sec = pb5_sec(wst.pb5) & wset,swest.win(0) & erase & plt,/LZwin
      
end