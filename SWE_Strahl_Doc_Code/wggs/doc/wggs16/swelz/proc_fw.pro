;=========================== proc_fw ==========================================

pro proc_fw,wid=wid,pltype=pltype,nopltf=nopltf,err=err,F_integ=F_integ,$
  statistcs=statistcs,lsscn=lsscn,vparc=vparc

common sharewidg,WDGT
common lzstuff,infile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common shared,d
common wstuff,wst
common swestuff,swest
common fplt,zgrd,F,nx,ny,vmax,fm,vxm,vym,em,pm,nm,$
f0cut,v0cut,p0cut,f180cut,v180cut,p180cut,f90cut,v90cut,fpara,fperp
common shareredf,sbase,button1,button2,draw1,draw2,field1,field2,$
  ndistribs,xc1,xc2,sgnslop,x1,F1,x2,F2

err=0

if keyword_set(infile) eq 0 then return
if infile eq '' then return
if keyword_set(wid) eq 0 then wid=1
if keyword_set(swest.subtrbkg) eq 0 then swest.subtrbkg='Yes'
if keyword_set(F_integ) eq 0 then F_integ=0
if keyword_set(statistcs) eq 0 then statistcs=0
if keyword_set(lsscn) eq 0 then lsscn=0
if keyword_set(vparc) eq 0 then vparc=0

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
lztest_code_ret,tmmode_ihk=tmmode_ihk,ntest2,retcode,repcode
if retcode then return         ;return to calling pro
if repcode then goto, point1   ;read next record
       
widget_control,WDGT.swelz_mode,set_value=string(vsmjf.scimode,format='(i1)')

wst.lz_is_read=1

lzisread:    ;an lz record has been read

widget_control,WDGT.swelz_recfld,set_value=recn
spinbl=swest.ispinbl & widget_control,WDGT.swelz_spinfld,set_value=spinbl
if keyword_set(swest.nvmax) eq 0 then swest.nvmax=vsmjf.n_vesteps
swest.nvmax = swest.nvmax < vsmjf.n_vesteps
 
if (vsmjf.scimode eq 0 or vsmjf.scimode eq 1 or vsmjf.scimode eq 4) then begin
   if vsmjf.eleion eq 1 then stop,'MODE 1 ION MODE!!'
endif

ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps
nsectors=vsmjf.n_sectors
   
;---get and print: tjd and pb5 times and time labels
times_labels
  
widget_control,WDGT.swelz_hmsfld,set_value=wst.lhms

;---get magnetic field
get_3slzmag,wst.pb5,magfld
print,'magfld ', magfld

;---get phase density vs velocity, f vs v; includes patch and mode2 vel sorting
get_f_v_patch,spinbl,vpot,vel,v_vpot,bncoef,fegaussfit,vgaussfit,$
  fblk,npatch,nvmin,nvsteps,wx,wy,wz,w,fe,/swf

;---print,v - vpot vs f (spinaveraged)
print_vf,vel,vpot,ue,w,fe,npatch,nvmin,ndets,nvsteps,nsectors
  
;-------arrays can now be prepared for contouring----------------------------

;---vsmjf.n_vesteps - nvmax = number of steps cut from top of velocity range,
;   whereas nvmin = number of steps to cut from bottom end of measured range,
;   i.e., nvmin = number of energy steps below sc potential
nvmax=swest.nvmax

;---truncate arrays above velocity index nvmax-1
if nvmax lt nvsteps then truncate_nvmax,fe,wx,wy,wz,w,nvmax

;---prepare plotting arrays 
fparr,fe,wx,wy,wz,w,npatch,nvmin,vel,vpot,reform(magfld),$
  pltype=pltype,err=err

;---do the selected type of distribution function (LZ) plot
if keyword_set(nopltf) eq 0 then nopltf=0 else nopltf=1
if nopltf eq 0 then $
  do_lzplot,pltype=pltype,F_integ=F_integ,wid=wid,lsscn=lsscn,vparc=vparc

print,'wst.timsel ',wst.timsel  
  
end


;------------------- end  proc_fw --------------------------------------------
