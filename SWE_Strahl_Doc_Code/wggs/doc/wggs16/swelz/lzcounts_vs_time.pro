

;======================= MAIN: lzcounts_vs_time ===============================



;reads and process a SWE lz file (RJF Jan95)

common lzstuff,lzfile,lundat,recn,fh,lz,ihk,sp,vsmjf,veis_hvtbl
common log_delog,comp_tbl,dcomp_tbl
common oastuff,atfile,tpb5_at,gse_ra,gse_dec
common orbstuff,orbfile,tpb5_orb,gse_pos,gse_vel
common wstuff,wst
common swestuff,swest
common shared,d

define_widgets
setpaths
panelist
structuresw

print,'dates to be processed:'
lzctsdates=['20011119','20011120','20011121','20011122','20011123','20011124',$
            '20011125','20011126','20011127','20011128','20011129','20011130']
print,lzctsdates
pb5ref=[2001l,323l,0l]

vstep0=6
vstep1=13
  
readlz=0
if readlz then begin
 

lprnt=1

univgmask=0   ;if 1 then use universal glint mask

scpot=0    ;spacecraft potential =0

;<<<<<<<<<<<<<<<<<<<<<<<<<<< end input parameters >>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ndet=6 & nsect=6
cts0=lonarr(ndet,100000l)
cts1=lonarr(ndet,100000l)
elaps=dblarr(100000l)
ebias1=fltarr(100000l)
ebias2=fltarr(100000l)
hvi1=fltarr(100000l)
hvi2=fltarr(100000l)
kspn=-1l
 

print,'processing options:'
print,'univgmask ',univgmask

answ='' & print,'Hit return to continue, or any other key to stop.' 
read,answ & if answ ne '' then stop

;----------------------- begin ---------------------------------------------- 

for idate=0,n_elements(lzctsdates)-1 do begin 
;for idate=0,1 do begin
date=lzctsdates(idate)
if idate eq 0 then recs=[100,2000] else recs=[1,2000]

;initialize structures
  structuresw
  panelist

;set background removal flag (it is initially set to 'Yes' in structuresw.pro)
  swest.subtrbkg='No'

wst.date_file='Date'    
wst.indate=date
wst.lzindate=wst.indate  
swest.univgmask=univgmask   ;if 1 then use universal glint mask

point1:

;------- Read SWE ion KP's
idatype=where(d.datype eq 'swe_ionkp')
input,idatype,err=err
 
ctmmode=['u','m','s','e']
ctmrate=['s','f']
elecion=['electrons','ions']

lpr=0
rfill=-1.0e31

;read compress/decompress tables
  decompress_tbl

;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1, mode1 tm map of science and genl hk data offsets into lz.mf
   mode1map
   mode6map
   mode2map

;get mode1 and mode2 sun phase angles of detectors, unit vectors
   phasem1
   phasem2

;open LZ data file and read header, 
;  read mag, orb-att, background data, glint masks
;  and process first two records to get spin period
  lzinput,err=err,oknoatt=oknoatt
  if err ne '' then stop,err

  
  elsc=dblarr(n_elements(d.swe_ionkpdat))
  for i=0,n_elements(d.swe_ionkpdat)-1 do $
    elsc(i)=pb5_elapsec(d.swe_ionkpdat(i).tpb5,pb5ref)
  if idate eq 0 then begin
    elaps_ionkp=elsc
    dens_ionkp=d.swe_ionkpdat.n  
  endif else begin
    elaps_ionkp=[elaps_ionkp,elsc]
    dens_ionkp=[dens_ionkp,d.swe_ionkpdat.n]
  endelse
  
;get size of orb-att arrays
  szpos=size(gse_pos)
  szra=size(gse_ra)
  szdec=size(gse_dec)
  
print,' '
print,'scimode ',vsmjf.scimode
print,'eleion_sweep ',vsmjf.eleion_sweep
print,' '

last_scimode=-1
last_atindx=-1

nfill=0l



;initialize (nwglint will be set ge 0 and glint searched just once per date)
nwglint=-1

recn_range=[recs(0),recs(1)<fh.nmf]  ;[1,fh.nmf] ;read entire file

recn1=recn_range(0)
recn2=recn_range(1)

print,'date, recn1,recn2 ',date, recn1,recn2

firstrecnout=1
 
for recn=recn1,recn2 do begin

;process selected lz record (set keyword lpr=1 to turn on print each record)
  proc_rec,date_time,tmmode_ihk=tmmode_ihk,lpr=0,$
    elec_ion_m1=elec_ion_m1,err=err
  scimodechange=0  
  if err ne '' then begin
     print,recn,'  ',err 
     if err eq $
       'change from mode1 to mode2; mode2 background and glint required' or $
        err eq $
     'change from mode2 to mode1; mode1 background and glint required' then begin
       scimodechange=1
       goto,endlzfile 
     endif else goto,endrecordloop
  endif

;determine whether in telemetry science mode
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  if tmmode_ihk ne 2 then begin
    print,' '&print,'tm not in science mode, tm mode = ',ctmmode(tmmode_ihk)
    goto,endrecordloop
  endif

;check data quality; if one mnf flag set, then skip entire record
  n_mnf=250
  if total(lz.qlty(0:n_mnf-1)) gt 0 then  goto, endrecordloop 

;check spinperiod
  if sp.spinp lt 0.9*3.05 or sp.spinp gt 1.1*3.05 then begin 
    print,'bad spinperiod'
    goto,endrecordloop
  endif

;science mode
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
  scimode1 = scimode_ihk eq 0 or scimode_ihk eq 1
  scimode2 = scimode_ihk eq 2 or scimode_ihk eq 11
  scimode4 = scimode_ihk eq 4
  scimode6 = scimode_ihk eq 6
  
  if not float(scimode1 or scimode4 or scimode6) then begin
    print,'not science mode 1 or scimode4 or science mode 6'
    goto,endrecordloop
  endif 


;---- ready to process mode1 or mode2 electrons and ions  ----------------------
;-----NOTE: (ion spectra not testd)
     
ndets=vsmjf.n_vdets
nvsteps=vsmjf.n_vesteps 
nsectors=vsmjf.n_sectors
nspins=vsmjf.n_spins

;print,sp.mfrecn,sp.mfyr,sp.mfdy,sp.mfms,sp.spinp

if nwglint eq -1 then begin
  wglint=where(vsmjf.xveis(*,*,*,0) eq -1,nwglint)
  print,' ' & print,'number of glint points = ',nwglint & print,' '
endif

;start spin loop
  for ispin=0,nspins-1 do begin
    
    eleion=0
    
        
    ;print,'recn, ispin, eleion ',recn, ispin, eleion

;voltage steps: offsets into voltage table 
;assumes all sectors electrons or ions
      if scimode1 or scimode4 then vsteps=vsmjf.veistep $
      else if scimode6 then vsteps=reform(vsmjf.veistep(*,ispin))
    
    ;get energies and speed each voltage step for given specie
      energy=volt_en(vsteps,/en,ion=eleion)   ;ev
      velocity=double(volt_en(vsteps,/vel,ion=eleion))  ;cm/s 
      
    ;NOTE: 
    ;The input counts, vsmjf.cveis, are corrected for relative gain changes 
    ;with time and for detector efficiency variation with energy,
    ;and vsmjf.cveis_b have background subtracted
    ;If background point should be larger than measured counts, 
    ;then vsmjf.cveis_b is et to 0.
    ;Glint points of the vsmjf.cveis array are negative their value.
    ;Pitch angle and spectra data will have glint mask turned ON, 
    ;i.e., glint points removed

    cblk=fltarr(ndets,nvsteps,nsectors)
    cblk(*,*,*)=vsmjf.veis(*,*,*,ispin)
    
     wsteps=vsteps

    ;set indices
      n_sectors=nsectors
      n_vdets=ndets
    
    endspinloop: 
    
    kspn=kspn+1
    ;if kspn eq 0 then pb5ref=vsmjf.pb5tim_vsbl(*,ispin)
    elaps(kspn)=pb5_elapsec(vsmjf.pb5tim_vsbl(*,ispin),pb5ref)
    ebias1(kspn)=.03918*lz.mf(ihk(18).offs)
    ebias2(kspn)=.03918*lz.mf(ihk(28).offs)
    hvi1(kspn)=1.9531*lz.mf(ihk(12).offs)
    hvi2(kspn)=1.9531*lz.mf(ihk(11).offs)
    for idet=0,ndets-1 do begin
      ;w=where(cblk(idet,vstep0,*) ge 0,nw)
      ctsdet=fltarr(nvsteps*nsectors)
      ctsdet(*)=reform(cblk(idet,*,*)) 
      w=where(ctsdet ge 0,nw)     
      ;if nw gt 0 then cts0(idet,kspn)=total(cblk(idet,vstep0,w))/nw 
      if nw gt 0 then cts0(idet,kspn)=max(ctsdet(w))
      
      w=where(cblk(idet,vstep1,*) ge 0,nw)   
      if nw gt 0 then cts1(idet,kspn)=total(cblk(idet,vstep1,w))/nw
     
    endfor
      
  endfor  ;end spin loop
 ;stop 
  skiprec=50
  if fix(recn/skiprec)*skiprec eq recn then  begin
     print,'lztim: recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)'
     print, recn,lz.recn,lz.yr,lz.dy,lz.ms,vsmjf.suntim_vsbl(0)
  endif

  endrecordloop:

endfor     ;end record loop

endlzfile:

endfor

cts0=cts0(*,0:kspn)
cts1=cts1(*,0:kspn)
elaps=elaps(0:kspn)
ebias1=ebias1(0:kspn)
ebias2=ebias2(0:kspn)
hvi1=hvi1(0:kspn)
hvi2=hvi2(0:kspn)
nspn=kspn+1

save,filename=getenv('IDLSAV')+'veiscts',cts0,cts1,elaps,ebias1,ebias2,$
  hvi1,hvi2,elaps_ionkp,dens_ionkp

stop
endif

restore,getenv('IDLSAV')+'veiscts_max_19_20'
help,cts0,cts1,elaps,ebias1,ebias2,hvi1,hvi2,elaps_ionkp,dens_ionkp
!p.thick=3.0
hardcopy=0
clrtbl_indx
clr=(140./147.)*!d.n_colors 
start:
if hardcopy eq 0 then window,0,xsize=800,ysize=800

hrdy=elaps/3600.d0
hrdy_ionkp=elaps_ionkp/3600.d0
npl=6
posn=fltarr(4,npl)
pos,npl,posn,xoff=0.15,xtop=0.84,yoff=0.075
xrange=[0.,2*24.];[0.,12*24.]
xticks=2;6
xtickv=xrange(0)+((xrange(1)-xrange(0))/xticks)*indgen(xticks+1)
xtickname=strarr(xticks+1)
for i=0,xticks do xtickname(i)=string(pb5ref(1)+xtickv(i)/24.,format='(i3)')
xticklen=0.05
xminor=12;8
noerase=1+intarr(npl) & noerase(0)=0
xcharsize=0.0001+fltarr(npl) & xcharsize(npl-1)=1.0
xtitle=strarr(npl) 
xtitle(npl-1)='calendar day after '+string(pb5_ymd(pb5ref),format='(i8)')
det=[0,1,4];[5,4,3] ;[0,1,2];
strdet=strarr(3)
for i=0,n_elements(det)-1 do strdet(i)=string(det(i),format='(i1)')

psym=1
symsize=0.2
charsize=1.35
charthick=3.0
npltmax=20000l ;100000l ;20000l ;35000l

ipl=0
plot,congrid(hrdy_ionkp,n_elements(hrdy_ionkp) < npltmax),$
  congrid(dens_ionkp,n_elements(hrdy_ionkp) < npltmax),$
  xstyle=1,xrange=xrange,xticks=xticks,charthick=charthick,$
  ystyle=1,yrange=[1.,100.],ytitle='density',ylog=1,title='SWE/VEIS',$
  psym=psym,symsize=symsize,pos=posn(*,ipl),xminor=xminor,xticklen=xticklen,$
  noerase=noerase(ipl),xcharsize=xcharsize(ipl),charsize=charsize

ipl=ipl+1  
plot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(ebias1,n_elements(hrdy) < npltmax),$
  xstyle=1,xrange=xrange,xticks=xticks,charthick=charthick,$
  ystyle=1,yrange=[0.,3.5],yticks=5,yminor=5,ytitle='ebias (kV)',$
  psym=psym,symsize=symsize,pos=posn(*,ipl),xminor=xminor,xticklen=xticklen,$
  noerase=noerase(ipl),xcharsize=xcharsize(ipl),charsize=charsize
oplot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(ebias2,n_elements(hrdy) < npltmax),$
  psym=psym,symsize=symsize,color=clr 
xyouts,xrange(0)+3.,1.5,/data,' side 1; dets 012',charsize=charsize
xyouts,xrange(0)+3.,1.0,/data,' side 2: dets 543',charsize=charsize,color=clr 

ipl=ipl+1  
plot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(hvi1,n_elements(hrdy) < npltmax),$
  xstyle=1,xrange=xrange,xticks=xticks,charthick=charthick,$
  ystyle=1,yrange=[0.,320],yticks=8,yminor=4,ytitle='hv curr (mA)',$
  psym=psym,symsize=symsize,pos=posn(*,ipl),xminor=xminor,xticklen=xticklen,$
  noerase=noerase(ipl),xcharsize=xcharsize(ipl),charsize=charsize
oplot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(hvi2,n_elements(hrdy) < npltmax),$
  psym=psym,symsize=symsize,color=clr 

ipl=ipl+1

plot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(reform(cts0(det(0),*)),n_elements(hrdy) < npltmax),$
  xstyle=1,xrange=xrange,xticks=xticks,charthick=charthick,$
  ystyle=1,yrange=[1.,1000.],ylog=1,ytitle='det '+strdet(0),$
  psym=psym,symsize=symsize,pos=posn(*,ipl),xminor=xminor,xticklen=xticklen,$
  noerase=noerase(ipl),xcharsize=xcharsize(ipl),charsize=charsize
oplot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(reform(cts1(det(0),*)),n_elements(hrdy) < npltmax),$
  psym=psym,symsize=symsize,color=clr
;xyouts,30.,300.,/data,'energy step '+string(vstep0,format='(i2)'),$
xyouts,20.,300.,/data,'spectral maximum',$
  charsize=charsize
xyouts,20.,150.,/data,'hi energy step '+string(vstep1,format='(i2)'),$
  charsize=charsize,color=clr
    
ipl=ipl+1
plot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(reform(cts0(det(1),*)),n_elements(hrdy) < npltmax),$
  xstyle=1,xrange=xrange,xticks=xticks,charthick=charthick,$
  ystyle=1,yrange=[1.,1000.],ylog=1,ytitle='det '+strdet(1),$
  psym=psym,symsize=symsize,pos=posn(*,ipl),xminor=xminor,xticklen=xticklen,$
  noerase=noerase(ipl),xcharsize=xcharsize(ipl),charsize=charsize  
oplot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(reform(cts1(det(1),*)),n_elements(hrdy) < npltmax),$
  psym=psym,symsize=symsize,color=clr

ipl=ipl+1
plot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(reform(cts0(det(2),*)),n_elements(hrdy) < npltmax),$
  xstyle=1,xrange=xrange,xticks=xticks,charthick=charthick,$
  xtitle=xtitle(ipl),xtickv=xtickv,xtickname=xtickname,$
  ystyle=1,yrange=[1.,1000.],ylog=1,ytitle='det '+strdet(2),$
  psym=psym,symsize=symsize,pos=posn(*,ipl),xminor=xminor,xticklen=xticklen,$
  noerase=noerase(ipl),xcharsize=xcharsize(ipl),charsize=charsize
oplot,congrid(hrdy,n_elements(hrdy) < npltmax),$
  congrid(reform(cts1(det(2),*)),n_elements(hrdy) < npltmax),$
  psym=psym,symsize=symsize,color=clr

if hardcopy then begin
  device,/close
  set_plot,'x'
  hardcopy=0
  print,'hardcopy file: ',pltfil
  !p.thick=1.
  stop
  ;spawn,'lp -d '+'lephpc114 '+pltfil   
endif

set_plot,'ps',/interpolate
clrtbl_indx,/hardcopy
pltfil=getenv('IDLSAV')+'veiscts_max.ps' 
device,/inches,xoffset=0.75,yoffset=1.,xsize=7.,ysize=9.5,filename=pltfil,$
  /color,bits=8
clr=(140./147.)*!d.n_colors  
hardcopy=1
goto,start
      
end



