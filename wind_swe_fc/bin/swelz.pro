; @(#)swelz.pro  VERSION 1.3    10/28/94   15:09:30
; This is a set of procedures to read and process SWE level zero data.
; Jim Byrnes provided the read procedures.
; A simple widget interface is used to select (direct access) data records and
; display a limited amount of housekeeping information.
; The calling procedure is SWELZ.PRO.

; R. J. Fitzenreiter, Dec 1993
; Modified April, 1994 (RJF)
; Modified July 13, 1994 (FVM)  straightened display SWE LZ Data, 
	;changed time format (but did it poorly)
	;edited time_utc.pro (but commented out print statement)
	;will edit to stamp source_file_name at top of outfile
	;should make file create filenames based on source file	name
	;		(or maybe on time stamp)	
	;want to add an option to output fcdata for all records
;================================= mode1 ==================================

pro mode1 

common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf
common sharewidg,wa

 
; determine tm mode, tm rate, science mode, and mjf count from instr hk
tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
tmrate_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
;stop
;get time tagged spincnt
  time_utc,lz.mf(scindx(hkm1(1).offs:hkm1(1).offs+6)),$
    tjd,sec,hour,min,isec,ms,hms,spincnt
  sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
  widget_control,set_value=hms,wa.field1(1)
  widget_control,set_value=string(sp.spincnt,format='(i3)'),wa.field1(2)
  
;get spin period
  sp.mjfcnt=lz.mf(ihk(1).offs)
  spinperiod,sp  
  widget_control,set_value=string(sp.spinp,format='(f6.3)'),wa.field1(3)

  print,' '
  print,'time tag:  tjd      secdy  hh:mm:ss.ms  spincnt  spinperiod'
  print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',ms,sp.spincnt,sp.spinp,$
    format='(9x,i5,2x,f9.3,2x,i2,a1,i2,a1,i2,a1,i3,2x,i3,f12.3)'
  ;print,tmmode_ihk,tmrate_ihk,scimode_ihk,sp.mjfcnt,format='(3i5,z3.2)'

if sp.spinp eq 0 then return ;if true, then spin period known

;**** getting to this point means there is a known spin period this mjf ******
;**** for now (1/12/94), spin period not determined for first mjf of file *** 


;set parameters
  iclicks_sunpulse=4096 & iclicks_bci=40
  spin_bci=float(iclicks_bci)/float(iclicks_sunpulse) 
  phi_bci=iclicks_bci*360.d0/iclicks_sunpulse 
  bcis_spin=360.d0/phi_bci & nbcis_spin= fix(bcis_spin) & phidly_sun=42.5
  phi1_bci0=181.1 & phi2_bci0=1.1 & phisc_bci0=46.1 & phistrl_bci0=200.6 
  n_hkvars=32 & n_hkmode=3 
  n_spins=7 & n_vs=924  & n_fcbl=31 & n_fc=122
  n_vdat=576  & n_sdat=336   
  n_vdets=6 & n_vesteps=16 & n_sectors=6  & n_strdets=12 & n_strphis=28

;set bci indices corrsponding to strahl samples (mode1)
  ibci_strl=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,    $
             52,53,54,55,56,57,58,59,60,61,62,63,64,65 ]

;determine spin phase at each bci (each data samle) when mode changes
;or at begin of mjf
;also, data structure for veis/strahl data samples defined in pro phasem1 
 scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
 if scimode_ihk ne sp.lst_scimod then phasem1  

	

;--------------- make veis/strahl data structure assignments this mjf -------

 vsmjf.descr='veis-strahl data samples'
 vsmjf.tjd=tjd      ;trucated julian day of time-tagged spin this mjf
 vsmjf.sec=sec      ;seconds of day of time-tagged spin this mjf
 vsmjf.hour=hour    ;hour of day of time-tagged spin this mjf
 vsmjf.min=min      ;min of hour of time-tagged spin this mjf
 vsmjf.isec=isec    ;sec of min of time-tagged spin this mjf
 vsmjf.ms=ms        ;ms of sec of time-tagged spin this mjf
 vsmjf.mjfcnt=lz.mf(ihk(1).offs)  ;mjf counter
 vsmjf.scimode=scimode_ihk        ;science mode


;---------- determine time at each bci (each data sample) for mode 1 --------

;sun time (suntim_vsbl(j) at spincnt_vsbl(j) ) for veis/strahl j'th spin block,
;i.e., seconds from tjd epoch for j'th spin when sc x-axis crosses sun line 
  spincnt_vsbl=intarr(n_spins)
  spincnt_vsbl(0:n_spins-1)=fix(lz.mf(scindx(vsm1(0).offs(0:n_spins-1))))
  ispin_diff=intarr(n_spins)
  ispin_diff=spincnt_vsbl-spincnt
  w=where(ispin_diff lt -33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)+256
  w=where(ispin_diff gt  33,nw) &if nw gt 0 then ispin_diff(w)=ispin_diff(w)-256
  sunsec_vsbl=dblarr(n_spins) 
  sunsec_vsbl=sec+ispin_diff*sp.spinp
  suntim_vsbl=dblarr(n_spins)
  suntim_vsbl=tjd*86400.d0+sunsec_vsbl

;time between bci's assuming constant spin period over mjf
  sec_bci=spin_bci*sp.spinp
 
;time of each bci relative to true sun pulse, suntim_vsbl(j), for each spin j
  time=indgen(nbcis_spin)*sec_bci + (phidly_sun/phi_bci)*sec_bci + sec_bci 


;time of each veis and strahl data sample
  for ispin=0,n_spins-1 do begin
    ;time of each veis data sample in seconds from tjd epoch
    ibci=-1
    for isector=0,n_sectors-1 do begin
      ibci=ibci+1                        ;skipping 1 bci each sweep
      for ivestep=0,n_vesteps-1 do begin
        ibci=ibci+1
        vsmjf.secveis(ivestep,isector,ispin)=suntim_vsbl(ispin)+time(ibci)
      endfor
    endfor
    ;time of each strahl data sample in seconds from tjd epoch
    for istrphi=0,n_strphis-1 do vsmjf.secstrl(istrphi,ispin)= $
      suntim_vsbl(ispin)+time(ibci_strl(istrphi))
  endfor


  sp.lst_scimod=scimode_ihk

  ;help,vsmjf,/str

  end


;================================= phasem1 =====================================

pro phasem1

;determine spin phase at each bci (each data sample) for mode 1
;at beginning of mjf and whenever there is a mode change

common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf

print,'phasem1'

;set parameters
  iclicks_sunpulse=4096 & iclicks_bci=40
  spin_bci=float(iclicks_bci)/float(iclicks_sunpulse) 
  phi_bci=iclicks_bci*360.d0/iclicks_sunpulse 
  bcis_spin=360.d0/phi_bci & nbcis_spin= fix(bcis_spin) & phidly_sun=42.5
  phi1_bci0=181.1 & phi2_bci0=1.1 & phisc_bci0=46.1 & phistrl_bci0=200.6 
  n_hkvars=32 & n_hkmode=3 
  n_spins=7 & n_vs=924  & n_fcbl=31 & n_fc=122
  n_vdat=576  & n_sdat=336   
  n_vdets=6 & n_vesteps=16 & n_sectors=6  & n_strdets=12 & n_strphis=28


;define data structure for veis/strahl data samples 
  vsmjf={  vsdata,$
  descr:'   ',tjd:0,sec:0d,hour:0l,min:0l,isec:0l,ms:0,mjfcnt:0,scimode:0,$
  n_vdets:0l,n_vesteps:0l,n_sectors:0l,n_spins:0l,n_strdets:0l,n_strphis:0l,$
  veis:bytarr(n_vesteps,n_sectors,n_vdets,n_spins),veistep:bytarr(n_vesteps),$
  secveis:dblarr(n_vesteps,n_sectors,n_spins),$
  phiveis:fltarr(n_vesteps,n_sectors,n_vdets),theveis:fltarr(n_vdets),$
  strl:bytarr(n_strphis,n_strdets,n_spins),strlstep:bytarr(n_spins),$
  secstrl:dblarr(n_strphis,n_spins),$
  phistrl:fltarr(n_strphis),thestrl:fltarr(n_strdets)  }
 
;--------------- make veis/strahl data structure assignments  ---------------
  vsmjf.n_vdets=n_vdets            ;number of veis detectors
  vsmjf.n_vesteps=n_vesteps        ;number of veis energy steps per scan
  vsmjf.n_sectors=n_sectors        ;number of sectors
  vsmjf.n_spins=n_spins            ;number of spins with data per mjf
  vsmjf.n_strdets=n_strdets        ;number strahl detectors
  vsmjf.n_strphis=n_strphis        ;number of strahl samples (azimuth) per mjf


;------------------------spin phase of each data sample -----------------------
	
;phivdet = azimuthal angle of each veis detector in spin plane relative to its 
;respective sensor normal, measured in the spin direction
  phivdet=[-13.78,53.00,-47.25,47.25,-53.00,13.78]

;thevdet = polar angle of each veis detector measured from spin axis
  thevdet=[143.49,73.79,58.36,121.64,106.21,36.51]

;thesdet = polar angle of each strahl detector measured from spin axis
  thesdet=[61,66,71,76,81,86,94,99,104,109,114,119]

;set bci indices corrsponding to strahl samples (mode1)
  ibci_strl=[ 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,    $
             52,53,54,55,56,57,58,59,60,61,62,63,64,65 ]

 
;spin phase angle of veis sensor normals, strahl sensor direction,
;and spacecraft x-axis relative to sun at each bci per spin 

phiv1=indgen(nbcis_spin)*phi_bci + phi1_bci0   ;sensor #1
phiv2=indgen(nbcis_spin)*phi_bci + phi2_bci0   ;sensor #2
phisc=indgen(nbcis_spin)*phi_bci + phisc_bci0  ;sc x-axis
phistrl=indgen(nbcis_spin)*phi_bci + phistrl_bci0 ;strahl sensor

;spin phase angle of veis data samples relative to direction of sun;
    ibci=-1
    for isector=0,n_sectors-1 do begin
      ibci=ibci+1                  ;skipping 1 bci each sweep
      for ivestep=0,n_vesteps-1 do begin
         ibci=ibci+1
         vsmjf.phiveis(ivestep,isector,0:2)=phiv1(ibci)-phivdet(0:2)
         vsmjf.phiveis(ivestep,isector,3:5)=phiv2(ibci)+phivdet(3:5)
      endfor
    endfor

;make range of phi 0:360
  w=where(vsmjf.phiveis ge 360.,nw)
  if nw gt 0 then vsmjf.phiveis(w)=vsmjf.phiveis(w)-360.

  w=where(vsmjf.phiveis lt 0.,nw)
  if nw gt 0 then vsmjf.phiveis(w)=vsmjf.phiveis(w)+360.

;polar angle of veis data samples relative to spin axis
  vsmjf.theveis=thevdet

;spin phase angle of strahl data samples relative to direction of sun,
;positive counterclockwise about spin axis (z-axis)
  vsmjf.phistrl=phistrl(ibci_strl)
  w=where(vsmjf.phistrl ge 360.,nw)
  if nw gt 0 then vsmjf.phistrl(w)=vsmjf.phistrl(w)-360.

  w=where(vsmjf.phistrl lt 0.,nw)
  if nw gt 0 then vsmjf.phistrl(w)=vsmjf.phistrl(w)+360.

;polar angle of strahl data samples relative to spin axis
  vsmjf.thestrl=thesdet

;determine unit vectors and form dot products to test orthogonality
  ;vctrs

end




;================================= vctrs ===================================

pro vctrs

;compute detetector unit vectors and test for orthogonality

common sharewidg,wa
common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf

openw,lun,'swe_angles.prt',/get_lun
print,'opening data file "swe_angles.prt"'
printf,lun,'SWE detector angles'

x=fltarr(vsmjf.n_vesteps,vsmjf.n_sectors,vsmjf.n_vdets) & y=x & z=x
;form vectors along detector centerline in the s/c coords
for iestep=0,vsmjf.n_vesteps-1 do for isector=0,vsmjf.n_sectors-1 do begin
    x(iestep,isector,*)=$
      sin(vsmjf.theveis(*)*!dtor)*cos(vsmjf.phiveis(iestep,isector,*)*!dtor)
    y(iestep,isector,*)=$
      sin(vsmjf.theveis(*)*!dtor)*sin(vsmjf.phiveis(iestep,isector,*)*!dtor)
    z(iestep,isector,*)=cos(vsmjf.theveis(*)*!dtor)
    printf,lun,' '
    printf,lun,'energy step=',iestep,'  sector=',isector
    printf,lun,'    detector   unit vectors(s/c coords) x,y,z      magnitude'
    for i=0,vsmjf.n_vdets-1 do printf,lun,i+1,$
      x(iestep,isector,i),y(iestep,isector,i),z(iestep,isector,i),$
      sqrt(x(iestep,isector,i)^2+y(iestep,isector,i)^2+z(iestep,isector,i)^2)

    printf,lun,' ' & printf,lun,'       detector pairs     dot product'    
    for i=0,vsmjf.n_vdets-1 do for j=0,vsmjf.n_vdets-1 do $
      printf,lun,i,j,$
                     x(iestep,isector,i)*x(iestep,isector,j)+$
                     y(iestep,isector,i)*y(iestep,isector,j)+$
                     z(iestep,isector,i)*z(iestep,isector,j)
endfor

free_lun,lun

openw,lun,'thephi_afv.dat',/get_lun
print,'opening data file "thephi_afv.dat"'
printf,lun,vsmjf.n_vesteps,vsmjf.n_sectors,vsmjf.n_vdets
printf,lun,vsmjf.theveis
printf,lun,vsmjf.phiveis
free_lun,lun

end


;================================= proc_rec ===================================

pro proc_rec

common sharewidg,wa
common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf


ctmmode=['u','m','s','e']
ctmrate=['s','f']

;process selected lz mjf record num recn

;get hhmmss.ms from msec of day (record header)
  ms_hms,lz.ms,h,m,s 

;determine tm mode from instr hk and test whether in science mode
  tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
  if tmmode_ihk ne 2 then begin
    print,' ' & print,'tm not in science mode...tm mode = ',ctmmode(tmmode_ihk)
    return
  endif

;******* getting to this point means we are in tm science mode ************** 

;determine science mode
  scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

;do mode dependent unpacking
  if scimode_ihk eq 0 or  scimode_ihk eq 1 then mode1

sp.datayes=1  ;set flag that at least one record has been processed
end



;================================= read_rec ==================================

pro read_rec,date_time

common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf

;read selected lz mjf record num recn (Jim Byrnes' procedure)
  lz=read_lzrcd(lundat,recn,fh.rln,fh.spcid) ;lz=data structure incl header

  ms_hms,lz.ms,h,m,s  ;get hhmmss.ms from msec of day
  date_time=string(lz.yr,format='(i4)') + ' ' + string(lz.dy,format='(i3)') +$
    '   ' + string(h,format='(i2)') + ':' + string(m,format='(i2.2)') +$
    ':' + string(s,format='(f6.3)')

  
  end




;============================= swelz_event ===================================

PRO swelz_event, event

common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf
common sharewidg,wa

;help,event,/str
;help,wa,/str

CASE event.id OF
              
        wa.field1(4): begin  ;run with new input lz file
                widget_control,get_value=infile,wa.field1(4)
                WIDGET_CONTROL, event.top, /DESTROY
		swelz
		endcase

	wa.field1(5): BEGIN  ;select mjf record
		
		WIDGET_CONTROL, wa.field1(5), GET_VALUE = val
                recn=val(0)
                print,' '		
		PRINT, 'mjf data recn selected = ' + STRING(recn)
                print,'physical record = ',fh.nphyrc - fh.nmf + recn
                read_rec,date_time ;read record and display time from header
                widget_control,set_value=date_time,wa.field1(0)
                proc_rec  ;process lz record 
		END

        wa.button1(0): begin
          case event.value of
             0 : begin                                   ;'New File'
                    WIDGET_CONTROL, event.top, /DESTROY
		    swelz
                 endcase

             1 : WIDGET_CONTROL, event.top, /DESTROY     ;'Quit'
          endcase
        endcase

        wa.button1(1): begin                             ;'Increment recn'
          case event.value of
             0 : recn_new=recn+1                             ;'+'

             1 : recn_new=recn-1                             ;'-'
          endcase
          if recn_new ge 1 and recn_new le fh.nmf then begin
            recn=recn_new
            widget_control,wa.field1(5),set_value=recn
            read_rec,date_time		;read record and display time from header
            widget_control,set_value=date_time,wa.field1(0)
            proc_rec			;process lz record
          endif 
        endcase

        wa.button1(2): begin
           case event.value of
             0: prt_flhdr,0,infile,fh		;print file header
             1: begin
                  ms_hms,lz.ms,h,m,s           	;get hhmmss.ms from msec of day
                  prt_hdr,0,lz,h,m,s,infile	;print major frame header
                endcase
             2: prt_mf,0,lz.mf,lz.recn,infile  	;print major frame data
             3: prt_ihk,0,ihk,lz,infile        	;print instr hk data
             4: prt_hk,0,hkm1,lz.mf(scindx)    	;print genl hk
             5: prt_fc,0,fcblm1,lz.mf(scindx)  	;print faraday cup data
           endcase
	endcase

        else:
                
ENDCASE
END


;=================== CALLING PROCEDURE: swelz =================================

PRO swelz, GROUP = GROUP

common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf
common sharewidg,wa

;set up structures
  sp={spinparams,spincnt:0b,tjd:0l,sec:0d,mjfcnt:0b,spinp:0d,$
      old_spincnt:0b,old_tjd:0l,old_sec:0d,old_mjfcnt:0b,$
      lst_spinp:0d,lst_tjd:0l,lst_sec:0d,newmjf:1,datayes:0,lst_scimod:-1}

  wa={wa_widgets,base:0l,slider:0l,button1:lonarr(10),field1:lonarr(10),$
    menu1:0l}  

    
;get indices, scindx, of science data, 
;i.e., the indices of the mjf array, lz.mf,  without the instr hk
  ind_scidat,scindx 

;get indices of instrument housekeeping into mjf array, lz.mf   
  ihkmap,ihk 

;get mode1 tm map of science and genl hk data offsets into scindx
  mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  ;hkm1=genl hk offsets for mode1
                                         ;fcblm1=faraday cup offsets
                                         ;vsm1=veis/strl offsets
                                         ;vdatc=veis data index
                                         ;sdatc=strl data index
                    
;get input lz data file
;  openr,lun,'swelzdatapath',/get_lun
;  lzpath=''
; readf,lun,lzpath & free_lun,lun
  lzpath='./lz/'
  infile=pickfile(/read,path=lzpath(0),filter='*.dat',$
  title='Level Zero Data Files')
;  openw,lun,'swelzdatapath',/get_lun
;  printf,lun,lzpath & free_lun,lun

;open level zero file
  openr,lundat,infile,/get_lun
  print, ' ' & print,'input data file name',infile

;read file header (Jim Byrnes' procedure)
  fh=read_lzhdr(lundat) ;fh=file header structure 
 
  prt_flhdr,0,infile,fh  ;print file header

;widget interface to select and read lz records
  wa.base = WIDGET_BASE(TITLE = 'SWE LZ Data', /COLUMN)

  wa.field1(4)=cw_field(wa.base,title='lz data file',$
    value=infile,xsize=30,/string,/noedit)

  wa.button1(1)=cw_bgroup(wa.base,['+','-'],row=1,label_left='Increment recn',$
    ids=ids)

  wa.field1(5)=cw_field(wa.base,title='record num   ',/return_events)
 
  wa.field1(0)=cw_field(wa.base,title='yr, day, hms ',$
    value='',xsize=20,/string,/noedit)
   
  cbase1=widget_base(wa.base,/column)
  wa.field1(1)=cw_field(cbase1,title='timetag      ',$
    value='',xsize=20,/string,/noedit)

  wa.field1(2)=cw_field(cbase1,title='spin count   ',$
    value='',xsize=20,/string,/noedit)

  wa.field1(3)=cw_field(cbase1,title='spin period  ',$
    value='',xsize=20,/string,/noedit)

  list=$
  [ 'file header',$ 
    'data record header',$
    'lz data',$ 
    'instr hk data',$
    'genl hk data',$
    'fc data'  ]
  wa.button1(2)=cw_bgroup(cbase1,list,row=7,label_top='Print Options')

  rbase1=widget_base(wa.base,/row)
  wa.button1(0)=cw_bgroup(rbase1,['New File','Quit'],row=1)

WIDGET_CONTROL, wa.base, /REALIZE

;read and process first record in file
  recn=1
  widget_control,set_value=recn,wa.field1(5)
  read_rec,date_time		;read record and display time from header
  widget_control,set_value=date_time,wa.field1(0)
  proc_rec			;process lz record

XMANAGER, "swelz", wa.base, GROUP_LEADER = GROUP  ;hand off to manager

END


