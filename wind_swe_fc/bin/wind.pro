; %Z%%M%  VERSION %I%    %G%   %U%
;This is a beta-version WIND analysis program by Frank V. Marcoline.
;It will contain lz file analysis procedures and a complete WIND help menu.
;Much of the lz code was supplied by Dick Fitzenreiter.

; This is a set of procedures to read and process SWE level zero data.
; Jim Byrnes provided the read procedures.
; A simple widget interface is used to select (direct access) data records and
; display a limited amount of housekeeping information.
; The calling procedure is WIND.PRO.

; R. J. Fitzenreiter, Dec 1993
; Modified April, 1994 (RJF)
; Modified July, 1994 (FVM)  
	;straightened display SWE LZ Data (padded cw_field titles w/spaces)
	;changed time format to include zeros ie:  15:05:09.035
	;edited time_utc.pro time format as well
	;changed prt_*.pro files to use output filenames based on output type 
	;edited prt_*.pro to stamp source_file_name at top of outfile
		;will make output filename based on source 
		;(or maybe on time stamp)	
		;want to add an option to output fcdata for all records
; Modified Oct, 1994 (FVM)
	;Changed widget hierarchy to structure based definitions
	;Changed look of window
	;Added window options Close (Iconify) & Help (Open fhelp)
	;Added lz analysis functions
	;Added text window for output
	;Added options for saving output from text window to file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION writefile, file, output
;on_error, 2
  file=file(0)			;convert string array to string!!!!!!!!!!!!!!!!
  ;help,file ;& print,file
  ;help,output
  openw, wunit, file, /get_lun, error=i
  IF (i ge 0) THEN BEGIN 
    printf, wunit, output, format='(a)'
    CLOSE,/ALL
    return, 1
  ENDIF ELSE BEGIN
    CLOSE,/ALL
    return, 0
  ENDELSE
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION readfile, file
;on_error, 2
  openr, runit, file, /get_lun, error=i
  IF i lt 0 then BEGIN 
    input = [ !err_string, ' Can not open ' + file]
  ENDIF ELSE BEGIN
    input = strarr(1000)
    i = 0
    c = ''
    while not eof(runit) do begin
      readf,runit,c
      input(i) = c
      i = i + 1
    endwhile
    input = input(0:i-1)
    CLOSE,/ALL
  ENDELSE
  return, input
END

;================================= mode1 ==================================

pro mode1 

common sharewidg,w
;common sharewidg,w,quitb,statisb,functionb,textb,displayb,extrab
common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf

 
; determine tm mode, tm rate, science mode, and mjf count from instr hk
tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
tmrate_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
;stop
;get time tagged spincnt
  time_utc,lz.mf(scindx(hkm1(1).offs:hkm1(1).offs+6)),$
    tjd,sec,hour,min,isec,ms,hms,spincnt
  sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
  widget_control,set_value=hms,w.statisb.field(1)
  widget_control,set_value=string(sp.spincnt,format='(i3)'),w.statisb.field(2)
  
;get spin period
  sp.mjfcnt=lz.mf(ihk(1).offs)
  spinperiod,sp  
  widget_control,set_value=string(sp.spinp,format='(f6.3)'),w.statisb.field(3)

;  print,' '
;  print,'time tag:  tjd      secdy  hh:mm:ss.ms  spincnt  spinperiod'
;  print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',ms,sp.spincnt,sp.spinp,$
    format='(9x,i5,2x,f9.3,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,2x,i3,f12.3)'
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
  ww=where(ispin_diff lt -33,nw) &if nw gt 0 then ispin_diff(ww)=ispin_diff(ww)+256
  ww=where(ispin_diff gt  33,nw) &if nw gt 0 then ispin_diff(ww)=ispin_diff(ww)-256
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

;print,'phasem1'

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

;;;;;;;;;;;;;;;;had to change all variables 'w' to 'ww'
;make range of phi 0:360
  ww=where(vsmjf.phiveis ge 360.,nw)
  if nw gt 0 then vsmjf.phiveis(ww)=vsmjf.phiveis(ww)-360.

  ww=where(vsmjf.phiveis lt 0.,nw)
  if nw gt 0 then vsmjf.phiveis(ww)=vsmjf.phiveis(ww)+360.

;polar angle of veis data samples relative to spin axis
  vsmjf.theveis=thevdet

;spin phase angle of strahl data samples relative to direction of sun,
;positive counterclockwise about spin axis (z-axis)
  vsmjf.phistrl=phistrl(ibci_strl)
  ww=where(vsmjf.phistrl ge 360.,nw)
  if nw gt 0 then vsmjf.phistrl(ww)=vsmjf.phistrl(ww)-360.

  ww=where(vsmjf.phistrl lt 0.,nw)
  if nw gt 0 then vsmjf.phistrl(ww)=vsmjf.phistrl(ww)+360.

;polar angle of strahl data samples relative to spin axis
  vsmjf.thestrl=thesdet

;determine unit vectors and form dot products to test orthogonality
  ;vctrs

end




;================================= vctrs ===================================

pro vctrs

;compute detetector unit vectors and test for orthogonality

common sharewidg,w
;common sharewidg,w,quitb,statisb,functionb,textb,displayb,extrab
common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf

openw,lun,'swe_angles.prt',/get_lun
;print,'opening data file "swe_angles.prt"'
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
;print,'opening data file "thephi_afv.dat"'
printf,lun,vsmjf.n_vesteps,vsmjf.n_sectors,vsmjf.n_vdets
printf,lun,vsmjf.theveis
printf,lun,vsmjf.phiveis
free_lun,lun

end


;================================= proc_rec ===================================

pro proc_rec

common sharewidg,w
;common sharewidg,w,quitb,statisb,functionb,textb,displayb,extrab
common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf

ctmmode=['u','m','s','e']	;unknown,?,science,engineering
ctmrate=['s','f']		;slow,fast

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
  date_time=string(lz.yr,format='(i4)') + ', ' + string(lz.dy,format='(i3)') +$
    ', ' + string(h,format='(i2)') + ':' + string(m,format='(i2.2)') +$
    ':' + string(s,format='(f6.3)')

  
  end




;============================= wind_event ===================================

PRO wind_event, event

common sharewidg,w
;common sharewidg,w,quitb,statisb,functionb,textb,displayb,extrab
common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf

help,event,/str
help,w,w.quitb,w.statisb,w.functionb,/str

CASE event.id OF
              
;        w.quitb.field(4): begin  ;run with new input lz file
;                widget_control,get_value=infile,w.statisb.field(4)
;                WIDGET_CONTROL, event.top, /DESTROY
;		wind
;		endcase

	w.statisb.field(5): BEGIN  ;select mjf record
		
		WIDGET_CONTROL, w.statisb.field(5), GET_VALUE = val
                recn=val(0)
;                print,' '		
;		PRINT, 'mjf data recn selected = ' + STRING(recn)
;                print,'physical record = ',fh.nphyrc - fh.nmf + recn
                read_rec,date_time ;read record and display time from header
                widget_control,set_value=date_time,w.statisb.field(0)
                proc_rec  ;process lz record 
		END

        w.quitb.button: begin
          case event.value of
             0 : begin                                   ;'New File'
                   WIDGET_CONTROL, event.top, /DESTROY
		   wind
                 endcase

             1 : WIDGET_CONTROL, event.top, /DESTROY     ;'Quit'

	     2 : WIDGET_CONTROL, event.top, /ICONIFY     ;'Close'

	     3 : fhelp					 ;'Help'
          endcase
        endcase

        w.statisb.button: begin                       ;'Increment recn'
          case event.value of
             0 : recn_new=recn+1                             ;'+'

             1 : recn_new=recn-1                             ;'-'
          endcase
          if recn_new ge 1 and recn_new le fh.nmf then begin
            recn=recn_new
            widget_control,w.statisb.field(5),set_value=recn
            read_rec,date_time		;read record and display time from header
            widget_control,set_value=date_time,w.statisb.field(0)
            proc_rec			;process lz record
          endif 
        endcase

        w.functionb.button(0): begin
           case event.value of
             0: prt_flhdr,0,infile,fh		;print file header
             1: begin
                  ms_hms,lz.ms,h,m,s		;get hhmmss.ms from msec of day
                  prt_hdr,0,lz,h,m,s,infile		;print major frame header
                endcase
             2: prt_mf,0,lz.mf,lz.recn,infile		;print major frame data
             3: prt_ihk,0,ihk,lz,infile			;print instr hk data
             4: prt_hk,0,hkm1,lz.mf(scindx)		;print genl hk
             5: prt_fc,0,fcblm1,lz.mf(scindx),infile	;print faraday cup data
           endcase
	endcase

        w.functionb.button(1): begin
           case event.value of
             0: begin		;collect fc data... this is too slow...
		  recn = 1
		  widget_control,w.statisb.field(5),set_value=recn
		  read_rec,date_time
		  widget_control,set_value=date_time,w.statisb.field(0)
		  proc_rec
		  prt_fc,0,fcblm1,lz.mf(scindx),infile,/QUIET
		  for recn = 2,(fh.nphyrc-1) do begin
	            widget_control,w.statisb.field(5),set_value=recn
		    ;if not eof(infile) then begin
		      read_rec,date_time
		      widget_control,set_value=date_time,w.statisb.field(0)
		      proc_rec
		      prt_fc,0,fcblm1,lz.mf(scindx),/QUIET,/APPEND
		    ;endif
		  endfor 
		endcase
             1: begin
;		  print,'Unpickling...'
		  unp_fitz,'fc_data.prt'
;		  print,'Produced files caldata and moddata'
		end
             2: spawn,'avg_cal2 caldata ; avg_cal2 -l caldata > caldata.avg',/sh
             3: spawn,'avg_mod moddata > moddata.avg ; more moddata.avg',/sh
             4: calgraph
             5: begin
;		  print,'Generating key parameters...'
		  idl_shell2,infile
;		  print,'Done'
		end
           endcase
	endcase

	w.displayb.button: begin
	  case event.value of
	    0:  begin
		  WIDGET_CONTROL, w.textb.text, get_value=w.textb.old_buffer
		  WIDGET_CONTROL, w.textb.text, set_value=''
		endcase
	    1:  begin
		  WIDGET_CONTROL, w.textb.text, get_value=w.textb.buffer
		  WIDGET_CONTROL, w.textb.text, set_value=w.textb.old_buffer
		  w.textb.old_buffer=w.textb.buffer
		  ;is this next line needed???
		  WIDGET_CONTROL, w.textb.text, get_value=w.textb.buffer
		endcase
	    2:  begin
		  WIDGET_CONTROL, w.textb.text, get_value=w.textb.old_buffer
		  WIDGET_CONTROL, w.displayb.field, get_value=w.displayb.file_name
		  w.textb.buffer=readfile(w.displayb.file_name(0))
	    	  WIDGET_CONTROL, w.text, set_value=w.textb.buffer
		endcase
	    3:  begin
		  WIDGET_CONTROL, w.textb.text, get_value=w.textb.buffer
		  WIDGET_CONTROL, w.displayb.field, get_value=w.displayb.file_name
		  w.displayb.file_name=w.displayb.file_name(0)
		  WIDGET_CONTROL, w.textb.text, set_value=string('Saving file ',w.displayb.file_name)
		  IF (not writefile(w.displayb.file_name,w.textb.buffer)) then BEGIN
		    WIDGET_CONTROL, w.textb.text, set_value=['Error saving file: '+w.displayb.file_name]
		    wait,5
		  ENDIF ELSE BEGIN
		    WIDGET_CONTROL, w.textb.text, set_value=['File: ' + w.displayb.file_name + ' saved']
		  ENDELSE
		  WIDGET_CONTROL, w.textb.text,set_value=w.textb.buffer
		endcase
	    4:  begin
		endcase
	  endcase
	endcase

	w.displayb.field:  WIDGET_CONTROL, w.displayb.field, $
			     get_value=w.displayb.buffer_name
	  
        else:
                
ENDCASE
END


;=================== CALLING PROCEDURE: wind ================================

PRO wind, GROUP = GROUP

common sharewidg,w
;common sharewidg,w,quitb,statisb,functionb,textb,displayb,extrab
common stuff,infile,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
  sp,vsmjf

;set up structures
  sp={spinparams,spincnt:0b,tjd:0l,sec:0d,mjfcnt:0b,spinp:0d,$
      old_spincnt:0b,old_tjd:0l,old_sec:0d,old_mjfcnt:0b,$
      lst_spinp:0d,lst_tjd:0l,lst_sec:0d,newmjf:1,datayes:0,lst_scimod:-1}

;widget structure
  w={w_widgets,							$
	base:0l,						$			
			;base widget
	quitb:	    {q,base:0l,button:0l,field:0l},		$
			;first subbase, window options			
	statisb:    {s,base:0l,button:0l,field:lonarr(8)},	$
			;lz record info
	functionb:  {f,base:0l,button:lonarr(2)},		$
			;functions for lz analysis
	textb:	    {t,text:0l,buffer:'',old_buffer:''},	$	
			;text display widget
	displayb:   {d,base:0l,button:0l,field:'',file_name:'',	$
		       buffer_name:'',old_buffer_name:''},	$
			;options for controling text display
	extrab:	    {e,base:0l,button:0l,field:0l}		$
			;an array of extra bases to pop up	
    }

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
  if (infile eq '') then infile='./lz/10022302.dat'
;  openw,lun,'swelzdatapath',/get_lun
;  printf,lun,lzpath & free_lun,lun

;open level zero file
  openr,lundat,infile,/get_lun
;  print, ' ' & print,'input data file name',infile

;read file header (Jim Byrnes' procedure)
  fh=read_lzhdr(lundat) ;fh=file header structure 
 
;prt_flhdr,0,infile,fh  ;print file header
;;;;;;;;
;construct WIND window from widgets
  w.base=WIDGET_BASE(title='WIND', /column, /tlb_size_events) 		;define base widget

    w.quitb.base=WIDGET_BASE(w.base, /row, /frame,space=20,xpad=40)	;define window opts widget
      quit_list=['New File','Quit','Close','Help']			;define labels for quitb
      w.quitb.button=cw_bgroup(w.quitb.base, quit_list, row=1,space=20)	;make buttons with labels
      w.quitb.field=widget_label(w.quitb.base,value='File: '+infile, /frame) ;window statis label

    w.statisb.base=WIDGET_BASE(w.base, row=2, /frame, xpad=0, xoffset=0,space=0);define lz mjf params

      w.statisb.button=cw_bgroup(w.statisb.base,['+','-'],row=1,	$
	label_left='Increment recn',ids=ids)

      w.statisb.field(5)=cw_field(w.statisb.base,title='Record num',xsize=4,/return_events)
 
      w.statisb.field(0)=cw_field(w.statisb.base,title='Yr, day, hms ',	$
        value='',xsize=20,/string,/noedit)
   
      w.statisb.field(1)=cw_field(w.statisb.base,title='Timetag',	$
        value='',xsize=11,/string,/noedit)

      w.statisb.field(2)=cw_field(w.statisb.base,title='Spin count',	$
        value='',xsize=4,/string,/noedit)

      w.statisb.field(3)=cw_field(w.statisb.base,title='Spin period ',	$
        value='',xsize=6,/string,/noedit)

    ;widget interface to select and read lz records
    w.functionb.base=WIDGET_BASE(w.base,row=2,/frame)

      list=$
        [ 'file header',	  $ 
          'data record header',   $
          'lz data',		  $ 
          'instr hk data',	  $
          'genl hk data',	  $
          'fc data'		    ]
      w.functionb.button(0)=cw_bgroup(w.functionb.base,list,row=1,label_top='Level Zero Record Options')
 
      list=$
        [ 'collect fc data',$ 
          'reorder fc data',$
          'average caldata',$ 
          'average moddata',$
          'plot caldata',   $
          'k p analysis'      ]
      w.functionb.button(1)=cw_bgroup(w.functionb.base,list,row=1,label_top='Level Zero File Options')

    w.textb.text=widget_text(w.base,/scroll,ysize=30,xsize=81,value=w.textb.buffer);

    w.displayb.base=WIDGET_BASE(w.base,/row,/frame)

      list=$
        [ 'Clear window', $ 
	  'Restore text', $
          'Display file', $ 
          'Save buffer',  $
          'Edit buffer'     ]
      w.displayb.button=cw_bgroup(w.displayb.base,list,row=1,label_top='Display Options')
      w.displayb.field=cw_field(w.displayb.base,title='Buffer: ',$
				     value=w.displayb.buffer_name,/string,/return_events)

WIDGET_CONTROL, w.base, /REALIZE

;read and process first record in file
  recn=1
  widget_control,set_value=recn,w.statisb.field(5)
  read_rec,date_time		;read record and display time from header
  widget_control,set_value=date_time,w.statisb.field(0)
  proc_rec			;process lz record

XMANAGER, "wind", w.base, GROUP_LEADER = GROUP  ;hand off to manager

END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PRO display, filename, append=append	
;			;typically called with a string instead of variable
;on_error, 2		;so common and file=file(0) exist to make file=string
; if not KEYWORD_SET(append) then append = 0
; common sharef, file, file_lun 	;file is shared to preserve the filename memory
; common sharew, w
; file=filename(0)      	;it converts string arrays to strings
; input=readfile(file)
; WIDGET_CONTROL,w.text,set_value=input,append=append
;END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PRO fhelp_event, event
;on_error, 2
;
;common sharef, file, file_lun
;common sharel, nextrab, window, last_option, topic, topic_list, quit_list, anal_list
;common sharew, w
;;print,topic
;;help,event,/stru	;terribly useful for development purposes
;CASE event.id OF
;
;	w.base:  BEGIN
;	  ;print,' nextrab,event.y'
;	  ;print, nextrab, event.y
;	  ;print, 35+(event.y-(640+46*nextrab))/15
;	  IF (event.y gt (640+46*nextrab)) then BEGIN
;	    WIDGET_CONTROL,w.text,ysize=35+(event.y-(640+46*nextrab))/15
;	  ENDIF
;	ENDCASE
;	w.quitb.button:    BEGIN		;Quit, Close, or Help
;	  CASE event.value OF
;	    0:	WIDGET_CONTROL, event.top, /DESTROY
;	    1:  WIDGET_CONTROL, event.top, /ICONIFY
;	    2:  BEGIN
;		  IF window ne 97 then BEGIN
;		    WIDGET_CONTROL, w.text, set_value='Help Help.  We''re sorry.'
;		    WIDGET_CONTROL, w.text, /append, $
;			set_value='The Option you have just hit is imaginary.'
;		    WIDGET_CONTROL, w.text, /append, set_value=['Please rotate '+$
;					'your head by 90 degrees and try again.']
;		    window=97
;		  ENDIF ELSE BEGIN
;		    WIDGET_CONTROL, w.text, set_value=['Help Help Help.  '+$
;					'Thank you for flying fhelp helpways...']
;		    display,'~wind/help/General.fhelp',/append
;		    window=-1
;		  ENDELSE
;		END
;	    ELSE: BEGIN & END
;	  ENDCASE
;	ENDCASE
;
;	w.button(1):	BEGIN		;Help Options
;	  ;Begin Clean up of mess from last Help Option
;	  CASE last_option OF
;	   -1:
;	    1:  BEGIN
;		  WIDGET_CONTROL, w.extrab(1), /DESTROY
;		  nextrab=nextrab-1
;		END
;	    5:  ;WIDGET_CONTROL, w.extrab(5), /DESTROY
;	    6:  ;WIDGET_CONTROL, w.extrab(6), /DESTROY
;	    7:  ;WIDGET_CONTROL, w.extrab(7), /DESTROY
;	    else:  
;	  ENDCASE
;	  ;End Clean up of mess from last Help Option
;	  ;Announce new topic
;	  topic='Topic: '+topic_list(event.value)
;	  WIDGET_CONTROL, w.quitb.field, set_value=topic
;	  ;Execute Help option
;	  CASE event.value OF
;	    0:  display,'~wind/help/General.fhelp'
;	    1:	BEGIN
;		  IF ( not WIDGET_INFO(w.extrab(0),/VALID_ID) ) then BEGIN
;		    w.extrab(1)=WIDGET_BASE(w.base, /row, map=1,space=20)
;		    w.button(2)=cw_bgroup(w.extrab(1), anal_list, /row,$
;						 label_left='Key Param Analysis')
;		  nextrab=nextrab+1
;		  ENDIF
;		END
;	    2:  display,'~wind/help/SIM.outline'
;	    3:  display,'~wind/help/VTTOOL_LZ_RECOVERY'
;	    4:  display,'~wind/help/DMP.FHELP'
;	    5:  IF ( not WIDGET_INFO(w.extrab(5),/VALID_ID) ) then BEGIN
;		  w.extrab(5)=WIDGET_BASE(w.base, /row, space=0)
;		  w.field(5)=cw_field(w.extrab(5), /string, /return_events,$
;			xsize=30, value='', title='Enter file path/name:')
;		  w.extrab(8)=WIDGET_BASE(w.extrab(5),/row,/NONEXCLUSIVE)
;		  w.button(5)=widget_button(w.extrab(8),value='Read Only')
;		  w.button(8)=widget_button(w.extrab(5),value='Dismiss')
;		  nextrab=nextrab+1
;		ENDIF
;	    6:  BEGIN
;		END
;	    7:  IF ( not WIDGET_INFO(w.extrab(7),/VALID_ID) ) then BEGIN
;		  w.extrab(7)=WIDGET_BASE(w.base, /row, space=20)
;		  w.field(1)=cw_field(w.extrab(7), /string, /return_events,$
;			xsize=30, value='', title='Enter command:       ')
;		  w.button(7)=widget_button(w.extrab(7),value='Dismiss')
;		  WIDGET_CONTROL,w.field(1),/input_focus
;		  nextrab=nextrab+1
;		ENDIF
;	  ENDCASE
;	  last_option = event.value
;	ENDCASE
;
;	w.button(2):    BEGIN		;Key Param Buttons
;	  CASE event.value OF
;	    0:	display,'/plasma/h1/wind/source/analysis/key_p/README'
;	    1:  display,'/plasma/h1/wind/source/analysis/key_p/Makefile'
;
;	    2:  BEGIN			;Run testkp (fortran shell version)
;  		  widget_control,w.text,set_value='Running testkp'
;		  widget_control,w.text,set_value='Please wait...',/APPEND
;		  spawn,['/plasma/h1/wind/source/analysis/key_p/testkp'+$
;					' >& /tmp/fhelp_kp']
;		  display,'/tmp/fhelp_kp'
;		END
;	    3:  display,'~fvm/GUIDE'
;	  ENDCASE
;	ENDCASE
;	
;	w.button(5):	BEGIN		;Read Only/Save File toggle button
;	  CASE event.select OF		;This is frank's little file editor...
;	    0:	BEGIN			;0 = you released the button
;		  WIDGET_CONTROL,w.button(5),get_value=val
;		  IF (val(0) eq 'Save File??') then dismiss=1 else dismiss=0
;		  WIDGET_CONTROL,w.button(5),set_value='Read Only'
;		  WIDGET_CONTROL,w.field(5),get_value=savefile
;		  WIDGET_CONTROL,w.field(5),set_value=string('Saving file ',savefile(0))
;		  WIDGET_CONTROL,w.text,editable=0
;		  WIDGET_CONTROL,w.text,get_value=output
;		  IF (not writefile(savefile(0),output)) then 	BEGIN
;		    WIDGET_CONTROL,w.field(5),$
;		                   set_value=['Error saving file: '+savefile(0)]
;		    wait,5
;		  ENDIF
;		  file=savefile(0)
;		  WIDGET_CONTROL,w.field(5),set_value=file
;		  IF (dismiss) then BEGIN
;		    WIDGET_CONTROL,w.extrab(5),/DESTROY
;		    dismiss=0
;		    nextrab=nextrab-1
;		    last_option = -1
;		  ENDIF
;		  WIDGET_CONTROL,w.button(1),/sensitive
;		END
;	    1:  BEGIN			;1=you pushed the button down
;		  WIDGET_CONTROL,w.button(1),sensitive=0
;		  WIDGET_CONTROL,w.field(5),get_value=file
;		  WIDGET_CONTROL,w.button(5),set_value='Save File'
;		  WIDGET_CONTROL,w.text,/editable
;		  savefile=string(file(0),'.fhelp')
;		  WIDGET_CONTROL,w.field(5),set_value=savefile(0)
;		END
;	  ENDCASE
;	ENDCASE
;
;	w.button(8):	BEGIN	;Dismiss button: gets rid of extra widget base
;	  WIDGET_CONTROL,w.text,editable=0
;	  WIDGET_CONTROL,w.button(5),get_value=mode
;	  mode = mode(0)
;	  CASE mode OF
;	    'Read Only':BEGIN	;normal Dismiss
;	      WIDGET_CONTROL,w.button(1),/sensitive
;	      WIDGET_CONTROL,w.extrab(5),/DESTROY
;	      nextrab=nextrab-1
;	    ENDCASE
;	    'Save File':BEGIN	;Dismiss if file still editable
;	      WIDGET_CONTROL,w.button(5),set_value='Save File??'
;	      WIDGET_CONTROL,w.button(5),set_value='Ignore Edit'
;	    ENDCASE
;	    'Save File??': BEGIN	;Dismiss but don't save edits
;	      display,file
;	      WIDGET_CONTROL,w.button(1),/sensitive
;	      WIDGET_CONTROL,w.extrab(5),/DESTROY
;	      nextrab=nextrab-1
;	    ENDCASE
;	  ENDCASE
;	  last_option = -1	      
;	ENDCASE
;	w.button(7):	BEGIN	;Dismiss button: gets rid of extra widget base
;	  WIDGET_CONTROL,w.extrab(7),/DESTROY
;	  nextrab=nextrab-1
;	  last_option = -1
;	ENDCASE
;
;	w.field(1):	BEGIN		;gets command and passes it to a C shell
;	  WIDGET_CONTROL, w.field(1), get_value=val
;	  command=val(0)
;	  WIDGET_CONTROL, w.text, set_value=string(command,':')
;	  command = string('(',command,') |& cat')
;	  ;print,command
;	  spawn,command,result
;	  ;print,result
;	  WIDGET_CONTROL, w.text, set_value=result,/APPEND
;	  WIDGET_CONTROL, w.field(1), set_value=''
;	ENDCASE
;
;	w.field(5):	BEGIN		;gets filename and displays file in w.text
;	  WIDGET_CONTROL, w.button(5), get_value=mode
;	  mode = mode(0)
;	  WIDGET_CONTROL, w.field(5), get_value=val
;	  print,mode,' ',val(0)
;	  IF (mode ne 'Save File') then BEGIN
;	    input=readfile(val(0))
;	    WIDGET_CONTROL, w.text, set_value=input
;	  ENDIF ELSE savefile=val(0)
;	ENDCASE
;
;	ELSE:	BEGIN & END
;ENDCASE
;END
;;		  input='You have entered a unix shell.'
;;		  WIDGET_CONTROL,w.text,set_value=input
;;		  input='Type "exit" to return to program'
;;		  WIDGET_CONTROL,w.text,set_value=input,/APPEND
;;		  spawn 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PRO fhelp, GROUP = GROUP
;on_error,2
;
;common sharef, file, file_lun
;common sharel, nextrab, window, last_option, topic, topic_list, quit_list, anal_list
;common sharew, w
;
;nextrab=0				;how many extra bases are open
;window = -1			;top row buttons
;last_option  = -1		;last_option stores index to last extrab used
;topic = 'Select a topic'		;initial topic string
;file  = ''				;declare file as a null string
;;file = '~wind/help/Welcome'		;startup welcome file
;					;set up widget structure
;  w={w_widgets,base:0l,$			;base widget
;	       quitb:0l,topicb:0l,text:0l,$	;3 major components of base
;	       extrab:lonarr(10),$		;an array of extra bases to pop up
;	       button:lonarr(10),$		;you guessed it: buttons
;	       field:lonarr(10)}		;display and input fields
;
;  w.topicb=WIDGET_BASE(w.base,/row,/frame)
;  topic_list=$
;  [ 'General Help',   $
;    'Key Param Anal', $
;    'FC Data Sim',    $
;    'lz  Files',      $
;    '.dmp Files',     $
;    'Display File',   $
;    'Edit Text',      $
;    'fmcsh'	      ]
;  w.button(1)=cw_bgroup(w.topicb, topic_list, row=2,$
;		        label_top='Help Options', /exclusive)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;this is where we keep the unmapped WIDGET_BASEs
;;they can vary any way and be mapped and unmapped at will (i hope)
;;the first one will be the analysis widget heirarchy
;;  w.extrab(0)=WIDGET_BASE(w.base, /row, /frame, map=0, title='ANALYSIS')
;  anal_list=$
;  [ 'README',$
;    'Making testkp',$
;    'Run testkp',   $
;    'Extra notes'   ]
;;  w.button(2)=cw_bgroup(w.extrab(0), anal_list,/row, label_top='testkp analysis')
;;  i decided to realize extra bases later
;;  as soon as a child is declared in an existing base, the child is realized
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ;input=readfile(file(0))	;this is for the startup text
;  w.text=widget_text(w.base,/scroll,ysize=35,xsize=81,$
;    value='                              Welcome to HELP!')
;
;WIDGET_CONTROL, w.base, /REALIZE
;XMANAGER, "fhelp", w.base, GROUP_LEADER = GROUP  ;hand off to manager 
;END
