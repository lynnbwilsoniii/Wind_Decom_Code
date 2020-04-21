; %Z%%M%  VERSION %I%    %G%   %U%
;This is a beta-version WIND3 analysis program by Frank V. Marcoline.
;It will contain lz file analysis procedures and a complete WIND3 help menu.
;Much of the lz code was supplied by Dick Fitzenreiter.

; This is a set of procedures to read and process SWE level zero data.
; Jim Byrnes provided the read procedures.
; A simple widget interface is used to select (direct access) data records and
; display a limited amount of housekeeping information.
; The calling procedure is WIND3

; R. J. Fitzenreiter, Dec 1993
; Modified April, 1994 (RJF)
; Modified July, 1994 (FVM)  
	;straightened display SWE LZ Data (padded cw_field titles w/spaces)
	;changed time format to include zeros ie:  15:05:09.035
	;edited time_utc.pro time format as well
	;changed prt_*.pro files to use output filenames based on output type 
	;edited prt_*.pro to stamp source_file_name at top of outputfile
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

; Modified May 10, 1995 (FVM) 
        ;Added idl_shell2.pro (half to wind3 and half to wind3_event 
; From: @(#)idl_shell2.pro  VERSION 1.9 95/05/10 23:20:24
; calling sequence: 
; IDL> wind3,'filename',start=hhmm,end=hhmm,kb=kb,test=test,$
;            shout=shout,fvm=fvm,fss=fss,archive=archive,pick=pick
;
; all keywords are optional and function independently or in any combination,
; but you must specify the filename
; ie:	      idl_shell2,'mylzfile',start=345,end=1523,/kb
;     	      this means go to time 3:45am, enter interactive mode (KeyBoard mode)
;     	      and quit when you hit 3:23pm.
; or:	      idl_shell2,'mylzfile',start=345,kb=0,test=1
;	      same start time, finish at end of day, non-keyboard mode, test mode
; kb mode:    interactive analysis mode.  allows you to step or skip through
;	      the lzfile, and plot the currents and fits...
;             See the help listing below for commands (in PRO printhelp).
; test mode:  This mode is useful for times when you do not want to do a complete
;	      analysis, and you do not want to overwrite any important files.
;	      All files will be written to dummy file names, (and consequently
;	      overwritten next test).  The shell defaults to dummy file names
;	      and passes a flag to the core via a logical unit number, which tells
;	      the core to use alternate output files.
; shout mode: SHell OUT mode: Shell will write param and operation stat files.
; fvm mode:   This is my mode. Run using my core, kp_core_ss.so
; fss mode:   An additional testing mode just for my defaults. This is
;             the only mode that doesn't require an input file name.
; archive:    Archive=0, nothing special, archive.ne.0: Decompress archived 
;             input file, run analysis, recompress when done.             
;             Please specify the full file name including path and extension.
; comments:   There may be reason to comment out large portions of the code	
;	      at a time. For purpose of easy recovery, i will use ;#; to mark a 
;	      series of statements commented out together, incrementing from ;0;
;	      Please list the purpose of the comments here:
; ideas:      I would like the shell to append to one file, all of the errors
;	      it incountered in analyzing the data files, when not in test mode.
;	      It might be handy to know where the problems are...
;============================ begin writefile =======================
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
;============================= end writefile ========================
;============================ begin  readfile =======================
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
;============================= end readfile =========================
;=========================== begin decode_utc =======================
FUNCTION decode_utc,bytes

;  print,bytes
  utc = long(bytes(2))
  utc = utc + ishft(long(bytes(3)),8)
  utc = utc + ishft(long(bytes(4)),16)
  utc = utc + ishft(long(bytes(5)),24)
  
  ms = long(bytes(6))
  ms = ms + ishft(long(bytes(7)),8)
  ms = 2L^16 - 1 - ms
  if ms gt 999 then ms = 0
  
  isec = utc and '1ffff'XL
  hour = isec / 3600
  min  = (isec - long(hour*3600))/60
  isec = isec - hour*3600 - min*60
  tjd  = ishft(utc,-17)
;  print,long(utc),long(tjd)
  time = string(tjd,hour,min,isec,$
        format='("day",i5,i3.2,":",i2.2,":",i2.2)')
  return, time
end
;========================== end  decode_utc =========================
;========================== begin  count_rn =========================
FUNCTION count_rn,c_file,prt=prt,exact=exact
;get number of exact number of records 
IF NOT keyword_set(prt) then prt=0
if (prt) then $                        
  print,'Counting records in file: ',c_file ;say hello
IF keyword_set(exact) THEN BEGIN
    if not keyword_set(prt) then prt=0
    openr,c_unit,c_file,/get_lun ;open the file
    rec=bytarr(11552)           ;define file record
    readu,c_unit,rec            ;read in the header
    rn=0                        ;file header = record 0
    
    REPEAT BEGIN                ;repeat
        rn=rn+1                 ;increment record counter
        readu,c_unit,rec        ;read in the rn-th record
    ENDREP UNTIL (eof(c_unit))  ;until the end of the file
    
    free_lun,c_unit             ;close the file
ENDIF ELSE BEGIN
    spawn,'ls -s '+c_file,ls_ans,/sh
    reads,ls_ans,kbytes             
    rn=fix(kbytes*1024./11552.)-4
ENDELSE
if (prt) then print,strtrim(rn,1),' records.' ;say goodbye
return,rn                                      ;return the number of records
end
;=========================== end count_rn ===========================
;========================= begin reset_core =========================
PRO reset_core
common core,core_so,object,                              $
            fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
            iqual,spec_tim,del_tim,anal_complete,reset,  $
            iunit_brds,iunit_mode,iunit_modlvls
fcblock(0)='ff'XB
fcblock(1)='00'XB
reset=1L
result=call_external(core_so,object,                              $
                     fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
                     iqual,spec_tim,del_tim,anal_complete,reset,  $
                     iunit_brds,iunit_mode,iunit_modlvls)
reset=0L
end
;=========================== end reset_core =========================
;=========================== begin gototime =========================
PRO gototime,infile,lundat,rn,rln,spcid,start_time
;hhmm is a variable, and a symbol denoting the military time format
;here is the plan: I'm going to work in MINUTES, so i have to convert
;military time to minutes:  minutes= hhmm/100*60+hhmm-(hhmm/100*100)
;                           minutes=(hh*60)+mm
;see how i use integer multiplication to do tens shifts? (a/100*100 is not eq a)
;start_time is where we want to go (in military time)
;hhmm is where we are (in military time)
;rn is the current record number
;tot_rn is the total number of records in infile
;rpm is the approx number of records per minute

start_time = start_time mod 2400                     ;make it reasonable
start_min  = start_time/100*60+start_time-(start_time/100*100)  ;get minutes
lz        = read_lzrcd(lundat,1,rln,spcid)          ;read first record
begin_min  = lz.ms/60000                            ;get beginning min into day
tot_rn     = count_rn(infile)                        ;get total number of records
lz        = read_lzrcd(lundat,tot_rn-5,rln,spcid)   ;get near last record 
end_min    = lz.ms/60000                            ;get final min into day
rpm        = float(tot_rn/float(end_min-begin_min))  ;get records per minute
rn         = fix(start_min*rpm)                      ;guess approx rn to go to
lz        = read_lzrcd(lundat,rn,rln,spcid)         ;read new record
min        = lz.ms/60000                            ;get current minutes into day

WHILE (min lt start_min and not eof(lundat)) DO BEGIN
    rn  = rn+1                                       ;increment record number
    lz = read_lzrcd(lundat,rn,rln,spcid)            ;read new record
    min = lz.ms/60000                               ;get current minutes into day
ENDWHILE
rn=rn-1
;reset_core   ;just incase it had some stuff stored in its arrays already
END
;============================ end gototime ===========================
;=========================== begin gotofract =========================
PRO gotofract,infile,lundat,rn,rln,spcid,fract=fract
IF NOT keyword_set(fract) THEN BEGIN
    print,'Enter decimal fraction of day to skip to: ',format='(a,$)'
    read,'',fract
ENDIF
fract=abs(fract mod 1)
if fract lt 0.001 then fract = 0.001
gototime,infile,lundat,rn,rln,spcid,fract*2400
END
;=========================== end gotofract ===========================
;========================== begin printhelp ==========================
PRO printhelp
helparr=['Command        Action',$
         '   ?      Print Command Summary',                    $
         ' + = >    Increment 1 record',                       $
         ' - _ <    Decrement 1 record',                       $
         '   1      splot1f',                                  $
         '   2      splot2f',                                  $
         '   3      splot3f',                                  $
         '   4      splot4f',                                  $
         '   5      splot5f',                                  $
         '   6      splot6f',                                  $
         '   7      splot7f',                                  $
         '   8      splot8f',                                  $
         '   9      splot9f',                                  $
         '   c      Plot Currents & Fit',                      $
         '   e      Plot & Print: SS Params vs Energy Window', $
         '   f      Go to fraction of day',                    $
         '   h      hplot',                                    $
         '   H      hardplot',                                 $
         '   l      log output',                               $
         '          (cp output_task[_ss].dat output_task[_ss]_mjfm_tim.rn#.dat)',$
         '   p      splot',                                    $
         '   P      startplot',                                $
         '  q Q     Quit Analysis Program',                    $
         '  s(#)    Skip +/- # Records',                       $
         '   S      IDL "stop," .cont resumes',                $
         '   t      print mjfm_tim',                           $
         '   x      Run analysis until user interupt',         $
         '   z      ssplotf (plot of output_task_ss.dat)',     $
         '   Z      portrait print of ssplotf'                 ]

print,helparr,format='(/(a))'
end
;        '   0      splot10f',                            $
;        '   !      splot11f',                            $
;============================= end printhelp ========================
;========================== begin mode_and_time =====================
PRO mode_and_time,lz,scindx,ihk,hkm1,sp,mode,spin_per,mjfm_tim
;; determine tm mode, tm rate, science mode, and mjf count from instr hk
tmmode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
tmrate_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
scimode_ihk=get_bits(lz.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

time_utc,lz.mf(scindx(hkm1(1).offs:hkm1(1).offs+6)),$
  tjd,sec,hour,min,isec,ms,hms,spincnt
sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt 

;;get spin period
sp.mjfcnt=lz.mf(ihk(1).offs)

spinperiod,sp  

;print,'time tag:  tjd      secdy  hh:mm:ss.ms  spincnt  spinperiod'
;print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',ms,sp.spincnt,sp.spinp,$
;format='(9x,i5,2x,f9.3,2x,i2,a1,i2,a1,i2,a1,i3,2x,i3,f12.3)'
;print,tmmode_ihk,tmrate_ihk,scimode_ihk,sp.mjfcnt,format='(3i5,z3.2)'
;;SET CORE VARIABLES
mode=long(scimode_ihk)
spin_per=float(sp.spinp)
mjfm_tim=(lz.yr*1000d0)+lz.dy+(lz.ms/8.640d+07)
END
;========================= end mode_and_time =========================
;========================= begin get_fcblock =========================
PRO get_fcblock,fcblock,j,lz,fcblm1,scindx
;fcblock is the returned fcblock, j is the block index within a record
;fcblm1 is structure containing info on the mjf contents

fcoff=scindx(fcblm1(j).offs+indgen(fcblm1(j).ln))

FOR k=0,fcblm1(j).ln-1 do BEGIN
    offset=scindx(fcblm1(j).offs+k)
    fcblock(k)=lz.mf(offset)
ENDFOR
END
;============================ end get_fcblock ========================
;============================== begin mode1 ==========================

pro mode1 

common sharewidg,w3
;common sharewidg,w3,quitb,statisb,functionb,textb,displayb,extrab
common stuff,infile,outfile,dsr,files,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
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
  WIDGET_CONTROL,set_value=hms,w3.statisb.field(1)
  WIDGET_CONTROL,set_value=string(sp.spincnt,format='(i3)'),w3.statisb.field(2)
  
;get spin period
  sp.mjfcnt=lz.mf(ihk(1).offs)
  spinperiod,sp  
  WIDGET_CONTROL,set_value=string(sp.spinp,format='(f7.5)'),w3.statisb.field(3)

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

common stuff,infile,outfile,dsr,files,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf

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

common sharewidg,w3
;common sharewidg,w3,quitb,statisb,functionb,textb,displayb,extrab
common stuff,infile,outfile,dsr,files,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
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

common sharewidg,w3
;common sharewidg,w3,quitb,statisb,functionb,textb,displayb,extrab
common stuff,infile,outfile,dsr,files,lundat,recn,fh,lz,scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,$
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

common stuff,infile,outfile,dsr,files,lundat,recn,fh,lz, $
  scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf

;read selected lz mjf record num recn (Jim Byrnes' procedure)
  lz=read_lzrcd(lundat,recn,fh.rln,fh.spcid) ;lz=data structure incl header

  ms_hms,lz.ms,h,m,s  ;get hhmmss.ms from msec of day
  date_time=string(lz.yr,format='(i4)') + ', ' + string(lz.dy,format='(i3)') +$
    ', ' + string(h,format='(i2)') + ':' + string(m,format='(i2.2)') +$
    ':' + string(s,format='(f6.3)')

  
  end

;============================= create_widget_structure =======================
PRO create_widget_structure,infile,outfile,dsr
common sharewidg,w3
;construct WIND3 window from widgets
  name='WIND3'
  w3.base=WIDGET_BASE(title=name, resource_name=name, /column, /tlb_size_events) 	
                                ;define base widget
  w3.quitb.base=WIDGET_BASE(w3.base, /row, /frame,space=20,xpad=40,resource_name='quit')	
                                ;define window opts widget
  quit_list=['New File','Quit','Close','Help']
                                ;define labels for quitb
  w3.quitb.button=cw_bgroup(w3.quitb.base, quit_list, row=1,space=20)
                                ;make buttons with labels
  w3.quitb.field=cw_field(w3.quitb.base,title='File:',value=infile,xsize=40)
                                ;level zero file name
                                ;widget interface to select and read lz records
  w3.functionb.base=WIDGET_BASE(w3.base,row=2,/frame,resource_name='function')  
  list=$
    [ 'file header',	   $ 
      'data record header',$
      'lz data',	   $ 
      'instr hk data',	   $
      'genl hk data',	   $
      'fc data'		   ]
  w3.functionb.button(0)=cw_bgroup(w3.functionb.base,list,row=1,$
                                   label_top='Level Zero Record Options')
  list=$
    [ 'collect fc data',$ 
      'reorder fc data',$
      'average caldata',$ 
      'average moddata',$
      'plot caldata',   $
      'k p analysis'    ]
  w3.functionb.button(1)=cw_bgroup(w3.functionb.base,list,row=1,$
                                   label_top='Level Zero File Options')
  w3.statisb.base=WIDGET_BASE(w3.base,row=2,/frame,xpad=20,resource_name='statis')
                                ;define lz mjf params
  w3.statisb.button=cw_bgroup(w3.statisb.base,['+','-'],row=1,$
                              label_left='Increment recn',ids=statids)
  w3.statisb.field(5)=cw_field(w3.statisb.base,title='Record num',$
                               xsize=4,/return_events)
  w3.statisb.field(0)=cw_field(w3.statisb.base,title='Yr, day, hms ',$
                               value='',xsize=20,/string,/noedit)
  w3.statisb.field(1)=cw_field(w3.statisb.base,title='Timetag',$
                               value='',xsize=11,/string,/noedit)
  w3.statisb.field(2)=cw_field(w3.statisb.base,title='Spin count',$
                               value='',xsize=4,/string,/noedit)
  w3.statisb.field(3)=cw_field(w3.statisb.base,title='Spin period  ',$
                               value='',xsize=8,/string,/noedit)
  w3.analysisb.base=WIDGET_BASE(w3.base, /frame, /row,resource_name='analysis')
  w3.analysisb.button(0)=cw_bgroup(w3.analysisb.base,['+','-'],row=1,$
                                   label_top='Step',ids=analids)
  w3.analysisb.field(0)=cw_field(w3.analysisb.base,title='Skip',$
                                 value=20,xsize=4,/integer,/return_events,/col)
  w3.analysisb.button(1)=cw_bgroup(w3.analysisb.base,['Run ','Quit'],$
                                   label_top='Continuous Mode',/exclusive,/row)
  w3.plotb.base(0)=WIDGET_BASE(w3.base, /frame, row=2,resource_name='')
  w3.plotb.base(1)=widget_base(w3.plotb.base(0),/row)
  outfile='output_task.dat' & dsr=''
;  w3.plotb.field(0)=cw_field(w3.plotb.base(1),title='Output_task File:',$
;                             value=outfile)
  w3.plotb.field(1)=widget_label(w3.plotb.base(1),value='Output_task File:')
  w3.plotb.field(0)=widget_label(w3.plotb.base(1),value=outfile,/frame)
  w3.plotb.button(4)=widget_button(w3.plotb.base(1),value='Find File')
  w3.plotb.base(2)=widget_base(w3.plotb.base(0),/row)
;  spawn,'ls ~wind/source/analysis/sskp/output/output_task{_1*,}.dat',files
;  files2=files
;  for i=0,n_elements(files)-1 do begin
;      pos=strpos(files(i),'output_task')
;      files2(i)=strmid(files(i),pos,100)
;      files2(i)='"'+files2(i)+'"   '+string(i+2000)
;  endfor
;  xpdmenu,['"Files" {',files2,'}'],w3.plotb.base(2),column=20
;  print,'w3.plotb.base(2)',w3.plotb.base(2)
  numbers=['1','2','3','4','5','6','7','8']
  w3.plotb.button(0)=cw_bgroup(w3.plotb.base(2),numbers,row=2,$
                               label_top='Splot#f',ids=plotids)
  w3.plotb.button(1)=cw_bgroup(w3.plotb.base(2),'',$
                               label_top='Fitplot')
  w3.plotb.button(2)=cw_bgroup(w3.plotb.base(2),['1','2','fit'],row=1,$
                               label_top='SSplot#f',ids=plotids2)
  w3.plotb.button(3)=cw_bgroup(w3.plotb.base(2),'',$
                               label_top='Log output_task.dat')
  w3.plotb.base(3)=widget_base(w3.plotb.base(2),row=2)
  plot_opts=['splot','view','hplot','lpq']
  w3.plotb.button(5)=cw_bgroup(w3.plotb.base(3),plot_opts,col=4);,/exclusive)
  w3.plotb.button(9)=cw_bgroup(w3.plotb.base(3),['Lanscape','Portrait'],$
                               set_value=0,exclusive=1,col=2)

;    w3.textb.text=widget_text(w3.base,/scroll,ysize=30,xsize=81,value=w3.textb.buffer);
;
;    w3.displayb.base=WIDGET_BASE(w3.base,/row,/frame)
;
;      list=$
;        [ 'Clear window', $ 
;	  'Restore text', $
;          'Display file', $ 
;          'Save buffer',  $
;          'Edit buffer'     ]
;      w3.displayb.button=cw_bgroup(w3.displayb.base,list,row=1,label_top='Display Options')
;      w3.displayb.field=cw_field(w3.displayb.base,title='Buffer: ',$
;				     value=w3.displayb.buffer_name,/string,/return_events)

END
;============================= wind3_event ===================================

PRO wind3_event, event

common sharewidg,w3
common stuff,infile,outfile,dsr,files,lundat,recn,fh,lz, $
  scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf
common core,core_so,object,                              $
            fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
            iqual,spec_tim,del_tim,anal_complete,reset,  $
            iunit_brds,iunit_mode,iunit_modlvls
common shell,busy,callcntr,callcntr_total,file,get_out,  $
            max_nbytes,prt,prt1,request,result,rn,skip,  $
            step,timetag,numfcblocks
;help,event,/str
;help,w3,w3.plotb,/str
;print,w3.plotb.button
;print,w3.extrab.base

CASE event.id OF

        w3.quitb.button: begin
          case event.value of
             0 : begin                                   ;'New File'
                   WIDGET_CONTROL, event.top, /DESTROY
		   wind3
                 endcase
             1 : WIDGET_CONTROL, event.top, /DESTROY     ;'Quit'
	     2 : WIDGET_CONTROL, event.top, /ICONIFY     ;'Close'
	     3 : fhelp					 ;'Help'
          endcase
        endcase

        w3.functionb.button(0): begin
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

        w3.functionb.button(1): begin
           case event.value of
             0: begin		;collect fc data... this is too slow...
		  recn = 1
		  WIDGET_CONTROL,w3.statisb.field(5),set_value=recn
		  read_rec,date_time
		  WIDGET_CONTROL,set_value=date_time,w3.statisb.field(0)
		  proc_rec
		  prt_fc,0,fcblm1,lz.mf(scindx),infile,/QUIET
		  for recn = 2,(fh.nphyrc-1) do begin
	            WIDGET_CONTROL,w3.statisb.field(5),set_value=recn
		    ;if not eof(infile) then begin
		      read_rec,date_time
		      WIDGET_CONTROL,set_value=date_time,w3.statisb.field(0)
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

	w3.statisb.field(5): BEGIN  ;select mjf record
            recn_new=event.value(0)
            if recn_new lt fh.nmf then begin
                recn=recn_new
                ;;PRINT, 'mjf data recn selected = ' + STRING(recn)
                ;;print,'physical record = ',fh.nphyrc - fh.nmf + recn
                read_rec,date_time ;read record and display time from header
                WIDGET_CONTROL,set_value=date_time,w3.statisb.field(0)
                proc_rec        ;process lz record 
            endif
        endcase

        w3.statisb.button: begin ;'Increment recn'
            case event.value of
                0 : recn_new=recn+1 ;'+'
                1 : recn_new=recn-1 ;'-'
            endcase
            if recn_new ge 1 and recn_new le fh.nmf then begin
                recn=recn_new
                WIDGET_CONTROL,w3.statisb.field(5),set_value=recn
                read_rec,date_time ;read record and display time from header
                WIDGET_CONTROL,set_value=date_time,w3.statisb.field(0)
                proc_rec        ;process lz record
            endif 
        endcase

        w3.analysisb.button(0): BEGIN
            case event.value of
                0 : BEGIN 
                    recn_new=recn+1 ;'+'
                    step=1          ;analyze one record
                ENDCASE
                1 : recn_new=recn-1 ;'-'
            endcase
            if recn_new ge 1 and recn_new le fh.nmf then begin
                recn=recn_new
                WIDGET_CONTROL,w3.statisb.field(5),set_value=recn
                read_rec,date_time ;read record and display time from header
                WIDGET_CONTROL,set_value=date_time,w3.statisb.field(0)
                proc_rec        ;process lz record
            endif 
        endcase

        w3.analysisb.field(0): BEGIN
            skip=event.value
            recn_new=recn+skip
            if recn_new le 0 then recn_new = 0
            if recn_new le fh.nmf then begin
                recn=recn_new
                WIDGET_CONTROL,w3.statisb.field(5),set_value=recn
                read_rec,date_time ;read record and display time from header
                WIDGET_CONTROL,set_value=date_time,w3.statisb.field(0)
                proc_rec        ;process lz record
            endif 
            
        endcase

        w3.analysisb.button(1): BEGIN
            case event.value of
                0: BEGIN
                    print,'Entering continuous run mode. '+$
                      'Click Quit to exit Continuous Mode.'
                    step=2
                endcase
                1: BEGIN
                    print,'Exiting Continuous Mode.'
                    step=0
                endcase
            endcase
        endcase

        w3.plotb.button(0): BEGIN
            WIDGET_CONTROL,w3.plotb.button(9),set_value=0
            suffix='_ss' 
            pos=strpos(outfile,suffix)
            if (pos ne -1) then strput,outfile,'   ',pos
            new_outfile=strcompress(outfile,/remove_all)
            print,strcompress(string('splot',event.value,'f'),/remove_all)
            case event.value of
                0:  splot1f,file=new_outfile
                1:  splot2f,file=new_outfile
                2:  splot3f,file=new_outfile
                3:  splot4f,file=new_outfile
                4:  splot5f,file=new_outfile
                5:  splot6f,file=new_outfile
                6:  splot7f,file=new_outfile
                7:  splot8f,file=new_outfile
                else:
            endcase
        endcase
            
        w3.plotb.button(1): begin
            WIDGET_CONTROL,w3.plotb.button(9),set_value=0
            suffix='_ss' 
            pos=strpos(outfile,suffix)
            if (pos ne -1) then strput,outfile,'   ',pos
            new_outfile=strcompress(outfile,/remove_all)
            fitplot
        endcase
        
        w3.plotb.button(2): begin
            WIDGET_CONTROL,w3.plotb.button(9),set_value=1
            root='output_task'
            outpos=strpos(outfile,root)
            dir=strmid(outfile,0,outpos)
            case event.value of
                0: ssplotf, dir=dir,dsr=dsr,num=6
                1: ssplot2f,dir=dir,dsr=dsr
                2: ssfit,dir=dir,dsr=dsr
            endcase
        endcase

        w3.plotb.button(3): begin
            time_stamp=strtrim(string(mjfm_tim,format='(d12.3)'),1)
            rn_stamp='_rn'+strtrim(recn,1)
            cmd=string('cp output_task.dat output_task_',$
                       time_stamp,rn_stamp,'.dat')
            spawn,cmd,/sh & print,cmd
            cmd=string('cp output_task_ss.dat output_task_ss_',$
                       time_stamp,rn_stamp,'.dat')
            spawn,cmd,/sh & print,cmd
        endcase

        w3.plotb.button(4): BEGIN
            root = 'output_task'
            ext  = '.dat'
            dir  = '/plasma/h1/wind/source/analysis/sskp/output/'
            outfile=pickfile(/read,path=dir,filter=root+'*'+ext,$
                            title='Single Spin Data Files')
            print,'Output_task file: ',outfile
            suffix='_ss'
            pos=strpos(outfile,suffix)              ;check if it is an '_ss' file
            if (pos ne -1) then root=root+suffix    ;fix the root name
            pos=strpos(outfile,root)                ;get new root name position
            endpos=strlen(root)+pos                 ;get end of root name pos
            if (pos eq -1) or (outfile eq '') then begin       ;did it work?
                print,'Strange output_task[string].dat filename...',$
                  'Program may be too stupid to comply...'
                dsr=''
            endif $
            else dsr=strmid(outfile,endpos,strlen(outfile)-endpos-strlen(ext))
            WIDGET_CONTROL,w3.plotb.field(0),set_value=outfile
        endcase

        w3.plotb.button(5): BEGIN

            case event.value of
                0: BEGIN
                    if !d.name eq 'PS' then set_plot,'X'
                    ;;that was cruel... I just overrode adam's protection...
                    WIDGET_CONTROL,w3.plotb.button(9),get_value=orient
                    splot,por=orient
                    print,'Select plot'
                end
                1: spawn,'ghostview -swap ~wind/idlpic.ps &'
                2: BEGIN
                    hplot
                    print,'Sending to printer...'
                END
                3: lpq
            endcase
        endcase
        
;        w3.plotb.button(9): BEGIN
;            case event.value of
;                0: 
;                1:
;            endcase
;        endcase

;	w3.displayb.button: begin
;	  case event.value of
;	    0:  begin
;		  WIDGET_CONTROL, w3.textb.text, get_value=w3.textb.old_buffer
;		  WIDGET_CONTROL, w3.textb.text, set_value=''
;		endcase
;	    1:  begin
;		  WIDGET_CONTROL, w3.textb.text, get_value=w3.textb.buffer
;		  WIDGET_CONTROL, w3.textb.text, set_value=w3.textb.old_buffer
;		  w3.textb.old_buffer=w3.textb.buffer
;		  ;is this next line needed???
;		  WIDGET_CONTROL, w3.textb.text, get_value=w3.textb.buffer
;		endcase
;	    2:  begin
;		  WIDGET_CONTROL, w3.textb.text, get_value=w3.textb.old_buffer
;		  WIDGET_CONTROL, w3.displayb.field, get_value=w3.displayb.file_name
;		  w3.textb.buffer=readfile(w3.displayb.file_name(0))
;	    	  WIDGET_CONTROL, w3.text, set_value=w3.textb.buffer
;		endcase
;	    3:  begin
;		  WIDGET_CONTROL, w3.textb.text, get_value=w3.textb.buffer
;		  WIDGET_CONTROL, w3.displayb.field, get_value=w3.displayb.file_name
;		  w3.displayb.file_name=w3.displayb.file_name(0)
;		  WIDGET_CONTROL, w3.textb.text, set_value=string('Saving file ',w3.displayb.file_name)
;		  IF (not writefile(w3.displayb.file_name,w3.textb.buffer)) then BEGIN
;		    WIDGET_CONTROL, w3.textb.text, set_value=['Error saving file: '+w3.displayb.file_name]
;		    wait,5
;		  ENDIF ELSE BEGIN
;		    WIDGET_CONTROL, w3.textb.text, set_value=['File: ' + w3.displayb.file_name + ' saved']
;		  ENDELSE
;		  WIDGET_CONTROL, w3.textb.text,set_value=w3.textb.buffer
;		endcase
;	    4:  begin
;		endcase
;	  endcase
;	endcase
;
;	w3.displayb.field:  WIDGET_CONTROL, w3.displayb.field, $
;			     get_value=w3.displayb.buffer_name
	  
        else:
    ENDCASE
        
    case event.top of
        w3.extrab.base(1): BEGIN
            root = 'output_task'
            outfile=files(event.value)
            print,'Output_task file: ',outfile
            pos=strpos(outfile,root) 
            endpos=strlen(root)+pos
            if (pos eq -1) or (outfile eq '') then begin ;did it work?
                print,'Strange output_task[string].dat filename...',$
                  'Program may be too stupid to comply...'
                dsr=''
            endif $
            else dsr=strmid(outfile,endpos,strlen(outfile)-endpos-strlen(ext))
            WIDGET_CONTROL,w3.plotb.field(0),set_value=outfile
        endcase 
        else:
    endcase
                                ;step:  0, skip analysis
                                ;       1, increment one mjf
                                ;       2, run continuously
    IF step ne 0 THEN BEGIN     ;if in analysis mode
;        la=1
        done=0
        event2=event
        WHILE not (done or eof(lundat)) DO BEGIN 
            ;;analyze one mjf
;            lz=read_lzrcd(lundat,recn,fh.rln,fh.spcid) ;read a major frame
;            recn=recn+1             ;increment counter ;;;done by the widgets...
            wbad=where(lz.qlty gt 0,nwbad) ;check quality flags
            
            ;;code to handle the specified end of anaylsis time
;            IF keyword_set(end_time) THEN BEGIN
;                IF ((lz.ms/8.640d+07) gt end_time) THEN BEGIN
;                    print,'You have reached the specified end_time:',end_time
;                    IF (kb) THEN BEGIN
;                        print,'Quit now [q], Erase end_time [e],'+$
;                          ' or return to start_time [s]? '
;                        CASE get_kbrd(1) OF
;                            'q': BEGIN & print,'Quiting' & get_out=1
;                            end
;                            'e': BEGIN & print,'End_time removed' & end_time=1 
;                            END
;                            's': BEGIN
;                                print,'Returning to start_time'
;                                recn=startrn & lz.ms = 0
;                            END
;                        ENDCASE
;                    ENDIF ELSE get_out=1
;                ENDIF
;            ENDIF
            
;            IF (skip) THEN BEGIN ;skip analysis
;                recn=recn-1         ;used when ploting
;                skip=skip-1
;            ENDIF ELSE IF nwbad gt 0 THEN BEGIN ;reset core and skip record
            IF nwbad gt 0 THEN BEGIN ;reset core and skip record
                print,'Bad mjf quality flags, recn:',recn,'    Last mjfm_tim:',mjfm_tim
;                IF shout THEN BEGIN
;                    printf,statunit,'Bad mjf quality flags, recn:',recn,$    
;                      'Number bad flags:', nwbad
;                    printf,statunit,'Last mjfm_tim:',mjfm_tim,format='(a,d20.7)'
;                ENDIF
                RESET_CORE        
            ENDIF ELSE BEGIN    ;analyze record
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ;get mode, time, spin_per
                mode_and_time,lz,scindx,ihk,hkm1,sp,mode,spin_per,mjfm_tim
                
;help,busy,callcntr,callcntr_total,file,get_out,  $
            max_nbytes,prt,prt1,request,result,rn,skip,  $
            step,timetag,numfcblocks,/str
;help,infile,outfile,dsr,files,lundat,recn,fh,lz, $
  scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf,/str

                FOR j=0,numfcblocks-1 do BEGIN ;loop through fcblocks in rcd
                    get_fcblock,fcblock,j,lz,fcblm1,scindx ;get the fcblock
                    
                                ;call the analysis program
                    result=CALL_EXTERNAL(core_so,object,                              $
                                         fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
                                         iqual,spec_tim,del_tim,anal_complete,reset,  $
                                         iunit_brds,iunit_mode,iunit_modlvls)
                    callcntr=callcntr+1l
                    IF (result eq 1) then print,'Errors not cleared...'
                    IF (anal_complete) THEN BEGIN
                        ;;timetag=decode_utc(fcblock(0:8))
                        ;;print,'Core calls: ',callcntr
                        callcntr_total=callcntr_total+callcntr
                        callcntr=0l
                        If prt THEN print,param,spec_tim,del_tim,$
                          format='(f9.2,g13.5,f13.5,f14.3,f14.3,f14.3,d18.5,d15.5)'
;                        IF shout THEN BEGIN
;                            IF prtf THEN printf,paramunit,''
;                            printf,paramunit,param,spec_tim,del_tim,$
;                              format='(f9.2,g13.5,f13.5,f14.3,f14.3,f14.3,d18.5,d15.5)'
;                        ENDIF
                    ENDIF
                ENDFOR
            ENDELSE
            
;            IF (not kb and request eq 'x') THEN $
;              IF get_kbrd(0) THEN kb=1
            
;            IF (kb and not get_out) THEN $
;              kb_opts,kb,recn,skip,lzfile,lundat,fh,mjfm_tim,request,get_out 
            
            ;;ENDREP UNTIL(eof(lundat) or get_out) ;continue until end
            ;;of file or user quits
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            IF step eq 1 THEN done=1 ELSE BEGIN
                ;;check for button events which signal to stop analysis
                event2=WIDGET_EVENT([w3.analysisb.button(1),$
                                     w3.quitb.button],/NOWAIT)
                CASE event2.id OF
                    w3.analysisb.button(1): BEGIN
                        IF n_tags(event2) eq 5 THEN BEGIN
                            IF event2.select eq 1 and event2.value eq 1 $
                              THEN done = 1
                        ENDIF
                    ENDCASE
                    w3.quitb.button: BEGIN
                        CASE event2.value OF
                            1:WIDGET_CONTROL, event2.top, /DESTROY
                            2:WIDGET_CONTROL, event2.top, /ICONIFY  
                            ELSE:
                        ENDCASE
                    ENDCASE
                    ELSE:
                ENDCASE
            ENDELSE
;            la=la+1
        ENDWHILE
;        print,la
        WIDGET_CONTROL,w3.base,/clear_events
    ENDIF
    step=0    ;accepting inputs...



END


;=================== CALLING PROCEDURE: wind3 ================================

PRO wind3, inputfile, start_time=start_time, end_time=end_time, kb=kb, $
	        test=test, shout=shout, fvm=fvm, fss=fss, archive=archive, $
                pick=pick, GROUP = GROUP

;Important:
;make sure you use the proper integer type
;variables types are not permanent in IDL
;any time you say "variable" = "number", 
;"variable" is assigned the integer type of "number"

;SET UP COMMON BLOCKS
common sharewidg,w3
common stuff,infile,outfile,dsr,files,lundat,recn,fh,lz, $
  scindx,ihk,hkm1,fcblm1,vsm1,vdatc,sdatc,sp,vsmjf
common core,core_so,object,                              $
            fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
            iqual,spec_tim,del_tim,anal_complete,reset,  $
            iunit_brds,iunit_mode,iunit_modlvls
common shell,busy,callcntr,callcntr_total,file,get_out,  $
            max_nbytes,prt,prt1,request,result,rn,skip,  $
            step,timetag,numfcblocks

;SET UP STRUCTURES
sp={spinparams,spincnt:0b,tjd:0l,sec:0d,mjfcnt:0b,spinp:0d,$
    old_spincnt:0b,old_tjd:0l,old_sec:0d,old_mjfcnt:0b,$
    lst_spinp:0d,lst_tjd:0l,lst_sec:0d,newmjf:1,datayes:0,lst_scimod:-1}

;WIDGET STRUCTURE
  w3={w3_widgets,						       $
      base:0l,						               $ 
                                ;base widget
	quitb:	    {q,base:0l,button:0l,field:0l},		       $
                                ;first subbase, window options	
        functionb:  {f,base:0l,button:lonarr(2)},		       $
                                ;functions for lz analysis
	statisb:    {s,base:0l,button:0l,field:lonarr(8)},	       $
                                ;lz record info
        analysisb:  {a,base:0l,button:lonarr(10),field:lonarr(10)},    $
                                ;analysis info...
        plotb:      {p,base:lonarr(10),button:lonarr(10),              $
                       field:lonarr(10)},                              $
                                ;ploting junk
	textb:	    {t,text:0l,buffer:'',old_buffer:''},	       $	
                                ;text display widget
	displayb:   {d,base:0l,button:0l,field:'',file_name:'',	       $
		       buffer_name:'',old_buffer_name:''},	       $
                                ;options for controling text display
	extrab:	    {e,base:lonarr(10),button:0l,field:0l}	       $
                                ;an array of extra bases to pop up	
    }

;BEGIN VARIABLE DECLARATIONS/INITIALIZATIONS

;LOCAL VARIABLE DECLARATIONS
busy           = 0              ;a looping flag
callcntr       = 0l             ;count calls to core, current spectrum
callcntr_total = 0l             ;count total calls
core_so        = 'kp_core.so'   ;name of core sharable object file
file           = ''             ;set file to input file name
get_out        = 0              ;to break out of a loop...
max_nbytes     = 284            ;future tm mode may need block this large
object         = '_translator'  ;executable subroutine found in core_so   
prt            = 1              ;1=verbose, 0=quiet
prt1           = 0              ;1=verbose, 0=quiet
request        = ''             ;user input string
result         = 0              ;error statis of call to core
rn             = 0              ;record num in file (includes file header)
skip           = 0              ;user input: number of records to skip
step           = 0              ;default continuous run mode
timetag        = ''

;PASSED VARIABLE DECLARATIONS, 
;;initial values are important as variable type setters
;;  > = passed to core from shell
;; <  = passed from core to shell
;;variables in call to translator.c and core_main.f, in order:
nbytes = 122L			;IDL long   = C int    = Fortran integer*4	 >
fcblock = bytarr(nbytes)	;IDL byte   = C char   = Fortran character*1	 >
mode = 1L			;IDL long					 >
spin_per = 3.			;IDL float  = C float  = Fortran real*4		 >
mjfm_tim = 1994056.00000000D	;IDL double = C double = Fortran real*8		 >
param = fltarr(6)		;IDL float					<
iqual = lonarr(6)		;IDL long					<
spec_tim = 0D			;IDL double					<
del_tim = 0.			;IDL float					<
anal_complete = 0L		;IDL long   -> C int    -> Fortran logical*4	<
reset = 0L			;IDL long   -> C int    -> Fortran logical*4     >
iunit_brds=20L                  ;IDL long					 >
iunit_mode=21L                  ;IDL long					 >
iunit_modlvls=22L		;IDL long					 >
                                ;C and IDL do not have type logical varibles 
                                ; so i used int types (same size)

;CHECK USER OPTIONS
IF NOT keyword_set(test) THEN test=0
IF NOT keyword_set(shout) THEN shout=0
IF keyword_set(fss) THEN BEGIN  ;temp single spin default mode 
    IF (inputfile eq '') THEN $
      file='/plasma/h1/wind/source/analysis/sskp/wi_lz_swe_19950102_v01.dat' $
    else file   = inputfile
    test        = 1
    fvm         = 1
    kb          = 1
    shout       = 0
ENDIF ELSE file = inputfile

IF keyword_set(fvm) THEN $
  core_so = '/plasma/h1/wind/source/analysis/sskp/kp_core_ss.so'

IF keyword_set(pick) THEN BEGIN
;get new input lz data file
    lzpath='/plasma/d9/wind/lz_files/'
    inputfile=pickfile(/read,path=lzpath(0),filter='*.dat *.dat.gz *.DAT.gz',$
                    title='Level Zero Data Files')
    if (inputfile ne '') then file=strtrim(inputfile) ;else file remains default...
ENDIF
len=strlen(file)
pos=strpos(file,'.gz') 
IF ((pos ge 1) and (pos eq len-3)) THEN BEGIN ;if file gziped...
    print,'Decompressing file.  Please wait...',format='(a,$)'
    spawn,'gunzip '+file,/sh    ;gunzip file
    print,'  Done.'
    strput,file,'   ',len-3     ;remove .gz extention
    file=strtrim(file)          ;remove extra spaces
    ;;print,file
ENDIF

;FILE VARIABLE INITIALIZATIONS
infile=file			;level zero filename
paramfile=file+'.prm'           ;output parameter file
statfile=file+'.stat'           ;file quality and shell operation status file
;orbfile=?			;describes spacecraft position
;attfile=?			;describes spacecraft orientation

;END VARIABLE DECLARATIONS


;OPEN LEVEL ZERO FILE.
openr,lundat,infile,/get_lun
print,'infile:',infile

;OPEN ORBIT AND ATTITUDE FILES, OPEN OPERATION STATIS FILE (NOTEFILE)
IF (test) then BEGIN
    paramfile='testmode.prm'
    statfile='testmode.stat'
    iunit_brds=25L              ;send message of testmode via unit number
    IF (shout) THEN print,'Using alternate output files: ',paramfile,$
      '& ',testfile
ENDIF

;OPEN PARAMETER AND STATIS OUTPUT FILE
IF shout THEN BEGIN
    openw,paramunit,paramfile,/get_lun
    spawn,'sccs prt -y idl_shell2.pro',ver,/sh
    spawn,'date',date,/sh
    printf,paramunit,['MIT SWE analysis, idl_shell2.pro version: ',$
                      strmid(ver,24,5)],date
    printf,paramunit,'Level zero input file: ',infile,format='(a,/)'
    printf,paramunit,'      Vx          Vy          Vz         H+ density'+$
      'He++ %density   Vtherm          Time             dTime'
    openw,statunit,statfile,/get_lun
    printf,statunit,['MIT SWE analysis status, idl_shell2.pro version: ',$
                     strmid(ver,24,5)],date
    printf,statunit,'Level zero input file: ',infile,format='(a,/)'
ENDIF

;INITIAL CLEANUP OF CORE VARIABLES
RESET_CORE

;GET LZFILE ATTRIBUTES
fh=read_lzhdr(lundat)           ;fh=file header structure 

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
s=size(fcblm1)
numfcblocks=s(1)                         ;get number of fcblocks per record
                   
;read file header (Jim Byrnes' procedure)
rn=1
if keyword_set(start_time) then begin
    gototime,infile,lundat,rn,fh.rln,fh.spcid,start_time
    startrn=rn
endif else startrn=0
if keyword_set(end_time) then begin
    end_time=float((end_time/100)*60+(end_time mod 60))/1440 
    print,'End_time: ',end_time
endif
if keyword_set(kb) then begin
    kb=1
    kbcount=0
    print,'Interactive mode'
endif else kb=0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
create_widget_structure,infile,outfile,dsr
WIDGET_CONTROL, w3.base, /REALIZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;read and process first record in file
  recn=rn
  WIDGET_CONTROL,set_value=recn,w3.statisb.field(5)
  read_rec,date_time		;read record and display time from header
  WIDGET_CONTROL,set_value=date_time,w3.statisb.field(0)
  proc_rec			;process lz record  

  XMANAGER, "wind3", w3.base, GROUP_LEADER = GROUP ;hand off to manager

END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
