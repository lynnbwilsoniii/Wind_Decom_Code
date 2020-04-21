
;     @(#)swelzfc.pro   VERSION 1.4    01/10/99  16:29:43
;
; swelzfc.pro    (adapted from idl_shell2.pro)
; This procedure reads the SWE LZ data and returns the Faraday Cup data
; in an IDL data structure.
;
; Example procedure calls:
; 
; Choose LZ data with pickfile.  Single step through LZ records.  
; Return data in structure "FCspectra"
;
;    IDL> swelzfc, FCspectra, /kb 
;
;
; Specify LZ data file with keyword.    
; Return data in structure "FCspectra" 
;
;    IDL> swelzfc, FCspectra, infile = '/nfs/oersted/d3/wind/lz_files/97102701.dat' 
;
;*********************************
; ***** How fast can this run? ***
;*********************************
; test.bat:
;   $date
;   swelzfc, FCspectra, infile = '/nfs/oersted/d3/wind/lz_files/WI_LZ_SWE_19980531_V01.DAT'
;   save, FCspectra
;   $date
;
;IDL> @test.bat
;Tue Jun  2 13:58:30 EDT 1998
;% Compiled module: SWELZFC.
;lzfile:/nfs/oersted/d3/wind/lz_files/WI_LZ_SWE_19980531_V01.DAT
;% Compiled module: READ_LZHDR.
;% Compiled module: CONV_VAX_UNIX.
;% Compiled module: IND_SCIDAT.
;% Compiled module: IHKMAP.
;% Compiled module: MODE1MAP.
;% Compiled module: MODE2MAP.
;% Compiled module: READ_LZRCD.
;% Compiled module: GET_BITS.
;% Compiled module: TIME_UTC.
;% Compiled module: SPINPERIOD.
;Bad mjf quality flags, recn:     223    Last mjfm_tim:       1998151.2
;Too many consecutive fcblocks without an FCFF block.  Data thrown away
;Done
;Tue Jun  2 14:01:53 EDT 1998
;IDL> 
;
;**************************************
;**************************************

;*******************************************************************************
;*******************************************************************************
;PRO swelzfc, FCspectra, infile = infile,$
;             start_time=start_time, end_time=end_time,$
;             kb=kb, d9=d9, dr=dr, $
;             pick=pick, archive=archive
;
; All keywords are optional and function independently or in almost
; any combination, with exceptions noted below.
; ie:	      idl_shell2,'mylzfile',start=345,end=1523,/kb
;     	      this means go to time 3:45am, enter interactive mode (KeyBoard mode)
;     	      and quit when you hit 3:23pm.
; or:	      idl_shell2,'mylzfile',start=345,kb=0
;	      same start time, finish at end of day, non-keyboard mode
;;;;;;;;;;;;;;
; kb mode:    interactive analysis mode.  allows you to step or skip through
;	      the lzfile, and plot the currents and fits...
;             See the help listing below for commands (in PRO printhelp).
; archive:    Archive=0, nothing special, archive.ne.0: Decompress archived 
;             input file, run analysis, recompress when done.             
;             Specify full file name and path (or use d9, dr, or d3).
; d9, dr, d3  Shortcuts, don't use both at the same time! These keywords
;             specify which directory the lz file should be found in.
;             ie: idl_shell2,'wi_lz_swe_19941201_v01.dat',/d9 
;             instead of
;                 idl_shell2,'/plasma/d9/wind/lz_files/wi_lz_swe_19941201_v01.dat'
; pick        pick allows you to scan for the lz file you want.  automatically
;             decompresses gziped files.  Works with d9 and dr keywords.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;begin decode_utc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION decode_utc,bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;; end  decode_utc ;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION count_rn,c_file,prt=prt,exact=exact
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;; end  count_rn ;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PRO reset_core
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;common core,core_so,object,                              $
;            fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
;            iqual,spec_tim,del_tim,anal_complete,reset,  $
;            iunit_brds,iunit_mode,iunit_modlvls
;fcblock(0)='ff'XB
;fcblock(1)='00'XB
;reset=1L
;result=call_external(core_so,object,                              $
;                     fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
;                     iqual,spec_tim,del_tim,anal_complete,reset,  $
;                     iunit_brds,iunit_mode,iunit_modlvls)
;reset=0L
;end
;;;;;;;;;;;;;;;;;;;;;;;;;;;; end reset_core ;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO gototime,lzfile,lzunit,rn,rln,spcid,start_time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;hhmm is a variable, and a symbol denoting the military time format
;here is the plan: I'm going to work in MINUTES, so i have to convert
;military time to minutes:  minutes= hhmm/100*60+hhmm-(hhmm/100*100)
;                           minutes=(hh*60)+mm
;see how i use integer multiplication to do tens shifts? (a/100*100 is not eq a)
;start_time is where we want to go (in military time)
;hhmm is where we are (in military time)
;rn is the current record number
;tot_rn is the total number of records in lzfile
;rpm is the approx number of records per minute

start_time = start_time mod 2400                     ;make it reasonable
start_min  = start_time/100*60+start_time-(start_time/100*100)  ;get minutes
rcd        = read_lzrcd(lzunit,1,rln,spcid)          ;read first record
begin_min  = rcd.ms/60000                            ;get beginning min into day
tot_rn     = count_rn(lzfile)                        ;get total number of records
rcd        = read_lzrcd(lzunit,tot_rn-5,rln,spcid)   ;get near last record 
end_min    = rcd.ms/60000                            ;get final min into day
rpm        = float(tot_rn/float(end_min-begin_min))  ;get records per minute
rn         = fix(start_min*rpm)                      ;guess approx rn to go to
rcd        = read_lzrcd(lzunit,rn,rln,spcid)         ;read new record
min        = rcd.ms/60000                            ;get current minutes into day

WHILE (min lt start_min and not eof(lzunit)) DO BEGIN
    rn  = rn+1                                       ;increment record number
    rcd = read_lzrcd(lzunit,rn,rln,spcid)            ;read new record
    min = rcd.ms/60000                               ;get current minutes into day
ENDWHILE

rn=rn-1
;reset_core   ;just incase it had some stuff stored in its arrays already
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;; end gototime ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO gotofract,lzfile,lzunit,rn,rln,spcid,fract=fract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF NOT keyword_set(fract) THEN BEGIN
    print,'Enter decimal fraction of day to skip to: ',format='(a,$)'
    read,'',fract
ENDIF
fract=abs(fract mod 1)
if fract lt 0.001 then fract = 0.001
gototime,lzfile,lzunit,rn,rln,spcid,fract*2400
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;; end gotofract ;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO printhelp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
helparr=['Command        Action',$
         '   ?      Print Command Summary',                    $
         ' + = >    Increment 1 record',                       $
         ' - _ <    Decrement 1 record',                       $
         '   1      raw currents, 20 panel plot',                   $
         '   2      raw currents, 9 panel plot',                    $
         '   3      Distribution function, angle 0',                $
         '   4      20 panels, with proton + alpha fits',              $
         '   5      9 panels, with proton + alpha fits',               $
         '   6      Distribution function, all angles',                  $
         '   7      Distribution function, all angles, both cups',       $
         '   8      12 panels, alpha fits, proton fits subtracted',      $
         '   e      Plot & Print: SS Params vs Energy Window', $
         '   f      Go to fraction of day',                    $
         '   h      hplot',                                    $
         '   H      hardplot',                                 $
         '   l      log output',                               $
         '          (cp output_task[_ss].dat output_task[_ss]_mjfm_tim.rn#.dat)',$
         '   m      calculate moments',                         $
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
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;; end printhelp ;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO kb_opts,kb,rn,skip,lzfile,lzunit,hdr,mjfm_tim,request,get_out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print,'idl_shell2 [rn='+strtrim(rn,1)+'> ',format='(a,$)'
request=get_kbrd(1)
print,request
skip=1
case request of
    '+': skip=0
    '.': skip=0
    '>': skip=0
    '=': skip=0
    '-': rn=rn-2
    '-': rn=rn-2
    ',': rn=rn-2
    '<': rn=rn-2 
    '1': begin
      print, ' ','20 Panels - Please choose an option:'
      print, '1. Current vs. Energy Window (default)'
      print, '2. Distribution Function vs. Proton Velocity'
      print, '3. Current vs. Proton Velocity'
      print, '4. Current vs. Voltage'
      req2=get_kbrd(1)
      print, req2, ' '
      print, 'Please choose your Faraday Cup (1 or 2): '
      req3=get_kbrd(1)
      if (req3 eq '2') then req3 = 2 else req3 = 1
      case req2 of
        '4': splot1f, /vsvolt, cup=req3
        '3': splot1f, /vsvel, cup=req3
        '2': splot1f, /dist, cup=req3
        else: splot1f, cup=req3
      endcase
     end 
    '2': begin
      print, ' ','9 Panels - Please choose an option:'
      print, '1. Current vs. Energy Window (default)'
      print, '2. Distribution Function vs. Proton Velocity'
      print, '3. Current vs. Proton Velocity'
      print, '4. Current vs. Voltage'
      req2=get_kbrd(1)
      print, req2, ' '
      print, 'Please choose your Faraday Cup (1 or 2): '
      req3=get_kbrd(1)
      if (req3 eq '2') then req3 = 2 else req3 = 1
      case req2 of
        '4': splot1a, /vsvolt, cup=req3
        '3': splot1a, /vsvel, cup=req3
        '2': splot1a, /dist, cup=req3
        else: splot1a, cup=req3
      endcase
     end 
    '3': begin
      print,' ', 'Please choose your Faraday Cup (1 or 2): '
      req2=get_kbrd(1)
      if (req2 ne 2) then req2 = 1
      splot3f, cup=req2
      end

    '4': begin
      print,' ','Please choose your Faraday Cup (1 or 2): '
      req2=get_kbrd(1)
      if (req2 ne 2) then req2 = 1
      splot12, cup=req2
      end

    '5': begin
      print,' ','Please choose your Faraday Cup (1 or 2): '
      req2=get_kbrd(1)
      if (req2 ne 2) then req2 = 1
      splot12a, cup=req2
      end

    '6': begin
      print,' ','Please choose your Faraday Cup (1 or 2): '
      req2=get_kbrd(1)
      if (req2 ne 2) then req2 = 1
      splot7f, cup=req2
      end

    '7': splot8f

    '8': begin
     print,' ','Please choose your Faraday Cup (1 or 2): '
      req2=get_kbrd(1)
      if (req2 ne 2) then req2 = 1
      splot14, cup=req2
      end

    'e': begin
        ssplot2f
        ssfit
    end
    'f': gotofract,lzfile,lzunit,rn,hdr.rln,hdr.spcid
    'h': hplot
    'H': hardplot
    'l': begin
        time_stamp=strtrim(string(mjfm_tim,format='(d12.3)'),1)
        rn_stamp='_rn'+strtrim(rn,1)
        cmd=string('cp output_task.dat output_task_',$
                   time_stamp,rn_stamp,'.dat')
        spawn,cmd,/sh
        cmd=string('cp output_task_ss.dat output_task_ss_',$
                   time_stamp,rn_stamp,'.dat')
        spawn,cmd,/sh
    end
    'm': moments
    'p': splot
    'P': startplot
    's': begin
        skip=0
        print,'Enter +/- number of records to skip: ',format='(a,$)'
        read,'',skip
        rn=rn+skip-1
        IF rn lt 0 THEN rn = 0
     end	
    'S': stop
    't': print,mjfm_tim,format='(d13.3)'
    'x': begin & print,'Exiting keyboard mode' & kb = 0 
    end
    '?': printhelp
    'q': begin
        print,'End program? [y,n] ',format='(a,$)'
        IF get_kbrd(1) eq 'y' THEN begin 
            get_out=1 & print,'y' 
        endif
     end
    'Q': get_out=1
    'z': ssplotf
    'Z': begin
        splot,/por
        ssplotf
        hplot
    end
    else: print,'Invalid command.  Choose ? for help.'
endcase
END    
;;;;;;;;;;;;;;;;;;;;;;;;;;;; end kb_opts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO mode_and_time,rcd,scindx,ihk,utc_1stspn,sp,mode,spin_per,mjfm_tim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PRO mode_and_time,rcd,scindx,ihk,hkm1,sp,mode,spin_per,mjfm_tim
;; determine tm mode, tm rate, science mode, and mjf count from instr hk
tmmode_ihk=get_bits(rcd.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
tmrate_ihk=get_bits(rcd.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
scimode_ihk=get_bits(rcd.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)

print,'scindx(utc_1stspn)',scindx(utc_1stspn)
stop
;time_utc,rcd.mf(scindx(hkm1(1).offs:hkm1(1).offs+6)),$
time_utc,rcd.mf(scindx(  utc_1stspn  )),$
  tjd,sec,hour,min,isec,ms,hms,spincnt
sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt 

;;get spin period
sp.mjfcnt=rcd.mf(ihk(1).offs)

spinperiod,sp  

;print,'time tag:  tjd      secdy  hh:mm:ss.ms  spincnt  spinperiod'
;print,sp.tjd,sp.sec,hour,':',min,':',isec,'.',ms,sp.spincnt,sp.spinp,$
;format='(9x,i5,2x,f9.3,2x,i2,a1,i2,a1,i2,a1,i3,2x,i3,f12.3)'
;print,tmmode_ihk,tmrate_ihk,scimode_ihk,sp.mjfcnt,format='(3i5,z3.2)'
;;SET CORE VARIABLES
mode=long(scimode_ihk)
spin_per=float(sp.spinp)
mjfm_tim=(rcd.yr*1000d0)+rcd.dy+(rcd.ms/8.640d+07)
END
;;;;;;;;;;;;;;;;;;;;;;;;;;;; end mode_and_time ;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PRO get_fcblock,fcblock,j,rcd,fcbl,scindx
;fcblock is the returned fcblock, j is the block index within a record
;fcblm1 is structure containing info on the mjf contents

  fcoff=scindx(fcbl(j).offs+indgen(fcbl(j).ln))

  FOR k=0,fcbl(j).ln-1 do BEGIN
      offset=scindx(fcbl(j).offs+k)
      fcblock(k)=rcd.mf(offset)
  ENDFOR


END
;;;;;;;;;;;;;;;;;;;;;;;;;;;; end get_fcblock ;;;;;;;;;;;;;;;;;;;;;;;;;


;*********************************************************************
;*********************************************************************
;*********************THE MAIN CALLING PROGRAM************************
;*********************THE MAIN CALLING PROGRAM************************
;*********************THE MAIN CALLING PROGRAM************************
;*********************THE MAIN CALLING PROGRAM************************
;*********************************************************************
;*********************************************************************
;************************PROCEDURE SWELZFC****************************
;************************PROCEDURE SWELZFC****************************
;*********************************************************************
;*********************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Example procedure calls:
;
; Choose LZ data with pickfile.  Single step through LZ records.
; Return data in structure "FCspectra"
;
; IDL> swelzfc, FCspectra, /kb
;
;
; Specify LZ data file with keyword.  
; Return data in structure "FCspectra"
;
; IDL> swelzfc, FCspectra, infile = '/nfs/oersted/d3/wind/lz_files/97102701.dat'
;
;

PRO swelzfc, FCspectra, infile = infile,$
             start_time=start_time, end_time=end_time,$
             kb=kb, d9=d9, dr=dr, $
             pick=pick, archive=archive

;common core,core_so,object,                              $
;            fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
;            iqual,spec_tim,del_tim,anal_complete,reset,  $
;            iunit_brds,iunit_mode,iunit_modlvls
;
if(not keyword_set(infile)) then begin
  infile = '' 
  pick = 1 
endif

;Important:
;make sure you use the proper integer type
;variables types are not permanent in IDL
;any time you say "variable" = "number", 
;"variable" is assigned the integer type of "number"

;BEGIN VARIABLE DECLARATIONS/INITIALIZATIONS

;******************************************************************
;SET UP STRUCTURE FOR FC SPECTRA
; This initializes the structure FCspectra.
; The maximum size is 2000 spectra, which should be enough for 1 day.
; FCspectra.swemode, FCspectra.spin_period, 
; FCspectra.fcblocks, FCspectra.numblocks, 
;    all initialized to zero
; FCspectra.majfrmtim initialized to 1994056.00000000D
 
     dummyblock = bytarr(33,122)  ; initialize an FC block 
     dummyblock(*,*) = 0B         ; fill initial FC block with 0B 
 
     spectra = {swemode: 0, $
                spin_period:0.0, $
                majfrmtim: 1994056.00000000D, $
                numblocks: 0, $
                fcblocks: dummyblock}

     FCspectra = REPLICATE( spectra, 2000)
;******************************************************************

;INITIALIZE COUNTERS

spec_count = 0
num_blocks = 0 
 
;LOCAL VARIABLE DECLARATIONS
busy           = 0              ;a looping flag
callcntr       = 0l             ;count calls to core, current spectrum
callcntr_total = 0l             ;count total calls
core_so        = 'kp_core.so'   ;name of core sharable object file
file           = ''             ;set file to input file name
get_out        = 0              ;to break out of a loop...
max_nbytes     = 284            ;future tm mode may need block this large
object         = '_translator'  ;executable subroutine found in core_so   
prt            = 1              ;1=verbose, 0=quiet   print to screen
prtf           = 0              ;1=verbose, 0=quiet   print to file
request        = ''             ;user input string
result         = 0              ;error statis of call to core
rn             = 0              ;record num in file (includes file header)
skip           = 0              ;user input: number of records to skip
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

IF N_PARAMS() eq 0 THEN BEGIN 
    infile=''
    pick=1 
ENDIF ELSE BEGIN
    if keyword_set(d9) THEN infile  = '/plas7/d9/wind/lz_files/'+infile
    if keyword_set(dr) THEN infile  = '/plasma/dr/wind/lz_files/'+infile
    if keyword_set(d3) THEN infile  = '/plas7/d3/wind/lz_files/'+infile
ENDELSE
 

IF keyword_set(pick) THEN BEGIN
    lzpath='/nfs/oersted/d3/wind/lz_files'
    if keyword_set(d9) then lzpath='/plas7/d9/wind/lz_files/'
    if keyword_set(dr) then lzpath='/plasma/dr/wind/lz_files/'
    if keyword_set(d3) then lzpath='/plas7/d3/wind/lz_files/'
    infile=pickfile(/read,path=lzpath(0),$
                    filter='*',$
                    title='Level Zero Data Files')
    if (infile ne '') then file=strtrim(infile)
ENDIF ELSE file = strtrim(infile)

IF (file eq '') THEN BEGIN
    print,'Invalid file name choosen, please start over.'
    return
ENDIF
archive=1 ;i've decided to always accept gziped files as input
IF keyword_set(archive) or keyword_set(pick) THEN BEGIN
    len=strlen(file)
    pos=strpos(file,'.gz') 
    IF (pos eq len-3) THEN BEGIN ;if file gziped...
        print,'Decompressing file.  Please wait...',format='(a,$)'
        spawn,'gunzip '+file,/sh ;gunzip file
        print,'  Done.'
        strput,file,'   ',len-3 ;replace .gz extention with '   '
        file=strtrim(file)
    ENDIF
ENDIF

;FILE VARIABLE INITIALIZATIONS
lzfile=file			;level zero filename
paramfile=file+'.prm'           ;output parameter file
statfile=file+'.stat'           ;file quality and shell operation status file
;orbfile=?			;describes spacecraft position
;attfile=?			;describes spacecraft orientation

;END VARIABLE DECLARATIONS


;OPEN LEVEL ZERO FILE.
openr,lzunit,lzfile,/get_lun
print,'lzfile:',lzfile

;OPEN ORBIT AND ATTITUDE FILES, OPEN OPERATION STATIS FILE (NOTEFILE)
;OPEN PARAMETER AND STATIS OUTPUT FILE

;INITIAL CLEANUP OF CORE VARIABLES
;RESET_CORE

;GET LZFILE ATTRIBUTES
hdr=read_lzhdr(lzunit) & rn = 1 ;read the file header

;get indices, scindx, of science data, 
;i.e., the indices of the rcd array, scidat, without the instr hk
ind_scidat,scindx 

;get indices of instrument housekeeping into rcd array, rcd.mf   
ihkmap,ihk 
 
;get mode1 tm map of science and genl hk data offsets into scindx
mode1map,hkm1,fcblm1            ;hkm1=genl hk offsets for mode1
                                ;fcblm1=faraday cup offsets

;get mode2 tm map of science and genl hk data offsets into scindx
mode2map,ghk,fcblm2            ;hkm2=genl hk offsets for mode1
                                ;fcblm2=faraday cup offsets

s=size(fcblm1)
numfcblocks=s(1)                ;get number of fcblocks per record
        
;set up spinparams sturcture used by fitzenreiter's PROcedure spinperiod
sp={spinparams,spincnt:0b,tjd:0l,sec:0d,mjfcnt:0b,spinp:0d,$
    old_spincnt:0b,old_tjd:0l,old_sec:0d,old_mjfcnt:0b,$
    lst_spinp:0d,lst_tjd:0l,lst_sec:0d,newmjf:1,datayes:0,lst_scimod:-1}

if keyword_set(start_time) then begin
    gototime,lzfile,lzunit,rn,hdr.rln,hdr.spcid,start_time
    startrn=rn
endif else startrn=0
if keyword_set(end_time) then begin
    end_time=float((end_time/100)*60+(end_time mod 60))/1440 
    print,'End_time: ',end_time
endif
if keyword_set(kb) then begin
    kb=1
    kbcount=0
    print,'Interactive mode','Press ? for help',format='(/a/a)'
endif else kb=0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;START ANALYSIS

REPEAT BEGIN
    rcd=read_lzrcd(lzunit,rn,hdr.rln,hdr.spcid) ;read a major frame
    rn=rn+1                                     ;increment counter
    wbad=where(rcd.qlty gt 0,nwbad)             ;check quality flags
    
    ;;code to handle the specified end of anaylsis time
    IF keyword_set(end_time) THEN BEGIN
        IF ((rcd.ms/8.640d+07) gt end_time) THEN BEGIN
            print,'You have reached the specified end_time:',end_time
            IF (kb) THEN BEGIN
                print,'Quit now [q], Erase end_time [e],'+$
                  ' or return to start_time [s]? '
                CASE get_kbrd(1) OF
                    'q': BEGIN & print,'Quiting' & get_out=1
                    end
                    'e': BEGIN & print,'End_time removed' & end_time=1 
                    END
                    's': BEGIN
                        print,'Returning to start_time'
                        rn=startrn & rcd.ms = 0
                    END
                ENDCASE
            ENDIF ELSE get_out=1
        ENDIF
    ENDIF

    IF (skip) THEN BEGIN                        ;skip analysis
        rn=rn-1                                 ;used when ploting
      skip=skip-1
    ENDIF ELSE IF nwbad gt 0 THEN BEGIN    	;reset core and skip record
        print,'Bad mjf quality flags, recn:',rn,'    Last mjfm_tim:',mjfm_tim
        ;RESET_CORE        
    ENDIF ELSE BEGIN                            ;analyze record
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    tmmode_ihk=get_bits(rcd.mf(ihk(2).offs),ihk(2).bv(1).p,ihk(2).bv(1).n)
    tmrate_ihk=get_bits(rcd.mf(ihk(2).offs),ihk(2).bv(2).p,ihk(2).bv(2).n)
    scimode_ihk=get_bits(rcd.mf(ihk(2).offs),ihk(2).bv(0).p,ihk(2).bv(0).n)
    mode=long(scimode_ihk)
 
    utc_1stspn =  [1,2,3,4,5,6,7]
;   if ( (mode eq 1) or (mode eq 4)) then utc_1stspn = hkm1(1).offs + [0,1,2,3,4,5,6]
    if ( (mode eq 1) or (mode eq 4) or (mode eq 6)) $
       then utc_1stspn = hkm1(1).offs + [0,1,2,3,4,5,6]

    if ( (mode eq 2) or (mode eq 11) or (mode eq 3) ) then $
        utc_1stspn = ghk(1).offs + [0,1,2,3,4,5,6]
    ;TEST;  print, 'mode = ', mode
    ; print,'utc_1stspn',utc_1stspn

    time_utc,rcd.mf(scindx(  utc_1stspn  )),$
      tjd,sec,hour,min,isec,ms,hms,spincnt
    sp.tjd=tjd & sp.sec=sec & sp.spincnt=spincnt
 
    ;;get spin period
    sp.mjfcnt=rcd.mf(ihk(1).offs)
 
    spinperiod,sp
 
    ;;SET CORE VARIABLES
      spin_per=float(sp.spinp)
      mjfm_tim=(rcd.yr*1000d0)+rcd.dy+(rcd.ms/8.640d+07)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



                                                ;get mode, time, spin_per
        ;mode_and_time,rcd,scindx,ihk,hkm1,sp,mode,spin_per,mjfm_tim
        ;mode_and_time,rcd,scindx,ihk,utc_1stspn,sp,mode,spin_per,mjfm_tim


        ;if ( (mode eq 1)or(mode eq 4)) then BEGIN 
         if ( (mode eq 1)or(mode eq 4)or(mode eq 6)) then BEGIN 

           fcbl = fcblm1
           numfcblocks = 31
        endif
        if ( (mode eq 2) or (mode eq 11) or (mode eq 3) ) then BEGIN
           fcbl = fcblm2
           numfcblocks = 17
        endif


;***************
;***************


                                
        FOR j=0,numfcblocks-1 do BEGIN              ;loop through fcblocks in rcd
            ;if ((mode eq 1) or (mode eq 4)) then $ 
             if ((mode eq 1)or(mode eq 4)or(mode eq 6)) then $
            get_fcblock,fcblock,j,rcd,fcbl,scindx ;get the fcblock

            if ( (mode eq 2) or (mode eq 3) ) then $
            ;fcblock = rcd.mf( scindx( fcbl(j).ind) )
            fcblock = rcd.mf(  fcbl(j).ind )
          ;*********************************************************
          ;****ADD LINE BELOW TO THROW OUT last 24 bytes of block.
          ;****For mode 2,3, fcblock size is 146, but last 24 bytes
          ;****are not used!  JTS 6/11/98
            fcblock = fcblock(0:121)
          ;*********************************************************
            ;print, fcbl(j).ind
            ;print,'fcblock', fcblock(0:1)

            ; Check for FCFF block -
            ; Look at mod levels -
            ;if( fcblock(0) eq 'fc'XB and fcblock(1) eq 'ff'XB) then begin $
               ;print, 'FCFF',fcblock 
                ;print,fcblock(13),fcblock(14) + (fcblock(15) and '0f'XB)*256,(fcblock(15)/16)+fcblock(16)*16
                ;print,fcblock(20),fcblock(21) + (fcblock(22) and '0f'XB)*256,(fcblock(22)/16)+fcblock(23)*16
                ;print,fcblock(27),fcblock(28) + (fcblock(29) and '0f'XB)*256,(fcblock(29)/16)+fcblock(30)*16
                ;print,fcblock(34),fcblock(35) + (fcblock(36) and '0f'XB)*256,(fcblock(36)/16)+fcblock(37)*16
                ;print,fcblock(41),fcblock(42) + (fcblock(43) and '0f'XB)*256,(fcblock(43)/16)+fcblock(44)*16
                ;print,fcblock(48),fcblock(49) + (fcblock(50) and '0f'XB)*256,(fcblock(50)/16)+fcblock(51)*16
                ;print,fcblock(55),fcblock(56) + (fcblock(57) and '0f'XB)*256,(fcblock(57)/16)+fcblock(58)*16
                ;print,fcblock(62),fcblock(63) + (fcblock(64) and '0f'XB)*256,(fcblock(64)/16)+fcblock(65)*16
                ;print,fcblock(69),fcblock(70) + (fcblock(71) and '0f'XB)*256,(fcblock(71)/16)+fcblock(72)*16
                ;print,fcblock(76),fcblock(77) + (fcblock(78) and '0f'XB)*256,(fcblock(78)/16)+fcblock(79)*16
                ;print,fcblock(83),fcblock(84) + (fcblock(85) and '0f'XB)*256,(fcblock(85)/16)+fcblock(86)*16
            ;endif
                                               ;call the analysis program
; print, 'CALL EXTERNAL'


;****************************************

if( (fcblock(0) eq 'ff'XB) and (fcblock(1) eq '00'XB)) $
     then GOTO, END_OF_LOOP ; skip fill blocks


num_blocks = num_blocks + 1               ; counts blocks within this spectrum
;TEST; print, 'num_blocks', num_blocks, 'spec_count', spec_count
;TEST; print, fcblock(0:1)
;TEST; if(num_blocks eq 1) then stop

if(num_blocks gt 33) then begin
   num_blocks = 1
   print, 'Too many consecutive fcblocks without an FCFF block.  Data thrown away'
   FCspectra(spec_count) = spectra 
endif

FCspectra(spec_count).fcblocks(num_blocks-1,*) = fcblock  ; add the next fcblock
FCspectra(spec_count).spin_period = spin_per
FCspectra(spec_count).swemode = mode
FCspectra(spec_count).majfrmtim = mjfm_tim 
FCspectra(spec_count).numblocks = num_blocks 

if( $
  (FCspectra(spec_count).fcblocks(num_blocks-1,0) eq 'fc'XB) $
  and $
  (FCspectra(spec_count).fcblocks(num_blocks-1,1) eq 'ff'XB) ) then begin

    spec_count = spec_count + 1
    num_blocks = 0
    ;TEST; stop
endif

            ;result=CALL_EXTERNAL(core_so,object,                              $
;                                 fcblock,nbytes,mode,spin_per,mjfm_tim,param, $
;                                 iqual,spec_tim,del_tim,anal_complete,reset,  $
;                                 iunit_brds,iunit_mode,iunit_modlvls)
            callcntr=callcntr+1l
         END_OF_LOOP:  
         ENDFOR
    ENDELSE
    
    IF (not kb and request eq 'x') THEN $
      IF get_kbrd(0) THEN kb=1
    
    IF (kb and not get_out) THEN $
      kb_opts,kb,rn,skip,lzfile,lzunit,hdr,mjfm_tim,request,get_out


ENDREP UNTIL(eof(lzunit) or get_out) ;continue until end of file or user quits
;print,'Core calls: ',callcntr_total

;END ANALYSIS

;FINAL CLEANUP OF CORE VARIABLES
;RESET_CORE

free_lun,lzunit
IF ( keyword_set(archive) and keyword_set(kb) ) THEN BEGIN
    print,'gzip file '+file+'? [y,n] ',format='(a,$)'
    IF get_kbrd(1) eq 'y' THEN BEGIN
        print,'Compressing file.'
        spawn,'gzip -9 '+file+' &',/sh ;gzip file
    ENDIF ELSE print,'File not compressed.'
ENDIF
print,'Done'

;***************************************
;***************************************
FCspectra = FCspectra( where(FCspectra.numblocks ne 0) )
;***************************************
;***************************************

END
