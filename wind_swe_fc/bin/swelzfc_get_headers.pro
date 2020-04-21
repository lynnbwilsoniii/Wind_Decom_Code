


pro header_scratch

restore, 'ionspec20141203.idl'
s1 = fcspectra
restore, 'ionspec20131203.idl'
s2 = fcspectra




end











; In this session, we will try to compare a recent and a pre-reset data file. First, we'll try to get the housekeeping info

pro swelzfc_get_headers, infile

if(not keyword_set(infile)) then begin
  infile = '' 
  pick = 1 
endif

;;BEGIN VARIABLE DECLARATIONS/INITIALIZATIONS

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

file = strtrim(infile)
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


