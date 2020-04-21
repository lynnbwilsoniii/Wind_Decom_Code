PRO idl_shell2,INFILE=INFILE

;Local variable declarations
  lpr = 0			;1=verbose, 0=quiet
  max_nbytes = 284		;future tm mode may need block this large  
				;check C static int and IDL parameter()
  result=0			;error statis of call to core

;Level zero file variable declarations
;lzfile filename
  lzunit=99			;random value, will be reset by get_lun
  lzfile=''
;define file header
  hdr=bytarr(11552)
;define data record structure
  mjf={junk,rechdr:bytarr(300),data:bytarr(11552-300)}

;Passed variable declarations, some initial values are just type setters
;  > = passed to core from shell
; <  = passed from core to shell
;variables in call to translator.c and core_main.f, in order:
  nbytes = 122L			;IDL long   = C int    = Fortran integer*4	 >
  fcblock = bytarr(nbytes)	;IDL byte   = C char   = Fortran character*1	 >
  mode = 0L			;IDL long					 >
  spin_per = 3.			;IDL float  = C float  = Fortran real*4		 >
  mjfm_tim = 1994056.00000000D	;IDL double = C double = Fortran real*8		 >
  param = fltarr(6)		;IDL float					<
  iqual = lonarr(6)		;IDL long					<
  spec_tim = 0D			;IDL double					<
  del_tim = 0.			;IDL float					<
  anal_complete = 0L		;IDL long   -> C int    -> Fortran logical*4	<
  reset = 0L			;IDL long   -> C int    -> Fortran logical*4     >
  iunit_brds=20L		;IDL long					 >
  iunit_mode=21L		;IDL long					 >
  iunit_modlvls=22L		;IDL long					 >
;C and IDL do not have type logical varibles so i converted int types
;end external call variable declarations
;value=bytarr(14)
	;value keyword to call_external will not be used
	;value = 0 -> pass by reference
	;value = 1 -> pass by value
	;only scalars may be passed by value
	;by default, variables and arrays are passed by reference
	;and numbers are passed by value

;get indices, scindx, of science data, 
;i.e., the indices of the mjf array, scidat, without the instr hk
  ind_scidat,scindx 	;idl procedure

;get mode1 tm map of science and genl hk data offsets into scindx
  mode1map,hkm1,fcblm1,vsm1,vdatc,sdatc  ;hkm1=genl hk offsets for mode1
                                         ;fcblm1=faraday cup offsets
                                         ;vsm1=veis/strl offsets
                                         ;vdatc=veis data index
                                         ;sdatc=strl data index

;open level zero file.
if NOT KEYWORD_SET(INFILE) then $
  lzfile='~wind/source/analysis/fvm/idl_shell/fcfast2lz.lz' $
else lzfile=INFILE
openr,lzunit,lzfile,/get_lun
print,'lzinfo:',lzunit,lzfile

;read the header file (discard for now)
readu,lzunit,hdr

junkfile='params.out'
openw,junkunit,junkfile,/get_lun

  REPEAT BEGIN
    readu,lzunit,mjf	;read a major frame

    ;decoded ihk mode byte from minor frame 37
    mode_tm = mjf.data(45*37)	
    scimode_ihk = get_bits(mode_tm,4,5)
    tmmode_ihk  = get_bits(mode_tm,6,2)
    tmrate_ihk  = get_bits(mode_tm,7,1)

;    if (tmmode_ihk eq 2) then BEGIN  
    s=size(fcblm1)
;    print,s
    FOR j=0,s(1)-1 do BEGIN
      IF lpr then BEGIN
        print,'faraday cup block # ',j
        print,'mjf.data byte offset of fc data in block # j' 
        print,fcoff,format='(10i6)'
        print,'original fc data'
        print,mjf.data(fcoff),format='(20z3.2)'
      ENDIF

      fcoff=scindx(fcblm1(j).offs+indgen(fcblm1(j).ln))

      ;insert new fc data
      for k=0,fcblm1(j).ln-1 do BEGIN
        offset=scindx(fcblm1(j).offs+k)
	fcblock(k)=mjf.data(offset)
        ;print,offset,mjf.data(offset),format='(i6,z3.2)'
      ENDFOR
      print,'*',format='(a1,$)'
      printf,junkunit,'*',format='(a1,$)'
      result=call_external('kp_core.so','_translator',$
			  fcblock,nbytes,mode,spin_per,mjfm_tim,param,$
			  iqual,spec_tim,del_tim,anal_complete,reset,$
			  iunit_brds,iunit_mode,iunit_modlvls)
      IF (result eq 1) then print,'Errors not cleared...'
      IF (anal_complete ne 0) then BEGIN
	print,''
	print, param 	;, format='(/,f8.2,g15.6,f13.6,f14.3,f14.3,f14.3)'
	printf,junkunit,''
	printf,junkunit,param
      ENDIF
    ENDFOR
  ENDREP until(eof(lzunit))

;start cleanup of core variables
  fcblock(0)='ff'XB
  fcblock(1)='00'XB
  reset=1
  result=call_external('kp_core.so','_translator',$
		        fcblock,nbytes,mode,spin_per,mjfm_tim,param,$
		        iqual,spec_tim,del_tim,anal_complete,reset,$
		        iunit_brds,iunit_mode,iunit_modlvls)
;end core cleanup

print,'Done'
;free_lun,junkunit
free_lun,lzunit
END


