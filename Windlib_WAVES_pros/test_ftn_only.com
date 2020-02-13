$!
$!	$Id: test_ftn_only.com,v 1.2 1996/11/26 23:43:51 ali Exp $
$!
$!  Set up command file environment
$!
$     SAVE_VERIFY = f$verify(0)
$     SET NOVERIFY
$     on ERROR then GOTO CLEANUP
$!
$! NAME:
$!	test_ftn_only
$!
$! PURPOSE:
$!	This VMS command procedure is used to test the IDL call_external
$!	function to Fortran Routines. This command file will build the 
$!	sharable image library and execute an IDL example program.
$!
$! CATEGORY:
$!	Dynamic Link
$!
$! CALLING SEQUENCE:
$!
$!	$ @TEST_C_ONLY [ TIDY ] [ CLEAN ]
$!
$! INPUTS:
$!
$!	TIDY	- Deletes OBJ files and Purges Executables
$!
$!	CLEAN	- Delets OBJ and Executables
$!
$! OUTPUTS:
$!	None.
$!
$! SIDE EFFECTS:
$!	IDL is started and an example program that uses CALL_EXTERNAL is
$!	executed.
$!
$! RESTRICTIONS:
$!	This procedure should be run in the same directory as the source
$!	test files are contained in.
$!
$!
$! MODIFICATION HISTORY:
$!	Written October, 1993		KDB
$!	
$! Test the value of P1. If it is equal to clean, then delete the created
$! object and executable files
$!
$  if(f$edit(P1,"collapse,trim,upcase") .eqs. "CLEAN")then GOTO CLEAN
$!
$! See if the user just want to get rid of the obj's
$!
$  if(f$edit(P1,"collapse,trim,upcase") .eqs. "TIDY")then GOTO TIDY
$!
$! Create a constant for the terminal bell. Just incase of an error
$!
$  BELL[0,32] = %x07
$!
$! If IDL_DIR is not defined exit. It needs to be defined to LINK
$!
$  if( f$trnlnm("IDL_DIR") .eqs. "")
$   then
$!
$   write sys$output "IDL_DIR is undefined, Unable to continue"
$   goto CLEANUP
$!
$  endif
$!
$! Check if the source code files are present in the current directory.
$! If not write a message and exit. Use the command procedure subroutine
$! FINDFILE.
$!
$ FILENAME= "FTN_ONLY.PRO"
$!
$ GOSUB FINDFILE
$!
$ FILENAME = "FTN_ONLY.FOR"
$!
$ GOSUB FINDFILE
$!
$ FILENAME = "FTN_STRARR.FOR"
$!
$ GOSUB FINDFILE
$!
$!
$! The compile and link method used is different between vax/vms and 
$! alpha openvms. Check which system we are on and perform the correct 
$! compile and link statements.
$!
$ if "''f$search("SYS$SYSTEM:VAXVMSSYS.PAR")'" .nes. "" 
$	then
$     set VERIFY
$!
$     fortran FTN_ONLY.FOR
$     fortran FTN_STRARR.FOR
$!
$     LINK /share FTN_ONLY, FTN_STRARR,				-
	SYS$INPUT/OPT/EXE=FTN_ONLY_SHARE.EXE 
	IDL_DIR:[BIN.BIN_VAX]IDL.EXE/SHARE
	UNIVERSAL = ftn_only
	UNIVERSAL = ftn_string_array
$!
$     set NOVERIFY
$!
$    else
$!
$     set VERIFY
$!
$     fortran /FLOAT=D_FLOAT FTN_ONLY.FOR
$     fortran /FLOAT=D_FLOAT FTN_STRARR.FOR
$!
$     LINK /share FTN_ONLY, FTN_STRARR, 			-
        SYS$INPUT/OPT /EXE=FTN_ONLY_SHARE.EXE
        IDL_DIR:[BIN.BIN_ALPHA]IDL.EXE/SHARE
	SYMBOL_VECTOR=(ftn_only=PROCEDURE)
	SYMBOL_VECTOR=(ftn_string_array=PROCEDURE)
$!
$     set NOVERIFY
$!
$endif
$!
$! Run the example program
$!
$  IDL
FTN_ONLY
$!
$CLEANUP:
$!
$ if SAVE_VERIFY then SET VERIFY
$!
$ EXIT
$!==========================================================================
$FINDFILE:
$!
$! Command procedure subroutine to see if a file is in the current directory
$! or not. If not, the command procedure is exited.
$!
$  if( f$search(FILENAME) .eqs. "")
$    then
$   write sys$output BELL
$   write sys$output "The file "+FILENAME+" is not in the current directory."
$   write sys$output "Please copy this file to the current directory."
$   write sys$output "Exiting."
$!
$   GOTO CLEANUP
$!
$  endif
$!
$  RETURN
$!
$!==========================================================================
$CLEAN:
$!
$! Clean up  the directory.
$!
$     if(f$search("FTN_ONLY.OBJ") .nes. "")then 			-
		DELETE /NOLOG FTN_ONLY.OBJ;*
$     if(f$search("FTN_STRARR.OBJ") .nes. "")then 		-
		DELETE /NOLOG FTN_STRARR.OBJ;*
$     if(f$search("FTN_ONLY_SHARE.EXE") .nes. "")then 		-
		DELETE /NOLOG FTN_ONLY_SHARE.EXE;*
$!
$     GOTO CLEANUP
$!==========================================================================
$TIDY:
$!
$! Tidy up the directory
$!
$     if(f$search("FTN_ONLY.OBJ") .nes. "")then                   -
                DELETE /NOLOG FTN_ONLY.OBJ;*
$     if(f$search("FTN_STRARR.OBJ") .nes. "")then             -
                DELETE /NOLOG FTN_STRARR.OBJ;*
$!
$     if(f$search("FTN_ONLY_SHARE.EXE") .nes. "")then		-
     		PURGE /NOLOG FTN_ONLY_SHARE.EXE
$!
$     GOTO CLEANUP
