;;	$Id: ftn_only.pro,v 1.2 1996/11/26 23:43:51 ali Exp $
;;
;; NAME:
;;	ftn_only.pro
;;
;; PURPOSE:
;;	This IDL procedure is used to demonstrate how to pass IDL variables
;;	to a VMS Fortran routines via the CALL_EXTERNAL function. 
;;
;; CATEGORY:
;;	Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This function is called in IDL by using the following command
;;
;;	IDL> ftn_only
;;
;; INPUTS:
;;	None.
;;
;; OUTPUTS:
;; 	None.
;;
;; SIDE EFFECTS:
;;	The value of IDL variables are written to standard output 
;;	before, during and after calls using CALL_EXTERNAL.
;;
;; RESTRICTIONS:
;;	This procedure assumes that all of the sharable libraries are 
;;	present. If they are not they can be built using the provided
;;	command file.
;;
;;
;; MODIFICATION HISTORY:
;;	Written October, 1993		KDB
;;
;;===========================================================================

	PRO FTN_ONLY

;;	Verify that the library files are already built. If they are not,
;;	return and write a message.

	SHARE= findfile('FTN_ONLY_SHARE.EXE;*',COUNT=CNT)

	IF(CNT eq 0)THEN BEGIN

	  MESSAGE,"The library file, FTN_ONLY_SHARE.EXE"+ $
                ", is not present. The library file must be built.",/CONTINUE

	  RETURN

	ENDIF

;;	Set up a logical that defines the location of the shareable 

        SETLOG, "FTN_ONLY$SHARE", SHARE(0)

;;      Declare the variables that are passed into the Fortran function
;;      FTN_ONLY

        BYTE_VAR        = 2B
        SHORT_VAR       = 3
        LONG_VAR        = 4L
        FLOAT_VAR       = 5.0
        DOUBLE_VAR      = 6D0
	STR_VAR		= 'Seven'
	FLOAT_ARRAY     = findgen(10)*!pi

;;	Lets print the parameters that we are going to pass into the 
;;	Fortran function. 

 	PRINT,""
	PRINT,"====================================================="
	PRINT,"Inside IDL: Before the call to the Fortran function FTN_ONLY
	PRINT,""
	PRINT,"Values of variables that will be passed in:"
	PRINT,""
	PRINT,"         IDL BYTE Variable:          ", BYTE_VAR, 	$
	FORMAT="(A,I4)"
	PRINT,"         IDL INT Variable:           ", SHORT_VAR,	$
	FORMAT="(A,I4)"
	PRINT,"         IDL LONG Variable:          ", LONG_VAR,	$
	FORMAT="(A,I4)"
    	PRINT,"         IDL FLOAT Variable:         ", FLOAT_VAR,	$
	FORMAT="(A,F4.1)"
	PRINT,"         IDL DOUBLE Variable:        ", DOUBLE_VAR,	$
	FORMAT="(A,F4.1)"
	PRINT,"         IDL STRING Variable:        ", STR_VAR, 	$
	FORMAT="(A,A)"
	PRINT,"         IDL FLOAT Array:"
        
	FOR I =0, n_elements(FLOAT_ARRAY)-1 DO 	$
	    PRINT,FLOAT_ARRAY(I), FORMAT="(40x,F6.2)"

	PRINT,""
	PRINT,"Calling the Fortran Function FTN_ONLY via CALL_EXTERNAL
	PRINT,"====================================================="

	PRINT," "

;;	The Fortran function will square the scalar values and return
;;	the SUM of the float array.

	result = CALL_EXTERNAL(	$
		'FTN_ONLY$SHARE', 	$ ;Logical pointing to sharable lib
	        'FTN_ONLY',	 	$ ;Desired routine name
                BYTE_VAR, 		$ 
		SHORT_VAR, 		$
		LONG_VAR, 		$
		FLOAT_VAR,       	$
                DOUBLE_VAR, STR_VAR,	$
		FLOAT_ARRAY, 		$ ;IDL float array
		n_elements(FLOAT_ARRAY),$ ;Number of elements in array
		/F_VALUE) ; Indicates that a float value will be returned

;;	Now print out the results

        PRINT,""
        PRINT,"====================================================="
	PRINT,"Inside IDL: Results of Fortran function FTN_ONLY"
	PRINT,"            FTN_ONLY squared the scalar variables:"
	PRINT,""
	PRINT,"Results:"
        PRINT,"         Squared BYTE Variable:           ", BYTE_VAR,	$
	FORMAT="(/A,I6)"
        PRINT,"         Squared INT Variable:            ", SHORT_VAR,	$
	FORMAT="(A,I6)"
        PRINT,"         Squared LONG Variable:           ", LONG_VAR,	$
	FORMAT="(A,I6)"
        PRINT,"         Squared FLOAT Variable:          ", FLOAT_VAR,	$
	FORMAT="(A,F6.1)"
        PRINT,"         Squared DOUBLE Variable:         ", DOUBLE_VAR,	$
	FORMAT="(A,F6.1,/)"
	PRINT,"         Returned Sum of the Float Array: ", RESULT, $
	FORMAT="(A,F6.2,/)"
        PRINT,"         Sum using IDL function TOTAL:    ", 	  	$
			total(FLOAT_ARRAY),FORMAT="(A,F6.2,/)"

        PRINT,"====================================================="

	PRINT," "

;;	Now set up the values that will be passed to the Fortran function 
;;	ftn_string_array. String array will take in a string array, change 
;;	its values and return.

	ARRAY_VAR = 'IDL String Element: ' + strtrim(sindgen(12),2)
        ARRAY_SIZE = n_elements(ARRAY_VAR)
 
  	status = CALL_EXTERNAL( $
			'FTN_ONLY$SHARE', $ ;Logical pointing to shareable lib
			'ftn_string_array',$ ;Routine name (entry point)
                        ARRAY_VAR, 	  $ ;IDL variables
			ARRAY_SIZE)

        PRINT," "
        PRINT,"===================================================="
	PRINT,"Inside IDL: Results of the Fortran function FTN_STRING_ARRAY"
	PRINT,""
        
	FOR I = 0, ARRAY_SIZE-1 DO $
	   PRINT,I,ARRAY_VAR(I), 	   $
	   FORMAT="('     Element: ',I2,t30,'Value: ',A)"

        PRINT,"===================================================="     
	PRINT,""
        PRINT,"Fortran only CALL_EXTERNAL test complete."

	RETURN

	END
