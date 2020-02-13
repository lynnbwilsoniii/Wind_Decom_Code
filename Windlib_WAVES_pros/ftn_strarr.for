C
C	$Id: ftn_strarr.for,v 1.2 1996/11/27 21:05:19 ali Exp $
C
C NAME:
C	FTN_STRING_ARRAY
C
C PURPOSE:
C       This Fortran function is used to demonstrate how IDL can
C       pass a string array to a Fortran routine, how to convert it to
C	a Fortran character array and then back to an IDL array. 
C
C CATEGORY:
C	Dynamic Link
C
C CALLING SEQENCE:
C      This function is called in IDL by using the following command.
C
C      IDL> result = CALL_EXTERNAL('FTN_ONLY$LOGICAL', 'FTN_STRING_ARRAY', $
C      IDL>      		   STRING_ARRAY, ARRAY_SIZE )
C
C INPUTS:
C
C	STRING_ARRAY:		An IDL string array
C
C	ARRAY_SIZE:		Number of elements in the array
C
C OUPUTS:
C	The value of each array element is changed and returned to IDL
C
C SIDE EFFECTS:
C       The values of the passed in variables are written to stdout
C
C RESTRICTIONS:
C       This example is setup to run using the VMS operating system.
C
C EXAMPLE:
C-----------------------------------------------------------------------------
C;; The following are the commands that would be used to call this
C;; routine in IDL.
C;;
C   STR_ARRAY = "IDL values: " + strtrim(sindgen(10),2))
C   result    = CALL_EXTERNAL('FTN_ONLY$LOGICAL', 'FTN_STRING_ARRAY', $
C      			       STR_ARRAY, ARRAY )
C
C-----------------------------------------------------------------------------
C
C MODIFICATION HISTORY:
C       Written October, 1993           KDB
C
C       Declare the Fortran function that is called by IDL via the
C       CALL_EXTERNAL Function.
C
C============================================================================
C$Subroutine FTN_STRING_ARRAY

	SUBROUTINE FTN_STRING_ARRAY(TEXT, DIM)

C PURPOSE:
C
C	Example VMS Fortran routine that passes an IDL string array into
C	a Fortran routine. Print the value of each IDL string and then
C	change the value to "String number nn".
C
C	Declare the passed in variables

	INTEGER*4	TEXT(0:*)
	INTEGER*4	DIM

C 	Declare a temporary Fortran string

	CHARACTER*100 	TEMP

	INTEGER*2	I	!Counter
	INTEGER*4	ILEN	

C	Write a header and output the passed in values.

	WRITE(*,10)
 10     FORMAT(1X,/,60('-') )

        WRITE(*,20)
 20     FORMAT(1X,'Inside Fortran Subroutine FTN_STRING_ARRAY ',/,1X,
     &            '(Called from IDL using CALL_EXTERNAL)',/)

 	WRITE(*,30)DIM
 30	FORMAT(1X,'The number of elmenets in the String Array: ',I4,/)

C	Use a Do loop to ouput the values of the string and change
C	the value of each element.

	DO I=0, DIM-1

C	   Call the subroutine GET_IDL_STRING to get the current
C	   string. The subroutine call is needed to "trick" Fortran
C	   that the value in TEXT is a character type variable.

	   CALL GET_IDL_STRING(TEMP, TEXT(I*2), ILEN)

C 	   Output the String Value
	
	   WRITE(*,40)I, TEMP(1:ILEN)
 40	   FORMAT(10X,'Element: ',I3,T30,'Value: ', A)

C	   Use the ENCODE command to transfer binary data to a character data
 		   
	   ENCODE(20, 1000, TEMP) I
1000	   FORMAT('String number ',I3)

C	   Now store the changed string into an IDL variable

	   CALL STORE_IDL_STRING(TEXT(I*2), TEMP(1:20))

	END DO

	WRITE(*,10)	!Draws a seperator line

C	Thats it
 
	RETURN
	END
	
C=============================================================================
C$Subroutine GET_IDL_STRING

	SUBROUTINE GET_IDL_STRING(RESULT, SOURCE, SOURCE_LEN)

C PURPOSE:
C	This subroutine is used to store an IDL string into a static
C	Fortran string. 
C
C I/O:
C
C	RESULT     - Destination string, passed by descriptor. A normal 
C		     Fortran string passed the normal way.
C
C	SOURCE     - IDL string descriptor.  Passed from the caller by 
C		     REFERENCE interpreted by this routine as a string 
C		     descriptor.
C
C	SOURCE_LEN - (output parameter) the number of characters in
C		     the original IDL string.
C
C 	Declare passed parameters

	CHARACTER*(*) 	RESULT
	CHARACTER*(*) 	SOURCE

	INTEGER*4 	SOURCE_LEN

C	Get the length of the source string

	SOURCE_LEN = len(SOURCE)

C	Now copy over the stings

	IF(SOURCE_LEN .le. 0)THEN		!Handle null strings
	   RESULT = ' '
	ELSE
	   RESULT = SOURCE
	ENDIF

C	Thats it

	RETURN
	END

C=============================================================================
C$Subroutine STORE_IDL_STRING

	SUBROUTINE STORE_IDL_STRING(RESULT, SOURCE)

C PURPOSE:
C	This routine is used to take a Fortran static string and 
C	store it as an IDL string. It makes use of IDL functions
C	that are linked to.  
C
C I/O:
C
C	RESULT 	- IDL string descriptor.  Passed from the caller by REFERENCE.
C		  interpreted by this routine as a string descriptor.
C		  It must either be an element of an IDL string array, or
C		  point to the value field of a string scalar.
C
C	SOURCE 	- Source string, normal Fortran string, passed the
C		  normal way.
C
C	Declare the passed in variables

	INTEGER*4 	RESULT(2)
	CHARACTER*(*) 	SOURCE

C	Create an IDL string using the IDL exportable routine
C	IDL_StrEnsureLength

	CALL IDL_StrEnsureLength(RESULT, %val(len(SOURCE)))  !make a new string

C	Use a subroutine call to trick Fortran that both variables 
C	are strings and then copy in the new string.

	CALL STORE_IDL_STRING1(RESULT, SOURCE)	! & copy data

	RETURN
	END

C=============================================================================
C$Subroutine STORE_IDL_STRING1

	SUBROUTINE STORE_IDL_STRING1(RESULT, SOURCE)

C PURPOSE:
C
C	Just a short routine that causes Fortran to interpret an integer
C	as a Fortran character.
C
C	Delcare the passed variables

	CHARACTER*(*) 	RESULT		!Integer converted to a Character
	CHARACTER*(*)	SOURCE

C	Now just copy over the string

	RESULT = SOURCE

C	Thats all

	RETURN

	END
