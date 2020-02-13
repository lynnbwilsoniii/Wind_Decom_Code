c Example of passing strings between IDL and Fortran:
c******************************************************************
c Called by the following IDL program:
c a = 'Input string: '+strtrim(sindgen(10),2)
c b = call_external('fort_string','text_example',a,n_elements(a),$
c	DEFAULT = 'DISKA:[IDL.CURRENT].exe')
c print,'Result is:'
c print, a
c end
c
c Linked by the command file:
c $link /share fort_string,sys$input/opt
c diska:[dist]idl.exe/share   !Or wherever your idl.exe is.
c universal = text_example
c $
c	
	subroutine text_example(text, dim)
C		VMS Example, Unix calling sequence is different (argc, argv).
C		Print the value of each IDL string, and then change
C		the value to "String number nn"
	INTEGER * 4 text(0:*), DIM
	CHARACTER * 100 temp
c
	type *,'Dim = ', dim
c
	DO i=0,dim-1
		call get_idl_string(temp, text(i*2), ilen)
		type *, i, ' ', temp(1:ilen)
		encode(20,1000, temp) i
1000		format('String number ',i3)
		call store_idl_string(text(i*2), temp(1:20))
		end do
	RETURN
	END
	

c******************************************************************
	SUBROUTINE GET_IDL_STRING(result, source, source_len)
C		Store a string from IDL into a static FORTRAN string.
C	Result = destination string, passed by descriptor. A normal Fortran
C		string passed the normal way.
C	Source = IDL string descriptor.  Passed from the caller by REFERENCE.
C		interpreted by this routine as a string descriptor.
C	Source_len = (output parameter) the number of characters in
C		the original IDL string.
C
	CHARACTER * (*) result
	CHARACTER * (*) source
	INTEGER * 4 Source_len

	source_len = len(source)
	if (source_len .le. 0) then		!Handle null strings
		result = ' '
	else
		result = source
	endif
	return
	end
c******************************************************************
	SUBROUTINE STORE_IDL_STRING(Result, Source)
C		Store a Fortran string (source) into an IDL string (result).
C	Result = IDL string descriptor.  Passed from the caller by REFERENCE.
C		interpreted by this routine as a string descriptor.
C		It must either be an element of an IDL string array, or
C		point to the value field of a string scalar.
C	Source = Source string, normal Fortran string, passed the
C		normal way.

	INTEGER*4 result(2)
	CHARACTER * (*) source

	CALL STR_ENSURE_LENGTH(result, %val(len(source)))  !make a new string
	CALL STORE_IDL_STRING1(result, source)	! & copy data
	RETURN
	END

	SUBROUTINE STORE_IDL_STRING1(result, source)
	CHARACTER *(*) result, source
	result = source
	return
	end
