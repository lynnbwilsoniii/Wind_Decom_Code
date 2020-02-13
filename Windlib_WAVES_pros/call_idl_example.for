c
c	This example shows how IDL can be called
c	from Fortran Programs.
c To link it:
c $ Define IDL_EXE IDL_DIR:IDL.EXE
c $ LINK CALL_IDL_EXAMPLE, SYS$INPUT/OPT
c IDL_EXE/SHARE
c $
c
c
c ********************************************

	integer * 4 function c_string(str)
	character *(*) str
c
c	Return the address of a null terminated C string, given a Fortran
c	string.  Trailing blanks are removed.  Note: only one static area
c	is used to contain the result so this result can not be used
c	for more that one string at a time.  Maximum string length = 80.
c
	Parameter MAXLEN = 80
	character * (MAXLEN+1) rslt
	integer * 4 str$trim
	include '($SSDEF)'
c
c	  			Copy & get length
	if (ss$_normal .ne. str$trim(rslt(1:MAXLEN), str, len)) then
		type *, 'C_STRING: String too long'
		return
		endif
	rslt(len+1:len+1) = char(0)    !Null terminate it
	c_string = %loc(rslt)		!Return address of text.
	return
	end	

c ********************************************

	subroutine pass_num_idl(nelements, vname, array, type)
c	Create and store data in an IDL array.
c		Nelements = # of array elements.
c		Vname = variable name, string, MUST be upper case.
c		Array = the array to which the IDL variable is to be set.
c		Type = data type code.
c	 	 1 for byte, 2 for short int, 3 for long, 4 for float,
c		 5 for double, 6 for complex.
c
	integer type
	character *(*) vname
	character * 10 funct_names(6)
	character * 80 command
c		IDL names to create the data we need.
	data funct_names / 'BYTARR', 'INTARR', 'LONARR', 'FLTARR', 
	1	'DBLARR', 'COMPLEXARR' /
	integer element_size(6), dims(8)
c				The size of each type:
	data element_size / 1, 2, 4, 4, 8, 8/
	integer * 4 var, pd
	integer * 4 ur_main, get_var_addr, for_getdims

	include 'idl_dir:[external]idldef.inc'

	if (type .lt. 1 .or. type .gt. 6) then
		type *,'PASS_NUM_IDL: bad type code'
		return
		endif

c	Create the array: (IDL statement is var = XXARR(nelements, /nozero)
c
	encode(80, 1000, command) vname, funct_names(type), nelements
1000	format(a,'=',a,'(',i,', /NOZERO)')
c		Send command to idl:
	istat = ur_main(%val(MAIN_EXECUTE), %val(1), 
	1	c_string(command))

c		Get the variable's pointer, and then the data pointer.
c
	var = get_var_addr(%val(c_string(vname))) !Get variable address.
	call for_getdims(%val(var), nd, dims, pd)  !get data addr
c		Copy the data
	call ots$move3(%val(element_size(type) * nelements),
	1	array, %val(pd))
	return
	end

c ********************************************
	program main
c
	include 'idl_dir:[external]idldef.inc'

	integer * 4 ur_main
	real * 4 sine(100)
c
	do i=1,100		!Make a sine wave
		sine(i) = sin(i /5.)
		end do
c
c		Just for the heck of it, issue some IDL statements:
c
	istat = ur_main(%val(MAIN_EXECUTE), %val(1), c_string('PRINT, 1'))
	istat = ur_main(%val(MAIN_EXECUTE), %val(1), c_string('PRINT, 2'))
c
c	Pass the Sine array to the IDL array Y.
c
	call pass_num_idl(100, 'Y', sine, 4)  !Pass to IDL
c		Plot the IDL variable Y:
	istat = ur_main(%val(MAIN_EXECUTE), %val(1), c_string('PLOT, Y'))
	pause 'Type CONTINUE to resume'
c			And exit
	istat = ur_main(%val(MAIN_EXIT), %val(0), %val(0))
	end
