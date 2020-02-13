------------------------------------------------------------
README file: <RSI_Directory>
 	       <IDL_Directory>	
		  external
		    call_ext
                      Fortran
------------------------------------------------------------

This directory contains examples that demonstrate the usage of the
CALL_EXTERNAL function under VMS. The directory contains several
groups of files, distinguished by the filename extension.

Filename		File
Extension		Type
=========		===========================================
.txt			Informational files
.pro			IDL routines which call CALL_EXTERNAL
.for			Fortran language example routines
.com 			VMS/DCL command files that build and run examples

To run the examples routines, issue one of the following commands:

    To make and run the Fortran only examples:
        $ @test_ftn_only

The makefiles use the correct options for the standard compiler
for each supported system.

These files should be run in the directory that contains the C and/or
Fortran code that is used in these examples. If this code is not present
the examples will fail to operate. These examples also require that the
logical IDL_DIR be defined to the location of the IDL directory.

For further information on the use of CALL_EXTERNAL, consult
the IDL External Development Guide.
