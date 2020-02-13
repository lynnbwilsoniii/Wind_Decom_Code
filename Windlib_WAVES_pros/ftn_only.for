C
C	$Id: ftn_only.for,v 1.1 1993/11/16 23:36:16 idl Exp $
C
C NAME:
C 	FTN_ONLY
C
C PURPOSE:
C
C	This Fortran function is used to demonstaight how IDL can
C	pass variables to a Fortran routine and then recieve these
C	variables once they are modified. 
C
C CATEGORY:
C
C	Dynamic Link
C
C CALLING SEQUENCE:
C
C      This function is called in IDL by using the following command.
C	
C      IDL> result = CALL_EXTERNAL('FTN_ONLY$LOGICAL', 'FTN_ONLY',    $
C      IDL>      bytevar, shortvar, longvar, floatvar, doublevar,     $
C      IDL>      str_var, floatarr, n_elments(floatarr),/F_VALUE ) 
C
C INPUTS:
C
C      bytevar:        A scalar byte variable
C
C      shortvar:       A scalar short integer variable
C
C      longvar:        A scalar long integer variable
C
C      floatvar:       A scalar float variable
C
C      doublevar:      A scalar float variable
C
C      str_var:	       A string variable
C
C      floatarr:       A floating point array
C      
C      cnt:	       Number of elements in the array.
C
C OUTPUTS:
C	The value of each variable is squared and the sum of the 
C	array is returned as the value of the function. 
C
C SIDE EFFECTS:
C	The values of the passed in variables are written to stdout	
C
C RESTRICTIONS:
C	This example is setup to run using the VMS operating system.
C
C EXAMPLE:
C-----------------------------------------------------------------------------
C;; The following are the commands that would be used to call this
C;; routine in IDL. 
C;;
C        byte_var        = 1b
C        short_var       = 2
C        long_var        = 3l
C        float_var       = 4.0
C        double_var      = 5d0
C	 str_var	 = "SIX"
C	 floatarr	 = findgen(30)*!pi
C
C        result = CALL_EXTERNAL('FTN_ONLY$SHARE', 'FTN_ONLY',     	$
C                        byte_var, short_var, long_var, float_var,      $
C                        double_var, str_var, floatarr, 		$
C			 n_elments(floatarr),	/F_VALUE )
C
C-----------------------------------------------------------------------------
C
C MODIFICATION HISTORY:
C	Written October, 1993		KDB
C
C 	Declare the Fortran function that is called by IDL via the 
C	CALL_EXTERNAL Function.
C
C=============================================================================
C$Function FTN_ONLY

      	REAL*4 FUNCTION FTN_ONLY(BYTEVAR, SHORTVAR, LONGVAR,
     &			FLOATVAR, DOUBLEVAR, STRVAR, FLOATARR, N)
	
C  	Delcare the passed in variables

        LOGICAL*1               BYTEVAR         !IDL byte

        INTEGER*2               SHORTVAR        !IDL integer

        INTEGER*4               LONGVAR         !IDL long integer
	INTEGER*4		N		!Size of array

	CHARACTER*(*)		STRVAR
        REAL*4                  FLOATVAR        !IDL float
	REAL*4			FLOATARR(N)	!IDL float array
	
        DOUBLE PRECISION       			DOUBLEVAR       !IDL double

C	Local Variable

	INTEGER			I		!Counter
	
	REAL*4			SUM		

C       Write the values of the variables that were passed in to
C       Fortran from IDL.

        WRITE(*,10)
 10     FORMAT(1X,/,52('-') )

        WRITE(*,20)
 20     FORMAT(1X,'Inside Fortran function ftn_only ',
     &            '(Called from IDL using CALL_EXTERNAL)',/)

        WRITE(*,30)
 30     FORMAT(1X,'Scalar Values Passed in From IDL:')

        WRITE(*,100)BYTEVAR
 100    FORMAT(10X,'BYTE Parameter:',T50,I4)

        WRITE(*,110)SHORTVAR
 110    FORMAT(10X,'SHORT Parameter:',T50,I4)

        WRITE(*,120)LONGVAR
 120    FORMAT(10X,'LONG Parameter:',T50,I4)

        WRITE(*,130)FLOATVAR
 130    FORMAT(10X,'FLOAT Parameter:',T50,F4.1)

        WRITE(*,140)DOUBLEVAR
 140    FORMAT(10X,'Double Parameter:',T50,F4.1)

	WRITE(*,150)strvar
 150    FORMAT(10X,'String Parameter:',T50,A)

 	WRITE(*,160)
 160	FORMAT(10X,'Float Array:')

	WRITE(*,170)(I, FLOATARR(I), I=1, N)
 170	FORMAT(15X,'Element ',I3,', Value: ',T47, F7.2)

        WRITE(*,10)     !Prints a line across the page

C       Perform a simple operation on each varable (square them).

        BYTEVAR   = BYTEVAR   * BYTEVAR
        SHORTVAR  = SHORTVAR  * SHORTVAR
        LONGVAR   = LONGVAR   * LONGVAR
        FLOATVAR  = FLOATVAR  * FLOATVAR
        DOUBLEVAR = DOUBLEVAR * DOUBLEVAR

C 	Now sum the array

	SUM = 0.0

	DO I = 1, N 

	   SUM = SUM + FLOATARR(I)

	ENDDO	

C	Set the function equal to the sum

        FTN_ONLY = SUM 

C       Thats it, return to the calling routine

        RETURN

        END


