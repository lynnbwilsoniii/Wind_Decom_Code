	PROGRAM CUBICHK
C
	COMPLEX ROOT(3)
C
	A = 0.
	B = -1.
	C = 0.
	CALL CUBIC(A,B,C,ROOT)
	PRINT*,ROOT
	A = -1.
	B = 1.
	C = -1.
	CALL CUBIC(A,B,C,ROOT)
	PRINT*,ROOT
	STOP
	END
