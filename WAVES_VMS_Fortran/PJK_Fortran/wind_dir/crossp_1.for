	SUBROUTINE CROSSP(A,B,C)
	DIMENSION A(3),B(3),C(3)
	A(1) = B(2)*C(3) - B(3)*C(2)
	A(2) = B(3)*C(1) - B(1)*C(3)
	A(3) = B(1)*C(2) - B(2)*C(1)
	RETURN
	END

