	PROGRAM TDSCALTEST
C
	REAL DATA(4)
C
	DO IK = 0,255
	    ISPS = 0
	    DATA(1) = TDSCAL(1,ISPS,IK)
	    DATA(2) = TDSCAL(2,ISPS,IK)
	    ISPS = 1
	    DATA(3) = TDSCAL(1,ISPS,IK)
	    DATA(4) = TDSCAL(2,ISPS,IK)
	  WRITE(55,*) IK,DATA(1),DATA(2),DATA(3),DATA(4)
	ENDDO
	STOP
	END

