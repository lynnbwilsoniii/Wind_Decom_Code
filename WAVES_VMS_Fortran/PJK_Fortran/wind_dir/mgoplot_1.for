	SUBROUTINE MGPLOT(IHARD,NL,PDATA)
C
	DIMENSION PDATA(500,3)
	CHARACTER*20 CH(20)
C
	CH(1) = 'TERMINAL 3'
	IF(IHARD.EQ.1) CH(1) = 'PRINTER 6'
	CH(2) = 'LINES 1 359'
	CH(3) = 'XCOLUMN 1'
	CH(4) = 'YCOLUMN 2'
	CH(5) = 'LIMITS'
	CH(6) = 'CONNECT'
	CH(7) = 'BOX'
	CH(8) = 'XLABEL ANGLE TO SUN'
	CH(9) = 'ID'
	CH(10) = 'END'
      IF(IHARD.EQ.1) THEN
	CH(10) = 'HARDCOPY'
	CH(11) = 'END'
      ENDIF
	CALL MONGO(10+IHARD,CH,500,3,PDATA)
      RETURN
      END

