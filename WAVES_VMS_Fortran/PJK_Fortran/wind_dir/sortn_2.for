      SUBROUTINE SORTN(XS,FOLLOW1,NS)
C
      INTEGER*4 FOLLOW1(1)
C
C     ORDER X COORDS IN INCREASING ORDER
C
      DO 11 N = 1,NS-1
      N1 = N + 1
      DO 12 M = N1,NS
      IF((XS(M)-XS(N)).GT.0.) GO TO 12
C     EXCHANGE
      XT = XS(N)
      XS(N) = XS(M)
      XS(M) = XT
      IXT = FOLLOW1(N)
      FOLLOW1(N) = FOLLOW1(M)
      FOLLOW1(M) = IXT
   12 CONTINUE
   11 CONTINUE
      RETURN
      END

