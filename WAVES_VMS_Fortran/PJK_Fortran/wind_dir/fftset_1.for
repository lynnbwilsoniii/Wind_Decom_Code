      SUBROUTINE FFTSET(NP2)
      COMMON /WTABLE/ M,N,NV2,NM1,WSAVE(24)
C
      DATA PI /3.14159265/
C
      M = NP2
      N = 2**M
      NV2 = N/2
      NM1 = N-1
      LE = 1
      DO 20 L = 1,M
      WSAVE(2*L-1) = COS(PI/LE)
      WSAVE(2*L) = SIN(PI/LE)
      LE = 2*LE
  20  CONTINUE
      RETURN
      END

