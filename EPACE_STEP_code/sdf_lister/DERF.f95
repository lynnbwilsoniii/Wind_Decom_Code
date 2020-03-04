C               N.B.S  APPLIED MATHEMATICS SERIES,
C                 PUBLICATION 55
C               SEE P 299, SERIES NUMBER 7.1.26
C               ACCURACY:  ERROR LT 1.5 X 10**(-7)
C
C               FOLLOWS IBM ERROR FUNCTION HANDLING--
C               I.E. ERF(-X) = -ERF(X)
C                    RANGE OF VALUES+  -1 TO +1
C
C
c       18-May-94       note! in call to DERF, "x" is NOT in
c                       units of standard deviations!  To get the
c                       integral of a Gaussian distribution out
c                       of this, divide "x" by sqrt(2.) !
c
c
c***********************************************
c
      real function derf(x)
c
c***********************************************
c
c    modified 3/22/96 by J. Dwyer for sdf_lister
c
      IMPLICIT REAL*8 (A-H,O-Z)
      XLC=X
      IF(XLC.LT.0.D0) XLC=-X
      T= 1.D0/(1.D0 + (0.3275911D0)*XLC)
      T2= T*T
      T3= T*T2
      T4= T*T3
      T5= T*T4
      EZX=DEXP(-XLC*XLC)
      DERF = 1.D0 -
     *             ((0.254829592D0)*T
     *            - (0.284496736D0)*T2
     *            + (1.421413741D0)*T3
     *            - (1.453152027D0)*T4
     *            + (1.061405429D0)*T5)*EZX
      IF(X.LT.0.D0) DERF=-DERF
      RETURN
      END     ! end derf
