c This subroutine is called by SUM_ARRAY and has no
c IDL specific code.
c
       SUBROUTINE SUM_ARRAY1(array, n, sum)
       INTEGER*4 n
       REAL*4 array(n), sum
 
       sum=0.0
       DO i=1,n
       sum = sum + array(i)
       ENDDO
       RETURN
       END
