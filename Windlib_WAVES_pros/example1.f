
       SUBROUTINE SUM_ARRAY(argc, argv)  !Called by IDL
       INTEGER*4 argc, argv(*)           !Argc and Argv are integers

       j = LOC(argc)         !Obtains the number of arguments (argc)
                      !Because argc is passed by VALUE.

c Call subroutine SUM_ARRAY1, converting the IDL parameters
c to standard FORTRAN, passed by reference arguments:

       CALL SUM_ARRAY1(%VAL(argv(1)), %VAL(argv(2)), %VAL(argv(3)))
       RETURN
       END

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
