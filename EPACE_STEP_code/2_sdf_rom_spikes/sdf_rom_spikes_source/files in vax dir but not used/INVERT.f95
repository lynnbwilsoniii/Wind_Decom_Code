c
c  This is a function used when IMSL is not available
c
c
c***********************************************
c
      logical function invert4(R,S)    
c
c***********************************************
c
c    calculates the inverse matrix of the 4x4 real matrix R
c
c    11/6/95 by J. Dwyer
c
      real R(4,4)    ! input matrix
      real  S(4,4)    ! inverse matrix of R   
      real Q(3,3)     ! submatrix
      real  detR, det3      ! determinant of R and Q
      integer  indexk, indexl
c 
      detR = 0.0
      i = 1
      do 30 j=1,4       
          indexk = 1
          do 20 k=1,4   
              indexl = 1          
              do 10 l=1,4
                  if ((k.ne.i).and.(l.ne.j)) then
                       Q(indexk,indexl)=R(k,l) 
                  end if
                 if (l.ne.j) indexl = indexl+1
10    continue
              if (k.ne.i) indexk = indexk+1
20    continue        
           detR = detR+((-1.0)**(i+j))*R(i,j)*det3(Q)
30    continue
  
c
      if (detR.ne.0.0) then
         invert4 = .true.
         do 100 i=1,4
            do 100 j=1,4
               indexk = 1              
               do 60 k=1,4   
                  indexl = 1          
                   do 50 l=1,4
                       if ((k.ne.i).and.(l.ne.j)) then
                           Q(indexk,indexl)=R(k,l) 
                       end if
                       if (l.ne.j) indexl = indexl+1
50               continue
                   if (k.ne.i) indexk = indexk+1
60           continue
               S(i,j) = ((-1.0)**(i+j))*det3(Q)/detR
100       continue 
c    
c        call check_inverse(R,S)  
c
         return  ! successful
      end if
c          
      invert4 = .false.  ! no inverse possible
      return
      end  ! end invert4
c
c
c
c
c************************************************
c
      real function det3(Q)
c
c************************************************
c
c  calculates the determinent of the 3x3 real matrix Q
c
      real  Q(3,3)
c
      det3 = Q(1,1)*(Q(2,2)*Q(3,3)-Q(3,2)*Q(2,3))-
     1          Q(2,1)*(Q(1,2)*Q(3,3)-Q(1,3)*Q(3,2))+
     1          Q(3,1)*(Q(1,2)*Q(2,3)-Q(1,3)*Q(2,2))
c
      return
      end  ! end det3
c
c
c
c
c***********************************************
c
      subroutine check_inverse(R,S)    
c
c***********************************************
c
c    calculates the inverse matrix of the 4x4 real matrix R
c
c    11/6/95 by J. Dwyer
c         fixed U(4,4)  5/1/96
c
      real  R(4,4)    ! input matrix
      real  S(4,4)    ! inverse matrix of R   
      real  U(4,4)     
 
      do 100 i=1,4
         do 100 j=1,4
             U(i,j) = 0.0
100     continue
c
      do 300 i=1,4
         do 300 j=1,4
             do 200 k=1,4
                 U(i,j) = U(i,j)+R(i,k)*S(k,j)
200     continue          
300     continue
c
      print *, ' '
      do 500 i=1,4
            print *, U(i,1), U(i,2), U(i,3), U(i,4)      
500     continue
c
      return
      end  ! check_inverse
c
