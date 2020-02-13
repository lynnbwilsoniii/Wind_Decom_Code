
;demonstrates the efficiency of matrix operations in place of do-loops 

check_timing=2

if check_timing eq 1 then begin
  l=80 & m=1000 & n=1000 & lpr=0 & print,'comparing cpu times'
endif else if check_timing eq 2 then begin
  l=5 & m=10 & n=20 & lpr=1 & print,'comparing cpu times' 
endif else begin
  l=2 & m=4 & n=3 & lpr=1;increase size of arrays for comparing execution times
endelse

a=0.1+indgen(l)*0.01
b=1.0+indgen(m)*0.1
c=2.0+indgen(n)*0.2
;stop

ic=0
start:
ic=wmenu(['select case for comparing (a)looping vs (b)matrix operation',$
          '   2-loop nesting invloving sums',$
          '   2-loop nesting invloving multiplication',$
          '   3-loop nesting invloving sums',$
          '   3-loop nesting invloving multiplication',$
          '   Given d1(i,j,k), multiply by an array with g(j) terms',$
          '   Given d1(i,j,k), multiply by an array with f(i)*g(j) terms',$
          'quit'],title=0,init=ic+1)
                
case ic of

1: begin   ;I. Two-loop nest involving summation.
     print,'doing loops....'
     d1=fltarr(m,n)
     for j=0,m-1 do for k=0,n-1 do d1(j,k)=b(j)+c(k)
     print,'I.a.end loop.' & if lpr then print,d1

     ;Equivalent matrix operation:
     print,'doing matrix operation....'
     d2=b # replicate(1,n) + replicate(1,m) # c
     print,'I.b.end matrix.' & if lpr then print,d2

     ;compare
     if lpr then print,'difference ' & if lpr then print,d1-d2 & print,' '
   endcase

2:  begin   ;II. Two-loop nest involving multiplication.
      print,'doing loops....'
      d1=fltarr(m,n)
      for j=0,m-1 do for k=0,n-1 do d1(j,k)=b(j)*c(k)
      print,'II.a.end loop.' & if lpr then print,d1

      ;Equivalent matrix operation:
      print,'doing matrix operation....'
      d2=b # c
      print,'I.b.end matrix.' & if lpr then print,d2

     ;compare
     if lpr then print,'difference ' & if lpr then print,d1-d2 & print,' '
    endcase

3:  begin    ;III. Three-loop nest involving summation.
      print,'doing loops....'
      d1=fltarr(l,m,n)
      for i=0,l-1 do for j=0,m-1 do for k=0,n-1 do d1(i,j,k)=a(i)+b(j)+c(k)
      print,'III.a.end loop.' & if lpr then print,d1

      ;Equivalent matrix operation:
      print,'doing matrix operation....'
      bc=fltarr(m*n)
      bc(*)=b # replicate(1,n) + replicate(1,m) # c
      d2=fltarr(l,m,n)
      d2(*,*,*)=a # replicate(1,m*n) + replicate(1,l) # bc
      print,'III.b.end matrix.' & if lpr then print,d2

      ;compare
      if lpr then print,'difference ' & if lpr then print,d1-d2 & print,' '
    endcase

4:  begin   ;IV. Three-loop nest involving multiplication.
      print,'doing loops....'
      d1=fltarr(l,m,n)
      for i=0,l-1 do for j=0,m-1 do for k=0,n-1 do d1(i,j,k)=a(i)*b(j)*c(k)
      print,'IV.a.end loop.' & if lpr then print,d1

      ;Equivalent matrix operation:
      print,'doing matrix operation....'
      bc=fltarr(m*n)
      bc(*)=b # c
      d2=fltarr(l,m,n)
      d2(*,*,*)=a # bc
      print,'IV.b.end matrix.' & if lpr then print,d2

      ;compare
      if lpr then print,'difference ' & if lpr then print,d1-d2 & print,' '
    endcase
    

5:  begin   ;V. Given d1(i,j,k), multiply by an array with g(j) terms:
      print,'doing loops....'
      d1=fltarr(l,m,n)
      for i=0,l-1 do for j=0,m-1 do for k=0,n-1 do d1(i,j,k)=a(i)*b(j)*c(k)
      e1=fltarr(l,m,n)
      for i=0,l-1 do for j=0,m-1 do for k=0,n-1 do     $
         e1(i,j,k)=(j+1)*d1(i,j,k) 
      print,'V.a.end loop.' & if lpr then print,e1

      ;Equivalent matrix operation:
      print,'doing matrix operation....'
      e2=fltarr(l,m,n)
      etmp=replicate(1,l) # (indgen(m)+1)
      e2(*,*,*)=(etmp(*) # replicate(1,n)) * d1
      print,'V.b.end matrix.' & if lpr then print,e2

      ;compare
     if lpr then print,'difference ',e1-e2 & if lpr then print,' '
    endcase    

6:  begin   ;V. Given d1(i,j,k), multiply by an array with f(i)*g(j) terms:
      print,'doing loops....'
      d1=fltarr(l,m,n)
      for i=0,l-1 do for j=0,m-1 do for k=0,n-1 do d1(i,j,k)=a(i)*b(j)*c(k)
      e1=fltarr(l,m,n)
      for i=0,l-1 do for j=0,m-1 do for k=0,n-1 do     $
         e1(i,j,k)=(i+1)*(j+1)*d1(i,j,k) 
      print,'V.a.end loop.' & if lpr then print,e1

      ;Equivalent matrix operation:
      print,'doing matrix operation....'
      e2=fltarr(l,m,n)
      etmp=(indgen(l)+1) # (indgen(m)+1)
      e2(*,*,*)=(etmp(*) # replicate(1,n)) * d1
      print,'V.b.end matrix.' & if lpr then print,e2

      ;compare
     if lpr then print,'difference ',e1-e2 & if lpr then print,' '
    endcase

7: stop,'end'

else:

endcase
goto,start
end
