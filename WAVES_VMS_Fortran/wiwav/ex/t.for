! t.for - tests 

	integer*4	i
	byte	a(8), b(8), c(8), d(8), e(8)

	data a /'a1'x,'b2'x,'c3'x,'d4'x,'e5'x,'f6'x,'g7'x,'08'x/
	data b /'A1'x,'B2'x,'C3'x,'D4'x,'E5'x,'F6'x,'G7'x,'08'x/
	data c /'a1'X,'b2'X,'c3'X,'d4'X,'e5'X,'f6'X,'g7'X,'08'X/
	data d /'A1'X,'B2'X,'C3'X,'D4'X,'E5'X,'F6'X,'G7'X,'08'X/
	data e /'q1'X,'w2'X,'x3'X,'y4'X,'Z5'X,'R6'X,'T7'X,'08'X/

  1	format(1x,a8,8(1x,z2.2))

	type 1, 'a=', (a(i), i=1,8)
	type 1, 'b=', (b(i), i=1,8)
	type 1, 'c=', (c(i), i=1,8)
	type 1, 'd=', (d(i), i=1,8)
	type 1, 'e=', (e(i), i=1,8)

	end
