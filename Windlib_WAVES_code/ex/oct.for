! oct.for - 
	program oct 
	integer*4	i

  1	format(1x,a,1x,o8.8)

	i =     '0777'o
	type 1, '0777 o ', i

	i =     '1000777'o
	type 1, '1000777 o ', i

	i =     '01020304'o
	type 1, '01020304 o ', i

	stop
	end
