	program lisfix
c
c	takes a .lis file and removes the numbers at the front, as a first 
c	step in converting it to a .for file
c
	character*100 line
c
	open(unit=44,name='fftpro.lis',status='old',readonly)
	open(unit=45,name='fftpro.for',status='new')
c
 100	continue
	read(44,1044,end=200) iq,line
	print*,'iq',iq
	print 1045, line(9:100)
	iqt = iq+9
	write (45,1045) line(9:iqt)
 1044	format(q,A)
 1045	format(A)
 	go to 100
 200	continue
	close(unit=44)
	close(unit=45)
	stop
	end
