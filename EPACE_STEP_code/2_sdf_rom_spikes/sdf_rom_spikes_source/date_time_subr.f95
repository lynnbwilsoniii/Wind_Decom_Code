	subroutine date(datew)
c	used for porting old vax code to Mac  /gm   3/28/08
	integer values(8)
	character*3 month(12)
	character da,tm,zo
	character datew*9
	character timew*8
	data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     *     'Sep','Oct','Nov','Dec'/
10	call date_and_time(da,tm,zo,values)
c	values(1) = ccyy
c	values(2) = MO
c	values(3) = DD
c	values(4) = zone
c	values(5) = HH
c	values(6) = MM
c	values(7) = sec
c	values(8) = milliseconds
c	e.g.,    2008  3  28 -240  16  19  44  519
	write(datew,15) values(3), month(values(2)), mod(values(1),100)
15	format( i2.2,'-',a3,'-',i2.2) 
	return
	entry time(timew)
	call date_and_time(da,tm,zo,values)
	write(timew,16) (values(i),i=5,7)
16	format(2(i2.2,':'),i2.2)
	end
