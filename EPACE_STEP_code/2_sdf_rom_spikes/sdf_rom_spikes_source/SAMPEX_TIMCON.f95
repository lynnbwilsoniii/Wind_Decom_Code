	subroutine sampex_timcon(stime,yyyy,mo,da,hr,mn,se,doy,isotime)
c
c	subroutine to convert sampex time (seconds of 1992) to 
c	standard date, doy, etc.
c
c	gm	6/5/92
c		modifications:  
c		1/5/94		change error message to 
c				write on unit 6 instead of unit
c				5 to be compatible with batch jobs
c		6/8/95: 	extend end time to 2009
c		10/26/99	split implicit statement into two lines for
c				compatibility with new fortran compiler/gm
c				extend range to end of 2023 
c		20-Aug-2015	extend range to end of 2040 /gm
c
c	arguments:	stime 	= seconds since midnight 1/1/92
c				  i.e. stime = 1 corresponds to 
c					     00:00:01 1-Jan-92
c			yyyy  	= year (e.g. 1992)
c			mo	= month of year
c			da	= day of month	
c			hr	= hour of day
c			mn	= minute of hour
c			se 	= second of minute
c			doy 	= day of year (Jan. 1 = 1)
c			isotime = 20 character array  YYYY-MO-DA HR:MN:SE
c
c	Restrictions:   stime must be >0
c			no good beyond 12/31/2040 -- returns zeroes & 
c						     error message
c
	implicit double precision (a-h)
	implicit double precision (o-z)
	Integer*4 stime,yyyy,mo,da,hr,mn,se,doy
	character*20 isotime
	dimension amstart(12,2),yearstart(50),year(50)
	DATA aMSTART/1,32,60,91,121,152,182,213,244,274,305,335,
     *            1,32,61,92,122,153,183,214,245,275,306,336/,
     *  yearstart/0,366,731,1096,1461,1827,2192,2557,2922,3288,3653,
     *                      4018,4383,4749,5114,5479,5844,6210,6575,
     * 			    6940,7305,7671,8036,8401,8766,9132,9497,
     *                      9862,10227,10593,10958,11323,
     *              11688, 12054, 12419, 12784, 13149, 13515, 13880,
     *				14245, 14610, 14976, 15341, 15706, 16071, 16437,
     *				16802, 17167, 17532, 17898/
     *  year/1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,
     *                      2003,2004,2005,2006,2007,2008,2009,2010,
     *                      2011,2012,2013,2014,2015,2016,2017,2018,
     *                      2019,2020,2021,2022,2023,2024,2025,2026,
     *						2027,2028,2029,2030,2031,2032,2033,2034,
     *						2035,2036,2037,2038,2039,2040,2041/
c	check for time limit bounds
	if((stime.ge.0.).and.(stime.lt.(17898*86400))) goto 5
c	time out of bounds: zero results and type message
	write(6,1) stime
1	format(' illegal sampex_timcon() time: 'i10)
	yyyy=0
 	mo=0
	da=0
	hr=0
	mn=0
	se=0
	doy=0
	goto 200
c
c	calculate days since 1/1/92
c
5	dayno=dfloat(stime)/86400.d0
c	
c	find year number
c
	do 10 i=1,50
	iyear=i
	if((dayno.ge.yearstart(i)).and.(dayno.lt.yearstart(i+1)))
     *  goto 11
10	continue
11	yyyy=year(iyear)
c
c	check for leap year
c
	ileap=1
	if(mod(yyyy,4).eq.0) ileap=2
c
c	calculate day of year
c
	adoy=dayno - yearstart(iyear) +1.d0
	doy = adoy
c
c	calculate month
c
	do 15 i=1,11
	mo=i
	if(adoy.lt.amstart(i+1,ileap)) goto 20
15	continue
	mo=12
20	da = adoy - amstart(mo,ileap) + 1.d0
c
c	calculate hr,mn,se
c
c	seconds of the day:
	secday = dfloat(stime) - 86400.d0*dint(dayno)
	ahr = secday/3600.d0
 	hr = ahr
	amn = (secday - dint(ahr)*3600.d0)/60.d0
	mn=amn
	se = secday - dint(ahr)*3600.d0 - dint(amn)*60.d0
c
c	write out isotime
c
200	write(isotime,50) yyyy,mo,da,hr,mn,se
50	format( i5'-'i2.2'-'i2.2,i3.2':'i2.2':'i2.2)
	return
	end
