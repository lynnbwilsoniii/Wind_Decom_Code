! vms_time_constants_def.for - vax/vms 64-bit system times - a few reference
! points for time calculations.
!
! Note that absolute times are given as the number of 100-nano-second
! increments since 17-NOV-1858 00:00:00.00.
! Delta times are given as an unsigned 64-bit count (different
! than the vax/vms delta times) of 100-ns increments.
!
	integer*4	msec_per_sec
	integer*4	msec_per_minute
	integer*4	msec_per_hour
	parameter	(msec_per_sec=1000)
	parameter	(msec_per_minute=60*1000)
	parameter	(msec_per_hour=3600*1000)

	byte ui8zero(8)
	byte boy1982(8)  
	byte boy1994(8)  
	byte ui8year(8)  
	byte ui8Lyear(8) 
	byte ui8day(8)   
	byte ui8hour(8)  
	byte ui8min(8)   
	byte ui8sec(8)   
	byte ui8hsec(8)  
	byte ui8msec(8)  

	! delta time of a major frame in 2x (46sec) and 1x (92s)
	byte ui8_46s(8)  
	byte ui8_92s(8)  

	! delta time of a minor frame in 2x (46sec) and 1x (92s)
	byte ui8_d2x(8) 
	byte ui8_d1x(8)  

	data ui8zero	/'00'x,'00'x,'00'x,'00'x,'00'x,'00'x,'00'x,'00'x/
	data boy1982	/'00'x,'80'x,'79'x,'74'x,'94'x,'09'x,'8a'x,'00'x/
	data boy1994	/'00'x,'c0'x,'07'x,'9d'x,'c1'x,'7d'x,'97'x,'00'x/
	data ui8year	/'00'x,'c0'x,'c6'x,'78'x,'d1'x,'1e'x,'01'x,'00'x/
	data ui8Lyear	/'00'x,'80'x,'30'x,'a3'x,'9a'x,'1f'x,'01'x,'00'x/
	data ui8day	/'00'x,'c0'x,'69'x,'2a'x,'c9'x,'00'x,'00'x,'00'x/
	data ui8hour	/'00'x,'68'x,'c4'x,'61'x,'08'x,'00'x,'00'x,'00'x/
	data ui8min	/'00'x,'46'x,'c3'x,'23'x,'00'x,'00'x,'00'x,'00'x/
	data ui8sec	/'80'x,'96'x,'98'x,'00'x,'00'x,'00'x,'00'x,'00'x/
	data ui8hsec	/'a0'x,'86'x,'01'x,'00'x,'00'x,'00'x,'00'x,'00'x/
	data ui8msec	/'10'x,'27'x,'00'x,'00'x,'00'x,'00'x,'00'x,'00'x/
	data ui8_46s	/'00'x,'0b'x,'6b'x,'1b'x,'00'x,'00'x,'00'x,'00'x/
	data ui8_92s	/'00'x,'16'x,'d6'x,'36'x,'00'x,'00'x,'00'x,'00'x/
	data ui8_d2x	/'80'x,'13'x,'1c'x,'00'x,'00'x,'00'x,'00'x,'00'x/
	data ui8_d1x	/'00'x,'27'x,'38'x,'00'x,'00'x,'00'x,'00'x,'00'x/
