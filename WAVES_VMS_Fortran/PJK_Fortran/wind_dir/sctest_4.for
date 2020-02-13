	PROGRAM SCTEST
C
	INTEGER YYMMDD,HHMMSS,YYYY,HH,MM,DDW,OK,OKT,TMCHANNEL,CH
	INTEGER YYYYW,YYMMDDW,HHW,MMW,NDAY
	REAL*8 SCET8,SCET
C
	SCETDAY = 4702.
	SCET8 = DBLE(SCETDAY)
	YYMMDD = 19961116
	HHMMSS = 093600
C
	print*,'input time',scetday,yymmdd,hhmmss
	YYYY = YYMMDD/10000.
	MON = (YYMMDD - 10000*YYYY)/100
	NDAY = MOD(YYMMDD,100)
	HH = HHMMSS/10000.
	MM = (HHMMSS - 10000.*HH)/100.
	SS = MOD(HHMMSS,100)
	SCET8 = DBLE(SCETDAY) + HH/24.D00 + MM/1440.E00 + SS/86400.D00
	print*,'calc scet8,yymmdd ',scet8,yymmdd
	call w_ur8_to_ymd(scet8,yyyyw,monw,ddw,hhw,mmw,ssw,ms)
	call w_ur8_from_ymd(scet,yyyyw,monw,ddw,hhw,mmw,ssw,ms)
	PRINT*,'RETURN SCET',SCET
	PRINT*,'return Y M D H M',YYYYW,MONW,DDW,HHW,MMW
	STOP
	END
