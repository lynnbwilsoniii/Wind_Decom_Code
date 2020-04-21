;+
; NAME:
; 	doy2cal
;
; PURPOSE:
;	This procedure converts 
;		from the YEAR and DOY    ===>  MONTH and DAY-OF-MONTH
;	The month is available as a number and as a string name.
;
; CATEGORY:
;	General user function. Useful with "julday" and "caldat" (both built-in
;	idl functions.).   The IMP-8 IDL analysis programs impdayplot94 and
;	carrington both rely on this program.
;
; CALLING SEQUENCE:
;	doy2cal,YEAR,DOY,MONTHNAME,MONTHDAY,MONTHNUMBER
;
; INPUTS:
;	YEAR:	An integer year. Like "1967", for example.
;	DOY:	An integer Day-of-Year. Like "60", for example.
;
;	MONTHNAME:	A variable which will receive the string month name.
;	MONTHDAY:	A variable which will receive the integer day of 
;			the month.
;	MONTHNUMBER:	A variable name which will receive the integer month.
;
; OUTPUT:
;	It just fills in the values for the passed variables.
;
; PROCEDURE:
;	It uses the built-in IDL "julday" and "caldat" (making use of julian
;	day numbers).
;
; EXAMPLE:
;	To print the date with a three-letter month abbreviation for doy 29 in
;	1967:
;		year=1967
;		doy=60
;		doy2cal,year,doy,month,day
;		print,month,day,year,format='(a3," ",i0,", ",i0)'
;-

pro doy2cal,year,doy,monthname,monthday,monthnumber

jul=julday(12,31,year-1)
caldat,jul+doy,monthnumber,monthday,year
case monthnumber of
	1: monthname='January'
	2: monthname='February'
	3: monthname='March'
	4: monthname='April'
	5: monthname='May'
	6: monthname='June'
	7: monthname='July'
	8: monthname='August'
	9: monthname='September'
	10: monthname='October'
	11: monthname='November'
	12: monthname='December'
endcase
end

