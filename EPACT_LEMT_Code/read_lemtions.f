		Program read_lemtions
!
!		to convert lemt ion file to CSF form 
!
!		L. C. Tan	04-Oct-11 
!		L. C. Tan	19-Dec-11 move to UMD                   
!		Add one channel to He
!
!		input date format: 	
!	
!		0   1994 307 0001.3    1994 307 0101.1       0.00348  0.000258   
!		123456789012345678901234567890123456789012345678901234567890	
!		length of line = 200
!
!		parameter (ni= 6)
!
 		character (1) :: blank 
		character (30) :: inpnam 
		character (15) :: oupnam 
		character (4) :: sdate, cname
		character (43) :: stime
		character (3) :: sm, smy(12)
		character (200) :: spar
		character (7) :: ctit, ctit1, ctit2, ctit3, ctit4, ctit5, ctit6
!
!		real*8 doy0, dd21
		integer :: is, iy0, iy1, ix, it(6), imy(12), na(6)
		real ::   fp(7), dfp(7), ea(7), eb(7), ea1(7), ea2(7), ea3(7), & 
		ea4(6), ea5(7), ea6(7), eb1(7), eb2(7), eb3(7), eb4(6), &
		eb5(7), eb6(7), fup(6)
!
		data smy/'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
  		 		'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
!
		data imy/1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12/
!
		data na/7, 7, 7, 6, 7, 7/
		data ea1/2.00, 2.40, 3.00, 3.70, 4.53, 6.00, 7.40/
		data eb1/2.40, 3.00, 3.70, 4.53, 6.00, 7.40, 9.64/
		data ea2/2.57, 3.19, 3.85, 4.80, 5.80, 7.20, 9.10/
		data eb2/3.19, 3.85, 4.80, 5.80, 7.20, 9.10, 13.70/
		data ea3/2.56, 3.17, 3.88, 4.68, 6.00, 7.40, 9.20/
		data eb3/3.17, 3.88, 4.68, 6.00, 7.40, 9.20, 13.40/
		data ea4/3.27, 3.98, 4.72, 5.92, 7.87, 9.96/
		data eb4/3.98, 4.72, 5.92, 7.87, 9.96, 12.70/
		data ea5/2.50, 3.20, 4.00, 4.90, 6.00, 7.90, 9.70/
		data eb5/3.20, 4.00, 4.90, 6.00, 7.90, 9.70, 13.60/
		data ea6/2.40, 3.00, 3.95, 4.80, 5.90, 7.80, 9.30/
		data eb6/3.00, 3.95, 4.80, 5.90, 7.80, 9.30, 12.50/
!
		data fup/1e4, 2e3, 2e3, 2e3, 2e3, 2e3/
!
		data ctit1/'He_omn_'/
		data ctit2/'C__omn_'/
		data ctit3/'O__omn_'/
		data ctit4/'Ne_omn_'/
		data ctit5/'Si_omn_'/
		data ctit6/'Fe_omn_'/
!
!
		write(*, 210)
210		format(' Enter 1[He],2[C],3[O],4[Ne],5[Si],6[Fe]=> ')
		read *, is
!
		if(is == 1) then
		  do 220 i= 1, na(is)
		    ea(i)= ea1(i)
		    eb(i)= eb1(i)
220		  continue
		  ctit= ctit1
		else
		  if(is == 2) then
		    do 230 i= 1, na(is)
		      ea(i)= ea2(i)
		      eb(i)= eb2(i)
230		    continue
		    ctit= ctit2
		  else
		    if(is == 3) then
		      do 240 i= 1, na(is)
		        ea(i)= ea3(i)
		        eb(i)= eb3(i)
240		      continue
		      ctit= ctit3
		    else
		      if(is == 4) then
		        do 250 i= 1, na(is)
		          ea(i)= ea4(i)
		          eb(i)= eb4(i)
250		        continue
		        ctit= ctit4
		      else
		        if(is == 5) then
		          do 260 i= 1, na(is)
		            ea(i)= ea5(i)
		            eb(i)= eb5(i)
260		          continue
		          ctit= ctit5
		        else
		          if(is == 6) then
		            do 270 i= 1, na(is)
		              ea(i)= ea6(i)
		              eb(i)= eb6(i)
270		            continue
		            ctit= ctit6
		          endif
		        endif  
		      endif
		    endif
		  endif
		endif
!		print *, 'is=', is
!		print *, 'ctit=', ctit
!		print *, 'ea=', ea
!		print *, 'eb=', eb
!
		write(*, 10)
10		format(' Enter name of  input file=> ')
		read(*, 20) inpnam
20		format(a)
!
  		open (unit= 3, file= inpnam, status = 'old')
!
!	read blank lines
!
2		read (3, 4) sdate
4		format(a)
		if(sdate == '#End') then
	  	  goto 6
		else
	      goto 2
		endif
!
6		continue
!
!		iy0= 1994
		write(*, 12) 
12		format(' Enter name of first year=> ')
		read *, iy0
!
		write(cname, 80) iy0
80		format(i4)
		oupnam= ctit//cname//'.txt'
!			print *, oupnam
			write(*, 14)  oupnam
14		format(a15)
!
		open(unit= 4, file= oupnam, status= 'new', recl= 300)
		ix= 1
!
		if(na(is) == 6) then
			write(4, 90) (ea(i), eb(i), i, i=1, na(is))
90			format(1x, 'Year, Month, Day, Hour, DOY, ', &
			5(f5.2, '-', f5.2, ', dJ', i1, ', '), &
			f5.2, '-', f5.2, ', dJ', i1)
		else
			write(4, 92) (ea(i), eb(i), i, i=1, na(is))
92			format(1x, 'Year, Month, Day, Hour, DOY, ', &
			6(f5.2, '-', f5.2, ', dJ', i1, ', '), &
			f5.2, '-', f5.2, ', dJ', i1)
		endif		
!
!
100		read(3, 110, end= 1000) stime, spar
110		format(a43, a200)
!
		read(stime, 120) iy1, idoy1, ih1, rm1, iy2, idoy2, ih2, rm2
120		format(2(4x, i4, 1x, i3, 1x, i2, f4.1))
!
!	remove data with sampling period less than 0.5 hr
!
		jy1= mod(iy1, 4)
		if(jy1.eq. 0) then
			iy1f= 366
		else
		 	iy1f= 365
		endif
!
		dd21= iy1f*(iy2- iy1)+ (idoy2- idoy1)+ (ih2- ih1)/ 24. &
	 			+(rm2- rm1)/ (24.* 60.)
!
		if(dd21 <= (31./(24.* 60.))) goto 100
!
		  if(iy1 > iy0) then
			close(unit= 4)
			iy0= iy1
			ix= 0
		  endif
!
		  if(ix == 0) then
!
		    write(cname, 80) iy0
		    oupnam= ctit//cname//'.txt'
		    print *, oupnam
!
		    open(unit= 4, file= oupnam, status= 'new', recl= 300)
		    ix= 1
!
		    if(na(is) == 6) then
			  write(4, 90) (ea(i), eb(i), i, i=1, na(is))
		    else
			  write(4, 92) (ea(i), eb(i), i, i=1, na(is))
		    endif		
		  endif
!
		  it(1)= iy1
		  call CALIDM(iy1, idoy1, im1, id1)
		  it(2)= im1
		  it(3)= id1
		  it(4)= nint(ih1+ rm1/ 60.)
		  doy0= idoy1+ (it(4)+ 0.5)/24.
!
		  read(spar, *) (fp(i), dfp(i), i=1, na(is))
!
		  do 178 i=1, na(is)
		  	if((fp(i) > fup(is)).or. (dfp(i) > fup(is))) then
		  	   fp(i)= -1.e31
		  	   dfp(i)= -1.e31
		  	endif
178		  continue
!
		  if(na(is) == 6) then
			write(4, 180) (it(i), i=1, 4), doy0, &
	 		  (fp(i), dfp(i), i=1, na(is))
!
180		    format(1x, i4, ', ', 3(i2, ', '), f7.3, ', ', &
	 				11(g11.4, ', '), g11.4)
		  else
			write(4, 182) (it(i), i=1, 4), doy0, &
	 		  (fp(i), dfp(i), i=1, na(is))
!
182		    format(1x, i4, ', ', 3(i2, ', '), f7.3, ', ', &
	 				13(g11.4, ', '), g11.4)
		  endif
!
		  goto 100		
!
1000	  continue
		stop
!		end
		End Program read_lemtions
!
!
!
		subroutine daytodoy(it, doy)
!
!		real*8 doy
		integer :: it(6), idm(12), idma(12), idmb(12)
!		dimension it(6), idm(12), idma(12), idmb(12)
!
		data idma/31,29,31,30,31,30,31,31,30,31,30,31/
		data idmb/31,28,31,30,31,30,31,31,30,31,30,31/
!
		iz= it(1)- 1960
		jz= mod(iz, 4)
!
		if(jz == 0) then
	  		do 10 i=1, 12
	    		idm(i)= idma(i)
10	  		continue
		else
	  		do 20 i=1, 12
	    		idm(i)= idmb(i)
20	  		continue
		endif
!
		ndoy= 0
		if(it(2) > 1) then
	  		do 30 i=1, (it(2)- 1)
	    		ndoy= ndoy+ idm(i)
30	  		continue
		endif
!
		doy= ndoy+ it(3)+ it(4)/24.+ it(5)/1440.+ it(6)/86400.
!
		return
		end
!
!
!
		SUBROUTINE CALIDM(IY0, IDOY, IM0, ID0)
!		DIMENSION IDM(12),IDMA(12),IDMB(12)
		integer :: IDM(12),IDMA(12),IDMB(12) 
!
		DATA IDMA/31,29,31,30,31,30,31,31,30,31,30,31/
		DATA IDMB/31,28,31,30,31,30,31,31,30,31,30,31/
!
		IX= IY0- 1960
		JX= MOD(IX, 4)
		DO 10 I=1, 12
	  	IF(JX == 0) THEN
	      IDM(I)= IDMA(I)
	  	ELSE
	      IDM(I)= IDMB(I)
	  	ENDIF
10		CONTINUE
!
		IX= IDOY
		DO 20 I= 1, 12
	  	IX= IX-IDM(I)
!
	  	IF(IX == 0)  THEN
	      IM0= I
	      ID0= IDM(I)
	      GOTO 30
	  	ENDIF
!
	  	IF(IX < 0)  THEN
	      IM0= I
	      ID0= IX+ IDM(I)
	      GOTO 30
	    ENDIF
20		CONTINUE
30		RETURN
		END
!