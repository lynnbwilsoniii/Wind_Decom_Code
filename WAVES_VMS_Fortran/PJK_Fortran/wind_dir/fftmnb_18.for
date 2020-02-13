! wind/waves fft data collection and shade plot program        
                                
 	program		fft_grayplot                         
 	implicit	integer*4 (a-z)                      
                                
 	ok = 1                        
 	if (ok) ok = get_set_tm_stream()                      
 	TYPE*,'GOING TO COLLECT EVENT DATA'                   
 	nchgp = 2                             
 	if (ok) ok = wait_for_events(nchgp)                   
 	nchgp = 3                             
 	ok = wait_for_events(nchgp)                   
 	ITERM = -1                            
 	CALL SHPLOT(ITERM)                    
 	ITERM = -1                            
 	CALL TPLOT(ITERM)                     
C                              
 	end                           
 	options/extend_source                         
!------------------------------------------------------------------------------
 	integer*4	function	get_set_tm_stream()                
c	implicit	none                        
 	include		'wind_examples:wind_tm_routine_def.for'              
 	include		'wind_examples:wind_return_code_def.for'             
 	integer*4	ok                          
 	character*4	event,pa(9)                       
 	integer*4	iq,i,j,k,n,iterm,nchgp,npt,np,nday                  
 	integer*4	iw                          
 	integer*4	get_stream_name                     
 	integer*4	wait_for_events			! an entry point                  
 	integer*4	err_count                           
 	character*80	stream                           
 	parameter	size=1024                           
 	integer*4	return_size                         
 	integer*4	raw(size)                           
 	integer*4	mantissa(size)                      
 	integer*4	gain(size)                          
 	integer*4	pwrdb(size)    ! actually, .5 db steps              
 	integer*4	fft_channel                         
 	integer*4	temp_waves                          
 	character*32	s                        
 	real*8		scet8,scetsrt                         
 	character*32	s_scet                           
 	integer*4	scet(2)                     
 	integer*4	ch, chhk, major, minor,itmcorr,iraw                 
 	character*80	file                     
 	character*10	title(4)                         
 	character*32	item                     
 	integer*4	ios,yyyy,idoy,msec                  
 	integer*4	NHRMN,IPNT,LAST,NPTMX,N1,N2,NFILL,NPPL              
 	REAL		PTIME,DELT,DELTS                        
 	REAL		tstart,eltime,evtime,tend,tintv                 
 	REAL		FFTSPACE(4),COUNT(4),SPACE                      
 	real		nhist(100,10),vdata(1024),dbspec(1024)                  
 	integer*2	pwrl                        
 	integer*2	pwrlt(1000,511)                     
 	integer*4 	ncount                     
!                              
                                
 	common /fftblk/ gain,mantissa,pwrdb                   
 	COMMON /PLTPAR/ NDAY,IPNT(4),ITMCORR,IDOY             
 	COMMON /SHBLK/ PTIME(1000),PWRL(1000,511,4)                   
 	common /headblk/ major,minor,fft_channel,s_scet,title         
!                              
 	DATA ITMCORR /1/     ! 1 = CORRECT FOR TIME, 0 = PLOT         
C					CONSECUTIVE SPECTRA                      
 	data ncount /0/                       
 	DATA IPNT /4*0/                       
 	DATA PA /'EXAC','EYAC','EZAC','EXDC','EYDC','EZDC',           
     1		'BX','BY','BZ'/                        
 	DATA JP /0/                           
C                              
 	ok = get_stream_name(stream)                  
 	if (.not. ok) stop 'no file supplied.'                
 	ok = w_channel_open(ch,stream)                
 	if (.not. ok) stop 'cannot open tm channel'                   
                                
 	write(6,*) 'type hr,min to start,end, e.g. 0414,0800'         
 	read(5,*) nhrmn,last                          
 	type*,nhrmn,last                      
 	tstart = (nhrmn/100) + mod(nhrmn,100)/60.             
  3	format(q,i10)                       
  4	format(1x,'enter 1 for hi channels, 2 for mid, 3 for low')          
  5	format(q,a)                         
 	do n = 1,4                            
 	  title(n) = '          '                     
 	  fftspace(n) = 0.                    
 	  count(n) = 1.E-8                    
 	enddo                         
                                
 	ok = w_channel_filename(ch,file)                      
 	print*,'file',file                    
                                
 	scet8 = 0.                            
 	call w_channel_position(ch,scet8)                     
 	nday = scet8                          
 	type*,'first channel position ',scet8,' ch',ch                
 	scet8 = dfloat(nday) + ((nhrmn/100) + mod(nhrmn,100)/60.)/24.         
 	scetsrt = scet8                       
 	type*,'set channel position to',scetsrt,' ch',ch              
C**********  TEMPORARY CODE TO GET PAST START                  
 	OK = WIND_TM_GET_MFMF(CH,MAJOR,MINOR)                 
 	MAJOR = MAJOR+1                       
 	OK = WIND_TM_GET_WORD(CH,MAJOR,MINOR,24,I)            
C*********** END OF TEMPORARY CODE                     
c 	ok = wind_tm_get_mfmf(ch,major,minor)               
c	if (.not. ok) stop 'cannot get stream position'              
                                
 	get_set_tm_stream = 1                         
 	return                        
                                
!----------------------------------------------------------------------       
 	entry	wait_for_events(nchgp)                  
c                              
 	PRINT*,'START NCHGP=',NCHGP                   
C                              
 	call w_channel_position(ch,scetsrt)                   
 	type*,'channel position set to',scetsrt               
                                
 	  event = 'FFT'                       
 	  if(nchgp.eq.1) event = 'FFTH'                       
 	  if(nchgp.eq.2) event = 'FFTM'                       
 	  if(nchgp.eq.3) event = 'FFTL'                       
 	  ok = w_event(ch,event)                      
 	  if(ok.eq.82) then                           
 		PRINT*,'END OF FILE'                         
C		TYPE*,'GO TO PLOT AT ',S_SCET,' previous'                   
 		print*,'rawcount',rawcount                   
 		print*,'fftcount',fftcount                   
 		RETURN                               
 	  endif                               
 	  NEWOLD = 0                          
 	  IF(NCHGP.EQ.0) THEN                         
 		NCHGP = 3                            
 	        NEWOLD = 1                    
 	  ENDIF                               
 	  print*,'collect events: ',event                     
                                
 	   item = 'EVENT_SCET'                        
 	   ok = w_item_i4(ch, item, scet, 2, return_size)             
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
 	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.            
     1		+ mod(scet(2),100)/3600.                       
 	   type*,'first event at',scet,'  evtime',evtime              
C                              
	   item = 'EVENT_SCET_R8'                     
 	   ok = w_item_r8(ch, item, scet8, 1, return_size)            
C	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok       
 	   call w_ur8_to_ydoy(scet8,yyyy,idoy,msec)                   
c                              
 	  go to 110                           
                                
 	! this is the main program loop                       
                                
  100	   continue                       
                                
c	  if( w_end_of_file(ch) ) then                       
                                
           ok = w_event(ch,event)                      
 	   if (.not. ok) then                         
 	      type *, char(7), '******** missing packet in event ********'    
 	      type *, 'Cannot get event at MF.mf: ', major, minor             
 	      if( ok .eq. 82) then                    
 		type*,'npt=',npt                     
 		return                               
 	      endif                           
 	      err_count = err_count + 1                       
 	      if (err_count .lt. 2) goto 100                  
 	      if (err_count .ge. 2) goto 200                  
 	   end if                             
                                
  110	  if(wind_tm_eof(ch,major,minor)) then            
 		PRINT*,'END OF FILE'                         
 		TYPE*,'GO TO PLOT AT ',S_SCET                
 		print*,'rawcount',rawcount                   
 		print*,'fftcount',fftcount                   
 		RETURN                               
 	  endif                               
                                
 	   ! now get the items                        
                                
 	   item = 'channel_number'                    
 	   ok = w_item_i4(ch, item, fft_channel, 1, return_size)              
 	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok        
 	   if(ok .eq. 82) type *,' end of file'               
 	   if(ok .eq. 82) return                      
                                
C******************                    
 	     iw = fft_channel-6                       
 	     if(nchgp.eq.1) iw = fft_channel                  
 	     if(nchgp.eq.2) iw = fft_channel-2                
 	     if(nchgp.eq.3) iw = fft_channel-6                
 	     iw = max0(iw,1)                          
 	     iw = min0(iw,4)                          
C                              
C	PLOT ONLY BX AND BY                          
C                              
 	   item = 'SOURCE'                    
 	   ok = w_item_i4(ch, item, IQ, 1, return_size)               
 	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok        
C                              
 	   IF(IQ.NE.7.AND.IQ.NE.8) GO TO 100                  
C                              
 	   IF(IQ.EQ.7.AND.NCHGP.EQ.3) IW = 2                  
 	   IF(IQ.EQ.8.AND.NCHGP.EQ.3) IW = 1                  
 	   IF(IQ.EQ.7.AND.NCHGP.EQ.2) IW = 4                  
 	   IF(IQ.EQ.8.AND.NCHGP.EQ.2) IW = 3                  
 	   IF(TITLE(iw).EQ.'          ') THEN                 
 	     WRITE(TITLE(iw),1001) FFT_CHANNEL,PA(IQ)                 
C	     TITLE(iw) = PA(IQ)                      
 	   ENDIF                              
 1001	   FORMAT('CH',I2,'  ',A4)                       
                                
C                              
C	TYPE*,'CALL FFT_PHYS'                        
C                              
C	call fft_phys(ch,iraw,vdata,dbspec)                  
 	   item = 'SPECTRUM_DB'                       
 	   ok = w_item_R4(ch, item, DBSPEC, 1024, return_size)        
C                              
C	CALCULATE TOTAL POWER                        
C                              
 	      TPOWER = 0.                     
 	      do n = 128,512                          
 		TPOWER = TPOWER + EXP(.23026*DBSPEC(N))              
 	      enddo                           
 	      TPOWERDB = 10.*ALOG10(TPOWER)                   
c                              
c                              
C	IW is the number of the window in the shade plot, 1-4 for mid,low    
c	NW = 4 here is the number of windows                 
                                
C                              
 	IF(NCHGP.EQ.2) THEN                           
 	  NW1 = 3                             
 	  NW2 = 4                             
 	ELSE                          
 	  NW1 = 1                             
 	  NW2 = 2                             
 	ENDIF                         
C                              
 	      do i = 1,4		! to take care of a problem      
 		space = amax1(space,fftspace(i))                     
 		fftspace(i) = space                          
 	      enddo                           
                                
c	   endif                             
                                
 	   item = 'EVENT_SCET'                        
 	   ok = w_item_i4(ch, item, scet, 2, return_size)             
 	   if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok 
 	   evtime = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.            
     1		+ mod(scet(2),100)/3600.                       
 	   eltime = evtime - tstart                   
C                              
 1002	FORMAT(2I8,I4,F9.3,E12.3)                
C	                             
                                
c*************                         
 	if(iraw.eq.0) then                    
 		fftcount = fftcount + 1.                     
 	else                          
 		rawcount = rawcount + 1.                     
 	endif                         
c                              
c	load histogram of intervals                  
c                              
 	ihist = amin1((eltime - eltimes)*1800.,100.) + .5             
 	ihist = max0(ihist,1)                         
 	nhist(ihist,fft_channel) = nhist(ihist,fft_channel) + 1               
 	eltimes = eltime                      
 	                              
 	if(ipnt(iw).gt.0) then                        
 	   if(iraw.eq.0) then                         
 		iq = ipnt(iw)                        
 	  	fftspace(iw) = fftspace(iw) + evtime - ptime(iq)           
 	     	count(iw) = count(iw) + 1.                      
 	   else                               
 		iq = ipnt(iw)                        
 	  	fftspace(iw) = fftspace(iw) + .25*(evtime - ptime(iq))   
 	     	count(iw) = count(iw) + 1.                      
 	   endif                              
 	endif                         
 	                              
 	   write(s,'(i8.8,i6.6)',iostat=ios) scet(1), scet(2)         
 	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//           
     1    	s(9:10)//':'//s(11:12)//':'//s(13:14)               
 	   ! type the report                          
c                              
 	   NPT = IPNT(IW) + 1                         
 	   IPNT(IW) = NPT                     
 	   PTIME(NPT) = (scet(2)/10000) + (mod(scet(2),10000)/100)/60.        
     1		+ mod(scet(2),100)/3600.                       
 	 type*,'npt,channel,ptime,scet(2)',npt,fft_channel,ptime(npt)          
     1     ,scet(2)                    
 	   do i=1,511                         
c		PWRL(NPT,I,IW) = RAW(I+1)                   
 		PWRL(NPT,I,IW) = DBSPEC(I+1)                 
 	   end do                             
                                
 	   IF(NPT.GT.995) GO TO 200                   
 	   IF(scet(2).LT.(100*LAST)) GO TO 100                
                                
  200	continue                          
                                
 	TYPE*,NPT,' SPECTRA FOUND, EACH CHANNEL'              
C	TYPE*,'GO TO PLOT AT ',S_SCET                
                                
c	do i = 1,100                         
c	  write(44,*) 2*i,nhist(i,1),nhist(i,3),nhist(i,7)           
c	enddo                        
c	write(44,*) file                     
c                              
 	IF(ITMCORR.EQ.0) THEN                         
 	  print*,'rawcount,CORR=0',rawcount                   
 	  print*,'fftcount,CORR=0',fftcount                   
C	call rawprint                        
 	  RETURN                              
 	ENDIF                         
                                
 	TEND = 0.                             
 	NPTMX = 0                             
 	DO I = NW1,NW2                        
 	  NPT = IPNT(I)                       
 	  NPTMX = MAX0(NPT,NPTMX)                     
 	  IF(NPT.GT.0) TEND = AMAX1(TEND,PTIME(NPT))                  
 	ENDDO                         
 	print*,'nptmx,ipnt',nptmx,ipnt                
                                
C                              
C	NPT IS NOW USED AS AN ESTIMATE OF THE NEXT PWRL TIME INDEX CORRESPONDING     
C		TO THE EXACT TIME   PTIME(1) + (NP-1)*TINTV                 
C                              
c	NPT = 1                              
C                              
C	DO N = 1,NPTMX-1                     
C	PRINT*,N,PTIME(N),(PTIME(N)-PTIME(1))/TINTV                  
C	ENDDO                        
C                              
 	      do i = NW1,nw2                          
 		space = amax1(space,fftspace(i))                     
 	      enddo                           
                                
 	DO I = NW1,NW2                        
 	  IF(IPNT(I).NE.0) THEN                       
C	    COPY PWRL INTO PWRLT                     
 	    DO NP = 1,NPTMX                           
 	       DO J = 1,511                           
 	  	PWRLT(NP,J) = PWRL(NP,J,I)                 
 	       ENDDO                          
 	    ENDDO                             
C                              
C	FIND TINTV = AVERAGE SPACING OF FFT'S, NOT COUNTING RAW'S            
 	    TINTV = SPACE/COUNT(I)                    
C	FIND NPPL = NUMBER OF PIXELS IN TIME DIRECTION               
 	    NPPL = (PTIME(NPTMX) - PTIME(1))/TINTV            
 	PRINT*,'SPACE,NO OF SLICES,INT',SPACE,NPPL,TINTV              
 	    IF(NPPL.GT.997) THEN                      
 	print*,'************** warning, too many slices ********'             
 	       NPPL = 997                     
 	       TINTV = (PTIME(NPTMX) - PTIME(1))/NPPL                 
 	    ENDIF                             
C                              
C	  DO NP = 1,NPPL                     
 	  NP = 0                              
 	  NPT = 0                             
 	  DOWHILE (NPT.LE.NPTMX.AND.NP.LE.IPNT(IW))                   
 	    NP = NP+1                         
 	    ELTIME = PTIME(1) + (NP-1)*TINTV                  
C	PRINT*,'NP,pt,eltm',NP,ptime(np),eltime,tintv                
C                              
 	    IF(ABS(PTIME(NP)-ELTIME).LT..5*TINTV) THEN                
C		NO CORRECTION REQUIRED                      
 	      NPT = NP+1                      
 	      GO TO 210                       
 	    ENDIF                             
C                              
C	    SEARCH THE RANGE NPT+-25 FOR NEAREST SPECTRUM            
 	    N1 = MAX0(NPT-25,1)                       
 	    N2 = MIN0(NPT+25,NPTMX)                   
 	    NFILL = 0                         
 	    DELTS = 20.*TINTV                         
C	FIND NEAREST SPECTRUM                        
 	    DO N = N1,N2                      
 	      DELT = ABS(PTIME(N)-ELTIME)                     
 	      IF(DELT.LT.DELTS) THEN                  
 		  DELTS = DELT                       
 		  NFILL = NFILL+1                    
 		  NPT = N                            
 	      ENDIF                           
 	    ENDDO                             
                                
C                              
C	IF NEAREST SPECTRUM IS TOO FAR AWAY, LOAD ZERO'S, ELSE LOAD SPECTRUM         
c	no, this turned out to be a bad idea                 
C	    IF(DELTS.GT.2.*TINTV) THEN                       
C		print*,'skipped a spectrum at',n1,n2                
C		DO J = 1,511                        
C		  PWRL(NP,J,I) = 0.                         
C		ENDDO                               
C	    ELSE                             
C                              
C	TYPE*,'PUT NUMBER',NPT,' INTO SLOT',NP,' TIME',PTIME(NPT)            
C                              
 		DO J = 1,511                         
 		  PWRL(NP,J,I) = PWRLT(NPT,J)                
 		ENDDO                        
C	    ENDIF                            
  210	    CONTINUE                      
  	    NPT = MIN0(NPT+1,NPTMX)                  
 	  ENDDO                               
  	  ENDIF                              
 	ENDDO                         
C                              
C	(N_CHANNEL_GROUP)  NCHGP=1 IS HIGH, 2 IS MID, 3 IS LOW               
 	   NPT = NPTMX                        
C	call rawprint                        
 		print*,'fftcount,RETURN',fftcount                    
 		print*,'rawcount,RETURN',rawcount                    
C	   IF(1) STOP                        
 	PRINT*,'RETURN FROM WAIT FOR EVENTS'                  
 	return                        
 	end                           
 	options/extend_source                         
 !------------------------------------------------------------------------------
 	integer*4	function	get_stream_name(stream)            
 ! This routine gets the user's TM stream type specification.           
 !                              
 	implicit	none                         
 	character*(*)	stream                          
 	common /nrblk/ NHRMN                          
 	integer*4	iq,NHRMN                    
                                
   6	format(1x,'Enter TM stream type [O=offline(default), R=realtime ]: ',$)    
   5	format(q,a)                        
   4	format(1x,'enter number of events to find and process')            
   3	format(q,i10)                      
                                
  10	write(6,6)                         
 	read(5,5,err=10,end=20) iq, stream                    
                                
 	if (iq .lt. 1) then                           
 	   stream = 'offline'                         
 	else if (stream(1:1) .eq. 'o' .or. stream(1:1) .eq. 'O') then         
 	   stream = 'offline'                         
 	else if (stream(1:1) .eq. 'r' .or. stream(1:1) .eq. 'R') then         
 	   stream = 'realtime'                        
 	else                          
 	   ! assume the user entered the name of an offline file              
 	end if                        
                                
 	get_stream_name = 1                           
                                
  20	return                             
 	end                           
 	SUBROUTINE SHPLOT(ITERM)                      
C                              
 	character*32 scet                     
 	integer*2 npwrl                       
 	COMMON /SHBLK/ PTIME(1000),NPWRL(1000,511,4)                  
 	COMMON /PLTPAR/ NDAY,IPNT(4),ITMCORR,IDOY             
 	common /headblk/ major,minor,ifftch,scet,title                
C                              
 	COMMON /MONGOPAR/                     
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,           
     1  GX,GY,CX,CY,                           
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,                    
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,           
     1  TERMOUT,XYSWAPPED,NUMDEV,                      
     1  PI,USERVAR(10),AUTODOT                 
 	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV                
C                              
 	CHARACTER*10 TITLE(4)                         
 	CHARACTER*120 STR                     
 	REAL FUNDF(3)                         
 	DIMENSION PWRL(511000)                        
 	DATA FUNDF /.021348,.005337, .33356/                  
C                              
C                              
 	NWNDOW = 4                            
 	XSTART = 400.                         
 	XEND = 2300.                          
 	IW = 1                        
 	NPT = IPNT(IW)                        
 	PRINT*,' IN SHPLOT,NPT',NPT                   
 	IF(NPT.LT.1) THEN                     
 	  IW = 2                              
 	  NPT = IPNT(IW)                      
 	ENDIF                         
 	IF(NPT.LT.1) THEN                     
 	  IW = 3                              
 	  NPT = IPNT(IW)                      
 	ENDIF                         
 	IF(NPT.LT.1) THEN                     
 	  IW = 4                              
 	  NPT = IPNT(IW)                      
 	  IF(NPT.LT.1) RETURN                         
 	ENDIF                         
 	PRINT*,' IN SHPLOT,TIMES',PTIME(1),PTIME(NPT),NPT             
C	CALCULATE BOX SIZE                           
 	PIXSIZ = .1                           
 	IF(NPT.GT.1) PIXSIZ = (PTIME(NPT) - PTIME(1))/(NPT-1)         
 	HSTART = PTIME(1) - .5*PIXSIZ                 
 	HEND =   PTIME(NPT) + .5*PIXSIZ                       
 	PRINT*,' IN SHPLOT,TIMES,NPT',HSTART,HEND,NPT                 
 	  CALL MGOINIT                        
 	  CALL MGOSETUP(ITERM)                        
 	  CALL MGOERASE                       
C                              
 	                              
 	IF(ITERM.LT.0) THEN                           
 	  CALL MGOSETLOC(XSTART,280.,XEND,3100.)              
 	ENDIF                         
C                              
 	DO IW = 1,NWNDOW                      
 	  CALL MGOWINDOW(1,NWNDOW,IW)          !              
 	  NPT = IPNT(IW)                      
 	  IF(NPT.LT.1) GO TO 200                      
 	  LIMFREQ = 511                       
 	  NCHGP = 3                           
 	  IF(IW.GE.3) THEN                    
 		LIMFREQ = 37                         
 		NCHGP = 2                            
 	  ENDIF                               
 	  PRINT*,' IN SHPLOT,IW,NPT',IW,NPT                   
 	  YMIN = 1.E6                         
 	  YMAX = -YMIN                        
 	  CALL HISTOG(2,TJUNK,256,-250.,0.,.01,TOTAL,RET)     ! CLEAR AND INIT        
 	  DO M = 1,NPT                        
 	  DO N = 1,LIMFREQ                    
 	    NM = M + (N-1)*NPT                        
 	    PWRL(NM) = NPWRL(M,N,IW)                  
 	    CALL HISTOG(1,PWRL(NM),256,-250.,0.,.5,TOTAL,RET)    ! LOAD ARRAY         
C                              
 	    YMIN = AMIN1(YMIN,PWRL(NM))                       
 	    YMAX = AMAX1(YMAX,PWRL(NM))                       
 	  ENDDO                               
 	  ENDDO                               
 	  PRINT*,' YMIN,MAX ACTUAL',YMIN,YMAX                 
C                              
 	  CALL HISTOG(0,TJUNK,256,-250.,0.,.03,TOTAL,YHMIN)   !DETERMINE 3 PCTILE     
 	  CALL HISTOG(0,TJUNK,256,-250.,0.,.97,TOTAL,YHMAX)  !DETERMINE 97 PCTILE     
C	  RAISING YMIN MAKES THE BACKGROUND LIGHTER                  
C	  LOWERING YMAX MAKES THE SIGNAL DARKER              
                                
 	  YAMIN = YHMIN                       
 	  YAMAX = YHMAX                       
 	  PRINT*,'YMIN,MAX SET TO',YAMIN,YAMAX                
c	  arguments are: array(m,n),m,n,white,black,linearity        
c	  m is the x direction                       
 	  CALL MGOHALFTONE(PWRL,NPT,LIMFREQ,YAMIN,YAMAX,1.E7)         
 	  CALL MGOSETEXPAND(.7)                       
 	  CALL MGOSETLIM(HSTART,FUNDF(NCHGP),HEND,LIMFREQ*FUNDF(NCHGP))       
 	  IF(NCHGP.EQ.3) THEN                         
 	    CALL MGOYLABEL(13,'FREQUENCY, Hz')	               
 	  ELSE                        
 	    CALL MGOYLABEL(14,'FREQUENCY, kHz')	              
 	  ENDIF                               
 	  CALL MGOBOX(1,2)                    
 	  CALL MGOGRELOCATE(GX1,GY2)                  
 	  CALL MGOPUTLABEL(10,TITLE(IW),9)                    
 	print*,'title in shplot  ',title(iw)                  
C                              
 	  WRITE(STR,704) .5*YAMIN,.5*YAMAX                    
 704	  FORMAT('WHITE,BLACK ',2F6.1,' DB ')             
 	  CALL MGOSETANGLE(90.)                       
 	  CALL MGOSETEXPAND(.5)                       
 	  XPR = GX2 + .005*(GX2-GX1)                  
 	  YPR = .5*(GY1+GY2)                          
 	  CALL MGOGRELOCATE(XPR,YPR)                  
 	  CALL MGOPUTLABEL(28,STR,2)                  
 	  CALL MGOSETANGLE(0.)                        
 	  CALL MGOSETEXPAND(1.)                       
  200	  CONTINUE                        
 	  IF(IW.EQ.1) THEN                    
C	   s_scet = s(1:4)//'/'//s(5:6)//'/'//s(7:8)//' '//          
C	1	s(9:10)//':'//s(11:12)//':'//s(13:14)              
          WRITE(STR,1001) SCET(1:10),IDOY            
 1001	  FORMAT('HOURS OF ',A10,' DOY ',I3)                   
 	  CALL MGOSETEXPAND(.8)                     
          CALL MGOXLABEL(28,STR)	                    
 	    CALL MGOSETEXPAND(.5)                     
 	    IF(ITMCORR.EQ.0) THEN                     
 		CALL MGOPUTLABEL(23,'    consecutive spectra',6)             
 	    ELSE                              
 		CALL MGOPUTLABEL(18,'    corrected time',6)                  
 	    ENDIF                             
 	    CALL MGOSETEXPAND(1.)                     
 	  ENDIF                               
C                              
 	  IF(IW.EQ.NWNDOW) THEN                       
 	    CALL MGOSETEXPAND(.8)                     
 	    CALL MGOPLOTID('[.WIND]FFTMNB','SHPLOT')                  
 	    CALL MGOSETEXPAND(1.)                     
 	  ENDIF                               
 	ENDDO                         
C                              
 	  CALL MGOSETEXPAND(1.)                       
 	  IF(ITERM.LT.0) THEN                         
 	    CALL MGOPRNTPLOT(NVEC)                    
 	    PRINT*,' NO. VECTORS PLOTTED',NVEC                
 	  ELSE                        
 	    CALL MGOTCLOSE                    
 	  ENDIF                               
C                              
 	RETURN                        
 	END                           
 	SUBROUTINE RAWPRINT                           
C                              
 	common /fftblk/ nexpt(1024),ndata(1024),ipwrdb(1024)          
 	common /headblk/ major,minor,ich,scet,title                   
 	character*32 scet                     
 	character*10	title(4)                         
C                              
 	write(36,*) 'fft data collection at ',scet,' channel',ich             
C                              
 	DO I = 1,103                          
 	  I2 = 10*I                           
 	  I1 = I2 - 9                         
 	  I2 = MIN0(I2,1024)                          
 	  WRITE(36,1006) I1-1,(NEXPT(J),NDATA(J),J=I1,I2)             
 	ENDDO                         
C                              
 	RETURN                        
 1006	FORMAT(I5,'.',(10(I3,I5)))                       
 	END                           
 	SUBROUTINE HISTOG(ILOAD,X,NBIN,XMIN,XMAX,PCTILE,TOTAL,RET)            
C                              
C	THIS IS A SPECIAL VERSION TO CALCULATE LEVEL AT A CERTAIN PERCENTILE         
C                              
C	INTEGER*4 NARR                       
 	DIMENSION NARR(256)                           
 	DATA NARR /256*0/                     
C                              
 	IF(ILOAD.GT.1) THEN                           
 	  DO N = 1,256                        
 	    NARR(N) = 0                       
 	  ENDDO                               
 	  TOTAL = 0.                          
 	  RETURN                              
 	ENDIF                         
C                              
 	IF(ILOAD.EQ.1) THEN                           
 	  XT = AMAX1(X,XMIN)                          
 	  XT = AMIN1(XT,XMAX)                         
 	  IBIN = (NBIN-1)*(XT-XMIN)/(XMAX-XMIN) + 1                   
 	  NARR(IBIN) = NARR(IBIN)+1                   
 	  TOTAL = TOTAL + 1.                          
 	  RETURN                              
 	ENDIF                         
C                              
 	IF(PCTILE.GT..5) THEN                         
C	  PRINT OUT RESULTS                          
C    	  PRINT*,' TOTAL NUMBER, NBIN',TOTAL,NBIN                
C	  PRINT*,'XMIN,XMAX',XMIN,XMAX                       
c	  PRINT*,'HISTOGRAM'                         
c	  PRINT 1111, (NARR(J),J=1,NBIN)                     
 1111	  FORMAT(10I7)                           
 	ENDIF                         
C                              
C	NOW CALCULATE VALUE OF X CORRESPONDING TO PCTILE PERCENTILE          
C                              
C	PCTILE = .2                          
 	SET = PCTILE*TOTAL                    
 	SUM = 0.                              
 	N = 0                         
  100	N = N+1                          
 	  SUM = SUM + NARR(N)                         
 	  IF(SUM.LT.SET) GO TO 100                    
C                              
C	NOW THE DESIRED VALUE LIES IN BIN N                  
C                              
 	SUMLOW = SUM - NARR(N)                        
 	XN = N                        
 	RETLOW = XMIN + ((XN-1.)/(NBIN-1.))*(XMAX-XMIN)               
 	RETHI = XMIN +       (XN/(NBIN-1.))*(XMAX-XMIN)               
 	RET = .5*(RETHI+RETLOW)                       
 	IF(NARR(N).NE.0)                      
     1	RET = RETLOW + (SET-SUMLOW)*(RETHI-RETLOW)/NARR(N)              
C	PRINT*,'N,SET,SUMLOW,LOW,RET',N,SET,SUMLOW,RET               
 	RETURN                        
 	END                           
 	SUBROUTINE TPLOT(ITERM)                       
C                              
C	PLOTS THE TIME DOMAIN DATA                   
C                              
 	character*32 scet                     
 	integer*2 npwrl                       
 	character*10	titlex(4)                        
 	REAL	FUNDF(3)                         
 	COMMON /SHBLK/ PTIME(1000),NPWRL(1000,511,4)                  
 	COMMON /PLTPAR/ NDAY,IPNT(4),ITMCORR,IDOY             
 	COMMON /SPBLK/ PWRW(1024),PWRNW(1024),FDATA(1024),DATA(1024)          
 	common /headblk/ major,minor,ifftch,scet,titlex               
C                              
 	COMMON /MONGOPAR/                     
     1  X1,X2,Y1,Y2,GX1,GX2,GY1,GY2,LX1,LX2,LY1,LY2,           
     1  GX,GY,CX,CY,                           
     1  EXPAND,ANGLE,LTYPE,LWEIGHT,                    
     1  CHEIGHT,CWIDTH,CXDEF,CYDEF,PSDEF,PYDEF,COFF,           
     1  TERMOUT,XYSWAPPED,NUMDEV,                      
     1  PI,USERVAR(10),AUTODOT                 
 	INTEGER*4 LX1,LX2,LY1,LY2,LTYPE,LWEIGHT,NUMDEV                
C                              
 	DIMENSION PP(1024),YY(1024)                   
 	CHARACTER*8 TITLE(4)                          
 	CHARACTER*120 STRG(5)                         
 	CHARACTER*120 STR                     
 	DATA TITLE/'RECT.','HAMMING','HANNING',' '/                   
 	DATA FUNDF /.021348,.005337, .33356/                  
C                              
C                              
 	  CALL MGOINIT                        
 	  CALL MGOSETUP(ITERM)                        
 	  CALL MGOERASE                       
 	IF(ITERM.LT.0) THEN                           
 	  PRINT*,'START TPLOT',GX1,GX2,GY1,GY2                
C	  CALL MGOSETLOC(500.,330.,GY2-200.,3000.)                   
C	  CALL MGOSETLOC(GX1,GY1,GX2,GY2-100.)               
 	ENDIF                         
C                              
 	IW = 1                        
 	NPT = IPNT(IW)                        
 	PRINT*,' IN TPLOT',NPT,iterm                  
 	IF(NPT.LT.1) THEN     	  ! protection                 
 	  IW = 2                              
 	  NPT = IPNT(IW)                      
 	  PRINT*,' IN TPLOT',NPT,' IW',IW                     
           IF(NPT.LT.1) RETURN                  
 	ENDIF                         
 	PRINT*,' IN TPLOT,TIMES',PTIME(1),PTIME(NPT),NPT              
C	CALCULATE BOX SIZE                           
 	XRANGE = PTIME(NPT) - PTIME(1)                
 	XMIN = PTIME(1) - .01*XRANGE                  
 	XMAX = PTIME(NPT) + .01*XRANGE                
C                              
 	  CALL MGOSETEXPAND(.8)                       
 	CALL MGOGRELOCATE(GX1,120.)                   
 	CALL MGOPUTLABEL(79,SCET(1:10),3)                     
        WRITE(STR,1001) SCET(1:10),IDOY            
 1001	  FORMAT('HOURS OF ',A10,' DOY ',I3)                   
 	  XPR = GX2 + .005*(GX2-GX1)                  
 	  YPR = .5*(GY1+GY2)                          
 	  CALL MGOGRELOCATE(XPR,YPR)                  
 	  CALL MGOPUTLABEL(28,STR,2)                  
 	  CALL MGOSETANGLE(0.)                        
 	  CALL MGOSETEXPAND(1.)                       
 200	  CONTINUE                        
 	  TOP = LY2                           
 	  CALL MGOGRELOCATE(GX1,TOP-100.)                     
 	  CALL MGOPUTLABEL(11,'FFT CHANNEL',3)                
 	  WRITE(STR,802) IFFTCH                       
 	  CALL MGOPUTLABEL(8,STR,3)                   
 	  CALL MGOPUTLABEL(10,'   SOURCE ',3)                 
 	  IW = IFFTCH                         
 	  IF(IFFTCH.GT.2) IW = IFFTCH-2                       
 	  IF(IFFTCH.GT.6) IW = IFFTCH-6                       
 	  CALL MGOPUTLABEL(10,TITLEX(IW),3)                   
C                              
 	DO JW = 1,6                           
 	  CALL MGOWINDOW(1,6,JW)                      
 	  CALL MGOSETEXPAND(.6)                       
 	  TOP = LY2                           
 802	  FORMAT(2I6)                     
 	  CALL MGOSETEXPAND(1.)                       
C                              
 	  YMAX = -1000.                       
 	  YMAX = -YMIN                        
 	  JP = 1                              
 	  DO J = 1,NPT                        
 	    JP = J                            
 	    PP(JP) = PTIME(J)                         
 	    YY(JP) = NPWRL(J,JW,IW)                   
 	      YMAX = AMAX1(YY(JP),YMAX)                       
 	      YMIN = AMIN1(YY(JP),YMIN)                       
 	  ENDDO                               
C                              
 	  RANGE = ABS(YMAX-YMIN)                      
 	  YMAX = YMAX + .05*RANGE                     
 	  YMIN = YMIN - .05*RANGE	                    
C	  CALL MGOTICKSIZE(20.,100.,0.,0.)                   
 	  CALL MGOSETLIM(XMIN,YMIN,XMAX,YMAX)                 
 	  CALL MGOCONNECT(PTIME,YY,JP)                
 	  WRITE(STR,801) JW*FUNDF(3)                  
  801	  FORMAT(F6.2,' Hz')                      
 	  CALL MGORELOCATE(XMIN +.1*XRANGE,YMAX-.2*RANGE)             
 	  CALL MGOLABEL(9,STR)                        
 	  CALL MGOSETEXPAND(.6)                       
 	  CALL MGOBOX(1,2)                    
 	  CALL MGOXLABEL(16,'HOURS OF THE DAY')               
 	  CALL MGOYLABEL(5,'DB nT')                   
 	  CALL MGOSETEXPAND(1.)                       
 	ENDDO                         
C                              
 	CALL MGOSETEXPAND(.7)                         
 	CALL MGOPLOTID('[.WIND]FFTMNB','TPLOT')               
 	CALL MGOSETEXPAND(1.)                         
 	IF(ITERM.LT.0) THEN                           
 	  CALL MGOPRNTPLOT(NVEC)                      
 	  PRINT*,' NO. VECTORS PLOTTED',NVEC                  
 	ELSE                          
 	  CALL MGOTCLOSE                      
 	ENDIF                         

C                              
 	RETURN                        
C                              
 	END                           
