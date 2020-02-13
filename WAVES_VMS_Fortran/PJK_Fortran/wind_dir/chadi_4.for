	OPTIONS/EXTEND_SOURCE
	program rdwnd_ffthx
c----------------------------------------------------------
c  to read FFTH data
c  PARIS, 19-03-98
c----------------------------------------------------------
	include 	'wind_examples:wind_tm_routine_def.for'
	real*8		scet,scetd,scetf,scetr,scet1,scet2
	real*8		scetfill,scettds
	character*160	string,file,fildat
        character*4     event,ant,eventn(1000),antn(1000),str4
        character*6     date
        character*8     str8
 	integer*4	ok,ch
	integer*4	return_size,tds_channel,return_size_o,return_size_s
c        integer*4       w_channel_open,w_event
	real*4		tab(2048)
        dimension nevent(1000)
c
	REAL*4 		DATA(2050),vdatax(2048),vdatay(2048),vdataz(2048)
	REAL*4 		spz(1024),spy(1024),spx(1024)
        INTEGER*4       NDATA(2048)
c
c
        data icheof /0/
c----------------------------------------------------------
c   GETTING STARTED
c         open(unit=15,status='old',file='tds_init.dat')
C----------------------------------------------------------
c
*
c5        CONTINUE 
c         nevtmx=50
*
c
c we fix the date;
c
c          read(15,*,err=10,end=20) ian,mois,jour
        type *,'date : AA MM JJ'
        accept *, ian,mois,jour  
c          ian=95
c          mois=07
c          jour=12
          idate = ian*10000+mois*100+jour
          encode(6,777,date) idate
777     format(i6)
         type *,'date: ',date          
          file ='wind_data:wi_lz_wav_19'//date//'_V*.dat'
          type *,file
c
c   Now, first, open "channel" to the data-base:
c   if 'offline' then we use the menu:
c
c	ok=w_channel_open(ch,  'OFFLINE')
c
c   if we know the date of the physical  event we want  to
c     study, for ex. : 12/o3/1995, then: 
c-------------------------------------------------------------------------
c
        ok = w_channel_open(ch,  file)
        if(ok.ne.1) type *,'unable to open telemetry for chann.: ',ch
        type *, 'channel: ',ch,' ok= ',ok
c------------------------------------------------------------------------- 
c warning: the V01 - V02 - ... are successive ameliorations by NASA;
c the wild card option uses the latest one; not supported by earlier 
c version of WINDLIB.
c-------------------------------------------------------------------------
c
	ok =w_channel_filename(ch, string)
	type *,'.....ch: ', string
c
c   string is the name of tdata file associated with the channel
c
c-------------------------------------------------------------------------
c Now we choose the experiment: RAD1,RAD2,FFTH, FFTM,FFTL,TNR,
c TDSF,TDSS,FILL, HK, CDF(?)
c-------------------------------------------------------------------------
c 
	ok = w_event(ch, 'FFTH')
c--------------------------------------------------------------------------
c We determine the time -scet-  of the first event in  the data file
c  This time is one of the item in a given event; the list of item's
c is available by the command query_dbms/tnr for example
c--------------------------------------------------------------------------

	ok = w_item_r8(ch, 'event_scet_R8', scet1, 1, return_size)
c 
c time conversion
c
	ok =w_ur8_to_ymd(scet1, 
	1			my_year, my_month, my_date,
	1			my_hour, my_minute, my_second, my_msecond)
*        type *,'FFTH,debut'
	write(6,900) my_year, my_month, my_date,
	1	 my_hour, my_minute, my_second, my_msecond
*
 900     format(2x,7i4)
c
c
c we fix the initial time we are interested in,
c
        type *,'temps debut : hh mn'
        accept *, my_hour,my_min  
c	my_hour = 2
c	my_minute = 0
c
c we convert in WINDLIB time
c
	ok =w_ur8_from_ymd(scetd, 
     1			my_year, my_month, my_date,
     1			my_hour, my_minute, my_second, my_msecond)
c
c we fix the final time we are interested in,
c
        type *,'temps fin : hh mn'
        accept *, my_hour,my_min  
c	my_hour = 2
c	my_minute = 30
c
c we convert in WINDLIB time
c
	ok =w_ur8_from_ymd(scetf, 
     1			my_year, my_month, my_date,
     1			my_hour, my_minute, my_second, my_msecond)



c We position on the first event immediately after the time
c we have chosen
c 
	call w_channel_position(ch, scetd)
c
c the same as above
c

	
	open(unit=11,status='new',form='unformatted',
     1		file='ffthspx_'//date//'.dat')
	
	open(unit=14,status='new',form='unformatted',
     1		file='ffthspy_'//date//'.dat')
	
	open(unit=16,status='new',form='unformatted',
     1		file='ffthspz_'//date//'.dat')


	numev=0
	numevspx=0
	numevspy=0
	numevspz=0
c	iantenne=0
c....................................................................
1       continue
c....................................................................

	type *,'************************ Evenement suivant ********'

	ok = w_event(ch, 'FFTH')
        if (ok.eq.82) go to 10

	ok = w_item_r8(ch, 'event_scet_R8', scet, 1, return_size)
c
cc	if(scet.gt.scetf) go to 10

	ok = w_item_char(ch, 'SOURCE', 
	1				string,1, return_size)

        ok = w_item_i4(ch, 'channel_number',ichn,1,return_size)

        str8=string
	str4=string

	if (str4.eq.'ExAC') iantenne=0
	if (str4.eq.'EyAC') iantenne=1
	if (str4.eq.'EzAC') iantenne=2



	if (str4.ne.'ExAC'.and.str4.ne.'EyAC'.and.str4.ne.'EzAC') go to 1

        ok = w_item_r4(ch, 'FUNDAMENTAL_FREQUENCY_R4',fr0,1,return_size)
        ok = w_item_r4(ch, 'NYQUIST_FREQUENCY_R4',frn,1,return_size)
        ok = w_item_r4(ch, 'BOTTOM_FREQUENCY_R4',frb,1,return_size)
        ok = w_item_r4(ch, 'STEP_FREQUENCY_R4',df,1,return_size)
c        ok = w_item_r4(ch, 'TOP_FREQUENCY_R4',frt,1,return_size)


	ok =w_ur8_to_ymd(scet, 
	1			my_year, my_month, my_date,
	1			my_hour, my_minute, my_second, my_msecond)
c             
c	type *,my_hour, my_minute, my_second, my_msecond


	if (str4.eq.'ExAC') then
	ok =w_item_r4(ch, 'SPECTRUM_EXAC_DBVOLTS',spx,1024,return_size_s)
	nptsx=return_size_s
	endif

	if (str4.eq.'EyAC') then
	ok =w_item_r4(ch, 'SPECTRUM_EYAC_DBVOLTS',spy,1024,return_size_s)
	nptsy=return_size_s
	endif

	if (str4.eq.'EzAC') then
	ok =w_item_r4(ch, 'SPECTRUM_EZAC_DBVOLTS',spz,1024,return_size_s)
	nptsz=return_size_s
	endif


	if (str4.eq.'ExAC'.and.nptsx.ne.0) then
	write(11) iantenne,nptsx,my_year, my_month, my_date,
	1	 my_hour, my_minute, my_second, my_msecond
	write(11) fr0,frn,frb,df
	write(11) (spx(isp),isp=1,nptsx)
	print *,'spx(0,n):',spx(1),spx(nptsx)
	numevspx=numevspx+1
	endif


	if (str4.eq.'EyAC'.and.nptsy.ne.0) then
	write(14) iantenne,nptsy,my_year, my_month, my_date,
	1	 my_hour, my_minute, my_second, my_msecond
	write(14) fr0,frn,frb,df
        write(14) (spy(isp),isp=1,nptsy)
	numevspy=numevspy+1
	endif


	if (str4.eq.'EzAC'.and.nptsz.ne.0) then
	write(16) iantenne,nptsz,my_year, my_month, my_date,
	1	 my_hour, my_minute, my_second, my_msecond
	write(16) fr0,frn,frb,df
        write(16) (spz(isp),isp=1,nptsz)
	numevspz=numevspz+1
	endif

	
	numev=numev+1

        go to 1
c	print 333,spect
333     format(1x,8e12.5)
10      continue
	type *,'Nombre total evenements : ',numev
	type *,'Nombre spectres ExAC : ',numevspx
	type *,'Nombre spectres EyAC : ',numevspy
	type *,'Nombre spectres EzAC : ',numevspz

        close(unit=11)
	close(unit=14)
	close(unit=16)

600     format(1x,6a,8(1x,i6))
620     format(8(f10.3))

      END

