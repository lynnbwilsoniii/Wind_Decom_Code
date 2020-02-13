	   dowhile(ihrmn.lt.nhrmn)
             ok = wind_tm_get_next_event(tdsch,major,minor,'TDSF')
	     item = 'EVENT_SCET_R8'
	     ok = w_item_r8(tdsch, item, scet, 2, return_size)
	     if (.not. ok) type *, 'cannot get item ', item, ', ok=', ok
	     call w_ur8_to_ymd(scet,yyyy,mon,dd,hh,mm,ss,ms)
	     ihrmn = 100*hh+mm
	     item = 'EVENT_NUMBER'
	     ok = wind_tm_get_item(tdsch, item, itemp, 1, return_size)
	     TYPE 1005,itemp,YYYY,MON,DD,IHRMN
C		write(26,*) itemp,YYYY,MON,DD,IHRMN
 1005	     format(' event no',i10,'  found at',i6,i3,i3,i5.4)
	  enddo

