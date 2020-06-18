! trad2ai.for -
	program trad2ai
	implicit	none
	integer*4	ok
	integer*4	ch
	parameter	event='RAD2'
	parameter	size=512
	integer*4	s(size)
	integer*4	z(size)
	integer*4	sp(size)
	integer*4	f(size)				! frequencies
	real*4		smv(size)
	real*8		s_scet(size)
	integer*4	rs1,rs2,rs3,rs4,rs5,rs6
	integer*4	i,j,k,m,n,o
	integer*4	event_number /0/
	integer*4	w_channel_open
	integer*4	w_channel_filename
	integer*4	w_item_r8
	integer*4	w_item_r4
	integer*4	w_item_i4
	integer*4	w_event
	integer*4	w_version
	character*16	ver
	real*8		scet
	character*60 	file
	logical*4	keep_going /.true./
	character*1	c1
	character*8	c_s /'S'/, c_z /'Z'/, c_sp /'S_PRIME'/
	character*20	c_f /'FREQUENCIES'/
	character*20	c_smv /'S_MICROVOLTS_R4'/
	character*20	c_s_scet /'S_SCET_R8'/


	ok = w_channel_open(ch,'offline')
	if (ok .ne. 1) stop 'cannot open channel'

	ok = w_version(ver)
	ok = w_channel_filename(ch,file)
	type *, ' '
	type *, 'Using Wind_Lib version ', ver
	type *, 'File: ', file

	do while (keep_going)
	   event_number = event_number + 1
	   type *, ' '
	   type *, event, ' event number ', event_number
	   ok = w_event(ch,event)
	   if (ok .ne. 1) goto 1000

	   ok = w_item_i4(ch,c_s,s,size,rs1)
	   if (ok .ne. 1) type *, 'Cannot get item ', c_s
	   ok = w_item_i4(ch,c_z,z,size,rs2)
	   if (ok .ne. 1) type *, 'Cannot get item ', c_z
	   ok = w_item_i4(ch,c_sp,sp,size,rs3)
	   if (ok .ne. 1) type *, 'Cannot get item ', c_sp

	   ok = w_item_i4(ch,c_f,f,size,rs4)
	   if (ok .ne. 1) type *, 'Cannot get item ', c_f

	   ok = w_item_r4(ch,c_smv,smv,size,rs5)
	   if (ok .ne. 1) type *, 'Cannot get item ', c_smv

	   ok = w_item_r8(ch,c_s_scet,s_scet,size,rs6)
	   if (ok .ne. 1) type *, 'Cannot get item ', c_s_scet

	   write(6,1,iostat=o) c_s, c_z, c_sp, c_f, c_smv, c_s_scet
	   write(6,2,iostat=o) rs1, rs2, rs3, rs4, rs5, rs6
	   write(6,1,iostat=o) ('--------------', i=1,6)
	   do j=1,17
	      write(6,3,iostat=o) j, s(j), z(j), sp(j), f(j), smv(j), s_scet(j)
	   end do

 1000	   continue
	   type *, ' '
 10	   write(6,'(1x,a,$)') 'Get another event [Y/N]? '
	   read(5,'(a)',iostat=o,err=10) c1
	   keep_going = .not. (c1 .eq. 'N' .or. c1 .eq. 'n')
	end do

 1	format(1x,4x,     4(1x,a3), 1x,a8,   1x,a12)
 2	format(1x,4x,     4(1x,i3), 1x,i8,   1x,i12)
 3	format(1x,i3,'.', 4(1x,i3), 1x,f8.4, 1x,f12.7)
	end
