! split.for - splits vms_wind_idl.pro into multiple files by function

	program split
	implicit	none
	parameter	fin='vms_wind_idl.pro_old'
	integer*4	iu
	character*80	fout
	integer*4	ou
	character*512	s
	integer*4	eof_test
	integer*4	n_out
	character*80	new
	integer*4	i,j,n
	integer*4	line_num
	integer*4	n_files
	integer*4	ios
	logical*4	put

	call lib$get_lun(iu)
	call lib$get_lun(ou)
	open(iu, file=fin, readonly, status='old', iostat=ios, err=10)
  1	format(q,a)
  2	format(a)

	line_num = 1
	put = .false.
	n_out = 0
	read(iu,1,err=20,end=999,iostat=ios) n,s
	do while(.true.)
	   if (s(1:8) .eq. 'function' .and. s(9:9) .le. ' ') then
	      n_files = n_files + 1
	      if (n_out .gt. 0) close(ou)
	      i = 10
	      do while(s(i:i) .le. ' ')
	         i = i + 1
	      end do
	      j = i
	      do while(s(j:j) .gt. ' ' .and. s(j:j) .ne. ',')
	         j = j + 1
	      end do
	      j = j - 1
	      new = s(i:j)//'.pro'
	      open(ou, file=new, status='new', 
	1	iostat=ios, carriagecontrol='list', err=30)
	      put = .true.
	      n_out = 1
	   end if
	   if (put) then
	      write(ou,2,iostat=ios,err=40) s(1:max(1,n))
	      n_out = n_out + 1
	   end if
	   if (s(1:3) .eq. 'end' .and. s(4:4) .le. ' ') then
	      put = .false.
	   end if
	   line_num = line_num + 1
	   read(iu,1,err=20,end=999,iostat=ios) n,s
	end do

 999	continue
	type '(1x,i5,a)', line_num-1, ' lines read from '//fin//'.'
	type '(1x,i3,a)', n_files, ' IDL .pro files produced.'
	stop
  10	continue
	type *, 'Cannot open '//fin//', iostat=', ios
	stop 'INPUT OPEN ERROR'
  20	continue
	type *, 'Cannot read '//fin//', iostat=', ios
	type *, 'Line number is ', line_num
	stop 'READ ERROR'
  30	continue
	type *, 'Cannot open '//new(i:j)//' for output, iostat=', ios
	stop 'OUTPUT OPEN ERROR'
  40	continue
	type *, 'Error writing to '//new(i:j)//', iostat=', ios
	stop 'WRITE ERROR'
	end
