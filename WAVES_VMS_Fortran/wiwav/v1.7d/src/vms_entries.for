! vms_entries.for - a bunch of entry points serving as place holders
! for the wind_tm_lib.exe shareable image in VMS.  These place holders
! are needed for compatibility with earlier versions of wind_tm_lib_transfer.mar
! in order to keep the transfer vectors aligned as they have always been.

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Extra functions for additions to shareable image
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

	integer*4	function	wind_tm_zz00()
	wind_tm_zz00 = 0
	entry	wind_tm_zz01
	return
	entry	wind_tm_zz02
	wind_tm_zz02 = 0
	return
	entry	wind_tm_zz03
	wind_tm_zz03 = 0
	return
	entry	wind_tm_zz04
	entry	wind_tm_zz05
	entry	wind_tm_zz06
	entry	wind_tm_zz07
	entry	wind_tm_zz08
	entry	wind_tm_zz09
	entry	wind_tm_zz10
	entry	wind_tm_zz11
	entry	wind_tm_zz12
	entry	wind_tm_zz13
	entry	wind_tm_zz14
	entry	wind_tm_zz15
	entry	wind_tm_zz16
	entry	wind_tm_zz17
	entry	wind_tm_zz18
	entry	wind_tm_zz19
	entry	wind_tm_zz20
	entry	wind_tm_zz21
	wind_tm_zz00 = 0
	return
	entry	wind_tm_zz23
	entry	wind_tm_zz24
	entry	wind_tm_zz25
	entry	wind_tm_zz26
	entry	wind_tm_zz27
	entry	wind_tm_zz28
	entry	wind_tm_zz29
	wind_tm_zz00 = 0
	return
	end

	subroutine wind_tm_zz22(i,j,k)
	integer*4	i,j,k
	type *, 'You have entered zz22.', i,j,k
	return
	end

	subroutine	getenv(ename, evalue)
	character*(*)	ename, evalue
	type *, 'You have entered getenv (and you are not supposed to).'
	return
	end
