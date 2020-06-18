! wind_sys_def.for - flags for portability

	logical*4	vms
	parameter	(vms=.false.)

	logical*4	sunos
	parameter	(sunos=.false.)

	logical*4	realtime
	parameter	(realtime=.false.)

	logical*4	macos_ppc
	parameter	(macos_ppc=.false.)

	logical*4	macos_intel
	parameter	(macos_intel=.true.)

! Set "unixos" to .true. ONLY if one of "sunos", "macos_ppc", or
! "macos_intel" are are .true.
	logical*4	unixos
	parameter	(unixos = .true.)
