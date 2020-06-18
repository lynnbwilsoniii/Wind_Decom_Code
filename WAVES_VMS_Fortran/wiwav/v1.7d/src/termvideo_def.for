! termvideo_def.for - vt200 7-bit terminal control video escape sequences
!
! b-bright
! r-reverse
! u-underline
! n-normal
!
	parameter	esc=char(27)		! escape character
	parameter	csi=esc//'['		! control sequence introducer
	parameter	erase_to_end		=csi//'0J'
	parameter	home			=csi//'01;01f'
	parameter	erase_line		=csi//'2K'
	parameter	numeric_keypad		=esc//'>'
	parameter	application_keypad	=esc//'='
	parameter	goto_80cols		=csi//'?3l'
	parameter	goto_132cols		=csi//'?3h'
	parameter	smooth_scroll		=csi//'?4h'
	parameter	jump_scroll		=csi//'?4l'
	parameter	cls			=csi//'2J'
	parameter	selg1			=esc//')0'
	parameter	beep			=char(7)
	parameter	g1on			=char(14)
	parameter	g1off			=char(15)
	parameter	vc='x', hc='q', ulc='l', urc='k', llc='m', lrc='j'
	parameter	ins1line		=csi//'1L'
	parameter	del1line		=csi//'1M'
	parameter	soft_reset		=csi//'!p'
	parameter	cursor_on		=csi//'?25h'
	parameter	cursor_off		=csi//'?25l'

	parameter	primary			=csi//'0m'
	parameter	bright			=csi//'1m'
	parameter	underline		=csi//'4m'
	parameter	blink			=csi//'5m'
	parameter	reverse			=csi//'7m'
	parameter	nobright		=csi//'21m'
	parameter	nounderline		=csi//'24m'
	parameter	noblink			=csi//'25m'
	parameter	noreverse		=csi//'27m'
	parameter	dbl_top			=esc//'#3'
	parameter	dbl_bot			=esc//'#4'

