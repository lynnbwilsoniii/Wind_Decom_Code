! rt_mode_def.for - realtime/.wnd file sc TM mode definitions

	! the following are the mode values used prelaunch for .wnd files
	! and realtime streams
	integer*4	s1,m1,e,c1,s2,m2,na,c2
	parameter	(s1='0000'x)		! science 1x, 92 sec
	parameter	(m1='0010'x)		! maneuver 1x, 92 sec
	parameter	(e ='0020'x)		! engineering
	parameter	(c1='0030'x)		! contingency 1x
	parameter	(s2='0040'x)		! science 2x, 46 sec
	parameter	(m2='0050'x)		! maneuver 2x, 46sec
	parameter	(na='0060'x)		! not applicable (I think)
	parameter	(c2='0070'x)		! contingency 2x
