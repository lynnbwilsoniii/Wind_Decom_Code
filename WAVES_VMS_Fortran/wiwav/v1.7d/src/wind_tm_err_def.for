! wind_tm_err_def.for - paramaterized error definitions for wind_tm_lib

	parameter	wtm_normal=1	! normal completion
	parameter	wtm_success=1	! successful completion
	parameter	wtm_error=0	! error condition, unsuccessful action
	parameter	wtm_bof=2	! record occurs before beginning of file
	parameter	wtm_missing=4	! record is in range, but not in file
	parameter	wtm_eof=6	! record occurs after end of file
