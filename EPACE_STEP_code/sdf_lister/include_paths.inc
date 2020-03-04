c	include file with path names and definitions
	character*120	data_path			! folder containing input udf files
	character*120	calibration_path		! folder containing instrument calibration data
	character*120	control_path		! folder containg udf_lister.cfg & other control files
	common /control_paths/ data_path, calibration_path, control_path