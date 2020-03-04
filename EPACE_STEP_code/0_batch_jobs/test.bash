#	command script step_update
#
#	updates STEP HEX, ROM_SPIKES, and basic data files    /gm   6/19/2008   path updates 6/25/08
#
#	set up IDL environment
	/Applications/rsi/idl/bin/idl_setup.bash
	export PATH=${PATH}:/Applications/rsi/idl_6.3/bin/
	leader="\n********************************************\n"
	

#	now run sit_lister update
	cd /Users/masongm1/Data/Production/STEP/4_basicrates_update/
	/Users/masongm1/Desktop/fortran_vax/STEP/sdf_lister/sdf_lister
	
#	mail message with days processed attached
	mail -s "SDF lister update is finished" glenn.mason@jhuapl.edu
	