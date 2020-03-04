#!/bin/bash
#	command script step_update
#
#	updates STEP HEX, ROM_SPIKES, and basic data files    /gm   6/19/2008   path updates 6/25/08
#
#	set up IDL environment
	/Applications/exelis/idl/bin/idl_setup.bash
	export PATH=${PATH}:/opt/X11/lib:/Applications/exelis/idl84/bin/
	export DYLD_LIBRARY_PATH=/opt/X11/lib
	export IDL_PATH='<IDL_DEFAULT>:.:+/Users/mdesai/Desktop/IDL/'
	leader="\n********************************************\n"

ls /Users/mdesai/Desktop/IDL/wind/step_kp_files/step/ > /Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/step_hex_files.txt

	
#	******************************* sdf_generator ***************************************
#	first find out if there are any new files to update & then run sdf_generator
	cd /Users/mdesai/Desktop/IDL/wind/fortran_vax/ 
    idl -quiet /Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/sdf_generator.batch

#	mail -s "STEP  SDF_generator finished" glenn.mason@jhuapl.edu < /Users/masongm1/Data/Production/STEP/1_sdf_update/sdf_generator/sdf_generator.txt

#	******************************* sdf_rom_spikes ***************************************	
#	now do the rom spikes
		date=$(date +"%r")
		printf "$leader sdf_rom_spikes run starting $date $leader\n"
	cd /Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/ 
	/Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/sdf_rom_spikes 
#	mail -s "STEP  sdf_rom_spikes finished" glenn.mason@jhuapl.edu < /Users/masongm1/Data/Production/STEP/2_sdf_rom_spikes/sdf_rom_spikes.dat
	
	
#	******************************* merge mask saturation ***************************************	
#	now merge mask saturation	
		date=$(date +"%r")
		printf "$leader merge_mask_saturation run starting $date $leader\n"
	idl -quiet /Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/merge_mask_saturation.batch
	
#	mail -s "STEP  merge_mask_saturation_update" glenn.mason@jhuapl.edu < /Users/masongm1/Data/Production/STEP/3_merge_mask_sat/merge_mask_saturation.txt
	
#	******************************* sit_lister basicrates update ***************************************	
#	now run sit_lister anisotropy 10-min updates
		date=$(date +"%r")
		printf "$leader sdf_lister updating step basicrates starting $date $leader\n"
	cd /Users/mdesai/Desktop/IDL/wind/fortran_vax/anisotropy_control/
	/Users/mdesai/Desktop/IDL/wind/fortran_vax/sdf_lister/sdf_lister 

mv ANISCNO1020.all\;1 ANISCNO1020.all
mv ANISHET1020.all\;1 ANISHET1020.all
mv ANISFET1020.all\;1 ANISFET1020.all
mv ANISHTE1020.all\;1 ANISHTE1020.all
mv schktel1020.all\;1 schktel1020.all

mv ANISCNO2020.all\;1 ANISCNO2020.all
mv ANISHET2020.all\;1 ANISHET2020.all
mv ANISFET2020.all\;1 ANISFET2020.all
mv ANISHTE2020.all\;1 ANISHTE2020.all
mv schktel2020.all\;1 schktel2020.all

	date=$(date +"%r")
	printf "$leader sdf_lister updating step anisotropy finished at $date $leader\n"
	idl -quiet /Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/step_anisotropy_update.batch
	
#	now run hourly rates updates

	date=$(date +"%r")
		printf "$leader sdf_lister updating step basicrates starting $date $leader\n"
	cd /Users/mdesai/Desktop/IDL/wind/fortran_vax/hourly_control/
	/Users/mdesai/Desktop/IDL/wind/fortran_vax/sdf_lister/sdf_lister 

mv ANISCNO1020.all\;1 ANISCNO1020.all
mv ANISHET1020.all\;1 ANISHET1020.all
mv ANISFET1020.all\;1 ANISFET1020.all
mv ANISHTE1020.all\;1 ANISHTE1020.all
mv schktel1020.all\;1 schktel1020.all

mv ANISCNO2020.all\;1 ANISCNO2020.all
mv ANISHET2020.all\;1 ANISHET2020.all
mv ANISFET2020.all\;1 ANISFET2020.all
mv ANISHTE2020.all\;1 ANISHTE2020.all
mv schktel2020.all\;1 schktel2020.all

	date=$(date +"%r")
	printf "$leader sit_lister updating step anisotropy finished at $date $leader\n"
	idl -quiet /Users/mdesai/Desktop/IDL/wind/fortran_vax/0_batch_jobs/step_hourly_update.batch

mv /Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/mask_saturation.tmp\;1 /Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/old_sdf_rom_spikes/mask_saturation.tmp\;1 
mv /Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/sdf_rom_spikes.kal\;1 /Users/mdesai/Desktop/IDL/wind/fortran_vax/2_sdf_rom_spikes/sdf_rom_spikes_source/old_sdf_rom_spikes/sdf_rom_spikes.kal\;1 

exit	
	
#	mail message with days processed attached
#	mail -s "SDF lister update is finished" glenn.mason@jhuapl.edu

#	******************************* idl_save file update ***************************************	
	date=$(date +"%r")	
	printf "$leader running: sdf_lister IDL files update starting $date $leader\n"
	cd /Users/masongm1/Data/Production/STEP/5_IDL_sav_file_update/
	/Users/masongm1/Desktop/fortran_vax/STEP/sdf_lister/sdf_lister
	printf "$leader running: IDL save file update $leader\n"
	cd /Users/masongm1/Data/Production/STEP/5_IDL_sav_file_update/IDL_sav_file_update/
	idl -quiet /Users/masongm1/Data/Production/STEP/0_batch_jobs/main_romsave_step_update.batch
			


			
