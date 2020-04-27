echo "Cleaning up previous runs"

coincidence_file='/shrg1/wind/sw/STICS_data_processing/input_info/coincidence.txt'
num_file='/shrg1/wind/sw/STICS_data_processing/input_info/counts.txt'
output_path_file='/shrg1/wind/sw/STICS_data_processing/input_info/wtdc_write_dir.txt'
input_dir_file='/shrg1/wind/sw/STICS_data_processing/input_info/input_dir.txt'
qsub_temp='/shrg1/wind/sw/STICS_data_processing/input_info/qsub_temp.txt'
year_file='/shrg1/wind/sw/STICS_data_processing/input_info/year.txt'

if [ -f $coincidence_file ]; then
	rm $coincidence_file
fi

if [ -f $num_file ]; then
        rm $num_file
fi

if [ -f $output_path_file ]; then
        rm $output_path_file
fi

if [ -f $input_dir_file ]; then
        rm $input_dir_file
fi

if [ -f $qsub_temp ]; then
        rm $qsub_temp
fi

if [ -f $year_file ]; then
	rm $year_file
fi

echo "Running python script to set up input files"

#python /shrg1/wind/sw/STICS_data_processing/python_scripts/batch_sub_process.py
python /home/kploof/STICS_data_processing/python_scripts/batch_sub_process.py

coincidence=`cat $coincidence_file`

template=''
qsub_jobprefix=''

if [[ $coincidence == "0" ]]; 
then
	#template='/shrg1/wind/sw/STICS_data_processing/templates/qsub_double_template.txt'
	template='/home/kploof/STICS_data_processing/templates/qsub_double_template.txt'
	qsub_jobprefix='wtdc'
elif [[ $coincidence == "1" ]]; 
then
	#template='/shrg1/wind/sw/STICS_data_processing/templates/qsub_triple_template.txt'
        template='/home/kploof/STICS_data_processing/templates/qsub_triple_template.txt'
	qsub_jobprefix='wttc'
else
	echo "Failed to create necessary files, exiting"
	exit 1
fi

echo "Setting up qsub run"
random=$$
div=$((10001))
r=$(($random%$div))
date=`date +%Y-%m-%d`
qsub_file="qsub_run_many_"$date"_"$r".in"

num_files=`cat $num_file`
output_path=`cat $output_path_file`
year=`cat $year_file`

qsub_full=$output_path"qsub/"$qsub_file

echo "#PBS -q batch" > $qsub_temp
echo "#PBS -m a  ### only message upon abort" >> $qsub_temp
echo "#PBS -j oe  ### combine error and output into single file" >> $qsub_temp
echo "#PBS -P :shrgdev" >> $qsub_temp 
echo "#PBS -V" >> $qsub_temp
echo "#PBS -t 0-$num_files%16" >> $qsub_temp
echo "#PBS -N $qsub_jobprefix$year" >> $qsub_temp
echo "# NOTE:  CONFIGURATION LINES ARE MARKED WITH '===>'" >> $qsub_temp
echo "# ===> set output directory" >> $qsub_temp
echo "echo 'Running in Directory:'" >> $qsub_temp
echo "cd $output_path" >> $qsub_temp

cat $qsub_temp $template > $qsub_full
rm $qsub_temp

echo "Running qsub"
cd $output_path/qsub/
echo "Running in:"
pwd
qsub $qsub_file

sleep 10
qstat
sleep 10

echo "Waiting until complete..."
echo "$qsub_jobprefix$year"
while [[ `qstat -r | grep -cw "$qsub_jobprefix$year"` == "1" ]]; do
fill=`qstat`
sleep 1
done

echo "Moving files to appropiate location"
cd $output_path
mv wstics_LV2_SCFrame_*.dat "./VDF/" #edited to use this instead of gzipping - Vishnu 10/17/2018
##Move moment file output
mv wstics_LV2_nvt*.dat "./moments/"

##Discard "sectored" moment files (don't trust them yet)
rm wtdcLV2_Lite_Sectored_Moments*.dat

##Move AFM data
mv wstics_afm_*.dat "./AFM/"

##Move ERPA data
mv wstics_erpa_*.dat "./ERPA/"

##Move AFM plots
echo wstics_afm_*.png | xargs mv -t "./AFM_plots/"

##Move ERPA plots
echo wstics_erpa_*.png | xargs mv -t "./ERPA_plots/"

##sort stuff into msphere and solar wind using python script
boundary_file='/shrg1/wind/sw/AFM_ERPA_processor/Wind_shock_crossing_edited_add_final_xing.txt'
echo "Separating into Msph and SW"
python /shrg1/wind/sw/AFM_ERPA_processor/divide_SW_msphere.py "$output_path" "$boundary_file"

echo "Complete"


