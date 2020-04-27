import subprocess as sub
import sys
import os
import uuid
import calendar
import datetime
import cPickle as pickle 
import shutil

#input_file = open('/shrg1/wind/sw/STICS_data_processing/input_info/input_info.txt', 'r')
input_file=open('/home/kploof/STICS_data_processing/input_info/input_info.txt', 'r')
file_lines = input_file.readlines()
input_file.close()
batch_info = [x.strip('\n') for x in file_lines]

#variables determined from batch info text file
start_date = ''
stop_date = ''
ion_list = []
coincidence = ''
save_dir = '/tmp/'
wtdc_write_dir = '/shrg1/wind/LV2_development/v2/'
ctr = 0

#handling input from text file
for batch_line in batch_info:
	ctr = ctr + 1
	if batch_line[0] == '#':
		continue
	elif 'ion list' in batch_line:
		if ion_list:
			print ('Input file has incorrect format')
			print ('Multiple ion lists provided')
			sys.exit(1)

		name_and_vars = batch_line.split(':')
		unfiltered_ion_list = name_and_vars[1]
		ion_list = [ion.strip() for ion in unfiltered_ion_list.split(',')]
	
	elif 'start date' in batch_line:
		if start_date:
			print ('Input file has incorrect format')
			print ('Multiple start dates provided')
			sys.exit(1)
	
		name_and_vars = batch_line.split(':')
		start_date = name_and_vars[1].strip()
	
	elif 'stop date' in batch_line:
		if stop_date:
			print ('Input file has incorrect format')
			print ('Multiple stop dates provided')
			sys.exit(1)

		name_and_vars = batch_line.split(':')
		stop_date = name_and_vars[1].strip()
	
	elif 'coincidence' in batch_line:
		if coincidence:
			print ('Input file has incorrect format')
			print ('Multiple coincidences provided')
			sys.exit(1)

		name_and_vars = batch_line.split(':')
		coincidence_full = name_and_vars[1].strip().lower()
		if coincidence_full == 'double':
			coincidence = '0'
		elif coincidence_full == 'triple':
			coincidence = '1'
		else:
			print ('Input file has incorrect format')
			print ('Coincidence must be either double or triple. Different method selected in input')
			sys.exit(1)
	else:
		print ('Invalid line (' + str(ctr) + ') in input file, skipping over')

#make sure all necessary variables are included
if not ion_list:
	print ('Input file has missing parameters')
	print ('Must provide ion list')
	sys.exit(1)
if not start_date:
	print ('Input file has missing parameters')
	print ('Must provide start date')
	sys.exit(1)
if not stop_date:
	print ('Input file has missing parameters')
	print ('Must provide stop date')
	sys.exit(1)
if not coincidence:
	print ('Input file has missing parameters')
	print ('Must provide ion list')
	sys.exit(1)


#create directories for run
cur_time = datetime.datetime.now()
cur_time_str = cur_time.isoformat()
cur_date = str(cur_time_str.split('T')[0])
cur_date = cur_date[0:4]+cur_date[5:7]+cur_date[8:10]
processing_year = start_date[0:4]
salt = str(uuid.uuid4())

save_dir = save_dir + cur_date + '_' + processing_year + '_' + salt + '/'

if not os.path.exists(save_dir):
	os.makedirs(save_dir)

wtdc_write_dir = wtdc_write_dir + processing_year + '/'

if not os.path.exists(wtdc_write_dir):
	os.makedirs(wtdc_write_dir)
	
	os.makedirs(wtdc_write_dir+'double_coincidence/')
	os.makedirs(wtdc_write_dir+'double_coincidence/qsub/')
	os.makedirs(wtdc_write_dir+'double_coincidence/AFM/')
	os.makedirs(wtdc_write_dir+'double_coincidence/AFM/msph/')
	os.makedirs(wtdc_write_dir+'double_coincidence/AFM/sw/')
	os.makedirs(wtdc_write_dir+'double_coincidence/AFM_plots/')
	os.makedirs(wtdc_write_dir+'double_coincidence/AFM_plots/msph/')
	os.makedirs(wtdc_write_dir+'double_coincidence/AFM_plots/sw/')
	os.makedirs(wtdc_write_dir+'double_coincidence/ERPA/')
	os.makedirs(wtdc_write_dir+'double_coincidence/ERPA/msph/')
	os.makedirs(wtdc_write_dir+'double_coincidence/ERPA/sw/')
	os.makedirs(wtdc_write_dir+'double_coincidence/ERPA_plots/')
	os.makedirs(wtdc_write_dir+'double_coincidence/ERPA_plots/msph/')
	os.makedirs(wtdc_write_dir+'double_coincidence/ERPA_plots/sw/')
	os.makedirs(wtdc_write_dir+'double_coincidence/moments/')
	os.makedirs(wtdc_write_dir+'double_coincidence/moments/msph/')
	os.makedirs(wtdc_write_dir+'double_coincidence/moments/sw/')
	os.makedirs(wtdc_write_dir+'double_coincidence/VDF/')
	os.makedirs(wtdc_write_dir+'double_coincidence/VDF/msph/')
	os.makedirs(wtdc_write_dir+'double_coincidence/VDF/sw/')
	os.makedirs(wtdc_write_dir+'double_coincidence/pha_dump/')
		
	os.makedirs(wtdc_write_dir+'triple_coincidence/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/qsub/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/AFM/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/AFM/msph/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/AFM/sw/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/AFM_plots/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/AFM_plots/msph/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/AFM_plots/sw/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/ERPA/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/ERPA/msph/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/ERPA/sw/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/ERPA_plots/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/ERPA_plots/msph/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/ERPA_plots/sw/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/moments/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/moments/msph/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/moments/sw/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/VDF/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/VDF/msph/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/VDF/sw/')
	os.makedirs(wtdc_write_dir+'triple_coincidence/pha_dump/')

#Specify Accumulation Time------------
# accum_time=30*60 #sec
accum_time=3*60 #sec

#Data type output options--------------
#0-counts, 1-PSD, 2-dJ/dE
# data_type_list=['C','DF','dJ']
data_type_list=['DF'] #just PSD, this should the only one we have to run with new reprocessed output files
data_type_dict=dict(C=0, DF=1, dJ=2)

#Create moment files?-----------
moment_toggle=1 #1= create file, 0=don't create

#########################End of inputs###############################
#preallocate list to store resultant filenames
dateFile_list=[];

#Generate list of start and stop dates
num_days=(datetime.date(int(stop_date[0:4]), int(stop_date[4:6]), int(stop_date[6:8])) - 
	datetime.date(int(start_date[0:4]), int(start_date[4:6]), int(start_date[6:8])) ).days+1
count = num_days * len(ion_list) - 1 # 0 index so subtract 1

base = datetime.date(int(start_date[0:4]), int(start_date[4:6]), int(start_date[6:8]))
date_list = [base + datetime.timedelta(days=x) for x in range(0, num_days)] #range cuts off last one
string_list=[temp.strftime('%Y%m%d') for temp in date_list]
year_month_day_list=string_list

if coincidence[0] == '0':
		wtdc_write_dir = wtdc_write_dir + 'double_coincidence/'
else:
		wtdc_write_dir = wtdc_write_dir + 'triple_coincidence/'

for k in xrange(len(year_month_day_list)):
	for j in xrange(len(ion_list)):
		for i in xrange(len(data_type_list)):

			year_month_day=year_month_day_list[k]
			ion=ion_list[j]
			data_type=data_type_dict[data_type_list[i]]

			#open pipe to "driver.pl" so I can make an output file
			pipe=sub.Popen("/home/kploof/STICS_data_processing/scripts/batch_file_setup.pl", stdin=sub.PIPE, stdout=sub.PIPE, stderr=sub.PIPE) #setup piping

			pipe.stdin.write(ion+"\n") #select ion
			pipe.stdin.write(str(accum_time)+"\n") #select accumulation time
			pipe.stdin.write("0.0\n") #selection efficiency threshold
			pipe.stdin.write("0\n") #select dimensionality of data
			pipe.stdin.write(str(coincidence)+"\n") #select TOF binning #edited to TOF (was previous MOQ) Vishnu 10-06-2018 0 = double coincidence, 1 = triple coincidence
			pipe.stdin.write(str(data_type)+"\n") #select output data type
			if data_type == 1: #if data type set to PSD
				if moment_toggle:
					pipe.stdin.write("1\n") #toggle for moment file creation, only asked this if output data type =1
					#set lower SW velocity bound to 0.0
					pipe.stdin.write("0.0\n") #set lower bound for SW velocity filer, only asks this if moment file
					#create toggle is =1
				
				else:
					pipe.stdin.write("0\n")
				
				
			pipe.stdin.write("/shrg1/wind/sw/wtdcLV2_Lite/SWE_Files/"+
				year_month_day[0:4]+"_WIND_SWE_90Sec.dat"+"\n") #SWE data location
				#ex. -> ../SWE_Files/2000_WIND_SWE_90Sec.dat, access the "year" portion of the date string
			pipe.stdin.write(wtdc_write_dir + "\n") #data storage directory
			pipe.stdin.write("2\n") #time range input select
			pipe.stdin.write(year_month_day+"\n") #select day of year (yyyymmdd)
			pipe.stdin.close() #close off input to file

			while pipe.returncode is None: #this part seems to be included in online samples... 
				pipe.poll()
			
			print(pipe.stdout.read())
			print(pipe.stderr.read())

			uniq_filename=('dateFile_'+year_month_day+'_'+ion+'_'+
				data_type_list[i]+'_'+str(uuid.uuid4())+'.dat')
			shutil.move('dateFile.dat', save_dir+uniq_filename) #had to change from os.rename since wind is mounted, and python renaming from one file system to another is not allowed

			#want to save list of filenames generated
			dateFile_list.append(save_dir+uniq_filename)
				
#save final list of file names				
save_name='driver_file_list'
f1=open(save_dir+save_name+'.pkl', 'wb')
pickle.dump(dateFile_list, f1) #save to file
f1.close() #close file

print '# of dateFiles produced = '
print str(len(dateFile_list))
count = len(dateFile_list) - 1

#save information for qsub to use
count_file = open('/shrg1/wind/sw/STICS_data_processing/input_info/counts.txt', 'w+')
count_file.write(str(count))
count_file.close()

savedir_file = open('/shrg1/wind/sw/STICS_data_processing/input_info/input_dir.txt', 'w+')
savedir_file.write(save_dir)
savedir_file.close()

wtdc_write_dir_file = open('/shrg1/wind/sw/STICS_data_processing/input_info/wtdc_write_dir.txt', 'w+')
wtdc_write_dir_file.write(wtdc_write_dir)
wtdc_write_dir_file.close()

coincidence_file = open('/shrg1/wind/sw/STICS_data_processing/input_info/coincidence.txt', 'w+')
coincidence_file.write(coincidence[0])
coincidence_file.close()

year_file = open('/shrg1/wind/sw/STICS_data_processing/input_info/year.txt', 'w+')
year_file.write(processing_year)
year_file.close()

