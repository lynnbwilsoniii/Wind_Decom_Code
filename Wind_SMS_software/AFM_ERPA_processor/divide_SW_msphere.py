'''
This script will run through and split up files in 
solar wind and magnetosphere.  

Note: For now this code just groups days that straddle the
magneotphere boundary with the other magnetosphere ones.

This script takes two arguments (from the shell terminal)

arg 1 = base directory that the wtdc, afm, and erpa files are in,
    this should be "./output_dir/YYYY/" where YYYY is the current year being analyzed.
    The data files are in the AFM, AFM_plots, ERPA, ERPA_plots, distributions, moments 
    folders under this base directory
    
arg 2 = directory of the Wind boundary file.  This contains the times where the Wind 
    spacecraft cross out of and back into the magnetosphere.  These file is used to split up the files
    based on the date stamp in the file name

'''

#import necessary libraries
import numpy as np
import os
import glob
import calendar
import time as tm
import datetime
import shutil #shell utilities (I think)
import sys


base_dir=sys.argv[1] #read first argument from shell script
print 'Sorting SW/MSph files in directory:'
print base_dir

boundary_file=sys.argv[2] #specify location of boundary file
print 'Boundary file path'
print boundary_file

#Define origin and destination file paths
#output_dir='../output_dir/'
#output_dir='./output_dir/test_sw_msphere_split/' #local test
#split_dir='/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/zephyrus_transfer/output_dir/test_sw_msphere_split/'
#SW_dir='/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/zephyrus_transfer/output_dir/test_sw_msphere_split/SW/'
#Msphere_dir='/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/zephyrus_transfer/output_dir/test_sw_msphere_split/msphere/'

#read in boundary crossing list------------
#Retrieve STICS data
boundary_list_data_format=np.dtype([('last_outbound_bs_cross','S14'), ('first_pure_SW_hr', 'S9'), 
    ('last_pure_SW_hr', 'S9'), ('first_inbound_bs_cross', 'S15')])
#there is one extra space before 4th column, need to just read it into string
#and deal with it later
#boundary_file='/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/zephyrus_transfer/Wind_shock_crossing_edited_add_final_xing.txt'

boundary_data=np.loadtxt(boundary_file, skiprows=2, dtype=boundary_list_data_format,
    delimiter='         ') #delimiter is 9 spaces
    
#extract datetime from boundary list
start_SW_datetime=[datetime.datetime.strptime(x, '%y %j %H') for x in boundary_data['first_pure_SW_hr']]
stop_SW_datetime=[datetime.datetime.strptime(x, '%y %j %H') for x in boundary_data['last_pure_SW_hr']]
#-----------------------    
#Define folders where the different file bases live <----NEED TO FIX THESE UP!
year_subfolders=['VDF', 'moments',
    'ERPA_plots', 'AFM_plots',
    'ERPA', 'AFM']
#Define different file bases encountered that need to be sorted (match up with index of folder above)
filebase_list=['wstics_LV2_SCFrame*.dat*', 'wstics_LV2_nvt*.dat', #add trailing * as VDF files could be gzipped
    'wstics_erpa_*.png','wstics_afm_*.png',
    'wstics_erpa_*.dat','wstics_afm_*.dat']
for kk in xrange(len(year_subfolders)):    
    
    subfolder=year_subfolders[kk]
    #collect in file names
    filebase=filebase_list[kk]
    file_list=glob.glob(base_dir+subfolder+'/'+filebase)
    if len(file_list) > 0: #only do processing if there are actually files.
        file_list_date_string=[] #preallocate list for datetimes
        
        SW_dir=base_dir+subfolder+'/sw/'
        Msphere_dir=base_dir+subfolder+'/msph/'
        #Define method to extract date from filename for each kind of file
        if (filebase in filebase_list[0:2]):
            for j in xrange(len(file_list)):
                temp1=file_list[j].split('-') #estimate start/stop year from file names in file_list
                temp2=temp1[-1].split('.')
                date_string=temp2[0]
                file_list_date_string.append(date_string)
                
        elif (filebase in filebase_list[2:4]):
            for j in xrange(len(file_list)):
                temp1=file_list[j].split('/') #split up by directory
                temp2=temp1[-1].split('_')
                temp3=temp2[3].split('T')
                date_string=temp3[0]
                file_list_date_string.append(date_string)
                
        elif (filebase in filebase_list[4:6]):
            for j in xrange(len(file_list)):
                temp1=file_list[j].split('/')
                temp2=temp1[-1].split('_')
                temp3=temp2[-1].split('.')
                date_string=temp3[0]
                file_list_date_string.append(date_string)
                
        else:
            raise ValueError("File type to be sorted is not in predefined list")
                    
        #Run through boundary list and partition M-sphere and SW files
        #only need to open and divide up files that straddle the M-sphere/ SW boundary time
        
        #convert file date string to date time
        file_list_datetime=np.array([datetime.datetime.strptime(x,'%Y%m%d') for x in file_list_date_string])
    
        #Need to find all files in my list that fall in "pristine" solar wind times
        #After 4/29/2004 Wind remain in the solar wind permanently as it orbits L1 (Earth-Sun system)
        SW_days=[]
        SW_day_inds=np.array([])
        for j in xrange(len(start_SW_datetime)):
            ind=np.where( (file_list_datetime >= start_SW_datetime[j]) &
                (file_list_datetime < datetime.datetime(stop_SW_datetime[j].year, stop_SW_datetime[j].month,stop_SW_datetime[j].day) ))
            if (ind[0].size > 0) :
                SW_days.extend([file_list[x] for x in ind[0]])
                SW_day_inds=np.append(SW_day_inds, ind[0])
            
        #find all files that fall outside "pristine" solar wind times 
        #(for now just group boundary ones in with the M-sphere ones)
        
        MSph_days=[]
        MSph_day_inds=np.array([])
        #cover times less than the first SW start time or larger than last SW stop time (modified data file
        #so that last time is in year 2065, therefore we should have to worry.  Default
        #python pivot year for two digit years if 1969.
        ind=np.where((file_list_datetime < start_SW_datetime[0]) | (file_list_datetime > stop_SW_datetime[-1]) )
        if (ind[0].size > 0):
            MSph_days.extend([file_list[x] for x in ind[0]])
            MSph_day_inds=np.append(MSph_day_inds, ind[0])
        for j in xrange(len(start_SW_datetime)-1): #need to stop index 1 early
            ind=np.where( (file_list_datetime < start_SW_datetime[j+1]) &
                (file_list_datetime >= datetime.datetime(stop_SW_datetime[j].year, stop_SW_datetime[j].month,stop_SW_datetime[j].day) ) )
            if (ind[0].size > 0) :
                MSph_days.extend([file_list[x] for x in ind[0]])
                MSph_day_inds=np.append(MSph_day_inds, ind[0])
            
        #Use these file lists to actually move the files       
        #Move solar wind times
        for f in SW_days:
            shutil.move(f, SW_dir)
        for f in MSph_days:
            shutil.move(f, Msphere_dir)
        
#In this current mode, the MSph_days list should be the complement of the SW_days list
#In future version, files for days that fall on SW-MSph boundaries will be split into
#their relevant regions  
      
#find file of days that straddle the boundary (save this for later)









