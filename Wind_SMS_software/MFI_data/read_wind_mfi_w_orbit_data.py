''' 
This script will read in the Wind MFI data w/ orbital information

'''

#import the requisite library modules
import numpy as np
import pandas as pd
import time as tm
import matplotlib
import matplotlib.pyplot as plt
import os
import glob
import datetime
import calendar
import numpy.lib.recfunctions as np_extra
import cPickle as pickle #supposedly cpickle is faster...

file_dir= '/shrg1/wind/MFI_data/'
file_base='WI_H0_MFI_'

file_list=glob.glob(file_dir+'/text_files/WI_*2019*')

#define data format
mfi_data_format=np.dtype([('day-month-year','S10'), ('hh_mm_ss', 'S12'), 
    ('B (nT)', np.float64), ('bx_gsm (nT)',np.float64), ('by_gsm (nT)', np.float64), ('bz_gsm (nT)', np.float64),
    ('bx_gse (nT)', np.float64), ('by_gse (nT)', np.float64), ('bz_gse (nT)', np.float64),
    ('x_gsm (R_e)',np.float64), ('y_gsm (R_e)',np.float64), ('z_gsm (R_e)',np.float64),
    ('x_gse (R_e)',np.float64), ('y_gse (R_e)',np.float64), ('z_gse (R_e)',np.float64)])
    
for j in xrange(len(file_list)):
    #for loop
    data=np.loadtxt(file_list[j], skiprows=107, dtype=mfi_data_format)
    
    #Convert time stamps to year fraction
    #date_time_arr=datetime.datetime.strptime(data['day-month-year'] + 'T'+data['hh_mm_ss'], '%d-%m-%YT%H:%M:%S.%f')
    
    #create array of datetimes
    date_time_arr=[datetime.datetime.strptime(x['day-month-year'] + 'T'+x['hh_mm_ss'], '%d-%m-%YT%H:%M:%S.%f') for x in data]
    yearfrac_arr=np.zeros(len(date_time_arr))
    #compute year fraction from datetime
    for i in xrange(len(date_time_arr)):
        year=date_time_arr[i].year
        frac_year_datetime=date_time_arr[i]-datetime.datetime(year=year, month=1, day=1)
        #yearfrac = year + (datetime - year)/(days in current year)
        frac_year=( ( frac_year_datetime.days + frac_year_datetime.seconds/(3600*24.0))
            /(int(calendar.isleap(year))+365) )
        yearfrac_arr[i]=year+frac_year
        
        
    #data_new=np_extra.append_fields(data, 'yearfrac', yearfrac_arr, dtypes=np.float64)
    mfi_data=matplotlib.mlab.rec_append_fields(data, ('yearfrac'), (yearfrac_arr))
    #changes the data to the "rec_array" type.  Not sure what that is exactly
    
    
    
    #save the mag data for each year in a "pickle" file
    save_name='wind_mfi_'+str(date_time_arr[0].year)
    f1=open(file_dir+save_name+'.pkl', 'wb')
    pickle.dump(mfi_data, f1) #save to file
    f1.close() #close file
#to read in use f1=open(save_name, 'rb') , new_variable_name= pickle.load(f1)
