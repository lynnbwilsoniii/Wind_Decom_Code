'''
This package contains function for the batch running of ERPA and AFM
'''


#import the requisite library modules
import numpy as np
import os
import sys
from ion_mq_stats import ion_mq_stats #weird way to access function in .py file with same name as .py file
import cPickle as pickle
import all_sky_flux_map.afm_data as AFM
import all_sky_flux_map.plot_afm_data as plot_AFM
import constants_pat as cnst 
import energy_resolved_pitch_angle.erpa_data as ERPA
import energy_resolved_pitch_angle.plot_erpa_data as plot_ERPA
import glob
import calendar
import time as tm
import datetime

def batch_run(target_file,mag_data_dir='/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/',
    output_dir='/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/zephyrus_transfer/output_dir/',
    create_ERPA_plot=True, create_AFM_plot=True):
    '''
    target_file - file produced by Jacob's processor, to be read in and
        used to create AFM, ERPA products
    mag_data_dir - directory where the mag_data is located (include default local location)
    create_ERPA_plot - toggle to create ERPA plot
    create_AFM_plot - toggle to create AFM plot
    '''
    #check that file exists
    if not(os.path.isfile(target_file)):
	print(target_file)
        sys.exit("No data file produced by wtdcLV2 processor")
    
    #Set the desired time resolution for the output files
    #input_time_res=3.0*60.0*60.0 #seconds
    input_time_res=30.0*60.0 #seconds
    
    #create list of input data files
    file_list=[target_file] #one element list
    
    #STICS_data_product='dJ' #require this for now

    #Retrieve STICS data
    #STICS_3D_data_format=np.dtype([('year',float), ('doy', float), 
    #    ('sector', int), ('telescope', int), ('eoq', float), ('ion', 'S4' ), (STICS_data_product, float),
    #    (STICS_data_product+'_error', float), ('delT', float)])
    
    #change to new data format
    STICS_3D_data_format=np.dtype([('year',float), ('doy', float), 
        ('sector', int), ('telescope', int), ('eoq', float), ('ion', 'S5' ), ('DF', float),
        ('DF'+'_error', float), ('Counts', float), ('Counts_error', float),
        ('dJ', float), ('dJ_error', float),
        ('delT', float)])
        
    #Load in mag data for entire year up front (speed up code running)
    temp1=file_list[0].split('-') #estimate start/stop year from file names in file_list
    temp2=temp1[-1].split('.')
    start_year=int(temp2[0][0:4])
    temp1=file_list[-1].split('-')
    temp2=temp1[-1].split('.')
    stop_year=int(temp2[0][0:4])
    
    #load in mag data

    mfi_data=ERPA.load_mag_data(start_year, stop_year,mag_data_dir)
    
    j=0
    STICS_data=np.loadtxt(file_list[j], skiprows=4, dtype=STICS_3D_data_format)
    
    #extract ion and time stamp from file name
    temp1=file_list[j].split('/') #split by folder to get just filename
    temp1=temp1[-1] #just take last element (filename)
    temp2=temp1.split('_')
    ion_name=temp2[3] #recover ion name
    temp3=temp2[6] #grab date portion
    date_string=temp3[0:8]
    
    ion_m, ion_q = ion_mq_stats(STICS_data['ion'][0]) #return mass [amu], charge [e]


    #seperate into the time subranges contained in the file
    leap_add=np.array([int(calendar.isleap(a)) for a in STICS_data['year']])
    #isleap only works on scalar years, need to use list comprehension
    yearfrac=STICS_data['year']+(STICS_data['doy']-1.0)/(365.0+leap_add)

    unique_times=np.unique(yearfrac) 
    n_time_steps=unique_times.size
    print 'date: ', date_string
    print '# time steps: ', n_time_steps
    
    #need to regroup time steps into desired time resolution...

    input_time_res_yrfrac=(input_time_res/( 60.0*60.0*24.0*(365.0+calendar.isleap(STICS_data['year'][0])) ))
    #n_time_steps_at_res=( (unique_times[-1]-unique_times[0]) /input_time_res_yrfrac )
        #estimate number of time steps in time period at given resolution
    time_group_ind=np.zeros(n_time_steps, dtype=int) #preallocate
    n=0 #counter, initiallize
    t0=unique_times[0] #initialize to first time in current time sequence
    #the way this is written, it behaves funny if you try to pick time resolution finer than what came
    #out of the STICS processer (it essentially doesn't do anything in that case).
    for k in xrange(n_time_steps):
        if (unique_times[k] < t0+input_time_res_yrfrac) : #check if we moved into next interval yet
            time_group_ind[k]=n
        else: #moved into next interval
            time_group_ind[k]=n+1
            n=n+1 #increment index
            t0=t0+input_time_res_yrfrac #increment time
            
    n_time_steps_at_res=len(np.unique(time_group_ind)) #number of time steps at specified resolution
    
    #Open a file for the current day to write all time steps to
    outfile1=output_dir+'wstics_afm_'+STICS_data['ion'][0]+'_'+date_string+'.dat'
    outfile2=output_dir+'wstics_erpa_'+STICS_data['ion'][0]+'_'+date_string+'.dat'
    with open(outfile1, 'a') as afm_handle, open(outfile2, 'a') as erpa_handle:
    
        for i in xrange(n_time_steps_at_res):
            unique_times_subset=unique_times[time_group_ind==i]
            small_val2=1.0E-10 #small number for yearfrac comparison
            
            #modify the time index method to group over multiple native time steps (native= time step of STICS
            # processor data file)
            #current_time_ind=np.where( ( (yearfrac+small_val2) > unique_times[i]) & 
            #    ( (yearfrac - small_val2) < unique_times[i] )  )
            
            current_time_ind=np.where( ( (yearfrac+small_val2) > unique_times_subset[0]) & 
                ( (yearfrac - small_val2) < unique_times_subset[-1] )  )
            
            #compute AFM
            afm_array, start_yearfrac, stop_yearfrac, delta_t, ave_b_vec= AFM.get_afm_data_mag_input(STICS_data[current_time_ind],mfi_data) 
            
            #Generate plot and save
            if create_AFM_plot:
                plot_AFM.create_mollweide_plot(afm_array, start_yearfrac, stop_yearfrac, delta_t,STICS_data['ion'][0], 
                    ave_b_vec, afm_plot_savedir=output_dir) #generates a lot of plots!
            
            #compute ERPA
            erpa_array,start_yearfrac2, stop_yearfrac2, delta_t= ERPA.get_erpa_data_mag_input(STICS_data[current_time_ind],mfi_data)
            
            #Generate ERPA plot and save
            if create_ERPA_plot:
                plot_ERPA.create_polar_plot(erpa_array,start_yearfrac, stop_yearfrac, delta_t,STICS_data['ion'][0],
                    erpa_plot_savedir=output_dir) #generates a lot of plots!
                
            #write to AFM file
            time_array=np.array(np.transpose([np.repeat(STICS_data['year'][current_time_ind[0][0]],3), 
                np.repeat(STICS_data['doy'][current_time_ind[0][0]],3), STICS_data['telescope'][0:3]]))
            temp=np.concatenate((time_array,afm_array),axis=1)
            if i==0: #only print header on first time through file
                afm_handle.write('# Wind / STICS Angular flux map (AFM) for ion ' + ion_name+ '.\n')
                afm_handle.write('# Created: ' + str(datetime.datetime.now())+'\n' )
                afm_handle.write('#\n')
                np.savetxt(afm_handle, temp, header='Year, doyfrac, Telescope, Sec 0, Sec 1, Sec 2, Sec 3, '
                    'Sec 4, Sec 5, Sec 6, Sec 7, Sec 8, Sec 9, Sec 10, Sec 11, Sec 12, Sec 13, Sec 14, Sec 15',
                    fmt=('%d %f %d %e %e %e %e %e %e %e %e %e %e %e %e %e %e %e %e'))
            else:  
                np.savetxt(afm_handle, temp,
                    fmt=('%d %f %d %e %e %e %e %e %e %e %e %e %e %e %e %e %e %e %e'))
            
            #write to ERPA file
            time_array=np.array(np.transpose([np.repeat(STICS_data['year'][current_time_ind[0][0]],32), 
                np.repeat(STICS_data['doy'][current_time_ind[0][0]],32), cnst.epq_table]))
            temp=np.concatenate((time_array,np.transpose(erpa_array)),axis=1)
            if i==0: #only print header on first time through file
                erpa_handle.write('# Wind / STICS Energy-resolved pitch-angle (ERPA) distributions for ion ' + ion_name+ '.\n')
                erpa_handle.write('# Created: ' + str(datetime.datetime.now())+'\n' )
                erpa_handle.write('#\n')
                np.savetxt(erpa_handle, temp, header='Year, doyfrac, eoq, PA 0, PA 1, PA 2, PA 3, '
                    'PA 4, PA 5, PA 6, PA 7, PA 8, PA 9, PA 10, PA 11',
                    fmt=('%d %f %f %e %e %e %e %e %e %e %e %e %e %e %e'))
            else:
                np.savetxt(erpa_handle, temp,
                    fmt=('%d %f %f %e %e %e %e %e %e %e %e %e %e %e %e'))
    
    print "Finished AFM-ERPA Normally" #Add this so I can search for it in the output files.
