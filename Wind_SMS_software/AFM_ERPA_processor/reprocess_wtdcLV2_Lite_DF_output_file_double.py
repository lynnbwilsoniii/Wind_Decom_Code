'''
Read in DF file generated from Jacob's code and compute the dJ and counts fields
to append to that data.  Then output a new data file

'''

#import the requisite library modules
import numpy as np
import numpy.lib.recfunctions as recfunctions #have to import directly to get at recfunctions
import os
import sys
import shutil
from ion_mq_stats import ion_mq_stats #weird way to access function in .py file with same name as .py file
import constants_pat as cnst 
import glob
import calendar
import matplotlib
import datetime

def write_combined_file(target_file,
    output_dir='/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/zephyrus_transfer/output_dir/'):

    STICS_data_product='DF'
    
    #Retrieve STICS data
    STICS_3D_data_format=np.dtype([('year',float), ('doy', float), 
        ('sector', int), ('telescope', int), ('eoq', float), ('ion', 'S5' ), (STICS_data_product, float),
        (STICS_data_product+'_error', float), ('delT', float)])
    STICS_data=np.loadtxt(target_file, skiprows=4, dtype=STICS_3D_data_format)
    #gather ion info
    m,q=ion_mq_stats(STICS_data['ion'][0])
    
    #Calculate the counts (from the stated error)
    counts_value=np.floor(np.round((STICS_data['DF']/STICS_data['DF_error'])**2))
    counts_error_value=counts_value*(1.0/np.sqrt(counts_value))
    
    #fix divide by zero errors
    small_num=1e-10
    zero_ind=np.where( (STICS_data['DF'] < small_num) & (STICS_data['DF'] > -1.0*small_num) )
    counts_value[zero_ind]=0.0
    counts_error_value[zero_ind]=0.0
    
    #Calculate the differential flux (PSD[s^3/km^6]=1.076*m[amu]^2/(2*E[keV] ) * dJ[1/(cm^2*sr*s*keV)]
    dJ_value=STICS_data['DF'] /( 1.076*(m**2)/(2.0*STICS_data['eoq']*q) ) #1/(cm^2*sr*s*keV)
    dJ_error_value=dJ_value*(1.0/np.sqrt(counts_value))
    
    #fix divide by zero errors
    dJ_value[zero_ind]=0.0
    dJ_error_value[zero_ind]=0.0
    

    #append counts data to overall STICS_data
    #STICS_data=matplotlib.mlab.rec_append_fields(STICS_data, ('Counts', 'Counts_error','dJ', 'dJ_error'), 
    #    (counts_value, counts_error_value, dJ_value, dJ_error_value))
    STICS_data=recfunctions.rec_append_fields(STICS_data, ('Counts', 'Counts_error','dJ', 'dJ_error'), 
        (counts_value, counts_error_value, dJ_value, dJ_error_value)) #don't have matplotlib version on zephyrus (yet)
    
    temp=target_file.split('/')
    just_filename=temp[-1] #remove directory info
    just_filename=just_filename.replace('wtdc','')
    just_filename=just_filename.replace('DF','VDF')
    just_filename=just_filename.replace('_Lite','')
    just_filename=just_filename.replace('_TOF','')
    just_filename='wstics_'+just_filename
    outfile1=output_dir+just_filename
    
    header_string=('year, doy, sector, telescope, eoq, ion, DF, DF_error,'
        ' counts, counts_error, dJ, dJ_error, delT')
    #format_string='%7.2f %8.4f %2d %1d %8.4f %4s %8.2e %8.2e %8.2e %8.2e %8.2e %8.2e %8.2e'
    format_string=['%7.2f', '%8.4f', '%2d', '%1d', '%8.4f', '%4s', '%8.2e',
        '%8.2e', '%8.2e', '%8.2e', '%8.2e', '%8.2e', '%8.2e'] #need to specify format in this way to use delimiter option in np.savetxt
    with open(outfile1,'w') as file_handle:
        #np.savetxt(file_handle, STICS_data, header='Year, doyfrac, Telescope, Sec 0, Sec 1, Sec 2, Sec 3, '
        #    'Sec 4, Sec 5, Sec 6, Sec 7, Sec 8, Sec 9, Sec 10, Sec 11, Sec 12, Sec 13, Sec 14, Sec 15',
        #    fmt=('%d %f %d %e %e %e %e %e %e %e %e %e %e %e %e %e %e %e %e'))
        file_handle.write('#  Wind / STICS 3D velocity distributions for ion '+STICS_data['ion'][0]+'.\n')
        file_handle.write('# Created: ' + str(datetime.datetime.now())+'\n' )
        file_handle.write('# Modified to have C, dJ, and DF\n')
        np.savetxt(file_handle,
            STICS_data[['year','doy','sector','telescope','eoq','ion','DF','DF_error', 
            'Counts','Counts_error','dJ','dJ_error','delT']], #fancy indexing of structured array
            header=header_string, fmt=format_string, delimiter='\t')
        

    #Remove old DF file after creation of the new one
    os.remove(target_file) #remove file from directory after run 
                        
    print "Finished reprocessing wtdcLV2_Lite distribution file normally" #Add this so I can search for it in the output files.

def rename_moment_file(target_file,
    output_dir='/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/zephyrus_transfer/output_dir/'):
    
    
    temp=target_file.split('/')
    just_filename=temp[-1] #remove directory info
    just_filename=just_filename.replace('wtdc','')
    just_filename=just_filename.replace('_Lite','')
    just_filename=just_filename.replace('_TOF','')
    just_filename='wstics_'+just_filename
    outfile1=output_dir+just_filename
    shutil.move(target_file,outfile1) #rename the moment file by moving it.



