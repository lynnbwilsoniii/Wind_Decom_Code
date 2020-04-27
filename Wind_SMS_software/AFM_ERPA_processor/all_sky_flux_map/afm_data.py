#This module contains tools for getting the all sky flux map data from Wind/STICS


#define libraries to import
import numpy as np
import matplotlib.pyplot as plt
import math
import os
import matplotlib.mlab as mlab
import matplotlib
import calendar
import constants_pat as cnst 
from ion_mq_stats import ion_mq_stats #weird way to access function in .py file with same name as .py file
import cPickle as pickle

def get_afm_data(STICS_data):
    '''
    Function to get the data required to create an all sky flux map
    from Wind/STICS data
    
    Arguments:
        STICS_data: an nd array of the STICS data for the current time period
    
    '''
    ion_m, ion_q = ion_mq_stats(STICS_data['ion'][0]) #return mass [amu], charge [e]
    
    #determine how many unique time steps exist in current data set
    #(may need to change this later to be a subset of the input data?)
    
    leap_add=np.array([int(calendar.isleap(a)) for a in STICS_data['year']])
    #isleap only works on scalar years, need to use list comprehension
    yearfrac=STICS_data['year']+(STICS_data['doy']-1.0)/(365.0+leap_add)
    
    unique_times=np.unique(yearfrac) 
    n_time_steps=unique_times.size
    
    #prepare total time range string: for use in plotting later
    delta_t=STICS_data['delT'][0] #seconds
    start_yearfrac=yearfrac[0] - delta_t/(60.0*60.0*24.0*(365+calendar.isleap(np.floor(yearfrac[0])))) #yearfrac
    stop_yearfrac=yearfrac[-1] #yearfrac
    
    #assemble all sky flux map array
    n_epq= 32 #number of E/q steps in data
    n_telescope= 3 #number of telescopes
    n_sector=16 #number of sectors
    
    #loop through and put every observation in its appropriate position in afm array
    afm_array_wtd=np.zeros( (n_telescope, n_sector),dtype=np.float64 ) #preallocate
    accum_time_array=afm_array_wtd.copy() #preallocate, NEED TO MAKE COPY HERE! OTHERWISE
    #THE NEW ARRAY IS SIMPLY A REFERENCE TO THE OLD!
    
    #Calculate the width, dE, to be used for each E/q bin
    epq_table=np.unique(STICS_data['eoq']) #keV/e
    delta=0.019 #(delta E/q) / (E/q) = 1.9% (Chotoo, 1998)
    epq_L=epq_table - delta*epq_table #keV/e, left E/q bin edges
    epq_R=epq_table + delta*epq_table #keV/e, right E/q bin edges
   
    #there are "gaps" between the E/q bins, so just extend the Reimann sum bins to 
    #cover these gaps (a rough solution)
    epq_L_new=epq_L.copy()
    epq_R_new=epq_R.copy()
    
    epq_R_new[0:-1]=epq_L[1:] #make right bin edges equal to left edge of next bin (except for last bin)
    
    #bin 26 is excluded from data, so it will never have a value above zero.  Actual
    #ambient phase will not always be zero, so we should try to integrate "over" this bin
    #w/o assuming the zero value, change right bin edge of bin 25, so it encompasses bin 26 as well
    epq_R_new[25]=epq_L[27] #account for bin 26
    
    #Assumed units for dJ/dE (abbreviated dJ in Jacob's code library) are
    # [dJ/dE] = 1/(cm^2 * sec * sr * keV)
    dE=ion_q*(epq_R_new - epq_L_new) #keV
    
    #Find the average magnetic field direciton for this time period as well-------------

    #Load in MFIcurr data for the relevant year
    #mag_data_dir= 'C:/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/'
    mag_data_dir= '/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/'#Mac compatible
    print 'before load afm mag'    
    if int(stop_yearfrac) == int(start_yearfrac):
        with open(mag_data_dir+'wind_mfi_'+str(int(stop_yearfrac))+'.pkl') as fp:
            mfi_data=pickle.load(fp)
        print 'after load afm mag'    
        #find all indices within current time range
        mag_ind=np.where( (mfi_data['yearfrac'] > start_yearfrac) & (mfi_data['yearfrac'] < stop_yearfrac) ) #returns tuple!
    
        #compute average component of B vector (GSE coords)
        ave_bx=np.mean(mfi_data['bx_gse (nT)'][mag_ind]) #nT -> may need to check for fill values and NaNs here...
        ave_by=np.mean(mfi_data['by_gse (nT)'][mag_ind]) #nT
        ave_bz=np.mean(mfi_data['bz_gse (nT)'][mag_ind]) #nT
        
        ave_b_vec=np.array([ave_bx, ave_by, ave_bz]) #nT
            
    elif int(stop_yearfrac)-1 == int(start_yearfrac): #spanning a year
        with open(mag_data_dir+'wind_mfi_'+str(int(start_yearfrac))+'.pkl') as fp:
            mfi_data_1=pickle.load(fp)
            
        with open(mag_data_dir+'wind_mfi_'+str(int(stop_yearfrac))+'.pkl') as fp:
            mfi_data_2=pickle.load(fp)  
            
        #find indices form year 1 that are within time range
        mag_ind1=np.where(mfi_data_1['yearfrac'] > start_yearfrac)
        mag_ind2=np.where(mfi_data_2['yearfrac'] < stop_yearfrac)
        
        bx_combined_arr=np.concatenate( (mfi_data_1['bx_gse (nT)'][mag_ind1], mfi_data_2['bx_gse (nT)'][mag_ind2]), axis=0)
        by_combined_arr=np.concatenate( (mfi_data_1['by_gse (nT)'][mag_ind1], mfi_data_2['by_gse (nT)'][mag_ind2]), axis=0)
        bz_combined_arr=np.concatenate( (mfi_data_1['bz_gse (nT)'][mag_ind1], mfi_data_2['bz_gse (nT)'][mag_ind2]), axis=0)
        
        #compute average component of B vector (GSE coords)
        ave_bx=np.mean(bx_combined_arr) #nT -> may need to check for fill values and NaNs here...
        ave_by=np.mean(by_combined_arr) #nT
        ave_bz=np.mean(bz_combined_arr) #nT

        ave_b_vec=np.array([ave_bx, ave_by, ave_bz]) #nT
    else:
        raise NameError, 'strange yearfractions detected for mag data loading'
    
    #---------------------------------
    
        
    for i in xrange(n_epq*n_telescope*n_sector*n_time_steps):
    
        #need to integrate numerically over E/q dimension.  Accomplish with Reimann sum
        #type integration so that each E/q step can contribute in integral independently
        
        #Find the E/q step of the current dJ value
        small_val=0.01 #small number, smaller than % difference of adjacent E/q steps (for np.where search)
        eoq_ind=np.where( (STICS_data['eoq'][i] > epq_table*(1.0-small_val)) & (STICS_data['eoq'][i] < epq_table*(1.0+small_val)) )
        
        #INTEGRATION OVER E DIMENSION
        integrated_flux_contribution= STICS_data['dJ'][i]*dE[eoq_ind] #[1/(cm^2 *sr * sec)], dJ/dE * dE
        #note that we are not multiplying by solid angle here, the final value we are 
        #integrating to find is the flux divided by solid angle. This ensures that
        #an isotropic distribution will show up as a flat field even when the 
        #angular bins have different solid angles.
        
        #need E/q width of the different E/q bins, (delta E/q) / (E/q) = 1.9% (Chotoo, 1998)
        afm_array_wtd[STICS_data['telescope'][i], STICS_data['sector'][i]] +=( #line continuation
           integrated_flux_contribution *STICS_data['delT'][i]) #time weighted dJ value
           
           #keep track of total observation time in each angular direction
        accum_time_array[STICS_data['telescope'][i], STICS_data['sector'][i]]+=STICS_data['delT'][i]
        #-----------end of for loop over data file measurements-----------------
        
    afm_array=afm_array_wtd / accum_time_array #divide time back out of weighted average
        
            
    return afm_array, start_yearfrac, stop_yearfrac, delta_t, ave_b_vec
    
    
def get_afm_data_mag_input(STICS_data,mfi_data):
    '''
    Function to get the data required to create an all sky flux map
    from Wind/STICS data. Differs from "get_afm_data" in that it takes
    a year of mfi data as an input so that the mfi data doesn't have to be loaded 
    repeatedly with each call to get_afm_data (speed improvement)
    
    Arguments:
        STICS_data: an nd array of the STICS data for the current time period
        mfi_data : mag data for the year of the current STICS data
    
    '''
    ion_m, ion_q = ion_mq_stats(STICS_data['ion'][0]) #return mass [amu], charge [e]
    
    #determine how many unique time steps exist in current data set
    #(may need to change this later to be a subset of the input data?)
    
    leap_add=np.array([int(calendar.isleap(a)) for a in STICS_data['year']])
    #isleap only works on scalar years, need to use list comprehension
    yearfrac=STICS_data['year']+(STICS_data['doy']-1.0)/(365.0+leap_add)
    
    unique_times=np.unique(yearfrac) 
    n_time_steps=unique_times.size
    
    #prepare total time range string: for use in plotting later
    delta_t=STICS_data['delT'][0] #seconds
    start_yearfrac=yearfrac[0] - delta_t/(60.0*60.0*24.0*(365+calendar.isleap(np.floor(yearfrac[0])))) #yearfrac
    stop_yearfrac=yearfrac[-1] #yearfrac
    
    #assemble all sky flux map array
    n_epq= 32 #number of E/q steps in data
    n_telescope= 3 #number of telescopes
    n_sector=16 #number of sectors
    
    #loop through and put every observation in its appropriate position in afm array
    afm_array_wtd=np.zeros( (n_telescope, n_sector),dtype=np.float64 ) #preallocate
    accum_time_array=afm_array_wtd.copy() #preallocate, NEED TO MAKE COPY HERE! OTHERWISE
    #THE NEW ARRAY IS SIMPLY A REFERENCE TO THE OLD!
    
    #Calculate the width, dE, to be used for each E/q bin
    epq_table=np.unique(STICS_data['eoq']) #keV/e
    delta=0.019 #(delta E/q) / (E/q) = 1.9% (Chotoo, 1998)
    epq_L=epq_table - delta*epq_table #keV/e, left E/q bin edges
    epq_R=epq_table + delta*epq_table #keV/e, right E/q bin edges
   
    #there are "gaps" between the E/q bins, so just extend the Reimann sum bins to 
    #cover these gaps (a rough solution)
    epq_L_new=epq_L.copy()
    epq_R_new=epq_R.copy()
    
    epq_R_new[0:-1]=epq_L[1:] #make right bin edges equal to left edge of next bin (except for last bin)
    
    #bin 26 is excluded from data, so it will never have a value above zero.  Actual
    #ambient phase will not always be zero, so we should try to integrate "over" this bin
    #w/o assuming the zero value, change right bin edge of bin 25, so it encompasses bin 26 as well
    epq_R_new[25]=epq_L[27] #account for bin 26
    
    #Assumed units for dJ/dE (abbreviated dJ in Jacob's code library) are
    # [dJ/dE] = 1/(cm^2 * sec * sr * keV)
    dE=ion_q*(epq_R_new - epq_L_new) #keV
    
    #Find the average magnetic field direciton for this time period as well-------------

    ##Load in MFIcurr data for the relevant year
    ##mag_data_dir= 'C:/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/'
    #mag_data_dir= '/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/'#Mac compatible
    #print 'before load afm mag'    
    #if int(stop_yearfrac) == int(start_yearfrac):
    #    with open(mag_data_dir+'wind_mfi_'+str(int(stop_yearfrac))+'.pkl') as fp:
    #        mfi_data=pickle.load(fp)
    #    print 'after load afm mag'    
    #    #find all indices within current time range
    mag_ind=np.where( (mfi_data['yearfrac'] > start_yearfrac) & (mfi_data['yearfrac'] < stop_yearfrac) ) #returns tuple!

    #compute average component of B vector (GSE coords)
    ave_bx=np.mean(mfi_data['bx_gse (nT)'][mag_ind]) #nT -> may need to check for fill values and NaNs here...
    ave_by=np.mean(mfi_data['by_gse (nT)'][mag_ind]) #nT
    ave_bz=np.mean(mfi_data['bz_gse (nT)'][mag_ind]) #nT
    
    ave_b_vec=np.array([ave_bx, ave_by, ave_bz]) #nT
    #functionality for crossing a year is not available yet
    '''        
    elif int(stop_yearfrac)-1 == int(start_yearfrac): #spanning a year
        with open(mag_data_dir+'wind_mfi_'+str(int(start_yearfrac))+'.pkl') as fp:
            mfi_data_1=pickle.load(fp)
            
        with open(mag_data_dir+'wind_mfi_'+str(int(stop_yearfrac))+'.pkl') as fp:
            mfi_data_2=pickle.load(fp)  
            
        #find indices form year 1 that are within time range
        mag_ind1=np.where(mfi_data_1['yearfrac'] > start_yearfrac)
        mag_ind2=np.where(mfi_data_2['yearfrac'] < stop_yearfrac)
        
        bx_combined_arr=np.concatenate( (mfi_data_1['bx_gse (nT)'][mag_ind1], mfi_data_2['bx_gse (nT)'][mag_ind2]), axis=0)
        by_combined_arr=np.concatenate( (mfi_data_1['by_gse (nT)'][mag_ind1], mfi_data_2['by_gse (nT)'][mag_ind2]), axis=0)
        bz_combined_arr=np.concatenate( (mfi_data_1['bz_gse (nT)'][mag_ind1], mfi_data_2['bz_gse (nT)'][mag_ind2]), axis=0)
        
        #compute average component of B vector (GSE coords)
        ave_bx=np.mean(bx_combined_arr) #nT -> may need to check for fill values and NaNs here...
        ave_by=np.mean(by_combined_arr) #nT
        ave_bz=np.mean(bz_combined_arr) #nT

        ave_b_vec=np.array([ave_bx, ave_by, ave_bz]) #nT
    else:
        raise NameError, 'strange yearfractions detected for mag data loading'
    '''
    #---------------------------------
    
        
    for i in xrange(n_epq*n_telescope*n_sector*n_time_steps):
    
        #need to integrate numerically over E/q dimension.  Accomplish with Reimann sum
        #type integration so that each E/q step can contribute in integral independently
        
        #Find the E/q step of the current dJ value
        small_val=0.01 #small number, smaller than % difference of adjacent E/q steps (for np.where search)
        eoq_ind=np.where( (STICS_data['eoq'][i] > epq_table*(1.0-small_val)) & (STICS_data['eoq'][i] < epq_table*(1.0+small_val)) )
        
        #INTEGRATION OVER E DIMENSION
        integrated_flux_contribution= STICS_data['dJ'][i]*dE[eoq_ind] #[1/(cm^2 *sr * sec)], dJ/dE * dE
        #note that we are not multiplying by solid angle here, the final value we are 
        #integrating to find is the flux divided by solid angle. This ensures that
        #an isotropic distribution will show up as a flat field even when the 
        #angular bins have different solid angles.
        
        #need E/q width of the different E/q bins, (delta E/q) / (E/q) = 1.9% (Chotoo, 1998)
        afm_array_wtd[STICS_data['telescope'][i], STICS_data['sector'][i]] +=( #line continuation
           integrated_flux_contribution *STICS_data['delT'][i]) #time weighted dJ value
           
           #keep track of total observation time in each angular direction
        accum_time_array[STICS_data['telescope'][i], STICS_data['sector'][i]]+=STICS_data['delT'][i]
        #-----------end of for loop over data file measurements-----------------
        
    afm_array=afm_array_wtd / accum_time_array #divide time back out of weighted average
        
            
    return afm_array, start_yearfrac, stop_yearfrac, delta_t, ave_b_vec
    
    
#def ion_mq_stats(ion_name):
#
#    '''
#    Define dictionary of ion names and mass, charge values
#    
#    List of ions taken from Gruesbeck thesis 2013
#    
#    Inputs:
#        ion_name - string name of ion
#    '''
#    ion_q={
#        'H+':1, 'He+':1, 'He2+':2, 'C4+':4, 'C5+':5, 'C6+':6,
#        'O+':1, 'O6+':6, 'O7+':7,
#        'Fe8+':8, 'Fe9+': 9, 'Fe10+':10, 'Fe11+':11,
#        'Fe12+':12, 'Fe14+':14, 'Fe16+': 16,
#    } #electron charge
#        
#    ion_m={
#        'H+':1, 'He+':4, 'He2+':4, 'C4+':12, 'C5+':12, 'C6+':12,
#        'O+':16, 'O6+':16, 'O7+':16,
#        'Fe8+':56, 'Fe9+': 56, 'Fe10+':56, 'Fe11+':56,
#        'Fe12+':56, 'Fe14+':56, 'Fe16+': 56,
#    } #atomic mass units
#    
#
#    return ion_m[ion_name],ion_q[ion_name]
    