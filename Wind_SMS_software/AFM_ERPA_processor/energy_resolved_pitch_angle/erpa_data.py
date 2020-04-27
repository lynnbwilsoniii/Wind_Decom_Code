#This module contains tools for getting the energy resolved pitch angle
#distribution from Wind/STICS

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

def get_erpa_data(STICS_data):
    '''
    Function to get the data required to create an energy resolved pitch angle
    distribution from Wind/STICS data
    
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

    #Define STICS measurement parameters
    n_epq=32 #number of E/q steps in data
    n_telescope=3 #number of telescopes
    n_sector=16 #number of sectors
    epq_table=np.unique(STICS_data['eoq']) #keV/e
    
    #Define edges for pitch angle bins
    PA_resolution=15 #deg
    PA_low_bin_edges=np.arange(0, 180, PA_resolution) #deg
    
    total_ERPA=np.zeros( (len(PA_low_bin_edges), n_epq) ) #sum contribution to ERPA over time (not properly weighted)
    viewtime_tot_arr=total_ERPA.copy() #preallocate view time array to same size as "total_ERPA", store total observation time
    #for each PA-E/q bin

    #Compute velocity direction/ solid angle for each telescope and sector 
    #combination for use in the loop
    bin_v_dir_data_type=np.dtype([ ('vx', np.float64) ,( 'vy', np.float64), ('vz', np.float64)])
    bin_v_dir_arr=np.zeros( (n_telescope, n_sector), bin_v_dir_data_type) #this is a 2D array of unit vectors
    #pretty sweet that we can make it so easily in python (can reference both with component names and 2D indexing!)
    bin_SA_arr=np.zeros( (n_telescope,n_sector) )

    #Define central theta/phi angle for each STICS bin
    telescope_num_arr=np.array([0,1,2]) #store index number of telescopes
    theta_bin_width=53.0 * np.pi/180.0 #rad, bin width in polar direction
    theta_lower_bin_edges=np.array([90-79.5, 90-26.5, 90 + 26.5]) * np.pi/180.0 #rad, polar angle
    theta_upper_bin_edges=theta_lower_bin_edges.copy() + theta_bin_width #rad
    theta_mid_bin=(theta_upper_bin_edges + theta_lower_bin_edges) / (2.0)

    #Azimuth direction bins 
    #Define zero degrees in azimuth as the sunward facing sector center (sector 9)
    sector_num_arr = np.arange(0,16,1) #store index number of sectors
    phi_bin_width=22.5*np.pi/180.0 #rad, azimuthal sector width
    phi_mid_bin=np.arange(202.5, 202.5-360, -22.5)*np.pi/180.0
    ind1=np.where(phi_mid_bin < 0.0)
    phi_mid_bin[ind1]=phi_mid_bin[ind1]+2.0*np.pi #set azimuth range to [0,360) deg
    
    #Loop over all bins and compute unit vector
    for i in xrange(len(theta_mid_bin)): #loop over telescope
        for j in xrange(len(phi_mid_bin)): #loop over sector
            #make sure sector numbers line up with indices! 
            #mid bin angle correspond to look direction, we need to 
            #take (-) of that to get observed velocity/flow direction
            bin_v_dir_arr[telescope_num_arr[i], sector_num_arr[j]]['vx']=-np.sin(theta_mid_bin[i])*np.cos(phi_mid_bin[j])
            bin_v_dir_arr[telescope_num_arr[i], sector_num_arr[j]]['vy']=-np.sin(theta_mid_bin[i])*np.sin(phi_mid_bin[j])
            bin_v_dir_arr[telescope_num_arr[i], sector_num_arr[j]]['vz']=-np.cos(theta_mid_bin[i])
            
            bin_SA_arr[telescope_num_arr[i], sector_num_arr[j]]=phi_bin_width*(np.cos(theta_lower_bin_edges[i])
                - np.cos(theta_upper_bin_edges[i]) ) #solid angle, steradians
        #End of loop over j
    #End of loop over i
    
    #loop over number of unique time steps
    for i in xrange(n_time_steps):
        current_stop_yearfrac= unique_times[i] #this should be the stop time for the current time step
        
        
        #find indices of STICS data that are in current time step
        small_num=1.0E-5
        STICS_time_ind=np.where( (yearfrac > current_stop_yearfrac -small_num) & (yearfrac < current_stop_yearfrac + small_num) )[0]
        
        current_start_yearfrac= current_stop_yearfrac - STICS_data['delT'][STICS_time_ind[0]]/(60.0*60.0*24.0*(365+calendar.isleap(np.floor(yearfrac[0]))))
        
        #Load in MFIcurr data for the relevant year
        #mag_data_dir= 'C:/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/'
        mag_data_dir= '/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/' #mac compatible
        #load in mag data takes a while
        if int(current_stop_yearfrac) == int(current_start_yearfrac):
            with open(mag_data_dir+'wind_mfi_'+str(int(current_stop_yearfrac))+'.pkl') as fp:
                mfi_data=pickle.load(fp)
            print 'after load mag erpa'    
            #find all indices within current time range
            mag_ind=np.where( (mfi_data['yearfrac'] > current_start_yearfrac) & (mfi_data['yearfrac'] < current_stop_yearfrac) ) #returns tuple!
        
            #compute average component of B vector (GSE coords)
            ave_bx=np.mean(mfi_data['bx_gse (nT)'][mag_ind]) #nT -> may need to check for fill values and NaNs here...
            ave_by=np.mean(mfi_data['by_gse (nT)'][mag_ind]) #nT
            ave_bz=np.mean(mfi_data['bz_gse (nT)'][mag_ind]) #nT
            
            ave_b_vec=np.array([ave_bx, ave_by, ave_bz]) #nT
            
        elif int(current_stop_yearfrac)-1 == int(current_start_yearfrac): #spanning a year
            with open(mag_data_dir+'wind_mfi_'+str(int(current_start_yearfrac))+'.pkl') as fp:
                mfi_data_1=pickle.load(fp)
                
            with open(mag_data_dir+'wind_mfi_'+str(int(current_stop_yearfrac))+'.pkl') as fp:
                mfi_data_2=pickle.load(fp)  
                
            #find indices form year 1 that are within time range
            mag_ind1=np.where(mfi_data_1['yearfrac'] > current_start_yearfrac)
            mag_ind2=np.where(mfi_data_2['yearfrac'] < current_stop_yearfrac)
            
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
        
        #preallocate arrays used overwritten each time step
        ERPA_array_SA_wtd=np.zeros( (len(PA_low_bin_edges), n_epq) ) #keep track of SA weighted PSD in each PA-E/q bin
        SA_arr=np.zeros( (len(PA_low_bin_edges), n_epq) ) #keep track of total solid angle that observed each PA-Eq bin

        #Compute the pitch angle of each telescope and sector combo of Wind STICS for this time step 
        #(and average b vector direction), need to loop over each telescope and sector combo of STICS, 
        #but don't need to to it for every E/q step as look direction don't change between E/q steps.
        PA_for_telescope_sector=np.zeros((n_telescope,n_sector)) #preallocate to store PA value of each angular bin of STICS
        PA_bin_ind_for_telescope_sector=np.zeros((n_telescope,n_sector)) #preallocate to store PA value of each angular bin of STICS
        
        
        for tele_ind in xrange(n_telescope): #loop over telescope (tele_ind = telescope index)
            for sec_ind in xrange(n_sector): #loop over sector (sec_ind = sector index)
                v_unit_vec=np.array([bin_v_dir_arr[tele_ind,sec_ind]['vx'], 
                    bin_v_dir_arr[tele_ind,sec_ind]['vy'], bin_v_dir_arr[tele_ind,sec_ind]['vz']]) #call unit vec from array
                PA_for_telescope_sector[tele_ind,sec_ind]=np.arccos(np.dot(ave_b_vec, v_unit_vec)/
                    (np.linalg.norm(ave_b_vec) * np.linalg.norm(v_unit_vec)))*180.0/np.pi #deg
                PA_bin_ind=np.where(PA_low_bin_edges == np.floor(PA_for_telescope_sector[tele_ind,sec_ind]/PA_resolution)*PA_resolution)
                PA_bin_ind_for_telescope_sector[tele_ind,sec_ind]=PA_bin_ind[0] #store pitch angle bin indices of each angular bin of STICS
                
                #record total solid angle in each PA- E/q bin
                SA_arr[PA_bin_ind, :]=SA_arr[PA_bin_ind, :]+bin_SA_arr[tele_ind,sec_ind]
                #now we can use this PA_bin_ind for each E/q step in this angular bin
                
                #find all entries at current sector/telescope (for current time ind), this covers E/q steps
                epq_subind=np.where( (STICS_data[STICS_time_ind]['telescope']==tele_ind) & (STICS_data[STICS_time_ind]['sector']==sec_ind) )[0]
                #take first element of returned tuple
                
                for kk in xrange(len(epq_subind)):
                    small_val=0.01 #small number, smaller than % difference of adjacent E/q steps (for np.where search)
                    eoq_step_ind=np.where( (STICS_data['eoq'][STICS_time_ind[epq_subind[kk]]] > epq_table*(1.0-small_val)) 
                        & (STICS_data['eoq'][STICS_time_ind[epq_subind[kk]]] < epq_table*(1.0+small_val)) )[0]
                    #"[0]" at end of where statement extracts 1D indices from tuple
                    PSD_temp= ( ion_m**2/(2.0*epq_table[eoq_step_ind]*ion_q) ) * STICS_data['dJ'][STICS_time_ind[epq_subind[kk]]] #units of (amu^2/keV) * (1/(cm^2*sec*sr*keV) )
                    PSD_temp=PSD_temp*( (1/cnst.keV2eV)*(1/cnst.e2C)*(cnst.amu2kg**2) )*( (1/cnst.cm2m**2)*(1/cnst.keV2eV)*(1/cnst.e2C) ) #s^3/m^6
                    PSD_temp=PSD_temp*(cnst.km2m**6) #s^3/km^6
                    ERPA_array_SA_wtd[PA_bin_ind,eoq_step_ind]=ERPA_array_SA_wtd[PA_bin_ind, eoq_step_ind] + bin_SA_arr[tele_ind,sec_ind]*PSD_temp #sr* s^3/km^6
                #End of loop over kk
            #End of loop over j
        #End of loop over i
        #Back to loop over time.  

        #normalize PSD by solid angle (accounts for the weighting by solid angle done previously)
        ERPA_array=ERPA_array_SA_wtd/SA_arr #divide element by element
            
        #set NaN values to zero
        zero_SA_ind=np.where(SA_arr < 1.0E-10)
        if len(zero_SA_ind[0]) > 0:
            ERPA_array[zero_SA_ind]=0.0 #set NaN values to zero (works out
            #to be same as not including them in weighted average)
            
        #Need to weight each scan time by the accumulation time       
        total_ERPA= total_ERPA + ERPA_array*STICS_data['delT'][STICS_time_ind[0]] # (s^3/km^6) * s
        #assume "delT" is same for all telescope/sector/epq bins in current time step
    
        nonzero_SA_ind=np.where(SA_arr > 0.0)
        if len(nonzero_SA_ind[0]) > 0:
            viewtime_tot_arr[nonzero_SA_ind]=viewtime_tot_arr[nonzero_SA_ind] + STICS_data['delT'][STICS_time_ind[0]] #s

    #End of loop over time steps
    final_ERPA=total_ERPA/viewtime_tot_arr #element by element division
    
    #ERPA bins that were never observed over the whole time period need to be seperately identified in the array.
    #We will set them to -1.
    zero_viewtime_ind=np.where(viewtime_tot_arr < 1.0E-5) #1.0E-5 is arbitrary low bound, just lower than single accum time
    if len(zero_viewtime_ind[0]) > 0:
        final_ERPA[zero_viewtime_ind]=-1.0 #should overwrite all remaining NaN values
        
    return final_ERPA, start_yearfrac, stop_yearfrac, delta_t # (s^3/km^6), at the moment
    
def get_erpa_data_mag_input(STICS_data, mfi_data):
    '''
    Function to get the data required to create an energy resolved pitch angle
    distribution from Wind/STICS data.  Differs from "get_erpa_data" in that is takes
    a year of mfi data as an input so that the mfi data doesn't have to be loaded 
    repeatedly with each call to get_erpa_data (speed improvement)
    
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

    #Define STICS measurement parameters
    n_epq=32 #number of E/q steps in data
    n_telescope=3 #number of telescopes
    n_sector=16 #number of sectors
    epq_table=np.unique(STICS_data['eoq']) #keV/e
    
    #Define edges for pitch angle bins
    PA_resolution=15 #deg
    PA_low_bin_edges=np.arange(0, 180, PA_resolution) #deg
    
    total_ERPA=np.zeros( (len(PA_low_bin_edges), n_epq) ) #sum contribution to ERPA over time (not properly weighted)
    viewtime_tot_arr=total_ERPA.copy() #preallocate view time array to same size as "total_ERPA", store total observation time
    #for each PA-E/q bin

    #Compute velocity direction/ solid angle for each telescope and sector 
    #combination for use in the loop
    bin_v_dir_data_type=np.dtype([ ('vx', np.float64) ,( 'vy', np.float64), ('vz', np.float64)])
    bin_v_dir_arr=np.zeros( (n_telescope, n_sector), bin_v_dir_data_type) #this is a 2D array of unit vectors
    #pretty sweet that we can make it so easily in python (can reference both with component names and 2D indexing!)
    bin_SA_arr=np.zeros( (n_telescope,n_sector) )

    #Define central theta/phi angle for each STICS bin
    telescope_num_arr=np.array([0,1,2]) #store index number of telescopes
    theta_bin_width=53.0 * np.pi/180.0 #rad, bin width in polar direction
    theta_lower_bin_edges=np.array([90-79.5, 90-26.5, 90 + 26.5]) * np.pi/180.0 #rad, polar angle
    theta_upper_bin_edges=theta_lower_bin_edges.copy() + theta_bin_width #rad
    theta_mid_bin=(theta_upper_bin_edges + theta_lower_bin_edges) / (2.0)

    #Azimuth direction bins 
    #Define zero degrees in azimuth as the sunward facing sector center (sector 9)
    sector_num_arr = np.arange(0,16,1) #store index number of sectors
    phi_bin_width=22.5*np.pi/180.0 #rad, azimuthal sector width
    phi_mid_bin=np.arange(202.5, 202.5-360, -22.5)*np.pi/180.0
    ind1=np.where(phi_mid_bin < 0.0)
    phi_mid_bin[ind1]=phi_mid_bin[ind1]+2.0*np.pi #set azimuth range to [0,360) deg
    
    #Loop over all bins and compute unit vector
    for i in xrange(len(theta_mid_bin)): #loop over telescope
        for j in xrange(len(phi_mid_bin)): #loop over sector
            #make sure sector numbers line up with indices! 
            #mid bin angle correspond to look direction, we need to 
            #take (-) of that to get observed velocity/flow direction
            bin_v_dir_arr[telescope_num_arr[i], sector_num_arr[j]]['vx']=-np.sin(theta_mid_bin[i])*np.cos(phi_mid_bin[j])
            bin_v_dir_arr[telescope_num_arr[i], sector_num_arr[j]]['vy']=-np.sin(theta_mid_bin[i])*np.sin(phi_mid_bin[j])
            bin_v_dir_arr[telescope_num_arr[i], sector_num_arr[j]]['vz']=-np.cos(theta_mid_bin[i])
            
            bin_SA_arr[telescope_num_arr[i], sector_num_arr[j]]=phi_bin_width*(np.cos(theta_lower_bin_edges[i])
                - np.cos(theta_upper_bin_edges[i]) ) #solid angle, steradians
        #End of loop over j
    #End of loop over i
    
    #loop over number of unique time steps
    for i in xrange(n_time_steps):
        current_stop_yearfrac= unique_times[i] #this should be the stop time for the current time step
        
        
        #find indices of STICS data that are in current time step
        small_num=1.0E-5
        STICS_time_ind=np.where( (yearfrac > current_stop_yearfrac -small_num) & (yearfrac < current_stop_yearfrac + small_num) )[0]
        
        current_start_yearfrac= current_stop_yearfrac - STICS_data['delT'][STICS_time_ind[0]]/(60.0*60.0*24.0*(365+calendar.isleap(np.floor(yearfrac[0]))))
        
        #Load in MFIcurr data for the relevant year
        ##mag_data_dir= 'C:/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/'
        #mag_data_dir= '/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/' #mac compatible
        ##load in mag data takes a while
        #if int(current_stop_yearfrac) == int(current_start_yearfrac):
        #    with open(mag_data_dir+'wind_mfi_'+str(int(current_stop_yearfrac))+'.pkl') as fp:
        #        mfi_data=pickle.load(fp)
        #    print 'after load mag erpa'    
            #find all indices within current time range
        mag_ind=np.where( (mfi_data['yearfrac'] > current_start_yearfrac) & (mfi_data['yearfrac'] < current_stop_yearfrac) ) #returns tuple!
        
        #compute average component of B vector (GSE coords)
        ave_bx=np.mean(mfi_data['bx_gse (nT)'][mag_ind]) #nT -> may need to check for fill values and NaNs here...
        ave_by=np.mean(mfi_data['by_gse (nT)'][mag_ind]) #nT
        ave_bz=np.mean(mfi_data['bz_gse (nT)'][mag_ind]) #nT
        
        ave_b_vec=np.array([ave_bx, ave_by, ave_bz]) #nT
        
        #Don't have this functionality yet...   
        '''
        elif int(current_stop_yearfrac)-1 == int(current_start_yearfrac): #spanning a year
            with open(mag_data_dir+'wind_mfi_'+str(int(current_start_yearfrac))+'.pkl') as fp:
                mfi_data_1=pickle.load(fp)
                
            with open(mag_data_dir+'wind_mfi_'+str(int(current_stop_yearfrac))+'.pkl') as fp:
                mfi_data_2=pickle.load(fp)  
                
            #find indices form year 1 that are within time range
            mag_ind1=np.where(mfi_data_1['yearfrac'] > current_start_yearfrac)
            mag_ind2=np.where(mfi_data_2['yearfrac'] < current_stop_yearfrac)
            
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
        #preallocate arrays used overwritten each time step
        ERPA_array_SA_wtd=np.zeros( (len(PA_low_bin_edges), n_epq) ) #keep track of SA weighted PSD in each PA-E/q bin
        SA_arr=np.zeros( (len(PA_low_bin_edges), n_epq) ) #keep track of total solid angle that observed each PA-Eq bin

        #Compute the pitch angle of each telescope and sector combo of Wind STICS for this time step 
        #(and average b vector direction), need to loop over each telescope and sector combo of STICS, 
        #but don't need to to it for every E/q step as look direction don't change between E/q steps.
        PA_for_telescope_sector=np.zeros((n_telescope,n_sector)) #preallocate to store PA value of each angular bin of STICS
        PA_bin_ind_for_telescope_sector=np.zeros((n_telescope,n_sector)) #preallocate to store PA value of each angular bin of STICS
        
        
        for tele_ind in xrange(n_telescope): #loop over telescope (tele_ind = telescope index)
            for sec_ind in xrange(n_sector): #loop over sector (sec_ind = sector index)
                v_unit_vec=np.array([bin_v_dir_arr[tele_ind,sec_ind]['vx'], 
                    bin_v_dir_arr[tele_ind,sec_ind]['vy'], bin_v_dir_arr[tele_ind,sec_ind]['vz']]) #call unit vec from array
                PA_for_telescope_sector[tele_ind,sec_ind]=np.arccos(np.dot(ave_b_vec, v_unit_vec)/
                    (np.linalg.norm(ave_b_vec) * np.linalg.norm(v_unit_vec)))*180.0/np.pi #deg
                PA_bin_ind=np.where(PA_low_bin_edges == np.floor(PA_for_telescope_sector[tele_ind,sec_ind]/PA_resolution)*PA_resolution)
                PA_bin_ind_for_telescope_sector[tele_ind,sec_ind]=PA_bin_ind[0] #store pitch angle bin indices of each angular bin of STICS
                
                #record total solid angle in each PA- E/q bin
                SA_arr[PA_bin_ind, :]=SA_arr[PA_bin_ind, :]+bin_SA_arr[tele_ind,sec_ind]
                #now we can use this PA_bin_ind for each E/q step in this angular bin
                
                #find all entries at current sector/telescope (for current time ind), this covers E/q steps
                epq_subind=np.where( (STICS_data[STICS_time_ind]['telescope']==tele_ind) & (STICS_data[STICS_time_ind]['sector']==sec_ind) )[0]
                #take first element of returned tuple
                
                for kk in xrange(len(epq_subind)):
                    small_val=0.01 #small number, smaller than % difference of adjacent E/q steps (for np.where search)
                    eoq_step_ind=np.where( (STICS_data['eoq'][STICS_time_ind[epq_subind[kk]]] > epq_table*(1.0-small_val)) 
                        & (STICS_data['eoq'][STICS_time_ind[epq_subind[kk]]] < epq_table*(1.0+small_val)) )[0]
                    #"[0]" at end of where statement extracts 1D indices from tuple
                    PSD_temp= ( ion_m**2/(2.0*epq_table[eoq_step_ind]*ion_q) ) * STICS_data['dJ'][STICS_time_ind[epq_subind[kk]]] #units of (amu^2/keV) * (1/(cm^2*sec*sr*keV) )
                    PSD_temp=PSD_temp*( (1/cnst.keV2eV)*(1/cnst.e2C)*(cnst.amu2kg**2) )*( (1/cnst.cm2m**2)*(1/cnst.keV2eV)*(1/cnst.e2C) ) #s^3/m^6
                    PSD_temp=PSD_temp*(cnst.km2m**6) #s^3/km^6
                    ERPA_array_SA_wtd[PA_bin_ind,eoq_step_ind]=ERPA_array_SA_wtd[PA_bin_ind, eoq_step_ind] + bin_SA_arr[tele_ind,sec_ind]*PSD_temp #sr* s^3/km^6
                #End of loop over kk
            #End of loop over j
        #End of loop over i
        #Back to loop over time.  

        #normalize PSD by solid angle (accounts for the weighting by solid angle done previously)
        ERPA_array=ERPA_array_SA_wtd/SA_arr #divide element by element
            
        #set NaN values to zero
        zero_SA_ind=np.where(SA_arr < 1.0E-10)
        if len(zero_SA_ind[0]) > 0:
            ERPA_array[zero_SA_ind]=0.0 #set NaN values to zero (works out
            #to be same as not including them in weighted average)
            
        #Need to weight each scan time by the accumulation time       
        total_ERPA= total_ERPA + ERPA_array*STICS_data['delT'][STICS_time_ind[0]] # (s^3/km^6) * s
        #assume "delT" is same for all telescope/sector/epq bins in current time step
    
        nonzero_SA_ind=np.where(SA_arr > 0.0)
        if len(nonzero_SA_ind[0]) > 0:
            viewtime_tot_arr[nonzero_SA_ind]=viewtime_tot_arr[nonzero_SA_ind] + STICS_data['delT'][STICS_time_ind[0]] #s

    #End of loop over time steps
    final_ERPA=total_ERPA/viewtime_tot_arr #element by element division
    
    #ERPA bins that were never observed over the whole time period need to be seperately identified in the array.
    #We will set them to -1.
    zero_viewtime_ind=np.where(viewtime_tot_arr < 1.0E-5) #1.0E-5 is arbitrary low bound, just lower than single accum time
    if len(zero_viewtime_ind[0]) > 0:
        final_ERPA[zero_viewtime_ind]=-1.0 #should overwrite all remaining NaN values
        
    return final_ERPA, start_yearfrac, stop_yearfrac, delta_t # (s^3/km^6), at the moment
    
def load_mag_data(start_year, stop_year, mag_data_dir):
    '''
    Load in mag data for a given year range.  This can be used in conjunction with
    get_erpa_data_mag_input.py
    
    INPUTS:
        start_year - start year of mag data
        stop_year - stop year of mag data
    '''   
    #Load in MFI data for the relevant year
    #mag_data_dir= 'C:/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/'
    #mag_data_dir= '/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/wind_mfi/pickled/' #mac compatible
    #load in mag data takes a while
    if int(stop_year) == int(start_year):
        with open(mag_data_dir+'wind_mfi_'+str(int(stop_year))+'.pkl') as fp:
            mfi_data=pickle.load(fp)
        return mfi_data
        
    elif int(stop_year)-1 == int(start_year): #spanning a year
        with open(mag_data_dir+'wind_mfi_'+str(int(start_year))+'.pkl') as fp:
            mfi_data_1=pickle.load(fp)
            
        with open(mag_data_dir+'wind_mfi_'+str(int(stop_year))+'.pkl') as fp:
            mfi_data_2=pickle.load(fp)  
      
        return mfi_data_1, mfi_data_2  
        
    else:
        raise NameError, 'strange yearfractions detected for mag data loading'
        
        return -999
