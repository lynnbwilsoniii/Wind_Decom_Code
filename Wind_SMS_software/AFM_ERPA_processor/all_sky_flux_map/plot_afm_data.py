#This module contains tools for plotting the all sky flux map data from Wind/STICS


#define libraries to import
import numpy as np
import matplotlib
matplotlib.pyplot.switch_backend('agg') #need this bit to plot on remote server w/o X forwarding
import matplotlib.pyplot as plt
import math
import os
import matplotlib.mlab as mlab

#os.chdir('/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/analysis_code/') #path on MAC
import library1 as lib1
#os.chdir('/Users/ptracy/Box Sync/00_postdoc_projects/Wind-STICS/analysis_code/all_sky_flux_map/') #path on MAC
def create_mollweide_plot(afm_array,start_yearfrac, stop_yearfrac, delta_t,ion_name, ave_b_vec,afm_plot_savedir=None):
    '''
    Function to plot the data for the all sky flux map 
    
    Arguments:
        afm_array: an 2d array of integrated flux in each angular bin
    
    '''
    if afm_plot_savedir:
        plt.ioff() #make sure interactive mode is off if just saving plots
    
    #Create start and stop time labels
    start_datetime=lib1.convert_year_fraction(np.array([start_yearfrac]))
    stop_datetime=lib1.convert_year_fraction(np.array([stop_yearfrac]))
    
    
    #from mpl_toolkits.basemap import Basemap
    from matplotlib.colors import LogNorm
    import datetime as dt
    
    #Define the bin edges for the plot, use polar angle and azimuth-------
    #Polar direction bins
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
    
    phi_lower_bin_edges=phi_mid_bin - phi_bin_width/2.0
    ind1=np.where(phi_lower_bin_edges < 0.0)
    phi_lower_bin_edges[ind1]=phi_lower_bin_edges[ind1]+2.0*np.pi #set range to [0,360) deg
    phi_upper_bin_edges=phi_mid_bin + phi_bin_width/2.0 #range should already be [0,360) deg
    #---------------------------------------------
    
    #Because the predefined sector boundaries span the +/- 180 deg and 0/360 deg
    #azimuth angle (angle those are both important directions) we will split every 
    #azimuth bin in half to that the half bin boundaries will fall exactly on the 
    #+/- 180 and 0/360 deg lines and make plotting more straight forward -> "2x" label
    
    #pcolor takes mesh arrays that are one larger than the array to be plotted
    #(in each dimension), so create an array of all edges (upper and lower for each bin)
    
    #Furthermore, we end up with a "bad" looking Mollweide projection when we only 
    #have 3 total bins in the zenith direction.  So artificially create more zenith bins
    #within the default 3 bins.  This will "smooth" out the bin edge lines in the 
    #Mollweide projection -> "nx" label. 
    #(duplicate AFM value from default bin that encompasses these subbins)
    theta_bin_edges_2x=np.zeros((theta_lower_bin_edges.size+1)) #all theta edges, just need to add 1 extra theta bin
    theta_bin_edges_2x[0:3]=theta_lower_bin_edges.copy()
    theta_bin_edges_2x[3]=theta_upper_bin_edges[-1].copy()
    
    n_zenith_mult=3 #how many bins to split each zenith bin into
    theta_bin_edges_nx=np.zeros(theta_lower_bin_edges.size*n_zenith_mult + 1)
    theta_bin_edges_nx[0:-1]=np.arange(theta_lower_bin_edges[0], theta_upper_bin_edges[-1], theta_bin_width/n_zenith_mult)#arange doesn't
    #add end value of arange(start,end, step), so add that in next line
    theta_bin_edges_nx[-1]=theta_upper_bin_edges[-1]
    ind1=np.where(theta_bin_edges_nx < 0.0)
    theta_bin_edges_nx[ind1]=theta_bin_edges_nx[ind1] + 2.0*np.pi
    
    phi_bin_edges_2x=np.zeros(phi_lower_bin_edges.size*2+1)#split each azimuth in two and add 1 edge
    phi_bin_edges_2x[0::2]=np.append(phi_upper_bin_edges,phi_upper_bin_edges[0])
    phi_bin_edges_2x[1::2]=phi_upper_bin_edges - phi_bin_width/2.0
    ind1=np.where(phi_bin_edges_2x < 0.0)
    phi_bin_edges_2x[ind1]=phi_bin_edges_2x[ind1]+2.0*np.pi #set range to [0,360) deg
    sector_num_arr_2x=np.zeros(phi_lower_bin_edges.size*2)
    sector_num_arr_2x[0::2]=sector_num_arr.copy() #keep track of which sectors go to which element of edges array
    sector_num_arr_2x[1::2]=sector_num_arr.copy()
    
    afm_array_2x=np.zeros((afm_array.shape[0], afm_array.shape[1]*2)) #need to double number of azimuth bins and copy
    #over flux values.  Notice we don't need an extra bin on the edge, these flux values are "between"
    #the "edges" arrays
    afm_array_2x[:,0::2]=afm_array
    afm_array_2x[:,1::2]=afm_array
    
    afm_array_nx_2x=np.zeros((afm_array.shape[0]*n_zenith_mult, afm_array.shape[1]*2))
    for i in xrange(telescope_num_arr.size):
        afm_array_nx_2x[i*n_zenith_mult:(i+1)*n_zenith_mult, 0::2]=afm_array[i,:] #python can broadcast 
        #this 1D array across all rows on Left hand side of assignment, neat!
        afm_array_nx_2x[i*n_zenith_mult:(i+1)*n_zenith_mult,1::2]=afm_array[i,:]
    
    
    #Now we have AFM array and a corresponding edges array in look direction angles!
    
    #Convert theta/ phi edges array to flow direction from look direction of Wind/STICS
    theta_flow_edges_2x=np.pi-theta_bin_edges_2x
    phi_flow_edges_2x=np.pi + phi_bin_edges_2x
    ind1=np.where(phi_flow_edges_2x > 2.0*np.pi)
    phi_flow_edges_2x[ind1]=phi_flow_edges_2x[ind1] -2.0*np.pi #resest to [0,360) range
    
    theta_flow_edges_nx=np.pi-theta_bin_edges_nx
    
    #Compute the equivalent lon and lat flow direction angles
    lon_flow_edges_2x=phi_flow_edges_2x - np.pi
    ind1=np.where(lon_flow_edges_2x < -1.0*np.pi)
    lon_flow_edges_2x[ind1]=lon_flow_edges_2x[ind1]+2.0*np.pi #shouldn't need this shift
    lat_flow_edges_2x=np.pi/2.0 - theta_flow_edges_2x
    
    lat_flow_edges_nx=np.pi/2.0-theta_flow_edges_nx
    #Shift arrays in the azimuth/longitude direction so we have monotonic bin values (different shifts for theta/phi vs lat/lon)
    array_shift_value=-3 #shift for longitude
    #array_shift_value=-3 #shift for phi (maybe they are the same?)
    
    plot_afm_2x=np.roll(afm_array_2x, array_shift_value, axis=1)
    plot_lon_flow_edges_2x=np.roll(lon_flow_edges_2x[0:-1], array_shift_value) #need to chop off last value and then duplicate 1st 
    #value at end after shift to account for wrap around of bin edges
    plot_lon_flow_edges_2x=np.append(plot_lon_flow_edges_2x, plot_lon_flow_edges_2x[-1]-phi_bin_width/2.0)
    plot_lat_flow_edges_2x=lat_flow_edges_2x
    plot_sector_arr_2x=np.roll(sector_num_arr_2x, array_shift_value)
    
    plot_afm_nx_2x=np.roll(afm_array_nx_2x, array_shift_value, axis=1)
    plot_lat_flow_edges_nx=lat_flow_edges_nx

    #Create grid for plotting contour
    lon_grid_2x, lat_grid_2x=np.meshgrid(plot_lon_flow_edges_2x, plot_lat_flow_edges_2x)
    
    lon_grid_nx_2x, lat_grid_nx_2x=np.meshgrid(plot_lon_flow_edges_2x, plot_lat_flow_edges_nx)
    #phi_edges=np.array([phi_lower_bin_edges, phi_upper_bin_edges[-1]])
    
    
    ##################################
    #Calculate where to put B vector symbols on plot (lat/ lon location
    B_plus=ave_b_vec
    B_minus=-ave_b_vec
    theta_B_plus=np.arccos(B_plus[2]/ np.linalg.norm(B_plus)) #rad
    phi_B_plus=np.arctan2(B_plus[1], B_plus[0]) #rad
    lat_B_plus=np.pi/2.0 - theta_B_plus #rad
    lon_B_plus=phi_B_plus - np.pi #rad

    theta_B_minus=np.arccos(B_minus[2]/ np.linalg.norm(B_minus)) #rad
    phi_B_minus=np.arctan2(B_minus[1], B_minus[0]) #rad
    lat_B_minus=np.pi/2.0 - theta_B_minus #rad
    lon_B_minus=phi_B_minus - np.pi #rad
    
    ############################
 
 ###########################################################################
    ## Make a cartesian plot of the flux map
    '''
    fig=plt.figure()
    ax=plt.subplot(111)
    afm_plot1=ax.pcolormesh(lon_grid_2x*180/np.pi, lat_grid_2x*180.0/np.pi, plot_afm_2x, norm=LogNorm())
    #afm_plot1=ax.pcolormesh(lon_grid_nx_2x*180/np.pi, lat_grid_nx_2x*180.0/np.pi, plot_afm_nx_2x, norm=LogNorm()) #should be same as 2x
    #afm_plot1=ax.pcolormesh(lon_grid*180/np.pi, lat_grid*180.0/np.pi, plot_afm_2x) #not log version
    plt.colorbar(afm_plot1,label='Integrated Flux\n' + '$(cm^{-2} s^{-1} sr^{-1})$')
    ax.set_xlabel('$Lon$ (deg)')
    ax.set_ylabel('$Lat$ (deg)')
    plt.xlim(-180,180)
    '''
    #Make a Mollweide Projection as well
    from mpl_toolkits.basemap import Basemap
    m=Basemap(projection='moll', lat_0=0, lon_0=0)
    m_lon_2x, m_lat_2x=m(lon_grid_2x*180/np.pi, lat_grid_2x*180/np.pi) #need to input in degrees
    m_lon_nx_2x, m_lat_nx_2x=m(lon_grid_nx_2x*180/np.pi, lat_grid_nx_2x*180/np.pi)
    m_lon_B_plus, m_lat_B_plus=m(lon_B_plus*180.0/np.pi, lat_B_plus*180.0/np.pi) #location of +/- mag vector directions
    m_lon_B_minus, m_lat_B_minus=m(lon_B_minus*180.0/np.pi, lat_B_minus*180.0/np.pi)
        
    fig=plt.figure()
    #afm_plot1=m.pcolormesh(m_lon,m_lat, plot_afm_2x, vmin=0.0, vmax=1000.0)
    #afm_plot1=m.pcolormesh(m_lon_2x,m_lat_2x, plot_afm_2x,cmap=plt.cm.jet, norm=LogNorm())
    if np.max(plot_afm_nx_2x) > 0:
        afm_plot1=m.pcolormesh(m_lon_nx_2x,m_lat_nx_2x, plot_afm_nx_2x,cmap=plt.cm.jet, norm=LogNorm())
    else:
        afm_plot1=m.pcolormesh(m_lon_nx_2x,m_lat_nx_2x, plot_afm_nx_2x,cmap=plt.cm.jet) #don't use logNorm if all zeros
    
    afm_plot_B_plus=m.scatter(m_lon_B_plus, m_lat_B_plus, label='+B', color= 'magenta', s=400, marker='$\odot$')
    afm_plot_B_minus=m.scatter(m_lon_B_minus, m_lat_B_minus, label='-B', color= 'magenta', s=400, marker='$\otimes$')
    fig.subplots_adjust(wspace=0.6, left=0.05, right=0.75) 
    cb_axes=fig.add_axes([0.82,0.15,0.03, 0.65])
    cb=plt.colorbar(afm_plot1,cax=cb_axes) 
    #tick_locator=matplotlib.ticker.MaxNLocator(nbins=6,min_n_ticks=3)
    #cb.locator = tick_locator
    #cb.update_ticks() #did more harm than good with these
    cb.ax.set_title('Integrated Flux\n' + '$(cm^{-2} s^{-1} sr^{-1})$',fontsize=10)
    cb.ax.yaxis.label.set_fontsize(20)
    cb.ax.tick_params(labelsize=10)
    afm_plot1.axes.set_ylabel('Latitude (deg)', fontsize=15)
    afm_plot1.axes.set_xlabel('Longitude (deg)', fontsize=15)
    afm_plot1.axes.set_title(ion_name+' - Plasma Flow Dir Hist\n'+str(start_datetime[0].replace(microsecond=0))+ '| '
        +str(stop_datetime[0].replace(microsecond=0)), fontsize=15, y=1.2, x=0.45)
    #set the current or "active" axes back to the flux map (so I don't have to 
    #figure out how to keep respecifying those axes!)
    plt.sca(afm_plot1.axes) #set current axes
    m.drawmeridians(plot_lon_flow_edges_2x[1::2]*180.0/np.pi) 
    m.drawparallels(plot_lat_flow_edges_2x*180.0/np.pi, labels=[False, True, False, False]) #labels=[left, right, top, bottom]
    
    #add label for antisunward, dawnward, and duskward flow
    label_arr=['$-x_{GSE}$', '$+y_{GSE}$', '$-y_{GSE}$']
    label_lon_pos=[0.0, -90.0,90.0] #deg
    for i in xrange(len(label_arr)):
        x1,y1=m(label_lon_pos[i],0.0)
        plt.annotate(label_arr[i], xy=(x1,y1), color='k', fontsize=20)
        
    #add labels for sectors
    for i in xrange(plot_sector_arr_2x.size/2):
        x1,y1=m( ((plot_lon_flow_edges_2x[2*i]+plot_lon_flow_edges_2x[2*i+1])/2)*180.0/np.pi, 30.0) #put label
        #at average lon location of sector edges
        plt.annotate(str(plot_sector_arr_2x[2*i].astype(int)), xy=(x1,y1),color='r', fontsize=10)
    
    #If a save name was supplied, use that
    if afm_plot_savedir:
        temp_start=str(start_datetime[0].replace(microsecond=0))
        temp1=temp_start.replace(':', '_')
        temp2=temp1.replace('-','')
        start_time_str=temp2.replace(' ', 'T')

        temp_stop=str(stop_datetime[0].replace(microsecond=0))
        temp1=temp_stop.replace(':', '_')
        temp2=temp1.replace('-','')
        stop_time_str=temp2.replace(' ', 'T')
        save_filename=afm_plot_savedir+'wstics_afm_'+ion_name+'_'+start_time_str+'_to_'+ stop_time_str+'.png'
        plt.savefig(save_filename)
    
    #plt.show() #comment out for batch mode
    plt.close() #close the plot (save memory)