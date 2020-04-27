#This module contains tools for plotting the Energy resolved pitch angle data from Wind/STICS


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
import constants_pat as cnst 

def create_polar_plot(erpa_array,start_yearfrac, stop_yearfrac, delta_t,ion_name,erpa_plot_savedir=None):
    '''
    Function to plot the data for the Energy Resolved Pitch Angle Distribution 
    
    Arguments:
        erpa_array: an 2d array of average PSD in each E/q and pitch angle bin
    
    '''
    if erpa_plot_savedir:
        plt.ioff() #make sure interactive mode is off if just saving plots
    
    #Create start and stop time labels
    start_datetime=lib1.convert_year_fraction(np.array([start_yearfrac]))
    stop_datetime=lib1.convert_year_fraction(np.array([stop_yearfrac]))

    #from mpl_toolkits.basemap import Basemap
    from matplotlib.colors import LogNorm
    import datetime as dt

    #Define edges for pitch angle bins
    PA_resolution=15 #deg
    PA_bin_edges=np.arange(0, 181, PA_resolution) #deg, top and bottom edges for each bin

    epq_table=cnst.epq_table #keV/e
    
    delta=cnst.delta_epq # ((delta E/q) / (E/q) ) = 1.9% (Chotoo, 1998)
    epq_L=epq_table - delta*epq_table #keV/e, left E/q bin edges
    epq_R=epq_table + delta*epq_table #keV/e, ight E/q bin edges
   
    #there are "gaps" between the E/q bins, so just extend the Reimann sum bins to 
    #cover these gaps (a rough solution)
    epq_L_new=epq_L.copy()
    epq_R_new=epq_R.copy()
    
    epq_R_new[0:-1]=epq_L[1:] #make right bin edges equal to left edge of next bin (except for last bin)
    
    #Make new array that is all edges (high and low)
    epq_edges=np.zeros(epq_L_new.size+1)#split each azimuth in two and add 1 edge
    epq_edges[0:-1]=epq_L_new.copy() #keV
    epq_edges[-1]=epq_R_new[-1].copy() #keV
    
    #create grid for plotting histogram
    epq_grid,PA_grid=np.meshgrid(epq_edges,PA_bin_edges)
    
    #create subsets of epq bin array for easier reading during plotting
    epq_edges_reduced=np.array([6.1907, 10.0, 25.0, 50.0, 100.0, 150.0, 223.1238]) #keV/e
    PA_edges_reduced=np.array([0.0, 30.0, 60.0, 90.0, 120.0, 150.0, 180.0])
    
    ############################################################################# 
    #Make a cartesian plot of the flux map
    '''
    fig=plt.figure()
    ax=plt.subplot(111)
    erpa_plot1=ax.pcolormesh(epq_grid,PA_grid, erpa_array, norm=LogNorm())
    cb=plt.colorbar(erpa_plot1,label='PSD ' + '$(s^{3}/km^{6})$')
    cb.ax.yaxis.label.set_fontsize(20)
    cb.ax.tick_params(labelsize=15)
    ax.set_xlabel('$Pitch Angle$ (deg)', fontsize=20)
    ax.set_ylabel('$E/q$ (keV/e)',fontsize=20)
    plt.xlim(-180,180)
    '''
   
    small_num=1.0E-11
    if np.max(erpa_array) > 0.0 : #check for case where all values in erpa array are zero
        min_nonzero_val=np.min(erpa_array[np.where(erpa_array > 0.0)])
    else:
        min_nonzero_val=2.0*small_num
    modify_zeros_ind=np.where( (erpa_array < small_num) & (erpa_array > -1.0*small_num))
    erpa_modify_zeros=erpa_array.copy()
    erpa_modify_zeros[modify_zeros_ind]=small_num
    
    masked_erpa_array=np.ma.masked_where(erpa_modify_zeros < 0.0, erpa_modify_zeros, copy=True)
    cmap1=plt.get_cmap('viridis')
    cmap1.set_bad(color='black', alpha=1.0) #<-- value of "0" are also showing up as bad data when
    #I use the LogNorm feature of pcolor mesh
    cmap1.set_under(color='white', alpha=1.0)
    
    fig,ax=plt.subplots(subplot_kw=dict(projection='polar'))
    #erpa_plot2=ax.pcolormesh(PA_grid*np.pi/180.0, epq_grid, masked_erpa_array,
    #    cmap=cmap1)
    erpa_plot2=ax.pcolormesh(PA_grid*np.pi/180.0, epq_grid, masked_erpa_array,
        cmap=cmap1, norm=LogNorm(), vmin=min_nonzero_val)
    
    plt.title('Energy Resolved Pitch Angle', fontsize=20)
    ax.tick_params(labelsize=15)
    ax.tick_params(axis='y', colors='red')
    ax.grid(True, axis='both', which='major', color='red', linestyle='--')
    cb=plt.colorbar(erpa_plot2,label='PSD ' + '$(s^{3}/km^{6})$')
    cb.ax.yaxis.label.set_fontsize(20)
    cb.ax.tick_params(labelsize=15)
    plt.close() #need to close this one when in batch mode
   
    #Try the method that Arika used-----------------------
    #n_plots=1
    #n_cols=2 #create one column for colorbars for alignment purposes
    #fig3_new,ax3_new = plt.subplots(n_plots, n_cols, gridspec_kw={'width_ratios':[10,1]} ) #may change sharex to 'col'
    log_epq_offset=np.log10(np.min(epq_grid))
    fig3=plt.figure()  #<---- just uncommented
    fig3.subplots_adjust(wspace=0.6, left=0.1, right=0.75) #<--- just uncommented
    ax3, aux_ax3=erpa_axes(fig3,111, PA_edges_reduced, epq_edges_reduced, log_epq_offset)
    #aux_ax2.set_axis_bgcolor('k')
    if np.max(erpa_array) > 0.0: #only call if non-zero values in erpa_array
        erpa_plot3=aux_ax3.pcolormesh(PA_grid*np.pi/180.0, np.log10(epq_grid)-log_epq_offset,masked_erpa_array,
            cmap=cmap1, norm=LogNorm(), vmin=min_nonzero_val)
        cb_axes=fig3.add_axes([0.82,0.1,0.03, 0.65])
        cb=plt.colorbar(erpa_plot3, cax=cb_axes) #dropped label, and moved to title 
        #(so it would be on top of color bar axis)
        
    #plt.title('Energy Resolved Pitch Angle', fontsize=20, y=1.2)
    ax3.set_title(ion_name+' - Energy Resolved Pitch Angle\n'+str(start_datetime[0].replace(microsecond=0))+ '| '
        +str(stop_datetime[0].replace(microsecond=0)), fontsize=15, y=1.18, x=0.5)
    ax3.tick_params(labelsize=10)
    #ax.tick_params(axis='y', colors='red')
    ax3.grid(True, axis='both', which='major', color='red', linestyle='--')    
    cb.ax.yaxis.label.set_fontsize(20)
    cb.ax.tick_params(labelsize=10)
    cb.ax.set_title('PSD ' + '$(s^{3}/km^{6})$')
    #tick_locator=matplotlib.ticker.MaxNLocator(min_n_ticks=3)
    #cb.locator = tick_locator
    #cb.update_ticks()
    #If a save name was supplied, use that
    if erpa_plot_savedir:
        temp_start=str(start_datetime[0].replace(microsecond=0))
        temp1=temp_start.replace(':', '_')
        temp2=temp1.replace('-','')
        start_time_str=temp2.replace(' ', 'T')

        temp_stop=str(stop_datetime[0].replace(microsecond=0))
        temp1=temp_stop.replace(':', '_')
        temp2=temp1.replace('-','')
        stop_time_str=temp2.replace(' ', 'T')
        save_filename=erpa_plot_savedir+'wstics_erpa_'+ion_name+'_'+start_time_str+'_to_'+ stop_time_str+'.png'
        plt.savefig(save_filename)
    
    #plt.show() #comment out for batch mode
    plt.close() #close plot to save memory
    
def erpa_axes(fig, rect, PA_bin_edges, epq_edges, log_epq_offset):
    import mpl_toolkits.axisartist.floating_axes as floating_axes
    from matplotlib.projections import PolarAxes
    from mpl_toolkits.axisartist.grid_finder import (FixedLocator,
                                                    DictFormatter)
    '''
    Function that creates the axes for the energy resolved pitch angle plots.
    Arguments:
        fig: the figure the axes will be plotted on
        rect: the subplot of the figure the axis belongs on
    
    Note that the extreme/boundary values are swapped.
    
    If this function is used within the function erpa_plotter, the arguments are defined within erpa_plotter
    '''
    
    tr = PolarAxes.PolarTransform()

    #pi = np.pi
    #setting tick marks and tick labels for the pitch angle/circumference axis
    angle_ticks=[(x*np.pi/180.0 ,str(x)+'$^\circ$') for x in PA_bin_edges ]

    grid_locator1 = FixedLocator([v for v, s in angle_ticks])
    tick_formatter1 = DictFormatter(dict(angle_ticks))

    #Make our log scale r axis start closer to r=0
    
    #setting tick marks and tick labels of the energy per charge/radial axis
    radius_ticks=[(np.log10(x)-log_epq_offset ,"{:.1f}".format(x)) for x in epq_edges ] #may cause issue if log(epq) < 0 (negative radius on polar plot...)

    grid_locator2 = FixedLocator([v for v, s in radius_ticks])
    tick_formatter2 = DictFormatter(dict(radius_ticks))

    #Arika's version with the extremes flipped
    grid_helper = floating_axes.GridHelperCurveLinear(
        tr, extremes=(np.max(PA_bin_edges)*np.pi/180.0, np.min(PA_bin_edges), np.max(np.log10(epq_edges))-log_epq_offset, np.min(np.log10(epq_edges))-log_epq_offset),
        grid_locator1=grid_locator1,
        grid_locator2=grid_locator2,
        tick_formatter1=tick_formatter1,
        tick_formatter2=tick_formatter2)
        
    #version w/o extremes flipped -> doesn't appear to work!
    #grid_helper = floating_axes.GridHelperCurveLinear(
    #    tr, extremes=(np.min(PA_bin_edges),np.max(PA_bin_edges)*np.pi/180.0,  np.min(np.log10(epq_edges),np.max(np.log10(epq_edges)))),
    #    grid_locator1=grid_locator1,
    #    grid_locator2=grid_locator2,
    #    tick_formatter1=tick_formatter1,
    #    tick_formatter2=tick_formatter2)
        
    #grid_helper = floating_axes.GridHelperCurveLinear(
    #    tr, extremes=(np.pi,0,1,0.5),
    #    grid_locator1=grid_locator1,
    #    grid_locator2=grid_locator2,
    #    tick_formatter1=tick_formatter1,
    #    tick_formatter2=tick_formatter2)
   
    ax1 = floating_axes.FloatingSubplot(fig, rect, grid_helper=grid_helper)
    fig.add_subplot(ax1)
    
    #Old way of doing plotting label and ticks
    ax1.axis["bottom"].major_ticklabels.set_rotation(180)

    ax1.axis["top"].major_ticklabels.set_axis_direction("top")
    ax1.axis["top"].label.set_axis_direction("top")
    ax1.axis['bottom'].major_ticklabels.set_fontsize(15) #pitch angle tick label
    #after some experimenting, this works!
    #bad documentation for this
    
    #new experiemnt with radial text on right hand side
    ax1.axis['left'].toggle(ticklabels=False) #turn off left side radial labels
    ax1.axis['right'].label.set_text('E/q (keV/e)')
    ax1.axis['right'].label.set_fontsize(15) 
    ax1.axis['right'].label.set_visible(True) #turn on right side radial labels
    ax1.axis['right'].toggle(ticklabels=True, label='E/q (keV/e)')
    ax1.axis['right'].major_ticklabels.set_fontsize(15) #E/q tick label size, after some guesswork, this works!
    
    #Alternate plotting label and ticks section --> doesn't appear to work!
    #adjust x axis (theta):
#    ticklabels=True
#    thlabel='Pitch Angle'
#    rlabel='E/q (keV/e)'
#    ax1.axis["bottom"].set_visible(False)
#    ax1.axis["top"].set_axis_direction("bottom") # tick direction
#    ax1.axis["top"].toggle(ticklabels=ticklabels, label=bool(thlabel))
#    ax1.axis["top"].major_ticklabels.set_axis_direction("top")
#    ax1.axis["top"].label.set_axis_direction("top")
#
#    # adjust y axis (r):
#    ax1.axis["left"].set_axis_direction("bottom") # tick direction
#    ax1.axis["right"].set_axis_direction("top") # tick direction
#    ax1.axis["left"].toggle(ticklabels=ticklabels, label=bool(rlabel))
#
#    # add labels:
#    ax1.axis["top"].label.set_text(thlabel)
#    ax1.axis["left"].label.set_text(rlabel)
        
    #create a parasite axes whose transData in RA, cz
    aux_ax = ax1.get_aux_axes(tr)
    aux_ax.set_axis_bgcolor('k')
        
    ###Note from Arika Egan: Not sure why this part is necessary, but it accompanied the example this function was created from, and it doesn't do any harm.
    aux_ax.patch = ax1.patch  # for aux_ax to have a clip path as in ax
    ax1.patch.zorder = 0.9  # but this has a side effect that the patch is
    # drawn twice, and possibly over some other
    # artists. So, we decrease the zorder a bit to prevent this.

    return ax1, aux_ax
    
    
    
    