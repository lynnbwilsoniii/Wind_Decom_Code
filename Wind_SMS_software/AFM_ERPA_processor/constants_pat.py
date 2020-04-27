#This script will just contain the constants that I am always using

import numpy as np

amu2kg=1.66054e-27 # kg/amu
e2C=1.602e-19 #C/e charge
keV2eV=1000.0 #eV/keV
km2m=1000.0 #m/km
cm2m=0.01 #m/cm
#Energy per charge table to Wind/STICS
epq_table=np.array([6.1907,    6.9496,    7.8015,    8.7579,    9.8315,   11.0367,
         12.3896,   13.9084,   15.6134,   17.5274,   19.676 ,   22.088 ,
         24.7956,   27.8352,   31.2474,   35.0779,   39.378 ,   44.2052,
         49.6241,   55.7073,   62.5362,   70.2022,   78.808 ,   88.4688,
         99.3138,  111.4882,  125.1551,  140.4973,  157.7203,  177.0545,
        198.7588,  223.1238]) #keV/e
#E/q width for Wind/STICS
delta_epq=0.019 #(delta E/q) / (E/q) = 1.9% (Chotoo, 1998)

#Define the angular bin edges for the sector and telescope bins of Wind/STICS
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


