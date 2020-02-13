Reading the SWE Strahl binary (*.strahl) files
Kosta Horaites, 4-25-2018

The binary (*.strahl) files were created by Richard Fitzenreiter (with Keith Ogilvie and Matt Holland?) in the early 2000s. The files contain distribution function
data collected by the Wind satellite's SWE strahl detector. The code contained in this package was adapted from code that was stored on NASA servers, 
that had previously been used to analyze these binary files.

In this package, the primary program for reading the SWE strahl data files is called swe_strahl_read_all_vs.pro. The program
opens and reads a strahl data file corresponding to that date (the most recent version), when called with the following syntax:

   date = '1995-01-01'           ; example. date is a string formatted as 'YYYY-MM-DD'
   struc = swe_strahl_read_all_vs(date)

The program swe_strahl_read_all_vs.pro returns a structure (here, "struc") that contains the
raw (counts) distribution function, the time (seconds since 1970-01-01/00:00:01) of the measurement, the angles (phi and theta) of measurement, etc. Both the strahl (f) 
and anti-strahl (f_anti) are contained in this structure.

NOTE: all binary data files must be stored in the same directory, that can be accessed through the environment variable STRAHL_DATA. 
For instance, in unix systems with a bash shell, the following line should be added to the .bashrc file in the user's home directory:

   export STRAHL_DATA=(path to strahl data directory)

The program swe_strahl_counts_to_phys.pro can be used to convert the distribution function into physical, cgs units (s^3 cm^-6). Here is an example of how to use
that code, as well as an example plot of an angular strahl eVDF:

   ;convert to physical units

   i = 0       ; index of the measurement, i = 0, 1, 2, 3...
   print, time_string(struc.time[i])   ; time of the measurement
   f_phys = swe_strahl_counts_to_phys(struc.f[*,*,i], struc.enstep[i], struc.time[i])
   f_anti_phys = swe_strahl_counts_to_phys(struc.f_anti[*,*,i], struc.enstep[i], struc.time[i])

   ;plot the data

   specplotk, struc.phi[*,i], struc.theta, f_phys
   specplotk, struc.phi_anti[*,i], struc.theta, f_anti_phys

