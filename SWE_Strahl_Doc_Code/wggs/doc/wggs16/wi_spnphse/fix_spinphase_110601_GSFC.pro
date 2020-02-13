; WIND is a spinning spacecraft and the orientation of the spacecraft at any instant is
;  given by a spin phase angle.  The spacecraft takes about 3 sec (spin period) to make
;  a full revolution (an angular rate of (2*pi rad)/(3 sec) or about 2 rad/sec), but a
;  sample spin phase value is taken about every 90 sec.  This is once every 30 spins.
; In a very high-flux solar event (for example) the spacecraft's sun-sensor--which
;  provides an angular reference--gets saturated and the spin phase data gets corrupted.
;  This causes major problems in the analysis of data from several on-board experiments.
; Here's what to do in such an event:
; 1) Get the CDF (common data format) data from the day(s) of the event.
;    - Online location: http://cdaweb.gsfc.nasa.gov (coordinated data analysis web).
;    - Follow link labelled: "Public data from current space physics missions".
;    - Select "Wind" as the 'Source' and "Ephemeris" as the 'Instrument Type'.
;    - 'Submit' your request and then, making sure only "Wind Spin Phase" is selected
;      ('Orbit' and 'Attitude' data are not needed), 'submit' a second request.
;    - At this point follow the directions on listing, plotting or dowloading the data.
;      Listing and plotting are useful for developing an understanding the problem, but
;      there are other ways of viewing the data and downloading the CDF files is the
;      most important thing to do here.
;    - Not only is the data from the time of the event needed, it is also necessary to
;      obtain data from the days before and after the event.  This is basically a
;      judgement call; in general, the longer the event the more "good" data is needed
;      as a buffer.  For instance, about one day on each side for each "bad" day.
;    - Finally, if there are problems, a contact point at the CDHF (central data
;      handling facility) is:
;             Kevin Magnum, email:magnum@istp1.gsfc.nasa.gov, phone:(301)286-3228.
; 2) Identify the problem interval.
;    - The website from the previous step provides a means for viewing the data, but a
;      much more useful browser can be found on any KP (key parameter) data CD-ROM.
;      To get started using the KPVT (key parameter visualization tool) go to:
;      <CD-ROM drive>:\TOOLS\KPVT\readme.doc (the rest is self-explanatory).
;    - Now is the time to decide what data needs to be "cleaned up" and what data is
;      to be used as the buffer of "good" data (recall the criterion mentioned above).
;    - Look carefully at the code below, it was used to fix an event on 11/06/2001 and
;      it should serve as an example of what is needed to fix events in general.
; 3) Modify the code for the current event.
;    - MAKE A COPY OF THE ORIGINAL FIRST.
;    - The notes in the code below should act as a guide, but much of this process is
;      context-dependant and is going to involve trial-and-error.  The only information
;      one has to work with is an estimate of the average spin period during the event
;      and the "good" data before and after the event.
;    - Use the process developed by R. Fitzenreiter to generate the spin period estimate
;      from the strahl instrument (more precision produces better results).
;    - Insert this into the code below in the obvious place and make all other necessary
;      changes in the section labelled "Basic User Inputs" and the 'changable' sections.
;    - Initially set writeCDF to false and doCheck to true so that diagnostic checks are
;      performed and the data produced by this routine is not yet written out.
; 4) Compile and run the code (iteratively).
;    - The 'fine tune' values must be altered and the diagnostic results checked in an
;      iterative fashion.  Continue until data corruption is negligible.
;    - The diagnostic results are a *.dat file and a plot of alternating values from
;      this file (differences between contiguous data points).  The plot should display
;      two horizontal lines formed from the alternating values and two vertical lines
;      bounding the problem interval.  If this is not what is displayed continue
;      iterating until the plot within the vertical lines is the same as without.
;    - Upon completion, the routine always provides a plot of the "cleaned-up" data.
;      Look at this as well to check the results (compare cleaned-up data to good data).
;    - The last step is to set writeCDF to true to write results to a CDF file.
;      It is VERY IMPORTANT to use the next-larger version number when doing this.
; 5) Verify results.
;    - Use KPVT to look at the final spin phase values and see if they "look right".
;    - Have Adam Szabo (magnetometer team) run his analysis code on the resultant CDF
;      file.  He should not be able to see any aberrations in the magnetometer data
;      calculated using this spin phase data.
; 6) Using contact point for help, submit results to the CDHF at istp1.gsfc.nasa.gov.
;    - Consult Bill Mish when doing this.
;++++++++++++++++++++++++++++++++++++++++++++++++++++++ Matt Holland (12/5/01) ++++++
PRO fix_spinphase_110601_GSFC

; This IDL procedure is written by Syau-Yun Hsieh.  It is used to analyze the WIND spin
;  phase data and produce the correct spin phase data.  It is also used for writing the
;  correct spin phase data, etc. into the CDF (common data format).
;
; 11/28/01: This application is used for correcting the 11/06/01 WIND spin phase data due
;            to the sun sensor problem.  Data used are starting from 11/05/01.
; 11/29/01: This copy is created for Matt Holland.
; 11/30/01: Annotation has been added and simple changes made to "clean up" code (MH).

loadct,31 & true = 1 & false = 0 ;   Set color table and define several useful constants.
nspms = (1.d/1000.d) & nspm = 60.d & nsph = 3600.d & nspd = 86400.d & twopi = 2.d*!dPI
;   nspx is: number of seconds per x, with x = ms(millisec),m(minute),h(hour) and d(day).

;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Basic User Inputs @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; Flags for writing out the CDF of data.  Set to 'false' if you DON'T want to write data
;  out to the CDF or perform diagnostic checks (resp.)--set to 'true' if you DO.
writeCDF = false & doCheck = true ; Note:       Don't write results to CDF until you are
;                                    satisfied with the results of the diagnostic checks.

; ********** Using either Dick's spin period or value computed from CDF's avg. spin rate.
;         avg_T = 3.1106880 (11/06/01 S-Y) <==this is calculated from CDF avg. spin rate.
avg_T = double(3.1105320) ;   R. Fitzenreiter's # for 11/06/01 (using Strahl instrument).

;                        Define information on the location of WIND spin phase CDF files.
cdf_location = 'C:\WINDOWS\Desktop\Fixing SpinPhase\' ;            Location of CDF files.
wind_cdf = cdf_location+['wi_k0_spha_20011105_v01.cdf',$  ;             good data
                         'wi_k0_spha_20011106_v01.cdf',$  ;     problem data + some good
                         'wi_k0_spha_20011107_v01.cdf' ]  ;             good data
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;@@@@@@@@@@@@@@@@@@@@@@@@ Gathering CDF Spin Phase Data @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
var_epoch = 'Epoch' & var_spinrate = 'AVG_SPIN_RATE' & var_spinphase = 'SPIN_PHASE'
n_CDFfiles = n_elements(wind_cdf) ;                 Store number of CDF files being used.
sum = 0L & sum_last = sum & time_tag = dblarr(n_CDFfiles) ;   Initialize these variables.

for file_number=0,(n_CDFfiles-1) do begin ;  For each of the WIND spin phase CDF files...
   id = cdf_open(wind_cdf[file_number]) ;         Open the file (using given path info.).

   cdf_control,id,Variable=var_epoch,Get_Var_Info=varinfo ;    Pull data from the file...
   cdf_varget,id,var_epoch,    varval_epoch,    Rec_Count=varinfo.maxrec+1
   cdf_varget,id,var_spinrate, varval_spinrate, Rec_Count=varinfo.maxrec+1
   cdf_varget,id,var_spinphase,varval_spinphase,Rec_Count=varinfo.maxrec+1
   varval_epoch     = reform(varval_epoch[0,*]) ;
   varval_spinrate  = reform(varval_spinrate[0,*])
   varval_spinphase = reform(varval_spinphase[0,*])
   time_tag[file_number] = varval_epoch[0]

   sum = sum_last+varinfo.maxrec+1 ;   Make a new set of arrays to accomodate new data...
   x = dblarr(sum) & y = dblarr(sum) & z = dblarr(sum)

   if (file_number eq 0) then begin ;    For first file, fill new arrays with new data...
      x = varval_epoch & y = varval_spinphase & z = varval_spinrate
   endif else begin ;            Otherwise, fill new arrays with both old and new data...
      x[0L:sum_last-1L] = x_old  &  x[sum_last:sum-1L] = varval_epoch
      y[0L:sum_last-1L] = y_old  &  y[sum_last:sum-1L] = varval_spinphase
      z[0L:sum_last-1L] = z_old  &  z[sum_last:sum-1L] = varval_spinrate
   endelse

   x_old = x & y_old = y & z_old = z & sum_last = sum & cdf_close,id ;        Close file.
endfor

test_x  = dblarr(sum) & test_y  = dblarr(sum) & test_z  = dblarr(sum) ; Create new arrys.
test_x2 = dblarr(sum) & test_y2 = dblarr(sum) & test_z2 = dblarr(sum)
test_x3 = dblarr(sum) & test_y3 = dblarr(sum) & test_z3 = dblarr(sum)

count  = 0L ;   Define a set of counters, used to acculmulate various event incidences...
count2 = 0L & count3 = 0L & count4 = 0L & count5 = 0L & count6 = 0L & count7 = 0L

time_main = dblarr(sum) ;        Define array for storing new time in continuous seconds.

for i=0L,(sum-1L) do begin ;          For each data point in the set of collected data...
   cdf_epoch,x[i],yr,mo,dy,hr,mn,sc,mi,/Break ;                Get current time values...
   hr = double(hr) & mn = double(mn) & sc = double(sc) & mi = double(mi)
   time_main[i] = (hr*nsph)+(mn*nspm)+sc+(mi*nspms) ;        Store current time values...

   ;*************************** BEGIN Changable Section *********************************
   ;  The user will need to make a judgement call about what data will be used to
   ;   reconstruct the "bad" interval.  The following code must be changed accordingly...
   if (dy eq 5) then time_main[i] = time_main[i]+(nspd*0.d) ;     For data from 11/05/01.
   if (dy eq 6) then time_main[i] = time_main[i]+(nspd*1.d) ;     For data from 11/06/01.
   if (dy eq 7) then time_main[i] = time_main[i]+(nspd*2.d) ;     For data from 11/07/01.

   ;---------------------------------------------------------- Good data BEFORE problems.
   if (dy eq 5) then begin ;                                      For data from 11/05/01.
      test_x2[count3] = x[i] & test_y2[count3] = y[i] & test_z2[count3] = z[i]
      count3 = count3+1 & count2 = count2+1 ;          Increment relevent counter values.
   endif ;-------------------------------------------------------------------------------

   ;---------------------------------------------------------- Middle part: problem area.
   if ((dy eq 6) and (hr lt 22)) then begin ;    For data from 11/06/01 (with hour < 22).
      test_x[count] = x[i] & test_y[count] = y[i] & test_z[count] = z[i]
      count = count+1 & count7 = count7+1 ;            Increment relevent counter values.
   endif ;-------------------------------------------------------------------------------

   ;----------------------------------------------------------- Good data AFTER problems.
   if ((dy eq 6) and (hr ge 22)) then begin ;   For data from 11/06/01 (with hour >= 22).
      test_x3[count4] = x[i] & test_y3[count4] = y[i] & test_z3[count4] = z[i]
      count4 = count4+1 & count5 = count5+1 ;          Increment relevent counter values.
   endif

   if (dy eq 7) then begin ;                                      For data from 11/07/01.
      test_x3[count4] = x[i] & test_y3[count4] = y[i] & test_z3[count4] = z[i]
      count4 = count4+1 & count6 = count6+1 ;          Increment relevent counter values.
   endif ;-------------------------------------------------------------------------------
   ;**************************** END Changable Section **********************************
endfor

;*************************** BEGIN Changable Section ***********************************
;               Output final totals of counts to the screen and consolidate the counts...
print,'No. of GOOD data on 11/5: ', count3 & print,'No. of BAD  data on 11/6: ', count7
print,'No. of GOOD data on 11/6: ', count5 & print,'No. of GOOD data on 11/7: ', count6
test_x = test_x[0:count-1] & test_y = test_y[0:count-1] & test_z = test_z[0:count-1]
;**************************** END Changable Section ************************************
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;@@@@@@@@@@@@@@@@@@@@@ Main Analysis is Performed Here @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
for i=0L,(count-1L) do begin ;   For each data point in the "problem area" of data set...
   if (i eq 0) then begin ;                              For the very first data point...
      tt1 = test_x[i] & tt0 = test_x2[count3-1] & yy0 = test_y2[count3-1]
      cdf_epoch,tt1,yr1,mo1,dy1,hr1,mn1,sc1,mi1,/Break
      cdf_epoch,tt0,yr0,mo0,dy0,hr0,mn0,sc0,mi0,/Break
      hr1 = double(hr1) & mn1 = double(mn1) & sc1 = double(sc1) & mi1 = double(mi1)
      hr0 = double(hr0) & mn0 = double(mn0) & sc0 = double(sc0) & mi0 = double(mi0)
      tt1 = (hr1*nsph)+(mn1*nspm)+sc1+(mi1*nspms) ;                             End time.
      tt0 = (hr0*nsph)+(mn0*nspm)+sc0+(mi0*nspms) ;                           Begin time.
   endif else begin ;                                    For any subsequent data point...
      tt1 = test_x[i] & cdf_epoch,tt1,yr1,mo1,dy1,hr1,mn1,sc1,mi1,/Break
      hr1 = double(hr1) & mn1 = double(mn1) & sc1 = double(sc1) & mi1 = double(mi1)
      tt1 = (hr1*nsph)+(mn1*nspm)+sc1+(mi1*nspms) ;                             End time.
   endelse

   if (dy1 eq dy0) then delta_tt = tt1-tt0 else delta_tt = nspd-tt0+tt1 ; Intrval length.
   delta_angle = (delta_tt/avg_T)-floor(delta_tt/avg_T) ; Remaining fraction of a period.
   if (i eq 0) then print,'/',(delta_tt/avg_T),'-',floor(delta_tt/avg_T) ; Screen output.
   test_y[i] = (twopi*delta_angle)+yy0 & cap = twopi ; Current estimate of ith phase ang.
   if test_y[i] ge cap then test_y[i] = test_y[i]-cap ;     Map estimate into: [0,twopi).

   ; ############################### Fine Tuning #######################################
   ;                  Use these "fine tune" values to improve the spin phase estimates...
   if (((test_y[i]-yy0) lt 3.6) and (test_y[i] gt -2.6)) then begin ;          Fine tune.
      test_y[i] = yy0-double(2.65820) ;                                        Fine tune.
      if (test_y[i] lt 0.0) then test_y[i] = cap+test_y[i] ; Map estimate into [0,twopi).
   endif

   if ((test_y[i]-yy0) gt 3.63) then test_y[i] = yy0+double(3.62520) ;         Fine tune.
   ; ###################################################################################

   tt0 = tt1 & yy0 = test_y[i] ;  Reset new begin time to current end time and new phase
endfor ;                                     angle standard to current phase angle value.

test_x2 = x & test_y2[0L:(count3-1L)] = y[0L:(count3-1L)] ;  Store final time values and
test_y2[count3:(count3+count-1L)] = test_y ;  final results of analysis and "cleanup" of
test_y2[(count3+count):(sum-1L)] = y[(count3+count):(sum-1L)] ;      spin phase values...

avg_spinrate = twopi/double(avg_T) & stndev_spinrate = 0.0 & faultval = 0 ;  Final rates.
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;@@@@@@@@@@@@@@@@@@@@@@@ Write Final Results to CDF File @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
if writeCDF eq true then begin ;                        If user opts for writing data...
   ;*************************** BEGIN Changable Section ********************************
   id = cdf_open(cdf_location+'wi_k0_spha_20011106_v02.cdf')
   cdf_attput,id,'MODS',3,'11/28/01 - Implementation of correct spin phase data'
   cdf_attput,id,'Data_version',0,'2' ;     MUST use next-larger ver. num. than original.
   cdf_attput,id,'Logical_file_id',0,'WI_K0_SPHA_20011106_V02' ;    Input new metadata...

   for i=0,860 do begin ;       Input all newly calculated spin phase (and fault) data...
      cdf_varput,id,'SPIN_PHASE',test_y2[936+i],Rec_Start=i,Interval=[1,1,1]
      ; cdf_varput,id,'AVG_SPIN_RATE',avg_spinrate,Rec_Start=i,Interval=[1,1,1]
      ; cdf_varput,id,'STNDEV_SPIN_RATE',stndev_spinrate,Rec_Start=i,Interval=[1,1,1]
      cdf_varput,id,'FAULT',faultval,Rec_Start=i,Interval=[1,1,1]
   endfor ;********************* END Changable Section *********************************

   cdf_close,id ;                                 Close the CDF file containing new data.
endif ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;@@@@@@@@@@@@@@@@@@@ Final Diagnostics: Plot "Cleaned Up" Data @@@@@@@@@@@@@@@@@@@@@@@@@
;***************************** BEGIN Changable Section *********************************
erase & plot,time_main,test_y2,yrange=[-8,8],xrange=[7.5e4,16.5e4]
oplot,[time_main[count3],time_main[count3]],[-100,100] ;     Beginning of "problem area".
oplot,[time_main[count3+count],time_main[count3+count]],[-100,100] ; End of "prob. area".
;***************************** END Changable Section ***********************************
stop,'Type .cont to continue.' ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;@@@@@@@@@@@@@@@@@@@@@@ Final Diagnostics: Check Spinphase @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; The plot generated here should look like two horizontal lines (alternating values from
;  *.dat file) intersected by two vertical lines (bounding the "problem interval").
if doCheck eq true then begin ;  If the user opts for checks of the new spinphase data...
   ;************************** BEGIN Changable Section *********************************
   openw,11,cdf_location+'check_spinphase_110601.dat' ;   Open file for spin phase check.
   ;************************** END Changable Section ***********************************
   y_new = fltarr(sum) & num_y_new = 0

   for i=0L,(sum-1L) do begin
      y_new[num_y_new] = test_y2[i] & num_y_new = num_y_new+1
   endfor

   y_new = y_new[0:(num_y_new-1)]
   delta_t = y_new & t1 = y_new[0] & t2 = y_new[0] & x = intarr(num_y_new)

   for i=0,(num_y_new-1) do begin
      if i eq 0 then begin
         t1 = y_new[0] & t2 = y_new[0] & delta_t[i] = t2-t1
      endif else begin
         t1 = y_new[i-1] & t2 = y_new[i] & delta_t[i] = t2-t1
      endelse

      x[i] = i & printf,11,i,delta_t[i],test_y2[i]
   endfor

   close,11 & erase & plot,x,delta_t,Psym=3,Yrange=[-6,6],Ystyle=1,Xstyle=1
   ;************************** BEGIN Changable Section *********************************
   oplot,[936,936],[-10,10] & oplot,[1800,1800],[-10,10] ; Close file, plot check rsults.
   ;************************** END Changable Section ***********************************
endif ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

end