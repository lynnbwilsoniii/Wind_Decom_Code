pro get_ticks,range_data,nticks_plot,min_tick_plot,minor_plot

;range_data in minutes
;range_plot in hours

min_tick=[1,2,5,6,10,15,20,30,60,120,180,240,360,720,1440]
nticks=[1,2,3,4,5,6]
minors=[1,2,3,4,5,6]
a=range_data/min_tick
wneint=where(double(fix(a))-a ne 0,nw)
if nw gt 0 then nticks_data=fix(a)+1 else nticks_data=fix(a)
wpossible=where(nticks_data ge min(nticks) and nticks_data le max(nticks))
range=nticks_data(wpossible)*min_tick(wpossible)
maxm=max(nticks_data(wpossible),wselect)
nticks_plot=nticks_data(wpossible(wselect))
min_tick_plot=min_tick(wpossible(wselect))

wpossible_minor=where(float(min_tick_plot)/minors-min_tick_plot/minors eq 0)
if wpossible_minor(0) ne -1 then $
 minor_plot=max(minors(wpossible_minor)) else minor_plot=1 


lpr=0
if lpr then begin
print,'min_tick ',min_tick
print,'nticks ',nticks
print,'hr_range_data ',hr_range_data
print,'range_data ',range_data
print,'nticks_data ',nticks_data
print,'nticks_data(wpossible) ',nticks_data(wpossible)
print,'min_tick(wpossible) ',min_tick(wpossible)
print,'range ',range
print,'nticks_plot ',nticks_plot
print,'min_tick_plot ',min_tick_plot
print,'minor_plot ',minor_plot
endif
end