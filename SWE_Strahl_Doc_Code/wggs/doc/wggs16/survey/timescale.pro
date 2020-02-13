pro timescale,range_mins,xh0,xh1,xrange,xtickv,xtickname,xticks,minor_plot,$
  refpb5,lablepb5

min_day=double(1440)
datez=19941129l
pb5z=ymd_pb5(datez)
ref_elapsec=pb5_elapsec(refpb5,pb5z)

minmx=[5.,7.,8.,10.,12.,14.,16.,18.,24.,30.,36.,48.,60.,120.,180.,240.,360.,720.,1080.,1441.]
min_tick=[1,1.,2.,2.,2.,2.,2., 3., 4., 5., 6., 8.,10., 20., 30., 40., 60.,120., 180., 240.]
minor=   $
[4,4,2,  2 ,2,2,2,  3,  4, 5,  3,  4,  5,   4,   5,   4,   4,   4,    6,    4]
minmx=double(minmx)
min_tick=double(min_tick)
w=where(minmx ge range_mins)
print,'range_mins ',range_mins

;daymx=    double([12., 18., 24., 30.])
day_tick= 3.0  ;double([2.,  3.,  3.,  4.])
dayminor= 6       ;[4,   4,   6,   4 ]
;wd=where(min_day*daymx ge range_mins)
 
if w(0) ne -1 then begin
  
  min_tick_plot=min_tick(w(0))
  minor_plot=minor(w(0))
  d_xtick=double(min_tick_plot)/60.d0 
  x_begin=fix(xh0/d_xtick)*d_xtick 
  n_end=fix(xh1/d_xtick)
  if xh1-n_end*d_xtick eq 0 then x_end=n_end*d_xtick $
  else x_end=(n_end+1)*d_xtick
  xticks=fix((x_end-x_begin)/d_xtick + 0.00001)
  xtickv=x_begin+indgen(xticks+1)*d_xtick
  
  xrange=[x_begin,x_end]
  
  hour_hms,xtickv,hms,hm=hm,/mod24
  xtickname=hm
  lablepb5=elapsec_pb5(ref_elapsec+x_begin*3600.d,pb5z) 
   
endif else if range_mins le 7 * 1440. then begin ;multiple days

  min_tick_plot=min_day
  minor_plot=6
  d_xtick=min_tick_plot/60.d0
  d_minor=d_xtick/minor_plot
  n_begin=fix(xh0/d_xtick)
  if xh0-d_minor-n_begin*d_xtick le 0 then x_begin=n_begin*d_xtick $
  else begin
    x_begin=xh0-d_minor
    n_begin=n_begin+1
  endelse
  n_end=fix((xh1+d_minor)/d_xtick)  
  if (xh1+d_minor)-n_end*d_xtick le d_minor then x_end=n_end*d_xtick $
  else x_end=xh1+d_minor
  xticks=n_end-n_begin
  xtickv=n_begin*d_xtick+indgen(xticks+1)*d_xtick
  xrange=[x_begin,x_end]
  
  xtickvsec=ref_elapsec+xtickv*3600.d 
  xtickname=strarr(n_elements(xtickv))
  for i=0,n_elements(xtickv)-1 do begin
    xtickvpb5=elapsec_pb5(xtickvsec(i),pb5z)
    xtickvymd=string(pb5_ymd(xtickvpb5),format='(i8)')
    xtickname(i)=strmid(xtickvymd,6,2)
  endfor  
   
  lablepb5=elapsec_pb5(ref_elapsec+x_begin*3600.d,pb5z)
       
  
endif else if range_mins gt 7*1440. then begin
  min_tick_plot=min_day*day_tick
  minor_plot=dayminor
  d_xtick=double(min_tick_plot)/60.d0
  n_begin=fix(xh0/d_xtick) 
  x_begin=n_begin*d_xtick 
  n_end=fix(xh1/d_xtick)
  if xh1-n_end*d_xtick gt 0 then n_end=n_end+1
  x_end=n_end*d_xtick
  xticks=n_end-n_begin
  ;if xh1-n_end*d_xtick eq 0 then x_end=n_end*d_xtick $
  ;else x_end=(n_end+1)*d_xtick
  ;xticks=fix((x_end-x_begin)/d_xtick + 0.00001)
  xtickv=x_begin+indgen(xticks+1)*d_xtick
  xrange=[x_begin,x_end]
  
  xtickvsec=ref_elapsec+xtickv*3600.d 
  xtickname=strarr(n_elements(xtickv))
  for i=0,n_elements(xtickv)-1 do begin
    xtickvpb5=elapsec_pb5(xtickvsec(i),pb5z)
    xtickvymd=string(pb5_ymd(xtickvpb5),format='(i8)')
    xtickname(i)=strmid(xtickvymd,6,2)
  endfor  
  
  lablepb5=elapsec_pb5(ref_elapsec+x_begin*3600.d,pb5z) 
   
endif

end