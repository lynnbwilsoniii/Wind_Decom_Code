function ymdtime_sec,ymdtime

;converts date and time as long array [yr,mo,da,hr,m,s,ms] to 
;seconds from tjd epoch

;ymd=19000000l+ymdtime(0)*10000l+ymdtime(1)*100l+ymdtime(2)
ymd=ymdtime(0)*10000l+ymdtime(1)*100l+ymdtime(2)
pb5=ymd_pb5(ymd)  ;at begin of date
pb5(2)=ymdtime(3)*3600000l+ymdtime(4)*60000l+ymdtime(5)*1000l+ymdtime(6) ;ms of day

return,pb5_sec(pb5)

end
