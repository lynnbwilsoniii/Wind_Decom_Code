function pb5_sec,timpb5

;converts pb5 time (y d millisec of day) to seconds from truncated julian day epoch

;first convert pb5 time (y d millisec of day) to truncated julian day and millisec of day

;rollover in truncated julian day (to tjd=0) occurred pb5=[1995 283 0], Oct 10
;rollover is accounted for in procedure pb5_tjd.pro

tjdmsec=pb5_tjd(timpb5)
tjd=tjdmsec(0)

seconds=tjd*86400.d + timpb5(2)/1000.d

return,seconds

end
