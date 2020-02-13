function ms_hhmmss,ms

;converts ms of day to string hh:mm:ss

hour_hms,double(ms)/3600000.d,hms

return,hms

end
