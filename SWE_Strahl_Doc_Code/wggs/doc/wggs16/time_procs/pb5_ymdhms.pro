function pb5_ymdhms,pb5

;converts pb5 time to (string) yymmdd+' '+hh:mm:ss

yymmdd=string(ymd(pb5),format='(i8)')
;yymmdd=strmid(yymmdd,2,6)

hour_hms,double(pb5(2))/3600000.d,hms

return,yymmdd+' '+hms

end

