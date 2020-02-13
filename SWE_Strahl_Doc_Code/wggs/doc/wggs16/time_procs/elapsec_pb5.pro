function elapsec_pb5,elapsec,pb5ref

;Given elapsed seconds from reference time and pb5 reference time, 
;  this function computes the pb5 time

;This is done by first converting pb5ref to sec from truncated julian 
;  date epoch using procedure pb5_sec.pro which takes into account the 
;  rollover in truncated julian date tjd to tjd=0 on pb5=[1995 283 0], Oct 10



secref=pb5_sec(pb5ref)
sec=secref+elapsec
if sec ge 10000*double(86400.) then sec=sec-10000*double(86400.)

pb5=sec_pb5(sec)
return,pb5

end
