function isee_sec_pb5,seconds

;converts seconds from isee epoch to pb5 time (y d millisec of day)

epoch=long([1977,0,0])
secyr=double(365*86400)
yr_epoch=long(seconds/secyr)
dday=long((seconds-yr_epoch*secyr)/86400.d)
msec=long((seconds-yr_epoch*secyr-dday*86400.d)*1000)

timpb5=long([1977l+yr_epoch, dday+1, msec])

return,timpb5

end
  


