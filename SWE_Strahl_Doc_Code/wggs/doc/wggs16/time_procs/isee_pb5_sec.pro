function isee_pb5_sec,timpb5

;converts pb5 time (y d millisec of day) to seconds from isee epoch [1977,0,0]

if timpb5(1) ge 1980l then stop,'bad isee year'

epoch=long([1977,0,0])
secyr=double(365*86400)
secdy=double(86400)
deltyr=timpb5(0)-epoch(0)
seconds=$
(timpb5(0)-epoch(0))*secyr+(timpb5(1)-1)*secdy+timpb5(2)/1000.d         

return,seconds

end
