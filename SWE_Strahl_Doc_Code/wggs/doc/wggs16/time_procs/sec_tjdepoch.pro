function sec_tjdepoch,tjd,ms

;returns seconds from tjd epoch given tjd and ms of day

tjdsec=tjd*86400.d
mssec=ms/1000.d

return, tjdsec+mssec

end
