.run wind_idl:wind_idl.pro
ok=0l
ch=0l
ok=w_channel_open(ch,"Offline")
IF ok EQ 1 THEN print, '     Ok, the selected channel (ch) is open...' ELSE print, '     The selected channel is NOT open!'
