function getrelgains_tbl_flnm

common wstuff,wst

rgflnm=''

if long(wst.lzdate) le 19971027l then $ 
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19941130_19971027.dat' $
       
else if long(wst.lzdate) ge 19971028l and long(wst.lzdate) le 19980514l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19971028_19980514.dat' $
        
else if long(wst.lzdate) ge 19980515l and long(wst.lzdate) le 19980610l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19980515_19980610.dat' $
  
else if long(wst.lzdate) ge 19980611l and long(wst.lzdate) le 19980728l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19980611_19980728.dat' $  

else if long(wst.lzdate) ge 19980729l and long(wst.lzdate) le 19980811l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19980729_19980811.dat' $  

else if long(wst.lzdate) ge 19980812l and long(wst.lzdate) le 19980829l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19980812_19980829.dat' $

else if long(wst.lzdate) ge 19980830l and long(wst.lzdate) le 19981025l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19980830_19981025.dat' $

else if long(wst.lzdate) ge 19981026l and long(wst.lzdate) le 19981124l then $
  rgflnm='' $
  ;rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19981026_19981124.dat' $

else if long(wst.lzdate) ge 19981125l and long(wst.lzdate) le 19981216l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19981125_19981216.dat' $

else if long(wst.lzdate) ge 19990108l and long(wst.lzdate) le 19990201l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990108_19990201.dat' $

else if long(wst.lzdate) ge 19990202l and long(wst.lzdate) le 19990208l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990202_19990208.dat' $

else if long(wst.lzdate) ge 19990209l and long(wst.lzdate) le 19990211l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990209_19990211.dat' $
    
else if long(wst.lzdate) ge 19990212l and long(wst.lzdate) le 19990325l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990212_19990325.dat' $

else if long(wst.lzdate) ge 19990326l and long(wst.lzdate) le 19990404l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990326_19990404.dat' $

else if long(wst.lzdate) ge 19990405l and long(wst.lzdate) le 19990505l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990405_19990505.dat' $

else if long(wst.lzdate) ge 19990506l and long(wst.lzdate) le 19990604l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990506_19990604.dat' $
  
else if long(wst.lzdate) ge 19990605l and long(wst.lzdate) le 19990704l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990605_19990704.dat' $
  
else if long(wst.lzdate) ge 19990705l and long(wst.lzdate) le 19990725l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990705_19990725.dat' $

else if long(wst.lzdate) ge 19990726l and long(wst.lzdate) le 19990902l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990726_19990902.dat' $

else if long(wst.lzdate) ge 19990903l and long(wst.lzdate) le 19991004l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19990903_19991004.dat' $

else if long(wst.lzdate) ge 19991005l and long(wst.lzdate) le 19991102l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19991005_19991102.dat' $

else if long(wst.lzdate) ge 19991103l and long(wst.lzdate) le 19991117l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19991103_19991117.dat' $

else if long(wst.lzdate) ge 19991203l and long(wst.lzdate) le 20000101l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_19991203_20000101.dat' $

else if long(wst.lzdate) ge 20000128l and long(wst.lzdate) le 20000226l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000128_20000226.dat' $

else if long(wst.lzdate) ge 20000227l and long(wst.lzdate) le 20000328l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000227_20000328.dat' $

else if long(wst.lzdate) ge 20000329l and long(wst.lzdate) le 20000427l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000329_20000427.dat' $

else if long(wst.lzdate) ge 20000428l and long(wst.lzdate) le 20000525l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000428_20000525.dat' $

else if long(wst.lzdate) ge 20000526l and long(wst.lzdate) le 20000611l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000526_20000611.dat' $

else if long(wst.lzdate) ge 20000612l and long(wst.lzdate) le 20000625l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000612_20000625.dat' $
                                
else if long(wst.lzdate) ge 20000626l and long(wst.lzdate) le 20000709l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000626_20000709.dat' $

else if long(wst.lzdate) ge 20000710l and long(wst.lzdate) le 20000724l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000710_20000724.dat' $
  
else if long(wst.lzdate) ge 20000725l and long(wst.lzdate) le 20000806l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000725_20000806.dat' $

else if long(wst.lzdate) ge 20000807l and long(wst.lzdate) le 20000822l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000807_20000822.dat' $

else if long(wst.lzdate) ge 20000823l and long(wst.lzdate) le 20000919l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000823_20000919.dat' $

else if long(wst.lzdate) ge 20000920l and long(wst.lzdate) le 20001022l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000920_20001022.dat' $
    
else if long(wst.lzdate) ge 20001023l and long(wst.lzdate) le 20001110l then $
  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20001023_20001110.dat' $
  
  else if long(wst.lzdate) ge 20001111l and long(wst.lzdate) le 20001125l then $
rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20001111_20001125.dat' $
  
  else if long(wst.lzdate) ge 20001126l and long(wst.lzdate) le 20001214l then $
rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20001126_20001214.dat' $

  else if long(wst.lzdate) ge 20001215l and long(wst.lzdate) le 20010115l then $
rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20001215_20010115.dat' $

  else if long(wst.lzdate) ge 20010116l and long(wst.lzdate) le 20010213l then $
rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20010116_20010213.dat' $
    
  else if long(wst.lzdate) ge 20010214l and long(wst.lzdate) le 20010314l then $
rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20010214_20010314.dat' $

  else if long(wst.lzdate) ge 20010315l and long(wst.lzdate) le 20010412l then $
rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20010315_20010412.dat' $
     
  else if long(wst.lzdate) ge 20010413l and long(wst.lzdate) le 20010513l then $
rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20010413_20010513.dat' $

  else if long(wst.lzdate) ge 20010514l and long(wst.lzdate) le 20010610l then $
rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20010514_20010610.dat' 
    
;else if long(wst.lzdate) eq 20000715l then $
;  rgflnm='swelz/swecal/gains/gainsrev/getrelgains_tbl_20000715_20000715.dat' $ 

return,rgflnm
      
end