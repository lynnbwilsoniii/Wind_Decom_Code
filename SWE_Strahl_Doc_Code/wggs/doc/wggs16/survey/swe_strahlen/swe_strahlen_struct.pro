pro swe_strahlen_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

if swe_electrons eq 0 then d.pnl(k).ztitle='SWE strahl electrons'

d.pnl(k).subtitle=strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp))),$
     strlen(d.flnm(idatyp))-strlen(getenv(d.pathenv(idatyp))))
     
case varbl of


        'strlen0' : begin
           d.pnl(k).labl='s'  ;'strl'
           d.pnl(k).range=[0,400]
           d.pnl(k).ticks=2  
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase
        
        'paen0' : begin
           d.pnl(k).labl='sp'  ;'pa'
           d.pnl(k).range=[0,180]
           d.pnl(k).ticks=2
           d.pnl(k).minor=2
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase  
            
        'strlen0-wdth' : begin
           d.pnl(k).labl='sw'  ;'strl wdth'
           d.pnl(k).range=[0,40]  ;30]
           d.pnl(k).ticks=2  ;3
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase 
        
        
        'anti_strlen0' : begin
           d.pnl(k).labl='as'  ;'anti-strl'
           d.pnl(k).psym=1
           d.pnl(k).symsize=0.2
           endcase
        
        'anti_paen0' : begin
           d.pnl(k).labl='asp'  ;'pa'
           d.pnl(k).range=[0,180]
           d.pnl(k).ticks=2
           d.pnl(k).minor=2
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase
            
        'anti-strlen0-wdth' : begin
           d.pnl(k).labl='asw'  ;'anti-strl wdth'
           d.pnl(k).range=[0,60]
           d.pnl(k).ticks=3
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase 


        'strlen1' : begin
           d.pnl(k).labl='s'  ;'strl'
           d.pnl(k).range=[0,400]
           d.pnl(k).ticks=2  ;3
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase
         
        'paen1' : begin
           d.pnl(k).labl='sp'  ;'pa'
           d.pnl(k).range=[0,180]
           d.pnl(k).ticks=2
           d.pnl(k).minor=2
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase
           
        'strlen1-wdth' : begin
           d.pnl(k).labl='sw'  ;'strl wdth'
           d.pnl(k).range=[0,40]  ;30]
           d.pnl(k).ticks=2  ;3
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase 
        
        
        'anti_strlen1' : begin
           d.pnl(k).labl='as'  ;'anti-strl'
           d.pnl(k).psym=1
           d.pnl(k).symsize=0.2
           endcase
        
        'anti_paen1' : begin
           d.pnl(k).labl='asp'  ;'pa'
           d.pnl(k).range=[0,180]
           d.pnl(k).ticks=2
           d.pnl(k).minor=2
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase
           
        'anti-strlen1-wdth' : begin
           d.pnl(k).labl='asw'  ;'anti-strl wdth'
           d.pnl(k).range=[0,60]
           d.pnl(k).ticks=3
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase 

endcase

d.pnl(k).pltype='y(x)'


end

