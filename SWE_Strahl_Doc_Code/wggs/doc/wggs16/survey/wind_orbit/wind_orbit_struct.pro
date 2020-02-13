pro wind_orbit_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d

idatype=where(d.datype eq 'wind_orbit')

d.pnl(k).ztitle='WIND orbit'

d.pnl(k).subtitle=strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatype))),$
     strlen(d.flnm(idatyp))-strlen(getenv(d.pathenv(idatype))))
     
case varbl of

        
         'X (gse)' : begin
            d.pnl(k).labl='Xgse'
            d.pnl(k).range=[-50.,250.]
            d.pnl(k).ticks=6
            d.pnl(k).minor=5
            d.pnl(k).horizlin=0
                     endcase                  
  
         'Y (gse)' : begin
            d.pnl(k).labl='Ygse'
            d.pnl(k).range=[-100.,100.]
            d.pnl(k).ticks=5
            d.pnl(k).minor=4
            d.pnl(k).horizlin=0
                     endcase 
                      
         'Z (gse)' : begin
            d.pnl(k).labl='Zgse'
            d.pnl(k).range=[-100.,100.]
            d.pnl(k).ticks=5
            d.pnl(k).minor=4
            d.pnl(k).horizlin=0
                     endcase     

endcase

d.pnl(k).pltype='y(x)'

end

