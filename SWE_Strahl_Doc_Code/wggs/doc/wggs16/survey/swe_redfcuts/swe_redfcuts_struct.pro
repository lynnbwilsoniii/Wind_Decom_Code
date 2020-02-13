pro swe_redfcuts_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

d.pnl(k).pltype='z(x,y)'

case varbl of

  'redf' : d.pnl(k).labl='redf'
               
  'fpara': d.pnl(k).labl='fpara'
     
  'fperp': d.pnl(k).labl='fperp'  
   
    
endcase

end

