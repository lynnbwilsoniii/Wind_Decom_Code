pro swe_moments_timerange,tmn,tmx,idatyp

common shared,d

tmn = d.swe_mdat(d.ndx(0,idatyp)).ta < tmn
tmx = d.swe_mdat(d.ndx(1,idatyp)).ta > tmx
                
end               
                
                
                
                
                
                
                
                
                
                
                
                
                
                