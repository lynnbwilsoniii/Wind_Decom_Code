pro wav_nehr_timerange,tmn,tmx,idatyp

common shared,d

idatype=where(d.datype eq 'wav_nehr')

tmn = d.wav_nehrdat(d.ndx(0,idatype)).ta < tmn
tmx = d.wav_nehrdat(d.ndx(1,idatype)).ta > tmx
                
end               
                
                
                
                
                
                
                
                
                
                
                
                
                
                