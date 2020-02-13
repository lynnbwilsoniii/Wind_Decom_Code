pro wav_nekp_timerange,tmn,tmx,idatyp

common shared,d

idatype=where(d.datype eq 'wav_nekp')

tmn = d.wav_nekpdat(d.ndx(0,idatype)).ta < tmn
tmx = d.wav_nekpdat(d.ndx(1,idatype)).ta > tmx
                
end               
                
                
                
                
                
                
                
                
                
                
                
                
                
                