pro wav_hrtnr_timerange,tmn,tmx,idatyp

common shared,d

idatype=where(d.datype eq 'wav_hrtnr')

tmn = d.wav_hrtnr_spctrm.ta(d.ndx(0,idatype)) < tmn
tmx = d.wav_hrtnr_spctrm.ta(d.ndx(1,idatype)) > tmx
                
end               
                
                
                
                
                
                
                
                
                
                
                
                
                
                