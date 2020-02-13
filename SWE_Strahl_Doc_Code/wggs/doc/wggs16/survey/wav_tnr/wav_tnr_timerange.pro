pro wav_tnr_timerange,tmn,tmx,idatyp

common shared,d

idatype=where(d.datype eq 'wav_tnr')

tmn = d.wav_tnr_spctrm.ta(d.ndx(0,idatype)) < tmn
tmx = d.wav_tnr_spctrm.ta(d.ndx(1,idatype)) > tmx
                
end               
                
                
                
                
                
                
                
                
                
                
                
                
                
                