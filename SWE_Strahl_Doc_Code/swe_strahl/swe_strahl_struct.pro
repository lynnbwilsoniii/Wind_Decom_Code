pro swe_strahl_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

if swe_electrons eq 0 then d.pnl(k).ztitle='SWE Strahl Electrons'

case varbl of

        'strahl energy spectrum' : begin
           d.pnl(k).labl='strahl energy'
           d.pnl(k).range=[2,31]
           d.pnl(k).pltype='z(x,y)'
           endcase

        'strl_max_cts' : begin
           d.pnl(k).labl='Strahl!C'  ;'s'  ;'strl'
           d.pnl(k).ticks=2
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           d.pnl(k).pltype='y(x)'
           endcase

        'pa(strl_max_cts)' : begin
           d.pnl(k).labl='sp'  ;'pa'
           d.pnl(k).range=[0,180]
           d.pnl(k).ticks=2
           d.pnl(k).minor=2
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           d.pnl(k).pltype='y(x)'
           endcase

        'strl-widthmx' : begin
           d.pnl(k).labl='Strahl width!C'  ;'sw'  ;'strl wdth'
           d.pnl(k).range=[0,50]  ;30]
           d.pnl(k).ticks=2  ;3
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           d.pnl(k).pltype='y(x)'
           endcase 
        
        
        'anti_strl_max_cts' : begin
           d.pnl(k).labl='Anti-strahl!C' ;'anti-strl'
           d.pnl(k).psym=1
           d.pnl(k).symsize=0.2
           d.pnl(k).pltype='y(x)'
           endcase

        'pa(anti_strl_max_cts)' : begin
           d.pnl(k).labl='asp' ;'anti-pa'
           d.pnl(k).range=[0,180]
           d.pnl(k).ticks=2
           d.pnl(k).minor=2
           d.pnl(k).psym=1
           d.pnl(k).symsize=0.2
           d.pnl(k).pltype='y(x)'
           endcase

        'anti-strl-widthmx' : begin
           d.pnl(k).labl='Anti-strahl width!C'  ;'anti-strl wdth'
           d.pnl(k).range=[0,50]
           d.pnl(k).ticks=2 ;3
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           d.pnl(k).pltype='y(x)'
           endcase 
           
        ;magnitude of magnetic field
          'B magnetic field': begin
            d.pnl(k).labl='B'  ;(nT)
            d.pnl(k).range=[0,20]  ;[0,10];[0,50];[5,10] ;
            d.pnl(k).ticks=2
            d.pnl(k).psym=1
           d.pnl(k).symsize=0.2
           d.pnl(k).pltype='y(x)'
            endcase

         ;azimuth of magnetic field
         'th_b': begin
            d.pnl(k).labl='thb'  ; (gse)'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
            d.pnl(k).pltype='y(x)'
            endcase


         ;elevation of magnetic field
         'ph_b': begin
            d.pnl(k).labl='phb'  ; (gse)'  ;(deg)
            d.pnl(k).range=[0,360]  ;[0,50];
            d.pnl(k).psym=1
           d.pnl(k).symsize=0.2
           d.pnl(k).pltype='y(x)'
            endcase

endcase

end

