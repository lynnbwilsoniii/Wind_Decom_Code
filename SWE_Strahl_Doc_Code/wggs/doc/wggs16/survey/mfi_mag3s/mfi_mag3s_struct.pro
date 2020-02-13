pro mfi_mag3s_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

if swe_electrons eq 0 then d.pnl(k).ztitle='WIND mag3s'

case varbl of
         
         'mag_b3s': begin
            d.pnl(k).labl='B3s'  ;(nT)
            d.pnl(k).range=[0,30]  ;[0,10];[0,30]  
            d.pnl(k).ticks=2
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.2
            endcase

         'th_b3s': begin
            d.pnl(k).labl='th b3s'  ; (gse)'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

         'ph_b3s': begin
            d.pnl(k).labl='phi b3s'  ; (gse)'  ;(deg)
            d.pnl(k).range=[0,360]
            ;d.pnl(k).psym=3
            endcase

           'R fshck': begin
            d.pnl(k).labl='R fshck'
            d.pnl(k).range=[-100.,100.]
            d.pnl(k).ticks=4
            d.pnl(k).psym=3
            endcase

         'X fshck': begin
            d.pnl(k).labl='X fshck (i+3sm)'
            d.pnl(k).range=[-30.,10.]
            d.pnl(k).ticks=4
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.0
            endcase

endcase

d.pnl(k).pltype='y(x)'

end

