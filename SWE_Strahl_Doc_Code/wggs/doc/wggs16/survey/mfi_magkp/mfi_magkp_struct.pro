pro mfi_magkp_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

if swe_electrons eq 0 then d.pnl(k).ztitle='WIND magkp'

case varbl of

         'mag_bkp': begin
            d.pnl(k).labl='Bkp'  ;(nT)
            d.pnl(k).range=[0,20]  ;[0,10];[0,30]  ;[0,60];
            d.pnl(k).ticks=2
            d.pnl(k).minor=3
            d.pnl(k).psym=0;3
            endcase

         'th_bkp': begin
            d.pnl(k).labl='th_bkp'  ; (gse)'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

         'ph_bkp': begin
            d.pnl(k).labl='ph_bkp'  ; (gse)'  ;(deg)
            d.pnl(k).range=[0,360]
            d.pnl(k).psym=3
            endcase


         ;gse x-component of kp mag field
         'bx_kp': begin
            d.pnl(k).labl='Bx_kp'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            endcase

         ;gse y-component of kp mag field
         'by_kp': begin
            d.pnl(k).labl='By_kp'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            endcase

          ;gse z-component of kp mag field
          'bz_kp': begin
            d.pnl(k).labl='Bz_kp'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            endcase
 
           'R fshck': begin
            d.pnl(k).labl='R fshck'
            d.pnl(k).range=[-100.,100.]
            d.pnl(k).ticks=4
            d.pnl(k).psym=3
            endcase

         'X fshck': begin
            d.pnl(k).labl='X fshck'
            d.pnl(k).range=[-50.,50.]
            d.pnl(k).ticks=4
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase
            
         'magkp pressure':  begin
            d.pnl(k).labl='P(b)' ;1e-10
            d.pnl(k).range=[0.1,100.]  ;[0,8];
            d.pnl(k).plotio=1
            d.pnl(k).ticks=1
            d.pnl(k).minor=5
            d.pnl(k).psym=3
            endcase  

endcase

d.pnl(k).pltype='y(x)'

end

