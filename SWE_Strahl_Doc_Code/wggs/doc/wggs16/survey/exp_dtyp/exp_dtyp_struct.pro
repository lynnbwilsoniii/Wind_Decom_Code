This procedure stores the desired keywords to the IDL plot.pro,
such as plot labels, number of ticks, etc.

var1 through var2 are the plot variable names defined in exp_dtyp_list.pro.




pro exp_dtyp_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

if swe_electrons eq 0 then d.pnl(k).ztitle='exp_dtyp'

case varbl of

         'var1': begin
            d.pnl(k).labl='variable1' ;units
            d.pnl(k).range=[0,20] 
            d.pnl(k).ticks=2
            d.pnl(k).minor=3
            d.pnl(k).psym=0;3
            endcase

         'var2': begin
            d.pnl(k).labl='variable2' ;units
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

         'var3': begin
            d.pnl(k).labl='variable3' ;units
            d.pnl(k).range=[0,360]
            d.pnl(k).psym=3
            endcase


         'var4': begin
            d.pnl(k).labl='variable4' ;units
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            endcase

        
endcase

end

