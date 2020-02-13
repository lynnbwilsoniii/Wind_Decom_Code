pro swe_ionkp_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

if swe_electrons eq 0 then d.pnl(k).ztitle='SWE ionkp'

case varbl of

        'Ui flow speed':  begin
           d.pnl(k).labl='Ui'  ;(km/s)
           d.pnl(k).range=[200.,800.];[0,1000]
           d.pnl(k).ticks=2
           d.pnl(k).minor=3
           d.pnl(k).psym=1;3
           d.pnl(k).symsize=0.4
           endcase

        'Uew':  begin
           d.pnl(k).labl='VEW'  ;(deg)
           d.pnl(k).range=[-20.,20.]
           d.pnl(k).ticks=2
           d.pnl(k).psym=3
           d.pnl(k).horizlin=0.
           endcase
    
        'Uns':  begin
           d.pnl(k).labl='VNS'  ;(deg)
           d.pnl(k).range=[-20.,20.]
           d.pnl(k).ticks=2
           d.pnl(k).psym=3
           d.pnl(k).horizlin=0.
           endcase
           
        ;solar wind speed gse components
         'Ux': begin
            d.pnl(k).labl='Uix'
            d.pnl(k).range=[-800,0]
            d.pnl(k).horizlin=0.
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.3
            endcase

         'Uy': begin
            d.pnl(k).labl='Uiy'
            d.pnl(k).range=[-100,100];[-100,100]
            d.pnl(k).horizlin=0.
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.3
            endcase

         'Uz': begin
            d.pnl(k).labl='Uiz'
            d.pnl(k).range=[-100,100]  ;[-100,100]
            d.pnl(k).horizlin=0.
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.3
            endcase
            
        'W thermal speed': begin
           pnl(k).labl='Wp (km/s)'
           d.pnl(k).range=[0,200]
           d.pnl(k).ticks=2
           d.pnl(k).psym=0;3
           endcase

        'Ti temperature' : begin
           d.pnl(k).labl='Ti'  ;(deg K)
           d.pnl(k).range=[1.e4,1.e6];[5.e4,1.e6]  ;[2.e5,2.e6]  ;94 dec01
           d.pnl(k).plotio=1
           d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.4
           endcase

        'Ni density': begin
           d.pnl(k).labl='Ni'  ;(/cm^3)
           wlt1=where(d.swe_ionkpdat.n lt 1.)
           if wlt1(0) eq -1 then $
             d.pnl(k).range=[1.,100.] else $
             d.pnl(k).range=[0.1,100.]
           d.pnl(k).plotio=1
           d.pnl(k).ticks=2
           d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.4
           endcase

        'plasma pressure (i+e)': begin 
           d.pnl(k).labl='P(i+e)'  ;1.e-10  
           d.pnl(k).range=[0.1,100.]  ;[0,8];
           d.pnl(k).plotio=1
           d.pnl(k).ticks=1
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase
           
        'ion + magkp pressure' : begin 
           d.pnl(k).labl='P(i+b)'  ;1.e-10  
           d.pnl(k).range=[0.1,100.]  ;[0,8];
           d.pnl(k).plotio=1
           d.pnl(k).ticks=1
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase
        
        'ion pressure': begin 
           d.pnl(k).labl='P(i)'  ;1.e-10  
           d.pnl(k).range=[0.1,100.]  ;[0,8];
           d.pnl(k).plotio=1
           d.pnl(k).ticks=1
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase

        'ram pressure': begin 
           d.pnl(k).labl='Ram (nPa)'  ;1e-8 dynes/cm^2 = nPa  
           d.pnl(k).range=[0.1,100.]  ;[0.1,10.] 
           d.pnl(k).plotio=1
           d.pnl(k).ticks=1
           d.pnl(k).minor=4
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase
           
        'total pressure (i+e+b)': begin 
          d.pnl(k).labl='P(tot)'  ;1.e-10  
          d.pnl(k).range=[0.1,100.] ;[0,8];[0.1,10.];
          d.pnl(k).plotio=1
          d.pnl(k).ticks=1
          d.pnl(k).minor=4
          d.pnl(k).psym=4
          d.pnl(k).symsize=0.2
          endcase


        'plasma beta': begin
           d.pnl(k).labl='beta'    
           d.pnl(k).range=[0.1,50.]
           d.pnl(k).ticks=3
           d.pnl(k).plotio=1
           d.pnl(k).psym=3
           endcase

         'Ue - Ui': begin
           d.pnl(k).labl='Ue-Ui'  ;(km/s)
           d.pnl(k).range=[-200.,200.];[0,1000]
           d.pnl(k).ticks=2
           d.pnl(k).psym=0;3
           d.pnl(k).horizlin=0.
           endcase

         'Te / Ti': begin
           d.pnl(k).labl='Te/Ti'  
           d.pnl(k).range=[0,10]
           d.pnl(k).ticks=2
           d.pnl(k).psym=0;3
           endcase

         'V ionacous thresh' : begin
             d.pnl(k).labl='Vthrsh'
             d.pnl(k).range=[0,2500]
             d.pnl(k).plotio=0
             ;d.pnl(k).psym=3
             d.pnl(k).oplot=1
             d.pnl(k).oplotvar='U flow speed'
             endcase 
                  
         'QI' : begin
            d.pnl(k).labl='QI' ;1e-10
            d.pnl(k).range=[0,100]  ;[0.1,100.]  ;[0,8];
            d.pnl(k).plotio=0;1
            d.pnl(k).ticks=4
            d.pnl(k).minor=1
            d.pnl(k).psym=3
         endcase
         
         'Te (interpolated)' : begin
           d.pnl(k).labl='Te(i)'  ;(deg K)
           d.pnl(k).range=[1.e4,1.e6];[5.e4,1.e6]  ;[2.e5,2.e6]  ;94 dec01
           d.pnl(k).plotio=1
           d.pnl(k).psym=0;3
           endcase
           
              
         'Te (avg on ion times)' : begin
            d.pnl(k).labl='Te avg'  ;(deg K)
            d.pnl(k).range=[1.e4,1.e6]
            d.pnl(k).plotio=1
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.3  
            endcase
             
         'Bx (mfikp avg on ion times)' : begin
            d.pnl(k).labl='Bx avg'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.3  
            endcase
 
         'By (mfikp avg on ion times)' : begin
            d.pnl(k).labl='By avg'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.3  
            endcase
            
         'Bz (mfikp avg on ion times)' : begin
            d.pnl(k).labl='Bz avg'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.3  
            endcase   
            
         'B (mfikp avg on ion times)' : begin
            d.pnl(k).labl='B avg'  ; (gse)'
            d.pnl(k).range=[0,20]
            d.pnl(k).horizlin=0.
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.3  
            endcase       
endcase

d.pnl(k).pltype='y(x)'

end

