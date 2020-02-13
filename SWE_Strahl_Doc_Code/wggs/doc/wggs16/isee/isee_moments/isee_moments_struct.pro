pro isee_moments_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

d.pnl(k).subtitle=strmid(d.flnm(idatyp),strlen(getenv('ISEEPATH')),$
     strlen(d.flnm(idatyp))-strlen(getenv('ISEEPATH')))
d.pnl(k).charsize=1.15
 
if swe_electrons eq 0 then d.pnl(k).ztitle='ISEE1 electrons'
     
case varbl of

         ;density
         'N density': begin 
            d.pnl(k).labl='Ne'  ;(el/cm^3)
            d.pnl(k).range=[0.1,100.];[0.1,10];[1.,10.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=3
            ;d.pnl(k).psym=3
            endcase
     
         ;bulk speed
         'U flow speed': begin
            d.pnl(k).labl='Ue'  ;(km/s)
            d.pnl(k).range=[0.,800.];[0,1000]
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         ;elevation of bulk speed
         'th_u': begin
            d.pnl(k).labl='thue'  ;(deg)
            d.pnl(k).range=[-90.,90.] ;[-20.,20.]
            d.pnl(k).ticks=4  ;2
            d.pnl(k).psym=3
            d.pnl(k).minor=3
            d.pnl(k).horizlin=0.
            endcase

  
         ;azimuth of bulk speed
         'ph_u': begin
            d.pnl(k).labl='phue' ;(deg)
            d.pnl(k).range=[0,360]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

         ;bulk speed gse components
         'Ux (gse)': begin
            d.pnl(k).labl='Uex'
            d.pnl(k).range=[-800,0]
            d.pnl(k).horizlin=0.
            endcase

         'Uy (gse)': begin
            d.pnl(k).labl='Uey'
            d.pnl(k).range=[-100,100]
            d.pnl(k).horizlin=0.
            endcase

         'Uz (gse)': begin
            d.pnl(k).labl='Uez'
            d.pnl(k).range=[-100,100]
            d.pnl(k).horizlin=0.
            endcase

  
         ;temperature
         'T temperature': begin
            d.pnl(k).labl='Te'  ;(deg K)
            d.pnl(k).range=[1.e5,1.e7];[5.e4,1.e6]  ;[1.e4,1.e6];[2.e5,2.e6]
            d.pnl(k).plotio=1
            ;d.pnl(k).psym=3
            endcase

         ;anisotropy
         'A anisotropy': begin 
            d.pnl(k).labl='anis'  ;'Tpara/Tperp'
            d.pnl(k).range=[0.5,2.0];[0.,2.]
            d.pnl(k).ticks=3
            d.pnl(k).horizlin=1.0
            endcase


         ;heat flux
         'Q heat flux': begin
            d.pnl(k).labl='Q'  ;(ergs cm^-2 s^-1) 
            d.pnl(k).range=[0.001,0.1];[0.02,0.2]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         ;elevation of heat flux
         'th_q': begin
            d.pnl(k).labl='thq'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

         ;azimuth of heat flux
         'ph_q': begin
            d.pnl(k).labl='phq'  ;(deg)
            d.pnl(k).range=[0,360]
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase
 

         ;gse x-component of mag field
         'Bx': begin
            d.pnl(k).labl='Bx'  ; (gse)'
            d.pnl(k).range=[-50,50]
            d.pnl(k).horizlin=0.
            endcase

         ;gse y-component of mag field
         'By': begin
            d.pnl(k).labl='By'  ; (gse)'
            d.pnl(k).range=[-50,50]
            d.pnl(k).horizlin=0.
            endcase

          ;gse z-component of mag field
          'Bz': begin
            d.pnl(k).labl='Bz'  ; (gse)'
            d.pnl(k).range=[-50,50]
            d.pnl(k).horizlin=0.
            endcase
 

          ;magnitude of magnetic field
          'B magnetic field': begin
            d.pnl(k).labl='B'  ;(nT)
            d.pnl(k).range=[0,20]  ;[0,10];[0,50];[5,10] ;
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

         ;azimuth of magnetic field
         'th_b': begin
            d.pnl(k).labl='thb'  ; (gse)'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase


         ;elevation of magnetic field
         'ph_b': begin
            d.pnl(k).labl='phb'  ; (gse)'  ;(deg)
            d.pnl(k).range=[0,360]  ;[0,50];
            d.pnl(k).psym=3
            endcase

          ;average energy/thermal speed
          'W thermal speed': begin 
            d.pnl(k).labl='We (km/s)'
            d.pnl(k).range=[1000,6000]
            d.pnl(k).ticks=2
            endcase

         ;angle between principle axis of pressure tensor and magnetic field
         'cos(Pa,B)': begin
            d.pnl(k).labl='P.B'  ;'cos(Pa,B)'
            d.pnl(k).range=[-1.,1.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

         ;angle between heat flux vector and magnetic field
         'cos(Q,B)': begin
            d.pnl(k).labl='Q.B'  ;'cos(Q,B)'
            d.pnl(k).range=[-1.,1.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

         ;heat flux gse components
         'Qx (gse)': begin
            d.pnl(k).labl='Qx'
            d.pnl(k).range=[-0.01,0.01]
            endcase

         'Qy (gse)': begin
            d.pnl(k).labl='Qy'
            d.pnl(k).range=[-0.01,0.01]
            endcase

         'Qz (gse)': begin 
            d.pnl(k).labl='Qz'
            d.pnl(k).range=[-0.01,0.01]
            endcase

         's/c potential': begin 
            d.pnl(k).labl='pot'
            d.pnl(k).range=[0,10]
            endcase


         'active exp (harvey)': begin 
            d.pnl(k).labl='harv'
            d.pnl(k).range=[0,5]
            d.pnl(k).psym=3
            endcase

         'active exp (mozer)': begin 
            d.pnl(k).labl='mozr'
            d.pnl(k).range=[0,5]
            d.pnl(k).psym=3
            endcase

         'R fshck': begin
            d.pnl(k).labl='R fshck'
            d.pnl(k).range=[-50.,50.]
            d.pnl(k).ticks=4
            d.pnl(k).psym=3
            endcase

         'X fshck': begin
            d.pnl(k).labl='X fshck'
            d.pnl(k).range=[-10.,10.]
            d.pnl(k).ticks=4
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

         'mode': begin 
            d.pnl(k).labl='mode'
            d.pnl(k).range=[-1,3]
            d.pnl(k).psym=3
            endcase

         'format': begin 
            d.pnl(k).labl='tmfrmt'
            d.pnl(k).range=[-1,2]
            d.pnl(k).psym=3
            endcase
 
         'X (gse)':begin
            d.pnl(k).labl='Xgse'
            d.pnl(k).range=[-20,20]
            d.pnl(k).horizlin=0.
            endcase
   
         'Y (gse)':begin
            d.pnl(k).labl='Ygse'
            d.pnl(k).range=[-20,20]
            d.pnl(k).horizlin=0.
            endcase

         'Z (gse)':begin
            d.pnl(k).labl='Zgse'
            d.pnl(k).range=[-20,20]
            d.pnl(k).horizlin=0.
            endcase

         'n (sector1)': begin 
            d.pnl(k).labl='nsect1'  ;(el/cm^3)
            d.pnl(k).range=[1.,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         'n (sector2)': begin 
            d.pnl(k).labl='nsect2'  ;(el/cm^3)
            d.pnl(k).range=[1.,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         'n (sector3)': begin 
            d.pnl(k).labl='nsect3'  ;(el/cm^3)
            d.pnl(k).range=[1.,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         'n (sector4)': begin 
            d.pnl(k).labl='nsect4'  ;(el/cm^3)
            d.pnl(k).range=[1.,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         'n (sector5)': begin 
            d.pnl(k).labl='nsect5'  ;(el/cm^3)
            d.pnl(k).range=[1.,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         'n (sector6)': begin 
            d.pnl(k).labl='nsect6'  ;(el/cm^3)
            d.pnl(k).range=[1.,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         'deig': begin
            d.pnl(k).labl='deig'  ;(deg K)
            d.pnl(k).range=[1.e3,1.e5]
            d.pnl(k).plotio=1
            ;d.pnl(k).psym=3
            endcase

         'eigval0': begin
            d.pnl(k).labl='eigval0'  ;(deg K)
            d.pnl(k).range=[1.e4,1.e6]
            d.pnl(k).plotio=1
            ;d.pnl(k).psym=3
            endcase 
              
         'eigval1': begin
            d.pnl(k).labl='eigval1'  ;(deg K)
            d.pnl(k).range=[1.e4,1.e6]
            d.pnl(k).plotio=1
            ;d.pnl(k).psym=3
            endcase
               
          'eigval2': begin
            d.pnl(k).labl='eigval2'  ;(deg K)
            d.pnl(k).range=[1.e4,1.e6]
            d.pnl(k).plotio=1
            ;d.pnl(k).psym=3
            endcase     
endcase

end

