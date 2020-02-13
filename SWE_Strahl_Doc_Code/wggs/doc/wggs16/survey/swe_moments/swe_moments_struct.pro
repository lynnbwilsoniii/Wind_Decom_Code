pro swe_moments_struct,varbl,k,idatyp,swe_electrons,swe_moments

common shared,d
common wstuff,wst

d.pnl(k).ztitle='SWE electrons'
savfil_id=strmid(d.flnm(idatyp),strlen(d.flnm(idatyp))-8,1)
if savfil_id eq 's' then $
  d.pnl(k).subtitle=strmid(d.flnm(idatyp),strlen(d.flnm(idatyp))-17,17)+$
     '  '+string(d.swe_mdat(d.ndx(0,0)).gains,format='(6f6.2)') else $
  d.pnl(k).subtitle=strmid(d.flnm(idatyp),strlen(getenv(d.pathenv(idatyp))),$
     strlen(d.flnm(idatyp))-strlen(getenv(d.pathenv(idatyp))))+$
     '  '+string(d.swe_mdat(d.ndx(0,0)).gains,format='(6f6.2)')

;--check for SWE VEIS mode2 date
  restore,getenv('SWEDATLIB')+'mode2_dates.txt'
  wm2=where(date_mode2 eq wst.indate)
  if wm2(0) ne -1 then begin
    print,' '
    print,'CAUTION: SWE mode2 moments. ' +$
    'Heat flux may be contaminated by sun glint.'
    print,' '
    yesmode2=1  
  endif else yesmode2=0
    
           
case varbl of

         ;density
         'N density': begin 
            d.pnl(k).labl='Ne'  ;(el/cm^3)
            wlt1=where(d.swe_mdat.fnout lt 1.)
            if wlt1(0) eq -1 then $
            d.pnl(k).range=[1.0,100.] else $
            d.pnl(k).range=[0.1,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.2
            d.pnl(k).oplot=1
            d.pnl(k).oplotvar='Ni density'
            endcase
     
         ;bulk speed
         'U flow speed': begin
            d.pnl(k).labl='Ue'  ;(km/s)
            d.pnl(k).range=[200.,800.];[0,1000]
            d.pnl(k).ticks=2
            d.pnl(k).minor=3
            ;d.pnl(k).psym=1;3
            ;d.pnl(k).symsize=0.2
            d.pnl(k).oplot=1
            d.pnl(k).oplotvar='Ui flow speed'
            endcase

         ;elevation of bulk speed
         'th_u': begin
            d.pnl(k).labl='thue'  ;(deg)
            d.pnl(k).range=[-30.,30.] ;[-20.,20.];[-90.,90.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=3
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

  
         ;azimuth of bulk speed
         'ph_u': begin
            d.pnl(k).labl='phue' ;(deg)
            d.pnl(k).range=[160.,200.];[0,360]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            d.pnl(k).horizlin=180.
            endcase

         ;bulk speed gse components
         'Ux (gse)': begin
            d.pnl(k).labl='Uex'
            d.pnl(k).range=[-800,0]
            d.pnl(k).horizlin=0.
            endcase

         'Uy (gse)': begin
            d.pnl(k).labl='Uey'
            d.pnl(k).range=[-100,100];[-100,100]
            d.pnl(k).horizlin=0.
            endcase

         'Uz (gse)': begin
            d.pnl(k).labl='Uez'
            d.pnl(k).range=[-100,100]  ;[-100,100]
            d.pnl(k).horizlin=0.
            endcase

         ;Alfvenic velocity fluctuations
         'v_Alfvenic': begin
            d.pnl(k).labl='v pred'
            d.pnl(k).range=[-800,800]
            d.pnl(k).horizlin=0.
            endcase
            
         ;temperature
         'T temperature': begin
            d.pnl(k).labl='Te'  ;(deg K)
            d.pnl(k).range=[1.e4,1.e6];[5.e4,1.e6]  ;[1.e4,1.e6];[2.e5,2.e6]
            d.pnl(k).plotio=1
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.2
            d.pnl(k).oplot=1
            d.pnl(k).oplotvar='Ti temperature'
            endcase

         ;anisotropy
         'A anisotropy': begin 
            d.pnl(k).labl='anis'  ;'Tpara/Tperp'
            d.pnl(k).range=[0.5,1.5];[0.,2.]
            d.pnl(k).ticks=2
            d.pnl(k).horizlin=1.0
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.2
            endcase


         ;heat flux
         'Q heat flux': begin
            if yesmode2 then d.pnl(k).labl='Q*' else $
            d.pnl(k).labl='Q'  ;(ergs cm^-2 s^-1) 
            d.pnl(k).range=[0.001,0.05]  ;[0.001,0.1]  ;[0.02,0.2]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            endcase

         ;elevation of heat flux
         'th_q': begin
            if yesmode2 then d.pnl(k).labl='thq*' else $
            d.pnl(k).labl='thq'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            d.pnl(k).minor=4
            ;d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

         ;azimuth of heat flux
         'ph_q': begin
            if yesmode2 then d.pnl(k).labl='phq*' else $
            d.pnl(k).labl='phq'  ;(deg)
            d.pnl(k).range=[0,360]
            d.pnl(k).ticks=2
            d.pnl(k).minor=4
            ;d.pnl(k).psym=3
            endcase
 

         ;gse x-component of mag field
         'Bx': begin
            d.pnl(k).labl='Bx'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            endcase

         ;gse y-component of mag field
         'By': begin
            d.pnl(k).labl='By'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            endcase

          ;gse z-component of mag field
          'Bz': begin
            d.pnl(k).labl='Bz'  ; (gse)'
            d.pnl(k).range=[-10,10]
            d.pnl(k).horizlin=0.
            endcase
 

          ;magnitude of magnetic field
          'B magnetic field': begin
            d.pnl(k).labl='B'  ;(nT)
            d.pnl(k).range=[0,30]  ;[0,10];[0,50];[5,10] ;
            d.pnl(k).ticks=3
            d.pnl(k).psym=1;0;3
            d.pnl(k).symsize=0.2
            endcase

         ;azimuth of magnetic field
         'th_b': begin
            d.pnl(k).labl='thb'  ; (gse)'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            d.pnl(k).minor=4
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase


         ;elevation of magnetic field
         'ph_b': begin
            d.pnl(k).labl='phb'  ; (gse)'  ;(deg)
            d.pnl(k).range=[0,360]  ;[0,50];
            d.pnl(k).minor=4
            d.pnl(k).psym=3
            endcase
  
         ;gyrotropy
         'gyrtrpy': begin
            d.pnl(k).labl='gtrpy' ;'gyrtrpy'
            d.pnl(k).range=[0,0.2];[-1.,1.]
            d.pnl(k).ticks=2
            endcase

          ;average energy: thermal speed
          'W thermal speed': begin 
            d.pnl(k).labl='We (km/s)'
            d.pnl(k).range=[1000,6000]
            d.pnl(k).ticks=2
            endcase

         ;average energy: 3kT/2 ev
         'enavg' : begin
            d.pnl(k).labl='eavg (eV)'
            d.pnl(k).range=[0.,100.]
            d.pnl(k).ticks=5
            endcase
         
         ;temperature (eV)
         'T eV' : begin
            d.pnl(k).labl='T eV'
            d.pnl(k).range=[0.,80.];100.]
            d.pnl(k).ticks=4
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.2
            endcase
         
         ;parallel temperature (eV)
         'T para eV' : begin
            d.pnl(k).labl='T para eV'
            d.pnl(k).range=[0.,80.];100.]
            d.pnl(k).ticks=4
            endcase
         
         ;perpendicular temperature (eV)
         'T perp eV': begin
            d.pnl(k).labl='T perp eV'
            d.pnl(k).range=[0.,80.];100.]
            d.pnl(k).ticks=4
            endcase 
                  
         'k(3/2Tpara+Tperp)' : begin
            d.pnl(k).labl='3/2 kTpara (eV)'
            d.pnl(k).range=[0.,250.]
            d.pnl(k).ticks=5
            endcase
         
         'htu'      : begin
            d.pnl(k).labl='htu (km/s)'
            d.pnl(k).range=[0.,5000.]
            d.pnl(k).ticks=5
         endcase
            
         'q/ne/htu' : begin
            d.pnl(k).labl='q/ne/htu (eV)'
            d.pnl(k).range=[0.,250.]
            d.pnl(k).ticks=5
            endcase
            
         'me/2 htu^2' : begin
            d.pnl(k).labl='me/2 htu^2 (eV)'
            d.pnl(k).range=[0.,250.]
            d.pnl(k).ticks=5
            endcase
         
         'htpot' : begin
            d.pnl(k).labl='htpot (eV)'
            d.pnl(k).range=[0.,250.]
            d.pnl(k).ticks=5
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
            if yesmode2 then d.pnl(k).labl='Q*.B' else $
            d.pnl(k).labl='Q.B'  ;'cos(Q,B)'
            d.pnl(k).range=[-1.,1.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

         ;heat flux gse components
         'Qx (gse)': begin
            if yesmode2 then d.pnl(k).labl='Qx*' else $
            d.pnl(k).labl='Qx'
            d.pnl(k).range=[-0.01,0.01]
            endcase

         'Qy (gse)': begin
            if yesmode2 then d.pnl(k).labl='Qy*' else $
            d.pnl(k).labl='Qy'
            d.pnl(k).range=[-0.01,0.01]
            endcase

         'Qz (gse)': begin 
            if yesmode2 then d.pnl(k).labl='Qz*' else $
            d.pnl(k).labl='Qz'
            d.pnl(k).range=[-0.01,0.01]
            endcase

         'magnetic pressure': begin
            d.pnl(k).labl='P(b)' ;1e-10
            d.pnl(k).range=[0.1,100.]  ;[0,8];
            d.pnl(k).plotio=1
            d.pnl(k).ticks=1
            d.pnl(k).minor=5
            d.pnl(k).psym=3
            endcase
 
         'elec plas pressure': begin
            d.pnl(k).labl='P(ele)' ;1e-10
            d.pnl(k).range=[0.1,100.];  [0,8];[0.1,100];[.001,.1];
            d.pnl(k).plotio=1
            d.pnl(k).ticks=1
            d.pnl(k).minor=5
            d.pnl(k).psym=3
            endcase

         'R fshck': begin
            d.pnl(k).labl='R fshck'
            d.pnl(k).range=[-100.,100.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

         'X fshck': begin
            d.pnl(k).labl='X fshck'
            d.pnl(k).range=[-30.,30.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=0;3
            d.pnl(k).horizlin=0.0
            endcase
        
        'A fshck': begin
            d.pnl(k).labl='A fshck'
            d.pnl(k).range=[-15.,15.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=0;3
            d.pnl(k).horizlin=0.0
            endcase
            
         'Xtanpt': begin
            d.pnl(k).labl='Xtanpt'
            d.pnl(k).range=[-50.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.0
            d.pnl(k).horizlin=0.
            endcase
            
         'ratio e/i density': begin
            d.pnl(k).labl='e/i ratio'
            d.pnl(k).range=[0.5,2.0]
            d.pnl(k).ticks=3
            d.pnl(k).psym=3
            d.pnl(k).horizlin=1.0
            d.pnl(k).olinestyle=2
            endcase

         'scpot': begin
            d.pnl(k).labl='scpot'
            d.pnl(k).range=[0,15]
            d.pnl(k).ticks=3
            d.pnl(k).minor=5
            d.pnl(k).psym=1;3
            d.pnl(k).symsize=0.2
            endcase

         
         ;temperature
         'T parallel': begin
            d.pnl(k).labl='Te para'  ;(deg K)
            d.pnl(k).range=[1.e4,1.e6]
            d.pnl(k).plotio=0;1
            ;d.pnl(k).psym=3
            d.pnl(k).oplot=0
            ;d.pnl(k).oplotvar='T perpendicular'
            endcase
            
        
         ;temperature
         'T perpendicular': begin
            d.pnl(k).labl='Te perp'  ;(deg K)
            d.pnl(k).range=[1.e4,1.e6]
            d.pnl(k).plotio=0;1
            ;d.pnl(k).psym=3
            d.pnl(k).oplot=0
            ;d.pnl(k).oplotvar='Ti temperature'
            endcase 
            
         'Qnml_qtherm' : begin
            d.pnl(k).labl='Q/qth'  
            d.pnl(k).range=[0.,1.0]
            d.pnl(k).psym=3
                         endcase 
                         
         'X (gse)' : begin
            d.pnl(k).labl='Xgse'
            d.pnl(k).range=[-50.,250.]
            d.pnl(k).ticks=6
            d.pnl(k).minor=5
                     endcase                  
  
         'Y (gse)' : begin
            d.pnl(k).labl='Ygse'
            d.pnl(k).range=[-100.,100.]
            d.pnl(k).ticks=5
            d.pnl(k).minor=4
                     endcase 
                      
         'Z (gse)' : begin
            d.pnl(k).labl='Zgse'
            d.pnl(k).range=[-100.,100.]
            d.pnl(k).ticks=5
            d.pnl(k).minor=4
                     endcase 
         
         ;core density
         'N core': begin 
            d.pnl(k).labl='N core'  ;(el/cm^3)
            wlt1=where(d.swe_mdat.fnout lt 1.)
            if wlt1(0) eq -1 then $
            d.pnl(k).range=[1.0,100.] else $
            d.pnl(k).range=[0.1,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            ;d.pnl(k).psym=3
            d.pnl(k).oplot=1
            d.pnl(k).oplotvar='Ni density'
            endcase
     
         ;core temperature
         'T core': begin
            d.pnl(k).labl='T core'  ;(deg K)
            d.pnl(k).range=[1.e4,1.e6];[5.e4,1.e6]  ;[1.e4,1.e6];[2.e5,2.e6]
            d.pnl(k).plotio=1
            ;d.pnl(k).psym=3
            d.pnl(k).oplot=1
            d.pnl(k).oplotvar='Ti temperature'
            endcase
         
         ;core bulk speed
         'U core flow speed': begin
            d.pnl(k).labl='Ue core'  ;(km/s)
            d.pnl(k).range=[0.,800.];[0,1000]
            d.pnl(k).ticks=2
            d.pnl(k).minor=4
            ;d.pnl(k).psym=3
            d.pnl(k).oplot=1
            d.pnl(k).oplotvar='Ui flow speed'
            endcase

         ;elevation of core bulk speed
         'th_u core': begin
            d.pnl(k).labl='thue core'  ;(deg)
            d.pnl(k).range=[-30.,30.] ;[-20.,20.];[-90.,90.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=3
            d.pnl(k).psym=3
            d.pnl(k).horizlin=0.
            endcase

  
         ;azimuth of core bulk speed
         'ph_u core': begin
            d.pnl(k).labl='phue core' ;(deg)
            d.pnl(k).range=[160.,200.];[0,360]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            d.pnl(k).horizlin=180.
            endcase

               
         'nodata' : begin
            d.pnl(k).labl='b dot nrml'
            d.pnl(k).range=[-1,1]
            d.pnl(k).ticks=4
            d.pnl(k).minor=5
            d.pnl(k).horizlin=0.
                     endcase   
          
         'Ni (interpolated)': begin 
            d.pnl(k).labl='Ni(i)'  ;(/cm^3)
            wlt1=where(d.swe_ionkpdat.n lt 1.)
            if wlt1(0) eq -1 then $
              d.pnl(k).range=[1.,100.] else $
              d.pnl(k).range=[0.1,100.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            d.pnl(k).psym=0;3           
            endcase
     
         
         'Ui (interpolated)': begin
            d.pnl(k).labl='Ui(i)'  ;(km/s)
            d.pnl(k).range=[0.,800.];[0,1000]
            d.pnl(k).ticks=2
            d.pnl(k).minor=4
            endcase
         
         'pxx' : begin
            d.pnl(k).labl='Pxx'  ;(km/s)
            d.pnl(k).range=[0.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pxy' : begin
            d.pnl(k).labl='Pxy'  ;(km/s)
            d.pnl(k).range=[-20.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pxz' : begin
            d.pnl(k).labl='Pxz'  ;(km/s)
            d.pnl(k).range=[-20.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pyy' : begin
            d.pnl(k).labl='Pyy'  ;(km/s)
            d.pnl(k).range=[0.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pyz' : begin
            d.pnl(k).labl='Pyz'  ;(km/s)
            d.pnl(k).range=[-20.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pzz' : begin
            d.pnl(k).labl='Pzz'  ;(km/s)
            d.pnl(k).range=[0.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pyx' : begin
            d.pnl(k).labl='Pyx'  ;(km/s)
            d.pnl(k).range=[-20.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pzx' : begin
            d.pnl(k).labl='Pzx'  ;(km/s)
            d.pnl(k).range=[-20.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pzy' : begin
            d.pnl(k).labl='Pzy'  ;(km/s)
            d.pnl(k).range=[-20.,20.]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            d.pnl(k).psym=1
            d.pnl(k).symsize=0.2
         endcase
         
         'pzz/pxx' : begin
            d.pnl(k).labl='pzz/pxx'  ;(km/s)
            d.pnl(k).range=[0.5,1.5]
            d.pnl(k).ticks=2
            d.pnl(k).minor=5
            ;d.pnl(k).psym=1
            ;d.pnl(k).symsize=0.2
            d.pnl(k).horizlin=1.0
         endcase
endcase

d.pnl(k).pltype='y(x)'

end

