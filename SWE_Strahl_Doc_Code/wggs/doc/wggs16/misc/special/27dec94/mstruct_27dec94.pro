pro mstruct,error=error

common sharewidg,wa
common includ1,mdir,fdir
common mpstuff,mflnm,pflnm,mdat,pdat,refsec
common selectstuff,ndx,datype,pnlist,pnl,pnlsel,$
  wmoments,wspnavg,wpitch,wstrahl,wionkp
common plt,$
y,xyrange,ylog,xywindow,xysize,ev_pnl,minmax,hardcopy,newticks,cscale,cf,$
nvmax_spnavg
common strlstuff, strlflnm,strldat
common ionkpstuff,ionkpflnm,ionkpdat
common shared,d

print, 'mstruct.pro'

newticks=0
if keyword_set(cf) eq 0 then cf=0  ;default is to plot compr counts

;pnlstr={dtp:'', varname:'',$
;    ztitle:'SWE electrons',$
;    labl:'',range:fltarr(2),ticks:2,minor:2,$
;    tickv:fltarr(30),tickname:strarr(30), subtitle:'',$
;    tmlabl:'',tmrange:dblarr(2),tmticks:2,tminor:6,fill:-1.e31,$
;    tmtickv:dblarr(30),tmtickname:strarr(30), $
;    plotio:0,psym:0,symsize:1.,oplot:0,oplotvar:-1,olinestyle:0,ocolor:225,$
;    pltype:'',enindx:0, indx:0, xory:0,ev_val:0,step:0.,$
;    lzrange:[[0,255],[-32.,-24.]],charthick:1.0,charsize:1.5}

;d.pnl=replicate(pnlstr,n_elements(d.pnlist.list))

pnlsel=d.pnlsel(where(d.pnlsel ne -1))
 
refpb5=sec_pb5(refsec)
hour_hms,refpb5(2)/3600000.d,hms1

;set time ticks (default)
     range_lower=[ 0,10,18,30,50, 90,120,180, 600,   1440]
     range_upper=[10,18,30,50,90,120,180,600,1440, 5*1440] 
     tickmarks  =[ 2, 4, 6,10,20, 30, 40, 60, 120, 240]

;time scale determined
  tmn=1.e30 & tmx=-1.e30

 ;organize the pnlist indices selected by data type
    for i=0,d.ndvar-1 do begin
      w=where(d.pnlist.dtp(pnlsel) eq d.datype(i),nw)
      if nw gt 0 then d.wdatype(0:nw-1,i)=w
    endfor
       ;d.wmoments=where(d.pnlist.dtp(pnlsel) eq d.datype(0))
       ;d.wspnavg=where(d.pnlist.dtp(pnlsel) eq d.datype(1))
      ; d.wpitch=where(d.pnlist.dtp(pnlsel) eq d.datype(2))
       ;d.wstrahl=where(d.pnlist.dtp(pnlsel) eq d.datype(3))
      ; d.wionkp=where(d.pnlist.dtp(pnlsel) eq d.datype(4)) 
       ;wmoments=where(d.wmoments ne -1)
       ;wspnavg=where(d.wspnavg ne -1)
      ; wpitch=where(d.wpitch ne -1)
       ;wstrahl=where(d.wstrahl ne -1)
       ;wionkp=where(d.wionkp ne -1)
       ;print,'wmoments ',wmoments
       ;print,'wspnavg ',wspnavg
       ;print,'wpitch ',wpitch
       ;print,'wstrahl ',wstrahl
       ;print,'wionkp ',wionkp

wflnm=where(d.flnm ne '')
flnm=''
for idatyp=0,n_elements(wflnm)-1 do begin
  case d.datype(wflnm(idatyp)) of

  'moments' : begin  
                tmn = mdat(d.ndx(0,0)).ta < tmn
                tmx = mdat(d.ndx(1,0)).ta > tmx
                flnm=d.flnm(idatyp)
              endcase
  'fspnavg' : begin  
                tmn = pdat(d.ndx(0,1)).ta < tmn
                tmx = pdat(d.ndx(1,1)).ta > tmx
                if flnm eq '' then flnm=d.flnm(idatyp)
              endcase
  'fpitch' : begin  
                tmn = pdat(d.ndx(0,2)).ta < tmn
                tmx = pdat(d.ndx(1,2)).ta > tmx
                if flnm eq '' then flnm=d.flnm(idatyp)
              endcase
  'strahl' : begin  
                tmn = strldat(d.ndx(0,3)).ta < tmn
                tmx = strldat(d.ndx(1,3)).ta > tmx
                if flnm eq '' then flnm=d.flnm(idatyp)
              endcase
  'ionkp' : begin  
                tmn = ionkpdat(d.ndx(0,4)).ta < tmn
                tmx = ionkpdat(d.ndx(1,4)).ta > tmx
                if flnm eq '' then flnm=d.flnm(idatyp)
              endcase
  endcase
endfor

range_mins=(tmx-tmn)/60
indxr=where(range_mins le range_upper and range_mins gt range_lower,n_ind)
print,'mstruct: newticks,indxr(0),range_mins ',newticks,indxr(0),range_mins
if newticks eq 0 and indxr(0) ne -1 then begin
    newticks=tickmarks(indxr(0)) 
    new_time_scale,newticks,$
     (tmn-refsec)/3600.d,(tmx-refsec)/3600.d,xrange,xtickv,xtickname,xticks
    print,newticks,xticks,xrange,xtickname
endif else begin
    widget_control,wa.slider(0),get_value=val
    tmn=double(val(0))
    widget_control,wa.slider(1),get_value=val
    tmx=double(val(0))
    print,'tmn, tmx ',tmn,tmx
    xticks=fix(1.001*(60.d*(tmx-tmn)/newticks))
    xtickv=tmn+indgen(xticks+1) * double(newticks)/60.d
    xrange=[xtickv(0),xtickv(n_elements(xtickv)-1)]
    hour_hms,xtickv,hms,hm,hh 
    if strlen(hm(0)) eq 0 and strlen(hh(0)) eq 0 then xtickname=hms 
    if strlen(hm(0)) ne 0 then xtickname=hm 
    if strlen(hh(0)) ne 0 then xtickname=hh
    print,newticks,xticks,xrange,xtickname
endelse


;set time scale for all panels
 d.pnl(pnlsel).tmlabl=$
       string(format='("Reference UT: tjd ",i4)',refsec/86400.d)+'   '+$
       yrmoda(refpb5)+'  '+string(refpb5(1),format='(i3)') +' '+hms1(0)
 d.pnl(pnlsel).subtitle=''
 d.pnl(pnlsel).tmrange=xrange
 d.pnl(pnlsel).tmtickv=xtickv
 d.pnl(pnlsel).tmtickname=xtickname
 d.pnl(pnlsel).tmticks=xticks

 d.pnl(pnlsel).subtitle=strmid(flnm,strlen(getenv('SURVEYPATH')),$
  strlen(flnm)-strlen(getenv('SURVEYPATH')))
 
;for i=0,n_elements(pnlsel(wmoments))-1 do begin
for i=0,n_elements(pnlsel)-1 do begin
  ;k=pnlsel(wmoments(i))
k=pnlsel(i)
case d.pnlist.dtp(k) of

'moments' : begin
     d.pnl(pnlsel).subtitle=strmid(flnm,strlen(getenv('SURVEYPATH')),$
     strlen(flnm)-strlen(getenv('SURVEYPATH')))+$
     '  '+string(mdat(d.ndx(0,0)).gains,format='(6f6.2)')

   ;case pnl(k).varname of
   case d.pnlist.list(k) of
    
         ;density
         'N density': begin 
            d.pnl(k).labl='Ne'  ;(el/cm^3)
            d.pnl(k).range=[0.1,10.];[1.,100.];[1.,10.]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            endcase
     
         ;bulk speed
         'U flow speed': begin
            d.pnl(k).labl='Ue'  ;(km/s)
            d.pnl(k).range=[0.,900.];[0,1000]
            d.pnl(k).ticks=1
            d.pnl(k).minor=3
            endcase

         ;elevation of bulk speed
         'th_u': begin
            d.pnl(k).labl='th_ue'  ;(deg)
            d.pnl(k).range=[-20.,20.];[-90.,90.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

  
         ;azimuth of bulk speed
         'ph_u': begin
            d.pnl(k).labl='ph_ue' ;(deg)
            d.pnl(k).range=[0,360]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

         ;bulk speed gse components
         'Ux (gse)': begin
            d.pnl(k).labl='Uex'
            d.pnl(k).range=[-800,800]
            endcase

         'Uy (gse)': begin
            d.pnl(k).labl='Uey'
            d.pnl(k).range=[-800,800];[-100,100]
            endcase

         'Uz (gse)': begin
            d.pnl(k).labl='Uez'
            d.pnl(k).range=[-100,100]
            endcase

  
         ;temperature
         'T temperature': begin
            d.pnl(k).labl='Te'  ;(deg K)
            d.pnl(k).range=[0.5e5,10.e5];[1.e5,1.e6];[2.e5,2.e6]
            d.pnl(k).plotio=1
            endcase

         ;anisotropy
         'A anisotropy': begin 
            ;d.pnl(k).labl='1-Tperp/Tpara'
            ;d.pnl(k).range=[-1.,1.]
            d.pnl(k).labl='Tpara/Tperp'
            d.pnl(k).range=[0.,2.]
            d.pnl(k).ticks=2
            ;d.pnl(k).oplot=1
            ;d.pnl(k).oplotvar='gyrtrpy'    ;overplot gyrotropy
            ;d.pnl(k).olinestyle=2
            endcase


         ;heat flux
         'Q heat flux': begin
            d.pnl(k).labl='Q'  ;(ergs cm^-2 s^-1) 
            d.pnl(k).range=[0.001,0.1];[0.02,0.2]
            d.pnl(k).plotio=1
            d.pnl(k).ticks=2
            endcase

         ;elevation of heat flux
         'th_q': begin
            d.pnl(k).labl='th_q'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

         ;azimuth of heat flux
         'ph_q': begin
            d.pnl(k).labl='phi_q'  ;(deg)
            d.pnl(k).range=[0,360]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase
 

         ;gse x-component of mag field
         'Bx': begin
            d.pnl(k).labl='Bx'  ; (gse)'
            d.pnl(k).range=[-50,50]
            ;d.pnl(k).fill=-999.
            endcase

         ;gse y-component of mag field
         'By': begin
            d.pnl(k).labl='By'  ; (gse)'
            d.pnl(k).range=[-50,50]
            ;d.pnl(k).fill=-999.
            endcase

          ;gse z-component of mag field
          'Bz': begin
            d.pnl(k).labl='Bz'  ; (gse)'
            d.pnl(k).range=[-50,50]
            ;d.pnl(k).fill=-999.
            endcase
 

          ;magnitude of magnetic field
          'B magnetic field': begin
            d.pnl(k).labl='B'  ;(nT)
            d.pnl(k).range=[4,10];[0,10];
            d.pnl(k).ticks=1
            d.pnl(k).minor=3
            ;d.pnl(k).fill=-999.
            d.pnl(k).psym=3
            endcase

         ;azimuth of magnetic field
         'th_b': begin
            d.pnl(k).labl='th b'  ; (gse)'  ;(deg)
            d.pnl(k).range=[-90,90]
            d.pnl(k).ticks=2
            d.pnl(k).minor=2
            ;d.pnl(k).fill=-999.
            d.pnl(k).psym=3
            endcase


         ;elevation of magnetic field
         'ph_b': begin
            d.pnl(k).labl='phi b'  ; (gse)'  ;(deg)
            d.pnl(k).range=[0.,50.];[0,90];[0,360]
            ;d.pnl(k).fill=-999.
            d.pnl(k).psym=3
            d.pnl(k).ticks=2
            d.pnl(k).minor=2
            endcase
  
         ;gyrotropy
         'gyrtrpy': begin
            d.pnl(k).labl='gyrtrpy'
            d.pnl(k).range=[-1.,1.]
            d.pnl(k).ticks=2
            endcase

          ;average energy/thermal speed
          'W thermal speed': begin 
            d.pnl(k).labl='We (km/s)'
            d.pnl(k).range=[1000,6000]
            d.pnl(k).ticks=2
            endcase

         ;angle between principle axis of pressure tensor and magnetic field
         'cos(Pa,B)': begin
            d.pnl(k).labl='cos(Pa,B)'
            d.pnl(k).range=[-1.,1.]
            d.pnl(k).ticks=2
            d.pnl(k).psym=3
            endcase

         ;angle between heat flux vector and magnetic field
         'cos(Q,B)': begin
            d.pnl(k).labl='cos(Q,B)'
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

         'magnetic pressure': begin
            d.pnl(k).labl='P(b)' ;1e-10
            d.pnl(k).range=[0,5];[0.1,100.]
            ;d.pnl(k).plotio=1
            d.pnl(k).ticks=1
            d.pnl(k).minor=5
            ;d.pnl(k).psym=3
            endcase
 
         'elec plas pressure': begin
            d.pnl(k).labl='P(ele)' ;1e-10
            d.pnl(k).range=[0,5];[.001,.1];[0.1,100.]
            ;d.pnl(k).plotio=1
            d.pnl(k).ticks=1
            d.pnl(k).minor=5
            ;d.pnl(k).psym=3
            endcase
    endcase
endcase      ;end moments

'fspnavg' : begin
         d.pnl(k).ztitle='SWE electrons'
         d.pnl(k).labl='log eV'
         d.pnl(k).range=d.pnl(k).lzrange(*,cf)
         endcase

'fpitch' : begin
         d.pnl(k).labl=$
         string(volt_en(pdat(d.ndx(0,2)).vsteps(d.pnlist.ev_val(k)),/en),$
         format='(i4)')+' ev'
         d.pnl(k).range=d.pnl(k).lzrange(*,cf) 
         endcase

'strahl' : begin
         d.pnl(k).labl='strl '+$
         string(d.pnlist.list(k),format='(i4)')+' ev'
         d.pnl(k).range=d.pnl(k).lzrange(*,cf)
         endcase
 
'ionkp' : begin
    case d.pnlist.list(k) of
        'Ui flow speed':  begin
           d.pnl(k).labl='Ui'  ;(km/s)
           d.pnl(k).range=[0.,900.];[0,1000]
           d.pnl(k).ticks=1
           d.pnl(k).minor=3
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase

        'Uew':  begin
           d.pnl(k).labl='VEW'  ;(km/s)
           d.pnl(k).range=[-30.,30.]
           d.pnl(k).ticks=2
           d.pnl(k).psym=3
           endcase
    
        'Uns':  begin
           d.pnl(k).labl='VNS'  ;(km/s)
           d.pnl(k).range=[-30.,30.]
           d.pnl(k).ticks=2
           d.pnl(k).psym=3
           endcase
    
        'W thermal speed': begin
           d.pnl(k).labl='Wp (km/s)'
           d.pnl(k).range=[0,200]
           d.pnl(k).ticks=2
           d.pnl(k).psym=3
           endcase

        'Ti temperature' : begin
           d.pnl(k).labl='Ti'  ;(deg K)
           d.pnl(k).range=[0.5e5,10.e5];[1.e5,1.e6]  ;[5.e4,5.e5]  ;94 dec01
           ;d.pnl(k).plotio=1
           d.pnl(k).plotio=1
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase

        'Ni density': begin
           d.pnl(k).labl='Ni'  ;(/cm^3)
           d.pnl(k).range=[0.1,10.];[1.,10.]
           d.pnl(k).plotio=1
           d.pnl(k).ticks=2
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase

        'plasma pressure (i+e)': begin 
           d.pnl(k).labl='P(i+e)'  ;1.e-10  
           d.pnl(k).range=[0,5];[0.1,100.]
           ;d.pnl(k).plotio=1
           d.pnl(k).ticks=1
           d.pnl(k).minor=5
           d.pnl(k).psym=4
           d.pnl(k).symsize=0.2
           endcase

         'total pressure (i+e+b)': begin 
         d.pnl(k).labl='P(tot)'  ;1.e-10  
         d.pnl(k).range=[0,5];[0.1,10.];[0.1,100.]
         ;d.pnl(k).plotio=1
         d.pnl(k).ticks=1
         d.pnl(k).minor=5
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
        endcase
endcase  ;end ionkp
endcase  ;end i'th selection 
endfor

end

