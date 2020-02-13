
;========================= input =============================================

pro readinput,relgains,relgainsnml,pb5tim,ebias1,ebias2,mncts,bangle,$
  enev,refdate,pb5ref,elapsec
  
refdate=19941130l  

pb5ref=ymd_pb5(refdate)

relgains=fltarr(6,1000)
relgainsnml=fltarr(6,1000)
pb5tim=lonarr(3,1000)
ebias1=intarr(1000)
ebias2=intarr(1000)
sectjd=dblarr(1000)
mncts=fltarr(1000)
bangle=fltarr(1000)
enev=fltarr(1000)


dir=getenv('WGGSBASE')+'swe/cal/gains/'
;flnm=dir+'savegains_all.dat'
;flnm=dir+'savegains_960805.dat'
;flnm=dir+'savegains_971208.dat'
flnm=dir+'savegains_980602.dat'

openr,lun,flnm,/get_lun

i=-1
while not eof(lun) do begin
  s=''
  readf,lun,s 
  
  sdate=''  
  readf,lun,sdate
  readf,lun,s
  en=fix(strmid(s,0,4))

  pb5=lonarr(3)
  nacc=0
  ebias=intarr(2)
  b=fltarr(3)
  s='mean(0),sdev,rtchi2 '
  mean=0.
  sdev=0.
  rtchi2=0.
  readf,lun,pb5,nacc,ebias,b
  readf,lun,s,mean,sdev,rtchi2,format='(a20,3f13.3)'
  rgn=fltarr(6)
  rgnml=fltarr(6)
  s=''
  readf,lun,s,rgn,format='(a11,6f7.3)'
  readf,lun,s
  readf,lun,s,rgnml,format='(a11,6f7.3)'
  if strmid(sdate,0,1) eq 'b' then goto,endwhl


  i=i+1
  pb5tim(*,i)=pb5
  ebias1(i)=ebias(0)
  ebias2(i)=ebias(1)  
  relgains(*,i)=rgn
  ;relgainsnml(*,i)=relgains(*,i)/min(relgains(*,i))
  relgainsnml(*,i)=relgains(*,i)/relgains(0,i)        ;normalize to det # 0
  mncts(i)=mean
  bangle(i)=acos(b(0)/sqrt(total(b*b)))/!dtor
  if bangle(i) gt 90. then bangle(i)=180.-bangle(i)
  enev(i)=en
  
  print,i
  print,sdate
  endwhl:
endwhile

free_lun,lun

print,'hit return to continue' & answ='' & read,answ 

n=i+1
relgains=relgains(*,0:n-1)
relgainsnml=relgainsnml(*,0:n-1)
pb5tim=pb5tim(*,0:n-1)
mncts=mncts(0:n-1)
bangle=bangle(0:n-1)
ebias1=ebias1(0:n-1)
ebias2=ebias2(0:n-1)
enev=enev(0:n-1)

sz=size(pb5tim)
nd=sz(2)
elapsec=dblarr(nd)
pb5chk=lonarr(3,nd)
for i=0,nd-1 do begin
  elapsec(i)=pb5_elapsec(reform(pb5tim(*,i)),pb5ref)
  pb5chk(*,i)=elapsec_pb5(elapsec(i),pb5ref)
endfor

for i=0,n-1 do $
  print,i,pb5tim(0,i),pb5tim(1,i),pb5tim(2,i),$
  pb5_ymd(reform(pb5tim(*,i))),elapsec(i),pb5chk(0,i),pb5chk(1,i),pb5chk(2,i)
 
end



;====================== timeorder ========================================

pro timeorder,relgains,relgainsnml,pb5tim,ebias1,ebias2,mncts,bangle,$
  enev,pb5ref,elapsec

;put input arrays in time order

tsort=sort(elapsec)
pb5tim(*,*)=pb5tim(*,tsort)
ebias1=ebias1(tsort)
ebias2=ebias2(tsort)
relgains(*,*)=relgains(*,tsort)
relgainsnml(*,*)=relgainsnml(*,tsort)
elapsec=elapsec(tsort)
mncts=mncts(tsort)
bangle=bangle(tsort)
enev=enev(tsort)
end

;====================== crvfit function =====================================

pro funct2,x,c,f,pder
f=c(0)+c(1)*x
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
return
end


pro funct3,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end




;============================ plotcrvfit =====================================

pro plotcrvfit,x,y,mncts,det,eqwts,cf,yfit,color,checkfit=checkfit,linfit=linfit

if keyword_set(checkfit) eq 0 then checkfit=0
if keyword_set(linfit) eq 0 then linfit=0

cf=fltarr(3)

if linfit then begin 
    c=fltarr(2)
    if eqwts then wt= 1.+fltarr(n_elements(y)) $
    else wt=1./(mncts/min(mncts))
    c(0)=total(y)/n_elements(y)
    ycrvfit=curvefit(x,y,wt,c,sigc,function_name='funct2',chi2=chi2)
    cf(0:1)=c
    oplot,x,ycrvfit,color=color
    yfit=c(0)+c(1)*x
    if checkfit then begin
      oplot,x,yfit,color=237,psym=4,symsize=0.5
      print,' det ',det
      for i=0,n_elements(x)-1 do print,x(i),yfit(i)
    endif

endif else begin
 
    c=fltarr(3)
    if eqwts then wt= 1.+fltarr(n_elements(y)) $
    else wt=1./(mncts/min(mncts))
    c(0)=total(y)/n_elements(y)
    ycrvfit=curvefit(x,y,wt,c,sigc,function_name='funct3',chi2=chi2)
    cf(0:2)=c
    oplot,x,ycrvfit,color=color
    yfit=c(0)+c(1)*x+c(2)*x^2
    if checkfit then begin
      oplot,x,yfit,color=237,psym=4,symsize=0.5
      print,' det ',det
      for i=0,n_elements(x)-1 do print,x(i),yfit(i)
    endif

endelse
end



;======================= MAIN ===============================================

usegetrelgains=1
createsavefile=0
hardcpy=0


& print,' '
print,'hardcopy ',hardcpy & print,' '
print,'if usegetrelgains eq 1 then current getrelgains procedure will be used'
print,'usegetrelgains ',usegetrelgains & print,' '
print,'if createsavefile eq 1 then relgain fit coefficients file will be saved'
print,'createsavefile ',createsavefile & print,' '
answ='' & print,'hit return to continue' & read,answ & if answ ne '' then stop

start:

loadct,18

;read input
readinput,relgains,relgainsnml,pb5tim,ebias1,ebias2,mncts,bangle,$
enev,refdate,pb5ref,elapsec

;put in time order
timeorder,relgains,relgainsnml,pb5tim,ebias1,ebias2,mncts,bangle,$
  enev,pb5ref,elapsec

;set plot parameters
symsize=0.4
if hardcpy then charsize=1.0 else charsize=1.5

;extend plot to end of 980530  ;971130
  tpb5_97=ymd_pb5(long(19980530))  ;19971130))
  elapsec_end=pb5_elapsec(tpb5_97,pb5ref)

xrange=[0,elapsec_end]
xticks=6
xminor=6
xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
pb5tickv=lonarr(3,n_elements(xtickv))
for i=0,n_elements(xtickv)-1 do $
  pb5tickv(*,i)=elapsec_pb5(xtickv(i),pb5ref)
xtickname=strmid(strtrim(string(pb5tickv(0,*)),2),2,2)+'.'+$
  strtrim(string(pb5tickv(1,*),format='(i3)'),1)
xticklen=0.09  ;0.075
noerase=1+intarr(6) & noerase(0)=0
xtitle=strarr(6) & xtitle(5)='date'
xcharsize=0.001+fltarr(6) & xcharsize(5)=1.0
;end set plot parameters



;-------------- order in groups acording to bias levels ----------------------
;group 1
   pb5_change1=long([1995,130,45104614])   ;=19950510
   elapsec_change1=pb5_elapsec(pb5_change1,pb5ref)
   set1=  ebias1 lt 56 and ebias2 eq 52 and $
          elapsec le elapsec_change1
   w1=where(set1,nw1)
   clr1=50
   linfit1=[0,0,0,0,0,0]
   
;group 2 
   pb5_change2=long([1995,325,54600000])   ;=19951121
   elapsec_change2=pb5_elapsec(pb5_change2,pb5ref)  
   set2=  ebias1 lt 56 and ebias2 eq 55 or $
          elapsec gt elapsec_change1 and elapsec le elapsec_change2
   w2=where(set2,nw2)
   clr2=90
   linfit2=[0,0,0,0,0,0]
   
;group 3
   pb5_change3=long([1996,134,75220409])   ;=19960513
   elapsec_change3=pb5_elapsec(pb5_change3,pb5ref)       
   set3=  ebias1 eq 59 and ebias2 ge 59 and $
          elapsec gt elapsec_change2 and elapsec le elapsec_change3
   w3=where(set3,nw3)
   clr3=115
   linfit3=[1,1,1,1,1,1]
   
;groups 4_1 and 4_2
   yrmd4_1=long(19960814)  ;=1996         227           0
   yrmd4_2=long(19970214)  ;=1997          45           0
   pb5_change4_1=ymd_pb5(yrmd4_1)  
   pb5_change4_2=ymd_pb5(yrmd4_2)  
   elapsec_change4_1=pb5_elapsec(pb5_change4_1,pb5ref)
   elapsec_change4_2=pb5_elapsec(pb5_change4_2,pb5ref)
   
   set4_1=  ebias1 eq 61 and ebias2 ge 59 and $
            elapsec gt elapsec_change3 and elapsec lt elapsec_change4_1
   w4_1=where(set4_1,nw4_1)
   linfit4_1=[1,1,1,0,1,1]
   
   set4_2=  ebias1 eq 61 and ebias2 ge 59 and $
            elapsec ge elapsec_change4_1 and elapsec lt elapsec_change4_2
   w4_2=where(set4_2,nw4_2) 
   linfit4_2=[0,0,0,0,0,0]

;groups 4_3 and 4_4 
   yrmd4_3=long(19970613)  ;=1997         164           0 
   pb5_change4_3=ymd_pb5(yrmd4_3) 
   elapsec_change4_3=pb5_elapsec(pb5_change4_3,pb5ref)
   set4_3=  ebias1 eq 61 and ebias2 ge 59 and $
            elapsec ge elapsec_change4_2 and elapsec lt elapsec_change4_3
   w4_3=where(set4_3,nw4_3) 
   linfit4_3=[1,1,1,0,1,1]    ;[0,0,0,0,0,0]   
     

;group 4_4
   yrmd4_4=long(19971028)  ;1997, 301, 0 
   pb5_change4_4=ymd_pb5(yrmd4_4)
   elapsec_change4_4=pb5_elapsec(pb5_change4_4,pb5ref)
   set4_4=  ebias1 eq 61 and ebias2 ge 59 and $
            elapsec ge elapsec_change4_3 and elapsec lt elapsec_change4_4
   w4_4=where(set4_4,nw4_4)
   linfit4_4=[1,1,1,1,1,1]   ;[1,1,1,0,1,1]
    
;group 4_5     when det2 set eq det3
   yrmd4_5=long(19980515) ; 1998, 135, 0; bias change on previous day
   pb5_change4_5=ymd_pb5(yrmd4_5)
   elapsec_change4_5=pb5_elapsec(pb5_change4_5,pb5ref)      
     set4_5=ebias1 eq 61 and ebias2 ge 59 and $
            elapsec ge elapsec_change4_4 and elapsec lt elapsec_change4_5
     w4_5=where(set4_5,nw4_5)
     linfit4_5=[1,1,1,1,1,1]
       
   clr4=145
   
   nw4=nw4_1+nw4_2+nw4_3+nw4_4+nw4_5
   w4=lonarr(nw4)
   w4(*)=[w4_1,w4_2,w4_3,w4_4,w4_5]
   
   print,nw1+nw2+nw3+nw4,n_elements(ebias1)
   if nw1+nw2+nw3+nw4 ne n_elements(ebias1) then begin
     for i=0,n_elements(ebias1)-1 do $
       print,i,pb5tim(0,i),pb5tim(1,i),pb5tim(2,i),ebias1(i),ebias2(i)
     stop
   endif  
           
           
;----------------------- plot ebias and energy levels ------------------------

posn=fltarr(4,2)
pos,2,posn


if hardcpy then begin
  set_plot,'ps'
  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5
endif
if hardcpy eq 0 then window,2

yrange=[50,62] 
yticks=6
yminor=2
plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    ytitle='ebias level',xcharsize=0.001,$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=' ',position=posn(*,0),noerase=0,charsize=charsize,$
    xminor=xminor,xticklen=xticklen

oplot,elapsec(w1),ebias1(w1),psym=4,symsize=symsize,color=clr1
xyouts,elapsec(n_elements(elapsec(w1))/2),ebias1(n_elements(elapsec(w1))/2),$
/data,'1'
oplot,elapsec(w1),ebias2(w1),psym=4
xyouts,elapsec(n_elements(elapsec(w1))/2),ebias2(n_elements(elapsec(w1))/2),$
/data,'2'
 
oplot,elapsec(w2),ebias1(w2),psym=4,symsize=symsize,color=clr2
oplot,elapsec(w2),ebias2(w2),psym=4
 
oplot,elapsec(w3),ebias1(w3),psym=4,symsize=symsize,color=clr3
oplot,elapsec(w3),ebias2(w3),psym=4
 
oplot,elapsec(w4),ebias1(w4),psym=4,symsize=symsize,color=clr4
oplot,elapsec(w4),ebias2(w4),psym=4

  
yrange=[0,150] 
yticks=5
yminor=1
plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    ytitle='energy step ev',xcharsize=1.0,$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle='date',position=posn(*,1),noerase=1,charsize=charsize,$
    xminor=xminor,xticklen=xticklen

oplot,elapsec,enev,psym=4,symsize=symsize


if hardcpy then begin
  device,/close
  spawn,'lpr idl.ps'
  print,'printing hardcopy'
  set_plot,'x'
endif


;---------------------------- plot measured relative gains -------------------



posn=fltarr(4,6)
pos,6,posn

z=relgains
yrange=[0,3.0] 
yticks=3
yminor=2


if hardcpy then begin
  set_plot,'ps'
  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5
endif
if hardcpy eq 0 then window,0

for i=0,5 do begin     ;detector loop for relgains plot

eqwts=0
checkfit=0


if i eq 0 then title='1 / relgains' else title=''

plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    title=title,ytitle='det '+string(i,format='(i1)'),xcharsize=xcharsize(i),$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=xtitle(i),position=posn(*,i),noerase=noerase(i),charsize=charsize,$
    xminor=xminor,xticklen=xticklen

    
oplot,elapsec(w1),z(i,w1),psym=4,symsize=symsize,color=clr1
plotcrvfit,elapsec(w1),reform(z(i,w1)),mncts(w1),i,eqwts,c,yfit,clr1,$
  linfit=linfit1(i)

oplot,elapsec(w2),z(i,w2),psym=4,symsize=symsize,color=clr2
plotcrvfit,elapsec(w2),reform(z(i,w2)),mncts(w2),i,eqwts,c,yfit,clr2,$
  linfit=linfit2(i)

oplot,elapsec(w3),z(i,w3),psym=4,symsize=symsize,color=clr3
plotcrvfit,elapsec(w3),reform(z(i,w3)),mncts(w3),i,eqwts,c,yfit,clr3,$
  linfit=linfit3(i)

oplot,elapsec(w4),z(i,w4),psym=4,symsize=symsize,color=clr4
plotcrvfit,elapsec(w4_1),reform(z(i,w4_1)),mncts(w4_1),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_1(i)
  
plotcrvfit,elapsec(w4_2),reform(z(i,w4_2)),mncts(w4_2),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_2(i)
  
plotcrvfit,elapsec(w4_3),reform(z(i,w4_3)),mncts(w4_3),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_3(i)

plotcrvfit,elapsec(w4_4),reform(z(i,w4_4)),mncts(w4_4),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_4(i)

plotcrvfit,elapsec(w4_5),reform(z(i,w4_5)),mncts(w4_5),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_5(i)
endfor   ;end detector loop for relgains plot 

if hardcpy then begin
  device,/close
  spawn,'lpr idl.ps'
  print,'printing hardcopy'
  set_plot,'x'
endif


         
;---------------------------- plot normalized relative gains -------------------

posn=fltarr(4,6)
pos,6,posn

z=relgainsnml
yrange=[0,10]  ;[0,6.0] 
yticks=3
yminor=2

nsegments=8
coeff=fltarr(3,6,nsegments)

if hardcpy then begin
  set_plot,'ps'
  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5
endif
if hardcpy eq 0 then window,1,xsize=900,ysize=900

;get relgains from getrelgains procedure
if usegetrelgains then begin
   rgw1=fltarr(6,n_elements(elapsec(w1)))
   for i=0,n_elements(elapsec(w1))-1 do begin
     pb5tm=elapsec_pb5(elapsec(w1(i)),pb5ref) 
     sectm=pb5_sec(pb5tm)
     getrelgains,rg,sectm,relgainchange
     rgw1(*,i)=rg
   endfor

   rgw2=fltarr(6,n_elements(elapsec(w2)))
   for i=0,n_elements(elapsec(w2))-1 do begin
     pb5tm=elapsec_pb5(elapsec(w2(i)),pb5ref) 
     sectm=pb5_sec(pb5tm)
     getrelgains,rg,sectm,relgainchange
     rgw2(*,i)=rg
   endfor

   rgw3=fltarr(6,n_elements(elapsec(w3)))    
   for i=0,n_elements(elapsec(w3))-1 do begin
     pb5tm=elapsec_pb5(elapsec(w3(i)),pb5ref) 
     sectm=pb5_sec(pb5tm)
     getrelgains,rg,sectm,relgainchange
     rgw3(*,i)=rg
   endfor

   rgw4_1=fltarr(6,n_elements(elapsec(w4_1)))    
   for i=0,n_elements(elapsec(w4_1))-1 do begin
     pb5tm=elapsec_pb5(elapsec(w4_1(i)),pb5ref) 
     sectm=pb5_sec(pb5tm)
     getrelgains,rg,sectm,relgainchange
     rgw4_1(*,i)=rg
   endfor

   rgw4_2=fltarr(6,n_elements(elapsec(w4_2)))    
   for i=0,n_elements(elapsec(w4_2))-1 do begin
     pb5tm=elapsec_pb5(elapsec(w4_2(i)),pb5ref) 
     sectm=pb5_sec(pb5tm)
     getrelgains,rg,sectm,relgainchange
     rgw4_2(*,i)=rg
   endfor

   rgw4_3=fltarr(6,n_elements(elapsec(w4_3)))    
   for i=0,n_elements(elapsec(w4_3))-1 do begin
     pb5tm=elapsec_pb5(elapsec(w4_3(i)),pb5ref) 
     sectm=pb5_sec(pb5tm)
     getrelgains,rg,sectm,relgainchange 
     rgw4_3(*,i)=rg
   endfor
 
   rgw4_4=fltarr(6,n_elements(elapsec(w4_4)))    
   for i=0,n_elements(elapsec(w4_4))-1 do begin
     pb5tm=elapsec_pb5(elapsec(w4_4(i)),pb5ref) 
     sectm=pb5_sec(pb5tm)
     getrelgains,rg,sectm,relgainchange 
     rgw4_4(*,i)=rg
   endfor 
 
   rgw4_5=fltarr(6,n_elements(elapsec(w4_5)))    
   for i=0,n_elements(elapsec(w4_5))-1 do begin
     pb5tm=elapsec_pb5(elapsec(w4_5(i)),pb5ref) 
     sectm=pb5_sec(pb5tm)
     getrelgains,rg,sectm,relgainchange 
     rgw4_5(*,i)=rg
   endfor 
     
  ;rg_after=fltarr(6,2)
  ;elapsec_after= [elapsec(w4_4(n_elements(w4_4)-1)),xrange(1)]
  ;for i=0,1 do begin
  ;  pb5tm=elapsec_pb5(elapsec_after(i),pb5ref)
  ;  sectm=pb5_sec(pb5tm)
  ;  getrelgains,rg,sectm,relgainchange
  ;  rg_after(*,i)=rg
  ;endfor
     
endif

for i=0,5 do begin     ;detector loop for nrmlzd relgains plot

eqwts=0
checkfit=0
if i eq 0 then title='1 / relgains (nrmlzd)' else title=''

plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    title=title,ytitle='det '+string(i,format='(i1)'),xcharsize=xcharsize(i),$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=xtitle(i),position=posn(*,i),noerase=noerase(i),charsize=charsize,$
    xminor=xminor,xticklen=xticklen

    
oplot,elapsec(w1),z(i,w1),psym=4,symsize=symsize,color=clr1
if usegetrelgains then oplot,elapsec(w1),rgw1(i,*),color=200 else $
  plotcrvfit,elapsec(w1),reform(z(i,w1)),mncts(w1),i,eqwts,c,yfit,clr1,$
  linfit=linfit1(i)
if createsavefile then coeff(*,i,0)=c
  
oplot,elapsec(w2),z(i,w2),psym=4,symsize=symsize,color=clr2
if usegetrelgains then oplot,elapsec(w2),rgw2(i,*),color=200 else $
  plotcrvfit,elapsec(w2),reform(z(i,w2)),mncts(w2),i,eqwts,c,yfit,clr2,$
  linfit=linfit2(i)
if createsavefile then coeff(*,i,1)=c
  
oplot,elapsec(w3),z(i,w3),psym=4,symsize=symsize,color=clr3
if usegetrelgains then oplot,elapsec(w3),rgw3(i,*),color=200 else $
  plotcrvfit,elapsec(w3),reform(z(i,w3)),mncts(w3),i,eqwts,c,yfit,clr3,$
  linfit=linfit3(i)
if createsavefile then coeff(*,i,2)=c

oplot,elapsec(w4),z(i,w4),psym=4,symsize=symsize,color=clr4
if usegetrelgains then oplot,elapsec(w4_1),rgw4_1(i,*),color=200 else $
  plotcrvfit,elapsec(w4_1),reform(z(i,w4_1)),mncts(w4_1),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_1(i)
if createsavefile then coeff(*,i,3)=c

if usegetrelgains then oplot,elapsec(w4_2),rgw4_2(i,*),color=200 else $
  plotcrvfit,elapsec(w4_2),reform(z(i,w4_2)),mncts(w4_2),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_2(i)
if createsavefile then coeff(*,i,4)=c

if usegetrelgains then oplot,elapsec(w4_3),rgw4_3(i,*),color=200 else $
  plotcrvfit,elapsec(w4_3),reform(z(i,w4_3)),mncts(w4_3),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_3(i)
if createsavefile then coeff(*,i,5)=c

if usegetrelgains then oplot,elapsec(w4_4),rgw4_4(i,*),color=200 else $
  plotcrvfit,elapsec(w4_4),reform(z(i,w4_4)),mncts(w4_4),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_4(i)
if createsavefile then coeff(*,i,6)=c

if usegetrelgains then oplot,elapsec(w4_5),rgw4_5(i,*),color=200 else $
  plotcrvfit,elapsec(w4_5),reform(z(i,w4_5)),mncts(w4_5),i,eqwts,c,yfit,clr4,$
  linfit=linfit4_5(i)
if createsavefile then coeff(*,i,7)=c

;if usegetrelgains then oplot,elapsec_after(*),rg_after(i,*),color=200


endfor   ;end detector loop for relgains plot 

if createsavefile then begin
  print,'saving relgain_coefficients'
  ;save,filename=getenv('IDLSAV')+'relgain_coefficients_97aug28',$
  ;save,filename=getenv('IDLSAV')+'relgain_coefficients_97dec08',$
  ;save,filename=getenv('IDLSAV')+'relgain_coefficients_98jun02',$
  save,filename=getenv('IDLSAV')+'relgain_coefficients_98jun02_mod',$
    refdate,pb5ref,pb5_change1,pb5_change2,pb5_change3,$
    pb5_change4_1,pb5_change4_2,pb5_change4_3,pb5_change4_4,pb5_change4_5,$
    coeff 
endif

if hardcpy then begin
  device,/close
  spawn,'lpr idl.ps'
  print,'printing hardcopy'
  set_plot,'x'
  hardcpy=0 & goto,start
endif


   
end  


