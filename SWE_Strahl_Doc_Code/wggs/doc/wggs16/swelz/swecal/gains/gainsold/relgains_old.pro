
pro funct3,x,c,f,pder
f=c(0)+c(1)*x+c(2)*x^2
if n_params(0) le 3 then return
pder=fltarr(n_elements(x),n_elements(c))
pder(*,0)=1.
pder(*,1)=x
pder(*,2)=x^2
return
end


;========================= input =============================================

pro readinput,relgains,relgainsnml,pb5tim,ebias1,ebias2,sectjd,mncts,bangle,$
  enev,refseconds
dir=getenv('WGGSBASE')+'swe/gains/'
;flnm=dir+'savegains_tmp.dat'
flnm=dir+'savegains_960805.dat'
;flnm='savegains.dat'
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
  

  i=i+1
  pb5tim(*,i)=pb5
  ebias1(i)=ebias(0)
  ebias2(i)=ebias(1)  
  relgains(*,i)=rgn
  ;relgainsnml(*,i)=relgains(*,i)/min(relgains(*,i))
  relgainsnml(*,i)=relgains(*,i)/relgains(0,i)        ;normalize to det # 0
  sectjd(i)=pb5_sec(pb5tim(*,i))
  if sectjd(i) lt refseconds then begin
   print,'reset tjd ',pb5,'   ',yrmoda(pb5)
   sectjd(i)=10000*double(86400.) + sectjd(i)
  endif
  mncts(i)=mean
  bangle(i)=acos(b(0)/sqrt(total(b*b)))/!dtor
  if bangle(i) gt 90. then bangle(i)=180.-bangle(i)
  enev(i)=en
endwhile

free_lun,lun


n=i+1
relgains=relgains(*,0:n-1)
relgainsnml=relgainsnml(*,0:n-1)
pb5tim=pb5tim(*,0:n-1)
sectjd=sectjd(0:n-1)
mncts=mncts(0:n-1)
bangle=bangle(0:n-1)
ebias1=ebias1(0:n-1)
ebias2=ebias2(0:n-1)
enev=enev(0:n-1)

end


;==================== select ===============================================

pro select,relgains,relgainsnml,pb5tim,ebias1,ebias2,sectjd,mncts,bangle,enev

;select by "mean counts" and "bangle"
ctsfloor= 0  ;60  ;80 ;38  ;60  ;70  ;200
maxbang=90.   ;45.   ;90.  ;10.   ;30.   ;60.    ;35.  
wok=where(mncts ge ctsfloor and bangle le maxbang,n)
relgains=relgains(*,wok)
relgainsnml=relgainsnml(*,wok)
pb5tim=pb5tim(*,wok)
sectjd=sectjd(wok)
mncts=mncts(wok)
bangle=bangle(wok)
enev=enev(wok)
ebias1=ebias1(wok)
ebias2=ebias2(wok)
end


;====================== timeorder ========================================

pro timeorder,relgains,relgainsnml,pb5tim,ebias1,ebias2,sectjd,mncts,bangle,$
  enev,dtjd,dtjdref

;put input arrays in time order

tsort=sort(sectjd)
pb5tim(*,*)=pb5tim(*,tsort)
ebias1=ebias1(tsort)
ebias2=ebias2(tsort)
relgains(*,*)=relgains(*,tsort)
relgainsnml(*,*)=relgainsnml(*,tsort)
sectjd=sectjd(tsort)
dtjd=double(sectjd)/86400.d - dtjdref
mncts=mncts(tsort)
bangle=bangle(tsort)
enev=enev(tsort)
end


;============================ plotcrvfit =====================================

pro  plotcrvfit,dtjd,z,mncts,set,setname,det,eqwts,c,x,yfit,color,checkfit,$
  normalizegains=normalizegains,nmldet=nmldet

    w=where(set)
    if w(0) eq -1 then return
    x=dtjd(w)
    if keyword_set(normalizegains) ne 0 then  y=z(det,w)/z(nmldet,w) else $
    y=z(det,w)
    if eqwts then wt= 1.+fltarr(n_elements(y)) $
    else wt=1./(mncts(w)/min(mncts(w)))
    c(0)=total(y)/n_elements(y)
    ycrvfit=curvefit(x,y,wt,c,sigc,function_name='funct3',chi2=chi2)
    oplot,x,ycrvfit,color=color
    yfit=c(0)+c(1)*x+c(2)*x^2
    if checkfit then begin
      oplot,x,yfit,color=237,psym=4,symsize=0.5
      print,setname,' det ',det
      for i=0,n_elements(x)-1 do print,x(i),yfit(i)
    endif

end

;=============================== plotladfit =================================

pro plotladfit,dtjd,z,set,setname,det,a,x,yfit,color,checkfit

  w=where(set)
  if w(0) eq -1 then return
  x=dtjd(w)
  y=z(det,w)
  a=ladfit(x,y)
  yladfit=a(0)+a(1)*x   
  oplot,x,yladfit,color=color
  yfit=a(0)+a(1)*x
  if checkfit then begin
      oplot,x,yfit,color=237,psym=4,symsize=0.5
      print,setname,' det ',det
      for i=0,n_elements(x)-1 do print,x(i),yfit(i)
    endif
end


;=============================== plotlinfit =================================

pro plotlinfit,dtjd,z,set,setname,det,a,x,yfit,color,checkfit

  w=where(set)
  if w(0) eq -1 then return
  x=dtjd(w)
  y=z(det,w)
  a=linfit(x,y)
  ylinfit=a(0)+a(1)*x   
  oplot,x,ylinfit,color=color
  yfit=a(0)+a(1)*x
  if checkfit then begin
      oplot,x,yfit,color=237,psym=4,symsize=0.5
      print,setname,' det ',det
      for i=0,n_elements(x)-1 do print,x(i),yfit(i)
    endif
end


;============================== initialfit ===================================

pro initialfit,dtjd,dtjdref,linearfit1,linearfit2,linearfit3,linearfit4,$
  nmlfitcoeff1,nmlfitcoeff2,nmlfitcoeff3,nmlfitcoeff4,$
  xset1,xset2,xset3,xset4,hardcopy

eqwts=0
parafitall=0
if hardcopy then hardcpy=1 else hardcpy=0
leastdev=0
checkfit=0

start:

refseconds=double(9686)*86400.d   ;reference time is 1994-11-30 tjd=9686
dtjdref=double(9686)
pb5ref=sec_pb5(refseconds)

relgains=fltarr(6,100)
relgainsnml=fltarr(6,100)
pb5tim=lonarr(3,100)
ebias1=intarr(100)
ebias2=intarr(100)
sectjd=dblarr(100)
mncts=fltarr(100)
bangle=fltarr(100)
enev=fltarr(100)

;read input data
readinput,relgains,relgainsnml,pb5tim,ebias1,ebias2,sectjd,mncts,bangle,$
  enev,refseconds

;finetune input
  wfntn=where(pb5tim(1,*) eq 334 and pb5tim(0,*) eq 1994)  ;day 334 is 11-30
  relgainsnml(5,wfntn)=2.0

;filter or select data subset
select,relgains,relgainsnml,pb5tim,ebias1,ebias2,sectjd,mncts,bangle,enev

;put input arrays in time order
timeorder,relgains,relgainsnml,pb5tim,ebias1,ebias2,sectjd,mncts,bangle,$
  enev,dtjd,dtjdref

sz=size(relgains)
relgainsfit=1.+fltarr(sz(1),sz(2))

print,'input '
for i=0,n_elements(dtjd)-1 do print,i,yrmoda(pb5tim(*,i)),$
  pb5_tjd(pb5tim(*,i)),dtjd(i),bangle(i),format='(i5,a15,2i10,2f12.5)'
print,' '


;define selected subsets of input data interval

;initialize
linearfit1=-1
set1=0
setn1=''
xset1=-1
w1=-1
linearfit2=-1
set2=0
setn2=''
xset2=-1
w2=-1
linearfit3=-1
set3=0
setn3=''
xset3=-1
w3=-1
linearfit4=-1
set4=0
setn4=''
xset4=-1
w4=-1

typesubset=0
case typesubset of
0: begin
     set1=  ebias1 lt 56 and ebias2 eq 52 
     setn1='ebias1 lt 56 and ebias2 eq 52'
     w1=where(set1)
     xset1=dtjd(w1)
     linearfit1=0
     print,' ' & print,setn1,' ',xset1

     set2=  ebias1 lt 56 and ebias2 eq 55
     setn2='ebias1 lt 56 and ebias2 eq 55'
     w2=where(set2)
     xset2=dtjd(w2)
     linearfit2=0
     print,' ' & print,setn2,' ',xset2

     set3=  ebias1 eq 59 and ebias2 ge 59
     setn3='ebias1 eq 59 and ebias2 ge 59'
     w3=where(set3)
     xset3=dtjd(w3)
     linearfit3=0
     print,' ' & print,setn3,' ',xset3

     set4=  ebias1 eq 61 and ebias2 ge 60
     setn4='ebias1 eq 61 and ebias2 ge 60'
     w4=where(set4)
     xset4=dtjd(w4)
     linearfit4=0
     print,' ' & print,setn4,' ',xset4

   endcase

1: begin
     set1=  (ebias1 lt 56 and ebias2 eq 52) or (ebias1 lt 56 and ebias2 eq 55)
     setn1='(ebias1 lt 56 and ebias2 eq 52) or (ebias1 lt 56 and ebias2 eq 55)'
     w1=where(set1)
     xset1=dtjd(w1)
     linearfit1=0
     print,' ' & print,setn1,' ',xset1
   endcase

2: begin
     set1=  dtjd lt 240.
     setn1='dtjd lt 240'
     w1=where(set1)
     xset1=dtjd(w1)
     linearfit1=0
     print,' ' & print,setn1,' ',xset1

     set2=  (dtjd ge 236.) and (ebias1 lt 56 and ebias2 eq 55)
     setn2='(dtjd ge 236.) and (ebias1 lt 56 and ebias2 eq 55)'
     w2=where(set2)
     xset2=dtjd(w2)
     linearfit2=1
     print,' ' & print,setn2,' ',xset2

   endcase

3: begin
     set1=dtjd le 160.
     setn1='dtjd le 160.'
     w1=where(set1)
     xset1=dtjd(w1)
     linearfit1=0
     print,' ' & print,setn1,' ',xset1

     set2=dtjd gt 160. and dtjd le 237.
     setn2='dtjd gt 160. and dtjd le 237.'
     w2=where(set2)
     xset2=dtjd(w2)
     linearfit2=1
     print,' ' & print,setn2,' ',xset2

     set3=(dtjd gt 237. and dtjd lt 357.) and (ebias1 lt 56 and ebias2 eq 55)
     setn3='(dtjd gt 237. and dtjd lt 357.) and (ebias1 lt 56 and ebias2 eq 55)'
     w3=where(set3)
     xset3=dtjd(w3)
     linearfit3=1
     print,' ' & print,setn3,' ',xset3

   endcase
else:
endcase

;set plot parameters
colr=[50,90,115,150,185,215]
symsize=0.4
charsize=1.5
pos=fltarr(4,6)
pos,6,pos,xoff=0.3 ,xtop=0.8
xrange=[0,700] 
xticks=7
xminor=5
xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
sectjdv=(xtickv+dtjdref)*86400.d
for i=0,n_elements(xtickv)-1 do $
  if sectjdv(i) gt  10000*86400.d then sectjdv(i)=sectjdv(i)-10000*86400.d
pb5timv=lonarr(3,n_elements(xtickv))
for i=0,n_elements(xtickv)-1 do pb5timv(*,i)=sec_pb5(sectjdv(i))
xtickname=string(pb5timv(1,*),format='(i3)')
xticklen=0.075
;end set plot parameters

;------------- do relgains ------------------------------------------------

if hardcpy then begin
  set_plot,'ps'
  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5
endif
if hardcpy eq 0 then window,0

z=relgains
yrange=[0,3.0] 
yticks=3
yminor=2
cfit1=fltarr(3,6)
cfit2=fltarr(3,6)
cfit3=fltarr(3,6)
cfit4=fltarr(3,6)
afit1=fltarr(2,6)
afit2=fltarr(2,6)
afit3=fltarr(2,6)
afit4=fltarr(2,6)

;do detector loop
for i=0,5 do begin

if i eq 0 then begin
  noerase=0 
  title='1 / relgains'
endif else begin
  noerase=1
  title=''
endelse

if i eq 5 then begin
  xtitle='days after '+string(pb5ref(0),format='(i4)')+' '+$
    string(pb5ref(1),format='(i3)')+' '+$
    string(pb5ref(2),format='(i8)') 
  xcharsize=1.0
endif else begin
  xtitle=''
  xcharsize=0.001
endelse

plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    title=title,ytitle='det '+string(i,format='(i1)'),xcharsize=xcharsize,$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=xtitle,position=pos(*,i),noerase=noerase,charsize=charsize,$
    xminor=xminor,xticklen=xticklen
   
  oplot,dtjd,z(i,*),psym=4,symsize=symsize,color=colr(i)
  ;oplot,sday,spline(dtjd,z(i,*),sday);,color=colr(i)

  ;do analysis for selected subsets of input data interval
    
    ;subset1 
    if linearfit1 then begin
      a=fltarr(2)
      if leastdev then $
        plotladfit,dtjd,z,set1,setn1,i,a,xset,yfit,colr(i),checkfit else $
        plotlinfit,dtjd,z,set1,setn1,i,a,xset,yfit,colr(i),checkfit
      afit1(*,i)=a
    endif else begin
      c=fltarr(3)
      plotcrvfit,dtjd,z,mncts,set1,setn1,i,eqwts,c,xset,yfit,colr(i),checkfit
      cfit1(*,i)=c
    endelse
    if w1(0) ne -1 then relgainsfit(i,w1)=yfit

    ;subset2
    if linearfit2 then begin
      a=fltarr(2)
      if leastdev then $
        plotladfit,dtjd,z,set2,setn2,i,a,xset,yfit,colr(i),checkfit else $
        plotlinfit,dtjd,z,set2,setn2,i,a,xset,yfit,colr(i),checkfit
      afit2(*,i)=a
    endif else begin
      c=fltarr(3)
      plotcrvfit,dtjd,z,mncts,set2,setn2,i,eqwts,c,xset,yfit,colr(i),checkfit
      cfit2(*,i)=c
    endelse
    if w2(0) ne -1 then relgainsfit(i,w2)=yfit

    ;subset3
    if linearfit3 then begin
      a=fltarr(2)
      if leastdev then $
        plotladfit,dtjd,z,set3,setn3,i,a,xset,yfit,colr(i),checkfit else $
        plotlinfit,dtjd,z,set3,setn3,i,a,xset,yfit,colr(i),checkfit
      afit3(*,i)=a
    endif else begin
      c=fltarr(3)
      plotcrvfit,dtjd,z,mncts,set3,setn3,i,eqwts,c,xset,yfit,colr(i),checkfit
      cfit3(*,i)=c
    endelse
    if w3(0) ne -1 then relgainsfit(i,w3)=yfit


    ;subset4
    if linearfit4 then begin
      a=fltarr(2)
      if leastdev then $
        plotladfit,dtjd,z,set4,setn4,i,a,xset,yfit,colr(i),checkfit else $
        plotlinfit,dtjd,z,set4,setn4,i,a,xset,yfit,colr(i),checkfit
      afit4(*,i)=a
    endif else begin
      c=fltarr(3)
      plotcrvfit,dtjd,z,mncts,set4,setn4,i,eqwts,c,xset,yfit,colr(i),checkfit
      cfit4(*,i)=c
    endelse
    if w4(0) ne -1 then relgainsfit(i,w4)=yfit

endfor

if hardcpy then begin
  device,/close
  spawn,'lpr idl.ps'
  print,'printing hardcopy'
endif



;------------- do relgainsnml ------------------------------------------------

if hardcpy eq 0 then window,1   

;z=relgainsnml
;for j=0,sz(2)-1 do z(*,j)=relgainsfit(*,j)/relgainsfit(0,j)

yrange=[0,4] 
yticks=1
yminor=8

pos=fltarr(4,6)
pos,6,pos,xoff=0.3 ,xtop=0.8

cfitnml1=fltarr(3,6)
cfitnml2=fltarr(3,6)
cfitnml3=fltarr(3,6)
cfitnml4=fltarr(3,6)

afitnml1=fltarr(2,6)
afitnml2=fltarr(2,6)
afitnml3=fltarr(2,6)
afitnml4=fltarr(2,6)


;do detector loop
for i=0,5 do begin

if i eq 0 then begin
  noerase=0 
  title='1 / relgains (nrmlzd)'
endif else begin
  noerase=1
  title=''
endelse

if i eq 5 then begin
  xtitle='days after '+string(pb5ref(0),format='(i4)')+' '+$
    string(pb5ref(1),format='(i3)')+' '+$
    string(pb5ref(2),format='(i8)') 
  xcharsize=1.0
endif else begin
  xtitle=''
  xcharsize=0.001
endelse

plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    title=title,ytitle='det '+string(i,format='(i1)'),xcharsize=xcharsize,$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=xtitle,position=pos(*,i),noerase=noerase,charsize=charsize,$
    xminor=xminor,xticklen=xticklen
   
  ;oplot,dtjd,z(i,*),psym=4,symsize=symsize,color=colr(i)
  ;oplot,sday,spline(dtjd,z(i,*),sday);,color=colr(i)

  ;do analysis for selected subsets of input data interval
    
    ;subset1
    nmldet=0
    oplot,dtjd(w1),z(i,w1)/z(nmldet,w1),psym=4,symsize=symsize,color=colr(i) 
    if linearfit1 then begin
      a=fltarr(2)
      if leastdev then $
        plotladfit,dtjd,z,set1,setn1,i,a,xset,yfit,colr(i),checkfit else $
        plotlinfit,dtjd,z,set1,setn1,i,a,xset,yfit,colr(i),checkfit
      afitnml1(*,i)=a
    endif else begin
      c=fltarr(3)
      plotcrvfit,dtjd,z,mncts,set1,setn1,i,eqwts,c,xset,yfit,colr(i),checkfit,$
        /normalizegains,nmldet=nmldet

      cfitnml1(*,i)=c
    endelse

    ;subset2
    nmldet=0
    oplot,dtjd(w2),z(i,w2)/z(nmldet,w2),psym=4,symsize=symsize,color=colr(i)
    if linearfit2 then begin
      a=fltarr(2)
      if leastdev then $
        plotladfit,dtjd,z,set2,setn2,i,a,xset,yfit,colr(i),checkfit else $
        plotlinfit,dtjd,z,set2,setn2,i,a,xset,yfit,colr(i),checkfit
      afitnml2(*,i)=a
    endif else begin
      c=fltarr(3)
      plotcrvfit,dtjd,z,mncts,set2,setn2,i,eqwts,c,xset,yfit,colr(i),checkfit,$
        /normalizegains,nmldet=nmldet

      cfitnml2(*,i)=c
    endelse

    ;subset3
    nmldet=0 ;4
    oplot,dtjd(w3),z(i,w3)/z(nmldet,w3),psym=4,symsize=symsize,color=colr(i)
    if linearfit3 then begin
      a=fltarr(2)
      if leastdev then $
        plotladfit,dtjd,z,set3,setn3,i,a,xset,yfit,colr(i),checkfit else $
        plotlinfit,dtjd,z,set3,setn3,i,a,xset,yfit,colr(i),checkfit
      afitnml3(*,i)=a
    endif else begin
      c=fltarr(3)
      plotcrvfit,dtjd,z,mncts,set3,setn3,i,eqwts,c,xset,yfit,colr(i),checkfit,$
        /normalizegains,nmldet=nmldet

      cfitnml3(*,i)=c
    endelse


    ;subset4
    nmldet=0 ;4
    oplot,dtjd(w4),z(i,w4)/z(nmldet,w4),psym=4,symsize=symsize,color=colr(i)
    if linearfit4 then begin
      a=fltarr(2)
      if leastdev then $
        plotladfit,dtjd,z,set4,setn4,i,a,xset,yfit,colr(i),checkfit else $
        plotlinfit,dtjd,z,set4,setn4,i,a,xset,yfit,colr(i),checkfit
      afitnml4(*,i)=a
    endif else begin
      c=fltarr(3)
      plotcrvfit,dtjd,z,mncts,set4,setn4,i,eqwts,c,xset,yfit,colr(i),checkfit,$
        /normalizegains,nmldet=nmldet

      cfitnml4(*,i)=c
    endelse

endfor

if hardcpy then begin
  device,/close
  spawn,'lpr idl.ps'
  print,'printing hardcopy'
endif


;--------------------------- plot ebias etc -------------------------------
pos=fltarr(4,2)
pos,2,pos,xoff=0.3 ,xtop=0.8,ysep=0.2

if hardcpy eq 0 then window,4

yrange=[50,62]
yticks=6
yminor=2
plot,dtjd,yrange,/nodata,yrange=yrange,ystyle=1,yticks=yticks,yminor=yminor,$
  charsize=charsize,title='ebias',position=pos(*,0),$
  xcharsize=xcharsize,$
  xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
  xtitle=xtitle,noerase=noerase,$
  xminor=xminor,xticklen=xticklen

oplot,dtjd,ebias1,psym=4,symsize=symsize
xyouts,dtjd(n_elements(dtjd)/2),ebias1(n_elements(dtjd)/2),/data,'1'

oplot,dtjd,ebias2,psym=4
xyouts,dtjd(n_elements(dtjd)/2),ebias2(n_elements(dtjd)/2),/data,'2'

yrange=[0,150]
yticks=5
plot,dtjd,yrange,/nodata,yrange=yrange,ystyle=1,yticks=yticks,$
  charsize=charsize,title='energy step ev',position=pos(*,1),/noerase,$
  xcharsize=xcharsize,$
  xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
  xtitle=xtitle,$
  xminor=xminor,xticklen=xticklen

oplot,dtjd,enev,color=175,psym=4,symsize=symsize

if hardcpy eq 0 then wshow,0

if hardcpy then begin
  set_plot,'ps'
  device,/close
  set_plot,'x'
  spawn, 'lpr idl.ps'
  print,'printing hardcopy'
  hardcpy=0
  goto,start
endif

if linearfit1 and setn1 ne '' then begin 
  print,' ' & print,setn1 
  print,'afitnml1(0,*) ',transpose(afitnml1(0,*))
  print,'afitnml1(1,*) ',transpose(afitnml1(1,*))
  nmlfitcoeff1=afitnml1
endif 
if linearfit1 eq 0 and setn1 ne '' then begin
  print,' ' & print,setn1 
  print,'cfitnml1(0,*) ',transpose(cfitnml1(0,*))
  print,'cfitnml1(1,*) ',transpose(cfitnml1(1,*))
  print,'cfitnml1(2,*) ',transpose(cfitnml1(2,*))
  nmlfitcoeff1=cfitnml1
endif 

if linearfit2 and setn2 ne '' then begin
  print,' ' & print,setn2
  print,'afitnml2(0,*) ',transpose(afitnml2(0,*))
  print,'afitnml2(1,*) ',transpose(afitnml2(1,*))
  nmlfitcoeff2=afitnml2  
endif 
if linearfit2 eq 0 and setn2 ne '' then begin
  print,' ' & print,setn2
  print,'cfitnml2(0,*) ',transpose(cfitnml2(0,*))
  print,'cfitnml2(1,*) ',transpose(cfitnml2(1,*))
  print,'cfitnml2(2,*) ',transpose(cfitnml2(2,*))
  nmlfitcoeff2=cfitnml2
endif

if linearfit3 and setn3 ne '' then begin
  print,' ' & print,setn3
  print,'afitnml3(0,*) ',transpose(afitnml3(0,*))
  print,'afitnml3(1,*) ',transpose(afitnml3(1,*))
  nmlfitcoeff3=afitnml3  
endif 
if linearfit3 eq 0 and setn3 ne '' then begin
  print,' ' & print,setn3
  print,'cfitnml3(0,*) ',transpose(cfitnml3(0,*))
  print,'cfitnml3(1,*) ',transpose(cfitnml3(1,*))
  print,'cfitnml3(2,*) ',transpose(cfitnml3(2,*))
  nmlfitcoeff3=cfitnml3
endif


if linearfit4 and setn4 ne '' then begin
  print,' ' & print,setn4
  print,'afitnml4(0,*) ',transpose(afitnml4(0,*))
  print,'afitnml4(1,*) ',transpose(afitnml4(1,*))
  nmlfitcoeff4=afitnml4  
endif 
if linearfit3 eq 0 and setn3 ne '' then begin
  print,' ' & print,setn4
  print,'cfitnml4(0,*) ',transpose(cfitnml4(0,*))
  print,'cfitnml4(1,*) ',transpose(cfitnml4(1,*))
  print,'cfitnml4(2,*) ',transpose(cfitnml4(2,*))
  nmlfitcoeff4=cfitnml4
endif

;stop



;--plot relgainsnml (fine-tuned, from getrelgains, flow angle tested) ---------
;use getrelgains and plot relgains for interval 
;for which relgains have been checked

ymd_begin=long(19941130)
ymd_end=long(19960805)

sec_begin=pb5_sec(ymd_pb5(ymd_begin))
sec_end=pb5_sec(ymd_pb5(ymd_end))

if sec_begin lt refseconds then sec_begin=10000*double(86400.) +sec_begin
if sec_end lt refseconds then sec_end=10000*double(86400.) +sec_end

if hardcpy eq 0 then window,2   

yrange=[0,5] 
yticks=1
yminor=10

pos=fltarr(4,6)
pos,6,pos,xoff=0.3 ,xtop=0.8


;do detector loop
for i=0,5 do begin

  if i eq 0 then begin
    noerase=0 
    title='1 / relgains (nrmlzd, fine-tuned, flow angle tested)'
  endif else begin
    noerase=1
    title=''
  endelse

  if i eq 5 then begin
    xtitle='days after '+string(pb5ref(0),format='(i4)')+' '+$
      string(pb5ref(1),format='(i3)')+' '+$
      string(pb5ref(2),format='(i8)') 
    xcharsize=1.0
  endif else begin
    xtitle=''
    xcharsize=0.001
  endelse

 plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    title=title,ytitle='det '+string(i,format='(i1)'),xcharsize=xcharsize,$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=xtitle,position=pos(*,i),noerase=noerase,charsize=charsize,$
    xminor=xminor,xticklen=xticklen
   
  ;oplot,dtjd,z(i,*),psym=4,symsize=symsize,color=colr(i)
  ;oplot,sday,spline(dtjd,z(i,*),sday);,color=colr(i)

  xx_begin=sec_begin/double(86400.)-dtjdref
  xx_end=sec_end/double(86400.)-dtjdref

  xx=xx_begin+indgen(xx_end-xx_begin+1)
  yy=fltarr(n_elements(xx))

  for j=0,n_elements(xx)-1 do begin
    secxx=(xx(j)+dtjdref)*double(86400.)
    if secxx gt 10000*double(86400.) then secxx=secxx-10000*double(86400.)
    getrelgains,relgain,secxx,relgainchange
    yy(j)=relgain(i)
  endfor
  oplot,xx,yy

  ;subset1
    nmldet=0 ;4
    oplot,dtjd(w1),z(i,w1)/z(nmldet,w1),psym=4,symsize=symsize,color=colr(i)
  ;subset2
    nmldet=0 ;4
    oplot,dtjd(w2),z(i,w2)/z(nmldet,w2),psym=4,symsize=symsize,color=colr(i)
  ;subset3
    nmldet=0 ;4
    oplot,dtjd(w3),z(i,w4)/z(nmldet,w3),psym=4,symsize=symsize,color=colr(i)
   ;subset4
    nmldet=0 ;4
    oplot,dtjd(w4),z(i,w4)/z(nmldet,w4),psym=4,symsize=symsize,color=colr(i)

endfor

if hardcpy then begin
  device,/close
  spawn,'lpr idl.ps'
  print,'printing hardcopy'
endif


stop
end


;=============================== main ====================================

;fitting time variation of relative gains of swe detectors

hardcopy=1

loadct,18


;-------------------------- initial fit ----------------------------------
;do initail fit using gains determined by matching pitch angle fit for each
;detector with all detectors  (Faifield method)


initialfit,dtjd_init,dtjdref,linearfit1,linearfit2,linearfit3,linearfit4,$
  fitnml1,fitnml2,fitnml3,fitnml4,$
  xset1,xset2,xset3,xset4,hardcopy

end
