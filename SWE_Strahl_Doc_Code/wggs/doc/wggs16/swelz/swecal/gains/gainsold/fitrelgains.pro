
;========================= input =============================================

pro readinput,relgains,pb5tim,ebias1,ebias2,mncts,bangle,$
  enev,refdate,pb5ref,elapsec
  
refdate=19941130l  

pb5ref=ymd_pb5(refdate)

relgains=fltarr(6,1000)
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
;flnm=dir+'savegains_971012.dat'
;flnm=dir+'savegains_980406.dat'
;flnm=dir+'savegains_980513.dat'
;flnm=dir+'savegains_980811.dat'
;flnm=dir+'savegains_981118.dat'
flnm=dir+'savegains_990610.dat'

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
  mncts(i)=mean
  bangle(i)=acos(b(0)/sqrt(total(b*b)))/!dtor
  if bangle(i) gt 90. then bangle(i)=180.-bangle(i)
  enev(i)=en
  
  print,i
  print,sdate
  endwhl:
endwhile

free_lun,lun

;print,'hit return to continue' & answ='' & read,answ 

n=i+1
relgains=relgains(*,0:n-1)
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

pro timeorder,relgains,pb5tim,ebias1,ebias2,mncts,bangle,$
  enev,pb5ref,elapsec

;put input arrays in time order

tsort=sort(elapsec)
pb5tim(*,*)=pb5tim(*,tsort)
ebias1=ebias1(tsort)
ebias2=ebias2(tsort)
relgains(*,*)=relgains(*,tsort)
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

if n_elements(y) lt 3 then return

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


;======================= intervals ==========================================

pro intervals, pb5ref,ymdend,pb5tim,elapsec,ebias1,ebias2,tsegm

tsegm=replicate({bias1lim:0,bias2lim:0,pb5ref:lonarr(3),$
  pb5lim1:lonarr(3),elapseclim1:0l,pb5lim2:lonarr(3),elapseclim2:0l,$
  indw:0l,nw:0l,clr:0,linfit:intarr(6)  },15)

k=-1
;time segment 1
   pb5_change1=long([1995,130,45104614])   ;=19950510
   elapsec_change1=pb5_elapsec(pb5_change1,pb5ref)
   set1=  ebias1 lt 56 and ebias2 eq 52 and $
          elapsec le elapsec_change1
   w1=where(set1,nw1)
   clr1=50
   linfit1=[0,0,0,0,0,0]

   k=k+1   
   tsegm(k).bias1lim=56
   tsegm(k).bias2lim=52
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5ref
   tsegm(k).elapseclim1=pb5_elapsec(pb5ref,pb5ref)
   tsegm(k).pb5lim2=pb5_change1
   tsegm(k).elapseclim2=elapsec_change1
   tsegm(k).indw=w1(0)
   tsegm(k).nw=nw1
   tsegm(k).clr=clr1
   tsegm(k).linfit=linfit1
   
;time segment 2 
   pb5_change2=long([1995,325,54600000])   ;=19951121
   elapsec_change2=pb5_elapsec(pb5_change2,pb5ref)  
   set2=  ebias1 lt 56 and ebias2 eq 55 or $
          elapsec gt elapsec_change1 and elapsec le elapsec_change2
   w2=where(set2,nw2)
   clr2=90
   linfit2=[0,0,0,0,0,0]

   k=k+1   
   tsegm(k).bias1lim=56
   tsegm(k).bias2lim=55
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change1
   tsegm(k).elapseclim1=elapsec_change1
   tsegm(k).pb5lim2=pb5_change2
   tsegm(k).elapseclim2=elapsec_change2
   tsegm(k).indw=w2(0)
   tsegm(k).nw=nw2
   tsegm(k).clr=clr2
   tsegm(k).linfit=linfit2
   
;time segment 3
   pb5_change3=long([1996,134,75220409])   ;=19960513
   elapsec_change3=pb5_elapsec(pb5_change3,pb5ref)       
   set3=  ebias1 eq 59 and ebias2 ge 59 and $
          elapsec gt elapsec_change2 and elapsec le elapsec_change3
   w3=where(set3,nw3)
   clr3=115
   linfit3=[1,1,1,1,1,1]
 
   k=k+1  
   tsegm(k).bias1lim=59
   tsegm(k).bias2lim=59
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change2
   tsegm(k).elapseclim1=elapsec_change2
   tsegm(k).pb5lim2=pb5_change3
   tsegm(k).elapseclim2=elapsec_change3
   tsegm(k).indw=w3(0)
   tsegm(k).nw=nw3
   tsegm(k).clr=clr3
   tsegm(k).linfit=linfit3
   
   
;time segments 4_1 and 4_2
   clr4=145
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
   
   clr4=145

   k=k+1
   tsegm(k).bias1lim=61
   tsegm(k).bias2lim=59
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change3
   tsegm(k).elapseclim1=elapsec_change3
   tsegm(k).pb5lim2=pb5_change4_1
   tsegm(k).elapseclim2=elapsec_change4_1
   tsegm(k).indw=w4_1(0)
   tsegm(k).nw=nw4_1
   tsegm(k).clr=clr4
   tsegm(k).linfit=linfit4_1
   
   
   set4_2=  ebias1 eq 61 and ebias2 ge 59 and $
            elapsec ge elapsec_change4_1 and elapsec lt elapsec_change4_2
   w4_2=where(set4_2,nw4_2) 
   linfit4_2=[0,0,0,0,0,0]

   k=k+1
   tsegm(k).bias1lim=61
   tsegm(k).bias2lim=59
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change4_1
   tsegm(k).elapseclim1=elapsec_change4_1
   tsegm(k).pb5lim2=pb5_change4_2
   tsegm(k).elapseclim2=elapsec_change4_2
   tsegm(k).indw=w4_2(0)
   tsegm(k).nw=nw4_2
   tsegm(k).clr=clr4
   tsegm(k).linfit=linfit4_2
     
;time segments 4_3 and 4_4 
   yrmd4_3=long(19970613)  ;=1997         164           0 
   pb5_change4_3=ymd_pb5(yrmd4_3) 
   elapsec_change4_3=pb5_elapsec(pb5_change4_3,pb5ref)
   set4_3=  ebias1 eq 61 and ebias2 ge 59 and $
            elapsec ge elapsec_change4_2 and elapsec lt elapsec_change4_3
   w4_3=where(set4_3,nw4_3) 
   linfit4_3=[1,1,1,0,1,1]    ;[0,0,0,0,0,0]   

   k=k+1
   tsegm(k).bias1lim=61
   tsegm(k).bias2lim=59
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change4_2
   tsegm(k).elapseclim1=elapsec_change4_2
   tsegm(k).pb5lim2=pb5_change4_3
   tsegm(k).elapseclim2=elapsec_change4_3
   tsegm(k).indw=w4_3(0)
   tsegm(k).nw=nw4_3
   tsegm(k).clr=clr4
   tsegm(k).linfit=linfit4_3
     
;time segment 4_4
   yrmd4_4=long(19971028)  ;1997, 301, 0   ;det gains begin to degrade rapidly
   pb5_change4_4=ymd_pb5(yrmd4_4)
   elapsec_change4_4=pb5_elapsec(pb5_change4_4,pb5ref)
   set4_4=  ebias1 eq 61 and ebias2 ge 59 and $
            elapsec ge elapsec_change4_3 and elapsec lt elapsec_change4_4
   w4_4=where(set4_4,nw4_4)
   linfit4_4=[1,1,1,0,1,1]
 
   k=k+1  
   tsegm(k).bias1lim=61
   tsegm(k).bias2lim=59
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change4_3
   tsegm(k).elapseclim1=elapsec_change4_3
   tsegm(k).pb5lim2=pb5_change4_4
   tsegm(k).elapseclim2=elapsec_change4_4
   tsegm(k).indw=w4_4(0)
   tsegm(k).nw=nw4_4
   tsegm(k).clr=clr4
   tsegm(k).linfit=linfit4_4
    
;time segment 4_5    
   yrmd4_5=long(19980514) ; 1998, 134, 55749000; bias change (66,64)on this day
   pb5_change4_5=ymd_pb5(yrmd4_5)
   pb5_change4_5(2)=55749000l
   elapsec_change4_5=pb5_elapsec(pb5_change4_5,pb5ref)      
   set4_5=ebias1 eq 61 and ebias2 ge 59 and $
            elapsec ge elapsec_change4_4 and elapsec lt elapsec_change4_5
   w4_5=where(set4_5,nw4_5)
   linfit4_5=[1,1,0,0,1,1]
 
   k=k+1         
   tsegm(k).bias1lim=61
   tsegm(k).bias2lim=59
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change4_4
   tsegm(k).elapseclim1=elapsec_change4_4
   tsegm(k).pb5lim2=pb5_change4_5
   tsegm(k).elapseclim2=elapsec_change4_5
   tsegm(k).indw=w4_5(0)
   tsegm(k).nw=nw4_5
   tsegm(k).clr=clr4
   tsegm(k).linfit=linfit4_5
   
   
   nw4=nw4_1+nw4_2+nw4_3+nw4_4+nw4_5
   w4=lonarr(nw4)
   w4(*)=[w4_1,w4_2,w4_3,w4_4,w4_5]


   
 ;time segment 5    
   yrmd5=long(19980728) ;1998, 209, 48669000; bias change (71,69) on this day
   pb5_change5=ymd_pb5(yrmd5)
   pb5_change5(2)=48669000l
   elapsec_change5=pb5_elapsec(pb5_change5,pb5ref)        
   set5=ebias1 eq 66 and ebias2 eq 64 and $
            elapsec gt elapsec_change4_5 and elapsec le elapsec_change5
   w5=where(set5,nw5)
   clr5=50
   linfit5=[1,1,1,1,1,1]  
   
   k=k+1         
   tsegm(k).bias1lim=66
   tsegm(k).bias2lim=64
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change4_5
   tsegm(k).elapseclim1=elapsec_change4_5
   tsegm(k).pb5lim2=pb5_change5
   tsegm(k).elapseclim2=elapsec_change5
   tsegm(k).indw=w5(0)
   tsegm(k).nw=nw5
   tsegm(k).clr=clr5
   tsegm(k).linfit=linfit5
   
   
;time segment 6
   yrmd6=long(19980811) ; 1998, 223, 70500000; bias change (73,69) on this day
   pb5_change6=ymd_pb5(yrmd6)
   pb5_change6(2)=70500000l
   elapsec_change6=pb5_elapsec(pb5_change6,pb5ref)      
   set6=ebias1 eq 71 and ebias2 eq 69 and $
            elapsec gt elapsec_change5 and elapsec le elapsec_change6
   w6=where(set6,nw6)
   clr6=90
   linfit6=[1,1,1,1,1,1]
  
    k=k+1         
   tsegm(k).bias1lim=71
   tsegm(k).bias2lim=69
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change5
   tsegm(k).elapseclim1=elapsec_change5
   tsegm(k).pb5lim2=pb5_change6
   tsegm(k).elapseclim2=elapsec_change6
   tsegm(k).indw=w6(0)
   tsegm(k).nw=nw6
   tsegm(k).clr=clr6
   tsegm(k).linfit=linfit6
   
     
;time segments 7_1 and 7_2
   yrmd7_1=long(19980905)  ;1998, 248, 0l; change in det 2 response     
   pb5_change7_1=ymd_pb5(yrmd7_1)
   elapsec_change7_1=pb5_elapsec(pb5_change7_1,pb5ref)
   set7_1=ebias1 eq 73 and ebias2 eq 69 and $
            elapsec gt elapsec_change6 and elapsec le elapsec_change7_1
   w7_1=where(set7_1,nw7_1)
   clr7=115
   linfit7_1=[1,1,0,1,1,1]

   k=k+1         
   tsegm(k).bias1lim=73
   tsegm(k).bias2lim=69
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change6
   tsegm(k).elapseclim1=elapsec_change6
   tsegm(k).pb5lim2=pb5_change7_1
   tsegm(k).elapseclim2=elapsec_change7_1
   tsegm(k).indw=w7_1(0)
   tsegm(k).nw=nw7_1
   tsegm(k).clr=clr7
   tsegm(k).linfit=linfit7_1
   
   
   yrmd7_2=long(19981124)  ;1998, 328, 0; bias change (76,71) on this day     
   pb5_change7_2=ymd_pb5(yrmd7_2)
   elapsec_change7_2=pb5_elapsec(pb5_change7_2,pb5ref)
   set7_2=ebias1 eq 73 and ebias2 eq 69 and $
            elapsec gt elapsec_change7_1 and elapsec le elapsec_change7_2
   w7_2=where(set7_2,nw7_2)
   clr7=115
   linfit7_2=[1,1,0,1,1,1]
   
   k=k+1         
   tsegm(k).bias1lim=73
   tsegm(k).bias2lim=69
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change7_1
   tsegm(k).elapseclim1=elapsec_change7_1
   tsegm(k).pb5lim2=pb5_change7_2
   tsegm(k).elapseclim2=elapsec_change7_2
   tsegm(k).indw=w7_2(0)
   tsegm(k).nw=nw7_2
   tsegm(k).clr=clr7
   tsegm(k).linfit=linfit7_2
   
   nw7=nw7_1+nw7_2
   w7=lonarr(nw7)
   w7=[w7_1,w7_2]
      
;time segment 8
   yrmd8=long(19990326)  ;1999, 85, 0; bias change (78,71) on this day     
   pb5_change8=ymd_pb5(yrmd8)
   elapsec_change8=pb5_elapsec(pb5_change8,pb5ref)
   set8=ebias1 eq 76 and ebias2 eq 71 and $
            elapsec gt elapsec_change7_2 and elapsec le elapsec_change8
   w8=where(set8,nw8)
   clr8=145
   linfit8=[1,1,1,1,1,1]

   k=k+1         
   tsegm(k).bias1lim=76
   tsegm(k).bias2lim=71
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change7_2
   tsegm(k).elapseclim1=elapsec_change7_2
   tsegm(k).pb5lim2=pb5_change8
   tsegm(k).elapseclim2=elapsec_change8
   tsegm(k).indw=w8(0)
   tsegm(k).nw=nw8
   tsegm(k).clr=clr8
   tsegm(k).linfit=linfit8
   
    
;time segment 9
   yrmd9=long(19990405)  ;1999, 95, 0; bias change (78,76) on this day     
   pb5_change9=ymd_pb5(yrmd9)
   elapsec_change9=pb5_elapsec(pb5_change9,pb5ref)
   set9=ebias1 eq 78 and ebias2 eq 71 and $
            elapsec gt elapsec_change8 and elapsec le elapsec_change9
   w9=where(set9,nw9)
   clr9=50
   linfit9=[1,1,1,1,1,1]
      
   k=k+1         
   tsegm(k).bias1lim=78
   tsegm(k).bias2lim=71
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change8
   tsegm(k).elapseclim1=elapsec_change8
   tsegm(k).pb5lim2=pb5_change9
   tsegm(k).elapseclim2=elapsec_change9
   tsegm(k).indw=w9(0)
   tsegm(k).nw=nw9
   tsegm(k).clr=clr9
   tsegm(k).linfit=linfit9
   
   
    
;time segment 10     ;last time segment, plot data , no fit 
   yrmd10=ymdend 
   pb5_change10=ymd_pb5(yrmd10)
   elapsec_change10=pb5_elapsec(pb5_change10,pb5ref)
   set10=ebias1 eq 78 and ebias2 eq 76 and $
            elapsec gt elapsec_change9 and elapsec le elapsec_change10
   w10=where(set10,nw10)          
   clr10=90
   
   k=k+1         
   tsegm(k).bias1lim=78
   tsegm(k).bias2lim=76
   tsegm(k).pb5ref=pb5ref
   tsegm(k).pb5lim1=pb5_change9
   tsegm(k).elapseclim1=elapsec_change9
   tsegm(k).pb5lim2=pb5_change10
   tsegm(k).elapseclim2=elapsec_change10
   tsegm(k).indw=w10(0)
   tsegm(k).nw=nw10
   tsegm(k).clr=clr10
   ;tsegm(k).linfit=linfit10
                               
   print,total(tsegm.nw),n_elements(ebias1)
   if total(tsegm.nw) ne n_elements(ebias1) then begin
     for i=0,n_elements(ebias1)-1 do $
       print,i,pb5tim(0,i),pb5tim(1,i),pb5tim(2,i),ebias1(i),ebias2(i)
     stop
   endif  
      


end



;======================= MAIN ===============================================



usegetrelgains=1
createsavefile=0
hardcpy=0
ymdend=19991231l


print,'enter 0 or 1 for usegetrelgains and createsavefile'
print,'hit return for defaults: usegetrelgains=1 and createsavefile=0'
answ=['',''] & read,answ & if answ(0) ne '' then begin
  usegetrelgains=fix(answ(0))
  createsavefile=fix(answ(1))
endif
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
readinput,relgains,pb5tim,ebias1,ebias2,mncts,bangle,$
enev,refdate,pb5ref,elapsec

;put in time order
timeorder,relgains,pb5tim,ebias1,ebias2,mncts,bangle,$
  enev,pb5ref,elapsec

;normalize relative gains to det # 0
relgainsnml=fltarr(6,n_elements(ebias1))   
for i=0,5 do relgainsnml(i,*)=relgains(i,*)/relgains(0,*)
      
sz=size(relgains)

openw,lunw,getenv('IDLSAV')+'relgains_daily.dat',/get_lun
for i=0,sz(2)-1 do begin
  printf,lunw,pb5_ymd(pb5tim(*,i)),pb5tim(*,i),relgains(*,i),$
    format='(i8,2x,i4,i4,i9,5x,6(f7.2))'
endfor
free_lun,lunw


;order in time segments acording to bias levels
intervals, pb5ref,ymdend,pb5tim,elapsec,ebias1,ebias2,tsegm
nsegm=n_elements(tsegm)


;set plot parameters

ymd_begin=19941130l
numdays=1800
xticks=6
xminor=5

print,'time sub-intervals:'
print,'index     ymd_begin    tmd_end   '
for k=0,nsegm-1 do $ 
  print,k,pb5_ymd(tsegm(k).pb5lim1),pb5_ymd(tsegm(k).pb5lim2)
 
 
print,'select begin interval index, ',$
      'or hit return for entire interval'
answ0='' & read,answ0 
if answ0 ne '' then begin
  ymd_begin=pb5_ymd(tsegm(fix(answ0)).pb5lim1)
  print,'select end interval index'
  answ1='' & read,answ1
  k0=fix(answ0)
  k1=fix(answ1)  
  
  intrvl=30
  nintrvls_actual=$
    ((float(tsegm(k1).elapseclim2-tsegm(k0).elapseclim1))/86400.)/intrvl
  if nintrvls_actual ne fix(nintrvls_actual) then $
    nintrvls=fix(nintrvls_actual)+1 else $
      nintrvls=fix(nintrvls_actual)
  numdays=nintrvls*intrvl
endif 


pb5_begin=ymd_pb5(ymd_begin)
elapsec_begin=pb5_elapsec(pb5_begin,pb5ref)
elapsec_end=elapsec_begin+numdays*86400l
   
xrange=[elapsec_begin,elapsec_end]

xtickv=xrange(0)+indgen(xticks+1)*(xrange(1)-xrange(0))/xticks
pb5tickv=lonarr(3,n_elements(xtickv))
xtickname=strarr(n_elements(xtickv))
for i=0,n_elements(xtickv)-1 do begin
  pb5tickv(*,i)=elapsec_pb5(xtickv(i),pb5ref)
  xtickname(i)=strmid(string(pb5_ymd(pb5tickv(*,i)),format='(i8)'),2,6)
endfor
  
;xtickname=strmid(strtrim(string(pb5tickv(0,*)),2),2,2)+'.'+$
;  strtrim(string(pb5tickv(1,*),format='(i3)'),1)
xticklen=0.09  ;0.075
noerase=1+intarr(6) & noerase(0)=0
xtitle=strarr(6) & xtitle(5)='date (yymmdd)'
xcharsize=0.001+fltarr(6) & xcharsize(5)=1.0

symsize=0.4
if hardcpy then charsize=1.0 else charsize=1.25

;end set plot parameters




;----------------------- plot ebias and energy levels ------------------------

posn=fltarr(4,2)
pos,2,posn


if hardcpy then begin
  set_plot,'ps'
  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5
endif
if hardcpy eq 0 then window,2

yrange=[54,84] 
yticks=6
yminor=5
plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    ytitle='ebias level',xcharsize=0.001,$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=' ',position=posn(*,0),noerase=0,charsize=charsize,$
    xminor=xminor,xticklen=xticklen

for k=0,nsegm-1 do begin  
  if tsegm(k).nw gt 1 then begin
    wset=tsegm(k).indw+indgen(tsegm(k).nw)
    oplot,elapsec(wset),ebias1(wset),psym=4,symsize=symsize,$
      color=tsegm(k).clr
    oplot,elapsec(wset),ebias2(wset),psym=4
    if k eq 0 then begin
      xyouts,elapsec(n_elements(elapsec(wset))/2),$
        ebias1(n_elements(elapsec(wset))/2),/data,'1'
      xyouts,elapsec(n_elements(elapsec(wset))/2),$
        ebias2(n_elements(elapsec(wset))/2),/data,'2'
    endif
  endif
endfor


yrange=[0,150] 
yticks=5
yminor=1
plot,xrange,yrange,/nodata,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    ytitle='energy step ev',xcharsize=1.0,$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle='date (yymmdd)',position=posn(*,1),noerase=1,charsize=charsize,$
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

for k=0,nsegm-1 do begin
  if tsegm(k).nw gt 1 then begin
    wset=tsegm(k).indw+indgen(tsegm(k).nw)
    oplot,elapsec(wset),z(i,wset),psym=4,symsize=symsize,color=clr1
    if k ne nsegm-1 then $
      plotcrvfit,elapsec(wset),reform(z(i,wset)),mncts(wset),$
      i,eqwts,c,yfit,tsegm(k).clr,linfit=tsegm(k).linfit(i)
      ;last time segment, plot data , no fit 
  endif    
endfor

endfor   ;end detector loop for relgains plot 

if hardcpy then begin
  device,/close
  spawn,'lpr idl.ps'
  print,'printing hardcopy'
  set_plot,'x'
endif


if usegetrelgains then begin  ;get relgains from relgains table
  ;restore,getenv('IDLSAV')+'relgains_tbl.dat'
  restore,getenv('WGGSBASE')+'swe/cal/gains/getrelgains_tbl.dat'
  ;restore,getenv('SWEDATLIB')+'relgains_tbl.dat'
  help,datetbl,rgtbl
  
  nsetmx=max(tsegm.nw)
  rgw=-1.+fltarr(6,nsetmx,nsegm) 
  for k=0,nsegm-1 do begin    
    if tsegm(k).nw gt 1 then begin
      wset=tsegm(k).indw+indgen(tsegm(k).nw)         
      for j=0,n_elements(elapsec(wset))-1 do begin
          pb5tm=elapsec_pb5(elapsec(wset(j)),pb5ref) 
          sectjd=pb5_sec(pb5tm)
          date=pb5_ymd(pb5tm)
          wdate=where(date eq datetbl,nwdate)
          if nwdate ne 1 then stop,'bad gains table'
          rgw(*,j,k)=rgtbl(*,wdate(0))
      endfor
    endif
  endfor    
endif
 ;stop        
;---------------------------- plot normalized relative gains -------------------

posn=fltarr(4,6)
pos,6,posn

z=relgainsnml
yrange=fltarr(2,6)
y_range=[0,6.0] 
yticks=4
yminor=2

coeff=fltarr(3,6,nsegm)

if hardcpy then begin
  set_plot,'ps'
  device,/inches,xoffset=1.,yoffset=1.,xsize=7.,ysize=9.5
endif
if hardcpy eq 0 then window,1,xsize=900,ysize=900


for i=0,5 do begin     ;detector loop for nrmlzd relgains plot

eqwts=0
checkfit=0
if i eq 0 then title='1 / relgains (nrmlzd)' else title=''
  
case i of
0: yrange=[0,16]
1: yrange=[0,4]
2: yrange=[0,16]
3: yrange=[0,16]
4: yrange=[0,4]
5: yrange=[0,4]
endcase

  
plot,xrange,yrange,yrange=yrange,yticks=yticks,ystyle=1,yminor=yminor,$
    title=title,ytitle='det '+string(i,format='(i1)'),xcharsize=xcharsize(i),$
    xrange=xrange,xticks=xticks,xstyle=1,xtickname=xtickname,$
    xtitle=xtitle(i),position=posn(*,i),noerase=noerase(i),charsize=charsize,$
    xminor=xminor,xticklen=xticklen,/nodata


for k=0,nsegm-1 do begin
    if tsegm(k).nw gt 1 then begin
      wset=tsegm(k).indw+indgen(tsegm(k).nw)
      oplot,elapsec(wset),z(i,wset),psym=4,symsize=symsize,color=tsegm(k).clr
      ;use relgains from relgains table (obtained from getrelgains procedure)
      if usegetrelgains then $ 
        oplot,elapsec(wset),rgw(i,*,k),color=200 $
      else if usegetrelgains eq 0 and k ne nsegm-1 then begin
        ;last interval, no fit coefficicients
        plotcrvfit,elapsec(wset),reform(z(i,wset)),mncts(wset),i,eqwts,c,$
          yfit,tsegm(k).clr,linfit=tsegm(k).linfit(i)
        if createsavefile then coeff(*,i,k)=c  
      endif
    endif  
endfor ;end time segment loop
     

endfor   ;end detector loop for relgains plot 

if createsavefile then begin
  print,'saving relgain_coefficients'
  ;save,filename=getenv('IDLSAV')+'relgain_coefficients_98jun02',$
  ;save,filename=getenv('IDLSAV')+'relgain_coefficients_98apr06',$
  ;save,filename=getenv('IDLSAV')+'relgain_coefficients_98may13',$
  ;save,filename=getenv('IDLSAV')+'relgain_coefficients_98aug11',$
  ;save,filename=getenv('IDLSAV')+'relgain_coefficients_98nov18',$
  ;  refdate,pb5ref,pb5_change1,pb5_change2,pb5_change3,$
  ;  pb5_change4_1,pb5_change4_2,pb5_change4_3,pb5_change4_4,pb5_change4_5,$
  ;  pb5_change5_1,pb5_change5_2,pb5_change6,pb5_change7,coeff 
  
  save,filename=getenv('IDLSAV')+'relgain_coefficients_99jun15',$
    refdate,pb5ref,pb5_change1,pb5_change2,pb5_change3,$
    pb5_change4_1,pb5_change4_2,pb5_change4_3,pb5_change4_4,pb5_change4_5,$
    pb5_change5,pb5_change6,pb5_change7_1,pb5_change7_2,pb5_change8,$
    pb5_change9,coeff 
endif

if hardcpy then begin
  device,/close
  spawn,'lpr idl.ps'
  print,'printing hardcopy'
  set_plot,'x'
  hardcpy=0 & goto,start
endif


   
end  


