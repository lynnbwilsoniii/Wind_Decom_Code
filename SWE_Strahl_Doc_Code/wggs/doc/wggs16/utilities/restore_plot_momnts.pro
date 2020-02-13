;-------------- a procedure to determine corners of plot panels --------------
;-------------- (main program follows)                          --------------

pro pos,npnl,posn,yoff=yoff,ytop=ytop,xoff=xoff,xtop=xtop,ysep=ysep,$
  ysize=ysize,col=col

posn=fltarr(4,npnl)
if keyword_set(xoff) eq 0 then xoff=0.175     ;xoff=0.15
if keyword_set(xtop) eq 0 then xtop=0.80  ;0.84      ;xtop=0.875
if keyword_set(yoff) eq 0 then yoff=0.125       ;yoff=0.1
if keyword_set(ytop) eq 0 then ytop=1.0       ;ytop=1.0
if keyword_set(ysep) eq 0 then ysep=0.02
if keyword_set(col) eq 0 then col=0

if col ge 1 then col=1
if npnl-2*fix(npnl/2) ne 0 and col eq 1 then return

if npnl eq 1 then begin
  yoff=0.20
  posn(1,0)=yoff
  posn(3,0)=0.75*(ytop-1.5*yoff-ysep)
endif

yspace=ytop-1.5*yoff-( (npnl-1)/(col+1))*ysep
if keyword_set(ysize) eq 0 then begin
  ypnl=replicate(yspace/(npnl/(col+1)),npnl) 
endif else ypnl=(ysize/total(ysize))*yspace

yl=fltarr(npnl)
yu=fltarr(npnl)

xl=fltarr(npnl)
xu=fltarr(npnl)

case col of
0: begin
     for i=0,npnl-1 do begin
       if i eq 0 then yl(i)=yoff else yl(i)=yl(i-1)+ypnl(i-1)+ysep
     endfor
     xl=replicate(xoff,npnl)
     xpnl=xtop-xoff
   endcase

1: begin
     xsep=0.05
     xpnl=(xtop-xoff)/2-xsep
     for i=0,npnl-1,2 do begin
       if i le 1 then yl(i)=yoff else yl(i)=yl(i-2)+ypnl(i-2)+ysep
       yl(i+1)=yl(i)
       xl(i+1)=xoff
       xl(i)=xoff+xpnl+xsep
     endfor
   endcase
endcase

yu=yl+ypnl
xu=xl+xpnl

posn(1,*)=reverse(yl)
posn(3,*)=reverse(yu)
posn(0,*)=reverse(xl)
posn(2,*)=reverse(xu)


end


;================= main program =============================================
;reads and plots variables from an idlsave file

start:
;set the data path
  ;setenv,'IDLSAV=/home/u3rjf/idlsav/'

;the idlsav data filename
  savpath=getenv('IDLSAV')
  filename=pickfile(/read,get_path=savpath,path=savpath,$
               filter='*idlsave.dat',$
               title='IDLsav Files')
  

hardcopy=0

;initialize the data structure
  idlsav=0  

;restore the idlsave file  
  restore,filename


;idlsav is an array of data structures, idlsav(i) for the i'th moment variable
;idlsav(i).x(0:ndata-1) and idlsav(i).y(0:ndata-1) are 
;  the hour of day, and value of the moment variable, 
;  where index i corresponds to i'th moment variable 
;  and ndata is the number of elements in the i'th variable
;  (ndata is the same for each i)
;the idlsav  structure also contains plot scale parameters etc for each variable 

;print information about the data structure  
help,idlsav
help,idlsav,/str
for i=0,n_elements(idlsav)-1 do begin
  ;help,idlsav(i),/str
  print,i,'  ',idlsav(i).datatype,'  ',idlsav(i).varname,'  '
endfor

  
  npl=n_elements(idlsav)      ;number of moments variables to plot
  pos=fltarr(4,npl)
  pos,npl,pos
  noerase=intarr(npl)+1 & noerase(0)=0
  title=strarr(npl) & title(0)=idlsav(0).title
  xtitle=strarr(npl) & xtitle(npl-1)=idlsav(npl-1).xtitle
  subtitle=strarr(npl) & subtitle(npl-1)=idlsav(npl-1).subtitle

  if hardcopy eq 0 then window,0,xsize=650,ysize=700
  for i=0,npl-1 do begin
    xrange=idlsav(i).xrange
    xticks=idlsav(i).xticks
    xminor=idlsav(i).xminor
    xtickname=replicate(string(32b),xticks+1)
    if i eq npl-1 then xtickname=idlsav(npl-1).xtickname(0:xticks)
    xtickv=idlsav(npl-1).xtickv(0:xticks)
    psym=idlsav(i).psym
    if idlsav(i).log then yticks=0 else yticks=idlsav(i).yticks
    wtn0=where(idlsav(i).x ne 0)
    x=idlsav(i).x(wtn0)
    y=idlsav(i).y(wtn0)

    plot,x,y,ylog=idlsav(i).log,psym=psym,$
         title=title(i),subtitle=subtitle(i),$
         xrange=xrange,xticks=xticks,xstyle=1,$
         xtitle=xtitle(i),xtickname=xtickname,$
         xtickv=xtickv,xminor=xminor,$
         yrange=idlsav(i).yrange,yticks=yticks,ystyle=1,$
         ytitle=idlsav(i).varname,$
         position=pos(*,i),noerase=noerase(i),/normal,charsize=1.25
  endfor   

if hardcopy then begin
  device,/close
  set_plot,'x'
  hardcopy=0
  spawn,'lpr '+getenv('IDLSAV')+'idl.ps'
endif    

goto,start

print,' '
print,'Do you want hardcopy? y/n'
answ='' & read,answ 
if answ eq 'y' then begin
  hardcopy=1
  set_plot,'ps'
endif


end
