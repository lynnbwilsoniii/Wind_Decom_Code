pro pos,npnl,posn,yoff=yoff,ytop=ytop,xoff=xoff,xtop=xtop,ysep=ysep,$
  ysize=ysize,col=col,ypnl_relsiz=ypnl_relsiz,xsep=xsep

posn=fltarr(4,npnl)
if keyword_set(xoff) eq 0 then xoff=0.175     ;xoff=0.15
if keyword_set(xtop) eq 0 then xtop=0.80  ;0.84      ;xtop=0.875
if keyword_set(yoff) eq 0 then yoff=0.125       ;yoff=0.1
if keyword_set(ytop) eq 0 then ytop=1.0       ;ytop=1.0
if keyword_set(ysep) eq 0 then ysep=0.005;2
if keyword_set(col) eq 0 then col=0
if keyword_set(yoff1) eq 0 then yoff1=0.20
if keyword_set(xsep) eq 0 then xsep=0.05


if col ge 1 then col=1
if npnl-2*fix(npnl/2) ne 0 and col eq 1 then return

if npnl eq 1 then begin
  ;yoff=0.2 & ytop=0.7
  posn(1,0)=yoff
  posn(3,0)=ytop
  ;posn(3,0)=0.6*(ytop-1.5*yoff-ysep)
endif

yspace=ytop-1.5*yoff-( (npnl-1)/(col+1))*ysep
if keyword_set(ysize) eq 0 then begin
  if keyword_set(ypnl_relsiz) eq 0 then  $
    ypnl=replicate(yspace/(npnl/(col+1)),npnl) else $
    ypnl=ypnl_relsiz*yspace
endif else begin
  if keyword_set(ypnl_relsiz) eq 0 then  $
    ypnl=(ysize/total(ysize))*yspace  else $
    ypnl=ypnl_relsiz*yspace
endelse  

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
