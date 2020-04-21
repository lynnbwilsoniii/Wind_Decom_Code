; %Z%%M%  VERSION %I%    %G%   %U%
; this reads a file produced by "record2" (MIT-extended Goddard code)
; and generates a plot of modulator monitor vs. ML

; OPTIONAL PARAMETERS:
;   skip -     number of FCFF blocks to be skipped at the beginning of file
;   number -   number of FCFF blocks to plot
;
;   filename - input file name
;   screen -   scales fonts appropriately for Xterminals
;
; ie, "showmod,skip=3,number=10" skips 3 initial blocks and plots the next 10.


PRO showmod,filename=fn,skip=pos,number=num,screen=scr
if NOT KEYWORD_SET(fn) then fn='moddata'
if NOT KEYWORD_SET(num) then num=6
if NOT KEYWORD_SET(scr) then scr=0
openr,unit,fn,/get_lun
file=''
readf,unit,file
utc=''
readf,unit,utc
if KEYWORD_SET(pos) then BEGIN
  cr=dblarr(5,11*pos)
  readf,unit,cr
END
cr=dblarr(5,11*num)
readf,unit,cr
free_lun,unit

sym=1

plot, [-2,50],[0,300],/nodata,/xstyle,position=[.07,.2,.48,.95], $
	title='Faraday Cup Subsystem 1',ytitle='modulator monitor'
mitlabel,-scr,file,utc,screen=scr
for i=0,num-1 do BEGIN
  L1=11*i
  L2=L1+10
  oplot,cr(0,L1:L2),cr(1,L1:L2),line=0,psym=sym
  oplot,cr(0,L1:L2),cr(2,L1:L2),line=0,psym=sym
END

plot, [-2,50],[0,300],/nodata,/xstyle,position=[.57,.2,.98,.95],/noerase, $
	title='Faraday Cup Subsystem 2',ytitle='modulator monitor'
for i=0,num-1 do BEGIN
  L1=11*i
  L2=L1+10
  oplot,cr(0,L1:L2),cr(3,L1:L2),line=0,psym=sym
  oplot,cr(0,L1:L2),cr(4,L1:L2),line=0,psym=sym
END

END
