; @(#)shownicemod.pro  VERSION 1.2    7/28/94   16:13:41
; this reads a file produced by "record2" (MIT-extended Goddard code)
; and generates a plot of modulator monitor samples in sequential
; order of measurement

; OPTIONAL PARAMETERS:
;   skip -     number of FCFF blocks to be skipped at the beginning of file
;   number -   number of FCFF blocks to plot
;
;   filename - input file name
;   screen -   scales fonts appropriately for Xterminals
;
; ie, "shownicemod,skip=3,number=10" skips 3 initial blocks and plots the
; next 10.


PRO shownicemod,filename=fn,skip=pos,number=num
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

plot, cr(1,*),line=0,psym=10,pos=[.12,.61,.95,.93],/xstyle, $
	title='Faraday Cup Subsystem 1',ytitle='modulator monitor'
oplot,cr(2,*),line=0,psym=10
oplot,cr(0,*),line=1,psym=10

mitlabel,-scr,file,utc,screen=scr

plot, cr(3,*),line=0,psym=10,pos=[.12,.20,.95,.52],/xstyle,/noerase, $
	title='Faraday Cup Subsystem 2',ytitle='modulator monitor'
oplot,cr(4,*),line=0,psym=10
oplot,cr(0,*),line=1,psym=10
END
