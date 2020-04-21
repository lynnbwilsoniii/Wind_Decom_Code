; @(#)showcal.pro  VERSION 1.2    7/28/94   16:13:38
; this reads a file produced by "record2" (MIT-extended Goddard code)
; or by unp_fitz.pro or unpickle_fitz
; and generates a plot of digitization count [ie, log(current)] in
; sequential order of measurement

; REQUIRED PARAMETERS:
;   cup number (1 or 2)
;   collector number (1=A, 2=B)
; OPTIONAL PARAMETERS:
;   spectrum - index of completed spectrum to plot
;		 0 = first spectrum in file (probably incomplete)
;		 1 = next spectrum, etc.
;
;   file -     input file name
;   screen -   scales fonts appropriately for Xterminals
;
; (NOT REALLY USEFUL:)
;   manual -   preform tweaking of limits by hand (not recommended =)
;   pos1 -     position of first spin to plot (manual mode only)
;   pos2 -     position of last spin to plot (manual mode only)
;
; (NOT EVEN A LITTLE USEFUL TO HUMANS:)
;   small -    plot at 1/4 size (used to do show4cal)
;   noinit -   do not set up MIT label, etc. (used to do show4cal)
;
; eg, "showcal,2,1,spectrum=3" plots data for cup 2, collector A, from
; the third completed_spectrum


FUNCTION calspind,unit
ind=intarr(3)
readf,unit,ind
RETURN,ind(0)
END

FUNCTION caldata,unit,cup
data=intarr(20)
line=intarr(4)
for i=0,19 do BEGIN
  readf,unit,line
  data(i) = line(cup)
ENDFOR
RETURN,data
END

FUNCTION callevel,unit
level=0
foo=''
readf,unit,foo				;read line where callevel might be
if (foo EQ '') then level=-1 $		;if foo is empty, level = -1
else reads,foo,level			;else make int level from string foo
RETURN,level				;return level
END

PRO calinitplot,cup,col,gpos,noe,cs	;cup,col,position,noerase,charsize
if (noe EQ 1) then $
  plot,[0,220],[0,4100],/nodata,/xstyle,/ystyle,position=gpos,noerase=noe, $
	charsize=cs,ytitle="log(current)",title=string(cup, $
	string(byte(col+64)),format='("cup ",I1,", collector ",A1)') $
else $
  plot,[0,220],[0,4100],/nodata,/xstyle,/ystyle,position=gpos, $
	charsize=cs,ytitle="log(current)",title=string(cup, $
	string(byte(col+64)),format='("cup ",I1,", collector ",A1)')
END

PRO caldataplot,unit,cup
spd=20.*calspind(unit)
data=caldata(unit,cup)

oplot,findgen(20)+spd,data
END

PRO calpos,unit,fpos
point_lun,unit,0
dd=''
for p=1,23*fpos do readf,unit,dd
END

PRO showcal,cup,col,sppoint,pos1=p1,pos2=p2,small=sm,file=fn,noinit=noi,$
	screen=scr, manual=man, spectrum=sp, show4=sho, unit=exunit
if NOT KEYWORD_SET(p1) then p1=0			;can't explain yet
if NOT KEYWORD_SET(p2) then p2=55			;can't explain yet
if NOT KEYWORD_SET(sp) then sp=0			;spectrum number
if NOT KEYWORD_SET(fn) then fn='caldata'		;input filename
if NOT KEYWORD_SET(noi) then noi=0 else noi=1		;noinit=don't erase graph 
if NOT KEYWORD_SET(sho) then sho=0 else sho=1		;sho will replace show4cal
if (sho) then sm = (cup = (col = 1))
if KEYWORD_SET(scr) then ft=-1 else ft=0		;chooses scaling for fonts
if KEYWORD_SET(scr) AND KEYWORD_SET(sm) then $		
  if (scr eq 2) then cs=.6 else cs=1. $
else cs=1.

!P.PSYM=3						;choose printing symbol
if NOT KEYWORD_SET(exunit) then openr,unit,fn,/get_lun $;open file if not done yet
else unit = exunit					;if open,unit=extern unit 

if (sho) then BEGIN					;if show all 4 plots
  for cupno = 0,3 do BEGIN				;for each cup and col
    if (cupno eq 0) then begin cup = 1 & col = 1 & end $
    else if (cupno eq 1) then begin col = 2 & noi = 1 & end $
    else if (cupno eq 2) then begin cup = 2 & col = 1 & end $
    else col = 2

    gpos=[.5*col-.42,1.02-.41*cup,.5*col-.02,1.35-.41*cup]    ;set graph geometry

    calinitplot,cup,col,gpos,noi,cs	;create the graph axes in the window

    if KEYWORD_SET(man) then BEGIN	;not worring about this option...
      file = ''
      calpos,unit,p1			;skip ahead fpos blocks in file
      readf,unit,file			;get filename
      for p=p1,p2 do caldataplot,unit,cupno	;plot the choosen data

    ENDIF else BEGIN

      if (cupno eq 0) then BEGIN	
        foo = ''				;dummy string
        file = ''				;source filename string
        utc1 = ''				;starting timetag
        utc2 = ''				; ending  timetag
        start = 0
        calpos,unit,0			;rewind to begining of file
        readf,unit,file			;get filename
        readf,unit,foo			;skip line
        readf,unit,foo			;skip line

	if (sppoint(0) eq 0) then BEGIN
	  point_lun,-unit,point		;find pointer to zeroth sp
	  sppoint(0) = point
	ENDIF

	index = rotate(where(where(sppoint) le sp),2)
	;print,'index: ',index,format='(a,40i3)'
	;index(0) is the highest sp for which the pointer is known below sp+1
	if (index(0) eq sp) then BEGIN
	  point_lun,unit,sppoint(sp)
	  index(0) = sp + 10			;prevent for loop from executing
	ENDIF

        for p=(index(0)>1),sp do BEGIN		;search for the sp'th spectrum
          for i=1,22 do if (not eof(unit)) then readf,unit,foo	;skip 1 fcblock
          start = start + 1				;count total lines skipped
          while (callevel(unit) NE 224) do BEGIN	;find start of spectrum p
	    for i=1,22 do if (not eof(unit)) then readf,unit,foo
	    start = start + 1				;keep that count correct
          ENDWHILE
	  point_lun,-unit,point			;get a pointer to the pth spectra
	  sppoint(p)=point			;set sppoint(sp# p) to point
        ENDFOR

      ENDIF else point_lun,unit,sppoint(sp)	;move file pointer to startpos
      ;print,'pos1 =',start

      p = start
      repeat BEGIN
        caldataplot,unit,cupno
        p = p + 1
        done = eof(unit)
        if NOT done then BEGIN
	    if (utc1 EQ '') then readf,unit,utc1 else readf,unit,utc2
	    done = (callevel(unit) EQ 224)
        ENDIF
      ENDREP until done
      ;print,'pos2 =',p-1
      if eof(unit) then print,'(EOF)'
    ENDELSE
  ENDFOR
  point_lun,-unit,point
  sppoint(sp+1) = point
  noi = 0
  ;print,sppoint,format='(8i8)'

ENDIF else BEGIN	;========endif: if (sho) then BEGIN ENDIF

  if NOT KEYWORD_SET(sm) then gpos=[.12,.2,.95,.95] $
  else gpos=[.5*col-.42,1.02-.41*cup,.5*col-.02,1.35-.41*cup]

  calinitplot,cup,col,gpos,noi,cs

  cupno = (2*(cup-1))+(col-1)
  if KEYWORD_SET(man) then BEGIN
    file = ''
    calpos,unit,p1
    readf,unit,file
    for p=p1,p2 do caldataplot,unit,cupno
  ENDIF else BEGIN
    start = 0
    foo = ''
    utc1 = ''
    utc2 = ''
    file = ''
    calpos,unit,0
    readf,unit,file
    readf,unit,foo
    readf,unit,foo
    for p=1,sp do BEGIN
      for i=1,22 do if (not eof(unit)) then readf,unit,foo
        start = start + 1
      while (callevel(unit) NE 224) do BEGIN
	for i=1,22 do if (not eof(unit)) then readf,unit,foo
	start = start + 1
      END
    END
    ;print,'pos1 =',start

    p = start
    repeat BEGIN
      caldataplot,unit,cupno
      p = p + 1
      done = eof(unit)
      if NOT done then BEGIN
	  if (utc1 EQ '') then readf,unit,utc1 else readf,unit,utc2
	  done = (callevel(unit) EQ 224)
      END
    END until done
    ;print,'pos2 =',p-1
    if eof(unit) then print,'(EOF)'
  END

ENDELSE ;=====endelse: if (sho) then BEGIN ENDIF else BEGIN ENDELSE

utc = string(utc1,' - ',strmid(utc2,strpos(utc2,',')+1,100))
if NOT KEYWORD_SET(noi) then BEGIN
  if KEYWORD_SET(scr) then mitlabel,ft,file,utc,/screen $
  else                     mitlabel,ft,file,utc
END

if NOT KEYWORD_SET(exunit) then free_lun,unit;if opened internally, close the file
!P.PSYM=0

END

