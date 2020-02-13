;modified to alter top color after call to loadct.  (R. J. Fitzenreiter, 3-1-93)

; $Id: xloadct.pro,v 1.4 1993/11/16 23:52:25 dave Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
PRO XLCT_PSAVE			;Save/Restore our plotting state.
;  Swaps our state with the current state each time its called.

COMMON xloadct_com, r0, g0, b0, tfun, state, filename, cps, psave, pnt, $
	top, bot, silent, chop, lock, g_lbl, vbot, vtop, g_slider, $
	gamma, color, use_values, ncolors

tmp = { xlct_psave, win: !d.window, x: !x.s, y: !y.s }

if psave.win ne -1 then wset, psave.win
!x.s = psave.x
!y.s = psave.y
psave = tmp
end

PRO xlct_draw_cps, i, c
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
COMMON xloadct_com, r0, g0, b0, tfun, state, filename, cps, psave, pnt, $
	top, bot, silent, chop, lock, g_lbl, vbot, vtop, g_slider, $
	gamma, color, use_values, ncolors


tc = color
if n_elements(c) gt 0 then begin
	tc = c
	if c ne 0 then color = c
	endif

if i(0) eq -1 then j = indgen(n_elements(cps)) else j = i

plots, cps(j), tfun(j), /noclip, color = tc
plots, cps(j), tfun(j), /noclip, psym=6, color = tc
end

PRO xlct_transfer
COMMON xloadct_com, r0, g0, b0, tfun, state, filename, cps, psave, pnt, $
	top, bot, silent, chop, lock, g_lbl, vbot, vtop, g_slider, $
	gamma, color, use_values, ncolors


COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

l = lonarr(ncolors)		;Subscripts
m = n_elements(cps)
for i=0, m-2 do begin
	n = cps(i+1)-cps(i)		;Interval
	b = (tfun(i+1)-tfun(i))/float(n)
	l(cps(i)) = findgen(n) * b + tfun(i)
	endfor
l(ncolors-1) = tfun(m-1)		;Last point
if use_values then begin
  r_curr = l(r_orig)
  g_curr = l(g_orig)
  b_curr = l(b_orig)
endif else begin
  r_curr = r_orig(l)
  g_curr = g_orig(l)
  b_curr = b_orig(l)
endelse

tvlct, r_curr, g_curr, b_curr
end

PRO xloadct_event, event
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
COMMON xloadct_com, r0, g0, b0, tfun, state, filename, cps, psave, pnt, $
	top, bot, silent, chop, lock, g_lbl, vbot, vtop, g_slider, $
	gamma, color, use_values, ncolors


IF event.id eq state.draw THEN BEGIN	;** PROCESS DRAWABLE EVENTS **
	if event.press ne 0 then begin		;Pressed button?
		dmin = 1.0e8		;Find closest control pnt
		xlct_psave		;Remove old
		p = convert_coord(event.x, event.y, /TO_DATA, /DEVICE)
		xlct_psave		;Restore old
		x = fix(p(0))
		y = fix(p(1))
		for i=0, n_elements(cps)-1 do begin
			d = (p(0)-cps(i))^2 + (p(1)-tfun(i))^2  ; dist ^ 2
			if d lt dmin then begin
				dmin = d
				pnt = i
				endif
			endfor
		return
		endif
	if event.release ne 0 then begin	;Released button?
		pnt = -1
		return
		endif
	if pnt lt 0 then return			;Don't care here...

	xlct_psave				;Remove old
	p = convert_coord(event.x, event.y, /TO_DATA, /DEVICE)	;Coord of mouse
	n = ncolors -1		;Into range....
	m = n_elements(cps)-1
	x = fix(p(0)) > 0 < n
	if pnt eq 0 then x = 0 else $		;1st & last are fixed
	if pnt eq m then x = n else $
	x = x > (cps(pnt-1) + 1) < (cps(pnt+1)-1)  ;Others must be between

	if pnt eq 0 then xlct_draw_cps, [0, 1],0 $  ;Erase old segment
	else if pnt eq m then xlct_draw_cps, [m-1, m],0 $
	else xlct_draw_cps, [pnt-1, pnt, pnt+1],0
	cps(pnt) = x
	tfun(pnt) = fix(p(1)) > 0 < n
	xlct_transfer

	b = r_curr * .3 + g_curr * .586 + b_curr * .114 ;Ntsc colors
	c = max(abs(b-b(0)), j)  ; *** J is color index furthest from 0

	if pnt eq 0 then xlct_draw_cps, [0, 1], j $
	else if pnt eq m then xlct_draw_cps, [m-1, m], j $
	else xlct_draw_cps, [pnt-1, pnt, pnt+1], j

	xlct_psave		;Remove old
	return
	ENDIF

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

abstop = NCOLORS -1

if event.id eq state.name_list then begin
	LOADCT, silent=silent, event.index, FILE=filename, NCOLORS=ncolors
;
;set top color (white in GSFC and IOWA color tables) to next to top color (RJF)
        ;r_orig(n_elements(r_orig)-1)=r_orig(n_elements(r_orig)-2)
        ;g_orig(n_elements(g_orig)-1)=g_orig(n_elements(r_orig)-2)
        ;b_orig(n_elements(b_orig)-1)=b_orig(n_elements(r_orig)-2)
        r_orig(n_elements(r_orig)-1)=255
        g_orig(n_elements(g_orig)-1)=255
        b_orig(n_elements(b_orig)-1)=255
        tvlct,r_orig,g_orig,b_orig


        tvlct,r_orig,g_orig,b_orig
;
       	goto, set_gamma
	ENDIF

CASE eventval OF
    "TOP":    BEGIN
		WIDGET_CONTROL, top, GET_VALUE = vtop
                if lock ne 0 then begin
                        vbot = (vtop - lock) > 0 < 100
                        widget_control, bot, SET_VALUE=vbot
                        endif
                GOTO, set_gamma
              END

    "BOTTOM": BEGIN
		WIDGET_CONTROL, bot, GET_value = vbot
                if lock ne 0 then begin
                        vtop = (vbot + lock) > 0 < 100
                        widget_control, top, SET_VALUE=vtop
                        ENDIF
   set_gamma:
	if use_values then nc = 256 else nc = ncolors
	s = (nc-1)/100.
	x0 = vbot * s
	x1 = vtop * s
	if x0 ne x1 then s = (nc-1.0)/(x1 - x0) else s = 1.0
	int = -s * x0
	if gamma eq 1.0 then s = round(findgen(nc) * s + int > 0.0) $
	else s = ((findgen(nc) * (s/nc) + (int/nc) > 0.0) ^ gamma) * nc
	if chop ne 0 then begin
	    too_high = where(s ge nc, n)
	    if n gt 0 then s(too_high) = 0L
	    endif
	if use_values then begin
	    s = s < 255L
	    r_curr = s(r_orig)
	    g_curr = s(g_orig)
	    b_curr = s(b_orig)
	endif else begin
	    r_curr = r_orig(s)
	    g_curr = g_orig(s)
	    b_curr = b_orig(s)
 	endelse
	tvlct, r_curr, g_curr, b_curr
	ENDCASE

    "GAMMA": BEGIN
                WIDGET_CONTROL, g_slider, GET_VALUE = gamma
                gamma = 10^((gamma/50.) - 1)
                WIDGET_CONTROL, g_lbl, SET_VALUE = $
			STRING(gamma, format='(f6.3)')
		goto, set_gamma
             ENDCASE

    "GANG" : IF event.value eq 0 then lock = 0 else lock = vtop - vbot

    "CHOP" : BEGIN
        chop = event.value
        goto, set_gamma         ;And redraw
        ENDCASE

    "VALUES": BEGIN
	use_values = event.value
	ENDCASE

    "HELP" : XDisplayFile, FILEPATH("xloadct.txt", subdir='help'), $
                TITLE = "Xloadct Help", $
                GROUP = event.top, $
                WIDTH = 55, $
                HEIGHT = 16

    "RESTORE" : BEGIN                   ;Restore the original tables
        r_curr = (r_orig = r0)
        g_curr = (g_orig = g0)
        b_curr = (b_orig = b0)
	tvlct, r_curr, g_curr, b_curr
        ENDCASE

    "OVERWRITE" : BEGIN                 ;overwrite original tables
	r0 = (r_orig = r_curr)
	g0 = (g_orig = g_curr)
	b0 = (b_orig = b_curr)
    reset_all:
        WIDGET_CONTROL, top, SET_VALUE = 100
        WIDGET_CONTROL, bot, SET_VALUE = 0
        WIDGET_CONTROL, g_slider, SET_VALUE = 50
	vbot = 0
	vtop = 100
	gamma = 1.0
	GOTO, set_gamma
	ENDCASE

    "REVERSE" : BEGIN                   ;Reverse the table
        r_orig = reverse(r_orig)
        g_orig = reverse(g_orig)
        b_orig = reverse(b_orig)
        goto, set_gamma                 ;And redraw
        ENDCASE

    "DONE": BEGIN
        WIDGET_CONTROL, event.top, /DESTROY
        r0 = 0 & g0 = 0 & b0 = 0  ;Free common
        ENDCASE

    "NEWBASE": BEGIN
	mode = event.value
	b = ([0, 0, 1])(mode)		;Top base to map: 0 or 1.
	for i=0,1 do WIDGET_CONTROL, state.bases(i), MAP=i eq b
	if b eq 0 then begin		;table or option mode?
	   b = ([2,3,0])(mode)		;bottom base to map (mode eq 0 or 1)
	   for i=2,3 do WIDGET_CONTROL, state.bases(i), MAP=i eq b
	   endif
	if mode eq 2 then begin
	    reset_all = 1
	    xlct_psave			;Save old state
	    plot, [0, ncolors-1], [0, ncolors-1], xstyle=3, $
		ystyle=3, xmargin = [1,1], ymargin=[1,1], ticklen = -0.03, $
		/NODATA, $
		xtickname = replicate(' ', 10), ytickname = replicate(' ', 10)
	    goto, interp_cps
	    endif
	
	ENDCASE

    "TFUNR": BEGIN
     reset_tfun:
	xlct_psave
	xlct_draw_cps, -1, 0	;Erase all
	tfun = cps		;Linear ramp
	goto, interp_cps
	ENDCASE

    "REMCP": BEGIN
	n = n_elements(cps)
	if n gt 2 then begin
	  xlct_psave
	  xlct_draw_cps, -1, 0
	  igap = 0
	  for i=0, n-2 do $
		if (cps(i+1) - cps(i)) lt (cps(igap+1)-cps(igap)) then $
			igap = i
	  keep = where(indgen(n) ne (igap > 1))
	  cps = cps(keep)
	  tfun = tfun(keep)
	  goto, interp_cps
	  ENDIF
	ENDCASE
    "ADDCP": BEGIN
	xlct_psave
	xlct_draw_cps, -1, 0
	igap = 0			;Find largest gap
	for i=0, n_elements(cps)-2 do $
		if (cps(i+1) - cps(i)) gt (cps(igap+1)-cps(igap)) then $
			igap = i
	cps = [ cps(0:igap), (cps(igap)+cps(igap+1))/2, cps(igap+1:*)]
	tfun = [ tfun(0:igap), (tfun(igap)+tfun(igap+1))/2, tfun(igap+1:*)]
      interp_cps:  xlct_draw_cps, -1, color  ;Redraw new
	xlct_transfer
	xlct_psave		;Restore old points
	if n_elements(reset_all) then goto, reset_all
	ENDCASE
ENDCASE

END


;+
; NAME:
;       XLOADCT
; PURPOSE:
;       A graphical interface to the LOADCT user library procedure.
;       XLOADCT displays the current color map and provides
;       an array of buttons, one per availible predefined color
;       table. Using the mouse to press these buttons causes
;       the corresponding color map to be loaded.
; CATEGORY:
;       Widgets
; CALLING SEQUENCE:
;       XLOADCT
; INPUTS:
;       None.
; KEYWORDS:
;	FILE:	If this keyword is set, the file by the given name is used
;		instead of the file colors1.tbl in the IDL directory.  This
;		allows multiple IDL users to have their own color table file.
;       GROUP = The widget ID of the widget that calls XLoadct.  When 
;               this ID is specified, a death of the caller results in a 
;               death of XLoadct
;	NCOLORS = number of colors to use.  Use color indices from 0
;		to the smaller of !D.TABLE_SIZE-1 and NCOLORS-1.
;		Default = !D.TABLE_SIZE = all available colors.
;       PICK_ONE - Normally, XLOADCT remains running until the user
;               presses the "QUIT" button. If PICK_ONE is present and 
;               non-zero, the "QUIT" button is not displayed, and
;               XLOADCT quits after a single selection.
;       SILENT - Normally, no informational message is printed when
;               a color map is loaded. If this keyword is present and
;               zero, this message is printed.
;	USE_CURRENT: If set, use the current color tables, regardless of
;		the contents of the COMMON block COLORS.
; OUTPUTS:
;       None.
; COMMON BLOCKS:
;       None.
; SIDE EFFECTS:
;       One of the predefined color maps may be loaded.
; RESTRICTIONS:
;       This routine uses the LOADCT user library procedure to
;       do the actual work.
; MODIFICATION HISTORY:
;       24, August, 1990, Written by AB, RSI.
;       March 1, 1992  Mark Rivers added Reverse Table to options menu.
;	7/92, DMS, Added new color tables (allows more than 16).
;	9/92, ACY, Add FILE keyword.
;-

PRO XLoadct, PICK_ONE=PICK_ONE, SILENT=silent_f, GROUP = GROUP, FILE=file, $
	USE_CURRENT = use_current, NCOLORS = nc

COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
COMMON xloadct_com, r0, g0, b0, tfun, state, filename, cps, psave, pnt, $
	top, bot, silent, chop, lock, g_lbl, vbot, vtop, g_slider, $
	gamma, color, use_values, ncolors

print,'xloadct mine'

IF(XRegistered("xloadct") NE 0) THEN return

values_button = lonarr(2)

IF KEYWORD_SET(SILENT_f) THEN silent = SILENT_F ELSE silent = 1

IF N_ELEMENTS(file) GT 0 THEN filename = file

siz = 256			;Basic width of tool
names = 0
LOADCT, GET_NAMES = names, FILE = file	;Get table names
w_height = 50                   ;Height of ramp
cur_win = !D.WINDOW
lock = 0
chop = 0
vbot = 0
vtop = 100
gamma = 1.0
use_values=0


; Bases:
;  0 = slider base  (stretch bottom, stretch top, gamma)
;  1 = transfer function drawable + buttons
;  2 = color table list
;  3 = options base  (sliders. top, stretch)

state = { bases: lonarr(4), draw: 0L, name_list: 0L }

base = WIDGET_BASE(TITLE="XLoadct", /COLUMN)


show = WIDGET_DRAW(base, YSIZE=w_height, XSIZE=siz, /FRAME, RETAIN = 2)
IF(NOT(KEYWORD_SET(pick_one))) THEN BEGIN
  junk = WIDGET_BASE(base, /ROW)
  done = WIDGET_BUTTON(junk, VALUE=' Done ', UVALUE = "DONE")
  junk1 = WIDGET_BUTTON(junk, VALUE=' Help ', UVALUE = "HELP")

  junk = CW_BGROUP(base, /ROW, /EXCLUSIVE, /NO_REL, $
	['Tables', 'Options', 'Function'], $
	UVALUE='NEWBASE', SET_VALUE=0)

  junk = widget_base(base)
  for i=0,1 do state.bases(i) = WIDGET_BASE(junk, /COLUMN)

  sbase=WIDGET_BASE(state.bases(0), /COLUMN)
  bot = WIDGET_SLIDER(sbase, TITLE = "Stretch Bottom", MINIMUM = 0, $
        MAXIMUM = 100, VALUE = 0, /DRAG, UVALUE = "BOTTOM", xsize=siz)
  top = WIDGET_SLIDER(sbase, TITLE = "Stretch Top", MINIMUM = 0, $
        MAXIMUM = 100, VALUE = 100, /DRAG, UVALUE = "TOP", xsize=siz)
  g_lbl = WIDGET_LABEL(sbase, VALUE = STRING(1.0))
  g_slider = WIDGET_slider(sbase, TITLE = "Gamma Correction", $
        MINIMUM = 0, MAXIMUM = 100, VALUE = 50, UVALUE = "GAMMA", $
        /SUPPRESS_VALUE, /DRAG, xsize=siz)
ENDIF ELSE BEGIN
  done = 0
ENDELSE

junk = WIDGET_BASE(sbase)
for i=2,3 do state.bases(i) = WIDGET_BASE(junk, /COLUMN)
DEVICE, GET_SCREEN = junk
if junk(1) le 768 then junk = 8 else junk = 16
state.name_list = WIDGET_LIST(state.bases(2), VALUE = names, ysize = junk)


;		Drawable for transfer function

junk = WIDGET_BASE(state.bases(1), /COLUMN, /FRAME)
junk1 = WIDGET_BUTTON(junk, VALUE = 'Reset Transfer Function', $
	UVALUE='TFUNR')
junk1 = WIDGET_BUTTON(junk, VALUE='Add Control Point', UVALUE='ADDCP')
junk1 = WIDGET_BUTTON(junk, VALUE='Remove Control Point', UVALUE='REMCP')

state.draw = WIDGET_DRAW(state.bases(1), xsize = siz, ysize = siz, $
	/BUTTON_EVENTS, /MOTION_EVENTS)


 opt_id = state.bases(3)
 junk = CW_BGROUP(opt_id, /ROW, LABEL_LEFT='Sliders:', /EXCLUSIVE, /NO_REL, $
		['Independent', 'Gang'], UVALUE='GANG', SET_VALUE=lock)
 junk = CW_BGROUP(opt_id, /ROW, LABEL_LEFT = 'Top:',  /EXCLUSIVE, /NO_REL, $
		['Clip', 'Chop'], SET_VALUE=chop, UVALUE='CHOP')
 junk = CW_BGROUP(opt_id, /ROW, LABEL_LEFT='Stretch:',  /EXCLUSIVE, /NO_REL, $
		['Indices', 'Intensity'], UVALUE='VALUES', $
		SET_VALUE=use_values)
 junk = WIDGET_BUTTON(opt_id, VALUE='Reverse Table', $
                UVALUE="REVERSE", /NO_REL)
 junk = WIDGET_BUTTON(opt_id, VALUE='REPLACE Original Table', $
		UVALUE = "OVERWRITE", /NO_REL)
 junk = WIDGET_BUTTON(opt_id, VALUE='RESTORE Original Table', $
                UVALUE="RESTORE", /NO_REL)

WIDGET_CONTROL, state.bases(1), MAP=0	;Tfun is not visible
WIDGET_CONTROL, state.bases(3), MAP=0	;options are not visible

WIDGET_CONTROL, base, /REALIZE
WIDGET_CONTROL, state.draw, GET_VALUE=tmp

wset, tmp			;Initial graph
ncolors = !d.table_size
if n_elements(nc) gt 0 then ncolors = ncolors < nc

plot, [0, ncolors-1], [0, ncolors-1], xstyle=3, ystyle=3, $
	xmargin = [1,1], ymargin=[1,1], ticklen = -0.03, /NODATA
psave = { xlct_psave, win: tmp, x: !x.s, y: !y.s }   ;Our initial state

			;If no common, use current colors
IF KEYWORD_SET(use_current) or N_ELEMENTS(r_orig) LE 0 THEN BEGIN
	TVLCT, r_orig, g_orig, b_orig, /GET
	r_curr = r_orig
	b_curr = b_orig
	g_curr = g_orig
	ENDIF

r0 = r_curr             ;Save original colors
g0 = g_curr
b0 = b_curr
color = ncolors-1
cps = [0, color]
tfun = cps
pnt = -1

WIDGET_CONTROL, show, GET_VALUE=show_win
WSET, show_win
TVSCL, BYTSCL(INDGEN(siz) # REPLICATE(1, w_height), top = ncolors-1)
IF(cur_win NE -1) THEN WSET, cur_win

XManager, "xloadct", base, GROUP_LEADER = GROUP

END
