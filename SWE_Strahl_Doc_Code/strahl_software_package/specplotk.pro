;+
; routine for making 2D plots, same basic syntax as specplot.pro (2D historgrams, etc.)
;
; :Categories:
;    Graphics
;    
; :History:
;     Change History::
;       Marc Pulupa's plot_2dhist adapted to make specplotk2.pro 10/3/2016 by Kosta Horaites
;
;-

;x,y are 1d arrays with nx, ny elements. z is 2d array with [nx,ny] elements

PRO specplotk, x, y, z, ISOTROPIC = isotropic,zlog = zlog, xrange = xrange, yrange = yrange, zrange = zrange, $
                         ztitle = ztitle, plot_title = plot_title, _EXTRA = ex

;                 xrange = xrange, yrange = yrange, $
;                 xnbins = xnbins, ynbins = ynbins, $
;                 xnorm = xnorm, ynorm = ynorm, $
;                 hls = hls_norm, $
;                 zrange = zrange, znrange = znrange, zlog = zlog, $
;                 xlog = xlog, ylog = ylog, isotropic = isotropic, $
;                 background = background, $
;                 percentile = percentile, min_counts = min_counts, $
;                 ztitle = ztitle, plot_title = plot_title, $
;                 xmoments = xmoments, ymoments = ymoments, $
;                 rix = rix, riy = riy, xtickformat=xtickformat, ytickformat=ytickformat,  _EXTRA = ex

IF not(keyword_set(xrange)) THEN  xrange = minmax(x, /nan)
IF not(keyword_set(yrange )) THEN  yrange = minmax(y, /nan)
IF not(keyword_set(zrange)) THEN zrange = minmax(z, /nan)

plot, xrange, yrange, isotropic = isotropic, /nodata, $
                      xrange = xrange, yrange = yrange, $
                      xstyle = 1, ystyle = 1, $
                      xticklen = -0.0125, yticklen = -0.0125, title = plot_title, _EXTRA = ex

plot, /nodata, /noerase, !X.CRANGE, !Y.CRANGE, xstyle = 5, ystyle = 5, $
      isotropic = isotropic

plot_pgrid, x, y, z,zrange = zrange, zlog = zlog, $
                 background = background, /cut_zrange

sharpcorners, thick = thick


if keyword_set(zlog) then begin

  ticknames = [STRCOMPRESS(STRING(alog10(zrange), format = '(F7.2)'), /REM)] 

endif else begin 

  zr = [zrange[0], zrange[1]]
  if data_type(zrange[0]) LT 3 then begin

     ticknames = [STRCOMPRESS(STRING(FIX((zrange))), $
                                 /REMOVE_ALL)]

  endif else begin

     ticknames = [STRCOMPRESS(STRING(zrange, format = '(E10.1)'), /REM)] 

     mz = max(abs(zrange), /nan)

     if (mz LT 1000.) and (mz GE 100.) then $
        ticknames = [STRCOMPRESS(STRING(zrange, format = '(I7)'), /REM)] 

     if (mz LT 100.) and (mz GE 10.) then $
        ticknames = [STRCOMPRESS(STRING(zrange, format = '(F7.0)'), /REM)] 

     if (mz LT 10.) and (mz GE 1.) then $
        ticknames = [STRCOMPRESS(STRING(zrange, format = '(F7.1)'), /REM)] 

     if (mz LT 1.) and (mz GE 0.1) then $
        ticknames = [STRCOMPRESS(STRING(zrange, format = '(F7.2)'), /REM)] 

  endelse

endelse

@colors_com

;if not keyword_set(pos) then begin $
   !P.POSITION = [0.155, 0.18, 0.9, 0.85]
;endif  else begin
;   !P.POSITION = pos
;endelse 

;cb_pos = [0.825, 0.18, 0.84, 0.925]
cb_pos = [!p.position[2]+0.02, !p.position[1], !p.position[2]+0.04, !p.position[3]]

pos_norm =  convert_coord(!X.CRANGE, !Y.CRANGE, /DATA, /TO_NORMAL)

IF KEYWORD_SET(isotropic) THEN BEGIN 
cb_pos[1] = pos_norm[1, 0]
cb_pos[3] = pos_norm[1, 1]
cb_pos[0] = pos_norm[0, 1] + 0.02
cb_pos[2] = pos_norm[0, 1] + 0.05
ENDIF ELSE BEGIN
cb_pos[1] = pos_norm[1, 0]
cb_pos[3] = pos_norm[1, 1]
ENDELSE

COLORBAR, POSITION = cb_pos, /VERTICAL, /RIGHT, DIVISIONS = 1, TICKNAMES = ticknames, $
                     TITLE = ztitle, NCOLORS = 255

END
