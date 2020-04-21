
pro make_nssdc_summary_plots_HeOK, year

year = 1994
s_year = string(year, format = '(I4)')
restore, '/crater/observatories/wind/swe/nssdc_idl/wi_swefc_apbimax.'+s_year+'.idl'
hold_np = np
hold_vxp = vxp
hold_wtrace = wtrace
hold_fit = fit
hold_na = na

for year = 1995, 2011 do begin &$
  s_year = string(year, format = '(I4)') &$
  restore, '/crater/observatories/wind/swe/nssdc_idl/wi_swefc_apbimax.'+s_year+'.idl' &$
  hold_np = [hold_np, np] &$
  hold_vxp = [hold_vxp,vxp] &$
  hold_wtrace = [hold_wtrace,wtrace] &$
  hold_fit = [hold_fit, fit] &$
  hold_na = [hold_na, na] &$
  endfor

np= hold_np
vxp=hold_vxp
wtrace=hold_wtrace
fit=hold_fit
na=hold_na


; ***** histogram generator ********
;device, decomposed = 0
;loadct, 39

; Make histograms of the data by fit flag
tk = where(na gt 100 or na lt 0)
; check that all crazy data are set to fill value
flagval = max(na)
if max(na[tk]) eq min(na[tk] ) then print, 'Fills are all good'

na_plot = 100.*na/np
na_plot[tk] = 0 - fit[tk]
plot, doy, na_plot
ntot = n_elements(na)



; we want to make plots of num_good/total as a function of vp, wp, and
; np

; ******** BY SPEED ********

xvar = abs(vxp)
nbins = 20.
x0 = min(xvar)
fill = max(xvar)
xm = 1200;max(xvar[where(xvar ne fill)])
xbins = x0+((xm-x0)/nbins)*findgen(nbins)
nok = fltarr(nbins)
ngood = fltarr(nbins)
ntot = fltarr(nbins)
for i = 0, n_elements(xbins) do begin &$
  tkall = where(xvar ge xbins[i] and xvar lt xbins[i+1], tempntot) &$
  ntot[i] = tempntot &$
  tkok = where(xvar ge xbins[i] and xvar lt xbins[i+1] and na gt 0 and na lt 10, tempnok) &$
  tkgood = where(xvar ge xbins[i] and xvar lt xbins[i+1] and fit eq 10, tempngood) &$
  nok[i] = tempnok &$
  ngood[i] = tempngood &$
  endfor

!p.multi = [0, 2, 1]
plot, xbins, ngood/ntot, psym = 10, ytitle = 'Ideal alpha fit', xtitle = 'v!dp!n', charsize = 2
plot, xbins, nok/ntot, psym = 10, ytitle = 'Acceptable n!da!n estimate', xtitle = 'v!dp!n', charsize = 2


; ******** BY DENSITY ********

xvar = abs(np)
nbins = 20.
x0 = min(xvar)
fill = max(xvar)
xm = 25
xbins = x0+((xm-x0)/nbins)*findgen(nbins)
nok = fltarr(nbins)
ngood = fltarr(nbins)
ntot = fltarr(nbins)
for i = 0, n_elements(xbins) do begin &$
  tkall = where(xvar ge xbins[i] and xvar lt xbins[i+1], tempntot) &$
  ntot[i] = tempntot &$
  tkok = where(xvar ge xbins[i] and xvar lt xbins[i+1] and na gt 0 and na lt 10, tempnok) &$
  tkgood = where(xvar ge xbins[i] and xvar lt xbins[i+1] and fit eq 10, tempngood) &$
  nok[i] = tempnok &$
  ngood[i] = tempngood &$
  endfor


!p.multi = [0, 2, 1]
plot, xbins, ngood/ntot, psym = 10, ytitle = 'Ideal alpha fit', xtitle = 'n!dp!n', charsize = 2
plot, xbins, nok/ntot, psym = 10, ytitle = 'Acceptable n!da!n estimate', xtitle = 'n!dp!n', charsize = 2


; ******** BY TEMPERATURE ********

xvar = abs(wtrace)
nbins = 20.
x0 = min(xvar)
fill = max(xvar)
xm =  150
xbins = x0+((xm-x0)/nbins)*findgen(nbins)
nok = fltarr(nbins)
ngood = fltarr(nbins)
ntot = fltarr(nbins)
for i = 0, n_elements(xbins) do begin &$
  tkall = where(xvar ge xbins[i] and xvar lt xbins[i+1], tempntot) &$
  ntot[i] = tempntot &$
  tkok = where(xvar ge xbins[i] and xvar lt xbins[i+1] and na gt 0 and na lt 10, tempnok) &$
  tkgood = where(xvar ge xbins[i] and xvar lt xbins[i+1] and fit eq 10, tempngood) &$
  nok[i] = tempnok &$
  ngood[i] = tempngood &$
  endfor

!p.multi = [0, 2, 1]
plot, xbins, ngood/ntot, psym = 10, ytitle = 'Ideal alpha fit', xtitle = 'w!dp!n', charsize = 2
plot, xbins, nok/ntot, psym = 10, ytitle = 'Acceptable n!da!n estimate', xtitle = 'w!dp!n', charsize = 2


; ******** BY FLUX ********

xvar = abs(np*vxp)
nbins = 20.
x0 = min(xvar)
fill = max(xvar)
xm =  20000.
xbins = x0+((xm-x0)/nbins)*findgen(nbins)
nok = fltarr(nbins)
ngood = fltarr(nbins)
ntot = fltarr(nbins)
for i = 0, n_elements(xbins) do begin &$
  tkall = where(xvar ge xbins[i] and xvar lt xbins[i+1], tempntot) &$
  ntot[i] = tempntot &$
  tkok = where(xvar ge xbins[i] and xvar lt xbins[i+1] and na gt 0 and na lt 10, tempnok) &$
  tkgood = where(xvar ge xbins[i] and xvar lt xbins[i+1] and fit eq 10, tempngood) &$
  nok[i] = tempnok &$
  ngood[i] = tempngood &$
  endfor

!p.multi = [0, 2, 1]
plot, xbins, ngood/ntot, psym = 10, ytitle = 'Ideal alpha fit', xtitle = 'p-flux', charsize = 2
plot, xbins, nok/ntot, psym = 10, ytitle = 'Acceptable n!da!n estimate', xtitle = 'p_flux', charsize = 2


; ******** BY mach number ********

xvar = abs(vxp/wtrace)
nbins = 20.
x0 = min(xvar)
fill = max(xvar)
xm =  200
xbins = x0+((xm-x0)/nbins)*findgen(nbins)
nok = fltarr(nbins)
ngood = fltarr(nbins)
ntot = fltarr(nbins)
for i = 0, n_elements(xbins) do begin &$
  tkall = where(xvar ge xbins[i] and xvar lt xbins[i+1], tempntot) &$
  ntot[i] = tempntot &$
  tkok = where(xvar ge xbins[i] and xvar lt xbins[i+1] and na gt 0 and na lt 10, tempnok) &$
  tkgood = where(xvar ge xbins[i] and xvar lt xbins[i+1] and fit eq 10, tempngood) &$
  nok[i] = tempnok &$
  ngood[i] = tempngood &$
  endfor

!p.multi = [0, 2, 1]
plot, xbins, ngood/ntot, psym = 10, ytitle = 'Ideal alpha fit', xtitle = 'p-mach', charsize = 2
plot, xbins, nok/ntot, psym = 10, ytitle = 'Acceptable n!da!n estimate', xtitle = 'p_mach', charsize = 2



; ******** 2D by DENSITY AND TEMPERATURE ********

xvar = abs(wtrace)
yvar = abs(np)
nbins = 20.
x0 = min(xvar)
xfill = max(xvar)
xm =  150
y0 = min(yvar)
yfill = max(yvar)
ym =  25
xbins = x0+((xm-x0)/nbins)*findgen(nbins)
ybins = y0+((ym-y0)/nbins)*findgen(nbins)

nok = fltarr(nbins, nbins)
ngood = fltarr(nbins, nbins)
ntot = fltarr(nbins, nbins)
for i = 0, n_elements(xbins)-2 do begin &$
  for j = 0, n_elements(ybins)-2 do begin &$
    tkall = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1], tempntot) &$
  ntot[i,j] = tempntot &$
  tkok = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1] and $
               na gt 0 and na lt 10, tempnok) &$
  tkgood = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1] $
                 and fit eq 10, tempngood) &$
  nok[i,j] = tempnok &$
  ngood[i,j] = tempngood &$
  endfor &$
  endfor

contour, smooth(ngood/ntot, 2), xbins, ybins, xtitle = 'w!dp!n', ytitle = 'n!dp!n', charsize = 2, /follow, c_charsize = 2



; ******** 2D by SPEED AND DENSITY ********

xvar = abs(vxp)
yvar = abs(np)
nbins = 20.
x0 = 150
xfill = max(xvar)
xm =  1200
y0 = min(yvar)
yfill = max(yvar)
ym =  25
xbins = x0+((xm-x0)/nbins)*findgen(nbins)
ybins = y0+((ym-y0)/nbins)*findgen(nbins)

nok = fltarr(nbins, nbins)
ngood = fltarr(nbins, nbins)
ntot = fltarr(nbins, nbins)
for i = 0, n_elements(xbins)-2 do begin &$
  for j = 0, n_elements(ybins)-2 do begin &$
    tkall = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1], tempntot) &$
  ntot[i,j] = tempntot &$
  tkok = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1] and $
               na gt 0 and na lt 10, tempnok) &$
  tkgood = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1] $
                 and fit eq 10, tempngood) &$
  nok[i,j] = tempnok &$
  ngood[i,j] = tempngood &$
  endfor &$
  endfor

contour, smooth(ngood/ntot, 2), xbins, ybins, xtitle = 'v!dp!n', ytitle = 'n!dp!n', charsize = 2, /follow, c_charsize = 2




; ******** 2D by MACH NUMBER AND DENSITY ********

xvar = abs(vxp/wtrace)
yvar = abs(np)
nbins = 25.
x0 = 0
xfill = max(xvar)
xm =  40.
y0 = 0.
yfill = max(yvar)
ym =  25.
xbins = x0+((xm-x0)/nbins)*findgen(nbins)
ybins = y0+((ym-y0)/nbins)*findgen(nbins)

nok = fltarr(nbins, nbins)
ngood = fltarr(nbins, nbins)
ntot = fltarr(nbins, nbins)
for i = 0, n_elements(xbins)-2 do begin &$
  for j = 0, n_elements(ybins)-2 do begin &$
    tkall = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1], tempntot) &$
  ntot[i,j] = tempntot &$
  tkok = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1] and $
               na gt 0 and na lt 10, tempnok) &$
  tkgood = where(xvar ge xbins[i] and xvar lt xbins[i+1] and yvar ge ybins[j] and yvar lt ybins[j+1] $
                 and fit eq 10, tempngood) &$
  nok[i,j] = tempnok &$
  ngood[i,j] = tempngood &$
  endfor &$
  endfor

contour, smooth(ngood/(ntot+1.), 2), xbins, ybins, xtitle = 'bulk flow mach number', ytitle = 'n!dp!n', charsize = 2, /follow, c_charsize = 2, xrange = [0, 25], yrange = [0, 10]

contour, ntot/max(ntot), xbins, ybins, /overplot, color = fsc_color('blue'), /follow, c_charsize = 2



contour, smooth(nok/(ntot+1.), 2), xbins, ybins, xtitle = 'bulk flow mach number', ytitle = 'n!dp!n', charsize = 2, /follow, c_charsize = 2, xrange = [0, 25], yrange = [0, 10]

contour, ntot/max(ntot), xbins, ybins, /overplot, color = fsc_color('blue'), /follow, c_charsize = 2

; let's convert ntot to a cumulative distribution
ztot = total(ntot)
cumul = total(ntot[sort(ntot)], /cumul)/total(ntot)
xcumul = ntot[sort(ntot)]
plot, xcumul, cumul


percentile = 0.*ntot
for i = 0, n_elements(xbins)-1 do begin &$
  for j = 0, n_elements(ybins)-1 do begin &$
  temp = where(ntot gt ntot[i,j], ntemp) &$
  if ntemp gt 0 then percentile[i,j] = 100.*total(ntot[temp])/ztot else percentile[i,j] = 100. &$
  endfor &$
endfor





set_plot, 'ps'
device, /encapsulate, /inches, xsize = 8.5, ysize = 11, /helvetica, /color, $
  filename = '~mstevens/temp.eps/'
!p.font = 0
!p.multi = 0
!x.margin = [7, 3]

contour, 100.*smooth(nok/(ntot+1.), 2), xbins, ybins, xtitle = 'bulk flow mach number', ytitle = 'n!dp!n', $
  charsize = 2, /follow, c_charsize = 2, xrange = [5, 30], yrange = [0, 20], levels = [15, 30, 45, 60, 75, 90], $
  color = fsc_color('black')

loadct, 0
contour, smooth(percentile, 2), xbins, ybins, /overplot, /follow, c_charsize = 4, /fill,$
  c_color = [150, 175, 200, 225, -1], levels = [0, 52, 84, 96, 99]

contour, smooth(percentile, 2), xbins, ybins, /overplot,  c_charsize = 2, /follow, levels = [52, 84, 96, 99], $
  c_label = [52, 84, 96, 99], c_annotation = ['52%', '84%', '96%', '99%'], color = fsc_color('grey'), $
  c_charthick = 2, c_thick = 2, c_linestyle = 0

contour, 100.*smooth(nok/(ntot+1.), 2), xbins, ybins, xtitle = 'bulk flow mach number', $
  ytitle = 'n!dp!n', charsize = 2, /follow, c_charsize = 2, xrange = [5, 30], yrange = [0, 20], $
  levels = [30, 60, 90], color = fsc_color('black'), c_thick = 3, c_charthick = 2, $
  c_annotation =  ['30%', '60%', '90%'], /downhill, /noerase, thick = 2

device, /close
set_plot, 'x'










;window, xsize = 850, ysize = 1100

set_plot, 'ps'
device, /encapsulate, filename = '~/temp/CDF_validation_Ahe_' + s_year + '_histograms.eps', $
  /inches, ysize = 11, xsize = 8.5, /color

!p.multi = [0, 3, 4]
!y.margin = [4, 4]
for i = 0, 11 do begin &$
  tk = where(fit eq i, ntk) &$
  percent = string(100.*ntk/ntot, format = '(F5.2)') &$
  if (ntk gt 1) then if (max(na_plot[tk]) ne min(na_plot[tk])) $
    then histoplot, na_plot[tk], charsize = 2, axiscolor = 'black', $
           title = percent + '% fit flag = ' + string(i, format = '(I2)'), xtitle = 'Ahe' $
    else plot, findgen(2), charsize = 2, $
           title = percent + '% fit flag = ' + string(i, format = '(I2)'), xtitle = 'Ahe' &$
endfor

device, /close
set_plot, 'x'


; ****** Time series generator ******
set_plot, 'ps'
device, /encapsulate, filename = '~/temp/CDF_validation_Ahe_' + s_year + '.eps', $
  /inches, ysize = 11, xsize = 8.5, /color

loadct, 39
;window, xsize = 850, ysize = 1100
!p.multi = [0, 1, 8]
!y.margin = [2, 1]
for i = 0, 7 do begin &$
  tk = where(8.*doy/366. ge i and 8.*doy/366. le (i+1) and na ne flagval, ntk) &$
  if ntk gt 1 then begin &$
   plot, doy[tk], na[tk], ytitle= 'Ahe', charsize = 2, psym = 3, $
   xrange = [min(doy[tk]), max(doy[tk])], xstyle =1, /yl  &$
   for j = 0, 11 do begin &$
     subtk = where(fit[tk] eq j, nsubtk) &$
     if nsubtk gt 0 then oplot, doy[tk[subtk]], na[tk[subtk]], color = j*24, psym = 3, thick = 3 &$
    endfor &$
  endif &$
endfor

device, /close
set_plot, 'x'

end



