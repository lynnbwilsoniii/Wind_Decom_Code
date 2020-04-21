
pro make_nssdc_summary_plots_Ahe, year

s_year = string(year, format = '(I4)')
restore, '/crater/observatories/wind/swe/nssdc_idl/wi_swefc_apbimax.'+s_year+'.idl'

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



