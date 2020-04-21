
pro make_nssdc_summary_plots_coords, year

s_year = string(year, format = '(I4)')
restore, '/crater/observatories/wind/swe/nssdc_idl/wi_swefc_apbimax.'+s_year+'.idl'

; ****** Time series generator ******
set_plot, 'ps'
device, /encapsulate, filename = '~/temp/CDF_validation_coords_' + s_year + '.eps', $
  /inches, ysize = 11, xsize = 8.5, /color

loadct, 39
;window, xsize = 850, ysize = 1100
!p.multi = [0, 1, 2]
!y.margin = [8, 1]

plot, xgse, ygse, xtitle = 'xgse', ytitle = 'ygse'
plot, ygse, zgse, xtitle = 'ygse', ytitle = 'zgse'

stop

device, /close
set_plot, 'x'

end



