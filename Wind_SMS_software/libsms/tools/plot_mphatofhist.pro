; plot_mphatofhist.pro -- Plot MASS TOF spectra

; Use data file created by wmpha.pl

; Plot MASS TOF histogram in order to confirm that the PHA bytes *do
; not* need to be reversed in libsms::decodeMPHA (opposite of STICS,
; apparently, which is why this is troubling)

; jahf - 01Dec2005 - Results: Sure enough, running this with the bytes
; NOT reversed in decodeMPHA() creates a proper-looking
; plot. Reversing the bytes and then running this creates a plot that
; looks like crap.

pro plot_mphatofhist, datafile=datafile, data=d

sys_var, /save

;-- set up the ascii templates -------------------------------------

; name of template file for read_ascii
restore, 'plot_mphatofhist.save'

d = read_ascii(datafile, template=plottemplate, header=h, comment='#')
;HELP, d, /STRUCTURES

tof_hist = histogram(d.tof, binsize=4, locations=tof_bins)

;;-- Set up postscript file ---------------------------------------
file_parse, file=datafile, base=base
psfile = base + '.ps'
psplt, file=psfile, /color
loadct, 39
fix_ct_ends, black=0, white=1

!p.title = 'Wind/MASS TOF'
!p.charsize = 2.0
!p.charthick = 3.0
!p.thick = 6.0

!x.style = 1
!x.thick = 4.0
switch_x_labels, /save

!y.style = 1
!y.thick = 4.0
!y.ticks = 0
!y.tickname = [' ', strarr(10) + '']  ;; handles 11 ticks max (plenty)

;; set up plot
psym = 3                        ; dot

year_str = string(format='(I4)', d.year[0])
doyfr_str = string(format='(I4)', d.doyfr[0])

plot, tof_bins, tof_hist, $
  xtitle='tof bins', ytitle='counts', $ 
  subtitle='yeardoyfr: '+year_str+' '+doyfr_str


ficlose
 
return
end
