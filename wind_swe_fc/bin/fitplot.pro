
; JTS 12/9/94  1st draft
; File to display the fitted currents.
; Read data from file named 'output_task.dat', produced by analysis code.

pro fitplot,file=file
if not keyword_set(file) then file = 'output_task.dat'

char_dummy  =  'dummy'
npts = 0
epochtime = 1.d3 ;initialize to a double precision

openr,1,file

readf, 1, char_dummy
readf, 1, char_dummy
readf, 1, a, epochtime,deltime, spin_per
year = long(epochtime/1000.)
day = long(epochtime - 1000*year)
fraction = epochtime - long(epochtime)
hour = long(fraction*24)
min = long((fraction*24 - hour)*60)
sec = ((fraction*24 - hour)*60 - min)*60.
;do 38 times
for i = 1, 38  do readf,1, char_dummy

readf,1, npts, n1angles, n2angles

energy = lonarr(npts)
cup = lonarr(npts)
angle = lonarr(npts)
current = fltarr(npts)
fit = fltarr(npts)

;do 4 times
for i = 1,4 do readf,1, char_dummy

for i = 0, npts-1 do begin 
   if not eof(1) then readf,1, a,b,c,d,e else print,'Help! end of file!'
   energy(i) = a
   cup(i) = b
   angle(i) = c 
   current(i) = d
   fit(i) = e
endfor

close,1

cup1min = min( angle( where(cup eq 1) ) )
cup1max = max( angle( where(cup eq 1) ) )
cup2min = min( angle( where(cup eq 2) ) )
cup2max = max( angle( where(cup eq 2) ) )

n_cup1 = n_elements( cup (where(cup eq 1) ) )
n_cup2 = n_elements( cup (where(cup eq 2) ) )
if n_cup1 le n_cup2 then n_x_axis = n_cup2
if n_cup1 gt n_cup2 then n_x_axis = n_cup1


ntot = 0


;xvert and yvert arrays will be used to draw vertical lines to 
;separate look angles.
yvert = fltarr(2)
xvert = fltarr(2)
yvert(0) = 5.e-14
yvert(1) = 5.e-8

;startplot

!p.multi = [0,1,2]

  i = cup1min
  plotcur1 = current(where( (angle eq i) and (cup eq 1) ) )
  n = n_elements(plotcur1)
  x = indgen(n) + 1 + ntot
  plot_io, x, plotcur1,xrange = [0,n_x_axis],yrange = [5.e-13,1.e-8],psym = 4, $
  ytitle = 'Current (amps)', $
   title = 'WIND SWE Faraday Cup 1     MIT',  charsiz = 1.5 
  
  oplot,x,fit(where( (angle eq i) and (cup eq 1) ) ), psym = 10 
  xvert(0) = x(n-1) + 0.5
  xvert(1) = x(n-1) + 0.5
  oplot,xvert,yvert,linestyle = 1 
  ntot = ntot + n

for i = cup1min+1, cup1max do begin
  plotcur1 = current(where( (angle eq i) and (cup eq 1) ) )
  n = n_elements(plotcur1)
  x = findgen(n) + 1 + ntot
  oplot, x, plotcur1, psym = 4
  oplot,x,fit(where( (angle eq i) and (cup eq 1) ) ),  psym = 10
  xvert(0) = x(n-1) + 0.5 
  xvert(1) = x(n-1) + 0.5 
  oplot,xvert,yvert,linestyle = 1
  ntot = ntot + n
endfor

ntot = 0
 
  i = cup2min
  plotcur2 = current(where( (angle eq i) and (cup eq 2) ) )
  n = n_elements(plotcur2)
  x = indgen(n) + 1 + ntot
  plot_io, x, plotcur2,xrange = [0,n_x_axis],yrange = [5.e-13,1.e-8],psym = 4, $
  ytitle = 'Current (amps)', $
   title = 'WIND SWE Faraday Cup 2     MIT',  charsiz = 1.5
  
  oplot,x,fit(where( (angle eq i) and (cup eq 2) ) ),  psym = 10
  xvert(0) = x(n-1) + 0.5
  xvert(1) = x(n-1) + 0.5
  oplot,xvert,yvert,linestyle = 1
  ntot = ntot + n
 
for i = cup2min+1, cup2max do begin
  plotcur2 = current(where( (angle eq i) and (cup eq 2) ) )
  n = n_elements(plotcur2)
  x = findgen(n) + 1 + ntot
  oplot, x, plotcur2, psym =4 
  oplot,x,fit(where( (angle eq i) and (cup eq 2) ) ),psym = 10 
  xvert(0) = x(n-1) + 0.5
  xvert(1) = x(n-1) + 0.5
  oplot,xvert,yvert,linestyle = 1
  ntot = ntot + n
endfor

!p.multi = [0,1,1]

end
