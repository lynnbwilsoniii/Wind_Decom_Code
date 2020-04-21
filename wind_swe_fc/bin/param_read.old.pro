; the variable filename must be assigned before calling this routine.
; filename = '/plasma/d9/wind/param_files/keep/1995001_param.dat'

spawn,'wc -l '+filename,string_lines
reads,string_lines,lines,format='(i)'
array_size = lines/4 
array_size = array_size(0)

char_dummy = 'dummy'

mode = lonarr(array_size)
year = lonarr(array_size)
day = lonarr(array_size)
sec = fltarr(array_size)
npts = lonarr(array_size)
chisqr = fltarr(array_size)
gvx = fltarr(array_size)
gvy = fltarr(array_size)
gvz = fltarr(array_size)
gdens = fltarr(array_size)
gtherm = fltarr(array_size)
pvx = fltarr(array_size)
pvy = fltarr(array_size)
pvz = fltarr(array_size)
pdens = fltarr(array_size)
ptherm = fltarr(array_size)
adens = fltarr(array_size)
atherm = fltarr(array_size)
avx = fltarr(array_size)
avy = fltarr(array_size)
avz = fltarr(array_size)
qual = lonarr(array_size,6)
iterations = lonarr(array_size)
alpha_fraction = fltarr(array_size)


openr,1, filename

i = 0
x = eof(1) 
;while not eof(1) do begin
while not x do begin
;  readf,1, year(i),day(i),sec(i),gvx(i),gvx(i),gvx(i)
;  readf,1, mode(i), npts(i), chisqr(i)
;  readf,1, gvx(i),gvy(i),gvz(i),gdens(i),gtherm(i)
;  readf,1, pvx(i),pvy(i),pvz(i),pdens(i),ptherm(i), $
;          adens(i),atherm(i),avx(i),avy(i),avz(i)
  readf,1, a,b,c,d,e,f 
   year(i) = a
   day(i)= b
   sec(i)= c
  readf,1, a,b,c,d,e,f,g,h,j,k,l
    mode(i)= a
    npts(i)= b
    chisqr(i) = c
    iterations(i) = d
    alpha_fraction(i) = e
    qual(i,0) = f
    qual(i,1) = g 
    qual(i,2) = h 
    qual(i,3) = j
    qual(i,4) = k 
    qual(i,5) = l 
  readf,1,a,b,c,d,e 
    gvx(i)= a
    gvy(i)= b
    gvz(i)= c
    gdens(i) = d
    gtherm(i) = e
  readf,1,a,b,c,d,e,f,g,h,k,j 
    pvx(i)= a
    pvy(i)= b
    pvz(i)= c
    pdens(i)= d
    ptherm(i)= e
    adens(i)= f
    atherm(i)= g
    avx(i)= h
    avy(i)=k
    avz(i) = j
  i = i + 1
    x = eof(1)
endwhile
close,1
time = sec/3600.
end

