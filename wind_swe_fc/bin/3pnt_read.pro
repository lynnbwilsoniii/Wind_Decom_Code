
spawn,'wc -l '+filename,string_lines
reads,string_lines,lines,format='(i)'
array_size = lines 
array_size = array_size(0)

char_dummy = 'dummy'
a = dblarr(1)
time  = dblarr(array_size)
vbulk1 = fltarr(array_size)
vbulk2 = fltarr(array_size)
dens1 = fltarr(array_size)
dens2 = fltarr(array_size)
vtherm1 = fltarr(array_size)
vtherm2 = fltarr(array_size)

openr,1, filename

i = 0
x = eof(1) 
while not x do begin
  readf,1, a,b,c,d,e,f,g 
   time(i) = a
   vbulk1(i) = b
   vbulk2(i) = c
   dens1(i) = d
   dens2(i) = e
   vtherm1(i) = f
   vtherm2(i) = g
  i = i + 1
  x = eof(1)
endwhile
close,1
end
