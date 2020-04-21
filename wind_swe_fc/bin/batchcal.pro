; @(#)batchcal.pro  VERSION 1.2    7/28/94   16:13:43
on_error,2

print,'plotting spectrum',0
initplot,/l
show4cal,spectrum=i
lpplot

for i=1,50 do BEGIN
  print,'plotting spectrum',i
  initplot,/l
  show4cal,spectrum=i
  hlpplot
END

END
