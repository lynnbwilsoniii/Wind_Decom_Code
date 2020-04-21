PRO freq

cal = ['00'X, '81'X, 'C1'X, '82'X, 'C2'X, '84'X, 'C4'X, '88'X, 'C8'X, $
	'90'X, 'D0'X, 'A0'X, 'E0'X]
info=intarr(5)

openr,unit,'freq.cal',/get_lun

repeat BEGIN
  readf,unit,info
  size=info(4)-info(3)+1
  data=intarr(size)
  readf,unit,data
  plot,psym=10, indgen(size)+info(3), data, xticklen=0, $
	title=string(info(0), info(1), string(byte(info(2)+64)), $
	format='("Cal level ",Z2,", cup ",I1,", collector ",A1)')

  print,'Press a key to continue'
  foo = get_kbrd(1)
END until eof(unit)

free_lun,unit

END
