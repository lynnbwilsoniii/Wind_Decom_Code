function makestring,tjd,ms

;makes a string from values


return,string(tjd,format='(i5)')+'_'+string(ms,format='(i8)')

end
