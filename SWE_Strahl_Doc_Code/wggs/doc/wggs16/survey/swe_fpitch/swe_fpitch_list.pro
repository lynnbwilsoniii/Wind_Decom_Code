pro swe_fpitch_list,list

list=strarr(17)
list(0)='Spin avg f(en,t)'
enindx=indgen(16) 
list(1:16) = string(fltarr(n_elements(enindx)),format='(i4)')+' ev'  
end