pro ind_scidat,index

; get offsets (array index) into mjf array of the science data, 
; i.e., the loose byte 19 allocated to SWE + the 43 fixed columns

n_mnf=250
l_mnf=45
col_mnf=indgen(n_mnf)*l_mnf + 2
len_mnf=intarr(n_mnf)+l_mnf-2
w=where(indgen(n_mnf)-fix(indgen(n_mnf)/10)*10 eq 2 or $
        indgen(n_mnf)-fix(indgen(n_mnf)/10)*10 eq 9)
col_mnf(w)=w*l_mnf + 1
len_mnf(w)=l_mnf-1
;for i=0,n_mnf-1 do print,i,col_mnf(i)

index=col_mnf(0)+indgen(len_mnf(0))
for i=1,n_mnf-1 do index=[index,col_mnf(i)+indgen(len_mnf(i))]

lpr_indx=0
if lpr_indx then begin
  print,' '
  print,'offsets of scidat, i.e., loose byte 19 + 43 fixed columns'
  help,index
  print,index,format='(10i6)'
endif

end
