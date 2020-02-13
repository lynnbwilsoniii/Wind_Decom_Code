function compare_strings,str1,str2,s1,s2,lpr=lpr

if keyword_set(lpr) eq 0 then lpr=0

;print,'comparing ',str1,'  ',str2,' :'
if s1 ne s2 then print,s1,'   ',s2,'   do not agree' $
else if lpr then print,s1,'   ',s2,'   agree'
return, s1 eq s2

end
