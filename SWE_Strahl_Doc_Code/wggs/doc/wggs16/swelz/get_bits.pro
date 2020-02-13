function get_bits,x,p,n

;gets range of n bits, beginning at bit p-n-1 and ending at bit p, right to left
;ex: to get the lowest 6 bits, 0 1 2 3 4 5: n=6, p=5
;a=ishft(ishft(x,7-p),n-8)
return,ishft(ishft(x,7-p),n-8)
end

