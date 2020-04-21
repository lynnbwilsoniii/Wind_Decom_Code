function get_bits,x,p,n
a=ishft(ishft(x,7-p),n-8)
return,a
end

