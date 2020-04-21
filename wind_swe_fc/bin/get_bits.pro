; @(#)get_bits.pro  VERSION 1.2    7/28/94   16:13:23
function get_bits,x,p,n
a=ishft(ishft(x,7-p),n-8)
return,a
end

