pro setgamma,gamma=gamma

;input gamma between 1.0 and 10.0 to 
; stretch high end and compress low end of scale......gamma lt 1, or
; stetch low end and compress high end of scale......gamma gt 1
 
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

if keyword_set(gamma) eq 0 then gamma=1.0
ncolors = !d.table_size
vbot = 0
vtop = 100
use_values=0
chop=0
 
   gamma = 10^((gamma/50.) - 1)
   set_gamma:
	if use_values then nc = 256 else nc = ncolors
	s = (nc-1)/100.
	x0 = vbot * s
	x1 = vtop * s
	if x0 ne x1 then s = (nc-1.0)/(x1 - x0) else s = 1.0
	int = -s * x0
	if gamma eq 1.0 then s = round(findgen(nc) * s + int > 0.0) $
	else s = ((findgen(nc) * (s/nc) + (int/nc) > 0.0) ^ gamma) * nc
	if chop ne 0 then begin
	    too_high = where(s ge nc, n)
	    if n gt 0 then s(too_high) = 0L
	    endif
	if use_values then begin
	    s = s < 255L
	    r_curr = s(r_orig)
	    g_curr = s(g_orig)
	    b_curr = s(b_orig)
	endif else begin
	    r_curr = r_orig(s)
	    g_curr = g_orig(s)
	    b_curr = b_orig(s)
 	endelse
	tvlct, r_curr, g_curr, b_curr

end
