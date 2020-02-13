pro clrtbl_indx,hardcopy=hardcopy

COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
common wstuff,wst

if keyword_set(hardcopy) eq 0 then hardcopy=0

ctbl=fix(getenv('COLORTABLE'))
ctbl_hc=fix(getenv('COLORTABLE_HARDCOPY'))

print,'Setting default color table...'
help,ctbl,ctbl_hc
wst.colortable=ctbl < 40
wst.colortable_ps=ctbl_hc < 40 


if hardcopy then wst.colortable=wst.colortable_ps

;set top color to white 
;NOTE: For color plots, set maximum number of color to !d.table_size-1

   loadct,wst.colortable 
   wst.ncolors=!d.table_size
   multi,256/float(wst.ncolors) 
   r_orig(!d.table_size-1)=255     ;white plot axes, etc
   g_orig(!d.table_size-1)=255
   b_orig(!d.table_size-1)=255
   
   tvlct,r_orig,g_orig,b_orig

   !p.background=0                      ;black background

print,'Color table  ',wst.colortable,'  is set.'



;18=GSFC  23=IOWA  13=RAINBOW

case wst.colortable of
18: begin
  clr_green=fix((67./142.)*!d.table_size)
  clr_orange=fix((100./142.)*!d.table_size)          
endcase

13: begin
  clr_green=fix((80./142.)*!d.table_size)
  clr_orange=fix((105./142.)*!d.table_size) 
endcase

23: begin
  clr_green=fix((90./142.)*!d.table_size)
  clr_orange=fix((115./142.)*!d.table_size) 
endcase

else: 
endcase


wst.clr_green=clr_green
wst.clr_orange=clr_orange

end
