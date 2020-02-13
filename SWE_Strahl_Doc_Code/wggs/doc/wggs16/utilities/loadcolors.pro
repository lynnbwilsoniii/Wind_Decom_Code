pro loadcolors,tbl=tbl

COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_cur

if keyword_set(tbl) eq 0 then tbl=18

;set top color (white in GSFC and IOWA color tables) to next to top color (RJF)
   loadct,tbl  ;GSFC color table
   ;loadct,13  ;RAINBOW color table
   r_orig(n_elements(r_orig)-1)=r_orig(n_elements(r_orig)-2)
   g_orig(n_elements(g_orig)-1)=g_orig(n_elements(r_orig)-2)
   b_orig(n_elements(b_orig)-1)=b_orig(n_elements(r_orig)-2)
   tvlct,r_orig,g_orig,b_orig
   !p.color=125

end
