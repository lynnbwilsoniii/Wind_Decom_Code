; @(#)mitlabel.pro  VERSION 1.2    7/28/94   16:13:40
PRO mitlabel,ft,fn,utc,screen=scr

 xyouts,0.1,.07,/normal,utc,ali=0.,font=ft
 xyouts,0.1,.12,/normal,'Mode 1 -- CAL',ali=0.,font=ft
 xyouts,0.9,.12,/normal,'GGS WIND',ali=1.,font=ft
 xyouts,0.9,.07,/normal,'SWE Faraday Cup',ali=1.,font=ft
 if KEYWORD_SET(scr) then BEGIN
   xyouts,0.5,.015,/normal,'Massachusetts Institute of Technology', $
	ali=.5, charsize=1.5, font=ft
 END else BEGIN
   xyouts,0.5,.12,/normal,string('file: ',fn),ali=.5,font=ft
   xyouts,0.5,.00,/normal,'!15Massachusetts Institute of Technology!X', $
	ali=.5, charsize=1.5, font=ft
 END

END
