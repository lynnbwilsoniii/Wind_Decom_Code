;fit detector efficiency data

x=      [0.,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8,  1.0,  $
        1.2,  1.4,  1.6,  1.8,  2.0,  2.2,  2.4]

y=     [0.7, 1.15,  1.5,  1.85, 2.35,  2.7,  3.0,  3.4,  3.7,  4.0,  $
        4.2,  4.3,  4.4, 4.35,  4.3, 4.15,  4.0]

dely= [0.40, 0.39, 0.39, 0.38, 0.37, 0.36, 0.34, 0.32, 0.30 ,0.28,  $
       0.26, 0.24, 0.22, 0.20, 0.20, 0.20, 0.20]

window,0
plot,x,y+dely,linestyle=1
oplot,x,y-dely,linestyle=1
oplot,x,y

window,1
logen=x+1 & en=10.^logen
dlogenp=logen+dely & enp=10.^dlogenp
dlogenm=logen-dely & enm=10.^dlogenm
eff=0.20+y*(0.80-0.20)/4
plot,en,eff,/xlog,xrange=[1e1,1e4],xstyle=1,yrange=[0,1.0],ystyle=1,yticks=5,$
  /nodata,xtitle='energy (eV)',ytitle='efficiency',charsize=1.5
oplot,en,eff


n_deg=5  ;4  ;3
c=poly_fit(logen,eff,n_deg,yfit)
oplot,10.^logen,yfit,psym=4,color=175

window,2
plot,en,eff,/xlog,xrange=[1e1,1e4],xstyle=1,yrange=[0,1.0],ystyle=1,yticks=5,$
  /nodata,xtitle='energy (eV)',ytitle='efficiency',charsize=1.5
oplot,en,eff,psym=4

if n_deg eq 3 then w=c(0)+c(1)*logen+c(2)*logen^2+c(3)*logen^3
if n_deg eq 4 then w=c(0)+c(1)*logen+c(2)*logen^2+c(3)*logen^3+c(4)*logen^4
if n_deg eq 5 then w=c(0)+c(1)*logen+c(2)*logen^2+c(3)*logen^3+c(4)*logen^4+$
  c(5)*logen^5
oplot,10.^logen,w,color=175
print,c
end
